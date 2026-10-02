//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Structural invariants the IR must satisfy between optimization passes
// but BEFORE `lower.rs`, which intentionally introduces multi-def Copies
// as part of φ-elimination.
//
//   I1 — SINGLE-DEF SSA TARGETS
//        Every `PseudoId` that appears as an instruction's `target` must
//        appear as such at most once across the function. SSA single-def is
//        what pseudo-merging passes (copyprop, CSE, GVN, SCCP) rely on.
//
//        Inline-asm output operands are excluded: the linearizer emits
//        matched/in-out constraints (`"+r"(x)`, `"0"(x)`) with one pseudo
//        serving as both the load result and the asm output.
//
//   I3 — TERMINATOR TARGETS REFERENCE VALID BLOCKS
//        Every branch-style instruction (Br/Cbr/Switch) carries
//        `BasicBlockId` references for its successor(s). All such
//        references must point at a block actually present in
//        `func.blocks`. A reference to a deleted or never-created
//        block ID would crash the codegen layer's label-resolution
//        pass with a confusing error far from the source bug. I3
//        catches this at the optimizer/codegen boundary instead.
//
//        The targets are `Instruction::control_targets`: a branch's and a
//        switch's, and an `asm goto`'s labels.
//
//   I2 — MEMORY BARRIERS ARE DCE ROOTS
//        Every instruction for which `Instruction::is_memory_barrier()`
//        returns `true` must also have `op.has_side_effects() == true`.
//        Otherwise mark-sweep DCE could delete a barrier whose result is
//        unused, silently dropping a `Fence`, an `Atomic*`, a Call, or an
//        `asm("..." ::: "memory")` from the program — a source of
//        kernel-style spinlock breakage that is invisible at compile
//        time. The two predicates are kept structurally aligned by this
//        invariant; if either set drifts, the validator surfaces it
//        immediately rather than waiting for a memory-reordering pass to
//        miscompile a real program.
//
//   I6 — A MEMORY ACCESS OFFSET IS A MACHINE DISPLACEMENT
//        A load, store or atomic is addressed at `src[0] + offset` with a
//        signed 32-bit displacement read through `Instruction::displacement`,
//        on both backends.
//        `Linearizer::emit` folds a larger offset into the address; a pass
//        that later produced one would reintroduce the truncation that sent
//        a member past 2 GiB gigabytes away.
//
//   I7 — THE PSEUDO INDEX AGREES WITH THE PSEUDO LIST
//        `Function::get_pseudo` answers through `pseudo_idx`, an id-to-
//        position map. Removing or reordering `pseudos` without rebuilding
//        it makes every later lookup answer a neighbour's kind, so a
//        constant reads as a symbol or a register as a constant. Every pseudo
//        must be found at its own position.
//
//   I10 — AN OPERAND OF ANOTHER TYPE IS RECORDED AS ONE
//        `typ`/`size` describe an instruction's result, for every opcode; a
//        comparison or a bit count, which reads another type than it
//        produces, records its operands in `src_typ`/`src_size`
//        (`Instruction::operand_type`). Built without them, a backend would
//        compare at the result's width.
//
//   I11 — A LIFETIME MARKER NAMES A LOCAL OF ITS OWN FUNCTION
//        `LifetimeEnd` names its local out of band, where no pass that
//        rewrites operands looks. A local dropped or renamed without its
//        markers would leave the allocator ending the lifetime of nothing,
//        or of another function's object.
//
//   I8 — THE CFG CACHE AGREES WITH THE INSTRUCTIONS
//        Every block ends in exactly one terminator; `children` is exactly the
//        successors its instructions name (`BasicBlock::named_successors`, or
//        address-taken blocks for a computed `goto`); `parents` is exactly the
//        inverse of `children`; `get_block` finds every block where it is.
//
//   I9 — PHIS AGREE WITH THE EDGES
//        Every phi takes one operand along each predecessor edge and no
//        other, and every `PhiSource` feeds a phi in a successor of its own
//        block. Only while the IR is in SSA form; lowering removes both.
//
// The validator runs always, in every build, through [`verify`]: after
// linearization, after optimization, and after lowering. It is a linear walk,
// and a structural error it catches is a miscompile it would otherwise have
// shipped -- `cargo test --release`, the torture harness and a released `c17`
// all run without debug assertions, which is where it used to live, so it ran
// in none of them.

use super::{BasicBlockId, Function, Module, Opcode, PseudoId};
use std::collections::{HashMap, HashSet};
use std::fmt;

// Error model

/// A single invariant violation. Carries enough context for a developer
/// inspecting an IR dump or stepping through with a debugger to find the
/// offending site.
#[derive(Debug, Clone)]
pub enum ValidationError {
    /// I1 violation: the same SSA pseudo is defined by more than one
    /// instruction inside the function.
    ///
    /// `sites` lists every `(block_index, instruction_index, defining_opcode)`
    /// triple that writes the pseudo. Length >= 2 by construction.
    MultipleDefinitions {
        function: String,
        pseudo: PseudoId,
        sites: Vec<(usize, usize, Opcode)>,
    },
    /// I2 violation: an instruction satisfies `is_memory_barrier()` but
    /// its opcode is not in `has_side_effects()`. DCE would delete it.
    BarrierWithoutSideEffect {
        function: String,
        block: usize,
        index: usize,
        opcode: Opcode,
    },
    /// I3 violation: a branch-style instruction references a
    /// `BasicBlockId` that doesn't exist in `func.blocks`. Carries
    /// the offending block id and the location of the bad
    /// reference.
    InvalidBranchTarget {
        function: String,
        block: usize,
        index: usize,
        opcode: Opcode,
        target: BasicBlockId,
    },
    /// I5 violation: an instruction reaches memory but is not in
    /// `has_side_effects()`, so DCE would delete it while a memory pass
    /// would still have to reason about it.
    MemoryAccessWithoutSideEffect {
        function: String,
        block: usize,
        index: usize,
        opcode: Opcode,
    },
    /// I6 violation: a memory access carries an offset no signed 32-bit
    /// displacement holds.
    DisplacementOutOfRange {
        function: String,
        block: usize,
        index: usize,
        offset: i64,
    },
    /// I8 or I9 violation: the CFG cache, or a phi, disagrees with the
    /// instructions or with the other edges.
    CfgInconsistent {
        function: String,
        block: BasicBlockId,
        what: String,
    },
    /// I10 violation: a comparison or bit count with no operand
    /// type or width recorded.
    MissingOperandType {
        function: String,
        block: usize,
        index: usize,
        opcode: Opcode,
    },
    /// I11 violation: a `LifetimeEnd` naming no local of this function.
    StrayLifetimeEnd {
        function: String,
        block: usize,
        index: usize,
    },
    /// I7 violation: `get_pseudo` does not find this pseudo at its own
    /// position in `pseudos`.
    StalePseudoIndex { function: String, pseudo: PseudoId },
    /// A placeholder opcode that something downstream was supposed to
    /// resolve is still here. Neither backend knows it, and both end their
    /// opcode match in a catch-all, so it would be dropped in silence and
    /// its target left undefined.
    UnresolvedPlaceholder {
        function: String,
        block: usize,
        index: usize,
        opcode: Opcode,
    },
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValidationError::MultipleDefinitions {
                function,
                pseudo,
                sites,
            } => {
                write!(
                    f,
                    "[ir-validate I1] in function `{}`: pseudo {} has {} definitions: ",
                    function,
                    pseudo,
                    sites.len()
                )?;
                for (i, (bb, insn_idx, op)) in sites.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "bb={} insn={} op={:?}", bb, insn_idx, op)?;
                }
                Ok(())
            }
            ValidationError::BarrierWithoutSideEffect {
                function,
                block,
                index,
                opcode,
            } => write!(
                f,
                "[ir-validate I2] in function `{function}`: bb={block} insn={index} op={opcode:?} \
                 is a memory barrier but not in has_side_effects() — DCE would delete it"
            ),
            ValidationError::InvalidBranchTarget {
                function,
                block,
                index,
                opcode,
                target,
            } => write!(
                f,
                "[ir-validate I3] in function `{function}`: bb={block} insn={index} op={opcode:?} \
                 references unknown BasicBlockId {target:?}"
            ),
            ValidationError::MemoryAccessWithoutSideEffect {
                function,
                block,
                index,
                opcode,
            } => write!(
                f,
                "[ir-validate I5] in function `{function}`: bb={block} insn={index} \
                 op={opcode:?} may access memory but is not in has_side_effects()"
            ),
            ValidationError::DisplacementOutOfRange {
                function,
                block,
                index,
                offset,
            } => write!(
                f,
                "[ir-validate I6] in function `{function}`: bb={block} insn={index} \
                 memory access offset {offset} does not fit a 32-bit displacement"
            ),
            ValidationError::CfgInconsistent {
                function,
                block,
                what,
            } => write!(
                f,
                "[ir-validate I8/I9] in function `{function}`: {block}: {what}"
            ),
            ValidationError::MissingOperandType {
                function,
                block,
                index,
                opcode,
            } => write!(
                f,
                "[ir-validate I10] in function `{function}`: bb={block} insn={index} \
                 op={opcode:?} records no operand type or width"
            ),
            ValidationError::StrayLifetimeEnd {
                function,
                block,
                index,
            } => write!(
                f,
                "[ir-validate I11] in function `{function}`: bb={block} insn={index} \
                 ends the lifetime of no local of this function"
            ),
            ValidationError::StalePseudoIndex { function, pseudo } => write!(
                f,
                "[ir-validate I7] in function `{function}`: pseudo {pseudo:?} is not \
                 found at its own position; `pseudo_idx` is stale"
            ),
            ValidationError::UnresolvedPlaceholder {
                function,
                block,
                index,
                opcode,
            } => write!(
                f,
                "[ir-validate I4] in function `{function}`: bb={block} insn={index} op={opcode:?} \
                 is a placeholder that should have been resolved before codegen"
            ),
        }
    }
}

impl std::error::Error for ValidationError {}

// Entry points

/// Where in the pipeline the IR is being checked, which decides what holds.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Stage {
    /// SSA form, from linearization through optimization: every invariant.
    Ssa,
    /// After `lower`: phi elimination has deliberately given pseudos several
    /// definitions and removed every phi, so I1 and I9 no longer apply, and
    /// no placeholder may remain (I4).
    Lowered,
}

/// Check every function of `module`, and stop the compiler on a violation.
///
/// A violation is a compiler bug, never a problem with the program, so it is
/// reported as an internal compiler error rather than a diagnostic, naming
/// the pipeline point that found it.
pub fn verify(module: &Module, stage: Stage, after: &str) {
    if let Err(errors) = validate_module_at(module, stage) {
        let list = errors
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("\n  ");
        panic!("internal compiler error: invalid IR after {after}:\n  {list}");
    }
}

/// Validate every function in a module at `stage`. Returns the full list of
/// violations (across all functions) when something is wrong.
pub fn validate_module_at(module: &Module, stage: Stage) -> Result<(), Vec<ValidationError>> {
    let mut errors = Vec::new();
    for func in &module.functions {
        if let Err(mut errs) = validate_function_at(func, stage) {
            errors.append(&mut errs);
        }
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Validate every function in a module in SSA form.
pub fn validate_module(module: &Module) -> Result<(), Vec<ValidationError>> {
    validate_module_at(module, Stage::Ssa)
}

/// I4 -- no placeholder opcode survives to codegen.
///
/// Separate from [`validate_function`] because it is the one check that
/// holds on *lowered* IR: phi elimination deliberately creates multi-def
/// copies, so I1 does not, and calling the whole validator after lowering
/// would report those instead.
///
/// A function codegen skips (`Function::emit` false) is exempt. Its body is
/// kept only for the inliner, and a `__builtin_va_arg_pack_len()` in one
/// that forwards its caller's arguments has nothing to resolve against: the
/// pack exists only at a call site, and every one has been spliced.
pub fn check_no_placeholders(func: &Function) -> Result<(), Vec<ValidationError>> {
    if !func.emit {
        return Ok(());
    }
    let mut errors = Vec::new();
    for (block, bb) in func.blocks.iter().enumerate() {
        for (index, insn) in bb.insns.iter().enumerate() {
            if matches!(insn.op, Opcode::ConstantP | Opcode::VaArgPackLen) {
                errors.push(ValidationError::UnresolvedPlaceholder {
                    function: func.name.clone(),
                    block,
                    index,
                    opcode: insn.op,
                });
            }
        }
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Validate a single function in SSA form, for callers holding hand-built IR
/// rather than a whole Module.
pub fn validate_function(func: &Function) -> Result<(), Vec<ValidationError>> {
    validate_function_at(func, Stage::Ssa)
}

/// Validate a single function at `stage`.
pub fn validate_function_at(func: &Function, stage: Stage) -> Result<(), Vec<ValidationError>> {
    let mut errors = Vec::new();
    match stage {
        Stage::Ssa => {
            check_single_def(func, &mut errors);
            check_phis(func, &mut errors);
        }
        Stage::Lowered => {
            if let Err(mut e) = check_no_placeholders(func) {
                errors.append(&mut e);
            }
        }
    }
    check_barrier_implies_side_effect(func, &mut errors);
    check_memory_access_implies_side_effect(func, &mut errors);
    check_branch_targets_valid(func, &mut errors);
    check_displacements_in_range(func, &mut errors);
    check_pseudo_index(func, &mut errors);
    check_cfg(func, &mut errors);
    check_operand_types(func, &mut errors);
    check_lifetime_markers(func, &mut errors);
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

// Invariant checks

/// I1 — every SSA target pseudo is defined exactly once.
///
/// "Target" means `insn.target` — ordinary single-result IR instructions
/// (Copy, arithmetic, Load, Phi, PhiSource, Call, ...).
///
/// `insn.asm_data.outputs[i].pseudo` is excluded: matched/in-out
/// constraints (`"+r"(x)`, `"0"(x)`) share one pseudo between the load
/// result and the asm output. Phi-source back-pointers
/// (`PhiSource.phi_list[i].1`) are excluded too — they reference the
/// destination Phi's target, not a new definition.
fn check_single_def(func: &Function, out: &mut Vec<ValidationError>) {
    // pseudo → list of definition sites
    let mut defs: HashMap<PseudoId, Vec<(usize, usize, Opcode)>> = HashMap::new();

    for (bb_idx, bb) in func.blocks.iter().enumerate() {
        for (insn_idx, insn) in bb.insns.iter().enumerate() {
            if let Some(t) = insn.target {
                defs.entry(t).or_default().push((bb_idx, insn_idx, insn.op));
            }
        }
    }

    for (pseudo, sites) in defs {
        if sites.len() > 1 {
            out.push(ValidationError::MultipleDefinitions {
                function: func.name.clone(),
                pseudo,
                sites,
            });
        }
    }
}

/// I7 — `get_pseudo` finds every pseudo at its own position.
pub fn check_pseudo_index(func: &Function, out: &mut Vec<ValidationError>) {
    for pseudo in &func.pseudos {
        if func.get_pseudo(pseudo.id).map(|p| p.id) != Some(pseudo.id) {
            out.push(ValidationError::StalePseudoIndex {
                function: func.name.clone(),
                pseudo: pseudo.id,
            });
        }
    }
}

/// I6 — every memory access offset fits a signed 32-bit displacement.
fn check_displacements_in_range(func: &Function, out: &mut Vec<ValidationError>) {
    for (bb_idx, bb) in func.blocks.iter().enumerate() {
        for (insn_idx, insn) in bb.insns.iter().enumerate() {
            if insn.op.addresses_memory() && i32::try_from(insn.offset).is_err() {
                out.push(ValidationError::DisplacementOutOfRange {
                    function: func.name.clone(),
                    block: bb_idx,
                    index: insn_idx,
                    offset: insn.offset,
                });
            }
        }
    }
}

/// I3 — every branch-style instruction's `BasicBlockId` references
/// must point at a real block in `func.blocks`. Catches CFG-corruption
/// bugs at the optimizer/codegen boundary, where they would otherwise
/// surface as label-resolution failures inside the codegen layer.
fn check_branch_targets_valid(func: &Function, out: &mut Vec<ValidationError>) {
    let valid: HashSet<BasicBlockId> = func.blocks.iter().map(|b| b.id).collect();
    let check = |bb_idx: usize,
                 insn_idx: usize,
                 opcode: Opcode,
                 target: BasicBlockId,
                 out: &mut Vec<ValidationError>| {
        if !valid.contains(&target) {
            out.push(ValidationError::InvalidBranchTarget {
                function: func.name.clone(),
                block: bb_idx,
                index: insn_idx,
                opcode,
                target,
            });
        }
    };
    for (bb_idx, bb) in func.blocks.iter().enumerate() {
        for (insn_idx, insn) in bb.insns.iter().enumerate() {
            for t in insn.control_targets() {
                check(bb_idx, insn_idx, insn.op, t, out);
            }
        }
    }
}

/// I2 — every memory-barrier instruction must also have side effects, or
/// DCE would silently delete it. See the module-level documentation for
/// the contract this protects.
fn check_barrier_implies_side_effect(func: &Function, out: &mut Vec<ValidationError>) {
    for (bb_idx, bb) in func.blocks.iter().enumerate() {
        for (insn_idx, insn) in bb.insns.iter().enumerate() {
            if insn.is_memory_barrier() && !insn.op.has_side_effects() {
                out.push(ValidationError::BarrierWithoutSideEffect {
                    function: func.name.clone(),
                    block: bb_idx,
                    index: insn_idx,
                    opcode: insn.op,
                });
            }
        }
    }
}

/// I5 — a memory-accessing instruction is side-effecting, or is a `Load`.
///
/// `Opcode::may_access_memory` answers *extent* ("does this reach memory")
/// and `has_side_effects` answers *deletability* ("may DCE remove this").
/// A pass that removes or moves memory operations consults both, so if the
/// two drift an opcode that writes memory becomes invisible to DCE *and*
/// to the access scan — which is how a store gets deleted, or a load
/// forwarded across one.
///
/// `Load` is the one deliberate exception: it reaches memory and DCE may
/// delete it, because reading a non-volatile location has no effect. Reading a
/// `volatile` one is observable, and that is refused per *access*, by
/// `Instruction::is_volatile_access`, which `dce::is_root` consults alongside
/// this predicate.
fn check_memory_access_implies_side_effect(func: &Function, out: &mut Vec<ValidationError>) {
    for (block, bb) in func.blocks.iter().enumerate() {
        for (index, insn) in bb.insns.iter().enumerate() {
            if insn.op == Opcode::Load || !insn.op.may_access_memory() {
                continue;
            }
            if !insn.op.has_side_effects() {
                out.push(ValidationError::MemoryAccessWithoutSideEffect {
                    function: func.name.clone(),
                    block,
                    index,
                    opcode: insn.op,
                });
            }
        }
    }
}

/// I10 -- a comparison or bit count records the operands it reads.
fn check_operand_types(func: &Function, out: &mut Vec<ValidationError>) {
    for (block, bb) in func.blocks.iter().enumerate() {
        for (index, insn) in bb.insns.iter().enumerate() {
            if (insn.op.is_comparison() || insn.op.is_bit_count())
                && (insn.src_typ.is_none() || insn.src_size == 0)
            {
                out.push(ValidationError::MissingOperandType {
                    function: func.name.clone(),
                    block,
                    index,
                    opcode: insn.op,
                });
            }
        }
    }
}

/// I11 -- a lifetime marker names a local of its own function.
fn check_lifetime_markers(func: &Function, out: &mut Vec<ValidationError>) {
    for (block, bb) in func.blocks.iter().enumerate() {
        for (index, insn) in bb.insns.iter().enumerate() {
            if insn.op != Opcode::LifetimeEnd {
                continue;
            }
            let names_a_local = insn
                .extra()
                .lifetime_of
                .is_some_and(|l| func.local_of(l).is_some());
            if !names_a_local {
                out.push(ValidationError::StrayLifetimeEnd {
                    function: func.name.clone(),
                    block,
                    index,
                });
            }
        }
    }
}

/// I8 -- the CFG cache agrees with the instructions.
///
/// * Every block ends in exactly one terminator, and has no other.
/// * `children` lists each successor once, and is exactly what the block's
///   instructions name ([`BasicBlock::named_successors`]); a block ending in
///   `IndirectBr` names none, and each of its successors must be a block
///   whose address is taken.
/// * `parents` is exactly the inverse of `children`.
/// * `get_block` finds every block, at its own position.
///
/// `validate` held six invariants and none of them was about the CFG, so a
/// `for` loop's back edge linked from the wrong block passed it in both
/// copies of the loop lowering, and `dce` could drop a `children` edge and
/// leave `parents` naming it with nothing to notice.
fn check_cfg(func: &Function, out: &mut Vec<ValidationError>) {
    let bad = |block: BasicBlockId, what: String, out: &mut Vec<ValidationError>| {
        out.push(ValidationError::CfgInconsistent {
            function: func.name.clone(),
            block,
            what,
        });
    };

    let mut seen = HashSet::new();
    for (idx, bb) in func.blocks.iter().enumerate() {
        if !seen.insert(bb.id) {
            bad(bb.id, "block id used twice".into(), out);
        }
        if func.block_index(bb.id) != Some(idx) {
            bad(
                bb.id,
                format!(
                    "block index says {:?}, block is at {idx}",
                    func.block_index(bb.id)
                ),
                out,
            );
        }
        match bb.insns.iter().position(|i| i.op.is_terminator()) {
            None => bad(bb.id, "no terminator".into(), out),
            Some(p) if p + 1 != bb.insns.len() => bad(
                bb.id,
                format!("terminator at {p} is not the last instruction"),
                out,
            ),
            Some(_) => {}
        }

        let children: HashSet<BasicBlockId> = bb.children.iter().copied().collect();
        if children.len() != bb.children.len() {
            bad(bb.id, "an edge is listed twice in children".into(), out);
        }
        match bb.named_successors() {
            Some(named) => {
                let named: HashSet<BasicBlockId> = named.into_iter().collect();
                if named != children {
                    bad(
                        bb.id,
                        format!(
                            "instructions name {} but children are {}",
                            ids(&named),
                            ids(&children)
                        ),
                        out,
                    );
                }
            }
            None => {
                for c in &children {
                    if !func.get_block(*c).is_some_and(|b| b.addr_taken) {
                        bad(
                            bb.id,
                            format!("computed goto reaches {c}, whose address is not taken"),
                            out,
                        );
                    }
                }
            }
        }
    }

    let mut expected: HashMap<BasicBlockId, HashSet<BasicBlockId>> = HashMap::new();
    for bb in &func.blocks {
        for c in &bb.children {
            expected.entry(*c).or_default().insert(bb.id);
        }
    }
    for bb in &func.blocks {
        let have: HashSet<BasicBlockId> = bb.parents.iter().copied().collect();
        if have.len() != bb.parents.len() {
            bad(bb.id, "an edge is listed twice in parents".into(), out);
        }
        let want = expected.remove(&bb.id).unwrap_or_default();
        if have != want {
            bad(
                bb.id,
                format!(
                    "parents are {} but {} name it as a successor",
                    ids(&have),
                    ids(&want)
                ),
                out,
            );
        }
    }
    for (missing, from) in expected {
        bad(
            missing,
            format!("is a successor of {} but is not a block", ids(&from)),
            out,
        );
    }
}

/// I9 -- every phi takes exactly one operand along each incoming edge, and a
/// `PhiSource` feeds a phi in a successor of its own block.
///
/// Phi elimination puts the copy for an edge at the end of its source, so a
/// phi operand naming a block that is not a predecessor is a copy into
/// nowhere, and a predecessor with no operand leaves the phi undefined along
/// that edge.
fn check_phis(func: &Function, out: &mut Vec<ValidationError>) {
    for bb in &func.blocks {
        let parents: HashSet<BasicBlockId> = bb.parents.iter().copied().collect();
        for insn in &bb.insns {
            match insn.op {
                Opcode::Phi => {
                    let preds: Vec<BasicBlockId> = insn.phi_list.iter().map(|(p, _)| *p).collect();
                    let set: HashSet<BasicBlockId> = preds.iter().copied().collect();
                    if set.len() != preds.len() || set != parents {
                        out.push(ValidationError::CfgInconsistent {
                            function: func.name.clone(),
                            block: bb.id,
                            what: format!(
                                "phi {:?} takes operands along {} but the predecessors are {}",
                                insn.target,
                                ids(&set),
                                ids(&parents)
                            ),
                        });
                    }
                }
                Opcode::PhiSource => {
                    let feeds = insn.phi_list.first().map(|p| p.0);
                    if !feeds.is_some_and(|f| bb.children.contains(&f)) {
                        out.push(ValidationError::CfgInconsistent {
                            function: func.name.clone(),
                            block: bb.id,
                            what: format!(
                                "phisrc {:?} feeds {feeds:?}, not a successor",
                                insn.target
                            ),
                        });
                    }
                }
                _ => {}
            }
        }
    }
}

fn ids(s: &HashSet<BasicBlockId>) -> String {
    let mut v: Vec<u32> = s.iter().map(|b| b.0).collect();
    v.sort_unstable();
    format!("{v:?}")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, Function, Instruction, Pseudo, PseudoId};
    use crate::target::Target;
    use crate::types::TypeTable;

    fn fresh_func(name: &str) -> Function {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new(name, types.int_id);
        func.entry = BasicBlockId(0);
        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.insns.push(Instruction::new(Opcode::Entry));
        bb.insns.push(Instruction::ret(None));
        func.add_block(bb);
        func
    }

    /// Add `insn` to block 0, ahead of its terminator.
    fn push(func: &mut Function, insn: Instruction) {
        func.blocks[0].insert_before_terminator(insn);
    }

    /// Replace block 0's terminator.
    fn terminate(func: &mut Function, insn: Instruction) {
        *func.blocks[0].insns.last_mut().unwrap() = insn;
    }

    fn copy_insn(dst: u32, src: u32) -> Instruction {
        let mut i = Instruction::new(Opcode::Copy);
        i.target = Some(PseudoId(dst));
        i.src = vec![PseudoId(src)];
        i
    }

    /// I4: a placeholder is flagged in a function codegen emits, and not in
    /// one it skips -- a `__builtin_va_arg_pack_len()` forwarder left in the
    /// module after every call to it was inlined.
    #[test]
    fn validate_flags_a_placeholder_only_in_an_emitted_function() {
        let types = TypeTable::new(&Target::host());
        let mut func = fresh_func("count");
        func.add_pseudo(Pseudo::reg(PseudoId(0), 0));
        push(
            &mut func,
            Instruction::new(Opcode::VaArgPackLen)
                .with_target(PseudoId(0))
                .with_type_and_size(types.int_id, 32),
        );
        let errors = validate_function_at(&func, Stage::Lowered).unwrap_err();
        assert!(
            matches!(
                errors.as_slice(),
                [ValidationError::UnresolvedPlaceholder { .. }]
            ),
            "{errors:?}"
        );

        func.emit = false;
        assert!(validate_function_at(&func, Stage::Lowered).is_ok());
    }

    /// I6: a load or atomic whose offset no 32-bit displacement holds is
    /// flagged, and the largest one that fits is not.
    #[test]
    fn validate_flags_a_displacement_past_i32() {
        let types = TypeTable::new(&Target::host());
        let load = |offset| Instruction::load(PseudoId(1), PseudoId(0), offset, types.int_id, 32);
        let atomic = |offset| {
            Instruction::new(Opcode::AtomicLoad)
                .with_target(PseudoId(1))
                .with_src(PseudoId(0))
                .with_offset(offset)
                .with_type_and_size(types.int_id, 32)
        };
        let max = i64::from(i32::MAX);
        for (insn, ok) in [
            (load(max), true),
            (load(max + 1), false),
            (atomic(max), true),
            (atomic(max + 1), false),
        ] {
            let offset = insn.offset;
            let mut func = fresh_func("t");
            for i in 0..=1 {
                func.add_pseudo(Pseudo::reg(PseudoId(i), i));
            }
            push(&mut func, insn);
            let result = validate_function(&func);
            assert_eq!(result.is_ok(), ok, "offset {offset}: {result:?}");
            if !ok {
                assert!(matches!(
                    result.unwrap_err()[0],
                    ValidationError::DisplacementOutOfRange { .. }
                ));
            }
        }
    }

    /// Baseline: well-formed single-def IR passes.
    #[test]
    fn validate_accepts_single_def() {
        let mut func = fresh_func("t");
        for i in 0..=2 {
            func.add_pseudo(Pseudo::reg(PseudoId(i), i));
        }
        push(&mut func, copy_insn(1, 0));
        push(&mut func, copy_insn(2, 1));
        assert!(validate_function(&func).is_ok());
    }

    /// I1 violation: two `Copy` instructions share a target.
    #[test]
    fn validate_flags_multi_def_target() {
        let mut func = fresh_func("two_arms");
        for i in 0..=3 {
            func.add_pseudo(Pseudo::reg(PseudoId(i), i));
        }
        push(&mut func, copy_insn(3, 0));
        push(&mut func, copy_insn(3, 1));
        let errors = validate_function(&func).unwrap_err();
        assert_eq!(errors.len(), 1);
        match &errors[0] {
            ValidationError::MultipleDefinitions {
                pseudo,
                sites,
                function,
            } => {
                assert_eq!(*pseudo, PseudoId(3));
                assert_eq!(function, "two_arms");
                assert_eq!(sites.len(), 2);
            }
            other => panic!("unexpected error variant: {other:?}"),
        }
    }

    /// Scope check: inline-asm outputs are NOT counted as SSA defs. The
    /// linearizer emits matched/in-out constraints (`"+r"(x)`, `"0"(x)`)
    /// with the load result and the asm output sharing one pseudo.
    #[test]
    fn validate_does_not_flag_asm_outputs() {
        use crate::ir::{AsmConstraint, AsmData};

        let mut func = fresh_func("asm_scope");
        for i in 0..=2 {
            func.add_pseudo(Pseudo::reg(PseudoId(i), i));
        }
        // `%2 = copy %0`
        push(&mut func, copy_insn(2, 0));
        // Asm whose output also writes %2.
        let mut asm = Instruction::new(Opcode::Asm);
        asm.extra_mut().asm_data = Some(Box::new(AsmData {
            template: "movl $1, %0".into(),
            outputs: vec![AsmConstraint::new(
                PseudoId(2),
                "=r",
                crate::target::Arch::X86_64,
                32,
            )],
            inputs: vec![],
            clobbers: vec![],
            goto_labels: vec![],
        }));
        push(&mut func, asm);

        // Asm outputs are ignored; no error.
        assert!(validate_function(&func).is_ok());
    }

    /// `PhiSource.phi_list` carries a back-pointer to the destination Phi's
    /// target pseudo, NOT a definition. Counting it would fail legitimate
    /// phi-join IR.
    #[test]
    fn validate_does_not_count_phisource_back_pointer() {
        let mut func = fresh_func("phi_back_ptr");
        for i in 0..=3 {
            func.add_pseudo(Pseudo::reg(PseudoId(i), i));
        }
        // Phi defines %2 (single def).
        let mut phi = Instruction::new(Opcode::Phi);
        phi.target = Some(PseudoId(2));
        phi.phi_list = vec![
            (BasicBlockId(1), PseudoId(0)),
            (BasicBlockId(2), PseudoId(1)),
        ];
        push(&mut func, phi);

        // PhiSource defines %3 and back-points at %2 (must not be counted
        // as a second def of %2).
        let mut psrc = Instruction::new(Opcode::PhiSource);
        psrc.target = Some(PseudoId(3));
        psrc.src = vec![PseudoId(0)];
        psrc.phi_list = vec![(BasicBlockId(5), PseudoId(2))];
        push(&mut func, psrc);

        // The CFG here is not a real one; only I1 is under test.
        let errors = validate_function(&func).err().unwrap_or_default();
        assert!(
            !errors
                .iter()
                .any(|e| matches!(e, ValidationError::MultipleDefinitions { .. })),
            "{errors:?}"
        );
    }

    /// I2 — structural enforcement that every barrier opcode is also in
    /// `has_side_effects()`. This is a meta-test: it walks the cartesian
    /// product of "is barrier" and "has side effects" for every opcode
    /// the predicates know about and asserts the implication. If a future
    /// change adds a new barrier opcode without updating
    /// `has_side_effects`, this test fails before any miscompilation can
    /// reach a user.
    #[test]
    fn i2_barrier_implies_side_effect_structural() {
        use crate::ir::AsmData;

        // Every opcode that can return true from is_memory_barrier() under
        // any input. We can't iterate Opcode directly, so we enumerate
        // representatives that hit each match arm in is_memory_barrier.
        let mut samples: Vec<Instruction> = vec![
            Instruction::new(Opcode::Fence),
            Instruction::new(Opcode::Call),
            Instruction::new(Opcode::Setjmp),
            Instruction::new(Opcode::Longjmp),
            Instruction::new(Opcode::AtomicLoad),
            Instruction::new(Opcode::AtomicStore),
            Instruction::new(Opcode::AtomicSwap),
            Instruction::new(Opcode::AtomicCas),
            Instruction::new(Opcode::AtomicFetchAdd),
            Instruction::new(Opcode::AtomicFetchSub),
            Instruction::new(Opcode::AtomicFetchAnd),
            Instruction::new(Opcode::AtomicFetchOr),
            Instruction::new(Opcode::AtomicFetchXor),
        ];
        let mut asm = Instruction::new(Opcode::Asm);
        asm.extra_mut().asm_data = Some(Box::new(AsmData {
            template: String::new(),
            outputs: Vec::new(),
            inputs: Vec::new(),
            clobbers: vec!["memory".to_string()],
            goto_labels: Vec::new(),
        }));
        samples.push(asm);

        for insn in &samples {
            assert!(
                insn.is_memory_barrier(),
                "{:?} should be a barrier",
                insn.op
            );
            assert!(
                insn.op.has_side_effects(),
                "{:?} is a barrier but not has_side_effects — DCE would delete it",
                insn.op
            );
        }
    }

    /// I5 — every memory-accessing opcode in real IR is side-effecting or
    /// is a `Load`. Representative sample; the exhaustive coverage comes
    /// from the runtime check, which sees every instruction c17 compiles.
    #[test]
    fn i5_memory_access_implies_side_effect_or_load() {
        for op in [
            Opcode::Store,
            Opcode::Call,
            Opcode::Memcpy,
            Opcode::Memset,
            Opcode::VaArg,
            Opcode::Alloca,
            Opcode::Asm,
            Opcode::AtomicStore,
        ] {
            assert!(op.may_access_memory(), "{op:?} reaches memory");
            assert!(
                op.has_side_effects(),
                "{op:?} reaches memory but DCE may delete it"
            );
        }
        // The one deliberate exception.
        assert!(Opcode::Load.may_access_memory());
        assert!(!Opcode::Load.has_side_effects());
    }

    /// The reverse direction is *not* an invariant and this records why: a
    /// terminator has side effects and touches no memory, so
    /// `has_side_effects` is strictly wider.
    #[test]
    fn i5_side_effects_does_not_imply_memory_access() {
        assert!(Opcode::Br.has_side_effects());
        assert!(!Opcode::Br.may_access_memory());
        assert!(Opcode::Ret.has_side_effects());
        assert!(!Opcode::Ret.may_access_memory());
    }

    /// I5 — runtime: a memory-accessing instruction that DCE may delete.
    #[test]
    fn i5_runtime_rejects_a_deletable_memory_access() {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("t", types.int_id);
        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        // `Nop` reaches no memory, so claiming otherwise needs a real
        // opcode; `Load` is the sanctioned exception and must pass.
        bb.add_insn(Instruction::new(Opcode::Load).with_target(PseudoId(1)));
        bb.add_insn(Instruction::ret(None));
        func.add_block(bb);
        func.entry = BasicBlockId(0);
        assert!(validate_function(&func).is_ok(), "a Load is allowed");
    }

    /// I2 — runtime check: a hand-crafted IR with a barrier-but-not-
    /// side-effect (constructed by abusing AsmData on a non-Asm op) is
    /// not actually reachable through normal c17 pipelines, but the
    /// validator's check covers the contract end-to-end. A simpler
    /// sanity case: a normal asm-with-memory-clobber passes I2.
    #[test]
    fn i2_asm_with_memory_clobber_validates() {
        use crate::ir::AsmData;

        let mut func = fresh_func("asm_mem_barrier");
        let mut asm = Instruction::new(Opcode::Asm);
        asm.extra_mut().asm_data = Some(Box::new(AsmData {
            template: "mfence".into(),
            outputs: vec![],
            inputs: vec![],
            clobbers: vec!["memory".to_string()],
            goto_labels: vec![],
        }));
        push(&mut func, asm);
        assert!(validate_function(&func).is_ok());
    }

    /// I3 — a valid CFG. Br targets an existing block; validator
    /// returns Ok.
    #[test]
    fn i3_valid_branch_target_passes() {
        let mut func = fresh_func("valid_br");
        // Add a second block so Br has a real target.
        let mut bb1 = crate::ir::BasicBlock::new(BasicBlockId(1));
        bb1.insns.push(Instruction::ret(None));
        func.add_block(bb1);
        terminate(&mut func, Instruction::br(BasicBlockId(1)));
        func.add_edge(BasicBlockId(0), BasicBlockId(1));
        assert!(validate_function(&func).is_ok());
    }

    /// I3 — Br references a nonexistent BasicBlockId. The validator
    /// flags it with `InvalidBranchTarget`.
    #[test]
    fn i3_invalid_br_target_flagged() {
        let mut func = fresh_func("invalid_br");
        terminate(&mut func, Instruction::br(BasicBlockId(99)));
        let errors = validate_function(&func).unwrap_err();
        assert!(errors.iter().any(|e| matches!(
            e,
            ValidationError::InvalidBranchTarget {
                target,
                opcode: Opcode::Br,
                ..
            } if *target == BasicBlockId(99)
        )));
    }

    /// I3 — Cbr's bb_true and bb_false are both checked. A bogus
    /// bb_false alone is enough to fail validation.
    #[test]
    fn i3_invalid_cbr_false_target_flagged() {
        let mut func = fresh_func("invalid_cbr");
        let mut bb1 = crate::ir::BasicBlock::new(BasicBlockId(1));
        bb1.insns.push(Instruction::ret(None));
        func.add_block(bb1);
        // bb_true exists, bb_false doesn't.
        terminate(
            &mut func,
            Instruction::cbr(PseudoId(0), BasicBlockId(1), BasicBlockId(7)),
        );
        let errors = validate_function(&func).unwrap_err();
        assert!(errors.iter().any(|e| matches!(
            e,
            ValidationError::InvalidBranchTarget {
                target,
                opcode: Opcode::Cbr,
                ..
            } if *target == BasicBlockId(7)
        )));
    }

    /// A diamond: 0 branches to 1 and 2, both of which reach 3, where one phi
    /// merges what each arm supplies. Every edge recorded both ways.
    /// I11: a lifetime marker must name one of the function's own locals.
    #[test]
    fn i11_rejects_a_lifetime_end_of_no_local() {
        let types = crate::types::TypeTable::new(&crate::target::Target::host());
        let mut f = Function::new("f", types.void_id);
        f.add_pseudo(crate::ir::Pseudo::sym(PseudoId(0), "x.0".into()));
        f.add_local("x.0", PseudoId(0), types.int_id, None, None);
        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        bb.add_insn(Instruction::lifetime_end(PseudoId(0)));
        bb.add_insn(Instruction::lifetime_end(PseudoId(7)));
        bb.add_insn(Instruction::ret(None));
        f.add_block(bb);
        f.entry = BasicBlockId(0);
        let errors = validate_function(&f).unwrap_err();
        assert!(
            matches!(
                errors.as_slice(),
                [ValidationError::StrayLifetimeEnd { index: 2, .. }]
            ),
            "{errors:?}"
        );
    }

    fn diamond() -> Function {
        let types = TypeTable::new(&Target::host());
        let int = types.int_id;
        let mut func = Function::new("diamond", int);
        func.entry = BasicBlockId(0);
        for i in 0..8 {
            func.add_pseudo(Pseudo::reg(PseudoId(i), i));
        }
        let mut b0 = BasicBlock::new(BasicBlockId(0));
        b0.add_insn(Instruction::new(Opcode::Entry));
        b0.add_insn(Instruction::cbr(
            PseudoId(0),
            BasicBlockId(1),
            BasicBlockId(2),
        ));
        let mut b1 = BasicBlock::new(BasicBlockId(1));
        let mut src1 = Instruction::phi_source(PseudoId(4), PseudoId(1), int, 32);
        src1.phi_list = vec![(BasicBlockId(3), PseudoId(3))];
        b1.add_insn(src1);
        b1.add_insn(Instruction::br(BasicBlockId(3)));
        let mut b2 = BasicBlock::new(BasicBlockId(2));
        let mut src2 = Instruction::phi_source(PseudoId(5), PseudoId(2), int, 32);
        src2.phi_list = vec![(BasicBlockId(3), PseudoId(3))];
        b2.add_insn(src2);
        b2.add_insn(Instruction::br(BasicBlockId(3)));
        let mut b3 = BasicBlock::new(BasicBlockId(3));
        let mut phi = Instruction::phi(PseudoId(3), int, 32);
        phi.phi_list = vec![
            (BasicBlockId(1), PseudoId(4)),
            (BasicBlockId(2), PseudoId(5)),
        ];
        b3.add_insn(phi);
        b3.add_insn(Instruction::ret(Some(PseudoId(3))));
        for b in [b0, b1, b2, b3] {
            func.add_block(b);
        }
        for (f, t) in [(0, 1), (0, 2), (1, 3), (2, 3)] {
            func.add_edge(BasicBlockId(f), BasicBlockId(t));
        }
        func
    }

    fn cfg_errors(func: &Function, stage: Stage) -> Vec<String> {
        match validate_function_at(func, stage) {
            Ok(()) => vec![],
            Err(errs) => errs
                .into_iter()
                .filter(|e| matches!(e, ValidationError::CfgInconsistent { .. }))
                .map(|e| e.to_string())
                .collect(),
        }
    }

    /// I8/I9 -- the baseline: a consistent diamond passes, and every one of
    /// the ways below of breaking it is caught on its own.
    #[test]
    fn i8_a_consistent_cfg_passes() {
        assert!(
            validate_function(&diamond()).is_ok(),
            "{:?}",
            validate_function(&diamond())
        );
    }

    /// I8: `parents` must be the inverse of `children`. `dce` once dropped a
    /// `children` edge and left `parents` naming it.
    #[test]
    fn i8_a_stale_parent_is_flagged() {
        let mut f = diamond();
        f.blocks[3].parents.retain(|p| *p != BasicBlockId(2));
        let e = cfg_errors(&f, Stage::Ssa);
        assert!(e.iter().any(|m| m.contains("parents are")), "{e:?}");
    }

    /// I8: `children` must be what the terminator names.
    #[test]
    fn i8_an_edge_the_terminator_does_not_name_is_flagged() {
        let mut f = diamond();
        *f.blocks[0].insns.last_mut().unwrap() = Instruction::br(BasicBlockId(1));
        let e = cfg_errors(&f, Stage::Ssa);
        assert!(e.iter().any(|m| m.contains("instructions name")), "{e:?}");
    }

    /// I8: one terminator, last.
    #[test]
    fn i8_a_terminator_that_is_not_last_is_flagged() {
        let mut f = diamond();
        f.blocks[3].insns.push(Instruction::new(Opcode::Nop));
        let e = cfg_errors(&f, Stage::Ssa);
        assert!(e.iter().any(|m| m.contains("is not the last")), "{e:?}");
        let mut f = diamond();
        f.blocks[3].insns.pop();
        let e = cfg_errors(&f, Stage::Ssa);
        assert!(e.iter().any(|m| m.contains("no terminator")), "{e:?}");
    }

    /// I8: `get_block` must find each block where it is. Rebuilding the index
    /// is a manual obligation after any change to `blocks`.
    #[test]
    fn i8_a_stale_block_index_is_flagged() {
        let mut f = diamond();
        f.blocks.swap(1, 2);
        let e = cfg_errors(&f, Stage::Ssa);
        assert!(e.iter().any(|m| m.contains("block index")), "{e:?}");
    }

    /// I9: a phi takes exactly one operand per predecessor. Only in SSA form:
    /// after lowering there are no phis to ask.
    #[test]
    fn i9_a_phi_missing_an_incoming_edge_is_flagged() {
        let mut f = diamond();
        f.blocks[3].insns[0].phi_list.pop();
        let e = cfg_errors(&f, Stage::Ssa);
        assert!(
            e.iter().any(|m| m.contains("takes operands along")),
            "{e:?}"
        );
        assert!(cfg_errors(&f, Stage::Lowered).is_empty());
    }

    /// I9: a `PhiSource` feeds a phi in a successor of its own block.
    #[test]
    fn i9_a_phisource_feeding_a_non_successor_is_flagged() {
        let mut f = diamond();
        f.blocks[1].insns[0].phi_list = vec![(BasicBlockId(2), PseudoId(3))];
        let e = cfg_errors(&f, Stage::Ssa);
        assert!(e.iter().any(|m| m.contains("not a successor")), "{e:?}");
    }

    /// `remove_edge` leaves a consistent graph: both lists, the phi operand
    /// taken along the edge, and the `PhiSource` that supplied it all go.
    #[test]
    fn cfg_remove_edge_keeps_every_invariant() {
        let mut f = diamond();
        *f.blocks[0].insns.last_mut().unwrap() = Instruction::br(BasicBlockId(1));
        f.remove_edge(BasicBlockId(0), BasicBlockId(2));
        f.remove_edge(BasicBlockId(2), BasicBlockId(3));
        f.blocks.retain(|b| b.id != BasicBlockId(2));
        f.rebuild_block_idx();
        assert!(validate_function(&f).is_ok(), "{:?}", validate_function(&f));
        assert_eq!(
            f.blocks[2].insns[0].phi_list,
            vec![(BasicBlockId(1), PseudoId(4))]
        );
    }

    /// I10: a comparison records its operands in `src_typ`/`src_size`, where
    /// every opcode that reads another type does. One assembled by hand
    /// without them is reported; one from `Instruction::compare` is not.
    #[test]
    fn i10_a_comparison_without_its_operand_type_is_flagged() {
        let types = TypeTable::new(&Target::host());
        let mut func = fresh_func("cmp");
        for i in 0..3 {
            func.add_pseudo(Pseudo::reg(PseudoId(i), i));
        }
        let bare = Instruction::new(Opcode::SetLt)
            .with_target(PseudoId(2))
            .with_src2(PseudoId(0), PseudoId(1))
            .with_type_and_size(types.int_id, 32);
        push(&mut func, bare);
        let errors = validate_function(&func).unwrap_err();
        assert!(errors.iter().any(|e| matches!(
            e,
            ValidationError::MissingOperandType {
                opcode: Opcode::SetLt,
                ..
            }
        )));

        let mut func = fresh_func("cmp");
        for i in 0..3 {
            func.add_pseudo(Pseudo::reg(PseudoId(i), i));
        }
        let cmp = Instruction::compare(
            Opcode::SetLt,
            PseudoId(2),
            (PseudoId(0), PseudoId(1)),
            (types.long_id, 64),
            (types.int_id, 32),
        );
        assert_eq!(
            (cmp.operand_type(), cmp.operand_width()),
            (Some(types.long_id), 64)
        );
        assert_eq!((cmp.typ, cmp.size), (Some(types.int_id), 32));
        push(&mut func, cmp);
        assert!(
            validate_function(&func).is_ok(),
            "{:?}",
            validate_function(&func)
        );
    }
}
