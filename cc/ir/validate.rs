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
//   I2 — A MEMORY BARRIER IS A DCE ROOT
//        A property of the opcode table, not of any program; see below.
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
//   I4 — NO PLACEHOLDER SURVIVES LOWERING
//        `ConstantP` and `VaArgPackLen` stand for a value something
//        downstream resolves. Neither backend knows them, and both end their
//        opcode match in a catch-all, so one left in an emitted function
//        would be dropped in silence and its target left undefined. Checked
//        after lowering only; a function codegen skips is exempt.
//
//   I5 — A MEMORY ACCESS OTHER THAN A `Load` HAS SIDE EFFECTS
//        A property of the opcode table, not of any program; see below.
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
//   I8 — THE CFG CACHE AGREES WITH THE INSTRUCTIONS
//        Every block ends in exactly one terminator; `children` is exactly the
//        successors its instructions name (`BasicBlock::named_successors`, or
//        address-taken blocks for a computed `goto`); `parents` is exactly the
//        inverse of `children`; `get_block` finds every block where it is.
//
//   I9 — PHIS AGREE WITH THE EDGES
//        Every phi takes one operand along each predecessor edge and no
//        other; the operand taken along the edge from B is the target of a
//        `PhiSource` in B; and every `PhiSource` feeds a phi in a successor
//        of its own block. Only while the IR is in SSA form; lowering
//        removes both.
//
//   I10 — AN OPERAND OF ANOTHER TYPE IS RECORDED AS ONE
//        `typ`/`size` describe an instruction's result, for every opcode; a
//        conversion, comparison, bit count or `Signbit`, which reads another
//        type than it produces (`Opcode::reads_another_type`), records its
//        operands in `src_typ`/`src_size` (`Instruction::operand_type`).
//        Built without them, a backend would compare at the result's width,
//        and a fold would have to refuse or guess: the result's width is the
//        wrong answer for every conversion, since an extension read at it is
//        the identity and a negative `char` comes back positive.
//
//   I11 — A LIFETIME MARKER NAMES A LOCAL OF ITS OWN FUNCTION
//        `LifetimeEnd` names its local out of band, where no pass that
//        rewrites operands looks. A local dropped or renamed without its
//        markers would leave the allocator ending the lifetime of nothing,
//        or of another function's object.
//
//   I12 — AN INTEGER WIDTH CHANGE CHANGES THE WIDTH ITS OPCODE SAYS
//        A `Sext` or `Zext` reads an operand narrower than its result and a
//        `Trunc` one wider, to a result of at least one bit
//        (`Opcode::is_int_width_change`). A same-width integer conversion is
//        no instruction at all, so the folds may read an extension's operand
//        at `src_size` and a truncation's at `size` without asking which of
//        the two is the narrower.
//
//   I13 — EVERY PSEUDO ID IS BELOW THE ALLOCATOR
//        `Function::alloc_pseudo` hands out `next_pseudo` and counts up, so
//        every id already in the function -- registered in `pseudos`, a
//        target, an operand -- must be below it. One at or past it is an id
//        the next allocation hands out again, giving a fresh value the name
//        of an existing one.
//
// I2 and I5 hold for every program or for none: `Opcode::has_side_effects`
// is derived from `may_access_memory`, and unit tests in `ir/mod.rs` check
// both over `Opcode::ALL`. The validator checks the other eleven.
//
// The validator runs always, in every build, through [`verify`]: after
// linearization, after optimization, and after lowering. It is one walk over
// the instructions, checking each where it stands, plus what can only be
// answered once the walk is done: a pseudo defined twice, the parent lists,
// and the `PhiSource` behind each phi operand. A structural error it catches
// is a miscompile it would otherwise have shipped -- `cargo test --release`,
// the torture harness and a released `c17` all run without debug assertions,
// which is where it used to live, so it ran in none of them.

use super::{BasicBlock, BasicBlockId, Function, Instruction, Module, Opcode, PseudoId};
use std::collections::{HashMap, HashSet};
use std::fmt;

// Error model

/// A single invariant violation: the function, where in it, and what is
/// wrong. Carries enough context for a developer inspecting an IR dump or
/// stepping through with a debugger to find the offending site.
#[derive(Debug, Clone)]
pub struct ValidationError {
    pub function: String,
    pub at: Location,
    pub kind: Invariant,
}

/// Where in a function a violation is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Location {
    /// The function as a whole.
    Function,
    /// A block.
    Block(BasicBlockId),
    /// An instruction: its block, its position there, and its opcode.
    Insn {
        block: BasicBlockId,
        index: usize,
        opcode: Opcode,
    },
}

/// Which invariant is broken, with what the check found.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Invariant {
    /// I1: the same SSA pseudo is the target of more than one instruction;
    /// `sites` lists every one, so it has two or more entries.
    MultipleDefinitions {
        pseudo: PseudoId,
        sites: Vec<Location>,
    },
    /// I3: a branch-style instruction names a block not in `func.blocks`.
    InvalidBranchTarget { target: BasicBlockId },
    /// I4: a placeholder opcode is still here after lowering.
    UnresolvedPlaceholder,
    /// I6: a memory access carries an offset no signed 32-bit displacement
    /// holds.
    DisplacementOutOfRange { offset: i64 },
    /// I7: `get_pseudo` does not find this pseudo at its own position in
    /// `pseudos`.
    StalePseudoIndex { pseudo: PseudoId },
    /// I8: the CFG cache disagrees with the instructions or with itself.
    CfgInconsistent { what: String },
    /// I9: a phi's operands are not taken along exactly the predecessor
    /// edges, one each.
    PhiEdges {
        along: Vec<BasicBlockId>,
        preds: Vec<BasicBlockId>,
    },
    /// I9: a phi's operand along the edge from `pred` is not the target of a
    /// `PhiSource` in `pred`.
    PhiOperandNotFromPred {
        pred: BasicBlockId,
        operand: PseudoId,
    },
    /// I9: a `PhiSource` feeds no phi in a successor of its own block.
    PhiSourceNotToSuccessor { feeds: Option<BasicBlockId> },
    /// I10: an opcode that reads another type with no operand type or
    /// width.
    MissingOperandType,
    /// I11: a `LifetimeEnd` naming no local of this function.
    StrayLifetimeEnd,
    /// I12: an integer width change from `from` bits to `to` that does not
    /// change the width the way its opcode says.
    WidthChangeBackwards { from: u32, to: u32 },
    /// I13: `pseudo`, the highest id in the function, is not below
    /// `next_pseudo`.
    PseudoPastAllocator { pseudo: PseudoId, next_pseudo: u32 },
}

impl Invariant {
    /// The invariant's number in the index at the top of this file.
    fn tag(&self) -> &'static str {
        match self {
            Invariant::MultipleDefinitions { .. } => "I1",
            Invariant::InvalidBranchTarget { .. } => "I3",
            Invariant::UnresolvedPlaceholder => "I4",
            Invariant::DisplacementOutOfRange { .. } => "I6",
            Invariant::StalePseudoIndex { .. } => "I7",
            Invariant::CfgInconsistent { .. } => "I8",
            Invariant::PhiEdges { .. }
            | Invariant::PhiOperandNotFromPred { .. }
            | Invariant::PhiSourceNotToSuccessor { .. } => "I9",
            Invariant::MissingOperandType => "I10",
            Invariant::StrayLifetimeEnd => "I11",
            Invariant::WidthChangeBackwards { .. } => "I12",
            Invariant::PseudoPastAllocator { .. } => "I13",
        }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Location::Function => write!(f, "function"),
            Location::Block(block) => write!(f, "{block}"),
            Location::Insn {
                block,
                index,
                opcode,
            } => write!(f, "{block} insn={index} op={opcode:?}"),
        }
    }
}

impl fmt::Display for Invariant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Invariant::MultipleDefinitions { pseudo, sites } => {
                write!(f, "pseudo {pseudo} has {} definitions: ", sites.len())?;
                for (i, site) in sites.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{site}")?;
                }
                Ok(())
            }
            Invariant::InvalidBranchTarget { target } => {
                write!(f, "references unknown BasicBlockId {target:?}")
            }
            Invariant::UnresolvedPlaceholder => write!(
                f,
                "is a placeholder that should have been resolved before codegen"
            ),
            Invariant::DisplacementOutOfRange { offset } => write!(
                f,
                "memory access offset {offset} does not fit a 32-bit displacement"
            ),
            Invariant::StalePseudoIndex { pseudo } => write!(
                f,
                "pseudo {pseudo:?} is not found at its own position; `pseudo_idx` is stale"
            ),
            Invariant::CfgInconsistent { what } => write!(f, "{what}"),
            Invariant::PhiEdges { along, preds } => write!(
                f,
                "phi takes operands along {} but the predecessors are {}",
                ids(along),
                ids(preds)
            ),
            Invariant::PhiOperandNotFromPred { pred, operand } => write!(
                f,
                "phi operand {operand} along the edge from {pred} is not a PhiSource in {pred}"
            ),
            Invariant::PhiSourceNotToSuccessor { feeds } => {
                write!(f, "phisrc feeds {feeds:?}, not a successor")
            }
            Invariant::MissingOperandType => write!(f, "records no operand type or width"),
            Invariant::StrayLifetimeEnd => {
                write!(f, "ends the lifetime of no local of this function")
            }
            Invariant::WidthChangeBackwards { from, to } => {
                write!(f, "converts {from} bits to {to}, against its opcode")
            }
            Invariant::PseudoPastAllocator {
                pseudo,
                next_pseudo,
            } => write!(
                f,
                "pseudo {pseudo:?} is not below next_pseudo {next_pseudo}; \
                 the allocator will hand it out again"
            ),
        }
    }
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[ir-validate {}] in function `{}`",
            self.kind.tag(),
            self.function
        )?;
        if self.at != Location::Function {
            write!(f, " at {}", self.at)?;
        }
        write!(f, ": {}", self.kind)
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

/// Validate a single function in SSA form, for callers holding hand-built IR
/// rather than a whole Module.
pub fn validate_function(func: &Function) -> Result<(), Vec<ValidationError>> {
    validate_function_at(func, Stage::Ssa)
}

/// Validate a single function at `stage`: one walk over its blocks and
/// instructions, then the checks that need the whole walk's findings.
pub fn validate_function_at(func: &Function, stage: Stage) -> Result<(), Vec<ValidationError>> {
    let mut walk = Walk::new(func, stage);
    for (idx, bb) in func.blocks.iter().enumerate() {
        walk.check_block(idx, bb);
    }
    walk.finish()
}

/// I7 — `get_pseudo` finds every pseudo at its own position.
pub fn check_pseudo_index(func: &Function, out: &mut Vec<ValidationError>) {
    for pseudo in &func.pseudos {
        if func.get_pseudo(pseudo.id).map(|p| p.id) != Some(pseudo.id) {
            out.push(ValidationError {
                function: func.name.clone(),
                at: Location::Function,
                kind: Invariant::StalePseudoIndex { pseudo: pseudo.id },
            });
        }
    }
}

// The walk

/// What the walk carries from one instruction to the next, and from the
/// instructions to [`Walk::finish`].
struct Walk<'a> {
    func: &'a Function,
    stage: Stage,
    /// Every block id, for I3: a branch may name a block the walk has not
    /// reached yet.
    blocks: HashSet<BasicBlockId>,
    /// Block ids met so far, for I8's "used twice".
    seen: HashSet<BasicBlockId>,
    /// I1 and I9, in SSA form only: every definition site of every target.
    defs: HashMap<PseudoId, Vec<Location>>,
    /// I9: every phi, whose operands are checked against `defs` at the end.
    phis: Vec<(Location, &'a Instruction)>,
    /// I8: the parents each block should have, by its predecessors' children.
    expected_parents: HashMap<BasicBlockId, HashSet<BasicBlockId>>,
    /// I13: the highest pseudo id the instructions name.
    max_pseudo: Option<PseudoId>,
    out: Vec<ValidationError>,
}

/// What the walk knows about the block it is in.
struct BlockState {
    id: BasicBlockId,
    /// `parents` as a set, for I9.
    parents: HashSet<BasicBlockId>,
    /// `children` as a set, for I9.
    children: HashSet<BasicBlockId>,
    /// Every block the instructions name, for I8.
    named: HashSet<BasicBlockId>,
    /// Where the first terminator is, for I8.
    first_terminator: Option<usize>,
}

impl<'a> Walk<'a> {
    fn new(func: &'a Function, stage: Stage) -> Self {
        Walk {
            func,
            stage,
            blocks: func.blocks.iter().map(|b| b.id).collect(),
            seen: HashSet::new(),
            defs: HashMap::new(),
            phis: Vec::new(),
            expected_parents: HashMap::new(),
            max_pseudo: None,
            out: Vec::new(),
        }
    }

    fn report(&mut self, at: Location, kind: Invariant) {
        self.out.push(ValidationError {
            function: self.func.name.clone(),
            at,
            kind,
        });
    }

    fn cfg(&mut self, block: BasicBlockId, what: String) {
        self.report(Location::Block(block), Invariant::CfgInconsistent { what });
    }

    /// Check block `idx` and every instruction in it.
    fn check_block(&mut self, idx: usize, bb: &'a BasicBlock) {
        if !self.seen.insert(bb.id) {
            self.cfg(bb.id, "block id used twice".into());
        }
        if self.func.block_index(bb.id) != Some(idx) {
            self.cfg(
                bb.id,
                format!(
                    "block index says {:?}, block is at {idx}",
                    self.func.block_index(bb.id)
                ),
            );
        }

        let mut state = BlockState {
            id: bb.id,
            parents: bb.parents.iter().copied().collect(),
            children: bb.children.iter().copied().collect(),
            named: HashSet::new(),
            first_terminator: None,
        };
        for (index, insn) in bb.insns.iter().enumerate() {
            self.check_insn(&mut state, index, insn);
        }

        self.check_block_edges(bb, &state);
    }

    /// Every invariant that one instruction can break on its own, and the
    /// records the end-of-walk checks need from it.
    fn check_insn(&mut self, state: &mut BlockState, index: usize, insn: &'a Instruction) {
        let at = Location::Insn {
            block: state.id,
            index,
            opcode: insn.op,
        };

        // I1 and I9 -- recorded here, judged in `finish`.
        if self.stage == Stage::Ssa {
            if let Some(t) = insn.target {
                self.defs.entry(t).or_default().push(at);
            }
        }

        // I13 -- recorded here, judged in `finish`.
        let named = insn.target.into_iter().chain(insn.uses()).chain(
            insn.extra()
                .asm_data
                .iter()
                .flat_map(|asm| asm.outputs.iter().map(|o| o.pseudo)),
        );
        self.max_pseudo = named.chain(self.max_pseudo).max();

        // I3, and I8's record of the successors this block names.
        for target in insn.control_targets() {
            if !self.blocks.contains(&target) {
                self.report(at, Invariant::InvalidBranchTarget { target });
            }
            state.named.insert(target);
        }
        if insn.op.is_terminator() && state.first_terminator.is_none() {
            state.first_terminator = Some(index);
        }

        // I4
        if self.stage == Stage::Lowered
            && self.func.emit
            && matches!(insn.op, Opcode::ConstantP | Opcode::VaArgPackLen)
        {
            self.report(at, Invariant::UnresolvedPlaceholder);
        }

        // I6
        if insn.op.addresses_memory() && i32::try_from(insn.offset).is_err() {
            self.report(
                at,
                Invariant::DisplacementOutOfRange {
                    offset: insn.offset,
                },
            );
        }

        // I9
        if self.stage == Stage::Ssa {
            self.check_phi_edges(state, at, insn);
        }

        // I10
        if insn.op.reads_another_type() && (insn.src_typ.is_none() || insn.src_size == 0) {
            self.report(at, Invariant::MissingOperandType);
        }

        // I12, of a width change that records its source width at all
        if insn.op.is_int_width_change() && insn.src_size != 0 {
            let (from, to) = (insn.src_size, insn.size);
            let as_said = if insn.op == Opcode::Trunc {
                0 < to && to < from
            } else {
                from < to
            };
            if !as_said {
                self.report(at, Invariant::WidthChangeBackwards { from, to });
            }
        }

        // I11
        if insn.op == Opcode::LifetimeEnd
            && !insn
                .extra()
                .lifetime_of
                .is_some_and(|l| self.func.local_of(l).is_some())
        {
            self.report(at, Invariant::StrayLifetimeEnd);
        }
    }

    /// I9 -- a phi takes exactly one operand along each incoming edge, and a
    /// `PhiSource` feeds a phi in a successor of its own block.
    ///
    /// Phi elimination puts the copy for an edge at the end of its source, so
    /// a phi operand naming a block that is not a predecessor is a copy into
    /// nowhere, and a predecessor with no operand leaves the phi undefined
    /// along that edge.
    fn check_phi_edges(&mut self, state: &BlockState, at: Location, insn: &'a Instruction) {
        match insn.op {
            Opcode::Phi => {
                let along: Vec<BasicBlockId> = insn.phi_list.iter().map(|(p, _)| *p).collect();
                let set: HashSet<BasicBlockId> = along.iter().copied().collect();
                if set.len() != along.len() || set != state.parents {
                    self.report(
                        at,
                        Invariant::PhiEdges {
                            along,
                            preds: sorted(&state.parents),
                        },
                    );
                }
                self.phis.push((at, insn));
            }
            Opcode::PhiSource => {
                let feeds = insn.phi_source_dest().map(|(bb, _)| bb);
                if !feeds.is_some_and(|f| state.children.contains(&f)) {
                    self.report(at, Invariant::PhiSourceNotToSuccessor { feeds });
                }
            }
            _ => {}
        }
    }

    /// I8 -- the block's terminator, and its `children` against what its
    /// instructions name. A block ending in `IndirectBr` names none, and each
    /// of its successors must be a block whose address is taken.
    fn check_block_edges(&mut self, bb: &BasicBlock, state: &BlockState) {
        match state.first_terminator {
            None => self.cfg(bb.id, "no terminator".into()),
            Some(p) if p + 1 != bb.insns.len() => self.cfg(
                bb.id,
                format!("terminator at {p} is not the last instruction"),
            ),
            Some(_) => {}
        }

        if state.children.len() != bb.children.len() {
            self.cfg(bb.id, "an edge is listed twice in children".into());
        }
        if bb.ends_in_computed_goto() {
            for c in sorted(&state.children) {
                if !self.func.get_block(c).is_some_and(|b| b.addr_taken) {
                    self.cfg(
                        bb.id,
                        format!("computed goto reaches {c}, whose address is not taken"),
                    );
                }
            }
        } else if state.named != state.children {
            self.cfg(
                bb.id,
                format!(
                    "instructions name {} but children are {}",
                    ids(&sorted(&state.named)),
                    ids(&sorted(&state.children))
                ),
            );
        }

        for c in &bb.children {
            self.expected_parents.entry(*c).or_default().insert(bb.id);
        }
    }

    /// The checks that need the whole walk: I1, I7, I8's parent lists, I9's
    /// `PhiSource` behind each phi operand, and I13.
    fn finish(mut self) -> Result<(), Vec<ValidationError>> {
        // I1
        let mut multi: Vec<(PseudoId, Vec<Location>)> = self
            .defs
            .iter()
            .filter(|(_, sites)| sites.len() > 1)
            .map(|(p, sites)| (*p, sites.clone()))
            .collect();
        multi.sort_by_key(|(p, _)| p.0);
        for (pseudo, sites) in multi {
            self.report(
                Location::Function,
                Invariant::MultipleDefinitions { pseudo, sites },
            );
        }

        // I7
        check_pseudo_index(self.func, &mut self.out);

        // I13
        let registered = self.func.pseudos.iter().map(|p| p.id);
        if let Some(pseudo) = registered.chain(self.max_pseudo).max() {
            let next_pseudo = self.func.next_pseudo;
            if pseudo.0 >= next_pseudo {
                self.report(
                    Location::Function,
                    Invariant::PseudoPastAllocator {
                        pseudo,
                        next_pseudo,
                    },
                );
            }
        }

        // I8: `parents` is exactly the inverse of `children`.
        let func = self.func;
        for bb in &func.blocks {
            let have: HashSet<BasicBlockId> = bb.parents.iter().copied().collect();
            if have.len() != bb.parents.len() {
                self.cfg(bb.id, "an edge is listed twice in parents".into());
            }
            let want = self.expected_parents.remove(&bb.id).unwrap_or_default();
            if have != want {
                self.cfg(
                    bb.id,
                    format!(
                        "parents are {} but {} name it as a successor",
                        ids(&sorted(&have)),
                        ids(&sorted(&want))
                    ),
                );
            }
        }
        let mut orphans: Vec<_> = std::mem::take(&mut self.expected_parents)
            .into_iter()
            .collect();
        orphans.sort_by_key(|(b, _)| b.0);
        for (missing, from) in orphans {
            self.cfg(
                missing,
                format!(
                    "is a successor of {} but is not a block",
                    ids(&sorted(&from))
                ),
            );
        }

        // I9: the operand along the edge from B is a `PhiSource` in B, the
        // pairing phi elimination and if-conversion both read the edge's
        // value through.
        for (at, phi) in std::mem::take(&mut self.phis) {
            for &(pred, operand) in &phi.phi_list {
                let from_pred = self.defs.get(&operand).is_some_and(|sites| {
                    sites.iter().any(|s| {
                        matches!(s, Location::Insn { block, opcode: Opcode::PhiSource, .. }
                            if *block == pred)
                    })
                });
                if !from_pred {
                    self.report(at, Invariant::PhiOperandNotFromPred { pred, operand });
                }
            }
        }

        if self.out.is_empty() {
            Ok(())
        } else {
            Err(self.out)
        }
    }
}

fn sorted(s: &HashSet<BasicBlockId>) -> Vec<BasicBlockId> {
    let mut v: Vec<BasicBlockId> = s.iter().copied().collect();
    v.sort_unstable_by_key(|b| b.0);
    v
}

fn ids(v: &[BasicBlockId]) -> String {
    let names: Vec<String> = v.iter().map(|b| b.to_string()).collect();
    format!("[{}]", names.join(", "))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, Function, Instruction, Pseudo, PseudoId};
    use crate::target::Target;
    use crate::types::{TypeId, TypeTable};

    /// Above every id a test here names, which I13 asks of a function.
    const NEXT_PSEUDO: u32 = 100;

    fn fresh_func(name: &str) -> Function {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new(name, types.int_id);
        func.next_pseudo = NEXT_PSEUDO;
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
                [ValidationError {
                    kind: Invariant::UnresolvedPlaceholder,
                    ..
                }]
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
                assert_eq!(
                    result.unwrap_err()[0].kind,
                    Invariant::DisplacementOutOfRange { offset }
                );
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
        assert_eq!(errors[0].function, "two_arms");
        match &errors[0].kind {
            Invariant::MultipleDefinitions { pseudo, sites } => {
                assert_eq!(*pseudo, PseudoId(3));
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
                .any(|e| matches!(e.kind, Invariant::MultipleDefinitions { .. })),
            "{errors:?}"
        );
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
            ValidationError {
                at: Location::Insn { opcode: Opcode::Br, .. },
                kind: Invariant::InvalidBranchTarget { target },
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
            ValidationError {
                at: Location::Insn { opcode: Opcode::Cbr, .. },
                kind: Invariant::InvalidBranchTarget { target },
                ..
            } if *target == BasicBlockId(7)
        )));
    }

    /// I13: an id at or past `next_pseudo`, whether only an operand or
    /// registered in `pseudos`, is one the allocator will hand out again.
    #[test]
    fn i13_an_id_the_allocator_would_reissue_is_flagged() {
        let past = |func: &Function| match validate_function(func) {
            Ok(()) => None,
            Err(errors) => match errors.as_slice() {
                [ValidationError {
                    kind: Invariant::PseudoPastAllocator { pseudo, .. },
                    ..
                }] => Some(*pseudo),
                _ => panic!("{errors:?}"),
            },
        };

        let mut func = fresh_func("operand");
        push(&mut func, copy_insn(1, NEXT_PSEUDO - 1));
        assert_eq!(past(&func), None);
        func.next_pseudo -= 1;
        assert_eq!(past(&func), Some(PseudoId(NEXT_PSEUDO - 1)));

        let mut func = fresh_func("registered");
        func.add_pseudo(Pseudo::reg(PseudoId(NEXT_PSEUDO), NEXT_PSEUDO));
        assert_eq!(past(&func), Some(PseudoId(NEXT_PSEUDO)));
    }

    /// I11: a lifetime marker must name one of the function's own locals.
    #[test]
    fn i11_rejects_a_lifetime_end_of_no_local() {
        let types = crate::types::TypeTable::new(&crate::target::Target::host());
        let mut f = Function::new("f", types.void_id);
        f.next_pseudo = NEXT_PSEUDO;
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
                [ValidationError {
                    at: Location::Insn { index: 2, .. },
                    kind: Invariant::StrayLifetimeEnd,
                    ..
                }]
            ),
            "{errors:?}"
        );
    }

    /// A diamond: 0 branches to 1 and 2, both of which reach 3, where one phi
    /// merges what each arm supplies. Every edge recorded both ways.
    fn diamond() -> Function {
        let types = TypeTable::new(&Target::host());
        let int = types.int_id;
        let mut func = Function::new("diamond", int);
        func.next_pseudo = NEXT_PSEUDO;
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
                .filter(|e| matches!(e.kind.tag(), "I8" | "I9"))
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

    /// I9: the operand a phi takes along the edge from B is a `PhiSource` in
    /// B. Swapping the diamond's two operands keeps every edge covered, so
    /// only this rule sees it; an operand some other instruction in the right
    /// block defines, or one nothing defines, is no better.
    #[test]
    fn i9_a_phi_operand_from_no_phisource_in_its_pred_is_flagged() {
        let not_from_pred = |f: &Function| -> Vec<(BasicBlockId, PseudoId)> {
            validate_function(f)
                .err()
                .unwrap_or_default()
                .into_iter()
                .filter_map(|e| match e.kind {
                    Invariant::PhiOperandNotFromPred { pred, operand } => Some((pred, operand)),
                    _ => None,
                })
                .collect()
        };
        assert!(not_from_pred(&diamond()).is_empty());

        let mut f = diamond();
        f.blocks[3].insns[0].phi_list = vec![
            (BasicBlockId(1), PseudoId(5)),
            (BasicBlockId(2), PseudoId(4)),
        ];
        assert_eq!(
            not_from_pred(&f),
            vec![
                (BasicBlockId(1), PseudoId(5)),
                (BasicBlockId(2), PseudoId(4))
            ]
        );

        let mut f = diamond();
        f.blocks[2].insns.insert(0, copy_insn(6, 2));
        f.blocks[3].insns[0].phi_list[1].1 = PseudoId(6);
        assert_eq!(not_from_pred(&f), vec![(BasicBlockId(2), PseudoId(6))]);

        let mut f = diamond();
        f.blocks[3].insns[0].phi_list[1].1 = PseudoId(7);
        let errors = validate_function(&f).unwrap_err();
        assert!(
            errors.iter().any(|e| e.to_string().starts_with(
                "[ir-validate I9] in function `diamond` at .L3 insn=0 op=Phi: \
                 phi operand %7 along the edge from .L2 is not a PhiSource in .L2"
            )),
            "{errors:?}"
        );
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
            ValidationError {
                at: Location::Insn {
                    opcode: Opcode::SetLt,
                    ..
                },
                kind: Invariant::MissingOperandType,
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

    /// The invariants one hand-built instruction breaks, by kind.
    fn kinds_of(insn: Instruction) -> Vec<Invariant> {
        let mut func = fresh_func("one");
        for i in 0..2 {
            func.add_pseudo(Pseudo::reg(PseudoId(i), i));
        }
        push(&mut func, insn);
        match validate_function(&func) {
            Ok(()) => Vec::new(),
            Err(errors) => errors.into_iter().map(|e| e.kind).collect(),
        }
    }

    /// `op` of pseudo 0 into pseudo 1, from `from` at `from_size` bits to
    /// `to` at `size`.
    fn conversion(
        op: Opcode,
        (from, from_size): (TypeId, u32),
        (to, size): (TypeId, u32),
    ) -> Instruction {
        let mut insn = Instruction::unop(op, PseudoId(1), PseudoId(0), to, size);
        insn.src_typ = Some(from);
        insn.src_size = from_size;
        insn
    }

    /// I10 covers every opcode that reads another type, the conversions and
    /// `Signbit` as much as the comparisons: built without its source width,
    /// each is reported, and with one it is not.
    #[test]
    fn i10_a_conversion_without_its_source_width_is_flagged() {
        let t = TypeTable::new(&Target::host());
        let cases = [
            (Opcode::Sext, (t.schar_id, 8), (t.int_id, 32)),
            (Opcode::Zext, (t.uint_id, 32), (t.ulong_id, 64)),
            (Opcode::Trunc, (t.long_id, 64), (t.short_id, 16)),
            (Opcode::FCvtS, (t.double_id, 64), (t.int_id, 32)),
            (Opcode::FCvtU, (t.float_id, 32), (t.ulong_id, 64)),
            (Opcode::SCvtF, (t.int_id, 32), (t.double_id, 64)),
            (Opcode::UCvtF, (t.ulong_id, 64), (t.float_id, 32)),
            (Opcode::FCvtF, (t.double_id, 64), (t.float_id, 32)),
            (Opcode::Signbit, (t.double_id, 64), (t.int_id, 32)),
        ];
        for (op, from, to) in cases {
            assert!(op.reads_another_type(), "{op:?}");
            assert_eq!(kinds_of(conversion(op, from, to)), vec![], "{op:?}");
            let mut bare = conversion(op, from, to);
            bare.src_size = 0;
            assert_eq!(
                kinds_of(bare),
                vec![Invariant::MissingOperandType],
                "{op:?}"
            );
            let mut bare = conversion(op, from, to);
            bare.src_typ = None;
            assert_eq!(
                kinds_of(bare),
                vec![Invariant::MissingOperandType],
                "{op:?}"
            );
        }
    }

    /// I12: an extension widens and a truncation narrows. The reverse, and
    /// a conversion to the width it started at, are each reported.
    #[test]
    fn i12_an_integer_width_change_goes_the_way_its_opcode_says() {
        let t = TypeTable::new(&Target::host());
        let (char8, int32, long64) = ((t.schar_id, 8), (t.int_id, 32), (t.long_id, 64));
        for (op, from, to) in [
            (Opcode::Sext, char8, int32),
            (Opcode::Zext, int32, long64),
            (Opcode::Trunc, long64, char8),
        ] {
            assert_eq!(kinds_of(conversion(op, from, to)), vec![], "{op:?}");
        }
        for (op, from, to) in [
            (Opcode::Sext, long64, int32),
            (Opcode::Zext, int32, int32),
            (Opcode::Trunc, char8, int32),
            (Opcode::Trunc, int32, int32),
            (Opcode::Trunc, int32, (t.bool_id, 0)),
        ] {
            assert_eq!(
                kinds_of(conversion(op, from, to)),
                vec![Invariant::WidthChangeBackwards {
                    from: from.1,
                    to: to.1
                }],
                "{op:?} {from:?} -> {to:?}"
            );
        }
    }
}
