//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// IR validator for pcc C99 compiler
//
// The validator enforces structural invariants the IR is required to
// satisfy at well-defined pipeline points (between optimization passes
// but BEFORE `lower.rs`, which intentionally introduces multi-def Copies
// as part of φ-elimination).
//
// Current invariants (M0 skeleton — later milestones extend):
//
//   I1 — SINGLE-DEF SSA TARGETS
//        Every `PseudoId` that appears as an instruction's `target` must
//        appear as such at most once across the function. SSA single-def is
//        what pseudo-merging passes (copyprop, CSE, GVN, SCCP) rely on; the
//        inliner's prior multi-def Copy pattern for return-value
//        materialization (each cloned `ret` writes the shared
//        `%ret_target`) is what M0 replaced with a Phi join.
//
//        Inline-asm output operands are intentionally excluded from I1 at
//        M0: the linearizer emits matched/in-out constraints (`"+r"(x)`,
//        `"0"(x)`) with one pseudo serving as both the load result and the
//        asm output. Later milestones (M2 "Sym vs pointer pseudo", M3
//        mem2reg) clean up that shape; I1 will extend to cover it then.
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
// The validator is intended to run only in debug builds — production
// builds skip it for zero overhead. Call via:
//
//     #[cfg(debug_assertions)]
//     validate::validate_module(&module).unwrap();
//
// Each new milestone documents the invariant it adds here, and extends
// `validate_function` with a corresponding check.

use super::{Function, Module, Opcode, PseudoId};
use std::collections::HashMap;
use std::fmt;

// ============================================================================
// Error model
// ============================================================================

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
        }
    }
}

impl std::error::Error for ValidationError {}

// ============================================================================
// Entry points
// ============================================================================

/// Validate every function in a module. Returns the full list of violations
/// (across all functions) when something is wrong, or `Ok(())` otherwise.
///
/// Validation is intentionally non-fatal here so that debug-build callers
/// can choose to log and continue (useful while a milestone is in flight).
/// Production builds skip the call entirely via `cfg!(debug_assertions)`.
pub fn validate_module(module: &Module) -> Result<(), Vec<ValidationError>> {
    let mut errors = Vec::new();
    for func in &module.functions {
        if let Err(mut errs) = validate_function(func) {
            errors.append(&mut errs);
        }
    }
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Validate a single function. Used by milestone tests that want to
/// exercise the validator on hand-built IR without going through a Module.
pub fn validate_function(func: &Function) -> Result<(), Vec<ValidationError>> {
    let mut errors = Vec::new();
    check_single_def(func, &mut errors);
    check_barrier_implies_side_effect(func, &mut errors);
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

// ============================================================================
// Invariant checks
// ============================================================================

/// I1 — every SSA target pseudo is defined exactly once.
///
/// "Target" here means `insn.target` — ordinary single-result IR
/// instructions (Copy, arithmetic, Load, Phi, PhiSource, Call, ...).
///
/// We do NOT include `insn.asm_data.outputs[i].pseudo` here. Inline asm
/// matched/in-out constraints (`"+r"(x)`, `"0"(x)`) are intentionally
/// emitted by the linearizer with the same pseudo serving as both the
/// load result and the asm's output — a documented non-SSA shape that
/// later milestones (M2 "Sym vs pointer pseudo" and M3 mem2reg) clean
/// up. Including asm outputs here would flag every matched-constraint
/// asm in the existing test suite, which is out of M0's scope.
///
/// We also do NOT count phi-source back-pointers
/// (`PhiSource.phi_list[i].1`) — those reference the destination Phi's
/// target, not new definitions.
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

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, Function, Instruction, Pseudo};
    use crate::target::Target;
    use crate::types::TypeTable;

    fn fresh_func(name: &str) -> Function {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new(name, types.int_id);
        func.entry = BasicBlockId(0);
        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.insns.push(Instruction::new(Opcode::Entry));
        func.add_block(bb);
        func
    }

    fn push(func: &mut Function, insn: Instruction) {
        func.blocks[0].insns.push(insn);
    }

    fn copy_insn(dst: u32, src: u32) -> Instruction {
        let mut i = Instruction::new(Opcode::Copy);
        i.target = Some(PseudoId(dst));
        i.src = vec![PseudoId(src)];
        i
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

    /// I1 violation: two `Copy` instructions share a target — the historical
    /// inliner-multi-def-return pattern before M0.
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

    /// Scope check: inline-asm outputs are NOT counted as SSA defs at M0.
    /// The linearizer emits matched/in-out constraints (`"+r"(x)`, `"0"(x)`)
    /// with the load result and the asm output sharing one pseudo — a
    /// documented non-SSA shape. Later milestones (M2, M3) clean this up;
    /// for M0 the validator must not flag the shape.
    #[test]
    fn validate_does_not_flag_asm_outputs() {
        use crate::ir::{AsmConstraint, AsmData};

        let mut func = fresh_func("asm_scope");
        for i in 0..=2 {
            func.add_pseudo(Pseudo::reg(PseudoId(i), i));
        }
        // `%2 = copy %0`
        push(&mut func, copy_insn(2, 0));
        // Asm whose output also writes %2. Pre-M2 linearizer pattern.
        let mut asm = Instruction::new(Opcode::Asm);
        asm.asm_data = Some(Box::new(AsmData {
            template: "movl $1, %0".into(),
            outputs: vec![AsmConstraint {
                pseudo: PseudoId(2),
                name: None,
                matching_output: None,
                constraint: "=r".into(),
                size: 32,
            }],
            inputs: vec![],
            clobbers: vec![],
            goto_labels: vec![],
        }));
        push(&mut func, asm);

        // M0 scope: asm outputs ignored; no error.
        assert!(validate_function(&func).is_ok());
    }

    /// `PhiSource.phi_list` carries a back-pointer to the destination Phi's
    /// target pseudo, NOT a definition. It must not be counted by the
    /// single-def check, otherwise legitimate phi-join IR (M0 output) would
    /// fail validation.
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

        assert!(validate_function(&func).is_ok());
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
        asm.asm_data = Some(Box::new(AsmData {
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

    /// I2 — runtime check: a hand-crafted IR with a barrier-but-not-
    /// side-effect (constructed by abusing AsmData on a non-Asm op) is
    /// not actually reachable through normal pcc pipelines, but the
    /// validator's check covers the contract end-to-end. A simpler
    /// sanity case: a normal asm-with-memory-clobber passes I2.
    #[test]
    fn i2_asm_with_memory_clobber_validates() {
        use crate::ir::AsmData;

        let mut func = fresh_func("asm_mem_barrier");
        let mut asm = Instruction::new(Opcode::Asm);
        asm.asm_data = Some(Box::new(AsmData {
            template: "mfence".into(),
            outputs: vec![],
            inputs: vec![],
            clobbers: vec!["memory".to_string()],
            goto_labels: vec![],
        }));
        push(&mut func, asm);
        assert!(validate_function(&func).is_ok());
    }
}
