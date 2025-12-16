//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Common register allocator utilities shared between architectures
//

use crate::ir::{Function, Opcode, PseudoId, PseudoKind};
use std::collections::HashSet;
use std::hash::Hash;

// ============================================================================
// Register Traits
// ============================================================================

/// Trait for general-purpose registers.
/// Implemented by architecture-specific register enums to enable generic algorithms.
pub trait GpReg: Copy + Eq + Hash + std::fmt::Debug {
    /// Register name (64-bit form)
    fn name(&self) -> &'static str;

    /// Register name for specific bit width
    fn name_for_size(&self, bits: u32) -> &'static str;

    /// Is this a callee-saved register?
    fn is_callee_saved(&self) -> bool;

    /// Argument-passing registers (in order)
    fn arg_regs() -> &'static [Self];

    /// Callee-saved registers available for allocation
    fn callee_saved_regs() -> &'static [Self];

    /// All registers available for allocation
    fn allocatable_regs() -> &'static [Self];
}

/// Trait for floating-point registers.
/// Implemented by architecture-specific FP register enums.
pub trait FpReg: Copy + Eq + Hash + std::fmt::Debug {
    /// Register name
    fn name(&self) -> &'static str;

    /// Register name for specific bit width (e.g., 32 for single, 64 for double)
    fn name_for_size(&self, bits: u32) -> &'static str;

    /// Is this a callee-saved register?
    fn is_callee_saved(&self) -> bool;

    /// Argument-passing FP registers
    fn arg_regs() -> &'static [Self];

    /// Callee-saved FP registers
    fn callee_saved_regs() -> &'static [Self];

    /// All FP registers available for allocation
    fn allocatable_regs() -> &'static [Self];
}

/// Register constraints for an opcode.
/// Specifies which registers are clobbered or required as inputs.
pub struct RegConstraints<R: 'static> {
    /// Registers clobbered by this operation
    pub clobbers: &'static [R],
    /// Registers required as inputs
    pub inputs: &'static [R],
}

impl<R: 'static> RegConstraints<R> {
    /// No constraints
    pub const NONE: RegConstraints<R> = RegConstraints {
        clobbers: &[],
        inputs: &[],
    };
}

impl<R: 'static> Default for RegConstraints<R> {
    fn default() -> Self {
        RegConstraints {
            clobbers: &[],
            inputs: &[],
        }
    }
}

// ============================================================================
// Common Types
// ============================================================================

/// Live interval for a pseudo-register
#[derive(Debug, Clone)]
pub struct LiveInterval {
    pub pseudo: PseudoId,
    pub start: usize,
    pub end: usize,
}

// ============================================================================
// Common Functions
// ============================================================================

/// Expire old intervals from the active list, returning freed registers to the free list.
/// Generic over register type R (works with both GP and FP register types).
pub fn expire_intervals<R: Copy>(
    active: &mut Vec<(LiveInterval, R)>,
    free_regs: &mut Vec<R>,
    point: usize,
) {
    let mut to_remove = Vec::new();
    for (i, (interval, reg)) in active.iter().enumerate() {
        if interval.end < point {
            free_regs.push(*reg);
            to_remove.push(i);
        }
    }
    for i in to_remove.into_iter().rev() {
        active.remove(i);
    }
}

/// Find all positions of call instructions in a function.
/// Used by spill_args_across_calls to identify where arguments may be clobbered.
/// Includes Call, Longjmp, and Setjmp opcodes since they all invoke external functions
/// and clobber caller-saved registers.
pub fn find_call_positions(func: &Function) -> Vec<usize> {
    let mut call_positions = Vec::new();
    let mut pos = 0usize;
    for block in &func.blocks {
        for insn in &block.insns {
            if matches!(insn.op, Opcode::Call | Opcode::Longjmp | Opcode::Setjmp) {
                call_positions.push(pos);
            }
            pos += 1;
        }
    }
    call_positions
}

/// Check if a live interval crosses any call position.
/// Note: We use <= for the end check because values used as call arguments
/// (where interval.end == call_pos) need to survive until after argument
/// setup, which clobbers caller-saved registers before the actual call.
pub fn interval_crosses_call(interval: &LiveInterval, call_positions: &[usize]) -> bool {
    call_positions
        .iter()
        .any(|&call_pos| interval.start <= call_pos && call_pos <= interval.end)
}

/// Identify pseudo-registers that should use floating-point registers.
/// This is a shared implementation used by both x86-64 and AArch64.
///
/// A pseudo is marked as FP if:
/// 1. It's an FVal constant
/// 2. It's the target of an FP arithmetic operation (FAdd, FSub, FMul, FDiv, FNeg)
/// 3. It's the target of an FP conversion (FCvtF, UCvtF, SCvtF)
/// 4. It has a float type (excluding FP comparisons which produce integer results)
///
/// Note: FP comparisons (FCmpOxx) produce integer results (0 or 1), so their
/// targets should NOT be in FP registers.
pub fn identify_fp_pseudos<F>(func: &Function, is_float_type: F) -> HashSet<PseudoId>
where
    F: Fn(crate::types::TypeId) -> bool,
{
    use crate::ir::Opcode;

    let mut fp_pseudos = HashSet::new();

    // Mark FVal constants as FP
    for pseudo in &func.pseudos {
        if matches!(pseudo.kind, PseudoKind::FVal(_)) {
            fp_pseudos.insert(pseudo.id);
        }
    }

    // Scan instructions for FP operations
    for block in &func.blocks {
        for insn in &block.insns {
            // Check if this is an FP operation that produces an FP result
            let is_fp_producing_op = matches!(
                insn.op,
                Opcode::FAdd
                    | Opcode::FSub
                    | Opcode::FMul
                    | Opcode::FDiv
                    | Opcode::FNeg
                    | Opcode::FCvtF
                    | Opcode::UCvtF
                    | Opcode::SCvtF
            );

            if is_fp_producing_op {
                if let Some(target) = insn.target {
                    fp_pseudos.insert(target);
                }
            }

            // Also check type information (but exclude comparisons)
            let is_comparison = matches!(
                insn.op,
                Opcode::FCmpOEq
                    | Opcode::FCmpONe
                    | Opcode::FCmpOLt
                    | Opcode::FCmpOLe
                    | Opcode::FCmpOGt
                    | Opcode::FCmpOGe
            );

            if !is_comparison {
                if let Some(typ) = insn.typ {
                    if is_float_type(typ) {
                        if let Some(target) = insn.target {
                            fp_pseudos.insert(target);
                        }
                    }
                }
            }
        }
    }

    fp_pseudos
}
