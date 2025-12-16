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

use crate::ir::{BasicBlockId, Function, Instruction, Opcode, PseudoId, PseudoKind};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

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

/// A point in the function where register constraints apply.
/// Used during allocation to avoid assigning pseudos to registers
/// that would be clobbered at this point.
/// Generic over register type R.
#[derive(Debug, Clone)]
pub struct ConstraintPoint<R> {
    /// Instruction position in the function
    pub position: usize,
    /// Registers clobbered at this point
    pub clobbers: Vec<R>,
    /// Pseudos that ARE the operands of the constrained instruction
    /// (these should NOT be evicted - they're the actual operands)
    pub involved_pseudos: Vec<PseudoId>,
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

/// Find registers that would conflict with this interval due to constraints.
/// If a pseudo is live at a constraint point and is NOT an operand of that
/// instruction, it cannot be allocated to any clobbered register.
/// Generic over register type R.
pub fn find_conflicting_registers<R: Copy + Eq + Hash>(
    interval: &LiveInterval,
    constraint_points: &[ConstraintPoint<R>],
) -> HashSet<R> {
    let mut conflicts = HashSet::new();

    for cp in constraint_points {
        // If interval is live at this constraint point...
        // Use <= for end to handle case where interval ends exactly at constraint point
        if interval.start <= cp.position && cp.position <= interval.end {
            // ...and this pseudo is NOT involved in the constrained instruction...
            if !cp.involved_pseudos.contains(&interval.pseudo) {
                // ...then it cannot be in any clobbered register
                for &reg in &cp.clobbers {
                    conflicts.insert(reg);
                }
            }
        }
    }

    conflicts
}

/// Compute live intervals and constraint points for a function.
///
/// This is the shared core of live interval computation used by both x86-64 and AArch64.
/// The `get_constraint_info` callback allows architecture-specific constraint handling.
///
/// # Arguments
/// * `func` - The function to analyze
/// * `get_constraint_info` - Callback that returns constraint info for an instruction:
///   - Returns `Some((clobbers, involved_pseudos))` if the instruction has register constraints
///   - Returns `None` if no constraints apply
///
/// The callback signature: `Fn(&Instruction) -> Option<(Vec<R>, Vec<PseudoId>)>`
pub fn compute_live_intervals<R, F>(
    func: &Function,
    get_constraint_info: F,
) -> (Vec<LiveInterval>, Vec<ConstraintPoint<R>>)
where
    R: Clone,
    F: Fn(&Instruction) -> Option<(Vec<R>, Vec<PseudoId>)>,
{
    struct IntervalInfo {
        pseudo: PseudoId,
        first_def: usize,
        last_def: usize,
        last_use: usize,
    }

    let mut intervals: HashMap<PseudoId, IntervalInfo> = HashMap::new();
    let mut constraint_points: Vec<ConstraintPoint<R>> = Vec::new();
    let mut pos = 0usize;

    // First pass: compute block start and end positions
    let mut block_start_pos: HashMap<BasicBlockId, usize> = HashMap::new();
    let mut block_end_pos: HashMap<BasicBlockId, usize> = HashMap::new();
    let mut temp_pos = 0usize;
    for block in &func.blocks {
        block_start_pos.insert(block.id, temp_pos);
        temp_pos += block.insns.len();
        block_end_pos.insert(block.id, temp_pos.saturating_sub(1));
    }

    // Pre-initialize intervals for argument pseudos with first_def = 0
    // because arguments are live from function entry
    for pseudo in &func.pseudos {
        if let PseudoKind::Arg(_) = pseudo.kind {
            intervals.insert(
                pseudo.id,
                IntervalInfo {
                    pseudo: pseudo.id,
                    first_def: 0,
                    last_def: 0,
                    last_use: 0,
                },
            );
        }
    }

    // Collect phi sources and targets
    let mut phi_sources: Vec<(BasicBlockId, PseudoId)> = Vec::new();
    let mut phi_targets: Vec<(BasicBlockId, PseudoId)> = Vec::new();

    for block in &func.blocks {
        for insn in &block.insns {
            if let Some(target) = insn.target {
                intervals
                    .entry(target)
                    .and_modify(|info| {
                        info.first_def = info.first_def.min(pos);
                        info.last_def = info.last_def.max(pos);
                    })
                    .or_insert(IntervalInfo {
                        pseudo: target,
                        first_def: pos,
                        last_def: pos,
                        last_use: pos,
                    });
            }

            for &src in &insn.src {
                if let Some(info) = intervals.get_mut(&src) {
                    info.last_use = info.last_use.max(pos);
                } else {
                    intervals.insert(
                        src,
                        IntervalInfo {
                            pseudo: src,
                            first_def: pos,
                            last_def: pos,
                            last_use: pos,
                        },
                    );
                }
            }

            // For indirect calls, the indirect_target pseudo is also used
            if let Some(indirect) = insn.indirect_target {
                if let Some(info) = intervals.get_mut(&indirect) {
                    info.last_use = info.last_use.max(pos);
                } else {
                    intervals.insert(
                        indirect,
                        IntervalInfo {
                            pseudo: indirect,
                            first_def: pos,
                            last_def: pos,
                            last_use: pos,
                        },
                    );
                }
            }

            for (src_bb, pseudo) in &insn.phi_list {
                phi_sources.push((*src_bb, *pseudo));
                if let Some(target) = insn.target {
                    phi_targets.push((*src_bb, target));
                }
            }

            // Collect constraint points via the architecture-specific callback
            if let Some((clobbers, involved_pseudos)) = get_constraint_info(insn) {
                constraint_points.push(ConstraintPoint {
                    position: pos,
                    clobbers,
                    involved_pseudos,
                });
            }

            pos += 1;
        }
    }

    // Extend phi source intervals to end of their source block
    for (src_bb, pseudo) in phi_sources {
        if let Some(&end_pos) = block_end_pos.get(&src_bb) {
            if let Some(info) = intervals.get_mut(&pseudo) {
                info.last_use = info.last_use.max(end_pos);
            } else {
                intervals.insert(
                    pseudo,
                    IntervalInfo {
                        pseudo,
                        first_def: end_pos,
                        last_def: end_pos,
                        last_use: end_pos,
                    },
                );
            }
        }
    }

    // Extend phi target intervals
    for (src_bb, target) in phi_targets {
        if let Some(&end_pos) = block_end_pos.get(&src_bb) {
            if let Some(info) = intervals.get_mut(&target) {
                info.last_def = info.last_def.max(end_pos);
            } else {
                intervals.insert(
                    target,
                    IntervalInfo {
                        pseudo: target,
                        first_def: end_pos,
                        last_def: end_pos,
                        last_use: end_pos,
                    },
                );
            }
        }
    }

    // Handle loop back edges
    let mut loop_back_edges: Vec<(BasicBlockId, BasicBlockId, usize)> = Vec::new();
    for block in &func.blocks {
        if let Some(last_insn) = block.insns.last() {
            let mut targets = Vec::new();
            if let Some(target) = last_insn.bb_true {
                targets.push(target);
            }
            if let Some(target) = last_insn.bb_false {
                targets.push(target);
            }

            let from_start = block_start_pos.get(&block.id).copied().unwrap_or(0);
            for target_bb in targets {
                let target_start = block_start_pos.get(&target_bb).copied().unwrap_or(0);
                if target_start < from_start {
                    let from_end = block_end_pos.get(&block.id).copied().unwrap_or(0);
                    loop_back_edges.push((block.id, target_bb, from_end));
                }
            }
        }
    }

    // Extend lifetimes for loop variables
    for (_from_bb, to_bb, back_edge_pos) in &loop_back_edges {
        let loop_start = block_start_pos.get(to_bb).copied().unwrap_or(0);

        for info in intervals.values_mut() {
            if info.first_def < loop_start
                && info.last_use >= loop_start
                && info.last_use <= *back_edge_pos
            {
                info.last_use = info.last_use.max(*back_edge_pos);
            }
        }
    }

    let max_pos = pos.saturating_sub(1);

    let mut result: Vec<_> = intervals
        .into_values()
        .map(|info| {
            let end = if info.last_def > info.last_use {
                max_pos
            } else {
                info.last_def.max(info.last_use)
            };
            LiveInterval {
                pseudo: info.pseudo,
                start: info.first_def,
                end,
            }
        })
        .collect();
    result.sort_by_key(|i| i.start);
    (result, constraint_points)
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
