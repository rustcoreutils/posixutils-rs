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
use std::collections::{BTreeMap, HashMap, HashSet};
use std::hash::Hash;

const DEFAULT_INTERVAL_CAPACITY: usize = 64;
const DEFAULT_CONSTRAINT_CAPACITY: usize = 16;
const DEFAULT_CALL_POS_CAPACITY: usize = 16;
const DEFAULT_SMALL_VEC_CAPACITY: usize = 8;

// ============================================================================
// Common Types
// ============================================================================

/// Live interval for a pseudo-register
#[derive(Debug, Clone)]
pub struct LiveInterval {
    pub pseudo: PseudoId,
    pub start: usize,
    pub end: usize,
    /// True if this interval spans a loop back edge (used within a loop)
    pub in_loop: bool,
}

/// A stack slot freed by an expired live interval, available for reuse.
#[derive(Debug, Clone)]
pub struct FreeSlot {
    pub offset: i32,
    pub alignment: i32,
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
    let mut to_remove = Vec::with_capacity(DEFAULT_SMALL_VEC_CAPACITY);
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

/// Expire stack intervals whose live range ended before `point`,
/// returning their slots to the free list.
pub fn expire_stack_intervals(
    active_stack: &mut Vec<(LiveInterval, i32, i32)>,
    free_slots: &mut BTreeMap<i32, Vec<FreeSlot>>,
    point: usize,
) {
    active_stack.retain(|(interval, offset, size)| {
        if interval.end < point {
            let alignment = if *size >= 16 { 16 } else { 8 };
            free_slots.entry(*size).or_default().push(FreeSlot {
                offset: *offset,
                alignment,
            });
            false
        } else {
            true
        }
    });
}

/// Find all positions of call instructions in a function.
/// Used by spill_args_across_calls to identify where arguments may be clobbered.
/// Includes Call, Longjmp, and Setjmp opcodes since they all invoke external functions
/// and clobber caller-saved registers.
pub fn find_call_positions(func: &Function) -> Vec<usize> {
    let mut call_positions = Vec::with_capacity(DEFAULT_CALL_POS_CAPACITY);
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
    let mut conflicts = HashSet::with_capacity(DEFAULT_SMALL_VEC_CAPACITY);

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
    let num_blocks = func.blocks.len();
    let mut constraint_points: Vec<ConstraintPoint<R>> =
        Vec::with_capacity(DEFAULT_CONSTRAINT_CAPACITY);

    // Phase A: Assign linear positions and build block position maps
    let mut block_start_pos: Vec<usize> = Vec::with_capacity(num_blocks);
    let mut block_end_pos: Vec<usize> = Vec::with_capacity(num_blocks);
    let mut bb_id_to_idx: HashMap<BasicBlockId, usize> = HashMap::with_capacity(num_blocks);
    let mut pos = 0usize;
    for (idx, block) in func.blocks.iter().enumerate() {
        bb_id_to_idx.insert(block.id, idx);
        let block_start = pos;
        block_start_pos.push(block_start);
        for insn in &block.insns {
            if let Some((clobbers, involved_pseudos)) = get_constraint_info(insn) {
                constraint_points.push(ConstraintPoint {
                    position: pos,
                    clobbers,
                    involved_pseudos,
                });
            }
            pos += 1;
        }
        block_end_pos.push(if pos > block_start { pos - 1 } else { block_start });
    }

    // Collect argument pseudo IDs (implicitly defined at function entry)
    let arg_pseudos: Vec<PseudoId> = func
        .pseudos
        .iter()
        .filter(|p| matches!(p.kind, PseudoKind::Arg(_)))
        .map(|p| p.id)
        .collect();

    // Phase B: Compute gen/kill sets and per-pseudo per-block positions
    let mut gen: Vec<HashSet<PseudoId>> = vec![HashSet::new(); num_blocks];
    let mut kill: Vec<HashSet<PseudoId>> = vec![HashSet::new(); num_blocks];
    let mut first_pos_map: Vec<HashMap<PseudoId, usize>> = vec![HashMap::new(); num_blocks];
    let mut last_pos_map: Vec<HashMap<PseudoId, usize>> = vec![HashMap::new(); num_blocks];

    for (idx, block) in func.blocks.iter().enumerate() {
        let mut ipos = block_start_pos[idx];
        for insn in &block.insns {
            // Uses: if not yet killed in this block, add to gen
            for &src in &insn.src {
                if !kill[idx].contains(&src) {
                    gen[idx].insert(src);
                }
                first_pos_map[idx].entry(src).or_insert(ipos);
                last_pos_map[idx].insert(src, ipos);
            }
            if let Some(indirect) = insn.indirect_target {
                if !kill[idx].contains(&indirect) {
                    gen[idx].insert(indirect);
                }
                first_pos_map[idx].entry(indirect).or_insert(ipos);
                last_pos_map[idx].insert(indirect, ipos);
            }
            // Inline asm operands are stored in asm_data, not in src/target
            if insn.op == Opcode::Asm {
                if let Some(asm) = &insn.asm_data {
                    for input in &asm.inputs {
                        let p = input.pseudo;
                        if !kill[idx].contains(&p) {
                            gen[idx].insert(p);
                        }
                        first_pos_map[idx].entry(p).or_insert(ipos);
                        last_pos_map[idx].insert(p, ipos);
                    }
                    for output in &asm.outputs {
                        let p = output.pseudo;
                        kill[idx].insert(p);
                        first_pos_map[idx].entry(p).or_insert(ipos);
                        last_pos_map[idx].insert(p, ipos);
                    }
                }
            }
            // Defs: add to kill
            if let Some(target) = insn.target {
                kill[idx].insert(target);
                first_pos_map[idx].entry(target).or_insert(ipos);
                last_pos_map[idx].insert(target, ipos);
            }
            ipos += 1;
        }
    }

    // Arguments are implicitly defined at entry block
    if num_blocks > 0 {
        for &arg_pseudo in &arg_pseudos {
            kill[0].insert(arg_pseudo);
            first_pos_map[0].entry(arg_pseudo).or_insert(0);
            last_pos_map[0].entry(arg_pseudo).or_insert(0);
        }
    }

    // Phase C: Backward dataflow fixpoint for liveness
    // LIVE_out[B] = ∪ LIVE_in[S] for all successors S of B
    // LIVE_in[B]  = GEN[B] ∪ (LIVE_out[B] − KILL[B])
    let mut live_in: Vec<HashSet<PseudoId>> = gen.clone();
    let mut live_out: Vec<HashSet<PseudoId>> = vec![HashSet::new(); num_blocks];
    let mut changed = true;
    while changed {
        changed = false;
        for idx in (0..num_blocks).rev() {
            for &child_id in &func.blocks[idx].children {
                if let Some(&child_idx) = bb_id_to_idx.get(&child_id) {
                    for &p in &live_in[child_idx] {
                        if live_out[idx].insert(p) {
                            changed = true;
                        }
                    }
                }
            }
            for &p in &live_out[idx] {
                if !kill[idx].contains(&p) && live_in[idx].insert(p) {
                    changed = true;
                }
            }
        }
    }

    // Phase D: Construct intervals from liveness
    let mut interval_start: HashMap<PseudoId, usize> =
        HashMap::with_capacity(DEFAULT_INTERVAL_CAPACITY);
    let mut interval_end: HashMap<PseudoId, usize> =
        HashMap::with_capacity(DEFAULT_INTERVAL_CAPACITY);

    for idx in 0..num_blocks {
        let mut referenced: HashSet<PseudoId> =
            HashSet::with_capacity(kill[idx].len() + live_in[idx].len());
        for &p in &kill[idx] {
            referenced.insert(p);
        }
        for &p in &live_in[idx] {
            referenced.insert(p);
        }
        for &p in &live_out[idx] {
            referenced.insert(p);
        }

        for &p in &referenced {
            let start = if live_in[idx].contains(&p) {
                block_start_pos[idx]
            } else {
                first_pos_map[idx]
                    .get(&p)
                    .copied()
                    .unwrap_or(block_start_pos[idx])
            };

            let end = if live_out[idx].contains(&p) {
                block_end_pos[idx]
            } else {
                last_pos_map[idx]
                    .get(&p)
                    .copied()
                    .unwrap_or(block_end_pos[idx])
            };

            interval_start
                .entry(p)
                .and_modify(|s| *s = (*s).min(start))
                .or_insert(start);
            interval_end
                .entry(p)
                .and_modify(|e| *e = (*e).max(end))
                .or_insert(end);
        }
    }

    // Phase E: Detect loop back-edges and mark loop-carried pseudos
    let mut loop_pseudos: HashSet<PseudoId> = HashSet::new();
    for (idx, block) in func.blocks.iter().enumerate() {
        for &child_id in &block.children {
            if let Some(&child_idx) = bb_id_to_idx.get(&child_id) {
                if block_start_pos[child_idx] < block_start_pos[idx] {
                    for &p in &live_out[idx] {
                        if live_in[child_idx].contains(&p) {
                            loop_pseudos.insert(p);
                        }
                    }
                }
            }
        }
    }

    // Phase F: Build sorted LiveInterval vec
    let mut result: Vec<LiveInterval> = interval_start
        .into_iter()
        .filter_map(|(pseudo, start)| {
            interval_end.get(&pseudo).map(|&end| LiveInterval {
                pseudo,
                start,
                end,
                in_loop: loop_pseudos.contains(&pseudo),
            })
        })
        .collect();

    result.sort_by_key(|i| (i.start, i.pseudo.0));
    (result, constraint_points)
}

/// Identify Sym pseudos whose address is taken (SymAddr opcode).
/// These must have stable stack addresses and cannot participate in slot reuse.
pub fn identify_addr_taken_syms(func: &Function) -> HashSet<PseudoId> {
    let mut addr_taken = HashSet::new();
    for block in &func.blocks {
        for insn in &block.insns {
            if insn.op == Opcode::SymAddr {
                for &src in &insn.src {
                    addr_taken.insert(src);
                }
            }
        }
    }
    addr_taken
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

    let mut fp_pseudos = HashSet::with_capacity(DEFAULT_SMALL_VEC_CAPACITY);

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
