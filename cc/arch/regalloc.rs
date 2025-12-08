//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Common register allocator utilities shared between architectures
//

use crate::ir::{Function, Opcode, PseudoId};

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
pub fn find_call_positions(func: &Function) -> Vec<usize> {
    let mut call_positions = Vec::new();
    let mut pos = 0usize;
    for block in &func.blocks {
        for insn in &block.insns {
            if insn.op == Opcode::Call {
                call_positions.push(pos);
            }
            pos += 1;
        }
    }
    call_positions
}

/// Check if a live interval crosses any call position.
pub fn interval_crosses_call(interval: &LiveInterval, call_positions: &[usize]) -> bool {
    call_positions
        .iter()
        .any(|&call_pos| interval.start <= call_pos && call_pos < interval.end)
}
