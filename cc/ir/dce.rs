//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Dead Code Elimination (DCE) pass for pcc C99 compiler
//
// This pass removes instructions whose results are never used.
// It uses a mark-sweep algorithm:
// 1. Mark "root" instructions (those with side effects)
// 2. Transitively mark all instructions that roots depend on
// 3. Delete all unmarked instructions
//

use super::{BasicBlockId, Function, Instruction, Opcode, PseudoId};
use std::collections::{HashSet, VecDeque};

// ============================================================================
// Main Entry Point
// ============================================================================

/// Run the DCE pass on a function.
/// Returns true if any changes were made.
pub fn run(func: &mut Function) -> bool {
    let mut changed = false;

    // Run all phases
    // 1. Fold conditional branches where one target is unreachable
    //    This converts `cbr cond, unreachable, other` to `br other`
    changed |= fold_branches_to_unreachable(func);

    // 2. Eliminate dead instructions (mark-sweep on SSA values)
    changed |= eliminate_dead_code(func);

    // 3. Remove blocks that are no longer reachable from entry
    changed |= remove_unreachable_blocks(func);

    changed
}

// ============================================================================
// Dead Code Elimination
// ============================================================================

/// Check if an opcode is a "root" (has side effects, cannot be deleted).
fn is_root(op: Opcode) -> bool {
    matches!(
        op,
        Opcode::Ret
            | Opcode::Br
            | Opcode::Cbr
            | Opcode::Switch
            | Opcode::Unreachable
            | Opcode::Store
            | Opcode::Call
            | Opcode::Entry
            | Opcode::VaStart
            | Opcode::VaEnd
            | Opcode::VaCopy
            | Opcode::VaArg
            | Opcode::Alloca
            | Opcode::Setjmp  // Has side effects (saves context)
            | Opcode::Longjmp // Never returns (noreturn)
    )
}

/// Get all pseudo IDs used by an instruction (operands).
fn get_uses(insn: &Instruction) -> Vec<PseudoId> {
    let mut uses = Vec::new();

    // Source operands
    uses.extend(insn.src.iter().copied());

    // Phi sources
    for (_, pseudo) in &insn.phi_list {
        uses.push(*pseudo);
    }

    // Switch value (stored in target for switch)
    if insn.op == Opcode::Switch {
        if let Some(target) = insn.target {
            uses.push(target);
        }
    }

    uses
}

/// Find the instruction that defines a pseudo.
/// Returns (block_index, instruction_index) if found.
fn find_def(func: &Function, id: PseudoId) -> Option<(usize, usize)> {
    for (bb_idx, bb) in func.blocks.iter().enumerate() {
        for (insn_idx, insn) in bb.insns.iter().enumerate() {
            if insn.target == Some(id) {
                return Some((bb_idx, insn_idx));
            }
        }
    }
    None
}

/// Eliminate dead code using mark-sweep algorithm.
fn eliminate_dead_code(func: &mut Function) -> bool {
    let mut live: HashSet<PseudoId> = HashSet::new();
    let mut worklist: VecDeque<PseudoId> = VecDeque::new();

    // Phase 1: Mark roots and their operands as live
    for bb in &func.blocks {
        for insn in &bb.insns {
            if is_root(insn.op) {
                // Mark all operands of root instructions as live
                for id in get_uses(insn) {
                    if live.insert(id) {
                        worklist.push_back(id);
                    }
                }
            }
        }
    }

    // Phase 2: Propagate liveness transitively
    while let Some(id) = worklist.pop_front() {
        // Find the instruction that defines this pseudo
        if let Some((bb_idx, insn_idx)) = find_def(func, id) {
            let insn = &func.blocks[bb_idx].insns[insn_idx];

            // Mark all operands of the defining instruction as live
            for use_id in get_uses(insn) {
                if live.insert(use_id) {
                    worklist.push_back(use_id);
                }
            }
        }
    }

    // Phase 3: Delete dead instructions (convert to Nop)
    let mut changed = false;
    for bb in &mut func.blocks {
        for insn in &mut bb.insns {
            // Skip roots - they're always live
            if is_root(insn.op) {
                continue;
            }

            // Skip Nop - already dead
            if insn.op == Opcode::Nop {
                continue;
            }

            // If this instruction has a target that's not live, it's dead
            if let Some(target) = insn.target {
                if !live.contains(&target) {
                    // Convert to Nop
                    insn.op = Opcode::Nop;
                    insn.src.clear();
                    insn.target = None;
                    insn.phi_list.clear();
                    changed = true;
                }
            }
        }
    }

    changed
}

// ============================================================================
// Unreachable Block Optimization
// ============================================================================

/// Identify blocks that end with Unreachable terminator.
fn find_unreachable_blocks(func: &Function) -> HashSet<BasicBlockId> {
    let mut unreachable_ends = HashSet::new();

    for bb in &func.blocks {
        // Check if the last instruction is Unreachable
        if let Some(last) = bb.insns.last() {
            if last.op == Opcode::Unreachable {
                unreachable_ends.insert(bb.id);
            }
        }
    }

    unreachable_ends
}

/// Fold conditional branches where one target is unreachable.
/// If `cbr cond, unreachable_block, other_block`, replace with `br other_block`.
/// This enables further DCE to remove the unreachable block entirely.
fn fold_branches_to_unreachable(func: &mut Function) -> bool {
    let unreachable_blocks = find_unreachable_blocks(func);
    if unreachable_blocks.is_empty() {
        return false;
    }

    let mut changed = false;

    for bb in &mut func.blocks {
        // Look for conditional branches
        for insn in &mut bb.insns {
            if insn.op != Opcode::Cbr {
                continue;
            }

            // Get the branch targets
            let (true_target, false_target) = match (insn.bb_true, insn.bb_false) {
                (Some(t), Some(f)) => (t, f),
                _ => continue,
            };

            // Check if one target leads to unreachable
            let true_unreachable = unreachable_blocks.contains(&true_target);
            let false_unreachable = unreachable_blocks.contains(&false_target);

            if true_unreachable && !false_unreachable {
                // If true branch goes to unreachable, always take false branch
                // Replace cbr with unconditional br to false_target
                insn.op = Opcode::Br;
                insn.bb_true = Some(false_target);
                insn.bb_false = None;
                insn.src.clear(); // Remove condition operand
                                  // Update children: remove true_target from successors
                bb.children.retain(|c| *c != true_target);
                changed = true;
            } else if false_unreachable && !true_unreachable {
                // If false branch goes to unreachable, always take true branch
                // Replace cbr with unconditional br to true_target
                insn.op = Opcode::Br;
                insn.bb_true = Some(true_target);
                insn.bb_false = None;
                insn.src.clear(); // Remove condition operand
                                  // Update children: remove false_target from successors
                bb.children.retain(|c| *c != false_target);
                changed = true;
            }
            // If both are unreachable, leave as-is (both paths are UB anyway)
        }
    }

    changed
}

// ============================================================================
// Unreachable Block Removal
// ============================================================================

/// Compute the set of reachable block IDs starting from entry.
fn compute_reachable(func: &Function) -> HashSet<BasicBlockId> {
    let mut reachable = HashSet::new();
    let mut worklist = VecDeque::new();

    worklist.push_back(func.entry);

    while let Some(bb_id) = worklist.pop_front() {
        if !reachable.insert(bb_id) {
            continue; // Already visited
        }

        // Find this block and add its successors
        if let Some(bb) = func.get_block(bb_id) {
            for child in &bb.children {
                if !reachable.contains(child) {
                    worklist.push_back(*child);
                }
            }
        }
    }

    reachable
}

/// Remove unreachable blocks from the function.
fn remove_unreachable_blocks(func: &mut Function) -> bool {
    let reachable = compute_reachable(func);
    let before = func.blocks.len();

    // Remove unreachable blocks
    func.blocks.retain(|bb| reachable.contains(&bb.id));

    // Update parent/child references to remove dead blocks
    let unreachable: HashSet<_> = func
        .blocks
        .iter()
        .map(|bb| bb.id)
        .collect::<HashSet<_>>()
        .difference(&reachable)
        .copied()
        .collect();

    for bb in &mut func.blocks {
        bb.parents.retain(|p| !unreachable.contains(p));
        bb.children.retain(|c| !unreachable.contains(c));
    }

    func.blocks.len() < before
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, Instruction, Pseudo};
    use crate::types::TypeTable;

    fn make_simple_func() -> Function {
        let types = TypeTable::new(64);
        let mut func = Function::new("test", types.int_id);

        func.add_pseudo(Pseudo::reg(PseudoId(0), 0));
        func.add_pseudo(Pseudo::reg(PseudoId(1), 1));
        func.add_pseudo(Pseudo::val(PseudoId(2), 42));

        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        // Live: used by ret
        bb.add_insn(Instruction::binop(
            Opcode::Add,
            PseudoId(0),
            PseudoId(2),
            PseudoId(2),
            types.int_id,
            32,
        ));
        // Dead: result unused
        bb.add_insn(Instruction::binop(
            Opcode::Mul,
            PseudoId(1),
            PseudoId(2),
            PseudoId(2),
            types.int_id,
            32,
        ));
        bb.add_insn(Instruction::ret(Some(PseudoId(0))));
        func.add_block(bb);
        func.entry = BasicBlockId(0);

        func
    }

    #[test]
    fn test_dead_instruction_removed() {
        let mut func = make_simple_func();

        // Before: Add (live), Mul (dead)
        assert_eq!(func.blocks[0].insns[1].op, Opcode::Add);
        assert_eq!(func.blocks[0].insns[2].op, Opcode::Mul);

        let changed = run(&mut func);
        assert!(changed);

        // After: Add is still Add, Mul is now Nop
        assert_eq!(func.blocks[0].insns[1].op, Opcode::Add);
        assert_eq!(func.blocks[0].insns[2].op, Opcode::Nop);
    }

    #[test]
    fn test_live_instruction_preserved() {
        let types = TypeTable::new(64);
        let mut func = Function::new("test", types.int_id);

        func.add_pseudo(Pseudo::reg(PseudoId(0), 0));
        func.add_pseudo(Pseudo::val(PseudoId(1), 10));
        func.add_pseudo(Pseudo::val(PseudoId(2), 20));

        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        bb.add_insn(Instruction::binop(
            Opcode::Add,
            PseudoId(0),
            PseudoId(1),
            PseudoId(2),
            types.int_id,
            32,
        ));
        bb.add_insn(Instruction::ret(Some(PseudoId(0))));
        func.add_block(bb);
        func.entry = BasicBlockId(0);

        let changed = run(&mut func);
        assert!(!changed); // No changes - Add is used

        assert_eq!(func.blocks[0].insns[1].op, Opcode::Add);
    }

    #[test]
    fn test_transitive_liveness() {
        let types = TypeTable::new(64);
        let mut func = Function::new("test", types.int_id);

        func.add_pseudo(Pseudo::reg(PseudoId(0), 0));
        func.add_pseudo(Pseudo::reg(PseudoId(1), 1));
        func.add_pseudo(Pseudo::val(PseudoId(2), 5));

        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        // %0 = add %2, %2 (live because %1 uses it)
        bb.add_insn(Instruction::binop(
            Opcode::Add,
            PseudoId(0),
            PseudoId(2),
            PseudoId(2),
            types.int_id,
            32,
        ));
        // %1 = mul %0, %2 (live because ret uses it)
        bb.add_insn(Instruction::binop(
            Opcode::Mul,
            PseudoId(1),
            PseudoId(0),
            PseudoId(2),
            types.int_id,
            32,
        ));
        bb.add_insn(Instruction::ret(Some(PseudoId(1))));
        func.add_block(bb);
        func.entry = BasicBlockId(0);

        let changed = run(&mut func);
        assert!(!changed); // Both Add and Mul are transitively live

        assert_eq!(func.blocks[0].insns[1].op, Opcode::Add);
        assert_eq!(func.blocks[0].insns[2].op, Opcode::Mul);
    }

    #[test]
    fn test_unreachable_block_removed() {
        let types = TypeTable::new(64);
        let mut func = Function::new("test", types.int_id);

        // Entry block
        let mut bb0 = BasicBlock::new(BasicBlockId(0));
        bb0.children = vec![BasicBlockId(1)];
        bb0.add_insn(Instruction::new(Opcode::Entry));
        bb0.add_insn(Instruction::br(BasicBlockId(1)));

        // Reachable block
        let mut bb1 = BasicBlock::new(BasicBlockId(1));
        bb1.parents = vec![BasicBlockId(0)];
        bb1.add_insn(Instruction::ret(None));

        // Unreachable block (no path from entry)
        let mut bb2 = BasicBlock::new(BasicBlockId(2));
        bb2.add_insn(Instruction::ret(None));

        func.add_block(bb0);
        func.add_block(bb1);
        func.add_block(bb2);
        func.entry = BasicBlockId(0);

        assert_eq!(func.blocks.len(), 3);

        let changed = run(&mut func);
        assert!(changed);

        assert_eq!(func.blocks.len(), 2);
        assert!(func.get_block(BasicBlockId(0)).is_some());
        assert!(func.get_block(BasicBlockId(1)).is_some());
        assert!(func.get_block(BasicBlockId(2)).is_none());
    }

    #[test]
    fn test_is_root() {
        assert!(is_root(Opcode::Ret));
        assert!(is_root(Opcode::Store));
        assert!(is_root(Opcode::Call));
        assert!(is_root(Opcode::Br));
        assert!(is_root(Opcode::Cbr));
        assert!(is_root(Opcode::Unreachable));

        assert!(!is_root(Opcode::Add));
        assert!(!is_root(Opcode::Mul));
        assert!(!is_root(Opcode::Load));
        assert!(!is_root(Opcode::Phi));
    }

    #[test]
    fn test_store_is_live() {
        let types = TypeTable::new(64);
        let mut func = Function::new("test", types.int_id);

        func.add_pseudo(Pseudo::reg(PseudoId(0), 0));
        func.add_pseudo(Pseudo::val(PseudoId(1), 42));
        func.add_pseudo(Pseudo::sym(PseudoId(2), "x".to_string()));

        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        // Store has side effects - should be kept
        bb.add_insn(Instruction::store(
            PseudoId(1),
            PseudoId(2),
            0,
            types.int_id,
            32,
        ));
        bb.add_insn(Instruction::ret(None));
        func.add_block(bb);
        func.entry = BasicBlockId(0);

        let changed = run(&mut func);
        assert!(!changed); // Store is a root, not dead

        assert_eq!(func.blocks[0].insns[1].op, Opcode::Store);
    }
}
