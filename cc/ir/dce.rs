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

const DEFAULT_USE_CAPACITY: usize = 4;
const DEFAULT_LIVE_CAPACITY: usize = 64;
const DEFAULT_REACHABLE_CAPACITY: usize = 16;

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
#[inline]
fn is_root(op: Opcode) -> bool {
    op.has_side_effects()
}

/// Get all pseudo IDs used by an instruction (operands).
fn get_uses(insn: &Instruction) -> Vec<PseudoId> {
    let mut uses = Vec::with_capacity(DEFAULT_USE_CAPACITY);

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

    // Indirect call target (function pointer)
    if let Some(indirect) = insn.indirect_target {
        uses.push(indirect);
    }

    // Inline assembly inputs (the pseudos that the asm reads)
    if let Some(ref asm_data) = insn.asm_data {
        for input in &asm_data.inputs {
            uses.push(input.pseudo);
        }
    }

    uses
}

/// Find all instructions that define a pseudo.
/// Returns vec of (block_index, instruction_index) for each definition.
/// After inlining, a pseudo may have multiple definitions from different branches
/// (e.g., the return target is written to from multiple return paths).
fn find_all_defs(func: &Function, id: PseudoId) -> Vec<(usize, usize)> {
    let mut defs = Vec::new();
    for (bb_idx, bb) in func.blocks.iter().enumerate() {
        for (insn_idx, insn) in bb.insns.iter().enumerate() {
            if insn.target == Some(id) {
                defs.push((bb_idx, insn_idx));
            }
        }
    }
    defs
}

/// Eliminate dead code using mark-sweep algorithm.
fn eliminate_dead_code(func: &mut Function) -> bool {
    let mut live: HashSet<PseudoId> = HashSet::with_capacity(DEFAULT_LIVE_CAPACITY);
    let mut worklist: VecDeque<PseudoId> = VecDeque::with_capacity(DEFAULT_LIVE_CAPACITY);

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
        // Find all instructions that define this pseudo
        // (there may be multiple after inlining, e.g., return target written from multiple paths)
        for (bb_idx, insn_idx) in find_all_defs(func, id) {
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
    let mut reachable = HashSet::with_capacity(DEFAULT_REACHABLE_CAPACITY);
    let mut worklist = VecDeque::with_capacity(DEFAULT_REACHABLE_CAPACITY);

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

    // Collect unreachable predecessors before removing blocks
    let unreachable: HashSet<_> = func
        .blocks
        .iter()
        .map(|bb| bb.id)
        .filter(|id| !reachable.contains(id))
        .collect();

    // Remove unreachable blocks
    func.blocks.retain(|bb| reachable.contains(&bb.id));

    // Update parent/child references and phi nodes to remove dead blocks
    for bb in &mut func.blocks {
        bb.retain_edges(&reachable);
        for &pred in &unreachable {
            bb.remove_phi_predecessor(pred);
        }
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

    #[test]
    fn test_indirect_call_target_is_use() {
        // Test that get_uses() includes indirect_target for function pointer calls.
        // This prevents DCE from eliminating the instruction that computes
        // the function pointer before an indirect call.
        let types = TypeTable::new(64);
        let call_insn = Instruction::call_indirect(
            Some(PseudoId(0)),                // return value target
            PseudoId(5),                      // func_addr (the function pointer)
            vec![PseudoId(1), PseudoId(2)],   // args
            vec![types.int_id, types.int_id], // arg_types
            types.int_id,
            32,
        );

        let uses = get_uses(&call_insn);

        // Verify indirect_target is in the uses list
        assert!(
            uses.contains(&PseudoId(5)),
            "get_uses should include indirect_target"
        );
        // Also verify call arguments are in uses
        assert!(uses.contains(&PseudoId(1)));
        assert!(uses.contains(&PseudoId(2)));
    }

    #[test]
    fn test_indirect_call_keeps_func_ptr_live() {
        // Full DCE test: verify an indirect call keeps its function pointer live.
        let types = TypeTable::new(64);
        let mut func = Function::new("test", types.int_id);

        func.add_pseudo(Pseudo::reg(PseudoId(0), 0)); // result
        func.add_pseudo(Pseudo::reg(PseudoId(1), 1)); // func pointer
        func.add_pseudo(Pseudo::val(PseudoId(2), 42)); // arg

        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));

        // %1 = load the function pointer (this should stay live)
        let mut load = Instruction::new(Opcode::Load);
        load.target = Some(PseudoId(1));
        load.src = vec![PseudoId(2)]; // load from some address
        load.typ = Some(types.pointer_to(types.int_id));
        load.size = 64;
        bb.add_insn(load);

        // %0 = call_indirect %1(%2)
        let call = Instruction::call_indirect(
            Some(PseudoId(0)),
            PseudoId(1),        // indirect through %1
            vec![PseudoId(2)],  // args
            vec![types.int_id], // arg_types
            types.int_id,
            32,
        );
        bb.add_insn(call);

        bb.add_insn(Instruction::ret(Some(PseudoId(0))));
        func.add_block(bb);
        func.entry = BasicBlockId(0);

        let changed = run(&mut func);

        // The load instruction that defines %1 (func pointer) should NOT be
        // eliminated because %1 is used as indirect_target in the call
        assert!(!changed || func.blocks[0].insns[1].op == Opcode::Load);
    }
}
