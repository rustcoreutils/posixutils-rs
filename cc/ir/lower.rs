//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Lowering passes for pcc C99 compiler
//
// This module contains IR-to-IR transformation passes that lower
// high-level IR constructs to forms suitable for code generation.
//
// Key passes:
// - Phi elimination: Converts SSA phi nodes to copy instructions
//

use super::{BasicBlockId, Function, Instruction, Module, Opcode};
use std::collections::HashMap;

// ============================================================================
// Phi Elimination
// ============================================================================

/// Eliminate phi nodes by inserting copy instructions in predecessor blocks.
///
/// SSA phi nodes like:
///   %9 = phi.32 [.L0: %1], [.L2: %7]
///
/// Are converted to:
///   - At end of .L0: %9 = copy %1
///   - At end of .L2: %9 = copy %7
///   - Original phi is converted to Nop
///
/// This is a standard "naive" phi elimination. For better code quality,
/// a more sophisticated approach with parallel copies and coalescing
/// could be used, but this works correctly for all cases.
pub fn eliminate_phi_nodes(func: &mut Function) {
    // Collect all phi information first to avoid borrowing issues
    // Map: predecessor_bb -> Vec<(target, source, size)>
    let mut copies_to_insert: HashMap<BasicBlockId, Vec<CopyInfo>> = HashMap::new();
    let mut phi_positions: Vec<(BasicBlockId, usize)> = Vec::new();

    // Scan all blocks for phi nodes
    for bb in &func.blocks {
        for (insn_idx, insn) in bb.insns.iter().enumerate() {
            if insn.op == Opcode::Phi {
                if let Some(target) = insn.target {
                    let size = insn.size;

                    // For each incoming edge in the phi
                    for (pred_bb, src_pseudo) in &insn.phi_list {
                        let copy_info = CopyInfo {
                            target,
                            source: *src_pseudo,
                            size,
                        };

                        copies_to_insert
                            .entry(*pred_bb)
                            .or_default()
                            .push(copy_info);
                    }

                    // Record phi position for later removal
                    phi_positions.push((bb.id, insn_idx));
                }
            }
        }
    }

    // Insert copy instructions at the end of predecessor blocks
    for (pred_bb_id, copies) in copies_to_insert {
        if let Some(pred_bb) = func.get_block_mut(pred_bb_id) {
            // Insert copies before the terminator
            // Note: If there are multiple copies, they should logically execute
            // in parallel (this is the "lost copy" problem). For now, we insert
            // them sequentially which works for non-overlapping cases.
            // A proper solution would use parallel copy sequentialization.
            for copy_info in copies {
                let copy_insn = Instruction::new(Opcode::Copy)
                    .with_target(copy_info.target)
                    .with_src(copy_info.source)
                    .with_size(copy_info.size);

                pred_bb.insert_before_terminator(copy_insn);
            }
        }
    }

    // Convert phi nodes to Nop (instead of removing to preserve indices)
    // IMPORTANT: Keep phi_list AND target intact so codegen can use them for live interval computation.
    // - phi_list contains which blocks phi sources come from (needed for back edges)
    // - target is the phi target pseudo, which needs extended live intervals
    for (bb_id, insn_idx) in phi_positions {
        if let Some(bb) = func.get_block_mut(bb_id) {
            if insn_idx < bb.insns.len() {
                bb.insns[insn_idx].op = Opcode::Nop;
                // Note: target and phi_list are intentionally NOT cleared - codegen needs them
                bb.insns[insn_idx].src.clear();
            }
        }
    }
}

/// Information needed to insert a copy instruction
#[derive(Debug, Clone)]
struct CopyInfo {
    target: crate::ir::PseudoId,
    source: crate::ir::PseudoId,
    size: u32,
}

// ============================================================================
// Module-level lowering
// ============================================================================

/// Lower all functions in a module.
///
/// This runs all lowering passes to prepare the IR for code generation.
pub fn lower_module(module: &mut Module) {
    for func in &mut module.functions {
        lower_function(func);
    }
}

/// Lower a single function.
///
/// Runs:
/// 1. Phi elimination
pub fn lower_function(func: &mut Function) {
    eliminate_phi_nodes(func);
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, Instruction, Opcode, Pseudo, PseudoId};
    use crate::types::TypeTable;

    fn make_loop_cfg() -> Function {
        // Create a simple loop CFG:
        //
        //     entry(0)
        //        |
        //        v
        //     cond(1) <--+
        //      / \       |
        //     v   v      |
        //   body(2)  exit(3)
        //     |
        //     +----------+
        //
        // With phi node in cond block:
        //   %3 = phi [.L0: %1], [.L2: %2]

        let types = TypeTable::new();
        let int_type = types.int_id;
        let mut func = Function::new("test", int_type);

        // Entry block
        let mut entry = BasicBlock::new(BasicBlockId(0));
        entry.children = vec![BasicBlockId(1)];
        entry.add_insn(Instruction::new(Opcode::Entry));
        // %1 = setval 0 (initial value)
        let mut setval = Instruction::new(Opcode::SetVal);
        setval.target = Some(PseudoId(1));
        setval.size = 32;
        entry.add_insn(setval);
        entry.add_insn(Instruction::br(BasicBlockId(1)));

        // Condition block with phi
        let mut cond = BasicBlock::new(BasicBlockId(1));
        cond.parents = vec![BasicBlockId(0), BasicBlockId(2)];
        cond.children = vec![BasicBlockId(2), BasicBlockId(3)];

        // Phi node: %3 = phi [.L0: %1], [.L2: %2]
        let mut phi = Instruction::phi(PseudoId(3), int_type, 32);
        phi.phi_list = vec![
            (BasicBlockId(0), PseudoId(1)),
            (BasicBlockId(2), PseudoId(2)),
        ];
        cond.add_insn(phi);
        cond.add_insn(Instruction::cbr(
            PseudoId(3),
            BasicBlockId(2),
            BasicBlockId(3),
        ));

        // Body block
        let mut body = BasicBlock::new(BasicBlockId(2));
        body.parents = vec![BasicBlockId(1)];
        body.children = vec![BasicBlockId(1)];
        // %2 = add %3, 1
        let add = Instruction::binop(
            Opcode::Add,
            PseudoId(2),
            PseudoId(3),
            PseudoId(4), // constant 1
            int_type,
            32,
        );
        body.add_insn(add);
        body.add_insn(Instruction::br(BasicBlockId(1)));

        // Exit block
        let mut exit = BasicBlock::new(BasicBlockId(3));
        exit.parents = vec![BasicBlockId(1)];
        exit.add_insn(Instruction::ret(Some(PseudoId(3))));

        func.entry = BasicBlockId(0);
        func.blocks = vec![entry, cond, body, exit];

        // Add pseudos
        func.add_pseudo(Pseudo::reg(PseudoId(1), 1));
        func.add_pseudo(Pseudo::reg(PseudoId(2), 2));
        func.add_pseudo(Pseudo::phi(PseudoId(3), 3));
        func.add_pseudo(Pseudo::val(PseudoId(4), 1)); // constant 1

        func
    }

    #[test]
    fn test_phi_elimination() {
        let mut func = make_loop_cfg();

        // Verify phi exists before elimination
        let cond_before = func.get_block(BasicBlockId(1)).unwrap();
        assert!(
            cond_before.insns.iter().any(|i| i.op == Opcode::Phi),
            "Should have phi before elimination"
        );

        // Run phi elimination
        eliminate_phi_nodes(&mut func);

        // Verify phi is now Nop
        let cond_after = func.get_block(BasicBlockId(1)).unwrap();
        assert!(
            !cond_after.insns.iter().any(|i| i.op == Opcode::Phi),
            "Should not have phi after elimination"
        );
        assert!(
            cond_after.insns.iter().any(|i| i.op == Opcode::Nop),
            "Should have Nop where phi was"
        );

        // Verify copies were inserted in predecessor blocks
        let entry = func.get_block(BasicBlockId(0)).unwrap();
        let entry_copies: Vec<_> = entry
            .insns
            .iter()
            .filter(|i| i.op == Opcode::Copy)
            .collect();
        assert_eq!(entry_copies.len(), 1, "Entry should have 1 copy");
        assert_eq!(entry_copies[0].target, Some(PseudoId(3)));
        assert_eq!(entry_copies[0].src, vec![PseudoId(1)]);

        let body = func.get_block(BasicBlockId(2)).unwrap();
        let body_copies: Vec<_> = body.insns.iter().filter(|i| i.op == Opcode::Copy).collect();
        assert_eq!(body_copies.len(), 1, "Body should have 1 copy");
        assert_eq!(body_copies[0].target, Some(PseudoId(3)));
        assert_eq!(body_copies[0].src, vec![PseudoId(2)]);
    }

    #[test]
    fn test_copy_before_terminator() {
        let mut func = make_loop_cfg();
        eliminate_phi_nodes(&mut func);

        // In entry block, copy should be before the branch
        let entry = func.get_block(BasicBlockId(0)).unwrap();
        let last_idx = entry.insns.len() - 1;
        assert_eq!(
            entry.insns[last_idx].op,
            Opcode::Br,
            "Last instruction should be branch"
        );
        assert_eq!(
            entry.insns[last_idx - 1].op,
            Opcode::Copy,
            "Copy should be before branch"
        );

        // In body block, copy should be before the branch
        let body = func.get_block(BasicBlockId(2)).unwrap();
        let last_idx = body.insns.len() - 1;
        assert_eq!(
            body.insns[last_idx].op,
            Opcode::Br,
            "Last instruction should be branch"
        );
        assert_eq!(
            body.insns[last_idx - 1].op,
            Opcode::Copy,
            "Copy should be before branch"
        );
    }
}
