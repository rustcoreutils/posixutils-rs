//
// Copyright (c) 2025-2026 Jeff Garzik
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

use super::{BasicBlockId, Function, Instruction, Module, Opcode, PseudoKind};
use std::collections::HashMap;

/// Default capacity for phi elimination copy tracking
const DEFAULT_COPY_CAPACITY: usize = 8;

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
    let mut copies_to_insert: HashMap<BasicBlockId, Vec<CopyInfo>> =
        HashMap::with_capacity(DEFAULT_COPY_CAPACITY);
    let mut phi_positions: Vec<(BasicBlockId, usize)> = Vec::with_capacity(DEFAULT_COPY_CAPACITY);

    // Scan all blocks for phi nodes
    for bb in &func.blocks {
        for (insn_idx, insn) in bb.insns.iter().enumerate() {
            if insn.op == Opcode::Phi {
                if let Some(target) = insn.target {
                    let size = insn.size;

                    // For each incoming edge in the phi
                    for (pred_bb, src_pseudo) in &insn.phi_list {
                        // Skip copies from undef sources - they represent uninitialized
                        // values and copying from them would read garbage.
                        // This is safe because if we reach this phi via the undef path,
                        // the value is semantically undefined anyway.
                        let is_undef = func
                            .pseudos
                            .iter()
                            .find(|p| p.id == *src_pseudo)
                            .is_some_and(|p| matches!(p.kind, PseudoKind::Undef));

                        if is_undef {
                            continue;
                        }

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
    // Use parallel copy sequentialization to handle the "lost copy" problem
    //
    // First sequentialize all copies (may create temporaries), then insert
    let mut sequenced_copies: HashMap<BasicBlockId, Vec<CopyInfo>> =
        HashMap::with_capacity(DEFAULT_COPY_CAPACITY);
    for (pred_bb_id, copies) in copies_to_insert {
        let sequenced = sequentialize_copies(&copies, func);
        sequenced_copies.insert(pred_bb_id, sequenced);
    }

    // Now insert the sequenced copies into blocks
    for (pred_bb_id, copies) in sequenced_copies {
        if let Some(pred_bb) = func.get_block_mut(pred_bb_id) {
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

/// Sequentialize parallel copies to handle the "lost copy" problem.
///
/// When multiple phi nodes in the same block have overlapping targets and sources,
/// naive sequential insertion can corrupt values. For example:
///
///   %a = phi [pred: %b]
///   %b = phi [pred: %a]
///
/// If we naively emit:
///   %a = copy %b
///   %b = copy %a   // BUG: %a already overwritten!
///
/// The solution is to:
/// 1. Detect when a target is used as a source in another copy
/// 2. Order copies so targets are written before they're read as sources
/// 3. For cycles, break them using a temporary variable
///
/// Algorithm (based on "Translating Out of SSA" by Sreedhar et al.):
/// - A copy is "free" if its TARGET is not used as SOURCE by any other pending copy
/// - Process free copies first (writing to a target that no one else needs to read)
/// - For cycles, save one source to a temp, update all uses, then continue
fn sequentialize_copies(copies: &[CopyInfo], func: &mut Function) -> Vec<CopyInfo> {
    use std::collections::HashSet;

    if copies.is_empty() {
        return Vec::new();
    }

    // If no overlapping targets/sources, return copies as-is
    let targets: HashSet<_> = copies.iter().map(|c| c.target).collect();
    let sources: HashSet<_> = copies.iter().map(|c| c.source).collect();
    let overlap: HashSet<_> = targets.intersection(&sources).copied().collect();

    if overlap.is_empty() {
        return copies.to_vec();
    }

    // There are overlapping targets and sources - need to sequentialize
    let mut result = Vec::with_capacity(copies.len() + 1); // +1 for possible temp
    let mut pending: Vec<CopyInfo> = copies.to_vec();

    // Keep processing until all copies are emitted
    while !pending.is_empty() {
        // Find a "free" copy: its TARGET is not used as SOURCE by any OTHER pending copy
        // This means we can safely overwrite the target without destroying a value someone needs
        let free_idx = pending.iter().enumerate().position(|(idx, copy)| {
            !pending
                .iter()
                .enumerate()
                .any(|(other_idx, other)| other_idx != idx && other.source == copy.target)
        });

        if let Some(idx) = free_idx {
            // Safe to emit this copy - its target isn't needed by anyone else
            let copy = pending.remove(idx);
            result.push(copy);
        } else {
            // All remaining copies form cycles - break with a temporary
            // Every target is used as a source by someone else
            //
            // Pick any copy and save its SOURCE to a temp, then update all copies
            // that use this source to use the temp instead.
            let copy = &pending[0];
            let original_source = copy.source;
            let copy_size = copy.size;

            // Create a temporary pseudo to hold the original source value
            let temp_id = super::PseudoId(func.next_pseudo);
            func.next_pseudo += 1;
            let temp_pseudo = super::Pseudo::reg(temp_id, temp_id.0);
            func.add_pseudo(temp_pseudo);

            // Emit: temp = copy source (save the source before it gets overwritten)
            result.push(CopyInfo {
                target: temp_id,
                source: original_source,
                size: copy_size,
            });

            // Update ALL pending copies that use this source to use temp instead
            for other in &mut pending {
                if other.source == original_source {
                    other.source = temp_id;
                }
            }
            // Note: don't remove any copy yet - they'll be emitted in subsequent iterations
            // Now at least one copy should be "free" because we broke a dependency
        }
    }

    result
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

        let types = TypeTable::new(64);
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

    // ========================================================================
    // Tests for sequentialize_copies (parallel copy sequentialization)
    // ========================================================================

    fn make_minimal_func() -> Function {
        let types = TypeTable::new(64);
        let int_type = types.int_id;
        let mut func = Function::new("test", int_type);
        func.next_pseudo = 100; // Start high to avoid conflicts
        func
    }

    #[test]
    fn test_sequentialize_no_overlap() {
        // No overlapping targets/sources - should return copies unchanged
        // a = x, b = y (completely independent)
        let mut func = make_minimal_func();
        let copies = vec![
            CopyInfo {
                target: PseudoId(1),
                source: PseudoId(10),
                size: 32,
            },
            CopyInfo {
                target: PseudoId(2),
                source: PseudoId(20),
                size: 32,
            },
        ];

        let result = sequentialize_copies(&copies, &mut func);

        assert_eq!(result.len(), 2);
        // Order preserved, no temporaries created
        assert_eq!(result[0].target, PseudoId(1));
        assert_eq!(result[0].source, PseudoId(10));
        assert_eq!(result[1].target, PseudoId(2));
        assert_eq!(result[1].source, PseudoId(20));
    }

    #[test]
    fn test_sequentialize_simple_reorder() {
        // Needs reordering but no cycle: a = b, c = d, b = x
        // b is a target and a source, but there's no cycle
        // Safe order: a = b first (reads b), then b = x (writes b)
        let mut func = make_minimal_func();
        let copies = vec![
            CopyInfo {
                target: PseudoId(1), // a
                source: PseudoId(2), // b
                size: 32,
            },
            CopyInfo {
                target: PseudoId(2),  // b
                source: PseudoId(10), // x
                size: 32,
            },
        ];

        let result = sequentialize_copies(&copies, &mut func);

        assert_eq!(result.len(), 2);
        // a = b must come before b = x
        assert_eq!(result[0].target, PseudoId(1)); // a = b
        assert_eq!(result[0].source, PseudoId(2));
        assert_eq!(result[1].target, PseudoId(2)); // b = x
        assert_eq!(result[1].source, PseudoId(10));
    }

    #[test]
    fn test_sequentialize_simple_cycle() {
        // Simple 2-node cycle: a = b, b = a
        // Requires temporary: temp = a, a = b, b = temp
        let mut func = make_minimal_func();
        let copies = vec![
            CopyInfo {
                target: PseudoId(1), // a
                source: PseudoId(2), // b
                size: 32,
            },
            CopyInfo {
                target: PseudoId(2), // b
                source: PseudoId(1), // a
                size: 32,
            },
        ];

        let result = sequentialize_copies(&copies, &mut func);

        // Should have 3 copies: temp = source, then the two original copies
        assert_eq!(result.len(), 3, "Cycle requires temporary variable");

        // First copy should save one of the sources to a temp
        let temp_id = result[0].target;
        assert!(temp_id.0 >= 100, "Temp should be a new pseudo (id >= 100)");

        // Verify the cycle is broken: we can now execute sequentially
        // The exact order depends on which source was saved first
        let targets: Vec<_> = result.iter().map(|c| c.target).collect();
        assert!(targets.contains(&PseudoId(1)), "Must write to a");
        assert!(targets.contains(&PseudoId(2)), "Must write to b");
    }

    #[test]
    fn test_sequentialize_three_node_cycle() {
        // 3-node cycle: a = b, b = c, c = a
        let mut func = make_minimal_func();
        let copies = vec![
            CopyInfo {
                target: PseudoId(1), // a
                source: PseudoId(2), // b
                size: 32,
            },
            CopyInfo {
                target: PseudoId(2), // b
                source: PseudoId(3), // c
                size: 32,
            },
            CopyInfo {
                target: PseudoId(3), // c
                source: PseudoId(1), // a
                size: 32,
            },
        ];

        let result = sequentialize_copies(&copies, &mut func);

        // Should have 4 copies: one temp save + 3 original
        assert_eq!(result.len(), 4, "3-node cycle requires one temporary");

        // Verify all original targets are written
        let targets: Vec<_> = result.iter().map(|c| c.target).collect();
        assert!(targets.contains(&PseudoId(1)));
        assert!(targets.contains(&PseudoId(2)));
        assert!(targets.contains(&PseudoId(3)));
    }

    #[test]
    fn test_sequentialize_empty() {
        let mut func = make_minimal_func();
        let copies: Vec<CopyInfo> = vec![];

        let result = sequentialize_copies(&copies, &mut func);

        assert!(result.is_empty());
    }
}
