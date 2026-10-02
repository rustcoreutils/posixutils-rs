//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// IR-to-IR passes that lower high-level constructs to a form the code
// generator accepts: `__builtin_constant_p` placeholders resolved to 0,
// critical edges split, and SSA phi nodes converted to copy instructions.
//

use super::{BasicBlockId, Function, Instruction, Module, Opcode, PseudoKind};
use crate::types::TypeId;
use std::collections::HashMap;

const DEFAULT_COPY_CAPACITY: usize = 8;

// Phi Elimination

/// Eliminate phi nodes by converting PhiSource instructions to Copy.
///
/// With PhiSource, every phi operand has an explicit PhiSource instruction
/// in its predecessor block. This pass:
/// 1. Scans PhiSource instructions and groups copies by block
/// 2. Sequentializes parallel copies (handles the "lost copy" problem)
/// 3. Replaces PhiSource instructions with Nop
/// 4. Inserts sequentialized Copy instructions before block terminators
/// 5. Converts Phi instructions to Nop
pub fn eliminate_phi_nodes(func: &mut Function) {
    let mut copies_to_insert: HashMap<BasicBlockId, Vec<CopyInfo>> =
        HashMap::with_capacity(DEFAULT_COPY_CAPACITY);
    let mut phisource_positions: Vec<(BasicBlockId, usize)> =
        Vec::with_capacity(DEFAULT_COPY_CAPACITY);
    let mut phi_positions: Vec<(BasicBlockId, usize)> = Vec::with_capacity(DEFAULT_COPY_CAPACITY);

    // A phi reached along an edge `split_critical_edges` could not split --
    // out of a computed `goto`, whose source jumps to an address rather
    // than to a block it names -- cannot have its copy placed on that edge
    // alone: the copy at the end of the dispatch block runs whichever label
    // is taken. So that phi takes its operands in a temporary of its own,
    // and the phi itself becomes the one copy out of it at the head of its
    // block (Sreedhar's method I). A temporary written on a path that does
    // not reach the phi is harmless, because every path that does reach it
    // writes it first.
    let routed = route_unsplit_phis(func);

    // Scan all blocks for PhiSource and Phi instructions
    for bb in &func.blocks {
        for (insn_idx, insn) in bb.insns.iter().enumerate() {
            match insn.op {
                Opcode::PhiSource => {
                    // PhiSource: target=phisrc_pseudo, src[0]=value,
                    //            phi_list[0]=(phi_bb, phi_target)
                    if let (Some((_phi_bb, phi_target)), Some(&source)) =
                        (insn.phi_source_dest(), insn.src.first())
                    {
                        debug_assert_eq!(
                            insn.phi_list.len(),
                            1,
                            "PhiSource must have exactly one back-pointer"
                        );
                        let phi_target = routed.get(&phi_target).copied().unwrap_or(phi_target);

                        // Skip copies from undef sources
                        let is_undef = func
                            .get_pseudo(source)
                            .is_some_and(|p| matches!(p.kind, PseudoKind::Undef));

                        if !is_undef {
                            copies_to_insert.entry(bb.id).or_default().push(CopyInfo {
                                target: phi_target,
                                source,
                                size: insn.size,
                                typ: insn.typ,
                            });
                        }
                    }
                    phisource_positions.push((bb.id, insn_idx));
                }
                Opcode::Phi => {
                    phi_positions.push((bb.id, insn_idx));
                }
                _ => {}
            }
        }
    }

    // Sequentialize parallel copies per block. Process blocks in id
    // order — sequentialize_copies allocates temp pseudos via
    // `func.create_reg_pseudo()` when it encounters a copy cycle, and
    // iterating the HashMap in random order would assign different
    // temp pseudo IDs across runs, producing non-deterministic IR
    // (and downstream non-deterministic register allocation).
    let mut sequenced_copies: HashMap<BasicBlockId, Vec<CopyInfo>> =
        HashMap::with_capacity(DEFAULT_COPY_CAPACITY);
    let mut sorted_bb_ids: Vec<BasicBlockId> = copies_to_insert.keys().copied().collect();
    sorted_bb_ids.sort();
    for bb_id in sorted_bb_ids {
        let copies = copies_to_insert.remove(&bb_id).unwrap();
        let sequenced = sequentialize_copies(&copies, func);
        sequenced_copies.insert(bb_id, sequenced);
    }

    // Nop all PhiSource instructions
    for (bb_id, insn_idx) in &phisource_positions {
        if let Some(bb) = func.get_block_mut(*bb_id) {
            if *insn_idx < bb.insns.len() {
                bb.insns[*insn_idx].kill();
            }
        }
    }

    // Insert sequentialized Copy instructions before terminator. Sort
    // by bb_id for determinism (within a block, copies are inserted in
    // their original sequenced order).
    let mut sorted_bb_ids: Vec<BasicBlockId> = sequenced_copies.keys().copied().collect();
    sorted_bb_ids.sort();
    for bb_id in sorted_bb_ids {
        let copies = sequenced_copies.remove(&bb_id).unwrap();
        if let Some(bb) = func.get_block_mut(bb_id) {
            for copy_info in copies {
                let mut copy_insn = Instruction::new(Opcode::Copy)
                    .with_target(copy_info.target)
                    .with_src(copy_info.source)
                    .with_size(copy_info.size);
                copy_insn.typ = copy_info.typ;

                bb.insert_before_terminator(copy_insn);
            }
        }
    }

    // Convert Phi instructions to Nop (PhiSource is the source of truth now),
    // or, for a routed one, to the copy out of its temporary.
    for (bb_id, insn_idx) in phi_positions {
        if let Some(bb) = func.get_block_mut(bb_id) {
            if insn_idx < bb.insns.len() {
                let phi = &mut bb.insns[insn_idx];
                match phi.target.and_then(|t| routed.get(&t).map(|tmp| (t, *tmp))) {
                    Some((target, tmp)) => {
                        let mut copy = Instruction::new(Opcode::Copy)
                            .with_target(target)
                            .with_src(tmp)
                            .with_size(phi.size);
                        copy.typ = phi.typ;
                        copy.pos = phi.pos;
                        *phi = copy;
                    }
                    None => phi.kill(),
                }
            }
        }
    }
}

/// The phis that must take their operands through a temporary, each mapped
/// to its temporary: every phi in a block reached along an edge out of a
/// computed `goto` that has other edges too. See `eliminate_phi_nodes`.
fn route_unsplit_phis(func: &mut Function) -> HashMap<crate::ir::PseudoId, crate::ir::PseudoId> {
    let mut targets = Vec::new();
    for bb in &func.blocks {
        let unsplit = bb.parents.iter().any(|p| {
            func.get_block(*p)
                .is_some_and(|pb| pb.ends_in_computed_goto() && pb.children.len() > 1)
        });
        if !unsplit {
            continue;
        }
        for insn in &bb.insns {
            if insn.op == Opcode::Phi {
                targets.extend(insn.target);
            }
        }
    }
    targets
        .into_iter()
        .map(|t| (t, func.create_reg_pseudo()))
        .collect()
}

/// Information needed to insert a copy instruction
#[derive(Debug, Clone)]
struct CopyInfo {
    target: crate::ir::PseudoId,
    source: crate::ir::PseudoId,
    size: u32,
    typ: Option<TypeId>,
}

/// Sequentialize parallel copies to handle the "lost copy" problem.
///
/// A block's phi copies are parallel: overwriting a target that another
/// pending copy still reads as a source destroys that value. So a copy whose
/// target no other pending copy reads is emitted first, and a cycle -- where
/// no such copy exists -- is broken by saving one source to a temp and
/// redirecting its readers. (Sreedhar et al., "Translating Out of SSA".)
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
            let copy_typ = copy.typ;

            // Create a temporary pseudo to hold the original source value
            let temp_id = func.create_reg_pseudo();

            // Emit: temp = copy source (save the source before it gets overwritten)
            result.push(CopyInfo {
                target: temp_id,
                source: original_source,
                size: copy_size,
                typ: copy_typ,
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

// Module-level lowering

/// Lower all functions in a module.
///
/// This runs all lowering passes to prepare the IR for code generation.
pub fn lower_module(module: &mut Module) {
    for func in &mut module.functions {
        lower_function(func);
    }
    super::validate::verify(module, super::validate::Stage::Lowered, "lowering");
}

/// Lower a single function.
///
/// Runs:
/// 1. `__builtin_constant_p` placeholders resolved to 0
/// 2. Critical-edge splitting, which phi elimination depends on -- see
///    `ir::cfg` for why, and for why nothing may merge blocks after it
/// 3. Phi elimination
pub fn lower_function(func: &mut Function) {
    resolve_constant_p(func);
    func.split_critical_edges();
    eliminate_phi_nodes(func);
}

/// Answer every `ConstantP` this far down the pipeline with 0.
///
/// `sccp` resolves the ones it can prove, and answers 0 itself for an
/// operand it proves *not* constant. What reaches here is everything it
/// never looked at -- which is every one of them at `-O0`, where
/// `opt::optimize_module` returns before the per-function passes run.
///
/// This runs unconditionally, and it has to: both backends end their opcode
/// match in a catch-all that emits nothing, so a survivor would leave its
/// target undefined rather than fail.
fn resolve_constant_p(func: &mut Function) {
    let sites: Vec<(usize, usize)> = func
        .blocks
        .iter()
        .enumerate()
        .flat_map(|(b, bb)| {
            bb.insns
                .iter()
                .enumerate()
                .filter(|(_, insn)| insn.op == Opcode::ConstantP)
                .map(move |(i, _)| (b, i))
        })
        .collect();
    if sites.is_empty() {
        return;
    }
    let zero = func.create_const_pseudo(0);
    for (b, i) in sites {
        let insn = &mut func.blocks[b].insns[i];
        insn.op = Opcode::Copy;
        insn.src = vec![zero];
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::validate::{validate_function_at, Invariant, Stage};
    use crate::ir::{BasicBlock, Instruction, Opcode, Pseudo, PseudoId};
    use crate::target::Target;
    use crate::types::TypeTable;

    fn make_loop_cfg() -> Function {
        // Create a simple loop CFG with PhiSource instructions:

        let types = TypeTable::new(&Target::host());
        let int_type = types.int_id;
        let mut func = Function::new("test", int_type);

        // PhiSource pseudos
        let phisrc1_id = PseudoId(5);
        let phisrc2_id = PseudoId(6);

        // Entry block
        let mut entry = BasicBlock::new(BasicBlockId(0));
        entry.children = vec![BasicBlockId(1)];
        entry.add_insn(Instruction::new(Opcode::Entry));
        // %1 = setval 0 (initial value)
        entry.add_insn(Instruction::set_val(PseudoId(1), int_type, 32));
        // PhiSource: %5 = phisrc %1 (-> .L1:%3)
        let mut phisrc1 = Instruction::phi_source(phisrc1_id, PseudoId(1), int_type, 32);
        phisrc1.phi_list = vec![(BasicBlockId(1), PseudoId(3))];
        entry.add_insn(phisrc1);
        entry.add_insn(Instruction::br(BasicBlockId(1)));

        // Condition block with phi (references PhiSource targets)
        let mut cond = BasicBlock::new(BasicBlockId(1));
        cond.parents = vec![BasicBlockId(0), BasicBlockId(2)];
        cond.children = vec![BasicBlockId(2), BasicBlockId(3)];

        // Phi node: %3 = phi [.L0: %5], [.L2: %6]
        let mut phi = Instruction::phi(PseudoId(3), int_type, 32);
        phi.phi_list = vec![(BasicBlockId(0), phisrc1_id), (BasicBlockId(2), phisrc2_id)];
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
        // PhiSource: %6 = phisrc %2 (-> .L1:%3)
        let mut phisrc2 = Instruction::phi_source(phisrc2_id, PseudoId(2), int_type, 32);
        phisrc2.phi_list = vec![(BasicBlockId(1), PseudoId(3))];
        body.add_insn(phisrc2);
        body.add_insn(Instruction::br(BasicBlockId(1)));

        // Exit block
        let mut exit = BasicBlock::new(BasicBlockId(3));
        exit.parents = vec![BasicBlockId(1)];
        exit.add_insn(Instruction::ret(Some(PseudoId(3))));

        func.entry = BasicBlockId(0);
        func.blocks = vec![entry, cond, body, exit];
        func.rebuild_block_idx();

        // Add pseudos
        func.add_pseudo(Pseudo::reg(PseudoId(1), 1));
        func.add_pseudo(Pseudo::reg(PseudoId(2), 2));
        func.add_pseudo(Pseudo::phi(PseudoId(3), 3));
        func.add_pseudo(Pseudo::val(PseudoId(4), 1)); // constant 1
        func.add_pseudo(Pseudo::phi(phisrc1_id, 5));
        func.add_pseudo(Pseudo::phi(phisrc2_id, 6));

        func
    }

    #[test]
    fn test_phi_elimination() {
        let mut func = make_loop_cfg();

        // Verify phi and phisource exist before elimination
        let cond_before = func.get_block(BasicBlockId(1)).unwrap();
        assert!(
            cond_before.insns.iter().any(|i| i.op == Opcode::Phi),
            "Should have phi before elimination"
        );
        let entry_before = func.get_block(BasicBlockId(0)).unwrap();
        assert!(
            entry_before.insns.iter().any(|i| i.op == Opcode::PhiSource),
            "Entry should have PhiSource before elimination"
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

        // Verify PhiSource instructions are now Nop
        let entry_after = func.get_block(BasicBlockId(0)).unwrap();
        assert!(
            !entry_after.insns.iter().any(|i| i.op == Opcode::PhiSource),
            "PhiSource should be Nop after elimination"
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

    // Tests for sequentialize_copies (parallel copy sequentialization)

    fn make_minimal_func() -> Function {
        let types = TypeTable::new(&Target::host());
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
                typ: None,
            },
            CopyInfo {
                target: PseudoId(2),
                source: PseudoId(20),
                size: 32,
                typ: None,
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
                typ: None,
            },
            CopyInfo {
                target: PseudoId(2),  // b
                source: PseudoId(10), // x
                size: 32,
                typ: None,
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
                typ: None,
            },
            CopyInfo {
                target: PseudoId(2), // b
                source: PseudoId(1), // a
                size: 32,
                typ: None,
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
                typ: None,
            },
            CopyInfo {
                target: PseudoId(2), // b
                source: PseudoId(3), // c
                size: 32,
                typ: None,
            },
            CopyInfo {
                target: PseudoId(3), // c
                source: PseudoId(1), // a
                size: 32,
                typ: None,
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

    #[test]
    fn test_sequentialize_two_disjoint_cycles() {
        // Two independent 2-node cycles in one parallel-copy block:
        //   a = b, b = a    (cycle 1)
        //   c = d, d = c    (cycle 2)
        // Each cycle requires its own temporary; the algorithm must
        // break them independently and emit all four destination
        // writes. Validates that the cycle-break loop terminates and
        // produces a correct sequenced result when the parallel-copy
        // graph has multiple disjoint strongly-connected components.
        let mut func = make_minimal_func();
        let copies = vec![
            CopyInfo {
                target: PseudoId(1), // a
                source: PseudoId(2), // b
                size: 32,
                typ: None,
            },
            CopyInfo {
                target: PseudoId(2), // b
                source: PseudoId(1), // a
                size: 32,
                typ: None,
            },
            CopyInfo {
                target: PseudoId(3), // c
                source: PseudoId(4), // d
                size: 32,
                typ: None,
            },
            CopyInfo {
                target: PseudoId(4), // d
                source: PseudoId(3), // c
                size: 32,
                typ: None,
            },
        ];

        let result = sequentialize_copies(&copies, &mut func);

        // 4 originals + 1 temp per cycle = 6 copies total.
        assert_eq!(result.len(), 6, "two disjoint 2-cycles need two temps");

        // All four original destinations must be written.
        let targets: Vec<_> = result.iter().map(|c| c.target).collect();
        assert!(targets.contains(&PseudoId(1)));
        assert!(targets.contains(&PseudoId(2)));
        assert!(targets.contains(&PseudoId(3)));
        assert!(targets.contains(&PseudoId(4)));

        // Both temps are fresh pseudos.
        let temps: Vec<_> = targets.iter().filter(|t| t.0 >= 100).collect();
        assert_eq!(temps.len(), 2, "exactly one temp per cycle");
    }

    #[test]
    fn test_sequentialize_fp_cycle_propagates_type() {
        // A 2-cycle on float-typed copies. The sequentializer creates
        // a temp pseudo that carries the source's type through the
        // emitted Copy instruction. Downstream `identify_fp_pseudos`
        // walks instruction types to mark FP pseudos for the FP
        // allocator bank — so the temp's Copy MUST carry the float
        // type forward, otherwise the temp would be misallocated as
        // a GP register and corrupt the FP cycle's values.
        let types = TypeTable::new(&Target::host());
        let float_type = types.float_id;
        let mut func = Function::new("test", float_type);
        func.next_pseudo = 100;

        let copies = vec![
            CopyInfo {
                target: PseudoId(1),
                source: PseudoId(2),
                size: 32,
                typ: Some(float_type),
            },
            CopyInfo {
                target: PseudoId(2),
                source: PseudoId(1),
                size: 32,
                typ: Some(float_type),
            },
        ];

        let result = sequentialize_copies(&copies, &mut func);

        assert_eq!(result.len(), 3, "FP 2-cycle needs one temp");
        // Every emitted Copy must carry the float type so the
        // identify_fp_pseudos scan picks up the temp.
        for c in &result {
            assert_eq!(
                c.typ,
                Some(float_type),
                "FP cycle copy must propagate float type to temp"
            );
        }
    }

    #[test]
    fn test_phisource_swap_problem() {
        // Two phis that swap values: %a = phi %b, %b = phi %a
        // PhiSource instructions in predecessor should produce correct copies
        // after sequentialization (requires a temporary).
        let types = TypeTable::new(&Target::host());
        let int_type = types.int_id;
        let mut func = Function::new("test", int_type);

        // PhiSource pseudos
        let phisrc1_id = PseudoId(10);
        let phisrc2_id = PseudoId(11);
        let phisrc3_id = PseudoId(12);
        let phisrc4_id = PseudoId(13);

        // entry(0): set %1, %2; phisrc for both phis; br cond(1)
        let mut entry = BasicBlock::new(BasicBlockId(0));
        entry.children = vec![BasicBlockId(1)];
        entry.add_insn(Instruction::new(Opcode::Entry));
        entry.add_insn(Instruction::set_val(PseudoId(1), int_type, 32));
        entry.add_insn(Instruction::set_val(PseudoId(2), int_type, 32));
        // PhiSource for phi_a (%3): src=%1
        let mut ps1 = Instruction::phi_source(phisrc1_id, PseudoId(1), int_type, 32);
        ps1.phi_list = vec![(BasicBlockId(1), PseudoId(3))];
        entry.add_insn(ps1);
        // PhiSource for phi_b (%4): src=%2
        let mut ps2 = Instruction::phi_source(phisrc2_id, PseudoId(2), int_type, 32);
        ps2.phi_list = vec![(BasicBlockId(1), PseudoId(4))];
        entry.add_insn(ps2);
        entry.add_insn(Instruction::br(BasicBlockId(1)));

        // cond(1): %3=phi(swap a), %4=phi(swap b), cbr
        let mut cond = BasicBlock::new(BasicBlockId(1));
        cond.parents = vec![BasicBlockId(0), BasicBlockId(2)];
        cond.children = vec![BasicBlockId(2), BasicBlockId(3)];
        let mut phi_a = Instruction::phi(PseudoId(3), int_type, 32);
        phi_a.phi_list = vec![
            (BasicBlockId(0), phisrc1_id),
            (BasicBlockId(2), phisrc3_id), // from body: was %4
        ];
        cond.add_insn(phi_a);
        let mut phi_b = Instruction::phi(PseudoId(4), int_type, 32);
        phi_b.phi_list = vec![
            (BasicBlockId(0), phisrc2_id),
            (BasicBlockId(2), phisrc4_id), // from body: was %3
        ];
        cond.add_insn(phi_b);
        cond.add_insn(Instruction::cbr(
            PseudoId(3),
            BasicBlockId(2),
            BasicBlockId(3),
        ));

        // body(2): swap - phisrc for %3 from %4, phisrc for %4 from %3
        let mut body = BasicBlock::new(BasicBlockId(2));
        body.parents = vec![BasicBlockId(1)];
        body.children = vec![BasicBlockId(1)];
        // PhiSource for phi_a (%3): src=%4 (the swap!)
        let mut ps3 = Instruction::phi_source(phisrc3_id, PseudoId(4), int_type, 32);
        ps3.phi_list = vec![(BasicBlockId(1), PseudoId(3))];
        body.add_insn(ps3);
        // PhiSource for phi_b (%4): src=%3 (the swap!)
        let mut ps4 = Instruction::phi_source(phisrc4_id, PseudoId(3), int_type, 32);
        ps4.phi_list = vec![(BasicBlockId(1), PseudoId(4))];
        body.add_insn(ps4);
        body.add_insn(Instruction::br(BasicBlockId(1)));

        // exit(3)
        let mut exit = BasicBlock::new(BasicBlockId(3));
        exit.parents = vec![BasicBlockId(1)];
        exit.add_insn(Instruction::ret(Some(PseudoId(3))));

        func.entry = BasicBlockId(0);
        func.blocks = vec![entry, cond, body, exit];
        func.rebuild_block_idx();

        // Add pseudos
        func.add_pseudo(Pseudo::reg(PseudoId(1), 1));
        func.add_pseudo(Pseudo::reg(PseudoId(2), 2));
        func.add_pseudo(Pseudo::phi(PseudoId(3), 3));
        func.add_pseudo(Pseudo::phi(PseudoId(4), 4));
        func.add_pseudo(Pseudo::phi(phisrc1_id, 10));
        func.add_pseudo(Pseudo::phi(phisrc2_id, 11));
        func.add_pseudo(Pseudo::phi(phisrc3_id, 12));
        func.add_pseudo(Pseudo::phi(phisrc4_id, 13));

        eliminate_phi_nodes(&mut func);

        // In body(2), the swap requires a temporary:
        // The body block should have 3 copies (temp save + 2 actual copies)
        let body_after = func.get_block(BasicBlockId(2)).unwrap();
        let body_copies: Vec<_> = body_after
            .insns
            .iter()
            .filter(|i| i.op == Opcode::Copy)
            .collect();
        assert_eq!(
            body_copies.len(),
            3,
            "Swap requires 3 copies (temp + 2 actual)"
        );

        // Verify both phi targets are written to
        let copy_targets: Vec<_> = body_copies.iter().filter_map(|c| c.target).collect();
        assert!(copy_targets.contains(&PseudoId(3)), "Must write to %3");
        assert!(copy_targets.contains(&PseudoId(4)), "Must write to %4");
    }

    /// Whatever `sccp` never looked at has to be answered here -- which is
    /// every placeholder at `-O0`, where the optimizer does not run. Both
    /// backends end their opcode match in a catch-all that emits nothing,
    /// so a survivor would leave its target undefined rather than fail.
    #[test]
    fn lowering_answers_a_leftover_constant_p_with_zero() {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("t", types.int_id);
        func.add_pseudo(Pseudo::arg(PseudoId(0), 0));
        func.add_pseudo(Pseudo::reg(PseudoId(1), 1));
        func.next_pseudo = 8;

        let mut b0 = BasicBlock::new(BasicBlockId(0));
        b0.add_insn(Instruction::new(Opcode::Entry));
        b0.add_insn(
            Instruction::new(Opcode::ConstantP)
                .with_target(PseudoId(1))
                .with_src(PseudoId(0))
                .with_type_and_size(types.int_id, 32),
        );
        b0.add_insn(Instruction::ret(Some(PseudoId(1))));
        func.add_block(b0);
        func.entry = BasicBlockId(0);

        let errors = validate_function_at(&func, Stage::Lowered).unwrap_err();
        assert!(errors
            .iter()
            .all(|e| e.kind == Invariant::UnresolvedPlaceholder));
        lower_function(&mut func);

        let insn = &func.blocks[0].insns[1];
        assert_eq!(insn.op, Opcode::Copy);
        assert_eq!(func.const_val(insn.src[0]), Some(0));
        assert!(validate_function_at(&func, Stage::Lowered).is_ok());
    }

    /// The lost-copy shape: a loop whose header is also its own latch, with
    /// the phi's value used after the loop.
    ///
    /// `.L1: x = phi(.L0: a, .L1: y); y = x + 1; cbr c, .L1, .L2` and
    /// `.L2: ret x`. The back edge `.L1 -> .L1` is critical -- `.L1` has two
    /// successors and two predecessors -- so the copy `x = y` placed at the
    /// end of `.L1` also runs on the way out, and `.L2` returns `y`. Splitting
    /// gives the back edge a block of its own, and the copy lands there.
    #[test]
    fn lower_splits_a_critical_edge_before_placing_its_copy() {
        let types = TypeTable::new(&Target::host());
        let int = types.int_id;
        let mut f = Function::new("lost_copy", int);
        for i in 0..8 {
            f.add_pseudo(Pseudo::reg(PseudoId(i), i));
        }
        f.next_pseudo = 8;
        let mut b0 = BasicBlock::new(BasicBlockId(0));
        b0.add_insn(Instruction::new(Opcode::Entry));
        let mut s0 = Instruction::phi_source(PseudoId(5), PseudoId(1), int, 32);
        s0.phi_list = vec![(BasicBlockId(1), PseudoId(3))];
        b0.add_insn(s0);
        b0.add_insn(Instruction::br(BasicBlockId(1)));
        let mut b1 = BasicBlock::new(BasicBlockId(1));
        let mut phi = Instruction::phi(PseudoId(3), int, 32);
        phi.phi_list = vec![
            (BasicBlockId(0), PseudoId(5)),
            (BasicBlockId(1), PseudoId(6)),
        ];
        b1.add_insn(phi);
        b1.add_insn(Instruction::binop(
            Opcode::Add,
            PseudoId(2),
            PseudoId(3),
            PseudoId(4),
            int,
            32,
        ));
        let mut s1 = Instruction::phi_source(PseudoId(6), PseudoId(2), int, 32);
        s1.phi_list = vec![(BasicBlockId(1), PseudoId(3))];
        b1.add_insn(s1);
        b1.add_insn(Instruction::cbr(
            PseudoId(7),
            BasicBlockId(1),
            BasicBlockId(2),
        ));
        let mut b2 = BasicBlock::new(BasicBlockId(2));
        b2.add_insn(Instruction::ret(Some(PseudoId(3))));
        f.entry = BasicBlockId(0);
        for b in [b0, b1, b2] {
            f.add_block(b);
        }
        for (a, b) in [(0, 1), (1, 1), (1, 2)] {
            f.add_edge(BasicBlockId(a), BasicBlockId(b));
        }
        assert!(crate::ir::validate::validate_function(&f).is_ok());

        lower_function(&mut f);

        let copies_into_x = |b: &BasicBlock| {
            b.insns
                .iter()
                .filter(|i| i.op == Opcode::Copy && i.target == Some(PseudoId(3)))
                .count()
        };
        // The header copies nothing into `x`: the exit edge must see it intact.
        let header = f.get_block(BasicBlockId(1)).unwrap();
        assert_eq!(
            copies_into_x(header),
            0,
            "the copy would run on the exit edge too"
        );
        // The back edge now goes through a block of its own, which holds it.
        let latch = header
            .children
            .iter()
            .find(|c| **c != BasicBlockId(2))
            .copied()
            .unwrap();
        assert_ne!(latch, BasicBlockId(1));
        let latch = f.get_block(latch).unwrap();
        assert_eq!(copies_into_x(latch), 1);
        assert_eq!(latch.children, vec![BasicBlockId(1)]);
        assert!(
            crate::ir::validate::validate_function_at(&f, crate::ir::validate::Stage::Lowered)
                .is_ok()
        );
    }

    /// An edge out of a computed `goto` cannot be split, so a phi reached
    /// along one takes its operands through a temporary of its own and
    /// becomes the copy out of it at the head of its block. Written straight
    /// into the phi's target, the copy at the end of the dispatch block would
    /// run whichever label the jump chose.
    #[test]
    fn lower_routes_a_phi_behind_an_unsplittable_edge_through_a_temporary() {
        let types = TypeTable::new(&Target::host());
        let int = types.int_id;
        let mut f = Function::new("dispatch", int);
        for i in 0..8 {
            f.add_pseudo(Pseudo::reg(PseudoId(i), i));
        }
        f.next_pseudo = 8;
        // .L0 -> .L1 (label) directly, and to .L3 (dispatch); .L3 jumps to
        // .L1 or .L2 by address. .L1 merges a value from .L0 and from .L3.
        let mut b0 = BasicBlock::new(BasicBlockId(0));
        b0.add_insn(Instruction::new(Opcode::Entry));
        let mut s0 = Instruction::phi_source(PseudoId(5), PseudoId(1), int, 32);
        s0.phi_list = vec![(BasicBlockId(1), PseudoId(3))];
        b0.add_insn(s0);
        b0.add_insn(Instruction::cbr(
            PseudoId(7),
            BasicBlockId(1),
            BasicBlockId(3),
        ));
        let mut b3 = BasicBlock::new(BasicBlockId(3));
        let mut s3 = Instruction::phi_source(PseudoId(6), PseudoId(2), int, 32);
        s3.phi_list = vec![(BasicBlockId(1), PseudoId(3))];
        b3.add_insn(s3);
        b3.add_insn(Instruction::indirect_br(PseudoId(4)));
        let mut b1 = BasicBlock::new(BasicBlockId(1));
        b1.addr_taken = true;
        let mut phi = Instruction::phi(PseudoId(3), int, 32);
        phi.phi_list = vec![
            (BasicBlockId(0), PseudoId(5)),
            (BasicBlockId(3), PseudoId(6)),
        ];
        b1.add_insn(phi);
        b1.add_insn(Instruction::ret(Some(PseudoId(3))));
        let mut b2 = BasicBlock::new(BasicBlockId(2));
        b2.addr_taken = true;
        b2.add_insn(Instruction::ret(Some(PseudoId(1))));
        f.entry = BasicBlockId(0);
        for b in [b0, b3, b1, b2] {
            f.add_block(b);
        }
        for (a, b) in [(0, 1), (0, 3), (3, 1), (3, 2)] {
            f.add_edge(BasicBlockId(a), BasicBlockId(b));
        }
        assert!(crate::ir::validate::validate_function(&f).is_ok());

        lower_function(&mut f);

        // The dispatch block writes a temporary, never `x` itself.
        let dispatch = f.get_block(BasicBlockId(3)).unwrap();
        let written: Vec<PseudoId> = dispatch
            .insns
            .iter()
            .filter(|i| i.op == Opcode::Copy)
            .filter_map(|i| i.target)
            .collect();
        assert_eq!(written.len(), 1);
        assert_ne!(
            written[0],
            PseudoId(3),
            "the dispatch block must not write the phi"
        );
        // And the label's head copies it into `x`.
        let head = &f.get_block(BasicBlockId(1)).unwrap().insns[0];
        assert_eq!(head.op, Opcode::Copy);
        assert_eq!(head.target, Some(PseudoId(3)));
        assert_eq!(head.src, written);
    }
}
