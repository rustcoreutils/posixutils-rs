//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Dead Code Elimination: removes instructions whose results are never
// used, by mark-sweep:
// 1. Mark "root" instructions (those with side effects)
// 2. Transitively mark all instructions that roots depend on
// 3. Delete all unmarked instructions
//
// Memory-ordering contract: DCE deletes dead instructions in place
// (rewriting them to `Opcode::Nop`) and never reorders surviving
// instructions. The relative order of `Load`, `Store`, `Asm`, `Call`,
// `Atomic*`, and `Fence` is preserved exactly. This is the contract
// that lets `Instruction::is_memory_barrier()` mean something —
// see the doc comment there. Any change to this pass that does start
// reordering must consult `is_memory_barrier()` before crossing.
//

use super::{BasicBlockId, Function, Instruction, Opcode, PseudoId};
use std::collections::{HashMap, HashSet, VecDeque};

const DEFAULT_LIVE_CAPACITY: usize = 64;

// Main Entry Point

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

// Dead Code Elimination

/// Check if an instruction is a "root" (has side effects, cannot be deleted).
///
/// Most of the answer is the opcode's, but not all of it: a `Load` is
/// deletable because reading an ordinary object has no effect, while reading a
/// `volatile` one is observable behaviour (C17 5.1.2.3p6) and must still
/// happen. That distinction is per *access*, not per opcode -- `*p` for a
/// `volatile int *p` is volatile and `*q` for an `int *q` is not -- so it is
/// asked of the instruction. Before this, `volatile int g; void f(void) { g; }`
/// emitted the load at `-O0` and nothing at all from `-O1` up.
///
/// `Store` is a root by its opcode alone and stays that way: its correctness
/// must not come to depend on the marker.
fn is_root(insn: &Instruction) -> bool {
    insn.op.has_side_effects() || insn.is_volatile_access()
}

/// Build a map from each pseudo to the instructions that define it.
fn build_def_map(func: &Function) -> HashMap<PseudoId, Vec<(usize, usize)>> {
    let mut defs: HashMap<PseudoId, Vec<(usize, usize)>> = HashMap::new();
    for (bb_idx, bb) in func.blocks.iter().enumerate() {
        for (insn_idx, insn) in bb.insns.iter().enumerate() {
            if let Some(target) = insn.target {
                defs.entry(target).or_default().push((bb_idx, insn_idx));
            }
        }
    }
    defs
}

/// Eliminate dead code using mark-sweep algorithm.
fn eliminate_dead_code(func: &mut Function) -> bool {
    let def_map = build_def_map(func);
    let mut live: HashSet<PseudoId> = HashSet::with_capacity(DEFAULT_LIVE_CAPACITY);
    let mut worklist: VecDeque<PseudoId> = VecDeque::with_capacity(DEFAULT_LIVE_CAPACITY);

    // Phase 1: Mark roots and their operands as live
    for bb in &func.blocks {
        for insn in &bb.insns {
            if is_root(insn) {
                // Mark all operands of root instructions as live
                for id in insn.uses() {
                    if live.insert(id) {
                        worklist.push_back(id);
                    }
                }
            }
        }
    }

    // Phase 2: Propagate liveness transitively
    while let Some(id) = worklist.pop_front() {
        // Look up all instructions that define this pseudo (O(1) via def_map)
        if let Some(def_sites) = def_map.get(&id) {
            for &(bb_idx, insn_idx) in def_sites {
                let insn = &func.blocks[bb_idx].insns[insn_idx];

                // Mark all operands of the defining instruction as live
                for use_id in insn.uses() {
                    if live.insert(use_id) {
                        worklist.push_back(use_id);
                    }
                }
            }
        }
    }

    // Phase 3: Delete dead instructions (convert to Nop)
    let mut changed = false;
    for bb in &mut func.blocks {
        for insn in &mut bb.insns {
            // Skip roots - they're always live
            if is_root(insn) {
                continue;
            }

            // Skip Nop - already dead
            if insn.op == Opcode::Nop {
                continue;
            }

            // If this instruction has a target that's not live, it's dead
            if let Some(target) = insn.target {
                if !live.contains(&target) {
                    insn.kill();
                    changed = true;
                }
            }
        }
    }

    changed
}

// Unreachable Block Optimization

/// Identify blocks that are *trivially unreachable* — i.e., entering the
/// block immediately leads to undefined behavior with no observable work.
///
/// A block qualifies only if its first non-trivial instruction is
/// `Unreachable`. Trivial = `Nop`, `Entry`, `Phi` (no observable effect).
///
/// This is intentionally stricter than "block ends in Unreachable": the
/// linearizer emits `Unreachable` *after* `noreturn` calls (e.g. `_exit`,
/// `abort`), so a block like `... ; call _exit ; unreachable` is **reachable**
/// — its call has side effects (process termination with a chosen status).
/// Folding `cbr cond, A, B` to `br A` when `B` contains such a call would
/// silently drop the call and is a miscompilation.
fn find_unreachable_blocks(func: &Function) -> HashSet<BasicBlockId> {
    let mut unreachable_ends = HashSet::new();

    for bb in &func.blocks {
        let first_meaningful = bb
            .insns
            .iter()
            .find(|i| !matches!(i.op, Opcode::Nop | Opcode::Entry | Opcode::Phi));
        if let Some(insn) = first_meaningful {
            if insn.op == Opcode::Unreachable {
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
    for b in 0..func.blocks.len() {
        let Some(term) = func.blocks[b].insns.last() else {
            continue;
        };
        if term.op != Opcode::Cbr {
            continue;
        }
        let (Some(t), Some(f)) = (term.bb_true, term.bb_false) else {
            continue;
        };
        // One side leads only to undefined behaviour, so the other is the
        // only way the branch can go. Both: leave it, either is as good.
        let taken = match (
            unreachable_blocks.contains(&t),
            unreachable_blocks.contains(&f),
        ) {
            (true, false) => f,
            (false, true) => t,
            _ => continue,
        };
        changed |= super::propagate::retarget_terminator(func, b, taken);
    }
    changed
}

// Unreachable Block Removal

/// Remove every block no path from the entry reaches, and every edge and
/// phi source it contributed.
///
/// Also the linearizer's last step on a function, at every level: gcc emits
/// no code no path reaches even at `-O0` -- the arm of a constant condition,
/// what follows a `return` or a `goto` -- and a program may depend on it, by
/// calling a function that exists nowhere from such an arm. A block reached
/// through a label, `case`, `default` or a taken address is kept.
pub(crate) fn remove_unreachable_blocks(func: &mut Function) -> bool {
    let reachable = func.reachable_blocks();
    let before = func.blocks.len();

    // Every edge out of a dead block goes before the block does, taking
    // with it the phi operand the edge carried into a live successor.
    let dead_edges: Vec<(BasicBlockId, BasicBlockId)> = func
        .blocks
        .iter()
        .filter(|bb| !reachable.contains(&bb.id))
        .flat_map(|bb| bb.children.iter().map(move |c| (bb.id, *c)))
        .collect();
    for (from, to) in dead_edges {
        func.remove_edge(from, to);
    }
    func.blocks.retain(|bb| reachable.contains(&bb.id));
    func.rebuild_block_idx();

    func.blocks.len() < before
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, Instruction, Pseudo};
    use crate::target::Target;
    use crate::types::TypeTable;

    fn make_simple_func() -> Function {
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
        let types = TypeTable::new(&Target::host());
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
    fn test_cbr_to_noreturn_call_block_not_folded() {
        // Regression for miscompile of CPython _posixsubprocess.c do_fork_exec:
        // `cbr cond, A, B` where B does `call _exit(); unreachable` must NOT
        // be folded to `br A`. The call to _exit is observable (chooses exit
        // status), so B is *reachable* — only its terminator is unreachable.
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("test", types.int_id);

        func.add_pseudo(Pseudo::reg(PseudoId(0), 0));
        func.add_pseudo(Pseudo::val(PseudoId(1), 0));

        // Entry: cbr %0, .L1 (true: ret), .L2 (false: _exit + unreachable)
        let mut bb0 = BasicBlock::new(BasicBlockId(0));
        bb0.children = vec![BasicBlockId(1), BasicBlockId(2)];
        bb0.add_insn(Instruction::new(Opcode::Entry));
        bb0.add_insn(Instruction::cbr(
            PseudoId(0),
            BasicBlockId(1),
            BasicBlockId(2),
        ));

        // True branch
        let mut bb1 = BasicBlock::new(BasicBlockId(1));
        bb1.parents = vec![BasicBlockId(0)];
        bb1.add_insn(Instruction::ret(Some(PseudoId(1))));

        // False branch — calls noreturn `_exit(0)`, then terminator Unreachable.
        // Pre-fix: this block was treated as unreachable, dropping the call.
        let mut bb2 = BasicBlock::new(BasicBlockId(2));
        bb2.parents = vec![BasicBlockId(0)];
        bb2.add_insn(Instruction::call(
            None,
            "_exit",
            vec![PseudoId(1)],
            vec![types.int_id],
            types.void_id,
            8,
        ));
        bb2.add_insn(Instruction::new(Opcode::Unreachable));

        func.add_block(bb0);
        func.add_block(bb1);
        func.add_block(bb2);
        func.entry = BasicBlockId(0);

        run(&mut func);

        // The cbr must remain a Cbr — must NOT be rewritten to an unconditional Br.
        let term = &func.get_block(BasicBlockId(0)).unwrap().insns[1];
        assert_eq!(
            term.op,
            Opcode::Cbr,
            "cbr must not be folded when false-target contains observable side effects"
        );

        // Both targets must still be reachable.
        assert!(func.get_block(BasicBlockId(1)).is_some());
        assert!(func.get_block(BasicBlockId(2)).is_some());
        // The _exit call must survive (Call is a side-effecting root).
        let bb2 = func.get_block(BasicBlockId(2)).unwrap();
        assert!(bb2.insns.iter().any(|i| i.op == Opcode::Call));
    }

    #[test]
    fn test_cbr_to_trivially_unreachable_block_folded() {
        // Companion to the test above: when the false-target is *trivially*
        // unreachable (only Unreachable, no observable work), the fold IS valid.
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("test", types.int_id);

        func.add_pseudo(Pseudo::reg(PseudoId(0), 0));
        func.add_pseudo(Pseudo::val(PseudoId(1), 0));

        let mut bb0 = BasicBlock::new(BasicBlockId(0));
        bb0.children = vec![BasicBlockId(1), BasicBlockId(2)];
        bb0.add_insn(Instruction::new(Opcode::Entry));
        bb0.add_insn(Instruction::cbr(
            PseudoId(0),
            BasicBlockId(1),
            BasicBlockId(2),
        ));

        let mut bb1 = BasicBlock::new(BasicBlockId(1));
        bb1.parents = vec![BasicBlockId(0)];
        bb1.add_insn(Instruction::ret(Some(PseudoId(1))));

        // Trivially unreachable (e.g. from __builtin_unreachable()).
        let mut bb2 = BasicBlock::new(BasicBlockId(2));
        bb2.parents = vec![BasicBlockId(0)];
        bb2.add_insn(Instruction::new(Opcode::Unreachable));

        func.add_block(bb0);
        func.add_block(bb1);
        func.add_block(bb2);
        func.entry = BasicBlockId(0);

        run(&mut func);

        let term = &func.get_block(BasicBlockId(0)).unwrap().insns[1];
        assert_eq!(
            term.op,
            Opcode::Br,
            "cbr to a trivially unreachable block should be folded to br"
        );
    }

    #[test]
    fn test_is_root() {
        let bare = |op| is_root(&Instruction::new(op));

        assert!(bare(Opcode::Ret));
        assert!(bare(Opcode::Store));
        assert!(bare(Opcode::Call));
        assert!(bare(Opcode::Br));
        assert!(bare(Opcode::Cbr));
        assert!(bare(Opcode::Unreachable));

        assert!(!bare(Opcode::Add));
        assert!(!bare(Opcode::Mul));
        assert!(!bare(Opcode::Load));
        assert!(!bare(Opcode::Phi));
    }

    #[test]
    fn test_volatile_load_is_root() {
        // A plain load is deletable; the same load of a volatile object is not.
        let plain = Instruction::new(Opcode::Load);
        assert!(!is_root(&plain));

        let vol = Instruction::new(Opcode::Load).with_volatile(true);
        assert!(is_root(&vol), "reading a volatile object is observable");

        // A volatile store is a root either way -- the marker must not be what
        // its correctness rests on.
        assert!(is_root(
            &Instruction::new(Opcode::Store).with_volatile(true)
        ));
        assert!(is_root(&Instruction::new(Opcode::Store)));
    }

    #[test]
    fn test_volatile_load_with_dead_result_survives() {
        // `volatile int g; void f(void) { g; }` -- the loaded value is never
        // used, and DCE deleted the load outright before the marker existed.
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("test", types.void_id);

        func.add_pseudo(Pseudo::reg(PseudoId(0), 0));
        func.add_pseudo(Pseudo::sym(PseudoId(1), "g".to_string()));

        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        bb.add_insn(
            Instruction::load(PseudoId(0), PseudoId(1), 0, types.int_id, 32).with_volatile(true),
        );
        bb.add_insn(Instruction::ret(None));
        func.add_block(bb);
        func.entry = BasicBlockId(0);

        assert!(!run(&mut func), "a volatile load is not dead code");
        assert_eq!(func.blocks[0].insns[1].op, Opcode::Load);
        assert!(func.blocks[0].insns[1].is_volatile_access());
    }

    #[test]
    fn test_volatile_load_keeps_its_address_live() {
        // `volatile int *p; void f(void) { *p; }` -- the second load is the
        // volatile access, and it is the only thing keeping the first (the
        // read of `p` itself) alive.
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("test", types.void_id);

        func.add_pseudo(Pseudo::sym(PseudoId(0), "p".to_string()));
        func.add_pseudo(Pseudo::reg(PseudoId(1), 1));
        func.add_pseudo(Pseudo::reg(PseudoId(2), 2));

        let ptr = types.pointer_to(types.int_id);
        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        // %1 = load p        (plain: reading the pointer variable)
        bb.add_insn(Instruction::load(PseudoId(1), PseudoId(0), 0, ptr, 64));
        // %2 = load *%1      (volatile: reading the pointed-to object)
        bb.add_insn(
            Instruction::load(PseudoId(2), PseudoId(1), 0, types.int_id, 32).with_volatile(true),
        );
        bb.add_insn(Instruction::ret(None));
        func.add_block(bb);
        func.entry = BasicBlockId(0);

        assert!(!run(&mut func), "neither load may be deleted");
        assert_eq!(func.blocks[0].insns[1].op, Opcode::Load);
        assert_eq!(func.blocks[0].insns[2].op, Opcode::Load);
    }

    #[test]
    fn test_kill_clears_the_volatile_marker() {
        // `kill` makes a `Nop`, which reaches no memory: a marker left behind
        // would be a stale claim to any pass reading the field directly.
        let types = TypeTable::new(&Target::host());
        let mut insn =
            Instruction::load(PseudoId(0), PseudoId(1), 0, types.int_id, 32).with_volatile(true);
        insn.kill();
        assert_eq!(insn.op, Opcode::Nop);
        assert!(!insn.is_volatile);
        assert!(!insn.is_volatile_access());
    }

    #[test]
    fn test_store_is_live() {
        let types = TypeTable::new(&Target::host());
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
    fn test_dead_phi_and_phisource_eliminated() {
        // Build a 2-block CFG: entry → merge
        // merge has a dead Phi (result unused), entry has a PhiSource feeding it.
        // DCE should nop both.
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("test", types.int_id);

        // Pseudos:
        //   %0 = val(10)         -- source value for phi
        //   %1 = phisource target (written by PhiSource in entry)
        //   %2 = phi target (written by Phi in merge) -- DEAD (unused)
        func.add_pseudo(Pseudo::val(PseudoId(0), 10));
        func.add_pseudo(Pseudo::reg(PseudoId(1), 1));
        func.add_pseudo(Pseudo::phi(PseudoId(2), 0));

        // Entry block (bb0): Entry, PhiSource, Br → bb1
        let mut bb0 = BasicBlock::new(BasicBlockId(0));
        bb0.children = vec![BasicBlockId(1)];
        bb0.add_insn(Instruction::new(Opcode::Entry));

        // PhiSource: %1 = phisrc %0, back-pointer → (bb1, %2)
        let mut phisrc = Instruction::phi_source(PseudoId(1), PseudoId(0), types.int_id, 32);
        phisrc.phi_list = vec![(BasicBlockId(1), PseudoId(2))];
        bb0.add_insn(phisrc);

        bb0.add_insn(Instruction::br(BasicBlockId(1)));
        func.add_block(bb0);

        // Merge block (bb1): Phi, Ret (no value — phi is dead)
        let mut bb1 = BasicBlock::new(BasicBlockId(1));
        bb1.parents = vec![BasicBlockId(0)];

        // Phi: %2 = phi [bb0: %1]
        let mut phi = Instruction::phi(PseudoId(2), types.int_id, 32);
        phi.phi_list = vec![(BasicBlockId(0), PseudoId(1))];
        bb1.add_insn(phi);

        bb1.add_insn(Instruction::ret(None));
        func.add_block(bb1);
        func.entry = BasicBlockId(0);

        // Before DCE: PhiSource and Phi are present
        assert_eq!(func.blocks[0].insns[1].op, Opcode::PhiSource);
        assert_eq!(func.blocks[1].insns[0].op, Opcode::Phi);

        let changed = run(&mut func);
        assert!(changed);

        // After DCE: both should be nop'd (dead — phi result %2 is unused)
        assert_eq!(func.blocks[0].insns[1].op, Opcode::Nop);
        assert_eq!(func.blocks[1].insns[0].op, Opcode::Nop);
    }

    #[test]
    fn test_phisource_backpointer_not_false_use() {
        // Verify the DCE fix: PhiSource.phi_list is a back-pointer, not an operand.
        // The phi_list pseudo should NOT keep a defining instruction live.
        let types = TypeTable::new(&Target::host());

        // Build a PhiSource with back-pointer to (bb1, %2)
        let mut phisrc = Instruction::phi_source(PseudoId(1), PseudoId(0), types.int_id, 32);
        phisrc.phi_list = vec![(BasicBlockId(1), PseudoId(2))];

        let uses = phisrc.uses();

        // Should contain src (%0) but NOT the back-pointer pseudo (%2)
        assert!(uses.contains(&PseudoId(0)), "src should be a use");
        assert!(
            !uses.contains(&PseudoId(2)),
            "phi_list back-pointer should NOT be a use for PhiSource"
        );
    }

    #[test]
    fn test_phi_list_is_use_for_phi() {
        // Verify that Phi instructions still report phi_list pseudos as uses
        let types = TypeTable::new(&Target::host());

        let mut phi = Instruction::phi(PseudoId(2), types.int_id, 32);
        phi.phi_list = vec![(BasicBlockId(0), PseudoId(1))];

        let uses = phi.uses();

        // Phi should report %1 as a use
        assert!(
            uses.contains(&PseudoId(1)),
            "phi_list pseudo should be a use for Phi"
        );
    }

    #[test]
    fn test_indirect_call_target_is_use() {
        // Test that uses() includes indirect_target for function pointer calls.
        // This prevents DCE from eliminating the instruction that computes
        // the function pointer before an indirect call.
        let types = TypeTable::new(&Target::host());
        let call_insn = Instruction::call_indirect(
            Some(PseudoId(0)),                // return value target
            PseudoId(5),                      // func_addr (the function pointer)
            vec![PseudoId(1), PseudoId(2)],   // args
            vec![types.int_id, types.int_id], // arg_types
            types.int_id,
            32,
        );

        let uses = call_insn.uses();

        // Verify indirect_target is in the uses list
        assert!(
            uses.contains(&PseudoId(5)),
            "uses() should include indirect_target"
        );
        // Also verify call arguments are in uses
        assert!(uses.contains(&PseudoId(1)));
        assert!(uses.contains(&PseudoId(2)));
    }

    #[test]
    fn test_indirect_call_keeps_func_ptr_live() {
        // Full DCE test: verify an indirect call keeps its function pointer live.
        let types = TypeTable::new(&Target::host());
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
