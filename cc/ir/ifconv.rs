//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// If-conversion of a short-circuit diamond.
//
// `a && b` and `a || b` lower to a two-block diamond with a phi at the merge,
// because C requires `b` not to be evaluated when `a` already decides the
// answer. When evaluating `b` anyway cannot be observed -- no memory, no call,
// nothing that can trap -- the branch buys nothing and costs the optimizer
// everything: two relationals in two blocks are two facts nothing can compare,
// while side by side they are a peephole.
//
//     if ((x == y) && (x != y)) boom();
//
// is the shape that motivates it. Proving the merge is 0 across the branch
// needs a dominating-predicate inference that plain constant propagation does
// not have; collapsed into `select(x == y, x != y, 0)` it is a question about
// two comparisons over one operand pair, which `instcombine` can answer.
//
// The result is an `Opcode::Select` rather than an `And`/`Or`, which would
// also need both arms to be known 0-or-1. `Select` is the exact meaning of the
// diamond whatever the operands are, and `sccp` already folds one whose
// condition is constant.
//
// Memory-ordering contract: the only instructions this moves are ones
// `is_speculatable` accepts, which excludes every memory op, call, barrier and
// anything that can trap. So no memory operation is reordered with respect to
// any other, which is what `Instruction::is_memory_barrier()` requires.
//

use super::{BasicBlockId, Function, Instruction, Opcode, PseudoId};
use std::collections::{HashSet, VecDeque};

/// Whether `insn` may be executed on a path that would not have run it.
///
/// An allowlist, not a denylist: an opcode is speculatable when someone has
/// established that it cannot trap, cannot touch memory and cannot be
/// observed. Anything unlisted is assumed unsafe, so a new opcode is
/// conservative by default rather than silently hoisted.
///
/// Division and remainder are deliberately absent -- they trap on a zero
/// divisor -- and so are the shifts, whose behaviour past the operand width is
/// undefined. Neither is needed for the shape this pass exists for.
///
/// A float comparison is listed because every one that reaches this pass is
/// emitted as a *quiet* compare -- `ucomis*` and `fucomip` on x86-64, `fcmp`
/// on aarch64 -- which raises nothing for a quiet NaN, so evaluating one on a
/// path that would not have cannot set a flag the program could observe.
/// (Annex F leaves signaling NaNs unspecified.) The comparisons that are
/// library calls, binary128 `long double` on aarch64, have already been
/// rewritten into `Call`s by the time this runs.
fn is_speculatable(insn: &Instruction) -> bool {
    matches!(
        insn.op,
        Opcode::Nop
            | Opcode::Copy
            | Opcode::SetVal
            | Opcode::Add
            | Opcode::Sub
            | Opcode::Mul
            | Opcode::And
            | Opcode::Or
            | Opcode::Xor
            | Opcode::Neg
            | Opcode::Not
            | Opcode::Trunc
            | Opcode::Zext
            | Opcode::Sext
            | Opcode::Select
    ) || insn.op.is_comparison()
}

/// One recognized diamond.
struct Diamond {
    /// The block ending in the conditional branch.
    pred: BasicBlockId,
    /// The block evaluated only when the branch is taken that way.
    arm: BasicBlockId,
    /// Where both paths meet.
    merge: BasicBlockId,
    /// The branch condition.
    cond: PseudoId,
    /// Each phi at the merge, by position, with the values it takes when the
    /// branch goes true and when it goes false: the select it becomes.
    selects: Vec<(usize, PseudoId, PseudoId)>,
}

/// Collapse every short-circuit diamond whose arm is safe to speculate.
///
/// A worklist in block order. Collapsing the inner diamond of `a && b && c`
/// turns its predecessor into a straight-line arm, which is what makes the
/// outer diamond recognizable -- so after a collapse only the blocks that
/// branch to that predecessor are looked at again. Rescanning the whole
/// function after every collapse, and removing each arm as it went, made a
/// function of n `if` statements n^2. The dead arms are removed once, at the
/// end.
pub fn run(func: &mut Function) -> bool {
    let mut queue: VecDeque<BasicBlockId> = func.blocks.iter().map(|b| b.id).collect();
    let mut queued: HashSet<BasicBlockId> = queue.iter().copied().collect();
    let mut dead: HashSet<BasicBlockId> = HashSet::new();
    while let Some(b) = queue.pop_front() {
        queued.remove(&b);
        if dead.contains(&b) {
            continue;
        }
        let Some(d) = recognize(func, b) else {
            continue;
        };
        collapse(func, &d);
        dead.insert(d.arm);
        let outer: Vec<BasicBlockId> = func
            .get_block(d.pred)
            .map(|p| p.parents.clone())
            .unwrap_or_default();
        for p in outer {
            if !dead.contains(&p) && queued.insert(p) {
                queue.push_back(p);
            }
        }
    }
    if dead.is_empty() {
        return false;
    }
    func.blocks.retain(|b| !dead.contains(&b.id));
    func.rebuild_block_idx();
    true
}

/// Whether `pred` ends a diamond this pass can collapse.
fn recognize(func: &Function, pred: BasicBlockId) -> Option<Diamond> {
    let p = func.get_block(pred)?;
    let term = p.insns.last()?;
    if term.op != Opcode::Cbr {
        return None;
    }
    let (t, f) = (term.bb_true?, term.bb_false?);
    let cond = *term.src.first()?;
    if t == f {
        return None;
    }

    // Exactly one side is the arm: reached only from here, falling straight
    // through to the other side.
    for (arm, merge, arm_on_true) in [(t, f, true), (f, t, false)] {
        let a = match func.get_block(arm) {
            Some(a) => a,
            None => continue,
        };
        if a.parents != [pred] || a.addr_taken {
            continue;
        }
        match a.insns.last() {
            Some(last) if last.op == Opcode::Br && last.bb_true == Some(merge) => {}
            _ => continue,
        }
        // Everything but the terminator has to be safe to run unconditionally.
        // A `PhiSource` is bookkeeping rather than a computation and is
        // rewritten by `collapse`, so it is allowed through here.
        let body_ok = a.insns[..a.insns.len() - 1]
            .iter()
            .all(|i| i.op == Opcode::PhiSource || is_speculatable(i));
        if !body_ok {
            continue;
        }
        // The merge must be exactly this diamond's join, or removing the two
        // edges would disturb a phi that has nothing to do with it.
        let m = func.get_block(merge)?;
        if m.parents.len() != 2 || !m.parents.contains(&pred) || !m.parents.contains(&arm) {
            continue;
        }
        let Some(selects) = merge_selects(func, pred, arm, merge, arm_on_true) else {
            continue;
        };
        return Some(Diamond {
            pred,
            arm,
            merge,
            cond,
            selects,
        });
    }
    None
}

/// The value a `PhiSource` targeting `psrc` carries, if it is in `bb`.
fn phi_source_value(func: &Function, bb: BasicBlockId, psrc: PseudoId) -> Option<PseudoId> {
    func.get_block(bb)?
        .insns
        .iter()
        .find(|i| i.op == Opcode::PhiSource && i.target == Some(psrc))
        .and_then(|i| i.src.first().copied())
}

/// The select each phi at `merge` becomes, or `None` if any phi's value
/// along either edge is not where the edge says it is.
///
/// All or nothing, because collapsing removes every `PhiSource` in `pred`
/// and the arm's edge into the merge, and with them every merge phi's
/// operands: a phi left behind would read a value nothing supplies.
fn merge_selects(
    func: &Function,
    pred: BasicBlockId,
    arm: BasicBlockId,
    merge: BasicBlockId,
    arm_on_true: bool,
) -> Option<Vec<(usize, PseudoId, PseudoId)>> {
    let m = func.get_block(merge)?;
    let mut selects = Vec::new();
    for (i, insn) in m.insns.iter().enumerate() {
        if insn.op != Opcode::Phi {
            continue;
        }
        let from = |b: BasicBlockId| -> Option<PseudoId> {
            insn.phi_list
                .iter()
                .find(|(pb, _)| *pb == b)
                .and_then(|(_, ps)| phi_source_value(func, b, *ps))
        };
        let (v_pred, v_arm) = (from(pred)?, from(arm)?);
        // The arm runs on the branch's `arm_on_true` edge, so the *other*
        // value is the one reaching the merge directly from `pred`.
        selects.push(if arm_on_true {
            (i, v_arm, v_pred)
        } else {
            (i, v_pred, v_arm)
        });
    }
    Some(selects)
}

fn collapse(func: &mut Function, d: &Diamond) {
    let merge_idx = func.block_index(d.merge).expect("merge exists");

    // Move the arm's computation into the predecessor, ahead of its branch.
    // `PhiSource` does not come with it: the phi it fed is about to stop
    // existing.
    let arm_idx = func.block_index(d.arm).expect("arm exists");
    let body: Vec<Instruction> = {
        let insns = &func.blocks[arm_idx].insns;
        insns[..insns.len() - 1]
            .iter()
            .filter(|i| i.op != Opcode::PhiSource && i.op != Opcode::Nop)
            .cloned()
            .collect()
    };
    let pred_idx = func.block_index(d.pred).expect("pred exists");
    let at = func.blocks[pred_idx].insns.len() - 1;
    for (n, insn) in body.into_iter().enumerate() {
        func.blocks[pred_idx].insns.insert(at + n, insn);
    }

    // The predecessor's own `PhiSource` instructions go too: every phi they
    // fed is one of `selects`.
    for insn in &mut func.blocks[pred_idx].insns {
        if insn.op == Opcode::PhiSource {
            insn.kill();
        }
    }

    // The branch goes straight to the merge now.
    let last = func.blocks[pred_idx].insns.len() - 1;
    let pos = func.blocks[pred_idx].insns[last].pos;
    func.blocks[pred_idx].insns[last] = Instruction::br(d.merge);
    func.blocks[pred_idx].insns[last].pos = pos;

    // Replace each phi with the select it turned out to be.
    for &(i, v_true, v_false) in &d.selects {
        let insn = &mut func.blocks[merge_idx].insns[i];
        let (target, typ, size) = (insn.target, insn.typ, insn.size);
        *insn = Instruction::select(
            target.expect("a phi defines a value"),
            d.cond,
            v_true,
            v_false,
            typ.expect("a phi carries its type"),
            size,
        );
    }

    // CFG: the arm is gone, and the merge is reached only from the
    // predecessor now.
    func.remove_edge(d.pred, d.arm);
    func.remove_edge(d.arm, d.merge);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, Pseudo, PseudoId};
    use crate::target::Target;
    use crate::types::TypeTable;

    /// `cond ? <arm> : 0` as the front end builds it: a two-block diamond
    /// whose arm computes one instruction and feeds a phi at the merge.
    ///
    /// `arm_body` is the instruction placed in the arm, which is what decides
    /// whether the diamond may be collapsed at all.
    fn diamond(arm_body: Instruction) -> Function {
        let types = TypeTable::new(&Target::host());
        let mut func = Function::new("test", types.int_id);
        let int = types.int_id;

        func.add_pseudo(Pseudo::arg(PseudoId(0), 0));
        func.add_pseudo(Pseudo::val(PseudoId(1), 0)); // the short-circuit 0
        func.add_pseudo(Pseudo::reg(PseudoId(2), 2)); // the arm's value
        func.add_pseudo(Pseudo::reg(PseudoId(3), 3)); // pred's PhiSource
        func.add_pseudo(Pseudo::reg(PseudoId(4), 4)); // arm's PhiSource
        func.add_pseudo(Pseudo::reg(PseudoId(5), 5)); // the phi
        func.next_pseudo = 10;

        let (pred, arm, merge) = (BasicBlockId(0), BasicBlockId(1), BasicBlockId(2));

        let mut b0 = BasicBlock::new(pred);
        b0.add_insn(Instruction::new(Opcode::Entry));
        let mut ps0 = Instruction::phi_source(PseudoId(3), PseudoId(1), int, 32);
        ps0.phi_list = vec![(merge, PseudoId(5))];
        b0.add_insn(ps0);
        b0.add_insn(Instruction::cbr(PseudoId(0), arm, merge));
        b0.children = vec![arm, merge];

        let mut b1 = BasicBlock::new(arm);
        b1.add_insn(arm_body);
        let mut ps1 = Instruction::phi_source(PseudoId(4), PseudoId(2), int, 32);
        ps1.phi_list = vec![(merge, PseudoId(5))];
        b1.add_insn(ps1);
        b1.add_insn(Instruction::br(merge));
        b1.parents = vec![pred];
        b1.children = vec![merge];

        let mut b2 = BasicBlock::new(merge);
        let mut phi = Instruction::phi(PseudoId(5), int, 32);
        phi.phi_list = vec![(pred, PseudoId(3)), (arm, PseudoId(4))];
        b2.add_insn(phi);
        b2.add_insn(Instruction::ret(Some(PseudoId(5))));
        b2.parents = vec![pred, arm];

        for bb in [b0, b1, b2] {
            func.add_block(bb);
        }
        func.entry = pred;
        func
    }

    fn pure_arm() -> Instruction {
        let types = TypeTable::new(&Target::host());
        Instruction::compare(
            Opcode::SetNe,
            PseudoId(2),
            (PseudoId(0), PseudoId(1)),
            (types.int_id, 32),
            (types.int_id, 32),
        )
    }

    #[test]
    fn ifconv_collapses_a_pure_diamond_into_a_select() {
        let mut func = diamond(pure_arm());
        assert!(run(&mut func));
        assert_eq!(func.blocks.len(), 2, "the arm is gone");
        let merge = func.get_block(BasicBlockId(2)).expect("merge survives");
        assert_eq!(merge.insns[0].op, Opcode::Select);
        // `cbr cond, arm, merge`: the arm is the *true* edge, so its value is
        // the true arm of the select.
        assert_eq!(
            merge.insns[0].src,
            vec![PseudoId(0), PseudoId(2), PseudoId(1)]
        );
        assert_eq!(merge.parents, vec![BasicBlockId(0)]);
        let pred = func.get_block(BasicBlockId(0)).expect("pred survives");
        assert_eq!(pred.children, vec![BasicBlockId(2)]);
        assert_eq!(pred.insns.last().unwrap().op, Opcode::Br);
        assert_eq!(
            pred.insns.last().unwrap().bb_false,
            None,
            "a folded branch must not keep a stale target"
        );
    }

    /// `(x < y) && (x > y)` over floats: the comparison is a quiet compare,
    /// so it is speculated like an integer one, and the two relationals land
    /// side by side where `instcombine` can compare them. Float arithmetic is
    /// not: a division can raise divide-by-zero.
    #[test]
    fn ifconv_speculates_a_float_comparison_but_not_float_arithmetic() {
        let types = TypeTable::new(&Target::host());
        for op in [
            Opcode::FCmpOEq,
            Opcode::FCmpONe,
            Opcode::FCmpOLt,
            Opcode::FCmpOLe,
            Opcode::FCmpOGt,
            Opcode::FCmpOGe,
        ] {
            let arm = Instruction::test_binary(
                op,
                PseudoId(2),
                (PseudoId(0), PseudoId(1)),
                types.double_id,
                64,
            );
            let mut func = diamond(arm);
            assert!(run(&mut func), "{op:?} must be speculated");
            let merge = func.get_block(BasicBlockId(2)).expect("merge survives");
            assert_eq!(merge.insns[0].op, Opcode::Select, "{op:?}");
        }
        let fdiv = Instruction::binop(
            Opcode::FDiv,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.double_id,
            64,
        );
        let mut func = diamond(fdiv);
        assert!(!run(&mut func), "a float division must not be speculated");
    }

    /// The guarantee C makes: the right operand of `&&` does not run when the
    /// left has already decided. Speculating one that calls, touches memory,
    /// or can trap breaks exactly the guard someone wrote it for.
    #[test]
    fn ifconv_refuses_an_arm_that_cannot_be_speculated() {
        let types = TypeTable::new(&Target::host());
        let unsafe_arms = [
            (
                "call",
                Instruction::call(Some(PseudoId(2)), "f", vec![], vec![], types.int_id, 32),
            ),
            (
                "load",
                Instruction::load(PseudoId(2), PseudoId(0), 0, types.int_id, 32),
            ),
            (
                "divide",
                Instruction::binop(
                    Opcode::DivS,
                    PseudoId(2),
                    PseudoId(0),
                    PseudoId(1),
                    types.int_id,
                    32,
                ),
            ),
            (
                "shift",
                Instruction::binop(
                    Opcode::Asr,
                    PseudoId(2),
                    PseudoId(0),
                    PseudoId(1),
                    types.int_id,
                    32,
                ),
            ),
        ];
        for (what, insn) in unsafe_arms {
            let mut func = diamond(insn);
            assert!(!run(&mut func), "a {what} must not be speculated");
            assert_eq!(func.blocks.len(), 3, "{what}: the diamond must survive");
        }
    }

    #[test]
    fn ifconv_leaves_the_ir_valid() {
        let mut func = diamond(pure_arm());
        run(&mut func);
        assert!(
            crate::ir::validate::validate_function(&func).is_ok(),
            "{:?}",
            crate::ir::validate::validate_function(&func).err()
        );
    }

    /// A merge reached from somewhere else as well is not this diamond's
    /// join, and collapsing it would disturb a phi that has nothing to do
    /// with the branch.
    #[test]
    fn ifconv_refuses_a_merge_with_another_predecessor() {
        let mut func = diamond(pure_arm());
        let outside = BasicBlockId(9);
        let mut bb = BasicBlock::new(outside);
        bb.add_insn(Instruction::br(BasicBlockId(2)));
        bb.children = vec![BasicBlockId(2)];
        func.add_block(bb);
        func.get_block_mut(BasicBlockId(2))
            .unwrap()
            .parents
            .push(outside);

        assert!(!run(&mut func));
    }

    /// Collapsing removes the predecessor's `PhiSource`s and the arm's edge
    /// into the merge, which takes every merge phi's operands with them. A
    /// phi whose incoming values cannot both be found where the edges say
    /// they are has no select to become, so the whole diamond stays: turning
    /// the phis it can find into selects would leave this one reading a
    /// value nothing supplies any more.
    #[test]
    fn ifconv_refuses_a_merge_phi_it_cannot_collapse() {
        let types = TypeTable::new(&Target::host());
        let int = types.int_id;
        let mut func = diamond(pure_arm());
        let (pred, arm, merge) = (BasicBlockId(0), BasicBlockId(1), BasicBlockId(2));
        // A second phi, whose operand along `pred` names a pseudo no
        // `PhiSource` in `pred` defines.
        for id in 6..=8 {
            func.add_pseudo(Pseudo::reg(PseudoId(id), id));
        }
        let mut ps = Instruction::phi_source(PseudoId(8), PseudoId(0), int, 32);
        ps.phi_list = vec![(merge, PseudoId(6))];
        func.get_block_mut(arm)
            .unwrap()
            .insert_before_terminator(ps);
        let mut phi = Instruction::phi(PseudoId(6), int, 32);
        phi.phi_list = vec![(pred, PseudoId(7)), (arm, PseudoId(8))];
        func.get_block_mut(merge).unwrap().insns.insert(1, phi);

        assert!(!run(&mut func), "the diamond must not be collapsed");
        assert_eq!(func.blocks.len(), 3, "the arm must survive");
        let merge = func.get_block(merge).unwrap();
        assert!(
            merge.insns[..2].iter().all(|i| i.op == Opcode::Phi),
            "neither phi may become a select"
        );
        assert_eq!(
            func.get_block(pred).unwrap().insns[1].op,
            Opcode::PhiSource,
            "the predecessor keeps its PhiSource"
        );
    }

    /// Every phi at the merge becomes a select, each over its own pair of
    /// incoming values, and every `PhiSource` that fed them goes.
    #[test]
    fn ifconv_collapses_every_merge_phi() {
        let types = TypeTable::new(&Target::host());
        let int = types.int_id;
        let mut func = diamond(pure_arm());
        let (pred, arm, merge) = (BasicBlockId(0), BasicBlockId(1), BasicBlockId(2));
        for id in 6..=8 {
            func.add_pseudo(Pseudo::reg(PseudoId(id), id));
        }
        let mut ps_pred = Instruction::phi_source(PseudoId(7), PseudoId(0), int, 32);
        ps_pred.phi_list = vec![(merge, PseudoId(6))];
        func.get_block_mut(pred)
            .unwrap()
            .insert_before_terminator(ps_pred);
        let mut ps_arm = Instruction::phi_source(PseudoId(8), PseudoId(1), int, 32);
        ps_arm.phi_list = vec![(merge, PseudoId(6))];
        func.get_block_mut(arm)
            .unwrap()
            .insert_before_terminator(ps_arm);
        let mut phi = Instruction::phi(PseudoId(6), int, 32);
        phi.phi_list = vec![(pred, PseudoId(7)), (arm, PseudoId(8))];
        func.get_block_mut(merge).unwrap().insns.insert(1, phi);

        assert!(run(&mut func));
        let m = func.get_block(merge).unwrap();
        assert_eq!(m.insns[0].op, Opcode::Select);
        assert_eq!(m.insns[0].src, vec![PseudoId(0), PseudoId(2), PseudoId(1)]);
        assert_eq!(m.insns[1].op, Opcode::Select);
        assert_eq!(m.insns[1].src, vec![PseudoId(0), PseudoId(1), PseudoId(0)]);
        assert!(func
            .blocks
            .iter()
            .flat_map(|b| &b.insns)
            .all(|i| i.op != Opcode::PhiSource));
        assert!(crate::ir::validate::validate_function(&func).is_ok());
    }

    #[test]
    fn ifconv_is_idempotent() {
        let mut func = diamond(pure_arm());
        assert!(run(&mut func));
        assert!(!run(&mut func), "a second run must find nothing");
    }
}
