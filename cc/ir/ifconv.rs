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
            | Opcode::SetEq
            | Opcode::SetNe
            | Opcode::SetLt
            | Opcode::SetLe
            | Opcode::SetGt
            | Opcode::SetGe
            | Opcode::SetB
            | Opcode::SetBe
            | Opcode::SetA
            | Opcode::SetAe
    )
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
    /// Whether `arm` is the branch's *true* target.
    arm_on_true: bool,
}

/// Collapse every short-circuit diamond whose arm is safe to speculate.
pub fn run(func: &mut Function) -> bool {
    let mut changed = false;
    // To a fixpoint: collapsing the inner diamond of `a && b && c` is what
    // makes the outer one recognizable.
    loop {
        let Some(d) = func
            .blocks
            .iter()
            .filter_map(|bb| recognize(func, bb.id))
            .next()
        else {
            return changed;
        };
        collapse(func, &d);
        changed = true;
    }
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
        return Some(Diamond {
            pred,
            arm,
            merge,
            cond,
            arm_on_true,
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

fn collapse(func: &mut Function, d: &Diamond) {
    // Each phi at the merge becomes a `select` over the two edges' values.
    let mut selects: Vec<(usize, PseudoId, PseudoId)> = Vec::new();
    let merge_idx = func.block_index(d.merge).expect("merge exists");
    for (i, insn) in func.blocks[merge_idx].insns.iter().enumerate() {
        if insn.op != Opcode::Phi {
            continue;
        }
        let from = |b: BasicBlockId| -> Option<PseudoId> {
            insn.phi_list
                .iter()
                .find(|(pb, _)| *pb == b)
                .and_then(|(_, ps)| phi_source_value(func, b, *ps))
        };
        let (Some(v_pred), Some(v_arm)) = (from(d.pred), from(d.arm)) else {
            continue;
        };
        // The arm runs on the branch's `arm_on_true` edge, so the *other*
        // value is the one reaching the merge directly from `pred`.
        let (v_true, v_false) = if d.arm_on_true {
            (v_arm, v_pred)
        } else {
            (v_pred, v_arm)
        };
        selects.push((i, v_true, v_false));
    }

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

    // The predecessor's own `PhiSource` instructions go too.
    for insn in &mut func.blocks[pred_idx].insns {
        if insn.op == Opcode::PhiSource {
            insn.kill();
        }
    }

    // Rewrite the branch. In place rather than `kill()`: that leaves
    // `bb_false` naming a block about to be removed, which the validator
    // inspects on every instruction whatever its opcode.
    let last = func.blocks[pred_idx].insns.len() - 1;
    let term = &mut func.blocks[pred_idx].insns[last];
    term.op = Opcode::Br;
    term.bb_true = Some(d.merge);
    term.bb_false = None;
    term.src.clear();

    // Replace each phi with the select it turned out to be.
    for (i, v_true, v_false) in selects {
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
    func.blocks[pred_idx].children.retain(|c| *c != d.arm);
    let merge_idx = func.block_index(d.merge).expect("merge exists");
    func.blocks[merge_idx].parents.retain(|p| *p != d.arm);
    func.blocks.retain(|b| b.id != d.arm);
    func.rebuild_block_idx();
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
        Instruction::binop(
            Opcode::SetNe,
            PseudoId(2),
            PseudoId(0),
            PseudoId(1),
            types.int_id,
            32,
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

    #[test]
    fn ifconv_is_idempotent() {
        let mut func = diamond(pure_arm());
        assert!(run(&mut func));
        assert!(!run(&mut func), "a second run must find nothing");
    }
}
