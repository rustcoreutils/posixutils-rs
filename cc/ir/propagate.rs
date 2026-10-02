//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Rewriting what an analysis has proved: replacing a value with a constant,
// and a conditional terminator with an unconditional one.
//
// No routine here decides anything. Each takes a conclusion its caller has
// already reached -- "this target is the constant `v`", "this branch goes to
// `taken`" -- and performs the edit, with the guards that make the edit safe
// and the pass idempotent.
//
// This is shared rather than copied because each guard is a miscompile when
// a copy forgets it: a terminator rewrite that leaves a jump to a block that
// no longer exists, a phi source folded over, a sixteen-byte constant with
// no `SetVal` to say so. `sccp`, `vrp` and `instcombine` fold values to
// integer constants; `instcombine` and `constglobal` turn a target itself
// into a constant, float or integer.
//

use super::{BasicBlockId, ConstValue, Function, Instruction, Opcode, PseudoId};
use std::collections::HashMap;

/// A `(block index, instruction index)` pair.
pub(crate) type Site = (usize, usize);

/// Replace the instruction at `site` with a `Copy` of the constant `v`.
///
/// The caller has proved the instruction's target is `v`; this decides
/// whether the *rewrite* is admissible and performs it. `minted` carries one
/// pseudo per distinct constant across a run, so repeated folds do not
/// inflate the pseudo table.
///
/// Returns whether anything changed.
pub(crate) fn fold_target_to_const(
    func: &mut Function,
    (b, i): Site,
    v: i128,
    minted: &mut HashMap<i128, PseudoId>,
) -> bool {
    let insn = &func.blocks[b].insns[i];
    if !rewritable(insn) {
        return false;
    }
    // Both allocators resolve a `Val` pseudo with no defining `SetVal` to
    // an immediate; x86-64's sixteen-byte-slot case keys on the
    // *`SetVal`'s* size, which a minted pseudo has none of, and aarch64
    // has no such case at all. Emitting one would mean inserting an
    // instruction, which the in-place discipline here does not do.
    if insn.size == 128 {
        return false;
    }
    // Already a copy of this very constant: rewriting would mint a fresh
    // pseudo, report a change, and do it again next iteration. This is what
    // keeps a pass out of `opt::MAX_ITERATIONS`.
    if insn.op == Opcode::Copy && insn.src.len() == 1 && func.const_val(insn.src[0]) == Some(v) {
        return false;
    }

    let c = match minted.get(&v) {
        Some(id) => *id,
        None => {
            let id = func.create_const_pseudo(v);
            minted.insert(v, id);
            id
        }
    };
    // The copy has the instruction's own result type and width, and reads
    // no operand of another type.
    let insn = &mut func.blocks[b].insns[i];
    insn.op = Opcode::Copy;
    insn.src = vec![c];
    insn.src_typ = None;
    insn.src_size = 0;
    // A folded phi keeps no incoming values; clearing the list is what
    // makes the now-unread `PhiSource` instructions dead, for the `dce`
    // run that follows to collect.
    insn.phi_list.clear();
    true
}

/// Whether the instruction is one a fold may rewrite at all: it defines a
/// value, and is not one whose opcode something else looks for.
fn rewritable(insn: &Instruction) -> bool {
    // A `PhiSource` is how `lower::eliminate_phi_nodes` finds a phi's
    // incoming value: it scans for the opcode, not for `Phi.phi_list`.
    // Rewriting one silently deletes that incoming value. Propagate
    // *through* it, never over it. A `SetVal` is already a constant.
    insn.target.is_some()
        && !matches!(
            insn.op,
            Opcode::PhiSource | Opcode::SetVal | Opcode::Nop | Opcode::Entry
        )
}

/// Make the target of the instruction at `site` the constant `value` itself,
/// and the instruction the `SetVal` that gives it a width.
///
/// The other way round from [`fold_target_to_const`]: the target keeps its
/// identity, so every use already names the constant. This is the only form
/// a float constant can take -- an `FVal` with no `SetVal` is read at 64
/// bits, so a folded `float` would be read out of eight bytes -- and the
/// form a sixteen-byte integer needs, whose stack slot x86-64 sizes from the
/// `SetVal`.
///
/// Declined, with nothing changed, for a target that is not a plain
/// temporary (`Function::is_plain_temp`) and for an instruction with no
/// type to give the `SetVal`. Returns whether anything changed.
pub(crate) fn fold_target_to_setval(func: &mut Function, (b, i): Site, value: ConstValue) -> bool {
    let insn = &func.blocks[b].insns[i];
    if !rewritable(insn) {
        return false;
    }
    let (Some(target), Some(typ)) = (insn.target, insn.typ) else {
        return false;
    };
    let (size, pos) = (insn.size, insn.pos);
    if !func.make_const(target, value) {
        return false;
    }
    let mut set = Instruction::set_val(target, typ, size);
    set.pos = pos;
    func.blocks[b].insns[i] = set;
    true
}

/// Turn block `b`'s terminator into an unconditional branch to `taken`, and
/// drop the edges the other targets lose.
///
/// The caller has proved `taken` is the only reachable successor. An
/// `asm goto` in the block keeps its own edges: they are not the
/// terminator's to give up.
///
/// Returns whether anything changed.
pub(crate) fn retarget_terminator(func: &mut Function, b: usize, taken: BasicBlockId) -> bool {
    let block_id = func.blocks[b].id;
    let Some(term) = func.blocks[b].insns.last() else {
        return false;
    };
    if term.op == Opcode::Br && term.bb_true == Some(taken) {
        return false;
    }
    let before = term.control_targets();
    if !before.contains(&taken) {
        debug_assert!(false, "propagate: {taken} is not a target of {block_id}");
        return false;
    }

    let pos = term.pos;
    let last = func.blocks[b].insns.len() - 1;
    func.blocks[b].insns[last] = Instruction::br(taken);
    func.blocks[b].insns[last].pos = pos;

    // A `Cbr` with both arms on one block, or a `Switch` with two cases
    // landing together, keeps its edge: it is still a successor. So is a
    // block an `asm goto` above the terminator names.
    let still = func.blocks[b].named_successors().unwrap_or_default();
    for d in before {
        if !still.contains(&d) {
            func.remove_edge(block_id, d);
        }
    }
    true
}

/// Which way a conditional branch on the constant `v` goes, or `None` when
/// the constant does not say.
///
/// A `Cbr` carries no type and a width of zero, and a `Val` pseudo may hold
/// bits above its nominal width, so the raw `i128` cannot simply be compared
/// against zero. A branch condition is a comparison's `int` or wider -- the
/// linearizer turns anything narrower into one -- and the back ends test it
/// at that width, so a value with any of its low 32 bits set is nonzero at
/// every width the hardware will test; and zero is zero at every width. Anything else --
/// `1 << 32` viewed at 32 bits -- proves nothing and is left alone.
pub(crate) fn cbr_taken(v: i128) -> Option<bool> {
    if v == 0 {
        return Some(false);
    }
    if super::constfold::at_width(v, 32, false) != 0 {
        return Some(true);
    }
    None
}

/// The block a `Switch` on the constant `v` transfers to.
///
/// Mirrors the backends' lowering rather than C's semantics, because that is
/// what actually runs: the selector is moved into a register of the switch's
/// width, rounded up to 32, and each case is a machine compare at that width.
/// A machine compare does not have a signedness -- `case 3000000000u` and a
/// selector of `3000000000u` agree on all 32 bits whether either is read as
/// negative or not -- so both sides are taken as their low `w` bits and
/// compared there. Reading the selector *signed* instead made
/// `switch (3000000000u) { case 3000000000u: }` fall to its default.
///
/// A GNU range `lo ... hi` is lowered as `(v - lo) <= (hi - lo)` unsigned, at
/// the same width, so the subtraction wraps there too.
pub(crate) fn switch_taken(insn: &Instruction, v: i128) -> Option<BasicBlockId> {
    // The backend prefers the type's width when the instruction carries one.
    // Without a `TypeTable` that width is unknowable, so such a switch is
    // left alone. Every `Switch` the linearizer builds has `typ: None`.
    if insn.typ.is_some() {
        return None;
    }
    let w = if insn.size.max(32) > 32 { 64 } else { 32 };
    let mask = |x: i128| -> u128 { (x as u128) & (u128::MAX >> (128 - w)) };

    let sel = mask(v);
    for (lo, hi, target) in &insn.extra().switch_cases {
        let low = mask(*lo as i128);
        let matched = if lo == hi {
            sel == low
        } else {
            let span = mask((*hi as i128).wrapping_sub(*lo as i128));
            mask(sel.wrapping_sub(low) as i128) <= span
        };
        if matched {
            return Some(*target);
        }
    }
    insn.extra().switch_default
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, Pseudo};
    use crate::target::Target;
    use crate::types::TypeTable;

    /// `entry: %2 = add %1, %1; cbr %1, .L1, .L2`, both arms joining at `.L3`
    /// with a phi over what each supplies.
    fn diamond() -> Function {
        let types = TypeTable::new(&Target::host());
        let int = types.int_id;
        let mut f = Function::new("f", int);
        f.add_pseudo(Pseudo::arg(PseudoId(1), 0));
        f.next_pseudo = 20;
        let (l0, l1, l2, l3) = (
            BasicBlockId(0),
            BasicBlockId(1),
            BasicBlockId(2),
            BasicBlockId(3),
        );
        let mut b0 = BasicBlock::new(l0);
        b0.add_insn(Instruction::new(Opcode::Entry));
        b0.add_insn(Instruction::binop(
            Opcode::Add,
            PseudoId(2),
            PseudoId(1),
            PseudoId(1),
            int,
            32,
        ));
        b0.add_insn(Instruction::cbr(PseudoId(1), l1, l2));
        b0.children = vec![l1, l2];
        f.add_block(b0);
        for (id, src) in [(l1, 10u32), (l2, 11)] {
            let mut b = BasicBlock::new(id);
            let mut ps = Instruction::phi_source(PseudoId(src), PseudoId(2), int, 32);
            ps.phi_list = vec![(l3, PseudoId(12))];
            b.add_insn(ps);
            b.add_insn(Instruction::br(l3));
            b.children = vec![l3];
            f.add_block(b);
        }
        let mut b3 = BasicBlock::new(l3);
        let mut phi = Instruction::phi(PseudoId(12), int, 32);
        phi.phi_list = vec![(l1, PseudoId(10)), (l2, PseudoId(11))];
        b3.add_insn(phi);
        b3.add_insn(Instruction::ret(Some(PseudoId(12))));
        f.add_block(b3);
        f.entry = l0;
        f.rebuild_parents();
        f
    }

    /// A proved value becomes a copy of one constant pseudo, minted once per
    /// run; doing it again changes nothing, and a phi source is never
    /// folded over.
    #[test]
    fn a_proved_value_becomes_a_copy_of_its_constant() {
        let mut f = diamond();
        let mut minted = HashMap::new();
        assert!(fold_target_to_const(&mut f, (0, 1), 6, &mut minted));
        let insn = &f.blocks[0].insns[1];
        assert_eq!(insn.op, Opcode::Copy);
        assert_eq!(f.const_val(insn.src[0]), Some(6));
        assert!(
            !fold_target_to_const(&mut f, (0, 1), 6, &mut minted),
            "idempotent"
        );
        assert_eq!(minted.len(), 1);
        assert!(
            !fold_target_to_const(&mut f, (1, 0), 6, &mut minted),
            "a PhiSource"
        );
        assert_eq!(f.blocks[1].insns[0].op, Opcode::PhiSource);
    }

    /// A 128-bit target is never rewritten into a copy of a minted
    /// constant: nothing would give the constant its sixteen bytes.
    #[test]
    fn a_128_bit_value_is_not_folded_to_a_copy() {
        let mut f = diamond();
        f.blocks[0].insns[1].size = 128;
        let mut minted = HashMap::new();
        assert!(!fold_target_to_const(&mut f, (0, 1), 1 << 70, &mut minted));
        assert_eq!(f.blocks[0].insns[1].op, Opcode::Add);
        assert!(minted.is_empty());
    }

    /// A target folded to a `SetVal` becomes the constant itself, keeps its
    /// width, type and source position, and is not folded twice. One that
    /// is not a plain temporary, or a phi source, is left alone.
    #[test]
    fn a_target_becomes_the_constant_its_setval_defines() {
        let mut f = diamond();
        let pos = crate::diag::Position {
            line: 7,
            ..Default::default()
        };
        f.blocks[0].insns[1].pos = Some(pos);
        f.blocks[0].insns[1].size = 128;
        let typ = f.blocks[0].insns[1].typ;

        assert!(fold_target_to_setval(
            &mut f,
            (0, 1),
            ConstValue::Int(1 << 70)
        ));
        let insn = &f.blocks[0].insns[1];
        assert_eq!(
            (insn.op, insn.target, insn.typ, insn.size, insn.pos),
            (Opcode::SetVal, Some(PseudoId(2)), typ, 128, Some(pos))
        );
        assert!(insn.src.is_empty());
        assert_eq!(f.const_val(PseudoId(2)), Some(1 << 70));
        assert!(
            !fold_target_to_setval(&mut f, (0, 1), ConstValue::Int(1 << 70)),
            "already a SetVal"
        );

        assert!(
            !fold_target_to_setval(&mut f, (1, 0), ConstValue::Int(6)),
            "a PhiSource"
        );
        assert_eq!(f.blocks[1].insns[0].op, Opcode::PhiSource);

        let mut g = diamond();
        let arg = g.blocks[0].insns[1].src[0];
        g.blocks[0].insns[1].target = Some(arg);
        assert!(
            !fold_target_to_setval(&mut g, (0, 1), ConstValue::Int(6)),
            "an argument is not a plain temporary"
        );
        assert_eq!(g.blocks[0].insns[1].op, Opcode::Add);
    }

    /// Folding a branch drops the edge it can no longer take. The arm it
    /// orphans keeps its own edge on, and so its operand of the join's phi,
    /// until `dce` removes the unreachable block.
    #[test]
    fn a_folded_branch_drops_the_edge_it_cannot_take() {
        let mut f = diamond();
        assert!(retarget_terminator(&mut f, 0, BasicBlockId(1)));
        let term = f.blocks[0].insns.last().unwrap();
        assert_eq!((term.op, term.bb_true), (Opcode::Br, Some(BasicBlockId(1))));
        assert_eq!(f.blocks[0].children, vec![BasicBlockId(1)]);
        assert!(f.blocks[2].parents.is_empty());
        assert_eq!(f.blocks[3].insns[0].phi_list.len(), 2);
        assert!(
            !retarget_terminator(&mut f, 0, BasicBlockId(1)),
            "already a br there"
        );
    }

    /// A constant decides a branch only when it reads the same at every
    /// width a condition is tested at.
    #[test]
    fn a_branch_on_a_constant_goes_one_way_only_when_it_says_so() {
        assert_eq!(cbr_taken(0), Some(false));
        assert_eq!(cbr_taken(1), Some(true));
        assert_eq!(cbr_taken(-1), Some(true));
        assert_eq!(cbr_taken(1 << 32), None, "zero in the low 32 bits only");
    }

    /// A machine compare has no signedness, and the selector and the case
    /// label are the same C type. Reading the selector *signed* made
    /// `switch (3000000000u) { case 3000000000u: }` miss its own case.
    #[test]
    fn switch_taken_matches_a_case_above_the_signed_range() {
        let taken = switch_case_for(3_000_000_000i64, 3_000_000_000i128);
        assert_eq!(taken, Some(BasicBlockId(7)), "the case must match itself");
    }

    #[test]
    fn switch_taken_falls_to_default_when_nothing_matches() {
        assert_eq!(switch_case_for(5, 6), Some(BasicBlockId(9)));
    }

    #[test]
    fn switch_taken_matches_a_gnu_range() {
        let mut insn = Instruction::new(Opcode::Switch);
        insn.size = 32;
        insn.extra_mut().switch_cases = vec![(30, 50, BasicBlockId(7))];
        insn.extra_mut().switch_default = Some(BasicBlockId(9));
        assert_eq!(switch_taken(&insn, 40), Some(BasicBlockId(7)));
        assert_eq!(switch_taken(&insn, 51), Some(BasicBlockId(9)));
        assert_eq!(switch_taken(&insn, 29), Some(BasicBlockId(9)));
    }

    /// A switch carrying a type has a width that cannot be computed without
    /// a `TypeTable`, so it must decline rather than guess.
    #[test]
    fn switch_taken_with_a_type_is_left_alone() {
        let types = TypeTable::new(&Target::host());
        let mut insn = Instruction::new(Opcode::Switch);
        insn.size = 32;
        insn.typ = Some(types.int_id);
        insn.extra_mut().switch_cases = vec![(5, 5, BasicBlockId(7))];
        insn.extra_mut().switch_default = Some(BasicBlockId(9));
        assert_eq!(switch_taken(&insn, 5), None);
    }

    fn switch_case_for(case: i64, selector: i128) -> Option<BasicBlockId> {
        let mut insn = Instruction::new(Opcode::Switch);
        insn.size = 32;
        insn.extra_mut().switch_cases = vec![(case, case, BasicBlockId(7))];
        insn.extra_mut().switch_default = Some(BasicBlockId(9));
        switch_taken(&insn, selector)
    }
}
