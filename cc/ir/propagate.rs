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
// Neither routine decides anything. Each takes a conclusion its caller has
// already reached -- "this target is the constant `v`", "this branch goes to
// `taken`" -- and performs the edit, with the guards that make the edit safe
// and the pass idempotent.
//
// This is shared rather than copied because the terminator rewrite is forty
// lines of CFG bookkeeping whose failure mode is a jump to a block that no
// longer exists, discovered somewhere else entirely. Two analyses now reach
// these conclusions, `sccp` from a constant lattice and `vrp` from a range
// one, and a second copy of this code is a second place for the two to
// drift.
//

use super::{BasicBlockId, Function, Instruction, Opcode, PseudoId};
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
    if insn.target.is_none() {
        return false;
    }
    // A `PhiSource` is how `lower::eliminate_phi_nodes` finds a phi's
    // incoming value: it scans for the opcode, not for `Phi.phi_list`.
    // Turning one into a `Copy` silently deletes that incoming value.
    // Propagate *through* it, never over it.
    if matches!(
        insn.op,
        Opcode::PhiSource | Opcode::SetVal | Opcode::Nop | Opcode::Entry
    ) {
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
/// against zero. Both backends compute the condition's width as at least 32,
/// so a value with any of its low 32 bits set is nonzero at every width the
/// hardware will test; and zero is zero at every width. Anything else --
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
    use crate::target::Target;
    use crate::types::TypeTable;

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
