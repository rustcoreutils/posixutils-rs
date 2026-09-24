//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// What a pass knows about a pseudo before it rewrites anything: the constant
// visible at it, the value it ultimately copies from, and the comparison that
// defined it.
//
// These are *queries*, not rewrites. Each is built once per pass run and is
// sound as a function-wide map with no dominance query, because SSA invariant
// I1 (`ir/validate.rs`, `check_single_def`) makes each target's definition
// unique -- so "the instruction that defines %n" is a fact about the whole
// function rather than about a program point. That is also why they live here
// and not on `Function`: after `ir::lower::lower_module`, phi elimination
// deliberately creates multi-def copies, and the same walks would be unsound
// there.
//
// Shared because a second analysis now needs the same canonicalization.
// `ConstMap::root` is what makes "two pseudos are the same value" decidable
// after promotion out of memory gives every use of a local its own `Copy`,
// and `CmpFacts` is what turns a branch condition into `(ordering mask,
// canonical operands, signedness, width)` -- the form an edge fact wants.
//

use super::constfold::{
    at_width, cmp_mask, cmp_operand_width, get_cmp_info, mirror_mask, unambiguous_at,
};
use super::{Function, Opcode, PseudoId, PseudoKind};
use crate::float::FloatVal;
use std::collections::HashMap;

/// The constant visible at a pseudo, seeing through `Copy` chains.
///
/// `Function::const_val` answers from the pseudo's *kind*, and a `Copy`
/// target is an ordinary `Reg`. Folding therefore used to stop at the first
/// copy: `a = 2 + 3; b = a * 4;` folded `a` and then stalled, and the
/// fixed-point loop in `opt.rs` spun without finding anything more. Since
/// SSA promotion rewrites a promoted `Load` into a `Copy`, copies are the
/// normal shape of a value that came out of a local, and stopping at them
/// means not folding through a variable at all.
///
/// Sound as a function-wide map, with no dominance query, because SSA
/// invariant I1 (`ir/validate.rs`, `check_single_def`) makes each target's
/// definition unique — so "the Copy that defines %n" is a fact about the
/// function rather than about a program point. That is also why this lives
/// here and not on `Function`: after `ir::lower::lower_module`, phi
/// elimination deliberately creates multi-def copies, and the same walk
/// would be unsound there.
pub(crate) struct ConstMap {
    /// Every `PseudoKind::Val` pseudo's value.
    vals: HashMap<PseudoId, i128>,
    /// Every `PseudoKind::FVal` pseudo's value, kept apart from `vals`
    /// because the two never mix: no rule reads a float as an integer
    /// without going through a conversion opcode that says so.
    fvals: HashMap<PseudoId, FloatVal>,
    /// Target -> (source, the width at which the two are the same value).
    ///
    /// A `Copy` records its own operand width. A `Trunc` belongs here too
    /// and records the width it truncates *to*: a truncation is its operand
    /// read at that width, which is precisely what this field means, and
    /// recording it is what lets `(int)(signed char)200` fold. The value in
    /// the middle means two things, and only the extension that consumes it
    /// says which -- so it is reachable through [`Self::get_at`], which is
    /// told the signedness, and not through [`Self::get`], which is not.
    copies: HashMap<PseudoId, (PseudoId, u32)>,
}

impl ConstMap {
    pub(crate) fn new(func: &Function) -> Self {
        let mut vals = HashMap::new();
        let mut fvals = HashMap::new();
        for p in &func.pseudos {
            match &p.kind {
                PseudoKind::Val(v) => {
                    // I1 should make this unique. If it is not, a release
                    // build has no validator to say so, and folding the wrong
                    // one silently is worse than not folding: poison the
                    // entry.
                    if let Some(prev) = vals.insert(p.id, *v) {
                        if prev != *v {
                            vals.remove(&p.id);
                        }
                    }
                }
                PseudoKind::FVal(v) => {
                    // Poisoned the same way, and on the *encoding*: two
                    // constants that compare equal but are not the same
                    // value (the signed zeros) must not silently merge.
                    if let Some(prev) = fvals.insert(p.id, *v) {
                        if prev.key() != v.key() {
                            fvals.remove(&p.id);
                        }
                    }
                }
                _ => {}
            }
        }

        let mut copies: HashMap<PseudoId, (PseudoId, u32)> = HashMap::new();
        let mut poisoned: Vec<PseudoId> = Vec::new();
        for bb in &func.blocks {
            for insn in &bb.insns {
                if !matches!(insn.op, Opcode::Copy | Opcode::Trunc) || insn.src.len() != 1 {
                    continue;
                }
                if let Some(target) = insn.target {
                    let entry = (insn.src[0], insn.size.max(1));
                    if let Some(prev) = copies.insert(target, entry) {
                        if prev != entry {
                            poisoned.push(target);
                        }
                    }
                }
            }
        }
        // An inline-asm output is a second definition of its pseudo that
        // invariant I1 deliberately exempts, so nothing else notices the
        // pseudo has two defs. A tied operand (`"0"(x)`) is written as a
        // `Copy` into the output pseudo *before* the asm, so following that
        // copy answers with the asm's input where the question was about its
        // result. `sccp::seed` already refuses these for the same reason; the
        // two must agree, because a pass that folds what `sccp` would not is
        // the one that miscompiles.
        for bb in &func.blocks {
            for insn in &bb.insns {
                let Some(ref asm) = insn.asm_data else {
                    continue;
                };
                for out in &asm.outputs {
                    poisoned.push(out.pseudo);
                }
            }
        }

        for id in poisoned {
            copies.remove(&id);
            vals.remove(&id);
            fvals.remove(&id);
        }

        Self {
            vals,
            fvals,
            copies,
        }
    }

    /// Record a simplification this run is about to apply.
    ///
    /// Every collected simplification is applied unconditionally, so this is
    /// a fact rather than a guess. It is what lets `a = 2+3; b = a*4;
    /// c = b+1;` fold all the way down in a single pass instead of needing
    /// one `opt.rs` iteration per level of expression depth.
    /// Note that `target` will hold the integer constant `v`.
    ///
    /// Refused when the value means two things at `size` -- the guard that
    /// stops an ambiguous constant chaining into a second fold.
    pub(crate) fn record_int(&mut self, target: PseudoId, size: u32, v: i128) {
        if unambiguous_at(v, size.max(1)) {
            self.vals.insert(target, v);
        }
    }

    /// Note that `target` will hold the float constant `v`.
    pub(crate) fn record_float(&mut self, target: PseudoId, v: FloatVal) {
        self.fvals.insert(target, v);
    }

    /// Note that `target` will be `src` read at `size` bits.
    pub(crate) fn record_copy(&mut self, target: PseudoId, size: u32, src: PseudoId) {
        self.copies.insert(target, (src, size));
    }

    /// The pseudo `id` ultimately copies from, reading it at `width` bits.
    ///
    /// Two values are the same value when they share a root, which is what
    /// the identity rules (`x - x`, `x & x`, `x == x`) actually need: after
    /// promotion out of memory every use of a local is its own `Copy`, so
    /// `x >> 0 != x` reaches the comparison as two distinct pseudos that are
    /// the same value. Comparing raw ids missed all of them.
    ///
    /// A copy *narrower* than `width` is not followed: it only carries its own
    /// width of the value, so treating it as value-preserving would equate two
    /// pseudos that differ above it.
    pub(crate) fn root(&self, id: PseudoId, width: u32) -> PseudoId {
        let mut cur = id;
        for _ in 0..=self.copies.len() {
            match self.copies.get(&cur) {
                Some(&(src, w)) if w >= width => cur = src,
                _ => return cur,
            }
        }
        cur
    }

    /// The constant at `id`, read at `size` bits in the given signedness.
    ///
    /// For a consumer that *knows* how to read its operands -- a comparison
    /// and a shift take their width and signedness from the opcode -- which
    /// is exactly the information [`Self::get`] refuses to guess. `get` is
    /// still right for everything else, and the two must not be merged: its
    /// guard is what stops an ambiguous value chaining into a second fold.
    ///
    /// Still `None` when the chain narrows *below* `size`, since the value
    /// was truncated before it got here.
    pub(crate) fn get_at(&self, id: PseudoId, size: u32, signed: bool) -> Option<i128> {
        let mut cur = id;
        let mut narrowest: Option<u32> = None;
        for _ in 0..=self.copies.len() {
            if let Some(&v) = self.vals.get(&cur) {
                return match narrowest {
                    Some(w) if w < size => None,
                    _ => Some(at_width(v, size, signed)),
                };
            }
            let &(src, width) = self.copies.get(&cur)?;
            narrowest = Some(narrowest.map_or(width, |w: u32| w.min(width)));
            cur = src;
        }
        None
    }

    /// The float constant at `id`, following `Copy` chains to their source.
    ///
    /// No width guard beyond the one [`Self::root`] applies: unlike an
    /// integer, a float is never narrowed by a `Copy` -- a change of format
    /// is an `FCvtF`, a separate instruction -- so the value at the root is
    /// the value here. What the caller still owes is rounding it to the
    /// format of the instruction consuming it, which a `FloatVal` has not
    /// been subjected to.
    pub(crate) fn fget(&self, id: PseudoId, width: u32) -> Option<FloatVal> {
        self.fvals.get(&self.root(id, width)).copied()
    }

    /// The constant at `id`, following `Copy` chains to their source.
    ///
    /// `None` for anything that is not a compile-time integer constant: a
    /// `Phi`, a `Load` or `Call` result, an `Arg`, an `Undef`, a float, or a
    /// chain whose value does not fit the width it is copied at.
    pub(crate) fn get(&self, id: PseudoId) -> Option<i128> {
        let mut cur = id;
        // The narrowest width the value is observed through bounds what the
        // final answer is allowed to be.
        let mut narrowest: Option<u32> = None;

        // Bounded rather than visited-set: a cycle terminates instead of
        // hanging, without relying on I1 having actually been checked.
        for _ in 0..=self.copies.len() {
            if let Some(&v) = self.vals.get(&cur) {
                return match narrowest {
                    Some(w) if !unambiguous_at(v, w) => None,
                    _ => Some(v),
                };
            }
            let &(src, width) = self.copies.get(&cur)?;
            narrowest = Some(narrowest.map_or(width, |w: u32| w.min(width)));
            cur = src;
        }
        None
    }
}

/// What is known about a pseudo that holds a comparison's result.
///
/// Built once per run, like `ConstMap`, and sound for the same reason: SSA
/// single-def makes "the comparison that defines %n" a fact about the whole
/// function rather than about a program point.
#[derive(Clone, Copy)]
pub(crate) struct CmpFact {
    /// Which of less/equal/greater make it true.
    pub(crate) mask: u8,
    pub(crate) lhs: PseudoId,
    pub(crate) rhs: PseudoId,
    /// A signed and an unsigned comparison over one pair are *not*
    /// comparable: `x < y` and `x > y` read signed are not complementary with
    /// the unsigned forms.
    pub(crate) signed: bool,
    pub(crate) width: u32,
}

/// Every pseudo defined by an integer comparison.
pub(crate) struct CmpFacts {
    facts: HashMap<PseudoId, CmpFact>,
}

impl CmpFacts {
    pub(crate) fn new(func: &Function, consts: &ConstMap) -> Self {
        let mut facts = HashMap::new();
        for bb in &func.blocks {
            for insn in &bb.insns {
                let (Some(target), Some(mask)) = (insn.target, cmp_mask(insn.op)) else {
                    continue;
                };
                if insn.src.len() != 2 {
                    continue;
                }
                let width = cmp_operand_width(insn);
                let signed = get_cmp_info(insn.op).map(|i| i.signed).unwrap_or(true);
                facts.insert(
                    target,
                    CmpFact {
                        mask,
                        lhs: consts.root(insn.src[0], width),
                        rhs: consts.root(insn.src[1], width),
                        signed,
                        width,
                    },
                );
            }
        }
        Self { facts }
    }

    pub(crate) fn get(&self, id: PseudoId) -> Option<CmpFact> {
        self.facts.get(&id).copied()
    }

    /// The fact for whatever `id` ultimately copies from.
    ///
    /// Needed because stripping the boolification leaves a `Copy` of the
    /// comparison in its place, and a later rule asking about that copy would
    /// otherwise learn nothing.
    pub(crate) fn get_through(
        &self,
        consts: &ConstMap,
        id: PseudoId,
        width: u32,
    ) -> Option<CmpFact> {
        self.get(consts.root(id, width))
    }

    /// `other`'s mask expressed over `base`'s operand order, or `None` when
    /// the two are not comparisons of the same pair in the same signedness.
    pub(crate) fn aligned_mask(&self, base: CmpFact, other: CmpFact) -> Option<u8> {
        if base.signed != other.signed || base.width != other.width {
            return None;
        }
        if base.lhs == other.lhs && base.rhs == other.rhs {
            return Some(other.mask);
        }
        if base.lhs == other.rhs && base.rhs == other.lhs {
            return Some(mirror_mask(other.mask));
        }
        None
    }
}
