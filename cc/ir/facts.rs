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
// canonical operands, domain, width)` -- the form an edge fact wants.
//

use super::constfold::{at_width, unambiguous_at, CmpDomain, Outcomes};
use super::{Function, Instruction, Opcode, PseudoId, PseudoKind};
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
        // An inline-asm output's recorded definition says nothing about its
        // value; see `Function::asm_defined_pseudos`.
        poisoned.extend(func.asm_defined_pseudos());

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
    /// A collected simplification states what its target holds, whether or
    /// not the rewrite is then admitted, so this is a fact rather than a
    /// guess. It is what lets `a = 2+3; b = a*4;
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

    /// Whether `a` and `b` are the same value read at `width` bits: whether
    /// they share a [`Self::root`].
    pub(crate) fn same(&self, a: PseudoId, b: PseudoId, width: u32) -> bool {
        self.root(a, width) == self.root(b, width)
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
        match self.walk(id)? {
            (_, Some(w)) if w < size => None,
            (v, _) => Some(at_width(v, size, signed)),
        }
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
        match self.walk(id)? {
            (v, Some(w)) if !unambiguous_at(v, w) => None,
            (v, _) => Some(v),
        }
    }

    /// The integer constant at the end of `id`'s copy chain, with the
    /// narrowest width the chain observes it through -- which bounds what the
    /// final answer is allowed to be, and is `None` for no copy at all.
    ///
    /// Bounded rather than visited-set: a cycle terminates instead of
    /// hanging, without relying on I1 having actually been checked.
    fn walk(&self, id: PseudoId) -> Option<(i128, Option<u32>)> {
        let mut cur = id;
        let mut narrowest: Option<u32> = None;
        for _ in 0..=self.copies.len() {
            if let Some(&v) = self.vals.get(&cur) {
                return Some((v, narrowest));
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
    /// Which outcomes make it true: less, equal, greater, and for a float
    /// also unordered.
    pub(crate) mask: Outcomes,
    pub(crate) lhs: PseudoId,
    pub(crate) rhs: PseudoId,
    pub(crate) domain: CmpDomain,
    pub(crate) width: u32,
}

/// What a 0-or-1 value says about the operands of the comparisons it was
/// computed from.
#[derive(Clone, Copy)]
pub(crate) enum Relation {
    /// The value is this, whatever the operands are.
    Known(bool),
    /// The value is 1 exactly when this comparison holds.
    Holds(CmpFact),
}

/// An instruction that combines 0-or-1 values into another one.
///
/// Recorded by operand rather than evaluated, because whether an operand is
/// a constant is only known when the question is asked: `instcombine`
/// records the constants it folds as it goes.
#[derive(Clone, Copy)]
enum Combinator {
    And(PseudoId, PseudoId),
    Or(PseudoId, PseudoId),
    Select(PseudoId, PseudoId, PseudoId),
    /// `SetEq`, which is logical negation when one side is zero.
    Eq(PseudoId, PseudoId),
    /// `SetNe`, which is the value itself when one side is zero.
    Ne(PseudoId, PseudoId),
}

/// How deep [`CmpFacts::relation`] follows combinators. A C condition nests
/// one level per `&&`, `||` or `!`, so this is far past anything written by
/// hand, and it keeps the walk bounded on generated code.
const MAX_RELATION_DEPTH: u32 = 16;

/// Every pseudo defined by a comparison, integer or float, and every one
/// that combines 0-or-1 values.
pub(crate) struct CmpFacts {
    facts: HashMap<PseudoId, CmpFact>,
    combinators: HashMap<PseudoId, Combinator>,
}

impl CmpFacts {
    pub(crate) fn new(func: &Function, consts: &ConstMap) -> Self {
        let mut cmps = Self {
            facts: HashMap::new(),
            combinators: HashMap::new(),
        };
        for bb in &func.blocks {
            for insn in &bb.insns {
                cmps.record(consts, insn);
            }
        }
        cmps
    }

    /// Learn what `insn` says, if it is a comparison or a combinator: for a
    /// pass that creates one after the facts were built, as `ifconv` does
    /// with each `Select` it makes.
    pub(crate) fn record(&mut self, consts: &ConstMap, insn: &Instruction) {
        let Some(target) = insn.target else {
            return;
        };
        if let Some(c) = Combinator::of(insn.op, &insn.src) {
            self.combinators.insert(target, c);
        }
        let Some((mask, domain)) = Outcomes::of_op(insn.op) else {
            return;
        };
        if insn.src.len() != 2 {
            return;
        }
        let width = insn.operand_width();
        self.facts.insert(
            target,
            CmpFact {
                mask,
                lhs: consts.root(insn.src[0], width),
                rhs: consts.root(insn.src[1], width),
                domain,
                width,
            },
        );
    }

    /// Whether the 0-or-1 value `cond` being `holds` proves the float
    /// operands `lhs` and `rhs`, read at `width`, ordered: neither a NaN.
    ///
    /// `!isunordered(x, y)` proves it, and so does `x < y` holding, or any
    /// combination of comparisons of the pair that excludes *unordered*.
    /// `x < y` *failing* does not, nor does anything about another pair.
    pub(crate) fn proves_ordered(
        &self,
        consts: &ConstMap,
        cond: PseudoId,
        holds: bool,
        (lhs, rhs): (PseudoId, PseudoId),
        width: u32,
    ) -> bool {
        let Some(relation) = self.relation(consts, cond, 1) else {
            return false;
        };
        let Relation::Holds(fact) = (if holds { relation } else { not(relation) }) else {
            return false;
        };
        let pair = CmpFact {
            mask: Outcomes::NONE,
            lhs: consts.root(lhs, width),
            rhs: consts.root(rhs, width),
            domain: CmpDomain::Float,
            width,
        };
        aligned_mask(pair, fact).is_some_and(|m| !m.contains(Outcomes::UN))
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

    /// What the 0-or-1 value `id` says about the operands it was computed
    /// from, or `None` when it is not known to be 0 or 1 or says something
    /// no single comparison expresses.
    ///
    /// This is the mask algebra that decides `(x < y) && (x > y)` and
    /// `isunordered(x, y) || x >= y || x < y` without knowing `x` or `y`:
    /// each comparison is the set of outcomes it is true for, `&&` and `||`
    /// intersect and unite those sets when both sides compare the same pair,
    /// and `!` complements within the domain -- which for a float includes
    /// *unordered*, so `(x < y) || (x >= y)` is correctly left undecided.
    ///
    /// `width` is how `id` itself is read. Everything below it is 0 or 1, and
    /// a 0-or-1 value is the same value through a copy of any width.
    pub(crate) fn relation(&self, consts: &ConstMap, id: PseudoId, width: u32) -> Option<Relation> {
        self.relation_at(consts, id, width, 0)
    }

    fn relation_at(
        &self,
        consts: &ConstMap,
        id: PseudoId,
        width: u32,
        depth: u32,
    ) -> Option<Relation> {
        if depth > MAX_RELATION_DEPTH {
            return None;
        }
        match consts.get(id) {
            Some(0) => return Some(Relation::Known(false)),
            Some(1) => return Some(Relation::Known(true)),
            Some(_) => return None,
            None => {}
        }
        let root = consts.root(id, width);
        let sub = |p: PseudoId| self.relation_at(consts, p, 1, depth + 1);
        let derived = match self.combinators.get(&root).copied() {
            Some(Combinator::And(a, b)) => and(sub(a)?, sub(b)?),
            Some(Combinator::Or(a, b)) => or(sub(a)?, sub(b)?),
            Some(Combinator::Select(c, t, f)) => {
                let c = sub(c)?;
                or(and(c, sub(t)?)?, and(not(c), sub(f)?)?)
            }
            Some(Combinator::Ne(a, b)) => against_zero(consts, a, b).and_then(sub),
            Some(Combinator::Eq(a, b)) => against_zero(consts, a, b).and_then(sub).map(not),
            None => None,
        };
        // A `SetNe`/`SetEq` is a comparison in its own right, and says
        // something about its operands even when they are not 0 or 1.
        derived.or_else(|| self.get(root).map(normalize))
    }
}

impl Combinator {
    fn of(op: Opcode, src: &[PseudoId]) -> Option<Self> {
        Some(match (op, src) {
            (Opcode::And, &[a, b]) => Combinator::And(a, b),
            (Opcode::Or, &[a, b]) => Combinator::Or(a, b),
            (Opcode::Select, &[c, t, f]) => Combinator::Select(c, t, f),
            (Opcode::SetEq, &[a, b]) => Combinator::Eq(a, b),
            (Opcode::SetNe, &[a, b]) => Combinator::Ne(a, b),
            _ => return None,
        })
    }
}

/// The side of `a ? b` that is not the constant zero, if one is.
fn against_zero(consts: &ConstMap, a: PseudoId, b: PseudoId) -> Option<PseudoId> {
    if consts.get(b) == Some(0) {
        Some(a)
    } else if consts.get(a) == Some(0) {
        Some(b)
    } else {
        None
    }
}

/// `f` with the outcomes it cannot have removed, or the constant it is when
/// that leaves it no choice.
///
/// The only outcomes knowable from a fact's shape alone are those of a value
/// compared with itself, and removing them is what makes `x != x` -- "`x` is
/// a NaN" -- combinable at all: see [`pair_of_self_tests`].
fn normalize(f: CmpFact) -> Relation {
    let possible = if f.lhs == f.rhs {
        f.domain.reflexive()
    } else {
        f.domain.all()
    };
    match f.mask.decide(possible) {
        Some(v) => Relation::Known(v),
        None => Relation::Holds(CmpFact {
            mask: f.mask & possible,
            ..f
        }),
    }
}

/// `other`'s mask over `base`'s operand order, or `None` when the two are
/// not comparisons of the same pair in the same domain at the same width.
fn aligned_mask(base: CmpFact, other: CmpFact) -> Option<Outcomes> {
    if base.domain != other.domain || base.width != other.width {
        return None;
    }
    if base.lhs == other.lhs && base.rhs == other.rhs {
        return Some(other.mask);
    }
    if base.lhs == other.rhs && base.rhs == other.lhs {
        return Some(other.mask.mirror());
    }
    None
}

/// The value a float self-comparison tests, when it holds exactly when that
/// value is a NaN (`want == Outcomes::UN`) or exactly when it is not
/// (`Outcomes::EQ`). `f` is normalized, so `x != x` arrives here as
/// `Outcomes::UN`.
fn self_test(f: CmpFact, want: Outcomes) -> Option<PseudoId> {
    (f.domain == CmpDomain::Float && f.lhs == f.rhs && f.mask == want).then_some(f.lhs)
}

/// Both operands tested one at a time, as `__builtin_isunordered` lowers:
/// `x != x || y != y` (`want == Outcomes::UN`) is `x` and `y` unordered,
/// and `x == x && y == y` (`Outcomes::EQ`) is the two ordered.
fn pair_of_self_tests(a: CmpFact, b: CmpFact, want: Outcomes) -> Option<Relation> {
    if a.width != b.width {
        return None;
    }
    let (x, y) = (self_test(a, want)?, self_test(b, want)?);
    let mask = if want == Outcomes::UN {
        Outcomes::UN
    } else {
        Outcomes::ORDERED
    };
    Some(normalize(CmpFact {
        mask,
        lhs: x,
        rhs: y,
        ..a
    }))
}

fn and(a: Relation, b: Relation) -> Option<Relation> {
    match (a, b) {
        (Relation::Known(false), _) | (_, Relation::Known(false)) => Some(Relation::Known(false)),
        (Relation::Known(true), r) | (r, Relation::Known(true)) => Some(r),
        (Relation::Holds(x), Relation::Holds(y)) => match aligned_mask(x, y) {
            Some(m) => Some(normalize(CmpFact {
                mask: x.mask & m,
                ..x
            })),
            None => pair_of_self_tests(x, y, Outcomes::EQ),
        },
    }
}

fn or(a: Relation, b: Relation) -> Option<Relation> {
    match (a, b) {
        (Relation::Known(true), _) | (_, Relation::Known(true)) => Some(Relation::Known(true)),
        (Relation::Known(false), r) | (r, Relation::Known(false)) => Some(r),
        (Relation::Holds(x), Relation::Holds(y)) => match aligned_mask(x, y) {
            Some(m) => Some(normalize(CmpFact {
                mask: x.mask | m,
                ..x
            })),
            None => pair_of_self_tests(x, y, Outcomes::UN),
        },
    }
}

fn not(a: Relation) -> Relation {
    match a {
        Relation::Known(v) => Relation::Known(!v),
        Relation::Holds(f) => normalize(CmpFact {
            mask: f.mask.complement(f.domain),
            ..f
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BasicBlock, BasicBlockId, Instruction, Pseudo};
    use crate::target::Target;
    use crate::types::TypeTable;

    /// `%1 = $v`; `%2 = copy.<copy_bits> %1`; `%3 = trunc.<trunc_bits> %2`;
    /// `%4 = setlt.32 %2, %5` and `%6 = copy.32 %4`, with `%5` an argument.
    fn chain(v: i128, copy_bits: u32, trunc_bits: u32) -> Function {
        let types = TypeTable::new(&Target::host());
        let int = types.int_id;
        let mut f = Function::new("f", int);
        f.add_pseudo(Pseudo::val(PseudoId(1), v));
        f.add_pseudo(Pseudo::arg(PseudoId(5), 0));
        let mut bb = BasicBlock::new(BasicBlockId(0));
        bb.add_insn(Instruction::new(Opcode::Entry));
        let mut copy = Instruction::new(Opcode::Copy)
            .with_target(PseudoId(2))
            .with_src(PseudoId(1));
        copy.size = copy_bits;
        bb.add_insn(copy);
        let mut trunc = Instruction::new(Opcode::Trunc)
            .with_target(PseudoId(3))
            .with_src(PseudoId(2));
        trunc.size = trunc_bits;
        trunc.src_size = copy_bits;
        bb.add_insn(trunc);
        bb.add_insn(Instruction::compare(
            Opcode::SetLt,
            PseudoId(4),
            (PseudoId(2), PseudoId(5)),
            (int, 32),
            (int, 32),
        ));
        let mut copy = Instruction::new(Opcode::Copy)
            .with_target(PseudoId(6))
            .with_src(PseudoId(4));
        copy.size = 32;
        bb.add_insn(copy);
        bb.add_insn(Instruction::ret(None));
        f.add_block(bb);
        f.entry = BasicBlockId(0);
        f
    }

    /// A copy is the same value only at or below its own width: a narrower
    /// one carries part of the value, and following it would equate two
    /// pseudos that differ above it.
    #[test]
    fn root_follows_a_copy_only_at_its_width() {
        let f = chain(5, 32, 8);
        let m = ConstMap::new(&f);
        assert_eq!(m.root(PseudoId(2), 32), PseudoId(1));
        assert_eq!(
            m.root(PseudoId(3), 8),
            PseudoId(1),
            "a trunc is its operand at its width"
        );
        assert_eq!(m.root(PseudoId(3), 32), PseudoId(3), "but no wider");
    }

    /// `get_at` reads a constant as the consumer says; `get`, which is told
    /// nothing, refuses one that means two things at the width it passed.
    #[test]
    fn a_constant_is_read_at_the_width_and_sign_asked() {
        let f = chain(200, 32, 8);
        let m = ConstMap::new(&f);
        assert_eq!(m.get(PseudoId(2)), Some(200));
        assert_eq!(
            m.get_at(PseudoId(3), 8, true),
            Some(-56),
            "(signed char)200"
        );
        assert_eq!(m.get_at(PseudoId(3), 8, false), Some(200));
        assert_eq!(m.get(PseudoId(3)), None, "200 at 8 bits is -56 or 200");
        assert_eq!(
            m.get_at(PseudoId(3), 32, true),
            None,
            "truncated below the width asked"
        );
    }

    /// A recorded fold is believed only when it reads one way at its width.
    #[test]
    fn record_int_refuses_an_ambiguous_constant() {
        let f = chain(1, 32, 8);
        let mut m = ConstMap::new(&f);
        m.record_int(PseudoId(9), 8, 0xff);
        assert_eq!(m.get(PseudoId(9)), None);
        m.record_int(PseudoId(9), 8, 5);
        assert_eq!(m.get(PseudoId(9)), Some(5));
    }

    /// A comparison's operands are recorded by their roots, so a copy of an
    /// operand is the same operand, and `get_through` reads the fact
    /// through a copy of the result.
    #[test]
    fn a_comparison_is_recorded_by_its_operands_roots() {
        let f = chain(7, 32, 8);
        let consts = ConstMap::new(&f);
        let cmps = CmpFacts::new(&f, &consts);
        let fact = cmps.get(PseudoId(4)).expect("setlt records a fact");
        assert_eq!(fact.lhs, PseudoId(1), "through the copy to the constant");
        assert_eq!(fact.rhs, PseudoId(5));
        assert_eq!(fact.mask, Outcomes::LT);
        assert_eq!(fact.width, 32);
        assert!(matches!(fact.domain, CmpDomain::Int { signed: true }));
        assert!(cmps.get(PseudoId(6)).is_none());
        assert_eq!(
            cmps.get_through(&consts, PseudoId(6), 32).map(|f| f.mask),
            Some(Outcomes::LT)
        );
    }

    /// An inline-asm output is defined twice, once by the asm, so a copy
    /// into it says nothing about its value.
    #[test]
    fn an_asm_output_is_no_copy() {
        let mut f = chain(7, 32, 8);
        let mut asm = Instruction::new(Opcode::Asm);
        asm.extra_mut().asm_data = Some(Box::new(crate::ir::AsmData {
            template: String::new(),
            outputs: vec![crate::ir::AsmConstraint::new(
                PseudoId(2),
                "=r",
                crate::target::Arch::X86_64,
                32,
            )],
            inputs: vec![],
            clobbers: vec![],
            goto_labels: vec![],
        }));
        f.blocks[0].insns.insert(2, asm);
        let m = ConstMap::new(&f);
        assert_eq!(m.get(PseudoId(2)), None);
        assert_eq!(m.root(PseudoId(2), 32), PseudoId(2));
    }
}
