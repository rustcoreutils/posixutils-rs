//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A set of W-bit integers, held as one interval that may wrap.
//
// Nothing here knows about the IR: no `Function`, no `Instruction`, no
// `TypeTable`. It is arithmetic on sets of machine integers, which is why it
// can be tested exhaustively -- at four bits there are 242 representable sets
// and every operation is checked against brute force over all of them.
//
// Two decisions shape everything else.
//
// **The interval may wrap**, so `[lo, hi)` with `lo > hi` means the set runs
// off the top and continues from zero. That is what lets one representation
// answer both signed and unsigned questions exactly: `x != 0` at 8 bits is
// `[1, 0)`, the whole space minus one point, which no non-wrapping interval
// can express without throwing the information away.
//
// **Signedness is a question you ask, not a property a set has.** The same
// set is `[-1, 1]` read signed and `{0xFF, 0x00, 0x01}` read unsigned, and
// both readings are correct. `signed_view` translates the space by half its
// size, which carries wrapped intervals to wrapped intervals and signed order
// to unsigned order; every signed operation is that translation, the unsigned
// routine, and the translation back. One body of case analysis rather than
// two is the main defence against the class of bug that makes wrapped
// intervals frightening.
//

use super::constfold::at_width;
use std::cmp::Ordering;

/// The low `width` bits set.
/// The signed range a `width`-bit value spans.
///
/// Not `-(1i128 << (w - 1))`: at `w == 128` that shift yields `i128::MIN`
/// and negating it overflows, which panics a debug build -- and CI runs the
/// tests in debug.
const fn signed_bounds(width: u32) -> (i128, i128) {
    if width >= 128 {
        (i128::MIN, i128::MAX)
    } else {
        (-(1i128 << (width - 1)), (1i128 << (width - 1)) - 1)
    }
}

const fn mask(width: u32) -> u128 {
    if width >= 128 {
        u128::MAX
    } else {
        (1u128 << width) - 1
    }
}

/// Reinterpret the low `width` bits of `v` as a signed value.
fn as_signed(v: u128, width: u32) -> i128 {
    at_width(v as i128, width, true)
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Kind {
    Empty,
    Full,
    /// `[lo, hi)`, wrapping when `lo > hi`. Both endpoints are masked to the
    /// width, and `lo != hi` -- the two coinciding would be a second spelling
    /// of `Empty` or `Full`.
    Half {
        lo: u128,
        hi: u128,
    },
}

/// A set of `width`-bit integers.
///
/// `Empty` and `Full` are their own variants rather than being encoded as
/// `[0,0)` and `[MAX,MAX)` the way LLVM's `ConstantRange` does, and `lo ==
/// hi` is forbidden inside `Half`. That costs a byte and buys the property
/// the solver depends on: **`PartialEq` is set equality**. A solver asks "did
/// this cell move?" with `!=`, so a second spelling of the same set would
/// look like movement and the worklist would never drain.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct Range {
    width: u32,
    kind: Kind,
}

impl Range {
    /// A usable width, or `None`.
    ///
    /// Width zero is real in this IR -- a `Cbr` carries it, a `Copy` can --
    /// and nothing here guesses one. A caller with no width has no range.
    pub(crate) fn at(width: u32) -> Option<u32> {
        (1..=128).contains(&width).then_some(width)
    }

    pub(crate) fn empty(width: u32) -> Range {
        Range {
            width,
            kind: Kind::Empty,
        }
    }

    pub(crate) fn full(width: u32) -> Range {
        Range {
            width,
            kind: Kind::Full,
        }
    }

    /// `[lo, hi)`. Coinciding endpoints mean the whole space, which is what
    /// every internal producer of this shape intends; a caller that means
    /// the empty set says so.
    fn half_open(width: u32, lo: u128, hi: u128) -> Range {
        let (lo, hi) = (lo & mask(width), hi & mask(width));
        if lo == hi {
            return Range::full(width);
        }
        Range {
            width,
            kind: Kind::Half { lo, hi },
        }
    }

    /// `[lo, hi]`, inclusive of both ends.
    pub(crate) fn inclusive(width: u32, lo: u128, hi: u128) -> Range {
        let m = mask(width);
        let (lo, hi) = (lo & m, hi & m);
        // An inclusive range whose end is one below its start is everything.
        if hi.wrapping_add(1) & m == lo {
            return Range::full(width);
        }
        Range::half_open(width, lo, hi.wrapping_add(1) & m)
    }

    pub(crate) fn singleton(width: u32, v: u128) -> Range {
        let m = mask(width);
        let v = v & m;
        Range {
            width,
            kind: Kind::Half {
                lo: v,
                hi: v.wrapping_add(1) & m,
            },
        }
    }

    /// The one bridge from the IR's raw `i128` bit patterns into this module.
    ///
    /// Goes through `constfold::at_width`, so the convention that
    /// `(int)0xFFFFFFFFu` is stored as `4294967295` rather than `-1` is
    /// honoured in exactly one place. Nothing else here accepts an `i128`.
    pub(crate) fn from_const(width: u32, v: i128) -> Range {
        Range::singleton(width, at_width(v, width, false) as u128)
    }

    pub(crate) fn width(&self) -> u32 {
        self.width
    }

    pub(crate) fn is_empty(&self) -> bool {
        matches!(self.kind, Kind::Empty)
    }

    pub(crate) fn is_full(&self) -> bool {
        matches!(self.kind, Kind::Full)
    }

    pub(crate) fn contains(&self, v: u128) -> bool {
        let v = v & mask(self.width);
        match self.kind {
            Kind::Empty => false,
            Kind::Full => true,
            Kind::Half { lo, hi } if lo < hi => v >= lo && v < hi,
            Kind::Half { lo, hi } => v >= lo || v < hi,
        }
    }

    /// How many values are in the set. `None` means all `2^width` of them,
    /// which does not fit a `u128` at width 128.
    fn size(&self) -> Option<u128> {
        match self.kind {
            Kind::Empty => Some(0),
            Kind::Full => None,
            Kind::Half { lo, hi } => Some(hi.wrapping_sub(lo) & mask(self.width)),
        }
    }

    pub(crate) fn single_value(&self) -> Option<u128> {
        match self.kind {
            Kind::Half { lo, .. } if self.size() == Some(1) => Some(lo),
            _ => None,
        }
    }

    /// The smallest value, read unsigned.
    ///
    /// A set that wraps runs `lo..=MAX` and then `0..hi`, so it contains
    /// zero -- **unless `hi` is zero**, when the second piece is empty and
    /// the set is really the contiguous run `lo..=MAX`. That case arises
    /// constantly rather than rarely: it is what `signed_view` produces for
    /// every non-negative signed range, so missing it makes `signed_min` of
    /// `[1, INT_MAX]` answer `INT_MIN`.
    pub(crate) fn unsigned_min(&self) -> u128 {
        match self.kind {
            Kind::Empty | Kind::Full => 0,
            Kind::Half { lo, hi } if lo < hi || hi == 0 => lo,
            Kind::Half { .. } => 0,
        }
    }

    pub(crate) fn unsigned_max(&self) -> u128 {
        match self.kind {
            Kind::Empty => 0,
            Kind::Full => mask(self.width),
            Kind::Half { lo, hi } if lo < hi => hi - 1,
            Kind::Half { .. } => mask(self.width),
        }
    }

    /// The same set, with the signed order laid out as the unsigned order.
    ///
    /// Adding half the space toggles the sign bit and drops the carry, so it
    /// is a translation -- and a translation maps a wrapped half-open
    /// interval to a wrapped half-open interval. It also sends the signed
    /// minimum to 0 and the signed maximum to `MAX`, which is what makes the
    /// unsigned routines answer signed questions.
    ///
    /// An involution: applying it twice is the identity.
    fn signed_view(&self) -> Range {
        let bit = 1u128 << (self.width - 1);
        match self.kind {
            Kind::Half { lo, hi } => Range {
                width: self.width,
                kind: Kind::Half {
                    lo: (lo ^ bit) & mask(self.width),
                    hi: (hi ^ bit) & mask(self.width),
                },
            },
            _ => *self,
        }
    }

    pub(crate) fn signed_min(&self) -> i128 {
        let bit = 1u128 << (self.width - 1);
        as_signed(self.signed_view().unsigned_min() ^ bit, self.width)
    }

    pub(crate) fn signed_max(&self) -> i128 {
        let bit = 1u128 << (self.width - 1);
        as_signed(self.signed_view().unsigned_max() ^ bit, self.width)
    }

    // Lattice

    /// The smallest single interval containing both.
    ///
    /// Never smaller than either argument, so a solver's cells only grow.
    /// Two intervals that overlap on neither end have two candidate hulls;
    /// the choice is deterministic so that two builds agree.
    pub(crate) fn union(&self, other: &Range) -> Range {
        if self.width != other.width {
            return Range::full(self.width);
        }
        match (self.kind, other.kind) {
            (Kind::Empty, _) => *other,
            (_, Kind::Empty) => *self,
            (Kind::Full, _) | (_, Kind::Full) => Range::full(self.width),
            (Kind::Half { lo: a1, hi: a2 }, Kind::Half { lo: b1, hi: b2 }) => {
                if self.covers(other) {
                    return *self;
                }
                if other.covers(self) {
                    return *other;
                }
                let c1 = Range::half_open(self.width, a1, b2);
                let c2 = Range::half_open(self.width, b1, a2);
                let ok1 = c1.covers(self) && c1.covers(other);
                let ok2 = c2.covers(self) && c2.covers(other);
                match (ok1, ok2) {
                    (true, false) => c1,
                    (false, true) => c2,
                    (true, true) => match c1.size().cmp(&c2.size()) {
                        // `None` is the full set and sorts above every
                        // `Some`, so the smaller hull wins naturally.
                        Ordering::Less => c1,
                        Ordering::Greater => c2,
                        Ordering::Equal => c1,
                    },
                    (false, false) => Range::full(self.width),
                }
            }
        }
    }

    /// Is every value of `other` in `self`?
    ///
    /// Answered by translating so `self` starts at zero, which turns a
    /// question about two arcs of a circle into one about two segments of a
    /// line. "Contains both ends and is at least as large" is *not* the same
    /// question and is wrong here: at four bits `{2..15, 0}` contains both
    /// ends of `{0, 1, 2}` and is far larger, yet does not contain the 1
    /// between them.
    fn covers(&self, other: &Range) -> bool {
        match (self.kind, other.kind) {
            (_, Kind::Empty) => true,
            (Kind::Full, _) => true,
            (Kind::Empty, _) => false,
            (_, Kind::Full) => false,
            (Kind::Half { lo: slo, .. }, Kind::Half { lo: olo, .. }) => {
                let (Some(s), Some(len)) = (self.size(), other.size()) else {
                    return false;
                };
                let start = olo.wrapping_sub(slo) & mask(self.width);
                start.checked_add(len).is_some_and(|end| end <= s)
            }
        }
    }

    /// The exact intersection, or `None` when it is not a single interval.
    ///
    /// Two wrapped intervals can meet in two disjoint pieces -- at four bits,
    /// `{2..9}` and `{8..15, 0..3}` share `{2,3}` and `{8,9}`. LLVM returns a
    /// chosen superset there. A superset is sound as an *answer* but it need
    /// not be a subset of either argument, so using it to narrow a lattice
    /// cell can move that cell sideways rather than down, and the solver can
    /// oscillate. Refusing costs nothing: the one caller keeps what it had.
    pub(crate) fn intersect_exact(&self, other: &Range) -> Option<Range> {
        if self.width != other.width {
            return None;
        }
        match (self.kind, other.kind) {
            (Kind::Empty, _) | (_, Kind::Empty) => Some(Range::empty(self.width)),
            (Kind::Full, _) => Some(*other),
            (_, Kind::Full) => Some(*self),
            (Kind::Half { .. }, Kind::Half { .. }) => {
                if self.covers(other) {
                    return Some(*other);
                }
                if other.covers(self) {
                    return Some(*self);
                }
                // Partial overlap. The result is one interval exactly when
                // one argument's start lies in the other and the ends do not
                // also cross a second time.
                let a = self.pieces();
                let b = other.pieces();
                let mut hits: Vec<(u128, u128)> = Vec::new();
                for (al, ah) in &a {
                    for (bl, bh) in &b {
                        let lo = (*al).max(*bl);
                        let hi = (*ah).min(*bh);
                        if lo <= hi {
                            hits.push((lo, hi));
                        }
                    }
                }
                match hits.len() {
                    0 => Some(Range::empty(self.width)),
                    1 => Some(Range::inclusive(self.width, hits[0].0, hits[0].1)),
                    // Two pieces that meet at the wrap point are still one
                    // interval, written the way round the circle.
                    2 => {
                        let (mut p, mut q) = (hits[0], hits[1]);
                        if q.0 < p.0 {
                            std::mem::swap(&mut p, &mut q);
                        }
                        let top = mask(self.width);
                        if p.0 == 0 && q.1 == top {
                            Some(Range::inclusive(self.width, q.0, p.1))
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
        }
    }

    /// The set as one or two non-wrapping **inclusive** `[lo, hi]` pieces,
    /// for the routines that are easier to write on a line than on a circle.
    ///
    /// Inclusive rather than half-open because the exclusive end of the
    /// whole space is `2^width`, which a `u128` cannot hold at width 128:
    /// `mask(128).wrapping_add(1)` is `0`, so `Full` came back as the empty
    /// piece `[0, 0)` and a wrapped set produced the inverted `(lo, 0)`.
    /// `intersect_exact` then silently dropped every piece touching the top
    /// of the space, turning its "exact or refuse" contract into "too
    /// small" -- and a caller that narrows to a wrong singleton folds.
    fn pieces(&self) -> Vec<(u128, u128)> {
        let top = mask(self.width);
        match self.kind {
            Kind::Empty => Vec::new(),
            Kind::Full => vec![(0, top)],
            Kind::Half { lo, hi } if lo < hi => vec![(lo, hi - 1)],
            Kind::Half { lo, hi } => {
                let mut v = vec![(lo, top)];
                if hi > 0 {
                    v.push((0, hi - 1));
                }
                v
            }
        }
    }
}

// Arithmetic
//
// Every routine answers `Full` rather than guessing when it cannot be
// precise, and `Empty` propagates. Each is checked at four bits against
// brute force over every pair of sets and every pair of members, which is a
// complete proof at that width -- and every bug these can have is
// width-independent in form.

impl Range {
    /// The number of values, as a `u128` that saturates at the whole space.
    /// Only for the overflow guards below, which compare against `2^width`.
    fn span(&self) -> u128 {
        match self.size() {
            Some(n) => n,
            None => mask(self.width),
        }
    }

    /// Would a result of `self`'s and `other`'s combined size wrap all the
    /// way round? Addition and subtraction both produce a set of size
    /// `|a| + |b| - 1`.
    fn sum_overflows(&self, other: &Range) -> bool {
        if self.is_full() || other.is_full() {
            return true;
        }
        let (a, b) = (self.span(), other.span());
        match a.checked_add(b) {
            // `mask + 1` is `2^width`, computed without overflowing at 128.
            Some(n) => n.saturating_sub(1) > mask(self.width),
            None => true,
        }
    }

    fn binary_guard(&self, other: &Range) -> Option<Range> {
        if self.width != other.width {
            return Some(Range::full(self.width));
        }
        if self.is_empty() || other.is_empty() {
            return Some(Range::empty(self.width));
        }
        None
    }

    pub(crate) fn add(&self, other: &Range) -> Range {
        if let Some(r) = self.binary_guard(other) {
            return r;
        }
        if self.sum_overflows(other) {
            return Range::full(self.width);
        }
        let (Kind::Half { lo: a1, hi: a2 }, Kind::Half { lo: b1, hi: b2 }) =
            (self.kind, other.kind)
        else {
            return Range::full(self.width);
        };
        Range::half_open(
            self.width,
            a1.wrapping_add(b1),
            a2.wrapping_add(b2).wrapping_sub(1),
        )
    }

    pub(crate) fn sub(&self, other: &Range) -> Range {
        if let Some(r) = self.binary_guard(other) {
            return r;
        }
        if self.sum_overflows(other) {
            return Range::full(self.width);
        }
        let (Kind::Half { lo: a1, hi: a2 }, Kind::Half { lo: b1, hi: b2 }) =
            (self.kind, other.kind)
        else {
            return Range::full(self.width);
        };
        Range::half_open(
            self.width,
            a1.wrapping_sub(b2).wrapping_add(1),
            a2.wrapping_sub(b1),
        )
    }

    /// One body, not a second case analysis.
    pub(crate) fn neg(&self) -> Range {
        Range::singleton(self.width, 0).sub(self)
    }

    /// Exact: `~x` is `MAX - x`, an order-reversing bijection, so it maps an
    /// interval to an interval with no loss at all.
    pub(crate) fn not(&self) -> Range {
        match self.kind {
            Kind::Half { lo, hi } => {
                let m = mask(self.width);
                Range::half_open(
                    self.width,
                    !hi.wrapping_sub(1) & m,
                    (!lo & m).wrapping_add(1),
                )
            }
            _ => *self,
        }
    }

    /// The four corner products, and `Full` the moment any of them leaves
    /// the width.
    ///
    /// Deliberately not LLVM's `multiply`, whose precision comes from
    /// intersecting an unsigned hull with a signed one plus special cases
    /// for the doubly-wrapped shapes -- which is where its historical bugs
    /// lived. Nothing here needs that precision.
    pub(crate) fn mul(&self, other: &Range) -> Range {
        if let Some(r) = self.binary_guard(other) {
            return r;
        }
        let w = self.width;
        // Unsigned, both operands one non-wrapped run.
        if self.pieces().len() == 1 && other.pieces().len() == 1 {
            let (a1, a2) = (self.unsigned_min(), self.unsigned_max());
            let (b1, b2) = (other.unsigned_min(), other.unsigned_max());
            if let (Some(lo), Some(hi)) = (a1.checked_mul(b1), a2.checked_mul(b2)) {
                if hi <= mask(w) {
                    return Range::inclusive(w, lo, hi);
                }
            }
        }
        // Signed, both operands one non-wrapped run in the signed view.
        if self.signed_view().pieces().len() == 1 && other.signed_view().pieces().len() == 1 {
            let corners = [
                self.signed_min().checked_mul(other.signed_min()),
                self.signed_min().checked_mul(other.signed_max()),
                self.signed_max().checked_mul(other.signed_min()),
                self.signed_max().checked_mul(other.signed_max()),
            ];
            if corners.iter().all(|c| c.is_some()) {
                let vals: Vec<i128> = corners.into_iter().map(|c| c.unwrap()).collect();
                let (lo, hi) = (*vals.iter().min().unwrap(), *vals.iter().max().unwrap());
                let (smin, smax) = signed_bounds(w);
                if lo >= smin && hi <= smax {
                    return Range::inclusive(
                        w,
                        at_width(lo, w, false) as u128,
                        at_width(hi, w, false) as u128,
                    );
                }
            }
        }
        Range::full(w)
    }

    /// Unsigned division. A divisor range containing zero answers `Full`:
    /// c17 does not assume undefined behaviour away, and
    /// `constfold::eval_divmod` already refuses a zero divisor rather than
    /// inventing a result. Disagreeing here would make `-O2` and `-O0`
    /// differ on a program that really does divide by zero.
    pub(crate) fn udiv(&self, other: &Range) -> Range {
        if let Some(r) = self.binary_guard(other) {
            return r;
        }
        if other.contains(0) {
            return Range::full(self.width);
        }
        let (amin, amax) = (self.unsigned_min(), self.unsigned_max());
        let (bmin, bmax) = (other.unsigned_min(), other.unsigned_max());
        if bmin == 0 || bmax == 0 {
            return Range::full(self.width);
        }
        Range::inclusive(self.width, amin / bmax, amax / bmin)
    }

    pub(crate) fn umod(&self, other: &Range) -> Range {
        if let Some(r) = self.binary_guard(other) {
            return r;
        }
        if other.contains(0) {
            return Range::full(self.width);
        }
        let hi = self
            .unsigned_max()
            .min(other.unsigned_max().saturating_sub(1));
        Range::inclusive(self.width, 0, hi)
    }

    /// Bitwise operations say almost nothing about interval endpoints, so
    /// these are bounds rather than results. They are stated in the unsigned
    /// view, where a wrapped operand degenerates to `[0, MAX]` -- sound by
    /// construction, and the reason no extra guard is needed.
    pub(crate) fn and(&self, other: &Range) -> Range {
        if let Some(r) = self.binary_guard(other) {
            return r;
        }
        let hi = self.unsigned_max().min(other.unsigned_max());
        Range::inclusive(self.width, 0, hi)
    }

    pub(crate) fn or(&self, other: &Range) -> Range {
        if let Some(r) = self.binary_guard(other) {
            return r;
        }
        let lo = self.unsigned_min().max(other.unsigned_min());
        let hi = fill_ones(self.unsigned_max() | other.unsigned_max()).min(mask(self.width));
        Range::inclusive(self.width, lo, hi)
    }

    pub(crate) fn xor(&self, other: &Range) -> Range {
        if let Some(r) = self.binary_guard(other) {
            return r;
        }
        // `x ^ MAX` is `~x`, which is exact.
        if other.single_value() == Some(mask(self.width)) {
            return self.not();
        }
        if self.single_value() == Some(mask(self.width)) {
            return other.not();
        }
        let hi = fill_ones(self.unsigned_max() | other.unsigned_max()).min(mask(self.width));
        Range::inclusive(self.width, 0, hi)
    }

    /// The shift count must be one known value inside the width. Anything
    /// else is undefined behaviour, which `constfold::eval_shift` refuses
    /// and this must refuse too.
    fn shift_amount(&self, amount: &Range) -> Option<u32> {
        let k = amount.single_value()?;
        (k < self.width as u128).then_some(k as u32)
    }

    pub(crate) fn shl(&self, amount: &Range) -> Range {
        if let Some(r) = self.binary_guard(amount) {
            return r;
        }
        let Some(k) = self.shift_amount(amount) else {
            return Range::full(self.width);
        };
        if self.pieces().len() != 1 {
            return Range::full(self.width);
        }
        let (lo, hi) = (self.unsigned_min(), self.unsigned_max());
        // Nothing may shift out of the width, or the endpoints stop
        // bounding the set.
        if hi > mask(self.width) >> k {
            return Range::full(self.width);
        }
        Range::inclusive(self.width, lo << k, hi << k)
    }

    /// Sound and useful even on a wrapped operand: a wrapped set degenerates
    /// to `[0, MAX]`, and `MAX >> k` is still a real bound.
    pub(crate) fn lshr(&self, amount: &Range) -> Range {
        if let Some(r) = self.binary_guard(amount) {
            return r;
        }
        let Some(k) = self.shift_amount(amount) else {
            return Range::full(self.width);
        };
        Range::inclusive(
            self.width,
            self.unsigned_min() >> k,
            self.unsigned_max() >> k,
        )
    }

    pub(crate) fn ashr(&self, amount: &Range) -> Range {
        if let Some(r) = self.binary_guard(amount) {
            return r;
        }
        let Some(k) = self.shift_amount(amount) else {
            return Range::full(self.width);
        };
        let w = self.width;
        Range::inclusive(
            w,
            at_width(self.signed_min() >> k, w, false) as u128,
            at_width(self.signed_max() >> k, w, false) as u128,
        )
    }

    /// Exact, and with no case analysis: reducing modulo `2^to` maps a
    /// half-open wrapped interval to a half-open wrapped interval exactly
    /// when the set is no larger than the destination space.
    pub(crate) fn trunc(&self, to: u32) -> Range {
        if Range::at(to).is_none() || to > self.width {
            return Range::full(to.max(1));
        }
        match self.kind {
            Kind::Empty => Range::empty(to),
            Kind::Full => Range::full(to),
            Kind::Half { lo, hi } => {
                let room = if to >= 128 { None } else { Some(1u128 << to) };
                match (self.size(), room) {
                    (Some(n), Some(space)) if n < space => Range::half_open(to, lo, hi),
                    (Some(_), None) => Range::half_open(to, lo, hi),
                    _ => Range::full(to),
                }
            }
        }
    }

    /// Zero extension **never** answers `Full`: whatever the source, the
    /// result lands in the low `self.width` bits of the destination. That is
    /// what carries a bound across a widening, and the degenerate case
    /// `[0, mask(src)]` is the one that correctly fails to prove anything.
    pub(crate) fn zext(&self, to: u32) -> Range {
        if Range::at(to).is_none() || to < self.width {
            return Range::full(to.max(1));
        }
        match self.kind {
            Kind::Empty => Range::empty(to),
            _ if self.pieces().len() == 1 && !self.is_full() => {
                Range::inclusive(to, self.unsigned_min(), self.unsigned_max())
            }
            _ => Range::inclusive(to, 0, mask(self.width)),
        }
    }

    pub(crate) fn sext(&self, to: u32) -> Range {
        if Range::at(to).is_none() || to < self.width {
            return Range::full(to.max(1));
        }
        match self.kind {
            Kind::Empty => Range::empty(to),
            _ => {
                let (lo, hi) = if self.signed_view().pieces().len() == 1 && !self.is_full() {
                    (self.signed_min(), self.signed_max())
                } else {
                    signed_bounds(self.width)
                };
                Range::inclusive(
                    to,
                    at_width(lo, to, false) as u128,
                    at_width(hi, to, false) as u128,
                )
            }
        }
    }
}

/// `v` with every bit below its highest set bit also set.
fn fill_ones(v: u128) -> u128 {
    if v == 0 {
        return 0;
    }
    u128::MAX >> v.leading_zeros()
}

// Predicates

/// Every `x` for which `x <mask> y` holds for **some** `y` in `other`.
///
/// The *allowed* region, not the *satisfying* one, and the distinction is
/// the most dangerous thing in this file. On a branch edge all that is known
/// is that the comparison held for the one `y` that actually occurred, which
/// is somewhere in `other`; the set of `x` consistent with that is the union
/// over `y`, not the intersection. Returning the intersection would narrow a
/// range below the truth, prove a live comparison constant, and delete a
/// live branch arm -- in real code the error path or the bounds check.
///
/// `mask` is `constfold`'s `CMP_LT | CMP_EQ | CMP_GT` encoding.
pub(crate) fn allowed_by_predicate(mask: u8, signed: bool, other: &Range) -> Range {
    if signed {
        allowed_unsigned(mask, &other.signed_view()).signed_view()
    } else {
        allowed_unsigned(mask, other)
    }
}

fn allowed_unsigned(mask: u8, other: &Range) -> Range {
    use super::constfold::{CMP_ALL, CMP_EQ, CMP_GT, CMP_LT};
    let w = other.width;
    if other.is_empty() || mask == 0 {
        return Range::empty(w);
    }
    let (omin, omax) = (other.unsigned_min(), other.unsigned_max());
    let top = mask_of(w);
    match mask & CMP_ALL {
        CMP_EQ => *other,
        CMP_LT if omax == 0 => Range::empty(w),
        CMP_LT => Range::inclusive(w, 0, omax - 1),
        m if m == CMP_LT | CMP_EQ => Range::inclusive(w, 0, omax),
        CMP_GT if omin == top => Range::empty(w),
        CMP_GT => Range::inclusive(w, omin + 1, top),
        m if m == CMP_GT | CMP_EQ => Range::inclusive(w, omin, top),
        // `x != y` for some `y`: everything, unless `other` is one value,
        // in which case the exact complement -- which only a wrapped
        // interval can say.
        m if m == CMP_LT | CMP_GT => match other.single_value() {
            Some(v) => Range::half_open(w, v.wrapping_add(1) & top, v),
            None => Range::full(w),
        },
        _ => Range::full(w),
    }
}

/// Which of less/equal/greater can hold between some `a` in `self` and some
/// `b` in `other`.
///
/// A superset of the truth: computed from the min/max hulls, so a set with a
/// gap may report an ordering it cannot actually achieve. That direction
/// only ever prevents a fold.
pub(crate) fn possible_orderings(a: &Range, b: &Range, signed: bool) -> u8 {
    if a.is_empty() || b.is_empty() || a.width != b.width {
        return 0;
    }
    // Each question is answered in its own domain. Casting the unsigned
    // hulls to `i128` to share one body is wrong at width 128, where a value
    // at or above 2^127 casts negative and an achievable ordering is
    // *missed* -- the unsafe direction, since a caller folds when an
    // ordering is reported impossible.
    if signed {
        hull_orderings(
            a.signed_min(),
            a.signed_max(),
            b.signed_min(),
            b.signed_max(),
        )
    } else {
        hull_orderings(
            a.unsigned_min(),
            a.unsigned_max(),
            b.unsigned_min(),
            b.unsigned_max(),
        )
    }
}

/// The orderings two min/max hulls admit, over any totally ordered domain.
fn hull_orderings<T: Ord>(amin: T, amax: T, bmin: T, bmax: T) -> u8 {
    use super::constfold::{CMP_EQ, CMP_GT, CMP_LT};
    let mut m = 0;
    if amin < bmax {
        m |= CMP_LT;
    }
    if amax > bmin {
        m |= CMP_GT;
    }
    if amin <= bmax && bmin <= amax {
        m |= CMP_EQ;
    }
    m
}

/// `mask` as a free function, for the module-level routines above.
const fn mask_of(width: u32) -> u128 {
    mask(width)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Four bits: sixteen values, 242 representable sets. Small enough to
    /// enumerate every set and every pair of sets, which turns "is this
    /// interval arithmetic sound?" from a reading exercise into a proof at
    /// this width -- and every bug in this file is width-independent in
    /// form.
    const W: u32 = 4;
    const N: u128 = 16;

    /// Every representable set at width 4.
    fn all_ranges() -> Vec<Range> {
        let mut v = vec![Range::empty(W), Range::full(W)];
        for lo in 0..N {
            for hi in 0..N {
                if lo != hi {
                    v.push(Range::half_open(W, lo, hi));
                }
            }
        }
        v
    }

    /// The set as a bitmask over the sixteen values, by asking `contains`.
    fn bits(r: &Range) -> u32 {
        (0..N).filter(|v| r.contains(*v)).fold(0, |m, v| m | 1 << v)
    }

    #[test]
    fn every_representable_set_is_distinct_and_canonical() {
        let all = all_ranges();
        assert_eq!(all.len(), 242);
        // Equal as values exactly when equal as sets: the property a solver's
        // "did this cell move?" test rests on.
        for a in &all {
            for b in &all {
                assert_eq!(
                    a == b,
                    bits(a) == bits(b),
                    "{a:?} vs {b:?}: value equality must be set equality"
                );
            }
        }
    }

    #[test]
    fn constructors_agree_with_contains() {
        for v in 0..N {
            let s = Range::singleton(W, v);
            assert_eq!(bits(&s), 1 << v, "singleton {v}");
            assert_eq!(s.single_value(), Some(v));
        }
        assert_eq!(bits(&Range::empty(W)), 0);
        assert_eq!(bits(&Range::full(W)), 0xFFFF);
        // An inclusive range whose end is one below its start is everything.
        assert!(Range::inclusive(W, 5, 4).is_full());
        assert_eq!(bits(&Range::inclusive(W, 2, 5)), 0b0000_0000_0011_1100);
        // And one that wraps.
        assert_eq!(bits(&Range::inclusive(W, 14, 1)), 0b1100_0000_0000_0011);
    }

    #[test]
    fn union_covers_both_arguments() {
        let all = all_ranges();
        for a in &all {
            for b in &all {
                let u = a.union(b);
                let want = bits(a) | bits(b);
                assert_eq!(
                    bits(&u) & want,
                    want,
                    "union({a:?}, {b:?}) = {u:?} must contain both"
                );
            }
        }
    }

    #[test]
    fn intersect_is_exact_or_absent() {
        let all = all_ranges();
        for a in &all {
            for b in &all {
                let want = bits(a) & bits(b);
                match a.intersect_exact(b) {
                    Some(r) => assert_eq!(
                        bits(&r),
                        want,
                        "intersect_exact({a:?}, {b:?}) = {r:?} must be exact"
                    ),
                    None => {
                        // Refused: the true intersection must genuinely not
                        // be representable as one interval.
                        let representable = all.iter().any(|c| bits(c) == want);
                        assert!(
                            !representable,
                            "intersect_exact({a:?}, {b:?}) refused a representable set"
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn signed_view_is_an_involution() {
        for r in all_ranges() {
            assert_eq!(r.signed_view().signed_view(), r, "{r:?}");
            // And it is the same set, just read in a different order.
            assert_eq!(bits(&r).count_ones(), bits(&r.signed_view()).count_ones());
        }
    }

    #[test]
    fn min_and_max_agree_with_brute_force() {
        for r in all_ranges() {
            if r.is_empty() {
                continue;
            }
            let members: Vec<u128> = (0..N).filter(|v| r.contains(*v)).collect();
            let umin = *members.iter().min().unwrap();
            let umax = *members.iter().max().unwrap();
            assert!(
                r.unsigned_min() <= umin && r.unsigned_max() >= umax,
                "{r:?}: unsigned hull [{}, {}] must contain [{umin}, {umax}]",
                r.unsigned_min(),
                r.unsigned_max()
            );
            let smin = members.iter().map(|v| as_signed(*v, W)).min().unwrap();
            let smax = members.iter().map(|v| as_signed(*v, W)).max().unwrap();
            assert!(
                r.signed_min() <= smin && r.signed_max() >= smax,
                "{r:?}: signed hull [{}, {}] must contain [{smin}, {smax}]",
                r.signed_min(),
                r.signed_max()
            );
            // A set that is one contiguous run in the reading being asked
            // about must be answered *exactly*. Only checking that the hull
            // contains the truth is too weak to catch a bound that collapses
            // to 0 or MAX, which is exactly the bug this once had.
            if r.pieces().len() == 1 && !r.is_full() {
                assert_eq!(r.unsigned_min(), umin, "{r:?} unsigned min");
                assert_eq!(r.unsigned_max(), umax, "{r:?} unsigned max");
            }
            if r.signed_view().pieces().len() == 1 && !r.is_full() {
                assert_eq!(r.signed_min(), smin, "{r:?} signed min");
                assert_eq!(r.signed_max(), smax, "{r:?} signed max");
            }
        }
    }

    /// **The test that makes wrapped intervals defensible.** Every binary
    /// transfer function, over every pair of the 242 sets, over every pair
    /// of members: the true result must be in the computed range. A
    /// complete proof at four bits, and every bug these can have is
    /// width-independent in form.
    #[test]
    fn binary_transfer_functions_are_sound() {
        let all = all_ranges();
        let m = mask(W);
        type Op = (
            &'static str,
            fn(&Range, &Range) -> Range,
            fn(u128, u128) -> Option<u128>,
        );
        let ops: &[Op] = &[
            ("add", Range::add, |x, y| Some(x.wrapping_add(y) & mask(W))),
            ("sub", Range::sub, |x, y| Some(x.wrapping_sub(y) & mask(W))),
            ("mul", Range::mul, |x, y| Some(x.wrapping_mul(y) & mask(W))),
            ("udiv", Range::udiv, |x, y| (y != 0).then(|| x / y)),
            ("umod", Range::umod, |x, y| (y != 0).then(|| x % y)),
            ("and", Range::and, |x, y| Some(x & y)),
            ("or", Range::or, |x, y| Some(x | y)),
            ("xor", Range::xor, |x, y| Some(x ^ y)),
            ("shl", Range::shl, |x, y| {
                (y < W as u128).then(|| (x << y) & mask(W))
            }),
            ("lshr", Range::lshr, |x, y| (y < W as u128).then(|| x >> y)),
            ("ashr", Range::ashr, |x, y| {
                (y < W as u128).then(|| at_width(as_signed(x, W) >> y, W, false) as u128)
            }),
        ];
        for (name, transfer, exact) in ops {
            for a in &all {
                for b in &all {
                    let got = transfer(a, b);
                    for x in (0..=m).filter(|v| a.contains(*v)) {
                        for y in (0..=m).filter(|v| b.contains(*v)) {
                            let Some(want) = exact(x, y) else { continue };
                            assert!(
                                got.contains(want),
                                "{name}({a:?}, {b:?}) = {got:?} must contain \
                                 {name}({x}, {y}) = {want}"
                            );
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn unary_transfer_functions_are_sound() {
        let m = mask(W);
        for a in all_ranges() {
            let neg = a.neg();
            let not = a.not();
            let z = a.zext(8);
            let s = a.sext(8);
            let t = a.trunc(2);
            for x in (0..=m).filter(|v| a.contains(*v)) {
                assert!(
                    neg.contains(x.wrapping_neg() & m),
                    "neg({a:?}) = {neg:?} must contain -{x}"
                );
                assert!(
                    not.contains(!x & m),
                    "not({a:?}) = {not:?} must contain ~{x}"
                );
                assert!(z.contains(x), "zext({a:?}) = {z:?} must contain {x}");
                assert!(
                    s.contains(at_width(as_signed(x, W), 8, false) as u128),
                    "sext({a:?}) = {s:?} must contain sext({x})"
                );
                assert!(
                    t.contains(x & 0b11),
                    "trunc({a:?}) = {t:?} must contain {x} & 3"
                );
            }
            // `not` is exact, so it is also the same size.
            assert_eq!(bits(&not).count_ones(), bits(&a).count_ones(), "{a:?}");
        }
    }

    /// Undefined behaviour is not assumed away: a divisor that could be
    /// zero, or a shift count that could leave the width, answers `Full`
    /// rather than pretending the case cannot arise.
    #[test]
    fn undefined_operations_are_not_folded() {
        let any = Range::full(W);
        let with_zero = Range::inclusive(W, 0, 3);
        assert!(any.udiv(&with_zero).is_full());
        assert!(any.umod(&with_zero).is_full());
        // A count that is not one known value.
        assert!(any.shl(&Range::inclusive(W, 0, 2)).is_full());
        // A count at or past the width.
        assert!(any.shl(&Range::singleton(W, 4)).is_full());
        assert!(any.lshr(&Range::singleton(W, 15)).is_full());
        // A known, in-range count does fold.
        assert!(!Range::inclusive(W, 0, 3)
            .shl(&Range::singleton(W, 1))
            .is_full());
    }

    /// Zero extension must never lose everything: the result always lands
    /// in the low bits of the wider type, and that is the step that carries
    /// a bound across a widening in `20041114-1`.
    #[test]
    fn zext_never_answers_full() {
        for a in all_ranges() {
            if a.is_empty() {
                continue;
            }
            let z = a.zext(8);
            assert!(!z.is_full(), "zext({a:?}) = {z:?}");
            assert!(z.unsigned_max() <= mask(W), "zext({a:?}) left the low bits");
        }
        // The exact chain the target case walks.
        let var_minus_one = Range::inclusive(32, 0, 0x7FFF_FFFE);
        let widened = var_minus_one.zext(64);
        assert_eq!(widened, Range::inclusive(64, 0, 0x7FFF_FFFE));
        assert!(widened.unsigned_max() < 0xFFFF_FFFF);
    }

    /// Truncation is exact up to the destination's capacity, and gives up
    /// exactly at it.
    #[test]
    fn truncation_is_exact_below_the_modulus() {
        // Four values into two bits: exact.
        assert_eq!(
            Range::inclusive(W, 4, 6).trunc(2),
            Range::inclusive(2, 0, 2)
        );
        // Exactly the destination space: everything.
        assert!(Range::inclusive(W, 0, 3).trunc(2).is_full());
        assert!(Range::full(W).trunc(2).is_full());
    }

    #[test]
    fn extreme_widths_do_not_panic() {
        for w in [1, 2, 63, 64, 65, 127, 128] {
            let f = Range::full(w);
            assert!(f.is_full());
            assert_eq!(f.unsigned_max(), mask(w));
            assert_eq!(f.size(), None);
            let s = Range::singleton(w, 1);
            assert_eq!(s.single_value(), Some(1));
            assert_eq!(s.union(&f), f);
            assert_eq!(s.intersect_exact(&f), Some(s));
            assert_eq!(f.signed_min(), signed_bounds(w).0);
            let _ = Range::from_const(w, -1);
        }
        assert_eq!(Range::at(0), None);
        assert_eq!(Range::at(129), None);
        assert_eq!(Range::at(32), Some(32));
    }

    /// The most dangerous property in this file, checked by construction:
    /// the result must contain every `x` for which the predicate holds
    /// against *some* member of `other`. A result that is too small proves
    /// a live comparison constant and deletes a live branch.
    #[test]
    fn allowed_by_predicate_contains_every_consistent_value() {
        use crate::ir::constfold::CMP_ALL;
        for other in all_ranges() {
            for m in 0..=CMP_ALL {
                for signed in [false, true] {
                    let got = allowed_by_predicate(m, signed, &other);
                    for x in 0..N {
                        let consistent = (0..N).filter(|y| other.contains(*y)).any(|y| {
                            let (a, b) = if signed {
                                (as_signed(x, W), as_signed(y, W))
                            } else {
                                (x as i128, y as i128)
                            };
                            let ord = match a.cmp(&b) {
                                Ordering::Less => 1,
                                Ordering::Equal => 2,
                                Ordering::Greater => 4,
                            };
                            m & ord != 0
                        });
                        if consistent {
                            assert!(
                                got.contains(x),
                                "mask={m} signed={signed} other={other:?}: \
                                 {x} is consistent but missing from {got:?}"
                            );
                        }
                    }
                }
            }
        }
    }

    /// And the other way: nothing in the result is impossible. A failure
    /// here is lost precision, not unsoundness -- but the arms are written
    /// to be exact, so it should hold.
    #[test]
    fn allowed_by_predicate_is_tight() {
        use crate::ir::constfold::CMP_ALL;
        for other in all_ranges() {
            for m in 0..=CMP_ALL {
                for signed in [false, true] {
                    // Only exact where `other` is an unbroken run in the
                    // order being asked about; a hull is all the arms use.
                    let hull = if signed {
                        Range::inclusive(
                            W,
                            at_width(other.signed_min(), W, false) as u128,
                            at_width(other.signed_max(), W, false) as u128,
                        )
                    } else {
                        Range::inclusive(W, other.unsigned_min(), other.unsigned_max())
                    };
                    if other.is_empty() || bits(&hull) != bits(&other) {
                        continue;
                    }
                    let got = allowed_by_predicate(m, signed, &other);
                    for x in 0..N {
                        if !got.contains(x) {
                            continue;
                        }
                        let consistent = (0..N).filter(|y| other.contains(*y)).any(|y| {
                            let (a, b) = if signed {
                                (as_signed(x, W), as_signed(y, W))
                            } else {
                                (x as i128, y as i128)
                            };
                            let ord = match a.cmp(&b) {
                                Ordering::Less => 1,
                                Ordering::Equal => 2,
                                Ordering::Greater => 4,
                            };
                            m & ord != 0
                        });
                        assert!(
                            consistent,
                            "mask={m} signed={signed} other={other:?}: \
                             {x} is in {got:?} but satisfies nothing"
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn possible_orderings_covers_every_achievable_pair() {
        for a in all_ranges() {
            for b in all_ranges() {
                for signed in [false, true] {
                    let m = possible_orderings(&a, &b, signed);
                    for x in (0..N).filter(|v| a.contains(*v)) {
                        for y in (0..N).filter(|v| b.contains(*v)) {
                            let (p, q) = if signed {
                                (as_signed(x, W), as_signed(y, W))
                            } else {
                                (x as i128, y as i128)
                            };
                            let ord = match p.cmp(&q) {
                                Ordering::Less => 1,
                                Ordering::Equal => 2,
                                Ordering::Greater => 4,
                            };
                            assert!(
                                m & ord != 0,
                                "{a:?} vs {b:?} signed={signed}: {x},{y} achieves \
                                 ordering {ord} not in mask {m}"
                            );
                        }
                    }
                }
            }
        }
    }

    /// `x != 0` is the case a non-wrapping interval must throw away.
    #[test]
    fn not_equal_to_a_singleton_is_the_exact_complement() {
        use crate::ir::constfold::{CMP_GT, CMP_LT};
        let got = allowed_by_predicate(CMP_LT | CMP_GT, false, &Range::singleton(W, 0));
        assert_eq!(bits(&got), 0xFFFF & !1, "everything but zero");
        let got = allowed_by_predicate(CMP_LT | CMP_GT, false, &Range::singleton(W, 7));
        assert_eq!(bits(&got), 0xFFFF & !(1 << 7));
    }

    /// The derivation `20041114-1` turns on: `var <= 0` false means
    /// `var >= 1`, read signed.
    #[test]
    fn negating_a_signed_le_gives_the_positive_half() {
        use crate::ir::constfold::{CMP_ALL, CMP_EQ, CMP_GT, CMP_LT};
        let zero = Range::singleton(32, 0);
        let le = CMP_LT | CMP_EQ;
        let got = allowed_by_predicate(!le & CMP_ALL, true, &zero);
        assert_eq!(got, Range::inclusive(32, 1, 0x7FFF_FFFF));
        assert_eq!(got.signed_min(), 1);
        assert_eq!(got.signed_max(), i32::MAX as i128);
        assert_eq!(CMP_GT, !le & CMP_ALL);
    }

    /// The IR stores `(int)0xFFFFFFFFu` as `4294967295`, not `-1`, so the one
    /// bridge from a raw `i128` must normalize rather than truncate.
    #[test]
    fn from_const_reads_at_the_stated_width() {
        assert_eq!(Range::from_const(32, -1).single_value(), Some(0xFFFF_FFFF));
        assert_eq!(
            Range::from_const(32, 4294967295).single_value(),
            Some(0xFFFF_FFFF)
        );
        assert_eq!(Range::from_const(8, 200).single_value(), Some(200));
        assert_eq!(Range::from_const(8, -56).single_value(), Some(200));
        assert_eq!(Range::from_const(64, -1).signed_min(), -1);
    }

    /// Width 128 is where every "one past the end" arithmetic breaks: the
    /// exclusive end of the space is `2^128`, which a `u128` cannot hold,
    /// and `1i128 << 127` is `i128::MIN` rather than a positive bound. The
    /// brute-force suite runs at four bits and cannot see any of it.
    const TOP: u128 = u128::MAX;
    const SIGN: u128 = 1u128 << 127;

    /// `pieces` must cover the whole space, including its top value.
    #[test]
    fn range_pieces_reach_the_top_of_a_128_bit_space() {
        let full = Range::full(128);
        assert!(full.contains(TOP));
        assert!(full.contains(0));

        // A wrapped set whose upper piece runs to the very top.
        let wrapped = Range::inclusive(128, TOP - 4, 2);
        assert!(wrapped.contains(TOP));
        assert!(wrapped.contains(TOP - 4));
        assert!(wrapped.contains(0));
        assert!(wrapped.contains(2));
        assert!(!wrapped.contains(3));
        assert!(!wrapped.contains(TOP - 5));
    }

    /// `intersect_exact` is "exact or refuse". Before `pieces` became
    /// inclusive it silently dropped everything touching the top of the
    /// space, and a caller that narrows to a wrong singleton folds.
    #[test]
    fn range_intersect_exact_is_exact_at_the_top_of_a_128_bit_space() {
        // Both wrap; the true intersection is the run at the top plus
        // `{0, 1, 2}`, which meets at the wrap point and so is one interval.
        let a = Range::inclusive(128, SIGN, 4);
        let b = Range::inclusive(128, TOP - 15, 2);
        let got = a.intersect_exact(&b).expect("one interval");
        for v in [TOP, TOP - 15, 0u128, 1, 2] {
            assert!(
                got.contains(v),
                "{v:#x} is in both arguments and must survive"
            );
        }
        assert!(!got.contains(3), "3 is only in `a`");
        assert!(!got.contains(TOP - 16), "just below `b` starts");

        // An intersection that really is two disjoint pieces is refused,
        // not narrowed: a superset is sound but need not be a subset of
        // either argument, which is what the caller relies on.
        let c = Range::inclusive(128, TOP - 5, 5);
        let d = Range::inclusive(128, 3, TOP - 3);
        assert_eq!(
            c.intersect_exact(&d),
            None,
            "{{3,4,5}} and {{TOP-5..TOP-3}} are two intervals, not one"
        );

        // Intersecting with the whole space gives the other argument back.
        assert_eq!(Range::full(128).intersect_exact(&a), Some(a));
        assert_eq!(a.intersect_exact(&Range::full(128)), Some(a));
    }

    /// `possible_orderings` is a *superset* of the truth, so a missed
    /// ordering is the unsafe direction -- a caller folds when an ordering
    /// is reported impossible. Casting the unsigned hulls through `i128`
    /// made everything at or above `2^127` look negative.
    #[test]
    fn range_possible_orderings_are_unsigned_at_128_bits() {
        use crate::ir::constfold::{CMP_EQ, CMP_GT, CMP_LT};
        let full = Range::full(128);
        let high = Range::singleton(128, SIGN);

        let m = possible_orderings(&full, &high, false);
        assert!(m & CMP_LT != 0, "0 < 2^127");
        assert!(m & CMP_EQ != 0, "2^127 is in the full set");
        assert!(m & CMP_GT != 0, "u128::MAX > 2^127");

        // The signed reading of the same pair is a different question and
        // must still be answered in its own domain.
        let ms = possible_orderings(&full, &high, true);
        assert!(ms & CMP_LT != 0 || ms & CMP_EQ != 0 || ms & CMP_GT != 0);

        // Two disjoint unsigned runs, both above the sign bit.
        let lo = Range::inclusive(128, SIGN, SIGN + 3);
        let hi = Range::inclusive(128, SIGN + 10, SIGN + 20);
        let m = possible_orderings(&lo, &hi, false);
        assert_eq!(
            m, CMP_LT,
            "every member of `lo` is below every member of `hi`"
        );
    }

    /// The signed bounds of a 128-bit value, which `-(1i128 << 127)`
    /// computes by overflowing -- a debug build panics, and CI is debug.
    #[test]
    fn range_signed_bounds_do_not_overflow_at_128_bits() {
        assert_eq!(signed_bounds(128), (i128::MIN, i128::MAX));
        assert_eq!(signed_bounds(32), (i32::MIN as i128, i32::MAX as i128));
        assert_eq!(signed_bounds(1), (-1, 0));

        // The two routines that used to spell it inline.
        let wide = Range::full(128);
        let _ = wide.mul(&Range::singleton(128, 3));
        let _ = Range::full(64).sext(128);
        let _ = Range::inclusive(64, 1, 5).sext(128);
    }
}
