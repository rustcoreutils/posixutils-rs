//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Evaluating an IR operation over constants, at the operand's own width and
// in the signedness the opcode implies.
//
// This is the one place C's width-and-signedness rules are written at the IR
// level, and it is shared rather than copied because the cost of the two
// diverging is asymmetric: in `instcombine` a divergence is a wrong number,
// in `sccp` it is a deleted basic block. Three separate defects already live
// in these rules -- a sign-extension reaching `Asr`, the same for `DivS`, and
// the chaining guard `unambiguous_at` exists to stop -- so a second copy is a
// second place for the fourth to be fixed.
//
// Nothing here looks at anything but the opcode, the two width fields and the
// values: no `TypeTable`, no `Function`, no state. Every function is a pure
// evaluation that answers `None` when the operation is not foldable, which
// includes the cases C leaves undefined (division by zero, an out-of-range
// shift count).
//

use crate::float::{FloatVal, FpFormat};

use std::cmp::Ordering;

use super::{Instruction, Opcode};

/// Does `v` mean the same thing at `size` bits whether it is read as signed
/// or as unsigned?
///
/// An `i128` in this IR holds whatever bit pattern its constant was built
/// from, and nothing on the instruction says how to read it back: the same
/// 32 bits are -1 or 4294967295 depending on the consumer, and a `Set*`
/// cannot even ask: it records how wide its operands are
/// (`Instruction::operand_width`), not how to read them.
///
/// That ambiguity is pre-existing and harmless while a folded constant only
/// ever reaches codegen, which truncates when it materializes an immediate.
/// Feeding one back into a *second* fold is what would make it visible --
/// `1u << 31` compared against `2147483648u`, or `0x40000000 * 4` divided by
/// two. Rather than guess a signedness this pass does not have the type
/// information to know, only values that read the same either way are
/// carried across a fold. The rest simply do not chain, which is exactly the
/// behaviour before copies were followed.
pub(crate) fn unambiguous_at(v: i128, size: u32) -> bool {
    at_width(v, size, true) == v && at_width(v, size, false) == v
}

/// A constant as it actually is at `size` bits.
///
/// `const_val` hands back an `i128` holding whatever bit pattern the constant
/// was built from, which for a narrower operand may be wider than the operand
/// itself: `(int)0xFFFFFFFFu` is stored as 4294967295, not -1. Every consumer
/// that only *emits* the value truncates and so never noticed, but folding
/// arithmetic on it does notice -- `Asr` on 4294967295 shifts in zeros and
/// answers 2147483647 where the operand is -1 and the answer is -1.
pub(crate) fn at_width(v: i128, size: u32, signed: bool) -> i128 {
    if size == 0 || size >= 128 {
        return v;
    }
    let pad = 128 - size;
    if signed {
        (v << pad) >> pad
    } else {
        (((v as u128) << pad) >> pad) as i128
    }
}

/// Which kind of comparison an opcode is, and so which outcomes it has.
///
/// The two are never combined. An integer comparison has three outcomes and
/// a signedness; a float one has no signedness and a fourth outcome,
/// [`Outcomes::UN`], which is what makes `(x < y) || (x >= y)` true for
/// integers and false for a NaN.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum CmpDomain {
    /// How to read the operands at their own width before comparing.
    ///
    /// The opcode is the only thing that carries this: `SetLt`/`Le`/`Gt`/`Ge`
    /// are the signed forms and `SetB`/`Be`/`A`/`Ae` the unsigned ones.
    /// `SetEq`/`SetNe` do not care which, as long as both sides are read the
    /// same way, and are recorded as signed. A signed and an unsigned
    /// comparison over one pair are *not* comparable: `x < y` and `x > y`
    /// read signed are not complementary with the unsigned forms.
    Int { signed: bool },
    /// IEEE: less, equal, greater or unordered.
    Float,
}

impl CmpDomain {
    /// Every outcome a comparison in this domain can have.
    pub(crate) fn all(self) -> Outcomes {
        match self {
            CmpDomain::Int { .. } => Outcomes::ORDERED,
            CmpDomain::Float => Outcomes::ORDERED | Outcomes::UN,
        }
    }

    /// The outcomes a value compared with *itself* can have: equal, and for
    /// a float also unordered -- a NaN is not equal to itself.
    pub(crate) fn reflexive(self) -> Outcomes {
        match self {
            CmpDomain::Int { .. } => Outcomes::EQ,
            CmpDomain::Float => Outcomes::EQ | Outcomes::UN,
        }
    }
}

/// A set of the outcomes comparing two operands can have: less, equal,
/// greater, and for a float also unordered.
///
/// The same set answers two questions. Of an opcode it is which outcomes make
/// the comparison true ([`Outcomes::of_op`]); of a pair of operands it is
/// which outcomes they can possibly have. A comparison is then decided by one
/// rule, [`Outcomes::decide`], whatever is known about the operands -- two
/// constants, two ranges, a value and itself, a float and an infinity. And two
/// comparisons over the *same* operand pair can be combined without knowing
/// the operands at all: `a && b` is never true when their sets are disjoint,
/// and `a || b` is always true when together they cover every outcome.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) struct Outcomes(u8);

impl Outcomes {
    /// No outcome: a comparison that is never true.
    pub(crate) const NONE: Outcomes = Outcomes(0);
    pub(crate) const LT: Outcomes = Outcomes(1);
    pub(crate) const EQ: Outcomes = Outcomes(2);
    pub(crate) const GT: Outcomes = Outcomes(4);
    /// The fourth outcome a float comparison has and an integer one does
    /// not: the operands are *unordered*, because at least one is a NaN.
    pub(crate) const UN: Outcomes = Outcomes(8);
    /// Less, equal or greater: every outcome of an integer comparison.
    pub(crate) const ORDERED: Outcomes = Outcomes(1 | 2 | 4);

    /// The outcomes `op` is true for, and the domain it compares in, or
    /// `None` if it is not a comparison.
    ///
    /// Every float arm but one is ordered and so excludes [`Outcomes::UN`].
    /// The exception is `FCmpONe`, which despite its name is C's `!=` -- true
    /// when either operand is a NaN -- and is emitted that way by both
    /// backends: `setne` OR'd with `setp` on x86-64, `cset ne` (which is
    /// taken on unordered) on aarch64.
    pub(crate) fn of_op(op: Opcode) -> Option<(Outcomes, CmpDomain)> {
        const LT: Outcomes = Outcomes::LT;
        const EQ: Outcomes = Outcomes::EQ;
        const GT: Outcomes = Outcomes::GT;
        let signed = CmpDomain::Int { signed: true };
        let unsigned = CmpDomain::Int { signed: false };
        let float = CmpDomain::Float;
        Some(match op {
            Opcode::SetEq => (EQ, signed),
            Opcode::SetNe => (LT | GT, signed),
            Opcode::SetLt => (LT, signed),
            Opcode::SetLe => (LT | EQ, signed),
            Opcode::SetGt => (GT, signed),
            Opcode::SetGe => (GT | EQ, signed),
            Opcode::SetB => (LT, unsigned),
            Opcode::SetBe => (LT | EQ, unsigned),
            Opcode::SetA => (GT, unsigned),
            Opcode::SetAe => (GT | EQ, unsigned),
            Opcode::FCmpOEq => (EQ, float),
            Opcode::FCmpONe => (LT | GT | Outcomes::UN, float),
            Opcode::FCmpOLt => (LT, float),
            Opcode::FCmpOLe => (LT | EQ, float),
            Opcode::FCmpOGt => (GT, float),
            Opcode::FCmpOGe => (GT | EQ, float),
            _ => return None,
        })
    }

    /// The one outcome two known values have; `None` is unordered.
    pub(crate) fn of_ordering(ord: Option<Ordering>) -> Outcomes {
        match ord {
            Some(Ordering::Less) => Outcomes::LT,
            Some(Ordering::Equal) => Outcomes::EQ,
            Some(Ordering::Greater) => Outcomes::GT,
            None => Outcomes::UN,
        }
    }

    /// Whether every outcome in `other` is in `self`.
    pub(crate) fn contains(self, other: Outcomes) -> bool {
        self.0 & other.0 == other.0
    }

    pub(crate) fn is_empty(self) -> bool {
        self.0 == 0
    }

    /// The same set read with the operands the other way round: `a < b` and
    /// `b < a` are the same comparison with less and greater exchanged.
    /// Equal and unordered are symmetric and stay where they are.
    pub(crate) fn mirror(self) -> Outcomes {
        let mut m = self & (Outcomes::EQ | Outcomes::UN);
        if self.contains(Outcomes::LT) {
            m = m | Outcomes::GT;
        }
        if self.contains(Outcomes::GT) {
            m = m | Outcomes::LT;
        }
        m
    }

    /// The outcomes of `domain` not in `self`: the comparison's negation.
    pub(crate) fn complement(self, domain: CmpDomain) -> Outcomes {
        Outcomes(!self.0 & domain.all().0)
    }

    /// The answer of a comparison true for the outcomes in `self`, when every
    /// outcome in `possible` gives the same one: false when none of them makes
    /// it true, true when all of them do, and `None` when they disagree.
    pub(crate) fn decide(self, possible: Outcomes) -> Option<bool> {
        if (self & possible).is_empty() {
            Some(false)
        } else if self.contains(possible) {
            Some(true)
        } else {
            None
        }
    }

    /// Every subset of `self`, the empty set included.
    #[cfg(test)]
    pub(crate) fn subsets(self) -> impl Iterator<Item = Outcomes> {
        (0..=self.0).filter(move |m| m & !self.0 == 0).map(Outcomes)
    }
}

impl std::ops::BitOr for Outcomes {
    type Output = Outcomes;
    fn bitor(self, rhs: Outcomes) -> Outcomes {
        Outcomes(self.0 | rhs.0)
    }
}

impl std::ops::BitAnd for Outcomes {
    type Output = Outcomes;
    fn bitand(self, rhs: Outcomes) -> Outcomes {
        Outcomes(self.0 & rhs.0)
    }
}

/// The outcomes of `x` compared with the constant `c`, for an unknown `x`
/// that may be told never to be below zero.
///
/// Nothing is greater than `+Inf`, nothing is less than `-Inf`, and nothing
/// is ordered with a NaN -- including a NaN `x`, which is why an infinity
/// still leaves *unordered* possible. A NaN `x` also leaves `never_below`
/// true, since it is not less than zero either.
pub(crate) fn possible_against(c: FloatVal, never_below: bool) -> Outcomes {
    let inf = FloatVal::infinity(false);
    let possible = match (c.cmp_value(inf), c.cmp_value(inf.negated())) {
        (None, _) => return Outcomes::UN,
        (Some(Ordering::Equal), _) => Outcomes::GT.complement(CmpDomain::Float),
        (_, Some(Ordering::Equal)) => Outcomes::LT.complement(CmpDomain::Float),
        _ => CmpDomain::Float.all(),
    };
    if never_below && c.cmp_value(FloatVal::from_f64(0.0)) != Some(Ordering::Greater) {
        possible & Outcomes::LT.complement(CmpDomain::Float)
    } else {
        possible
    }
}

/// The answer of `op` comparing an unknown value with the constant `c`
/// (`c op x` when `const_first`), when no value -- a NaN included -- can
/// change it: `x > +Inf` is 0, `x != NaN` is 1. The one rule the optimizer
/// folds by, and the linearizer too under `-fno-trapping-math`.
pub(crate) fn fcmp_against_constant(op: Opcode, c: FloatVal, const_first: bool) -> Option<bool> {
    let (mask, CmpDomain::Float) = Outcomes::of_op(op)? else {
        return None;
    };
    let possible = possible_against(c, false);
    let possible = if const_first {
        possible.mirror()
    } else {
        possible
    };
    mask.decide(possible)
}

/// `insn`'s operation applied to two constants, or `None` if the opcode is
/// not one this folds or the operation is undefined for these operands.
///
/// The operands are raw: every narrowing this needs is applied here, and
/// narrowing is idempotent, so a caller that has already read them at their
/// own width may pass those instead.
fn eval_binop(insn: &Instruction, a: i128, b: i128) -> Option<i128> {
    match insn.op {
        // Congruent modulo 2^n, so the width does not enter into it.
        Opcode::Add => Some(a.wrapping_add(b)),
        Opcode::Sub => Some(a.wrapping_sub(b)),
        Opcode::Mul => Some(a.wrapping_mul(b)),

        Opcode::And => Some(a & b),
        Opcode::Or => Some(a | b),
        Opcode::Xor => Some(a ^ b),

        Opcode::DivS | Opcode::DivU | Opcode::ModS | Opcode::ModU => eval_divmod(insn, a, b),
        Opcode::Shl | Opcode::Lsr | Opcode::Asr => eval_shift(insn, a, b),

        _ => {
            let (mask, CmpDomain::Int { signed }) = Outcomes::of_op(insn.op)? else {
                return None;
            };
            let size = insn.operand_width();
            let a = at_width(a, size, signed);
            let b = at_width(b, size, signed);
            let ord = if signed {
                a.cmp(&b)
            } else {
                (a as u128).cmp(&(b as u128))
            };
            mask.decide(Outcomes::of_ordering(Some(ord)))
                .map(i128::from)
        }
    }
}

/// The most negative value at `size` bits, read as signed: the one dividend
/// whose quotient by -1 is not representable.
///
/// `size == 0` and `size >= 128` both answer `i128::MIN`, matching
/// [`at_width`], which leaves a value alone at those widths.
fn signed_min_at(size: u32) -> i128 {
    if size == 0 || size >= 128 {
        i128::MIN
    } else {
        -1i128 << (size - 1)
    }
}

/// Whether `op` -- one of `DivS`/`DivU`/`ModS`/`ModU` -- computed at `size`
/// bits over these operands may raise a hardware trap. `None` is an operand
/// that is not known, which may be anything, and so may trap.
///
/// This is the one place the rule lives, because the three passes that fold a
/// division each know a different amount about the operands and the rule drifted
/// apart between them. `eval_divmod` knows both values; `instcombine`'s
/// algebraic arms know one and nothing about the other; `range::udiv`/`umod`
/// know sets rather than values, and answer this question of a set by asking
/// whether zero is in it (the signed trap has no unsigned counterpart).
///
/// Two operand pairs trap, and they are the two `idiv` raises #DE for:
///
/// * a zero divisor, in either signedness; and
/// * the single signed overflow, `INT_MIN / -1`, whose quotient is not
///   representable. The remainder form traps with it, because on x86-64 it is
///   the same instruction, computing the same quotient.
///
/// c17 does not assume this undefined behaviour away. Folding either one turns
/// a program that faults at `-O0` into one that prints an answer at `-O2`, and
/// the two disagreeing about the same source is worse than either answer.
/// gcc and clang both fold these, treating the undefined behaviour as licence;
/// this is a deliberate divergence from both rather than a bug-for-bug match.
///
/// The operands are raw: the narrowing this needs is applied here, and
/// narrowing is idempotent, so a caller that has already read them at their own
/// width may pass those instead.
pub(crate) fn divmod_may_trap(op: Opcode, size: u32, a: Option<i128>, b: Option<i128>) -> bool {
    let signed = match op {
        Opcode::DivS | Opcode::ModS => true,
        Opcode::DivU | Opcode::ModU => false,
        // Nothing else in this IR traps on its operands.
        _ => return false,
    };
    let size = size.max(1);

    // An unknown divisor may be zero.
    let Some(b) = b.map(|v| at_width(v, size, signed)) else {
        return true;
    };
    if b == 0 {
        return true;
    }
    if !signed || b != -1 {
        return false;
    }
    // Divisor -1: the trap turns on whether the dividend is the most negative
    // value, so an unknown dividend may trap.
    match a.map(|v| at_width(v, size, true)) {
        Some(a) => a == signed_min_at(size),
        None => true,
    }
}

/// Division and remainder.
///
/// Read at the operand's own width, in the signedness the opcode implies.
/// Division is not congruent modulo 2^n the way add/sub/mul are: it reads the
/// whole value and its sign, so `(int)0xFFFFFFFFu` arriving as 4294967295
/// rather than -1 answered 2147483647 where C says 0.
///
/// An operation that traps is not folded: see [`divmod_may_trap`]. That single
/// refusal covers `instcombine`, `sccp` and `vrp` at once, since all three
/// route their constant folding through [`eval_binop`].
fn eval_divmod(insn: &Instruction, a: i128, b: i128) -> Option<i128> {
    let signed = matches!(insn.op, Opcode::DivS | Opcode::ModS);
    let size = insn.size.max(1);
    if divmod_may_trap(insn.op, size, Some(a), Some(b)) {
        return None;
    }
    let a = at_width(a, size, signed);
    let b = at_width(b, size, signed);
    let folded = match (insn.op, signed) {
        (Opcode::DivS, _) => a.wrapping_div(b),
        (Opcode::DivU, _) => (a as u128).wrapping_div(b as u128) as i128,
        (Opcode::ModS, _) => a.wrapping_rem(b),
        (Opcode::ModU, _) => (a as u128).wrapping_rem(b as u128) as i128,
        _ => return None,
    };
    Some(at_width(folded, size, signed))
}

/// Shifts, each folded at the operand's own width and read with the
/// signedness that shift implies: `Asr` is the arithmetic one by definition,
/// `Lsr` the logical one. A shift count outside `[0, width)` is undefined and
/// is not folded.
fn eval_shift(insn: &Instruction, a: i128, b: i128) -> Option<i128> {
    let size = insn.size.max(1);
    if !(0..size as i128).contains(&b) {
        return None;
    }
    Some(match insn.op {
        // The result is truncated back, so an overflowing shift wraps at the
        // operand width rather than growing into the i128.
        Opcode::Shl => at_width(at_width(a, size, true).wrapping_shl(b as u32), size, true),
        // Shifted in the unsigned view, which is what makes it the logical
        // shift. Doing it on the `i128` instead only agrees below 128 bits,
        // where `at_width` has already cleared the high half: at 128 bits
        // `at_width` hands the value back unchanged and `i128::wrapping_shr`
        // is the arithmetic shift, so a negative operand would shift in ones.
        // Unreachable as the passes stand: `arch::mapping` runs before the
        // optimizer, and it leaves no 128-bit shift whose count this can read
        // -- a literally constant count expands the shift into 64-bit halves,
        // and any other count is rewritten into a `Pair64` the constant map
        // cannot answer. Written correctly anyway, so that the arm does not
        // depend on that pass ordering for its answer.
        Opcode::Lsr => at_width(
            (at_width(a, size, false) as u128).wrapping_shr(b as u32) as i128,
            size,
            false,
        ),
        Opcode::Asr => at_width(a, size, true).wrapping_shr(b as u32),
        _ => return None,
    })
}

/// A float arithmetic operation over two constants, at the format of its
/// operands, or `None` if `op` is not one this folds.
///
/// Rounded exactly once, on the result, after rounding each operand to the
/// format the program computes in -- which is what makes `0.1 + 0.2` unequal
/// to `0.3` here as it is at run time.
///
/// Nothing involving a NaN or an infinity is folded, on either side. Those
/// are the operands whose evaluation raises a floating-point exception, and
/// C lets a program observe one (`<fenv.h>`); folding the operation away
/// would quietly take that flag with it. Ordinary finite arithmetic raises
/// only *inexact*, which is not separable from the fold in the first place.
///
/// `CopySign` is not arithmetic, and folds for every pair: it moves one sign
/// bit, raises nothing, and keeps a NaN's payload. `FMin` and `FMax` fold
/// as [`FloatVal::fmin`] says, a quiet NaN operand included, since neither
/// raises anything for one.
pub(crate) fn eval_fbinop(op: Opcode, fmt: FpFormat, a: FloatVal, b: FloatVal) -> Option<FloatVal> {
    let (a, b) = (a.round_to_format(fmt), b.round_to_format(fmt));
    match op {
        Opcode::CopySign => return Some(a.with_sign_of(b)),
        Opcode::FMin => return a.fmin(b, fmt),
        Opcode::FMax => return a.fmax(b, fmt),
        _ => {}
    }
    if !a.is_finite() || !b.is_finite() {
        return None;
    }
    let r = match op {
        Opcode::FAdd => a.add(b, fmt),
        Opcode::FSub => a.sub(b, fmt),
        Opcode::FMul => a.mul(b, fmt),
        // A zero divisor gives infinity and raises `FE_DIVBYZERO`, so it
        // falls to the same rule as the operands above.
        Opcode::FDiv if !b.is_zero() => a.div(b, fmt),
        _ => return None,
    };
    r.is_finite().then_some(r)
}

/// A float operation of three constants: `Fma`, as [`FloatVal::fma`] says --
/// rounded once, and held to the arithmetic's rule for what raises.
pub(crate) fn eval_fternop(
    op: Opcode,
    fmt: FpFormat,
    a: FloatVal,
    b: FloatVal,
    c: FloatVal,
) -> Option<FloatVal> {
    match op {
        Opcode::Fma => a.fma(b, c, fmt),
        _ => None,
    }
}

/// A float unary operation over a constant.
///
/// `FNeg` and `Fabs`, and unlike the arithmetic above they fold for every
/// operand including the infinities and NaN: flipping or clearing a sign bit
/// computes nothing and raises nothing, and a NaN keeps its payload. Both are
/// also exact, so the result needs no rounding that the operand has not
/// already had.
///
/// `Sqrt` folds as [`FloatVal::sqrt`] says: rounded once like arithmetic,
/// and exact for a zero, `+inf` and a quiet NaN; a negative operand, whose
/// NaN is the target's own, and a signalling NaN are left to run time.
///
/// `RoundToIntegral` folds as [`FloatVal::round_to_integral`] says: exactly,
/// except a signalling NaN, and a `rint` or `nearbyint` whose answer is the
/// rounding direction's.
pub(crate) fn eval_funop(op: Opcode, fmt: FpFormat, a: FloatVal) -> Option<FloatVal> {
    match op {
        Opcode::FNeg => Some(a.round_to_format(fmt).negated()),
        Opcode::Fabs => Some(a.round_to_format(fmt).magnitude()),
        Opcode::Sqrt => a.sqrt(fmt),
        Opcode::RoundToIntegral(how) => a.round_to_integral(how, fmt),
        _ => None,
    }
}

/// A float-to-float conversion of a constant, from one format to another:
/// [`FloatVal::convert`], which rounds at the source format first.
///
/// Held to the same rule as the arithmetic above otherwise: a non-finite
/// operand is left alone, and so is a narrowing that overflows to infinity,
/// because both are where the conversion raises.
pub(crate) fn eval_fcvtf(
    op: Opcode,
    src_fmt: FpFormat,
    dst_fmt: FpFormat,
    a: FloatVal,
) -> Option<FloatVal> {
    if op != Opcode::FCvtF || !a.is_finite() {
        return None;
    }
    let r = a.convert(src_fmt, dst_fmt);
    r.is_finite().then_some(r)
}

/// A float-to-integer conversion of a constant, to `dst_size` bits, or the
/// other integer a float operand gives in the same shape: `Signbit`.
///
/// `None` when the value does not fit, which is exactly where C leaves the
/// conversion undefined (6.3.1.4): a folded answer there would be this
/// compiler's invention rather than the target's, and the two differ.
///
/// `Signbit` folds for every operand, the infinities and NaN included: it
/// reads a bit and raises nothing, and rounding to a format never changes a
/// sign.
pub(crate) fn eval_fcvt(op: Opcode, dst_size: u32, src_fmt: FpFormat, a: FloatVal) -> Option<i128> {
    let signed = match op {
        Opcode::FCvtS => true,
        Opcode::FCvtU => false,
        Opcode::Signbit => return Some(i128::from(a.sign_bit())),
        _ => return None,
    };
    a.round_to_format(src_fmt)
        .to_integer(dst_size.clamp(1, 128), signed)
}

/// How many integer operands `op` takes when [`eval_int`] evaluates it, or
/// `None` when it evaluates no such opcode.
pub(crate) fn int_fold_arity(op: Opcode) -> Option<usize> {
    if matches!(
        op,
        Opcode::Neg | Opcode::Not | Opcode::Sext | Opcode::Zext | Opcode::Trunc
    ) || bit_opcode(op).is_some()
    {
        Some(1)
    } else if op.is_int_arith() || op.is_int_comparison() {
        Some(2)
    } else {
        None
    }
}

/// Whether [`eval_int`] evaluates `op`.
pub(crate) fn is_int_foldable(op: Opcode) -> bool {
    int_fold_arity(op).is_some()
}

/// `insn`'s integer operation applied to the constants `ops`, one per
/// operand: the single dispatch every pass that folds over known operands
/// goes through, so that none of them can model an opcode the others do not.
///
/// `None` when the opcode is not one this folds, `ops` is the wrong arity
/// for it, or the operation is undefined for these operands.
pub(crate) fn eval_int(insn: &Instruction, ops: &[i128]) -> Option<i128> {
    if int_fold_arity(insn.op)? != ops.len() {
        return None;
    }
    match *ops {
        [a] => eval_unop(insn, a),
        [a, b] => eval_binop(insn, a, b),
        _ => None,
    }
}

/// `insn`'s unary operation applied to a constant.
///
/// Includes the integer width conversions, which are unary in the IR and
/// carry the source width in `src_size` -- `sext.8to32` being the shape.
/// The extensions are the whole reason a sub-`int` constant chains at all:
/// every `char` or `short` read is widened before anything is done with it,
/// so a fold that stops at the conversion stops one instruction after it
/// started.
fn eval_unop(insn: &Instruction, a: i128) -> Option<i128> {
    match insn.op {
        Opcode::Neg => Some(a.wrapping_neg()),
        Opcode::Not => Some(!a),
        // The count of the operand at its own width. It is at most 64, so
        // it reads the same at every width the `int` result is taken at.
        // Read the operand at the width it was stored in, in the signedness
        // the opcode names, and leave it there: the destination is wider.
        Opcode::Sext | Opcode::Zext => {
            Some(at_width(a, insn.operand_width(), insn.op == Opcode::Sext))
        }
        // Truncation keeps the low `size` bits -- but *which value* those
        // bits are is decided by the consumer, not here, and the IR does not
        // record it: `-(unsigned char)200` reaches the negation as
        // `trunc.32to8` feeding `neg.32`, with no extension in between and
        // nothing saying the widening is unsigned. Leaving the `Trunc` in
        // place is what lets the backend's move decide, so only a value that
        // reads the same either way may be folded away. That is a narrower
        // rule than `unambiguous_at` enforces on *chaining* elsewhere in this
        // file, and deliberately so: this one governs the rewrite itself.
        Opcode::Trunc => {
            let size = insn.size.max(1);
            let v = at_width(a, size, true);
            unambiguous_at(v, size).then_some(v)
        }

        op => {
            let (bit_op, width) = bit_opcode(op)?;
            Some(eval_bit_op(bit_op, width, a))
        }
    }
}

/// An operation of the bit builtins.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum BitOp {
    /// Reverse the bytes.
    Bswap,
    /// Count the trailing zero bits.
    Ctz,
    /// Count the leading zero bits.
    Clz,
    /// Count the leading bits that repeat the sign bit.
    Clrsb,
    /// Count the one bits.
    Popcount,
    /// One plus the index of the lowest one bit, or 0 for 0.
    Ffs,
}

/// The bit operation `op` performs and the operand width it reads, or
/// `None` if `op` is no bit operation.
pub(crate) fn bit_opcode(op: Opcode) -> Option<(BitOp, u32)> {
    Some(match op {
        Opcode::Bswap16 => (BitOp::Bswap, 16),
        Opcode::Bswap32 => (BitOp::Bswap, 32),
        Opcode::Bswap64 => (BitOp::Bswap, 64),
        Opcode::Ctz32 => (BitOp::Ctz, 32),
        Opcode::Ctz64 => (BitOp::Ctz, 64),
        Opcode::Clz32 => (BitOp::Clz, 32),
        Opcode::Clz64 => (BitOp::Clz, 64),
        Opcode::Popcount32 => (BitOp::Popcount, 32),
        Opcode::Popcount64 => (BitOp::Popcount, 64),
        _ => return None,
    })
}

/// `op` of the `width`-bit operand `a`: the one rule for each bit builtin,
/// which both this file's opcode folds and the C17 6.6 walk in
/// [`crate::constexpr`] evaluate by.
///
/// Only the low `width` bits of `a` are read, in the signedness the
/// operation implies: every one reads an unsigned operand but `Clrsb`. A
/// count is at most 64, so it reads the same at every width; a byte swap is
/// the unsigned value of its width.
///
/// `ctz` and `clz` of 0 are undefined at run time, but a constant one is
/// still a constant to gcc, which folds it to the operand width on both
/// targets -- so `static int x = __builtin_ctz(0);` is 32 there and here.
pub(crate) fn eval_bit_op(op: BitOp, width: u32, a: i128) -> i128 {
    let bits = at_width(a, width, false) as u128;
    let unused = 128 - width;
    match op {
        BitOp::Bswap => (bits.swap_bytes() >> unused) as i128,
        BitOp::Ctz => i128::from(bits.trailing_zeros().min(width)),
        BitOp::Clz => i128::from(bits.leading_zeros() - unused),
        BitOp::Popcount => i128::from(bits.count_ones()),
        BitOp::Ffs if bits == 0 => 0,
        BitOp::Ffs => i128::from(bits.trailing_zeros() + 1),
        BitOp::Clrsb => {
            let signed = at_width(a, width, true);
            let magnitude = if signed < 0 { !signed } else { signed };
            eval_bit_op(BitOp::Clz, width, magnitude) - 1
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DOMAINS: [CmpDomain; 3] = [
        CmpDomain::Int { signed: true },
        CmpDomain::Int { signed: false },
        CmpDomain::Float,
    ];

    const SINGLES: [Outcomes; 4] = [Outcomes::LT, Outcomes::EQ, Outcomes::GT, Outcomes::UN];

    /// `decide` against its definition, over every mask and every set of
    /// possible outcomes: decided only when every possible outcome gives the
    /// same answer, and false when there is none to give.
    #[test]
    fn outcomes_decide_is_unanimity() {
        let all = CmpDomain::Float.all();
        for mask in all.subsets() {
            for possible in all.subsets() {
                let answers: Vec<bool> = SINGLES
                    .into_iter()
                    .filter(|o| possible.contains(*o))
                    .map(|o| mask.contains(o))
                    .collect();
                let want = if answers.iter().all(|a| !a) {
                    Some(false)
                } else if answers.iter().all(|a| *a) {
                    Some(true)
                } else {
                    None
                };
                assert_eq!(mask.decide(possible), want, "{mask:?} of {possible:?}");
            }
        }
    }

    /// Swapping the operands exchanges less and greater and nothing else,
    /// and doing it twice is no change.
    #[test]
    fn outcomes_mirror_exchanges_less_and_greater() {
        let swap = |o| match o {
            Outcomes::LT => Outcomes::GT,
            Outcomes::GT => Outcomes::LT,
            o => o,
        };
        for mask in CmpDomain::Float.all().subsets() {
            for o in SINGLES {
                assert_eq!(
                    mask.mirror().contains(swap(o)),
                    mask.contains(o),
                    "{mask:?}"
                );
            }
            assert_eq!(mask.mirror().mirror(), mask);
        }
    }

    /// The complement is the rest of the domain: disjoint from the mask,
    /// together the whole domain, and its own inverse.
    #[test]
    fn outcomes_complement_is_the_rest_of_the_domain() {
        for domain in DOMAINS {
            let all = domain.all();
            for mask in all.subsets() {
                let not = mask.complement(domain);
                assert!((mask & not).is_empty(), "{mask:?} in {domain:?}");
                assert_eq!(mask | not, all, "{mask:?} in {domain:?}");
                assert_eq!(not.complement(domain), mask, "{mask:?} in {domain:?}");
            }
        }
    }

    /// The negation of every comparison opcode is the opcode's complement,
    /// in the same domain -- except for a float, whose ordered predicates
    /// are all false on a NaN and so are not one another's negations.
    #[test]
    fn outcomes_complement_of_an_int_op_is_its_negation() {
        for (op, neg) in [
            (Opcode::SetEq, Opcode::SetNe),
            (Opcode::SetLt, Opcode::SetGe),
            (Opcode::SetLe, Opcode::SetGt),
            (Opcode::SetB, Opcode::SetAe),
            (Opcode::SetBe, Opcode::SetA),
        ] {
            let (m, d) = Outcomes::of_op(op).unwrap();
            let (n, e) = Outcomes::of_op(neg).unwrap();
            assert_eq!(d, e, "{op:?}");
            assert_eq!(m.complement(d), n, "{op:?}");
            assert_eq!(n.complement(d), m, "{neg:?}");
        }
        let (lt, d) = Outcomes::of_op(Opcode::FCmpOLt).unwrap();
        let (ge, _) = Outcomes::of_op(Opcode::FCmpOGe).unwrap();
        assert_eq!(lt.complement(d), ge | Outcomes::UN);
    }

    /// `of_op` names exactly the comparisons, each in its own domain, with
    /// a mask inside that domain; and swapping operands is the mirrored
    /// opcode.
    #[test]
    fn outcomes_of_op_covers_exactly_the_comparisons() {
        for &op in Opcode::ALL {
            match Outcomes::of_op(op) {
                Some((mask, domain)) => {
                    assert!(domain.all().contains(mask), "{op:?}");
                    assert_eq!(op.is_float_comparison(), domain == CmpDomain::Float);
                    assert!(op.is_comparison(), "{op:?}");
                }
                None => assert!(!op.is_comparison(), "{op:?}"),
            }
        }
        for (op, swapped) in [
            (Opcode::SetLt, Opcode::SetGt),
            (Opcode::SetLe, Opcode::SetGe),
            (Opcode::SetB, Opcode::SetA),
            (Opcode::SetBe, Opcode::SetAe),
            (Opcode::SetEq, Opcode::SetEq),
            (Opcode::SetNe, Opcode::SetNe),
            (Opcode::FCmpOLt, Opcode::FCmpOGt),
            (Opcode::FCmpOLe, Opcode::FCmpOGe),
            (Opcode::FCmpOEq, Opcode::FCmpOEq),
            (Opcode::FCmpONe, Opcode::FCmpONe),
        ] {
            let (m, d) = Outcomes::of_op(op).unwrap();
            assert_eq!(Some((m.mirror(), d)), Outcomes::of_op(swapped), "{op:?}");
        }
    }

    /// A value compared with itself: decided for every integer comparison
    /// by whether it admits equality, and for a float only when it admits
    /// neither equal nor unordered -- `x == x` is false for a NaN.
    #[test]
    fn outcomes_decide_a_self_comparison() {
        for (op, want) in [
            (Opcode::SetEq, Some(true)),
            (Opcode::SetNe, Some(false)),
            (Opcode::SetLt, Some(false)),
            (Opcode::SetLe, Some(true)),
            (Opcode::SetGt, Some(false)),
            (Opcode::SetGe, Some(true)),
            (Opcode::SetB, Some(false)),
            (Opcode::SetBe, Some(true)),
            (Opcode::SetA, Some(false)),
            (Opcode::SetAe, Some(true)),
            (Opcode::FCmpOEq, None),
            (Opcode::FCmpONe, None),
            (Opcode::FCmpOLt, Some(false)),
            (Opcode::FCmpOLe, None),
            (Opcode::FCmpOGt, Some(false)),
            (Opcode::FCmpOGe, None),
        ] {
            let (mask, domain) = Outcomes::of_op(op).unwrap();
            assert_eq!(mask.decide(domain.reflexive()), want, "{op:?}");
        }
    }

    /// Two constants compare as the opcode reads them: at the operand width,
    /// signed or unsigned, including at 128 bits where the unsigned reading
    /// of a negative `i128` is the larger.
    #[test]
    fn eval_binop_compares_in_the_opcodes_signedness() {
        let values = [0i128, 1, -1, 0x7f, 0x80, 0xff, i128::MIN, i128::MAX];
        for &op in Opcode::ALL.iter().filter(|op| op.is_int_comparison()) {
            let (_, domain) = Outcomes::of_op(op).unwrap();
            let CmpDomain::Int { signed } = domain else {
                unreachable!()
            };
            for size in [8, 128] {
                // A comparison's operand width is its source width; the
                // result is an `int`.
                let mut insn = binop_at(op, 32);
                insn.src_size = size;
                for a in values {
                    for b in values {
                        let (x, y) = (at_width(a, size, signed), at_width(b, size, signed));
                        let holds = match op {
                            Opcode::SetEq => x == y,
                            Opcode::SetNe => x != y,
                            Opcode::SetLt => x < y,
                            Opcode::SetLe => x <= y,
                            Opcode::SetGt => x > y,
                            Opcode::SetGe => x >= y,
                            Opcode::SetB => (x as u128) < (y as u128),
                            Opcode::SetBe => (x as u128) <= (y as u128),
                            Opcode::SetA => (x as u128) > (y as u128),
                            Opcode::SetAe => (x as u128) >= (y as u128),
                            _ => unreachable!(),
                        };
                        assert_eq!(
                            eval_binop(&insn, a, b),
                            Some(i128::from(holds)),
                            "{op:?}.{size} {a} {b}"
                        );
                    }
                }
            }
        }
    }

    /// The rule both the optimizer and the linearizer fold by: decided only
    /// when every value of the unknown side, a NaN included, gives the same
    /// answer -- and read the right way round when the constant comes first.
    #[test]
    fn fcmp_against_constant_decides_only_what_no_value_changes() {
        let inf = FloatVal::infinity(false);
        let nan = FloatVal::nan();
        for (op, c, const_first, want) in [
            (Opcode::FCmpOGt, inf, false, Some(false)), // x > +Inf
            (Opcode::FCmpOLt, inf, true, Some(false)),  // +Inf < x
            (Opcode::FCmpOLt, inf.negated(), false, Some(false)), // x < -Inf
            (Opcode::FCmpOGt, inf.negated(), true, Some(false)), // -Inf > x
            (Opcode::FCmpOGt, nan, false, Some(false)), // x > NaN
            (Opcode::FCmpOEq, nan, true, Some(false)),  // NaN == x
            (Opcode::FCmpONe, nan, false, Some(true)),  // x != NaN
            (Opcode::FCmpOLe, inf, false, None),        // false for a NaN only
            (Opcode::FCmpOGe, inf, false, None),        // x == +Inf
            (Opcode::FCmpOLt, inf, false, None),
            (Opcode::FCmpOEq, inf, false, None),
            (Opcode::FCmpONe, inf, false, None),
            (Opcode::FCmpOGt, inf, true, None), // +Inf > x
            (Opcode::FCmpOGt, FloatVal::from_f64(1.0), false, None),
        ] {
            assert_eq!(
                fcmp_against_constant(op, c, const_first),
                want,
                "{op:?} {c:?} const_first={const_first}"
            );
        }
        assert_eq!(fcmp_against_constant(Opcode::SetGt, inf, false), None);
    }

    const DIVMOD: [Opcode; 4] = [Opcode::DivS, Opcode::DivU, Opcode::ModS, Opcode::ModU];

    fn is_signed(op: Opcode) -> bool {
        matches!(op, Opcode::DivS | Opcode::ModS)
    }

    /// A zero divisor traps in either signedness, at every width, whatever the
    /// dividend is -- including when the dividend is itself unknown.
    #[test]
    fn divmod_may_trap_refuses_every_zero_divisor() {
        for op in DIVMOD {
            for size in [8, 16, 32, 64, 128] {
                for a in [None, Some(0), Some(1), Some(-1), Some(i128::MAX)] {
                    assert!(
                        divmod_may_trap(op, size, a, Some(0)),
                        "{op:?}.{size} {a:?} / 0"
                    );
                }
                // The zero may also arrive unnarrowed: 2^size reads as zero at
                // `size` bits, and it is the narrowed value that divides.
                if size < 128 {
                    assert!(
                        divmod_may_trap(op, size, Some(1), Some(1i128 << size)),
                        "{op:?}.{size} 1 / 2^{size}"
                    );
                }
            }
        }
    }

    /// An operand that is not known may be anything, so it may be the zero
    /// divisor, or the `INT_MIN` dividend that overflows against -1.
    #[test]
    fn divmod_may_trap_refuses_an_unknown_operand() {
        for op in DIVMOD {
            // An unknown divisor, whatever the dividend.
            assert!(divmod_may_trap(op, 32, Some(7), None), "{op:?} 7 / x");
            assert!(divmod_may_trap(op, 32, Some(0), None), "{op:?} 0 / x");
            assert!(divmod_may_trap(op, 32, None, None), "{op:?} y / x");

            // An unknown dividend only matters against -1, and only when the
            // opcode is a signed one -- for `DivU`/`ModU`, -1 at 32 bits is
            // 4294967295, an ordinary divisor.
            assert_eq!(
                divmod_may_trap(op, 32, None, Some(-1)),
                is_signed(op),
                "{op:?} x / -1"
            );
            assert!(!divmod_may_trap(op, 32, None, Some(3)), "{op:?} x / 3");
        }
    }

    /// The one signed overflow, at every width it exists at, and only for the
    /// signed opcodes.
    #[test]
    fn divmod_may_trap_refuses_the_signed_overflow() {
        for size in [8, 16, 32, 64, 128] {
            let min = signed_min_at(size);
            for op in DIVMOD {
                // Only the signed opcodes: the unsigned reading of the same
                // bits is a large positive dividend divided by a larger one,
                // which is 0 and cannot trap.
                assert_eq!(
                    divmod_may_trap(op, size, Some(min), Some(-1)),
                    is_signed(op),
                    "{op:?}.{size} MIN / -1"
                );
            }
        }
        assert_eq!(signed_min_at(8), -128);
        assert_eq!(signed_min_at(32), -2147483648);
        assert_eq!(signed_min_at(64), i64::MIN as i128);
        assert_eq!(signed_min_at(128), i128::MIN);
    }

    /// The dividend is read at its own width first, so `INT_MIN` written as
    /// the unsigned pattern 2147483648 is still the overflowing dividend, and
    /// a 64-bit `INT_MIN` divided at 32 bits is not.
    #[test]
    fn divmod_may_trap_reads_the_dividend_at_its_width() {
        assert!(divmod_may_trap(
            Opcode::DivS,
            32,
            Some(2147483648),
            Some(-1)
        ));
        assert!(divmod_may_trap(
            Opcode::DivS,
            32,
            Some(-2147483648),
            Some(-1)
        ));
        // -2^31 at 64 bits is an ordinary negative number, not `LONG_MIN`.
        assert!(!divmod_may_trap(
            Opcode::DivS,
            64,
            Some(-2147483648),
            Some(-1)
        ));
        // The divisor, likewise: 4294967295 at 32 bits signed is -1.
        assert!(divmod_may_trap(
            Opcode::DivS,
            32,
            Some(-2147483648),
            Some(4294967295)
        ));
    }

    /// The safe neighbours of both traps still fold, which is what stops the
    /// rule from becoming "never fold a division".
    #[test]
    fn divmod_may_trap_allows_the_safe_neighbours() {
        for op in DIVMOD {
            for (a, b) in [
                (12, 4),
                (13, 4),
                (0, 4),            // 0 / c, with the divisor known
                (-2147483647, -1), // one above the overflowing dividend
                (-2147483648, 1),  // the dividend, but not against -1
                (-2147483648, -2),
                (1, -1),
                (i128::from(i32::MAX), -1),
            ] {
                assert!(
                    !divmod_may_trap(op, 32, Some(a), Some(b)),
                    "{op:?} {a} / {b} cannot trap"
                );
            }
            // `x / 1` and `x % 1` fold with the dividend unknown.
            assert!(!divmod_may_trap(op, 32, None, Some(1)), "{op:?} x / 1");
        }
    }

    /// Nothing else in this IR trips the divide trap, so nothing else is asked
    /// to answer for it.
    #[test]
    fn divmod_may_trap_answers_only_for_division() {
        for op in [Opcode::Add, Opcode::Mul, Opcode::Shl, Opcode::Lsr] {
            assert!(!divmod_may_trap(op, 32, Some(1), Some(0)), "{op:?}");
            assert!(!divmod_may_trap(op, 32, None, None), "{op:?}");
        }
    }

    fn binop_at(op: Opcode, size: u32) -> Instruction {
        Instruction::new(op).with_size(size)
    }

    /// `eval_binop` is the one door `instcombine`, `sccp` and `vrp` fold
    /// through, so the refusal has to be visible from there.
    #[test]
    fn eval_binop_does_not_fold_a_trapping_division() {
        for (op, size) in [
            (Opcode::DivS, 32),
            (Opcode::ModS, 32),
            (Opcode::DivS, 64),
            (Opcode::ModS, 64),
        ] {
            let insn = binop_at(op, size);
            assert_eq!(eval_binop(&insn, 1, 0), None, "{op:?}.{size} 1 / 0");
            assert_eq!(
                eval_binop(&insn, signed_min_at(size), -1),
                None,
                "{op:?}.{size} MIN / -1"
            );
        }
        for op in [Opcode::DivU, Opcode::ModU] {
            assert_eq!(eval_binop(&binop_at(op, 32), 1, 0), None, "{op:?} 1 / 0");
        }
    }

    /// And still gives the answers it gave, at the truncation C requires.
    #[test]
    fn eval_binop_still_folds_a_safe_division() {
        for (op, a, b, want) in [
            (Opcode::DivS, 12, 4, 3),
            (Opcode::ModS, 13, 4, 1),
            (Opcode::DivS, -13, 4, -3),
            (Opcode::ModS, -13, 4, -1),
            (Opcode::DivS, 13, -4, -3),
            (Opcode::ModS, 13, -4, 1),
            (Opcode::DivS, -2147483647, -1, 2147483647),
            (Opcode::DivS, -2147483648, 1, -2147483648),
            (Opcode::ModS, -2147483648, 1, 0),
            (Opcode::DivU, 4294967295, 5, 858993459),
            (Opcode::ModU, 4294967295, 5, 0),
        ] {
            assert_eq!(
                eval_binop(&binop_at(op, 32), a, b),
                Some(want),
                "{op:?} {a} op {b}"
            );
        }
    }

    /// `Lsr` is the logical shift at every width, the 128-bit one included,
    /// where the value is not narrowed first and an `i128` shift would be the
    /// arithmetic one. No pass reaches this today (`arch::mapping` decomposes
    /// every 128-bit shift before the optimizer runs), so this pins the
    /// evaluation rather than a pass's behaviour.
    #[test]
    fn eval_shift_lsr_is_logical_at_every_width() {
        assert_eq!(
            eval_binop(&binop_at(Opcode::Lsr, 128), -1, 4),
            Some((u128::MAX >> 4) as i128),
            "-1 >>u 4 at 128 bits fills with zeros"
        );
        assert_eq!(
            eval_binop(&binop_at(Opcode::Lsr, 128), i128::MIN, 127),
            Some(1),
            "the sign bit shifts down to bit 0"
        );
        assert_eq!(
            eval_binop(&binop_at(Opcode::Asr, 128), -1, 4),
            Some(-1),
            "`Asr` is still the arithmetic shift"
        );
        // The narrower widths, which were already right, are unchanged.
        assert_eq!(eval_binop(&binop_at(Opcode::Lsr, 32), -1, 28), Some(15));
        assert_eq!(eval_binop(&binop_at(Opcode::Asr, 32), -1, 28), Some(-1));
        // An out-of-range count is undefined and is not folded, at 128 as
        // anywhere else.
        assert_eq!(eval_binop(&binop_at(Opcode::Lsr, 128), -1, 128), None);
    }

    /// Each bit operation reads only its operand's width, unsigned but for
    /// `Clrsb`, and `ctz`/`clz` of 0 is the width -- gcc's folded value.
    #[test]
    fn eval_bit_op_reads_the_operand_at_its_width() {
        use BitOp::*;
        for (op, width, a, want) in [
            (Bswap, 16, 0x12345, 0x4523),
            (Bswap, 32, 0xff, 0xff00_0000),
            (Bswap, 64, -1, u64::MAX as i128),
            (Ctz, 32, 1 << 40, 32),
            (Ctz, 64, 1 << 40, 40),
            (Ctz, 32, -1, 0),
            (Ctz, 64, 0, 64),
            (Clz, 32, 1, 31),
            (Clz, 64, 1, 63),
            (Clz, 32, 0, 32),
            (Clz, 32, -1, 0),
            (Clrsb, 32, 0, 31),
            (Clrsb, 32, -1, 31),
            (Clrsb, 32, i128::from(i32::MIN), 0),
            (Clrsb, 64, -5, 60),
            (Clrsb, 32, 0xffff_ffff, 31),
            (Popcount, 32, (1 << 40) | 7, 3),
            (Popcount, 64, -1, 64),
            (Ffs, 32, 0, 0),
            (Ffs, 32, 8, 4),
            (Ffs, 64, 1 << 40, 41),
            (Ffs, 32, 1 << 40, 0),
        ] {
            assert_eq!(eval_bit_op(op, width, a), want, "{op:?}.{width} of {a:#x}");
        }
    }

    /// Each bit opcode folds through `eval_unop` at the width it names,
    /// whatever width its `int` result is recorded at.
    #[test]
    fn eval_unop_folds_every_bit_opcode() {
        for (op, a, want) in [
            (Opcode::Bswap16, 0x1234, 0x3412),
            (Opcode::Bswap32, 0x1234_5678, 0x7856_3412),
            (
                Opcode::Bswap64,
                0x0102_0304_0506_0708,
                0x0807_0605_0403_0201,
            ),
            (Opcode::Ctz32, 0, 32),
            (Opcode::Ctz64, 1 << 40, 40),
            (Opcode::Clz32, 1, 31),
            (Opcode::Clz64, 1, 63),
            (Opcode::Popcount32, -1, 32),
            (Opcode::Popcount64, -1, 64),
        ] {
            let insn = Instruction::new(op).with_size(32);
            assert_eq!(eval_unop(&insn, a), Some(want), "{op:?} of {a:#x}");
        }
    }

    /// The one dispatch folds an operation only at its own arity, and
    /// nothing it does not model: an operand list of the wrong length is a
    /// malformed instruction, not one to guess at.
    #[test]
    fn eval_int_folds_each_operation_at_its_own_arity() {
        for (op, ops, want) in [
            (Opcode::Neg, &[5][..], Some(-5)),
            (Opcode::Popcount32, &[0xF0], Some(4)),
            (Opcode::Sub, &[7, 2], Some(5)),
            (Opcode::SetLt, &[1, 2], Some(1)),
            (Opcode::Neg, &[5, 5], None),
            (Opcode::Sub, &[7], None),
            (Opcode::Popcount32, &[], None),
            (Opcode::DivS, &[1, 0], None),
            (Opcode::Load, &[1], None),
            (Opcode::UMulHi, &[1, 1], None),
        ] {
            let insn = Instruction::new(op).with_size(32);
            assert_eq!(eval_int(&insn, ops), want, "{op:?} of {ops:?}");
        }
        for op in [Opcode::Load, Opcode::UMulHi, Opcode::FAdd, Opcode::Lo64] {
            assert!(!is_int_foldable(op), "{op:?}");
        }
    }
}
