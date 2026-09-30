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

/// Comparison behavior for identity (x op x) and constant folding
pub(crate) struct CmpInfo {
    /// Result when comparing x to itself (e.g., x == x -> 1, x < x -> 0)
    pub identity_result: i128,
    /// Constant comparison function
    pub compare: fn(i128, i128) -> bool,
    /// How to read the operands at their own width before comparing.
    ///
    /// The opcode is the only thing that carries this: `SetLt`/`Le`/`Gt`/`Ge`
    /// are the signed forms and `SetB`/`Be`/`A`/`Ae` the unsigned ones.
    /// `SetEq`/`SetNe` do not care which, as long as both sides are read the
    /// same way -- but they do care about the width.
    pub signed: bool,
}

/// Get comparison info for the given opcode
pub(crate) fn get_cmp_info(op: Opcode) -> Option<CmpInfo> {
    match op {
        Opcode::SetEq => Some(CmpInfo {
            identity_result: 1,
            compare: |a, b| a == b,
            signed: true,
        }),
        Opcode::SetNe => Some(CmpInfo {
            identity_result: 0,
            compare: |a, b| a != b,
            signed: true,
        }),
        Opcode::SetLt => Some(CmpInfo {
            identity_result: 0,
            compare: |a, b| a < b,
            signed: true,
        }),
        Opcode::SetLe => Some(CmpInfo {
            identity_result: 1,
            compare: |a, b| a <= b,
            signed: true,
        }),
        Opcode::SetGt => Some(CmpInfo {
            identity_result: 0,
            compare: |a, b| a > b,
            signed: true,
        }),
        Opcode::SetGe => Some(CmpInfo {
            identity_result: 1,
            compare: |a, b| a >= b,
            signed: true,
        }),
        Opcode::SetB => Some(CmpInfo {
            identity_result: 0,
            compare: |a, b| (a as u128) < (b as u128),
            signed: false,
        }),
        Opcode::SetBe => Some(CmpInfo {
            identity_result: 1,
            compare: |a, b| (a as u128) <= (b as u128),
            signed: false,
        }),
        Opcode::SetA => Some(CmpInfo {
            identity_result: 0,
            compare: |a, b| (a as u128) > (b as u128),
            signed: false,
        }),
        Opcode::SetAe => Some(CmpInfo {
            identity_result: 1,
            compare: |a, b| (a as u128) >= (b as u128),
            signed: false,
        }),
        _ => None,
    }
}

/// Which orderings of its two operands make a comparison true.
///
/// Every integer comparison is a subset of `{less, equal, greater}`, and the
/// opcode names which subset. Two comparisons over the *same* operand pair can
/// then be answered without knowing the operands at all: `a && b` is never
/// true when their subsets are disjoint, and `a || b` is always true when
/// together they cover all three.
pub(crate) const CMP_LT: u8 = 1;
pub(crate) const CMP_EQ: u8 = 2;
pub(crate) const CMP_GT: u8 = 4;
/// Every ordering: a comparison that is always true.
pub(crate) const CMP_ALL: u8 = CMP_LT | CMP_EQ | CMP_GT;

/// The orderings `op` is true for, or `None` if it is not a comparison.
pub(crate) fn cmp_mask(op: Opcode) -> Option<u8> {
    Some(match op {
        Opcode::SetEq => CMP_EQ,
        Opcode::SetNe => CMP_LT | CMP_GT,
        Opcode::SetLt | Opcode::SetB => CMP_LT,
        Opcode::SetLe | Opcode::SetBe => CMP_LT | CMP_EQ,
        Opcode::SetGt | Opcode::SetA => CMP_GT,
        Opcode::SetGe | Opcode::SetAe => CMP_GT | CMP_EQ,
        _ => return None,
    })
}

/// The fourth outcome a float comparison has and an integer one does not:
/// the operands are *unordered*, because at least one is a NaN.
pub(crate) const CMP_UN: u8 = 8;
/// Every outcome of a float comparison.
pub(crate) const FCMP_ALL: u8 = CMP_ALL | CMP_UN;

/// The outcomes `op` is true for, or `None` if it is not a float comparison.
///
/// Every arm but one is ordered and so excludes [`CMP_UN`]. The exception is
/// `FCmpONe`, which despite its name is C's `!=` -- true when either operand
/// is a NaN -- and is emitted that way by both backends: `setne` OR'd with
/// `setp` on x86-64, `cset ne` (which is taken on unordered) on aarch64.
pub(crate) fn fcmp_mask(op: Opcode) -> Option<u8> {
    Some(match op {
        Opcode::FCmpOEq => CMP_EQ,
        Opcode::FCmpONe => CMP_LT | CMP_GT | CMP_UN,
        Opcode::FCmpOLt => CMP_LT,
        Opcode::FCmpOLe => CMP_LT | CMP_EQ,
        Opcode::FCmpOGt => CMP_GT,
        Opcode::FCmpOGe => CMP_GT | CMP_EQ,
        _ => return None,
    })
}

/// The one outcome two known float values have.
pub(crate) fn fcmp_outcome(ord: Option<Ordering>) -> u8 {
    match ord {
        Some(Ordering::Less) => CMP_LT,
        Some(Ordering::Equal) => CMP_EQ,
        Some(Ordering::Greater) => CMP_GT,
        None => CMP_UN,
    }
}

/// The answer of a float comparison true for the outcomes in `mask`, when
/// every outcome in `possible` gives the same one: 0 when none of them makes
/// it true, 1 when all of them do.
pub(crate) fn fcmp_decided(mask: u8, possible: u8) -> Option<bool> {
    if possible & mask == 0 {
        Some(false)
    } else if possible & !mask & FCMP_ALL == 0 {
        Some(true)
    } else {
        None
    }
}

/// The outcomes of `x` compared with the constant `c`, for an unknown `x`
/// that may be told never to be below zero.
///
/// Nothing is greater than `+Inf`, nothing is less than `-Inf`, and nothing
/// is ordered with a NaN -- including a NaN `x`, which is why an infinity
/// still leaves *unordered* possible. A NaN `x` also leaves `never_below`
/// true, since it is not less than zero either.
pub(crate) fn possible_against(c: FloatVal, never_below: bool) -> u8 {
    let inf = FloatVal::infinity(false);
    let mut possible = match (c.cmp_value(inf), c.cmp_value(inf.negated())) {
        (None, _) => return CMP_UN,
        (Some(Ordering::Equal), _) => FCMP_ALL & !CMP_GT,
        (_, Some(Ordering::Equal)) => FCMP_ALL & !CMP_LT,
        _ => FCMP_ALL,
    };
    if never_below && c.cmp_value(FloatVal::from_f64(0.0)) != Some(Ordering::Greater) {
        possible &= !CMP_LT;
    }
    possible
}

/// The answer of `op` comparing an unknown value with the constant `c`
/// (`c op x` when `const_first`), when no value -- a NaN included -- can
/// change it: `x > +Inf` is 0, `x != NaN` is 1. The one rule the optimizer
/// folds by, and the linearizer too under `-fno-trapping-math`.
pub(crate) fn fcmp_against_constant(op: Opcode, c: FloatVal, const_first: bool) -> Option<bool> {
    let possible = possible_against(c, false);
    let possible = if const_first {
        mirror_mask(possible)
    } else {
        possible
    };
    fcmp_decided(fcmp_mask(op)?, possible)
}

/// The same mask read with the operands the other way round: `a < b` and
/// `b < a` are the same comparison with `less` and `greater` exchanged.
/// Equal and unordered are symmetric and stay where they are.
pub(crate) fn mirror_mask(mask: u8) -> u8 {
    (mask & (CMP_EQ | CMP_UN))
        | if mask & CMP_LT != 0 { CMP_GT } else { 0 }
        | if mask & CMP_GT != 0 { CMP_LT } else { 0 }
}

/// `insn`'s operation applied to two constants, or `None` if the opcode is
/// not one this folds or the operation is undefined for these operands.
///
/// The operands are raw: every narrowing this needs is applied here, and
/// narrowing is idempotent, so a caller that has already read them at their
/// own width may pass those instead.
pub(crate) fn eval_binop(insn: &Instruction, a: i128, b: i128) -> Option<i128> {
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
            let info = get_cmp_info(insn.op)?;
            let size = insn.operand_width();
            let a = at_width(a, size, info.signed);
            let b = at_width(b, size, info.signed);
            Some(if (info.compare)(a, b) { 1 } else { 0 })
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

/// `insn`'s unary operation applied to a constant.
///
/// Includes the integer width conversions, which are unary in the IR and
/// carry the source width in `src_size` -- `sext.8to32` being the shape.
/// The extensions are the whole reason a sub-`int` constant chains at all:
/// every `char` or `short` read is widened before anything is done with it,
/// so a fold that stops at the conversion stops one instruction after it
/// started.
pub(crate) fn eval_unop(insn: &Instruction, a: i128) -> Option<i128> {
    match insn.op {
        Opcode::Neg => Some(a.wrapping_neg()),
        Opcode::Not => Some(!a),
        // The count of the operand at its own width. It is at most 64, so
        // it reads the same at every width the `int` result is taken at.
        Opcode::Popcount32 => Some(i128::from((a as u32).count_ones())),
        Opcode::Popcount64 => Some(i128::from((a as u64).count_ones())),

        // Read the operand at the width it was stored in, in the signedness
        // the opcode names, and leave it there: the destination is wider.
        Opcode::Sext | Opcode::Zext => {
            let src = conversion_src_width(insn)?;
            Some(at_width(a, src, insn.op == Opcode::Sext))
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

        _ => None,
    }
}

/// The width a conversion reads its operand at, or `None` when the
/// instruction does not say.
///
/// Refusing is right rather than guessing from `size`: that is the
/// *destination* width, so an extension read at it is the identity and a
/// negative `char` would come back positive.
fn conversion_src_width(insn: &Instruction) -> Option<u32> {
    (insn.src_size != 0).then_some(insn.src_size)
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
