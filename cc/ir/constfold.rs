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
use crate::types::{TypeId, TypeTable};
use std::cmp::Ordering;

use super::{Instruction, Opcode};

/// Does `v` mean the same thing at `size` bits whether it is read as signed
/// or as unsigned?
///
/// An `i128` in this IR holds whatever bit pattern its constant was built
/// from, and nothing on the instruction says how to read it back: the same
/// 32 bits are -1 or 4294967295 depending on the consumer, and a `Set*`
/// cannot even ask, because its `size` is the width of its own `_Bool`/`int`
/// result rather than of its operands (`(_Bool)x` lowers to `setne.8` over
/// two 32-bit values).
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

/// The same mask read with the operands the other way round: `a < b` and
/// `b < a` are the same comparison with `less` and `greater` exchanged.
pub(crate) fn mirror_mask(mask: u8) -> u8 {
    (mask & CMP_EQ)
        | if mask & CMP_LT != 0 { CMP_GT } else { 0 }
        | if mask & CMP_GT != 0 { CMP_LT } else { 0 }
}

/// The width a `Set*` reads its operands at.
///
/// `insn.size` is the width of the *result* at one of the four `Set*`
/// construction sites -- a `_Bool` conversion lowers to `setne.8` over two
/// 32-bit operands -- and that site is the only one that records the operand
/// width, in `src_size`. Preferring `src_size` when it is set is right at all
/// four.
pub(crate) fn cmp_operand_width(insn: &Instruction) -> u32 {
    if insn.src_size != 0 {
        insn.src_size
    } else {
        insn.size.max(1)
    }
}

/// True for the opcodes that record their *operands* in `typ`/`size` and
/// produce an `int`.
///
/// Integer and floating comparisons both, which is the whole set: no other
/// opcode describes anything but its own result there.
pub(crate) fn is_comparison(op: Opcode) -> bool {
    cmp_mask(op).is_some()
        || matches!(
            op,
            Opcode::FCmpOEq
                | Opcode::FCmpONe
                | Opcode::FCmpOLt
                | Opcode::FCmpOLe
                | Opcode::FCmpOGt
                | Opcode::FCmpOGe
        )
}

/// The type and width `insn` leaves in its target.
///
/// A rewrite that replaces an instruction with a `Copy` of its value has to
/// give that copy the type of the *value*, and for all but one family of
/// opcodes `insn.typ`/`insn.size` are exactly that. The exception is the
/// comparisons, which describe their operands there (see
/// [`cmp_operand_width`]) and produce an `int`.
///
/// Carrying the operand type across is invisible for an integer comparison --
/// an integer of the wrong width still lands in a general register -- and a
/// miscompile for a float one: the folded constant is typed `double`, so the
/// backend puts it in an SSE register and the caller reads the return value
/// out of the wrong one.
pub(crate) fn result_type_of(insn: &Instruction, types: &TypeTable) -> (Option<TypeId>, u32) {
    if is_comparison(insn.op) {
        let int_id = types.int_id;
        (Some(int_id), types.size_bits(int_id))
    } else {
        (insn.typ, insn.size)
    }
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
            let size = cmp_operand_width(insn);
            let a = at_width(a, size, info.signed);
            let b = at_width(b, size, info.signed);
            Some(if (info.compare)(a, b) { 1 } else { 0 })
        }
    }
}

/// Division and remainder.
///
/// Read at the operand's own width, in the signedness the opcode implies.
/// Division is not congruent modulo 2^n the way add/sub/mul are: it reads the
/// whole value and its sign, so `(int)0xFFFFFFFFu` arriving as 4294967295
/// rather than -1 answered 2147483647 where C says 0.
fn eval_divmod(insn: &Instruction, a: i128, b: i128) -> Option<i128> {
    let signed = matches!(insn.op, Opcode::DivS | Opcode::ModS);
    let size = insn.size.max(1);
    let a = at_width(a, size, signed);
    let b = at_width(b, size, signed);
    if b == 0 {
        return None;
    }
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
        Opcode::Lsr => at_width(a, size, false).wrapping_shr(b as u32),
        Opcode::Asr => at_width(a, size, true).wrapping_shr(b as u32),
        _ => return None,
    })
}

/// A float comparison over two constants, at the format its operands are in.
///
/// Two things here are not guessable from the opcode name, and both change
/// the answer rather than its precision.
///
/// **`FCmpONe` is the unordered form.** It is C's `!=`, which is *true* when
/// either operand is a NaN, and both backends emit it that way -- on x86-64
/// as `setne` OR'd with `setp`, where every other arm AND's in `setnp` or
/// relies on `ucomisd` setting CF for unordered. Folding it as the ordered
/// comparison its name suggests would make a folded program disagree with
/// the same program unfolded.
///
/// **The operands must be rounded first.** A [`FloatVal`] carries the literal
/// at 128 significand bits, wider than any target format, and rounds to the
/// target once on the way out -- so the constant in a `double` expression is
/// not yet the `double` the program computes with. Comparing unrounded makes
/// `0.1 + 0.2 == 0.3` true, which in `double` it is not.
pub(crate) fn eval_fcmp(op: Opcode, fmt: FpFormat, a: FloatVal, b: FloatVal) -> Option<i128> {
    let ord = a.round_to_format(fmt).cmp_value(b.round_to_format(fmt));
    let r = match op {
        Opcode::FCmpOEq => ord == Some(Ordering::Equal),
        Opcode::FCmpONe => ord != Some(Ordering::Equal),
        Opcode::FCmpOLt => ord == Some(Ordering::Less),
        Opcode::FCmpOLe => matches!(ord, Some(Ordering::Less | Ordering::Equal)),
        Opcode::FCmpOGt => ord == Some(Ordering::Greater),
        Opcode::FCmpOGe => matches!(ord, Some(Ordering::Greater | Ordering::Equal)),
        _ => return None,
    };
    Some(i128::from(r))
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
pub(crate) fn eval_fbinop(op: Opcode, fmt: FpFormat, a: FloatVal, b: FloatVal) -> Option<FloatVal> {
    let (a, b) = (a.round_to_format(fmt), b.round_to_format(fmt));
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

/// A float unary operation over a constant.
///
/// `FNeg` alone, and unlike the arithmetic above it folds for every operand
/// including the infinities and NaN: flipping a sign bit computes nothing
/// and raises nothing. It is also exact, so the result needs no rounding
/// that the operand has not already had.
pub(crate) fn eval_funop(op: Opcode, fmt: FpFormat, a: FloatVal) -> Option<FloatVal> {
    match op {
        Opcode::FNeg => Some(a.round_to_format(fmt).negated()),
        _ => None,
    }
}

/// A float-to-float conversion of a constant, from one format to another.
///
/// **Rounded twice, and both roundings are load-bearing.** The operand is a
/// literal at 128 significand bits, not yet the value its own type holds, so
/// rounding straight to the destination skips a step the program does not:
/// `(float)(_Float16)0.3f16` is `0.30004883`, the nearest `float` to the
/// nearest `_Float16` to `0.3`, and converting in one go gives `0.3f`
/// instead. Widening looks harmless and is not -- that is the direction
/// this got wrong.
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
    if op != Opcode::FCvtF {
        return None;
    }
    let a = a.round_to_format(src_fmt);
    if !a.is_finite() {
        return None;
    }
    let r = a.round_to_format(dst_fmt);
    r.is_finite().then_some(r)
}

/// A float-to-integer conversion of a constant, to `dst_size` bits.
///
/// `None` when the value does not fit, which is exactly where C leaves the
/// conversion undefined (6.3.1.4): a folded answer there would be this
/// compiler's invention rather than the target's, and the two differ.
pub(crate) fn eval_fcvt(op: Opcode, dst_size: u32, src_fmt: FpFormat, a: FloatVal) -> Option<i128> {
    let signed = match op {
        Opcode::FCvtS => true,
        Opcode::FCvtU => false,
        _ => return None,
    };
    let v = a.round_to_format(src_fmt).trunc_to_i128()?;
    let size = dst_size.max(1);
    if !signed && v < 0 {
        return None;
    }
    (at_width(v, size, signed) == v).then_some(v)
}

/// `insn`'s unary operation applied to a constant.
pub(crate) fn eval_unop(insn: &Instruction, a: i128) -> Option<i128> {
    match insn.op {
        Opcode::Neg => Some(a.wrapping_neg()),
        Opcode::Not => Some(!a),
        _ => None,
    }
}
