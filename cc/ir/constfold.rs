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

/// `insn`'s unary operation applied to a constant.
pub(crate) fn eval_unop(insn: &Instruction, a: i128) -> Option<i128> {
    match insn.op {
        Opcode::Neg => Some(a.wrapping_neg()),
        Opcode::Not => Some(!a),
        _ => None,
    }
}
