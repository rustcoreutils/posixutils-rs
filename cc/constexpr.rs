//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//! The C17 6.6 constant-expression walk, written once.
//!
//! There were two of these: one in the parser, for array bounds, enumerators,
//! `case` labels, bit-field widths and `_Static_assert`, and one in the
//! linearizer, for static initializers and branch folding. They started as
//! copies and drifted -- the parser grew a floating-comparison arm, the
//! linearizer grew `_Alignof` and a `const`-object scope, and each acquired
//! fixes the other did not -- so the same expression could be one value in a
//! declaration and another in the initializer beside it.
//!
//! What the two hosts genuinely need to answer differently is small: what an
//! identifier means, and which struct a member path starts from. That is the
//! [`ConstEnv`] trait; everything else is here.

use crate::parse::ast::{BinaryOp, Expr, ExprKind, OffsetOfPath, UnaryOp};
use crate::symbol::SymbolId;
use crate::types::{TypeId, TypeTable};

/// Which identifiers carry a value in a constant expression.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum ConstScope {
    /// C's own rule: an enumeration constant is the only identifier with a
    /// value here. Array sizes, `case` labels, `_Static_assert`, enumerators
    /// and bit-field widths all ask this one, and gcc is equally strict.
    Standard,
    /// Additionally a `const`-qualified object with a visible constant
    /// initializer, which gcc folds in a static initializer and nowhere else.
    StaticInitializer,
}

/// What the shared walk needs from whichever half of the compiler is asking.
pub(crate) trait ConstEnv {
    fn types(&self) -> &TypeTable;

    /// The value of an identifier, or `None` when it is not a constant in this
    /// scope. An enumeration constant answers in both scopes; a `const` object
    /// answers only in [`ConstScope::StaticInitializer`].
    fn ident_value(&self, sym: SymbolId, scope: ConstScope) -> Option<i128>;

    /// The struct or union a `.`/`->` member lookup should start from, with
    /// typedefs and qualifiers stripped.
    fn struct_of(&self, typ: TypeId) -> TypeId;

    /// The value of a constant *floating* subexpression, folded to `f64`.
    ///
    /// Only two arms need it -- a comparison, whose result is an integer, and
    /// a cast to an integer type -- and `f64` decides both. Each host folds at
    /// its own precision underneath: the linearizer's static initializers are
    /// computed in the expression's own format, which matters for a
    /// `long double` and does not matter for an ordering.
    fn float_value(&self, scope: ConstScope, expr: &Expr) -> Option<f64>;
}

/// Reduce a value to what its type can hold.
///
/// C evaluates every operation *in* a type, and the result of one is an
/// operand of the next. Carrying a full-width `i128` through instead answered
/// `-1u / 3u` with 0 rather than 1431655765, `(unsigned)-1 % 7u` with
/// 4294967295 rather than 3, and `(unsigned char)-1` with -1 rather than 255:
/// the negative was still negative when the division saw it. Applied at every
/// node, this is also what makes a cast convert, since a cast node's type is
/// the type cast to.
///
/// A type wider than the `i128` the walk carries -- `unsigned __int128` --
/// is left alone: there is nothing to reduce it to. The unsigned arms of
/// [`eval_binary`] handle that case directly instead.
fn normalize(env: &impl ConstEnv, typ: Option<TypeId>, value: i128) -> i128 {
    let Some(t) = typ else { return value };
    let types = env.types();
    if !types.is_integer(t) {
        return value;
    }
    // A conversion to `_Bool` yields 0 or 1 (6.3.1.2), not the low byte:
    // `(_Bool)2` is 1, where masking to eight bits left it 2.
    if types.kind(t) == crate::types::TypeKind::Bool {
        return (value != 0) as i128;
    }
    let bits = types.size_bits(t);
    if bits == 0 || bits >= 128 {
        return value;
    }
    let shift = 128 - bits;
    if types.is_unsigned(t) {
        (((value as u128) << shift) >> shift) as i128
    } else {
        (value << shift) >> shift
    }
}

/// Does either operand of a comparison have floating type?
///
/// Asked of the operands rather than the node, because a comparison's own type
/// is `int` whatever it compares.
fn has_float_operand(env: &impl ConstEnv, left: &Expr, right: &Expr) -> bool {
    [left, right]
        .iter()
        .any(|e| e.typ.is_some_and(|t| env.types().is_float(t)))
}

/// Evaluate an integer constant expression, or `None` if it is not one.
///
/// The answer is reduced to what the expression's own type can hold, which is
/// what makes arithmetic wrap where C wraps and a cast convert where C
/// converts. See [`normalize`].
pub(crate) fn eval(env: &impl ConstEnv, scope: ConstScope, expr: &Expr) -> Option<i128> {
    Some(normalize(
        env,
        expr.typ,
        eval_unnormalized(env, scope, expr)?,
    ))
}

fn eval_unnormalized(env: &impl ConstEnv, scope: ConstScope, expr: &Expr) -> Option<i128> {
    // Note the absence of a `FloatLit` arm: a floating literal is deliberately
    // *not* an integer constant expression. 6.6p6 admits one only as the
    // immediate operand of a cast, which the `Cast` arm below handles. Folding
    // it here made `int a[1.5];`, `enum E { X = 1.5 };`, `struct S { int b:1.5; };`
    // and `_Static_assert(1.5, "")` all compile, where gcc rejects each.
    match &expr.kind {
        ExprKind::IntLit(val) => Some(*val as i128),
        ExprKind::Int128Lit(val) => Some(*val),
        ExprKind::CharLit(c) => Some(*c as i128),

        ExprKind::Ident(symbol_id) => env.ident_value(*symbol_id, scope),

        // The address of a member of a pointer constant is itself an integer
        // constant; see [`eval_pointer`].
        ExprKind::Unary {
            op: UnaryOp::AddrOf,
            operand,
        } => eval_pointer(env, scope, operand),

        ExprKind::Unary { op, operand } => {
            let val = eval(env, scope, operand)?;
            match op {
                UnaryOp::Neg => Some(val.wrapping_neg()),
                UnaryOp::Not => Some(if val == 0 { 1 } else { 0 }),
                UnaryOp::BitNot => Some(!val),
                _ => None,
            }
        }

        ExprKind::Binary { op, left, right } => eval_binary(env, scope, *op, left, right),

        ExprKind::Conditional {
            cond,
            then_expr,
            else_expr,
        } => {
            let cond_val = eval(env, scope, cond)?;
            if cond_val != 0 {
                eval(env, scope, then_expr)
            } else {
                eval(env, scope, else_expr)
            }
        }

        // sizeof(type), constant for a complete type but *not* for a variable
        // length array, whose size 6.5.3.4p2 computes at run time. The type
        // table cannot tell `int[n]` from `int[]`, so answering from it alone
        // gave 0 -- and a 0 that was still an integer constant expression, so
        // `int z[sizeof(int[n])];` silently became a zero-length array.
        ExprKind::SizeofType(type_id, dims) => {
            if crate::parse::ast::sizeof_type_is_runtime(env.types(), *type_id, dims) {
                return None;
            }
            Some((env.types().size_bits(*type_id) / 8) as i128)
        }

        ExprKind::SizeofExpr(inner) => {
            // `sizeof a` where `a` is a variable-length array is computed at
            // run time (6.5.3.4p2) and is not an integer constant expression.
            // `SizeofType` grew this guard; this arm did not, so `case sizeof a:`
            // folded to 0 -- `size_bits` reports 0 for an array with no extent --
            // and the case label was silently accepted with the wrong value.
            //
            // A `TypeId` for `int[n]` is indistinguishable from one for `int[]`,
            // so the question has to be asked of the levels, not the size.
            let typ = inner.typ?;
            if env.types().unsized_array_levels(typ) > 0 {
                return None;
            }
            Some((env.types().size_bits(typ) / 8) as i128)
        }

        ExprKind::AlignofType(type_id) => Some(env.types().alignment(*type_id) as i128),

        ExprKind::AlignofExpr(inner) => inner.typ.map(|typ| env.types().alignment(typ) as i128),

        ExprKind::Cast {
            expr: inner,
            cast_type,
        } => {
            // A cast *from* a floating operand truncates the floating value,
            // so the arithmetic below it has to be done in floating point:
            // folding `1.5 + 1.5` through the integer walk made it `1 + 1`,
            // and `(int)(1.5 + 1.5)` came out 2 rather than 3.
            if inner.typ.is_some_and(|t| env.types().is_float(t))
                && env.types().is_integer(*cast_type)
            {
                if let Some(v) = env.float_value(scope, inner) {
                    return Some(v.trunc() as i128);
                }
            }
            eval(env, scope, inner)
        }

        ExprKind::OffsetOf { type_id, path } => {
            let mut offset: i128 = 0;
            let mut current_type = *type_id;

            for element in path {
                match element {
                    OffsetOfPath::Field(field_id) => {
                        let struct_type = env.struct_of(current_type);
                        // None if the field is not found
                        let member_info = env.types().find_member(struct_type, *field_id)?;
                        offset += member_info.offset as i128;
                        current_type = member_info.typ;
                    }
                    OffsetOfPath::Index(index) => {
                        // None if this is not an array type
                        let elem_type = env.types().base_type(current_type)?;
                        offset += *index as i128 * env.types().size_bytes(elem_type) as i128;
                        current_type = elem_type;
                    }
                }
            }

            Some(offset)
        }

        _ => None,
    }
}

fn eval_binary(
    env: &impl ConstEnv,
    scope: ConstScope,
    op: BinaryOp,
    left: &Expr,
    right: &Expr,
) -> Option<i128> {
    // A comparison of *floating* operands has an integer result, so it is an
    // integer constant expression even though neither operand is one --
    // `_Static_assert(1.5 > 1.0, "")` is legal and so is `int a[1.5 > 1.0 ? 4 : 8]`.
    // Truncating each side to `i128` first made `1.5 > 1.0` into `1 > 1`.
    if op.is_comparison() && has_float_operand(env, left, right) {
        let l = env.float_value(scope, left)?;
        let r = env.float_value(scope, right)?;
        return Some(compare(op, l.partial_cmp(&r)?) as i128);
    }

    let l = eval(env, scope, left)?;
    let r = eval(env, scope, right)?;

    // C converts both operands to the common type before operating, so the
    // signedness of the operation is the common type's -- not "either operand
    // is unsigned", and not the left operand's.
    //
    // Both shortcuts get the promotions wrong. `(unsigned char)200 > -1` is a
    // *signed* comparison, because both operands promote to `int` (6.3.1.1p2)
    // and the answer is true; asking whether either operand is unsigned made
    // it false. `-1L < 1u` is *signed* on LP64, because `long` represents
    // every `unsigned int`; the same shortcut made it false.
    let common = match (left.typ, right.typ) {
        (Some(lt), Some(rt)) if env.types().is_integer(lt) && env.types().is_integer(rt) => {
            Some(env.types().common_type(lt, rt))
        }
        _ => None,
    };
    let unsigned = common.is_some_and(|t| env.types().is_unsigned(t));

    // Naming the common type is only half of it: C *converts* both operands to
    // it, and the walk carries each value reduced to its own type instead. For
    // `-1 / 2u` the left operand has to become 4294967295 before the unsigned
    // division, not stay an `i128` -1 whose `as u128` is 2^128-1. Likewise
    // `-1 > 4294967295u` is false, and comparing the raw values as `u128` made
    // it true.
    //
    // The shifts below deliberately use the raw `l` and `r`: 6.5.7p3 promotes
    // each operand separately, so there is no common type to convert to.
    let (lc, rc) = match common {
        Some(t) => (normalize(env, Some(t), l), normalize(env, Some(t), r)),
        None => (l, r),
    };

    if op.is_comparison() {
        if unsigned {
            return Some(compare(op, (lc as u128).cmp(&(rc as u128))) as i128);
        }
        return Some(compare(op, lc.cmp(&rc)) as i128);
    }

    // Division and remainder are the other operations whose answer depends on
    // signedness. `normalize` has already made an unsigned operand
    // non-negative for every width it can represent, so this matters mainly at
    // 128 bits -- where it matters absolutely, since `(unsigned __int128)-1` is
    // a negative `i128` -- and wherever the common type's signedness differs
    // from the left operand's, as in `-1 / 2u`.
    //
    // The shifts below deliberately do *not* use it: 6.5.7p3 promotes each
    // operand separately and makes the result the left operand's type, so a
    // shift follows the left operand alone.
    let left_unsigned = left
        .typ
        .is_some_and(|t| env.types().is_integer(t) && env.types().is_unsigned(t));

    match op {
        BinaryOp::Add => Some(l.wrapping_add(r)),
        BinaryOp::Sub => Some(l.wrapping_sub(r)),
        BinaryOp::Mul => Some(l.wrapping_mul(r)),
        BinaryOp::Div if unsigned => {
            (rc != 0).then(|| ((lc as u128).wrapping_div(rc as u128)) as i128)
        }
        BinaryOp::Mod if unsigned => {
            (rc != 0).then(|| ((lc as u128).wrapping_rem(rc as u128)) as i128)
        }
        BinaryOp::Div => (r != 0).then(|| l.wrapping_div(r)),
        BinaryOp::Mod => (r != 0).then(|| l.wrapping_rem(r)),
        BinaryOp::BitAnd => Some(l & r),
        BinaryOp::BitOr => Some(l | r),
        BinaryOp::BitXor => Some(l ^ r),
        BinaryOp::Shl | BinaryOp::Shr => {
            // Mask the count the way the target hardware does -- 8/16/32-bit
            // masks 31, 64-bit masks 63, 128-bit masks 127 -- so a folded
            // shift agrees with the same shift computed at run time. The
            // operand is integer-promoted first, so the width is never
            // narrower than `int`.
            //
            // 6.5.7p3 makes a count outside [0, width) undefined and gcc
            // folds one to 0 for `<<` while leaving `-1 >> 64` at -1, so
            // there is no single gcc answer to match; c17 diverges knowingly
            // and consistently. What gcc has and c17 does not is the
            // *warning* -- see #C125.
            let width = left
                .typ
                .map(|t| env.types().size_bits(t))
                .unwrap_or(32)
                .max(32);
            let mask: i128 = if width > 64 {
                127
            } else if width > 32 {
                63
            } else {
                31
            };
            let amount = (r & mask) as u32;
            Some(match (op, left_unsigned) {
                (BinaryOp::Shl, _) => l.wrapping_shl(amount),
                // A logical shift, not an arithmetic one. Below 128 bits
                // `normalize` has already cleared the sign bit; at 128 the
                // value really is negative as an `i128`.
                (_, true) => ((l as u128).wrapping_shr(amount)) as i128,
                (_, false) => l.wrapping_shr(amount),
            })
        }
        BinaryOp::LogAnd => Some((l != 0 && r != 0) as i128),
        BinaryOp::LogOr => Some((l != 0 || r != 0) as i128),
        // Every comparison returned above.
        _ => None,
    }
}

/// Turn an ordering into the 0/1 a relational or equality operator yields.
fn compare(op: BinaryOp, ord: std::cmp::Ordering) -> bool {
    use std::cmp::Ordering::*;
    match op {
        BinaryOp::Lt => ord == Less,
        BinaryOp::Le => ord != Greater,
        BinaryOp::Gt => ord == Greater,
        BinaryOp::Ge => ord != Less,
        BinaryOp::Eq => ord == Equal,
        _ => ord != Equal,
    }
}

/// The address of `expr`, when it is an integer constant rather than a
/// symbol's address.
///
/// `(size_t)&((struct S *)0)->member` is how offsetof was spelled before
/// <stddef.h> was relied on to provide it, and it is still what a good many
/// headers expand to. Nothing is dereferenced -- the address of a member of a
/// null pointer is arithmetic on the null pointer -- so there is no symbol to
/// relocate against and the result is a plain integer, usable anywhere an
/// integer constant expression is: an array bound, a case label, a bit-field
/// width.
///
/// The recursion bottoms out only at an integer constant, so `&global.f` is
/// not folded here: that one *does* need a symbol.
pub(crate) fn eval_pointer(env: &impl ConstEnv, scope: ConstScope, expr: &Expr) -> Option<i128> {
    match &expr.kind {
        ExprKind::IntLit(v) => Some(*v as i128),
        ExprKind::Int128Lit(v) => Some(*v),

        ExprKind::Cast { expr: inner, .. } => eval_pointer(env, scope, inner),

        ExprKind::Unary {
            op: UnaryOp::AddrOf,
            operand,
        } => eval_pointer(env, scope, operand),

        ExprKind::Member { expr: base, member } => {
            let base_offset = eval_pointer(env, scope, base)?;
            let struct_type = env.struct_of(base.typ?);
            let member_info = env.types().find_member(struct_type, *member)?;
            Some(base_offset + member_info.offset as i128)
        }

        ExprKind::Arrow { expr: base, member } => {
            let base_offset = eval_pointer(env, scope, base)?;
            let pointee_type = env.types().base_type(base.typ?)?;
            let struct_type = env.struct_of(pointee_type);
            let member_info = env.types().find_member(struct_type, *member)?;
            Some(base_offset + member_info.offset as i128)
        }

        ExprKind::Index { array, index } => {
            let base_offset = eval_pointer(env, scope, array)?;
            // The subscript is an ordinary constant expression, and it is in
            // whatever scope brought us here: routing it through the strict
            // entry point rejected `const int c = 5; int *p = &a[c - 3];`,
            // which the scope mechanism exists to accept.
            let idx = eval(env, scope, index)?;
            let elem_type = env.types().base_type(array.typ?)?;
            Some(base_offset + idx * env.types().size_bytes(elem_type) as i128)
        }

        _ => None,
    }
}
