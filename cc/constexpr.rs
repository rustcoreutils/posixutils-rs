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

use crate::float::{Complex, FloatVal, FpFormat};
use crate::ir::constfold::{eval_bit_op, BitOp};
use crate::parse::ast::{BinaryOp, Expr, ExprKind, FpTest, InlineLibraryFn, OffsetOfPath, UnaryOp};
use crate::strings::StringId;
use crate::symbol::SymbolId;
use crate::target::Target;
use crate::types::{TypeId, TypeKind, TypeTable};

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

    /// The value of a deferred `__builtin_constant_p`, or `None` where the
    /// question is better left to the optimizer.
    ///
    /// The parser builds that node only for an operand it could not fold, so
    /// 0 is its honest answer -- and where C *requires* a constant
    /// expression there has to be one: `__builtin_choose_expr`'s condition,
    /// an array bound, a `case` label, a static initializer. gcc answers 0 in
    /// every one of them.
    ///
    /// Where a fold is merely an optimization the answer must be withheld,
    /// or the builtin is resolved before propagation has run and gives the
    /// wrong one. `linearize_ternary` is the case that matters:
    /// `__builtin_constant_p(x) ? a : b` is the idiom the builtin exists for,
    /// and folding its condition here decides it as 0 forever.
    fn deferred_constant_p(&self, scope: ConstScope) -> Option<i128>;

    /// The value of an identifier of floating type, or `None` when it is not
    /// a constant in this scope: [`Self::ident_value`] for a `const double`.
    fn float_ident_value(&self, sym: SymbolId, scope: ConstScope) -> Option<FloatVal>;

    /// The value of an element or member of an object -- `a[1]`, `s.m` --
    /// or `None` when it is not a constant in this scope. Like a `const`
    /// object named whole ([`Self::ident_value`]), one answers only in
    /// [`ConstScope::StaticInitializer`].
    fn subobject_value(&self, expr: &Expr, scope: ConstScope) -> Option<i128>;

    /// [`Self::subobject_value`] for a floating subobject.
    fn float_subobject_value(&self, expr: &Expr, scope: ConstScope) -> Option<FloatVal>;
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
fn normalize(types: &TypeTable, typ: Option<TypeId>, value: i128) -> i128 {
    let Some(t) = typ else { return value };
    if !types.is_integer(t) {
        return value;
    }
    // A conversion to `_Bool` yields 0 or 1 (6.3.1.2), not the low byte:
    // `(_Bool)2` is 1, where masking to eight bits left it 2.
    if types.kind(t) == TypeKind::Bool {
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

/// Does either operand of a comparison have floating or complex type?
///
/// Asked of the operands rather than the node, because a comparison's own type
/// is `int` whatever it compares.
fn has_float_operand(env: &impl ConstEnv, left: &Expr, right: &Expr) -> bool {
    let types = env.types();
    [left, right].iter().any(|e| {
        e.typ
            .is_some_and(|t| types.is_float(t) || types.is_complex(t))
    })
}

/// Evaluate an integer constant expression, or `None` if it is not one.
///
/// The answer is reduced to what the expression's own type can hold, which is
/// what makes arithmetic wrap where C wraps and a cast convert where C
/// converts. See [`normalize`].
pub(crate) fn eval(env: &impl ConstEnv, scope: ConstScope, expr: &Expr) -> Option<i128> {
    Some(normalize(
        env.types(),
        expr.typ,
        eval_unnormalized(env, scope, expr)?,
    ))
}

fn eval_unnormalized(env: &impl ConstEnv, scope: ConstScope, expr: &Expr) -> Option<i128> {
    if let Some((op, arg)) = bit_builtin(&expr.kind) {
        return eval_bit_builtin(env, scope, op, arg);
    }
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
        ExprKind::Index { .. } | ExprKind::Member { .. } => env.subobject_value(expr, scope),

        // A deferred `__builtin_constant_p`. Whether it answers at all is
        // the asker's business: see [`ConstEnv::deferred_constant_p`].
        ExprKind::ConstantP(_) => env.deferred_constant_p(scope),

        // The address of a member of a pointer constant is itself an integer
        // constant; see [`eval_pointer`].
        ExprKind::Unary {
            op: UnaryOp::AddrOf,
            operand,
        } => eval_pointer(env, scope, operand),

        // `!` asks whether its operand is zero, which a floating or complex
        // operand answers too.
        ExprKind::Unary {
            op: UnaryOp::Not,
            operand,
        } => Some(i128::from(!eval_truth(env, scope, operand)?)),

        // `__real__` and `__imag__` of an integer: a GNU complex integer's
        // half, or a real integer and its zero imaginary part.
        ExprKind::Unary {
            op: op @ (UnaryOp::Real | UnaryOp::Imag),
            operand,
        } => {
            let typ = expr.typ.filter(|&t| env.types().is_integer(t))?;
            let (re, im) = eval_complex(env, scope, operand)?;
            let half = if *op == UnaryOp::Real { re } else { im };
            float_to_integer(env.types(), half, typ)
        }

        ExprKind::Unary { op, operand } => {
            let val = eval(env, scope, operand)?;
            match op {
                UnaryOp::Neg => Some(val.wrapping_neg()),
                UnaryOp::BitNot => Some(!val),
                _ => None,
            }
        }

        ExprKind::Binary { op, left, right } => eval_binary(env, scope, *op, left, right),

        // `signbit` of a floating constant. gcc makes it an integer constant
        // expression, so `enum { E = __builtin_signbit(-1.0) };` and a `case`
        // label of one are accepted in both scopes, as there.
        ExprKind::FpTest {
            test: FpTest::SignBit,
            arg,
        } => Some(i128::from(eval_float(env, scope, arg)?.sign_bit())),

        // `abs` of a constant, in a static initializer only: a call is not an
        // integer constant expression, and gcc rejects `int a[abs(-2)];` at
        // file scope while folding `static int b = abs(-2);`. The argument
        // is at this node's type, and `abs(INT_MIN)` wraps.
        ExprKind::InlineLibraryCall {
            func: InlineLibraryFn::IntAbs,
            args,
            ..
        } if scope == ConstScope::StaticInitializer => match args.as_slice() {
            [x] => Some(eval(env, scope, x)?.wrapping_abs()),
            _ => None,
        },

        // `a ?: b` folds like `a ? a : b`; at constant-evaluation time `a`
        // has no side effects to duplicate.
        ExprKind::CondElvis { cond, else_expr } => {
            let cond_val = eval(env, scope, cond)?;
            if cond_val != 0 {
                Some(cond_val)
            } else {
                eval(env, scope, else_expr)
            }
        }

        ExprKind::Conditional {
            cond,
            then_expr,
            else_expr,
        } => {
            if eval_truth(env, scope, cond)? {
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
            Some(env.types().size_bytes(*type_id) as i128)
        }

        ExprKind::SizeofExpr(inner) => {
            // `sizeof a` where `a` is a variable-length array is computed at
            // run time (6.5.3.4p2) and is not an integer constant expression.
            //
            // A `TypeId` for `int[n]` is indistinguishable from one for `int[]`,
            // so the question has to be asked of the levels, not the size.
            let typ = inner.typ?;
            if env.types().unsized_array_levels(typ) > 0 {
                return None;
            }
            Some(env.types().size_bytes(typ) as i128)
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
            // and `(int)(1.5 + 1.5)` came out 2 rather than 3. A complex
            // operand converts by its real part, or to `_Bool` by both.
            let types = env.types();
            if inner
                .typ
                .is_some_and(|t| types.is_float(t) || types.is_complex(t))
                && types.is_integer(*cast_type)
            {
                return match (eval_as_integer(env, scope, inner, *cast_type)?, scope) {
                    (IntConversion::InRange(v), _) => Some(v),
                    // A value C does not define is no integer constant
                    // expression -- gcc makes `int a[(int)1e300 > 0];` a VLA.
                    (IntConversion::Saturated(_), ConstScope::Standard) => None,
                    // A static initializer must have a value, and gcc's is
                    // the saturated one.
                    (IntConversion::Saturated(v), ConstScope::StaticInitializer) => Some(v),
                };
            }
            eval(env, scope, inner)
        }

        ExprKind::OffsetOf { type_id, path } => offset_of(env, *type_id, path),

        _ => None,
    }
}

/// A bit builtin of a constant, which gcc makes an integer constant
/// expression in every context. The argument is already converted to the
/// builtin's parameter type, whose width the operation reads.
fn eval_bit_builtin(env: &impl ConstEnv, scope: ConstScope, op: BitOp, arg: &Expr) -> Option<i128> {
    let width = env.types().size_bits(arg.typ?);
    Some(eval_bit_op(op, width, eval(env, scope, arg)?))
}

/// The bit operation a bit builtin's node performs, and its argument.
fn bit_builtin(kind: &ExprKind) -> Option<(BitOp, &Expr)> {
    Some(match kind {
        ExprKind::Bswap16 { arg } | ExprKind::Bswap32 { arg } | ExprKind::Bswap64 { arg } => {
            (BitOp::Bswap, arg)
        }
        ExprKind::Ctz { arg } | ExprKind::Ctzl { arg } | ExprKind::Ctzll { arg } => {
            (BitOp::Ctz, arg)
        }
        ExprKind::Clz { arg } | ExprKind::Clzl { arg } | ExprKind::Clzll { arg } => {
            (BitOp::Clz, arg)
        }
        ExprKind::Clrsb { arg } | ExprKind::Clrsbl { arg } | ExprKind::Clrsbll { arg } => {
            (BitOp::Clrsb, arg)
        }
        ExprKind::Popcount { arg } | ExprKind::Popcountl { arg } | ExprKind::Popcountll { arg } => {
            (BitOp::Popcount, arg)
        }
        _ => return None,
    })
}

/// `offsetof(typ, path)`: the byte offset the member designator `path`
/// names inside an object of type `typ`, or `None` if it names nothing.
///
/// The one walk of an `offsetof` path. The linearizer, asked for the value
/// of the same expression outside a constant context, asks this too.
pub(crate) fn offset_of(env: &impl ConstEnv, typ: TypeId, path: &[OffsetOfPath]) -> Option<i128> {
    let mut offset: i128 = 0;
    let mut current = typ;
    for element in path {
        match element {
            OffsetOfPath::Field(field) => {
                let (at, typ) = member_at(env, current, *field)?;
                offset += at;
                current = typ;
            }
            OffsetOfPath::Index(index) => {
                let elem = env.types().base_type(current)?;
                offset += *index as i128 * env.types().size_bytes(elem) as i128;
                current = elem;
            }
        }
    }
    Some(offset)
}

/// The byte offset of `member` inside an object of type `aggregate`, and the
/// member's declared type. `None` if the aggregate has no such member.
///
/// The step `s.m`, `p->m` (with `aggregate` the pointee) and `offsetof` all
/// take, whether folding a constant or placing a static address.
pub(crate) fn member_at(
    env: &impl ConstEnv,
    aggregate: TypeId,
    member: StringId,
) -> Option<(i128, TypeId)> {
    let info = env.types().find_member(aggregate, member)?;
    Some((info.offset as i128, info.typ))
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
        return eval_float_comparison(env, scope, op, left, right);
    }
    // `&&` and `||` ask whether each operand is zero, which a floating or
    // complex operand answers as well as an integer one. The right operand
    // is not evaluated when the left decides (C17 6.5.13p4, 6.5.14p4), and
    // an operand that is not evaluated may be anything (6.6p3): `1 || 1/0`
    // is the constant 1, as `0 ? 1/0 : 2` is 2.
    if matches!(op, BinaryOp::LogAnd | BinaryOp::LogOr) {
        let l = eval_truth(env, scope, left)?;
        if l == (op == BinaryOp::LogOr) {
            return Some(i128::from(l));
        }
        return Some(i128::from(eval_truth(env, scope, right)?));
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
        Some(t) => (
            normalize(env.types(), Some(t), l),
            normalize(env.types(), Some(t), r),
        ),
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
            // and consistently. The *warning* is emitted by
            // `check_shift_count`, where the shift's type is computed.
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
        // Every comparison and logical operator returned above.
        _ => None,
    }
}

/// Evaluate a constant expression of arithmetic type to its exact floating
/// value, or `None` if it is not one.
///
/// The value is the one the expression has *at its own type*: a `double`
/// literal is rounded to `double` here, an operation is done in its result
/// type's format (the one the usual arithmetic conversions gave the node), and
/// a cast converts. So whatever consumes the answer -- a conversion to an
/// integer, a comparison, a static initializer -- sees what the program would
/// compute, and none of it has passed through `f64`.
///
/// An expression of integer type has an integer value, so it is folded by
/// [`eval`] and converted exactly: `(1 / 2) + 0.5` is 0.5, and a cast to an
/// integer type truncates.
pub(crate) fn eval_float(env: &impl ConstEnv, scope: ConstScope, expr: &Expr) -> Option<FloatVal> {
    let types = env.types();
    // A complex value is two reals, which this has no way to answer with.
    if expr.typ.is_some_and(|t| types.is_complex(t)) {
        return None;
    }
    if let Some(t) = expr.typ.filter(|&t| types.is_integer(t)) {
        let v = eval(env, scope, expr)?;
        return Some(integer_to_float(types, t, v));
    }
    let fmt = expr.typ.and_then(|t| types.fp_format(t));
    match &expr.kind {
        ExprKind::FloatLit(v) => Some(fmt.map_or(*v, |f| v.round_to_format(f))),

        ExprKind::Ident(sym) => env.float_ident_value(*sym, scope),
        ExprKind::Index { .. } | ExprKind::Member { .. } => env.float_subobject_value(expr, scope),

        ExprKind::Unary {
            op: UnaryOp::Neg,
            operand,
        } => Some(eval_float(env, scope, operand)?.negated()),

        // A half of a complex value, already at the base type this node has;
        // of a real operand, the value itself and a zero.
        ExprKind::Unary {
            op: op @ (UnaryOp::Real | UnaryOp::Imag),
            operand,
        } => {
            let (re, im) = eval_complex(env, scope, operand)?;
            Some(if *op == UnaryOp::Real { re } else { im })
        }

        ExprKind::Binary { op, left, right } => {
            let l = eval_float(env, scope, left)?;
            let r = eval_float(env, scope, right)?;
            let fmt = fmt?;
            Some(match op {
                BinaryOp::Add => l.add(r, fmt),
                BinaryOp::Sub => l.sub(r, fmt),
                BinaryOp::Mul => l.mul(r, fmt),
                BinaryOp::Div => l.div(r, fmt),
                _ => return None,
            })
        }

        // A cast converts to the target format. Discarding the cast type
        // let `(float)0.1q` keep every bit of its binary128 value in a static
        // initializer, where the same cast at run time rounds.
        ExprKind::Cast {
            expr: inner,
            cast_type,
        } if types.is_float(*cast_type) => eval_as_float(env, scope, inner, *cast_type),

        // The chosen arm, converted to the type the usual arithmetic
        // conversions gave the whole expression.
        ExprKind::Conditional { .. } | ExprKind::CondElvis { .. } => {
            eval_as_float(env, scope, chosen_arm(env, scope, expr)?, expr.typ?)
        }

        // `fabs`, `copysign`, `sqrt`, the roundings, `fmin`, `fmax` and
        // `fma` of constants, whose arguments are already at this node's
        // type. A call is never an integer constant expression, and gcc
        // agrees -- `int a[(int)fabs(-2.0)];` is a VLA there -- but it folds
        // one in a static initializer. One with no answer of its own -- a
        // root's domain error, a `rint(2.5)` that depends on the rounding
        // direction -- is not a constant, there as here.
        ExprKind::InlineLibraryCall { func, args, .. }
            if scope == ConstScope::StaticInitializer =>
        {
            match (func, args.as_slice()) {
                (InlineLibraryFn::Fabs, [x]) => Some(eval_float(env, scope, x)?.magnitude()),
                (InlineLibraryFn::CopySign, [x, y]) => {
                    let sign = eval_float(env, scope, y)?;
                    Some(eval_float(env, scope, x)?.with_sign_of(sign))
                }
                (InlineLibraryFn::Sqrt(_), [x]) => eval_float(env, scope, x)?.sqrt(fmt?),
                (InlineLibraryFn::RoundToIntegral(how), [x]) => {
                    eval_float(env, scope, x)?.round_to_integral(*how, fmt?)
                }
                // gcc does not take a NaN argument to either as a constant,
                // though the optimizer folds one to the other argument.
                (InlineLibraryFn::FMin | InlineLibraryFn::FMax, [x, y]) => {
                    let (x, y) = (eval_float(env, scope, x)?, eval_float(env, scope, y)?);
                    if x.is_nan() || y.is_nan() {
                        return None;
                    }
                    if *func == InlineLibraryFn::FMin {
                        x.fmin(y, fmt?)
                    } else {
                        x.fmax(y, fmt?)
                    }
                }
                (InlineLibraryFn::Fma, [x, y, z]) => {
                    let (x, y) = (eval_float(env, scope, x)?, eval_float(env, scope, y)?);
                    x.fma(y, eval_float(env, scope, z)?, fmt?)
                }
                _ => None,
            }
        }

        _ => None,
    }
}

/// An integer constant of type `typ` as a floating value: exactly, since a
/// 128-bit magnitude fits the significand. `v` is the two's-complement
/// pattern the integer walk carries, so an `unsigned __int128` at or above
/// 2^127 arrives negative and is read back as the unsigned value it is.
pub(crate) fn integer_to_float(types: &TypeTable, typ: TypeId, v: i128) -> FloatVal {
    if types.is_unsigned(typ) {
        FloatVal::from_parts(false, v as u128, 0)
    } else {
        FloatVal::from_i128(v)
    }
}

/// The constant `val`, the value of an expression of type `from`, converted
/// to the floating type `to` as the program converts it at run time: rounded
/// at `from`'s format first, and a NaN quieted when the format changes (see
/// [`FloatVal::convert`]). From an integer, whose value is exact here, it is
/// one rounding; to a type that is not floating, nothing.
pub(crate) fn convert_float(
    types: &TypeTable,
    val: FloatVal,
    from: Option<TypeId>,
    to: TypeId,
) -> FloatVal {
    let src = from.and_then(|t| types.fp_format(t));
    match (src, types.fp_format(to)) {
        (Some(src), Some(dst)) => val.convert(src, dst),
        (None, Some(dst)) => val.round_to_format(dst),
        (_, None) => val,
    }
}

/// The width and signedness [`FloatVal::to_integer`] converts to for the
/// integer type `to`, or `None` for `_Bool`, whose conversion is no
/// truncation: every non-zero value -- 0.5 and a NaN included -- becomes 1
/// (6.3.1.2).
fn integer_shape(types: &TypeTable, to: TypeId) -> Option<(u32, bool)> {
    if types.kind(to) == TypeKind::Bool {
        return None;
    }
    Some((types.size_bits(to).clamp(1, 128), !types.is_unsigned(to)))
}

/// C's conversion of the constant `val` to the integer type `to` (6.3.1.2,
/// 6.3.1.4), or `None` where it is undefined because the value is out of
/// range. `val` is at its own type's format already, as [`eval_float`]
/// leaves it.
pub(crate) fn float_to_integer(types: &TypeTable, val: FloatVal, to: TypeId) -> Option<i128> {
    match integer_shape(types, to) {
        Some((bits, signed)) => val.to_integer(bits, signed),
        None => Some(i128::from(!val.is_zero())),
    }
}

/// [`float_to_integer`] for a context that must have a value, with gcc's
/// saturated answer where C gives none: see
/// [`FloatVal::to_integer_saturating`].
fn float_to_integer_saturating(types: &TypeTable, val: FloatVal, to: TypeId) -> i128 {
    match integer_shape(types, to) {
        Some((bits, signed)) => val.to_integer_saturating(bits, signed),
        None => i128::from(!val.is_zero()),
    }
}

/// Evaluate a constant expression of arithmetic type to its complex value,
/// or `None` if it is not one. A real expression is a complex one with a
/// zero imaginary part, which is also how the run-time lowering promotes it.
///
/// Each half is exact at the base of the expression's own type, as
/// [`eval_float`] leaves a real constant, and an operation is done in that
/// base's format by the algorithm c17 runs for the same operation at run
/// time -- componentwise for `+` and `-`, libgcc's for `*` and `/` (see
/// [`FloatVal::complex_mul`]). A GNU complex integer's halves are integers.
pub(crate) fn eval_complex(env: &impl ConstEnv, scope: ConstScope, expr: &Expr) -> Option<Complex> {
    let types = env.types();
    let typ = expr.typ?;
    if !types.is_complex(typ) {
        return Some((eval_float(env, scope, expr)?, FloatVal::ZERO));
    }
    let base = types.complex_base(typ);
    match &expr.kind {
        // `I` itself is `__builtin_complex(0.0, 1.0)`.
        ExprKind::BuiltinComplex { real, imag } => {
            let half =
                |e: &Expr| convert_complex_half(types, eval_float(env, scope, e)?, e.typ?, base);
            Some((half(real)?, half(imag)?))
        }

        ExprKind::Cast { expr: inner, .. } => eval_complex_as(env, scope, inner, base),

        ExprKind::Conditional { .. } | ExprKind::CondElvis { .. } => {
            eval_complex_as(env, scope, chosen_arm(env, scope, expr)?, base)
        }

        ExprKind::Unary {
            op: UnaryOp::Neg,
            operand,
        } => {
            let (re, im) = eval_complex_as(env, scope, operand, base)?;
            Some((re.negated(), im.negated()))
        }

        // GNU `~z` on a complex operand is the conjugate, as is `conj(z)`:
        // only the imaginary half is negated. On an integer operand `~` is
        // the bitwise complement, which is not a complex fold at all.
        ExprKind::Unary {
            op: UnaryOp::BitNot,
            operand,
        } if operand.typ.is_some_and(|t| types.is_complex(t)) => {
            let (re, im) = eval_complex_as(env, scope, operand, base)?;
            Some((re, im.negated()))
        }
        ExprKind::InlineLibraryCall {
            func: InlineLibraryFn::Conjugate,
            args,
            ..
        } => {
            let [operand] = args.as_slice() else {
                return None;
            };
            let (re, im) = eval_complex_as(env, scope, operand, base)?;
            Some((re, im.negated()))
        }

        // The usual arithmetic conversions have made `base` the type both
        // operands are converted to.
        ExprKind::Binary { op, left, right } => {
            let x = eval_complex_as(env, scope, left, base)?;
            let y = eval_complex_as(env, scope, right, base)?;
            match types.fp_format(base) {
                Some(fmt) => fold_complex_float(*op, x, y, fmt, &types.target()),
                None => fold_complex_int(types, *op, x, y, base),
            }
        }

        _ => None,
    }
}

/// [`eval_complex`], converted to the complex type whose halves are `base`
/// (C17 6.3.1.6: each half converts as a real value of its type would).
pub(crate) fn eval_complex_as(
    env: &impl ConstEnv,
    scope: ConstScope,
    expr: &Expr,
    base: TypeId,
) -> Option<Complex> {
    let types = env.types();
    let (re, im) = eval_complex(env, scope, expr)?;
    let from = types.complex_base(expr.typ?);
    Some((
        convert_complex_half(types, re, from, base)?,
        convert_complex_half(types, im, from, base)?,
    ))
}

/// One half of a complex constant, of the real type `from`, converted to
/// the real type `to`. A GNU complex integer's halves are integers: one
/// converting to another wraps as any integer conversion does, and a
/// floating half converting to one truncates.
fn convert_complex_half(
    types: &TypeTable,
    v: FloatVal,
    from: TypeId,
    to: TypeId,
) -> Option<FloatVal> {
    if !types.is_integer(to) {
        return Some(convert_float(types, v, Some(from), to));
    }
    let n = if types.is_integer(from) {
        normalize(types, Some(to), float_to_integer(types, v, from)?)
    } else {
        float_to_integer(types, v, to)?
    };
    Some(integer_to_float(types, to, n))
}

/// `x op y` for floating complex constants at `fmt`, as c17's run-time
/// lowering computes it: `+` and `-` componentwise, `*` and `/` by the
/// `__mul?c3` and `__div?c3` that lowering calls -- `target`'s own, since
/// Linux ships libgcc's and Apple and FreeBSD ship compiler-rt's, and the
/// two divide differently.
///
/// The optimizer asks the same question of the same method
/// (`ir::libcall_fold::complex`). It may decline where it cannot answer for
/// the routine; this cannot, because a static initializer has to fold or
/// the program does not compile -- which is why folding as libgcc divides
/// wherever the answer was wanted left a static quotient one place from the
/// `__divdc3` beside it on Darwin.
fn fold_complex_float(
    op: BinaryOp,
    x: Complex,
    y: Complex,
    fmt: FpFormat,
    target: &Target,
) -> Option<Complex> {
    Some(match op {
        BinaryOp::Add => (x.0.add(y.0, fmt), x.1.add(y.1, fmt)),
        BinaryOp::Sub => (x.0.sub(y.0, fmt), x.1.sub(y.1, fmt)),
        BinaryOp::Mul => FloatVal::complex_mul(x, y, fmt),
        BinaryOp::Div => {
            let division = target.complex_division(fmt.complex_routine_format());
            FloatVal::complex_div_on(x, y, fmt, division)
        }
        _ => return None,
    })
}

/// `x op y` for GNU complex integers whose halves are `base`, as c17's
/// run-time lowering computes it: the textbook product, and Smith's method
/// for the quotient (see `emit_complex_int_div`), every step at `base`'s
/// width and truncating.
fn fold_complex_int(
    types: &TypeTable,
    op: BinaryOp,
    x: Complex,
    y: Complex,
    base: TypeId,
) -> Option<Complex> {
    let unsigned = types.is_unsigned(base);
    let int = |v: FloatVal| float_to_integer(types, v, base);
    let (a, b, c, d) = (int(x.0)?, int(x.1)?, int(y.0)?, int(y.1)?);
    let w = |v: i128| normalize(types, Some(base), v);
    let add = |p: i128, q: i128| w(p.wrapping_add(q));
    let sub = |p: i128, q: i128| w(p.wrapping_sub(q));
    let mul = |p: i128, q: i128| w(p.wrapping_mul(q));
    // Division by zero has no value; leaving it unfolded leaves the
    // diagnosis to the caller.
    let div = |p: i128, q: i128| match (q, unsigned) {
        (0, _) => None,
        (_, false) => Some(w(p.wrapping_div(q))),
        (_, true) => Some(w((p as u128 / q as u128) as i128)),
    };
    let (re, im) = match op {
        BinaryOp::Add => (add(a, c), add(b, d)),
        BinaryOp::Sub => (sub(a, c), sub(b, d)),
        BinaryOp::Mul => (sub(mul(a, c), mul(b, d)), add(mul(a, d), mul(b, c))),
        BinaryOp::Div => {
            let c_smaller = if unsigned {
                (c as u128) < (d as u128)
            } else {
                w(c.wrapping_abs()) < w(d.wrapping_abs())
            };
            if c_smaller {
                let r = div(c, d)?;
                let denom = add(d, mul(c, r));
                (
                    div(add(mul(a, r), b), denom)?,
                    div(sub(mul(b, r), a), denom)?,
                )
            } else {
                let r = div(d, c)?;
                let denom = add(c, mul(d, r));
                (
                    div(add(a, mul(b, r)), denom)?,
                    div(sub(b, mul(a, r)), denom)?,
                )
            }
        }
        _ => return None,
    };
    Some((
        integer_to_float(types, base, re),
        integer_to_float(types, base, im),
    ))
}

/// The arm a constant `?:` or `?:`-elvis chooses, or `None` when `expr` is
/// neither or its condition is not a constant. The arm still has to be
/// converted to the node's type, which the caller knows how to do.
fn chosen_arm<'e>(env: &impl ConstEnv, scope: ConstScope, expr: &'e Expr) -> Option<&'e Expr> {
    match &expr.kind {
        ExprKind::Conditional {
            cond,
            then_expr,
            else_expr,
        } => Some(if eval_truth(env, scope, cond)? {
            then_expr
        } else {
            else_expr
        }),
        ExprKind::CondElvis { cond, else_expr } => Some(if eval_truth(env, scope, cond)? {
            cond
        } else {
            else_expr
        }),
        _ => None,
    }
}

/// The value `expr` has once converted to a real type: its real part if it is
/// complex (6.3.1.7p2), and the real type that part has.
fn real_part(
    env: &impl ConstEnv,
    scope: ConstScope,
    expr: &Expr,
) -> Option<(FloatVal, Option<TypeId>)> {
    let types = env.types();
    match expr.typ {
        Some(t) if types.is_complex(t) => Some((
            eval_complex(env, scope, expr)?.0,
            Some(types.complex_base(t)),
        )),
        _ => Some((eval_float(env, scope, expr)?, expr.typ)),
    }
}

/// The constant `expr` converted to the real floating type `to`, as a cast
/// or an assignment converts it: rounded from its own type, and a complex
/// value's imaginary part discarded.
pub(crate) fn eval_as_float(
    env: &impl ConstEnv,
    scope: ConstScope,
    expr: &Expr,
    to: TypeId,
) -> Option<FloatVal> {
    let (v, from) = real_part(env, scope, expr)?;
    Some(convert_float(env.types(), v, from, to))
}

/// A constant converted to an integer type: in range, or out of range with
/// gcc's saturated value standing in for the one C does not define.
pub(crate) enum IntConversion {
    InRange(i128),
    Saturated(i128),
}

/// The constant `expr` of floating or complex type converted to the integer
/// type `to`, as a cast or an assignment converts it (6.3.1.2, 6.3.1.4,
/// 6.3.1.7): `_Bool` asks whether the value is zero, any other integer type
/// truncates the real part. A complex integer's real part is already an
/// integer, and the caller's reduction to `to` wraps it.
pub(crate) fn eval_as_integer(
    env: &impl ConstEnv,
    scope: ConstScope,
    expr: &Expr,
    to: TypeId,
) -> Option<IntConversion> {
    let types = env.types();
    if types.kind(to) == TypeKind::Bool {
        return Some(IntConversion::InRange(i128::from(eval_truth(
            env, scope, expr,
        )?)));
    }
    let (v, from) = real_part(env, scope, expr)?;
    if let Some(from) = from.filter(|&f| types.is_integer(f)) {
        return Some(IntConversion::InRange(float_to_integer(types, v, from)?));
    }
    Some(match float_to_integer(types, v, to) {
        Some(n) => IntConversion::InRange(n),
        None => IntConversion::Saturated(float_to_integer_saturating(types, v, to)),
    })
}

/// Whether a scalar constant is non-zero -- what `!`, `&&`, `||` and a
/// condition ask of it (6.5.3.3p5, 6.5.13, 6.5.15p4). A floating value is
/// zero only as either zero, so a NaN is true; a complex one is zero only
/// when both halves are.
pub(crate) fn eval_truth(env: &impl ConstEnv, scope: ConstScope, expr: &Expr) -> Option<bool> {
    let types = env.types();
    match expr.typ {
        Some(t) if types.is_complex(t) => {
            let (re, im) = eval_complex(env, scope, expr)?;
            Some(!re.is_zero() || !im.is_zero())
        }
        Some(t) if types.is_float(t) => Some(!eval_float(env, scope, expr)?.is_zero()),
        _ => Some(eval(env, scope, expr)? != 0),
    }
}

/// C's `op`, a relational or equality operator, on two floating values
/// already converted to their common real type. Exact, since `cmp_value`
/// is; unordered, a NaN is unequal to everything and ordered against
/// nothing, so only `!=` holds.
fn compare_floats(op: BinaryOp, l: FloatVal, r: FloatVal) -> bool {
    match l.cmp_value(r) {
        Some(ord) => compare(op, ord),
        None => op == BinaryOp::Ne,
    }
}

/// A comparison with a floating or complex operand. Both operands convert to
/// their common type first (6.5.8p3, 6.5.9p4; 6.3.1.8), then compare exactly
/// in it: a complex pair is equal when both halves are. Only `==` and `!=`
/// take a complex operand -- the parser rejects an ordering of one.
fn eval_float_comparison(
    env: &impl ConstEnv,
    scope: ConstScope,
    op: BinaryOp,
    left: &Expr,
    right: &Expr,
) -> Option<i128> {
    let types = env.types();
    let common = types.common_type(left.typ?, right.typ?);
    if types.is_complex(common) {
        if !matches!(op, BinaryOp::Eq | BinaryOp::Ne) {
            return None;
        }
        let base = types.complex_base(common);
        let x = eval_complex_as(env, scope, left, base)?;
        let y = eval_complex_as(env, scope, right, base)?;
        let equal =
            compare_floats(BinaryOp::Eq, x.0, y.0) && compare_floats(BinaryOp::Eq, x.1, y.1);
        return Some(i128::from(equal == (op == BinaryOp::Eq)));
    }
    let l = eval_as_float(env, scope, left, common)?;
    let r = eval_as_float(env, scope, right, common)?;
    Some(i128::from(compare_floats(op, l, r)))
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
            Some(base_offset + member_at(env, base.typ?, *member)?.0)
        }

        ExprKind::Arrow { expr: base, member } => {
            let base_offset = eval_pointer(env, scope, base)?;
            let pointee = env.types().base_type(base.typ?)?;
            Some(base_offset + member_at(env, pointee, *member)?.0)
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
