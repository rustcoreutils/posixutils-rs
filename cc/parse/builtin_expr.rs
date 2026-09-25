//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GCC/clang __builtin_* expressions, and the object-size and _chk
// support they need
//

use super::ast::{
    BinaryOp, CheckedOp, Expr, ExprKind, FpCompare, FpTest, GnuAtomicOp, OffsetOfPath, UnaryOp,
};
use super::parser::{ParseError, ParseResult, Parser};
use crate::diag;
use crate::float::FloatVal;
use crate::strings::StringId;
use crate::symbol::{Namespace, Symbol, SymbolId};
use crate::token::lexer::Position;
use crate::types::{Type, TypeId, TypeKind};
use gettextrs::gettext;

/// A statically known object, and where inside it a pointer points.
///
/// `whole` / `offset` describe the complete object; `sub` / `sub_offset`
/// describe the innermost aggregate containing the designated byte. For a
/// plain array the two coincide.
#[derive(Clone, Copy)]
pub(crate) struct ObjectExtent {
    whole: u64,
    offset: u64,
    sub: u64,
    sub_offset: u64,
}

impl ObjectExtent {
    /// A pointer to the start of an object of `size` bytes.
    fn whole_of(size: u64) -> Option<Self> {
        Some(ObjectExtent {
            whole: size,
            offset: 0,
            sub: size,
            sub_offset: 0,
        })
    }

    /// Move the pointer by `bytes`, staying inside both objects.
    fn advance(self, bytes: i128) -> Option<Self> {
        let offset = i128::from(self.offset).checked_add(bytes)?;
        let sub_offset = i128::from(self.sub_offset).checked_add(bytes)?;
        // Out of bounds either way: gcc gives up rather than reporting a size
        // that would licence an overrun.
        if offset < 0 || offset > i128::from(self.whole) {
            return None;
        }
        if sub_offset < 0 || sub_offset > i128::from(self.sub) {
            return None;
        }
        Some(ObjectExtent {
            offset: offset as u64,
            sub_offset: sub_offset as u64,
            ..self
        })
    }

    /// Step into a member at `offset` bytes with size `size`, which becomes
    /// the new innermost subobject.
    fn narrow(self, offset: u64, size: u64) -> Option<Self> {
        Some(ObjectExtent {
            whole: self.whole,
            offset: self.offset.checked_add(offset)?,
            sub: size,
            sub_offset: 0,
        })
    }

    /// Bytes from the pointer to the end of the selected object.
    fn remaining(self, closest_subobject: bool) -> u64 {
        if closest_subobject {
            self.sub.saturating_sub(self.sub_offset)
        } else {
            self.whole.saturating_sub(self.offset)
        }
    }
}

/// Which real floating type a libm entry point computes in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LibmReal {
    Float,
    Double,
    LongDouble,
}

impl Parser<'_> {
    /// Whether an expression is a literal constant, with nothing to evaluate.
    ///
    /// Used to decide whether a discarded operand can be dropped outright or
    /// has to be kept for its side effects. Deliberately conservative: it
    /// answers `true` only for the shapes that plainly compute nothing, so a
    /// wrong answer keeps a harmless dead operand rather than losing a side
    /// effect.
    fn is_literal_constant(expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::IntLit(_)
            | ExprKind::FloatLit(_)
            | ExprKind::CharLit(_)
            | ExprKind::StringLit(_) => true,
            ExprKind::Cast { expr: inner, .. } => Self::is_literal_constant(inner),
            ExprKind::Unary { op, operand } => {
                matches!(op, UnaryOp::Neg | UnaryOp::BitNot | UnaryOp::Not)
                    && Self::is_literal_constant(operand)
            }
            _ => false,
        }
    }

    /// Whether a declaration in scope displaces the builtin meaning the parser
    /// would otherwise give `name_id`.
    ///
    /// `offsetof`, `alignof`, `setjmp` and `longjmp` are not C17 keywords --
    /// the first is a macro, the second a C23 spelling, the last two ordinary
    /// library functions -- so a program may use any of them as an identifier,
    /// and gcc accepts that. `setjmp` and `longjmp` are the exception, and
    /// only against a *function* declaration: `<setjmp.h>` declares exactly
    /// those, and they need code generation an ordinary call cannot produce.
    ///
    /// The reserved spellings (`__builtin_*`, `_Alignof`, `__alignof__`) are
    /// never displaced: C17 7.1.3 reserves them to the implementation in every
    /// scope, so a program that declares one has no claim on the name.
    pub(super) fn builtin_is_shadowed(&self, name_id: StringId) -> bool {
        let shadowed_by_any_decl = matches!(name_id, crate::kw::OFFSETOF | crate::kw::ALIGNOF_C23);
        let shadowable = shadowed_by_any_decl
            || matches!(
                name_id,
                crate::kw::SETJMP
                    | crate::kw::SETJMP2
                    | crate::kw::LONGJMP
                    | crate::kw::LONGJMP2
                    | crate::kw::ALLOCA
                    | crate::kw::FABS
                    | crate::kw::FABSF
                    | crate::kw::FABSL
                    | crate::kw::FLOOR
                    | crate::kw::CEIL
                    | crate::kw::TRUNC
                    | crate::kw::ROUND
                    | crate::kw::RINT
                    | crate::kw::NEARBYINT
            );
        if !shadowable {
            return false;
        }

        // `-fno-builtin` / `-fno-builtin-NAME` turn the bare spellings off
        // outright, whether or not anything declares them. gcc's rule is that
        // the flag disables builtins not beginning with `__builtin_`, and
        // `shadowable` is exactly that set -- the names that are not reserved
        // to the implementation, and so are the user's to mean something else
        // by.
        if let Some(name) = self.idents.get_opt(name_id) {
            if crate::builtins::bare_builtin_disabled(name) {
                return true;
            }
        }

        let Some(symbol_id) = self.symbols.lookup_id(name_id, Namespace::Ordinary) else {
            return false;
        };
        shadowed_by_any_decl
            || self.types.kind(self.symbols.get(symbol_id).typ) != TypeKind::Function
    }

    /// Try to parse a builtin function expression.
    /// Returns `Some(result)` if `name_id` is a recognized builtin, `None` otherwise.
    /// A `__builtin_*(x)` taking one argument, wrapping it in `kind`, and
    /// carrying result type `typ`.
    fn parse_unary_builtin(
        &mut self,
        token_pos: Position,
        typ: TypeId,
        kind: fn(Box<Expr>) -> ExprKind,
    ) -> ParseResult<Expr> {
        self.expect_special(b'(')?;
        let arg = self.parse_assignment_expr()?;
        self.expect_special(b')')?;
        Ok(Self::typed_expr(kind(Box::new(arg)), typ, token_pos))
    }

    /// The parenthesised level of `__builtin_frame_address` or
    /// `__builtin_return_address`.
    ///
    /// It must be a non-negative integer constant: the backend walks that
    /// many frame records, and there is no loop for a run-time count. gcc
    /// rejects anything else with the same "invalid argument" wording.
    fn parse_frame_level(&mut self, builtin: &str) -> ParseResult<u32> {
        self.expect_special(b'(')?;
        let level = self.parse_assignment_expr()?;
        self.expect_special(b')')?;
        self.eval_const_expr(&level)
            .and_then(|n| u32::try_from(n).ok())
            .ok_or_else(|| {
                ParseError::new(
                    format!(
                        "invalid argument to '{builtin}': \
                         the level must be a non-negative integer constant"
                    ),
                    level.pos,
                )
            })
    }

    /// `fabs(x)` / `fabsf(x)`, whose opcode reads its operand at a fixed
    /// width, so the argument has to arrive already converted.
    ///
    /// `fabs(-3)` is a well-formed call under the prototype the standard
    /// gives it -- and under the `extern double fabs(double);` a program
    /// writes for itself -- but the opcode is an SSE instruction that masks
    /// the sign bit, so an unconverted `int` operand is read as a `double`
    /// bit pattern and the answer is nonsense.
    fn parse_fabs(
        &mut self,
        token_pos: Position,
        typ: TypeId,
        kind: fn(Box<Expr>) -> ExprKind,
    ) -> ParseResult<Expr> {
        self.expect_special(b'(')?;
        let arg = self.parse_assignment_expr()?;
        self.expect_special(b')')?;
        let arg = if arg.typ == Some(typ) {
            arg
        } else {
            Self::typed_expr(
                ExprKind::Cast {
                    cast_type: typ,
                    expr: Box::new(arg),
                },
                typ,
                token_pos,
            )
        };
        Ok(Self::typed_expr(kind(Box::new(arg)), typ, token_pos))
    }

    /// One of the C99 7.12.14 relations: `__builtin_isgreater` and its five
    /// siblings.
    ///
    /// The operands go through the usual arithmetic conversions, as the
    /// relational operators they stand for do, so `isless(1, 2.0)` compares
    /// two `double`s rather than reading an `int` as one. The result is `int`,
    /// 0 or 1.
    ///
    /// The comparison itself is desugared in the linearizer: writing it out as
    /// `a < b` here would duplicate the operand expressions, and
    /// `isunordered(f(), g())` must call each function once.
    fn parse_fp_compare(&mut self, token_pos: Position, cmp: FpCompare) -> ParseResult<Expr> {
        self.expect_special(b'(')?;
        let lhs = self.parse_assignment_expr()?;
        self.expect_special(b',')?;
        let rhs = self.parse_assignment_expr()?;
        self.expect_special(b')')?;

        let common = match (lhs.typ, rhs.typ) {
            (Some(l), Some(r)) => self.types.common_type(l, r),
            (Some(t), None) | (None, Some(t)) => t,
            (None, None) => self.types.double_id,
        };
        // A relation between two integers is not what these are for, but gcc
        // accepts it and answers the ordinary comparison; converting to a real
        // floating type keeps one lowering path rather than two.
        let common = if self.types.is_float(common) {
            common
        } else {
            self.types.double_id
        };

        let lhs = self.converted_to(lhs, common, token_pos);
        let rhs = self.converted_to(rhs, common, token_pos);
        Ok(Self::typed_expr(
            ExprKind::FpCompare {
                cmp,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            self.types.int_id,
            token_pos,
        ))
    }

    /// `expr` as `typ`, adding a cast only when one is needed.
    fn converted_to(&mut self, expr: Expr, typ: TypeId, pos: Position) -> Expr {
        if expr.typ == Some(typ) {
            return expr;
        }
        Self::typed_expr(
            ExprKind::Cast {
                cast_type: typ,
                expr: Box::new(expr),
            },
            typ,
            pos,
        )
    }

    /// A libm call that narrows to its `float` form when its argument is one.
    ///
    /// `(float)floor((double)x)` is `floorf(x)` exactly: the result is an
    /// integer no greater in magnitude than `x`, so a value representable as
    /// a `float` stays representable, and converting it up to `double` and
    /// back changes nothing. The condition is on the **argument** type and
    /// not the result -- `double q(float a) { return floor(a); }` narrows
    /// too, because the narrowing happens before the widening.
    ///
    /// Only the exactly-rounding functions qualify, which is why the set is
    /// enumerated rather than derived. `sin` and `log` are not among them:
    /// `sinf(x)` and `(float)sin((double)x)` differ in the last bit for some
    /// `x`, and narrowing one is a wrong answer rather than a faster one.
    fn narrowing_libm_call(
        &mut self,
        wide: &str,
        narrow: &str,
        arg: Expr,
        pos: Position,
    ) -> ParseResult<Expr> {
        let is_float = arg
            .typ
            .and_then(|t| self.types.fp_format(t))
            .is_some_and(|fmt| fmt == crate::float::FpFormat::Binary32);
        Ok(if is_float {
            let f = self.types.float_id;
            self.libm_call(narrow, f, &[f], arg, pos)
        } else {
            let d = self.types.double_id;
            self.libm_call(wide, d, &[d], arg, pos)
        })
    }

    /// `fabsl(x)`, lowered as an ordinary call to `fabsl` rather than as an
    /// opcode: the `Fabs64` opcode moves its argument as a `double`, so on
    /// x86-64 it would read only the low eight bytes of an 80-bit x87 value.
    /// A real call gets the long-double ABI from the call path, which already
    /// carries one for `__mulxc3`.
    fn parse_fabsl(&mut self, token_pos: Position) -> ParseResult<Expr> {
        self.expect_special(b'(')?;
        let arg = self.parse_assignment_expr()?;
        self.expect_special(b')')?;
        let ld = self.types.longdouble_id;
        Ok(self.libm_call("fabsl", ld, &[ld], arg, token_pos))
    }

    /// `__builtin_va_*`: the variadic-argument builtins, plus `_Generic`.
    fn parse_varargs_builtin(
        &mut self,
        name_id: StringId,
        token_pos: Position,
    ) -> Option<ParseResult<Expr>> {
        match name_id {
            crate::kw::BUILTIN_VA_ARG_PACK | crate::kw::BUILTIN_VA_ARG_PACK_LEN => Some((|| {
                let is_len = name_id == crate::kw::BUILTIN_VA_ARG_PACK_LEN;
                let spelling = if is_len {
                    "__builtin_va_arg_pack_len"
                } else {
                    "__builtin_va_arg_pack"
                };
                self.expect_special(b'(')?;
                self.expect_special(b')')?;

                // Both name the *caller's* variadic arguments, so there has to
                // be a caller whose arguments are known: the enclosing
                // function must be variadic, and must be `always_inline` so
                // that the call site is substituted in. Checked here because
                // this is the last point where either fact is visible --
                // `ir::Function` records neither, and the backends infer
                // variadic-ness from the presence of `va_start`.
                if !self.in_forwarding_function {
                    crate::diag::error_args(
                        token_pos,
                        "'{0}' may only be used in a variadic function declared __attribute__((always_inline))",
                        &[spelling],
                    );
                }

                let (kind, typ) = if is_len {
                    (ExprKind::VaArgPackLen, self.types.int_id)
                } else {
                    (ExprKind::VaArgPack, self.types.void_id)
                };
                Ok(Self::typed_expr(kind, typ, token_pos))
            })(
            )),

            crate::kw::BUILTIN_VA_START => Some((|| {
                // __builtin_va_start(ap, last_param)
                self.expect_special(b'(')?;
                let ap = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                // Second arg is a parameter name
                let last_param = self.expect_identifier()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::VaStart {
                        ap: Box::new(ap),
                        last_param,
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::GENERIC => Some(self.parse_generic_selection(token_pos)),
            crate::kw::BUILTIN_VA_ARG => Some((|| {
                // __builtin_va_arg(ap, type)
                self.expect_special(b'(')?;
                let ap = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                // Second arg is a type
                let arg_type = self.parse_type_name()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::VaArg {
                        ap: Box::new(ap),
                        arg_type,
                    },
                    arg_type,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_VA_END => Some((|| {
                // __builtin_va_end(ap)
                self.expect_special(b'(')?;
                let ap = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::VaEnd { ap: Box::new(ap) },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_VA_COPY => Some((|| {
                // __builtin_va_copy(dest, src)
                self.expect_special(b'(')?;
                let dest = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let src = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::VaCopy {
                        dest: Box::new(dest),
                        src: Box::new(src),
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            _ => None,
        }
    }

    /// Byte swaps, bit counts, checked arithmetic and `__builtin_choose_expr`.
    fn parse_bit_builtin(
        &mut self,
        name_id: StringId,
        token_pos: Position,
    ) -> Option<ParseResult<Expr>> {
        match name_id {
            crate::kw::BUILTIN_BSWAP16 => Some(self.parse_unary_builtin(
                token_pos,
                self.types.ushort_id,
                |arg| ExprKind::Bswap16 { arg },
            )),

            crate::kw::BUILTIN_BSWAP32 => Some(self.parse_unary_builtin(
                token_pos,
                self.types.uint_id,
                |arg| ExprKind::Bswap32 { arg },
            )),

            crate::kw::BUILTIN_BSWAP64 => Some(self.parse_unary_builtin(
                token_pos,
                self.types.ulonglong_id,
                |arg| ExprKind::Bswap64 { arg },
            )),

            crate::kw::BUILTIN_CTZ => Some(self.parse_unary_builtin(
                token_pos,
                self.types.int_id,
                |arg| ExprKind::Ctz { arg },
            )),

            crate::kw::BUILTIN_CTZL => Some(self.parse_unary_builtin(
                token_pos,
                self.types.int_id,
                |arg| ExprKind::Ctzl { arg },
            )),

            crate::kw::BUILTIN_CTZLL => Some(self.parse_unary_builtin(
                token_pos,
                self.types.int_id,
                |arg| ExprKind::Ctzll { arg },
            )),

            // Checked arithmetic: compute exactly, store the wrapped
            // result, and answer whether wrapping lost anything.
            crate::kw::BUILTIN_ADD_OVERFLOW_P
            | crate::kw::BUILTIN_SUB_OVERFLOW_P
            | crate::kw::BUILTIN_MUL_OVERFLOW_P => Some((|| {
                // `__builtin_<op>_overflow_p(a, b, type_value)` asks the same
                // question as the storing form and answers it the same way,
                // but names the destination type with a *value* rather than a
                // pointer to one, and writes nothing. The argument is still
                // *evaluated*, as gcc evaluates it: only its value is unused.
                let op = match name_id {
                    crate::kw::BUILTIN_ADD_OVERFLOW_P => CheckedOp::Add,
                    crate::kw::BUILTIN_SUB_OVERFLOW_P => CheckedOp::Sub,
                    _ => CheckedOp::Mul,
                };
                self.expect_special(b'(')?;
                let a = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let b = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let res = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::CheckedArith {
                        op,
                        a: Box::new(a),
                        b: Box::new(b),
                        res: Box::new(res),
                        store: false,
                    },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_ADD_OVERFLOW
            | crate::kw::BUILTIN_SADD_OVERFLOW
            | crate::kw::BUILTIN_SADDL_OVERFLOW
            | crate::kw::BUILTIN_SADDLL_OVERFLOW
            | crate::kw::BUILTIN_UADD_OVERFLOW
            | crate::kw::BUILTIN_UADDL_OVERFLOW
            | crate::kw::BUILTIN_UADDLL_OVERFLOW => Some((|| {
                self.expect_special(b'(')?;
                let a = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let b = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let res = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::CheckedArith {
                        op: CheckedOp::Add,
                        a: Box::new(a),
                        b: Box::new(b),
                        res: Box::new(res),
                        store: true,
                    },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_SUB_OVERFLOW
            | crate::kw::BUILTIN_SSUB_OVERFLOW
            | crate::kw::BUILTIN_SSUBL_OVERFLOW
            | crate::kw::BUILTIN_SSUBLL_OVERFLOW
            | crate::kw::BUILTIN_USUB_OVERFLOW
            | crate::kw::BUILTIN_USUBL_OVERFLOW
            | crate::kw::BUILTIN_USUBLL_OVERFLOW => Some((|| {
                self.expect_special(b'(')?;
                let a = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let b = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let res = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::CheckedArith {
                        op: CheckedOp::Sub,
                        a: Box::new(a),
                        b: Box::new(b),
                        res: Box::new(res),
                        store: true,
                    },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_MUL_OVERFLOW
            | crate::kw::BUILTIN_SMUL_OVERFLOW
            | crate::kw::BUILTIN_SMULL_OVERFLOW
            | crate::kw::BUILTIN_SMULLL_OVERFLOW
            | crate::kw::BUILTIN_UMUL_OVERFLOW
            | crate::kw::BUILTIN_UMULL_OVERFLOW
            | crate::kw::BUILTIN_UMULLL_OVERFLOW => Some((|| {
                self.expect_special(b'(')?;
                let a = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let b = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let res = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::CheckedArith {
                        op: CheckedOp::Mul,
                        a: Box::new(a),
                        b: Box::new(b),
                        res: Box::new(res),
                        store: true,
                    },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CLZ => Some(self.parse_unary_builtin(
                token_pos,
                self.types.int_id,
                |arg| ExprKind::Clz { arg },
            )),

            crate::kw::BUILTIN_CLZL => Some(self.parse_unary_builtin(
                token_pos,
                self.types.int_id,
                |arg| ExprKind::Clzl { arg },
            )),

            crate::kw::BUILTIN_CLZLL => Some(self.parse_unary_builtin(
                token_pos,
                self.types.int_id,
                |arg| ExprKind::Clzll { arg },
            )),

            crate::kw::BUILTIN_CLRSB => Some(self.parse_unary_builtin(
                token_pos,
                self.types.int_id,
                |arg| ExprKind::Clrsb { arg },
            )),

            crate::kw::BUILTIN_CLRSBL => Some(self.parse_unary_builtin(
                token_pos,
                self.types.int_id,
                |arg| ExprKind::Clrsbl { arg },
            )),

            crate::kw::BUILTIN_CLRSBLL => Some(self.parse_unary_builtin(
                token_pos,
                self.types.int_id,
                |arg| ExprKind::Clrsbll { arg },
            )),

            crate::kw::BUILTIN_POPCOUNT => Some(self.parse_unary_builtin(
                token_pos,
                self.types.int_id,
                |arg| ExprKind::Popcount { arg },
            )),

            crate::kw::BUILTIN_POPCOUNTL => Some(self.parse_unary_builtin(
                token_pos,
                self.types.int_id,
                |arg| ExprKind::Popcountl { arg },
            )),

            crate::kw::BUILTIN_POPCOUNTLL => Some(self.parse_unary_builtin(
                token_pos,
                self.types.int_id,
                |arg| ExprKind::Popcountll { arg },
            )),

            crate::kw::BUILTIN_PARITY
            | crate::kw::BUILTIN_PARITYL
            | crate::kw::BUILTIN_PARITYLL => Some((|| {
                // __builtin_parity(x) - 1 if x has an odd number of set bits.
                //
                // That is the low bit of the population count, so it reuses
                // `Popcount` rather than introducing an opcode. Written as a
                // mask on the single `Popcount` node, the argument is
                // evaluated once -- `__builtin_parity(f())` calls `f` once,
                // which a `popcount(x) & 1` textual expansion would not
                // guarantee.
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                let kind = match name_id {
                    crate::kw::BUILTIN_PARITY => ExprKind::Popcount { arg: Box::new(arg) },
                    crate::kw::BUILTIN_PARITYL => ExprKind::Popcountl { arg: Box::new(arg) },
                    _ => ExprKind::Popcountll { arg: Box::new(arg) },
                };
                let count = Self::typed_expr(kind, self.types.int_id, token_pos);
                let one = Self::typed_expr(ExprKind::IntLit(1), self.types.int_id, token_pos);
                Ok(Self::typed_expr(
                    ExprKind::Binary {
                        op: BinaryOp::BitAnd,
                        left: Box::new(count),
                        right: Box::new(one),
                    },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CHOOSE_EXPR => Some((|| {
                // __builtin_choose_expr(c, a, b) - selects at parse time.
                //
                // Unlike `?:` the condition must be a constant expression and
                // only the selected arm is kept, so the other one need not
                // even type-check. That is the whole point of it: glibc uses
                // it to pick between expressions that are valid for different
                // argument types. The selection therefore happens here, next
                // to `_Generic`, rather than anywhere downstream.
                self.expect_special(b'(')?;
                let cond_pos = self.current_pos();
                let cond = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let then_expr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let else_expr = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                match self.eval_const_expr(&cond) {
                    Some(v) => Ok(if v != 0 { then_expr } else { else_expr }),
                    None => {
                        diag::error(
                            cond_pos,
                            &gettext(
                                "first argument to '__builtin_choose_expr' must be a constant expression",
                            ),
                        );
                        Ok(then_expr)
                    }
                }
            })()),
            _ => None,
        }
    }

    /// `alloca` and the `mem*` builtins, which lower to library calls.
    fn parse_memory_builtin(
        &mut self,
        name_id: StringId,
        token_pos: Position,
    ) -> Option<ParseResult<Expr>> {
        match name_id {
            crate::kw::BUILTIN_ALLOCA | crate::kw::ALLOCA => Some((|| {
                // __builtin_alloca(size) - returns void*
                self.expect_special(b'(')?;
                let size = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Alloca {
                        size: Box::new(size),
                    },
                    self.types.void_ptr_id,
                    token_pos,
                ))
            })()),
            // Memory builtins - generate calls to C library functions
            crate::kw::BUILTIN_MEMSET => Some((|| {
                // __builtin_memset(dest, c, n) - returns void*
                self.expect_special(b'(')?;
                let dest = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let c = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let n = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Memset {
                        dest: Box::new(dest),
                        c: Box::new(c),
                        n: Box::new(n),
                    },
                    self.types.void_ptr_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_MEMCPY => Some((|| {
                // __builtin_memcpy(dest, src, n) - returns void*
                self.expect_special(b'(')?;
                let dest = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let src = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let n = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Memcpy {
                        dest: Box::new(dest),
                        src: Box::new(src),
                        n: Box::new(n),
                    },
                    self.types.void_ptr_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_MEMMOVE => Some((|| {
                // __builtin_memmove(dest, src, n) - returns void*
                self.expect_special(b'(')?;
                let dest = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let src = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let n = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Memmove {
                        dest: Box::new(dest),
                        src: Box::new(src),
                        n: Box::new(n),
                    },
                    self.types.void_ptr_id,
                    token_pos,
                ))
            })()),
            // Infinity builtins - return float constants
            _ => None,
        }
    }

    /// Floating-point constants, classification and sign tests.
    fn parse_float_builtin(
        &mut self,
        name_id: StringId,
        token_pos: Position,
    ) -> Option<ParseResult<Expr>> {
        // A plain spelling is recognized only where it is being *called*.
        // `double (*p)(double) = fabs;` names the library function, and an
        // arm that ran here would report a missing `(` rather than letting
        // the identifier reach the ordinary path. The `__builtin_` spellings
        // need no such guard: they are not objects, so demanding the `(` is
        // the right diagnostic for them.
        let called = self.is_special(b'(');
        match name_id {
            crate::kw::BUILTIN_INF | crate::kw::BUILTIN_HUGE_VAL => Some((|| {
                self.expect_special(b'(')?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::FloatLit(FloatVal::infinity(false)),
                    self.types.double_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_INFF | crate::kw::BUILTIN_HUGE_VALF => Some((|| {
                self.expect_special(b'(')?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::FloatLit(FloatVal::infinity(false)),
                    self.types.float_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_INFL | crate::kw::BUILTIN_HUGE_VALL => Some((|| {
                self.expect_special(b'(')?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::FloatLit(FloatVal::infinity(false)),
                    self.types.longdouble_id,
                    token_pos,
                ))
            })()),
            // NaN builtins - returns quiet NaN
            // The string argument is typically empty "" for quiet NaN
            crate::kw::BUILTIN_NAN | crate::kw::BUILTIN_NANS => Some((|| {
                self.expect_special(b'(')?;
                let _arg = self.parse_assignment_expr()?; // string argument (ignored)
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::FloatLit(FloatVal::nan()),
                    self.types.double_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_NANF | crate::kw::BUILTIN_NANSF => Some((|| {
                self.expect_special(b'(')?;
                let _arg = self.parse_assignment_expr()?; // string argument (ignored)
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::FloatLit(FloatVal::nan()),
                    self.types.float_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_NANL | crate::kw::BUILTIN_NANSL => Some((|| {
                self.expect_special(b'(')?;
                let _arg = self.parse_assignment_expr()?; // string argument (ignored)
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::FloatLit(FloatVal::nan()),
                    self.types.longdouble_id,
                    token_pos,
                ))
            })()),
            // FLT_ROUNDS - returns current rounding mode (1 = to nearest)
            crate::kw::BUILTIN_FLT_ROUNDS => Some((|| {
                self.expect_special(b'(')?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::IntLit(1), // IEEE 754 default: round to nearest
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            // Fabs builtins - absolute value for floats
            crate::kw::BUILTIN_FABS => Some(self.parse_fabs(
                token_pos,
                self.types.double_id,
                |arg| ExprKind::Fabs { arg },
            )),
            crate::kw::FABS if called => Some(self.parse_fabs(
                token_pos,
                self.types.double_id,
                |arg| ExprKind::Fabs { arg },
            )),

            crate::kw::BUILTIN_FABSF => Some(self.parse_fabs(
                token_pos,
                self.types.float_id,
                |arg| ExprKind::Fabsf { arg },
            )),
            crate::kw::FABSF if called => Some(self.parse_fabs(
                token_pos,
                self.types.float_id,
                |arg| ExprKind::Fabsf { arg },
            )),

            crate::kw::BUILTIN_FABSL => Some(self.parse_fabsl(token_pos)),
            crate::kw::FABSL if called => Some(self.parse_fabsl(token_pos)),

            crate::kw::FLOOR
            | crate::kw::CEIL
            | crate::kw::TRUNC
            | crate::kw::ROUND
            | crate::kw::RINT
            | crate::kw::NEARBYINT
                if called =>
            {
                let (wide, narrow) = match name_id {
                    crate::kw::FLOOR => ("floor", "floorf"),
                    crate::kw::CEIL => ("ceil", "ceilf"),
                    crate::kw::TRUNC => ("trunc", "truncf"),
                    crate::kw::ROUND => ("round", "roundf"),
                    crate::kw::RINT => ("rint", "rintf"),
                    _ => ("nearbyint", "nearbyintf"),
                };
                Some((|| {
                    self.expect_special(b'(')?;
                    let arg = self.parse_assignment_expr()?;
                    self.expect_special(b')')?;
                    self.narrowing_libm_call(wide, narrow, arg, token_pos)
                })())
            }
            // Signbit builtins - test sign bit of floats
            crate::kw::BUILTIN_ISNAN
            | crate::kw::BUILTIN_ISNANF
            | crate::kw::BUILTIN_ISNANL
            | crate::kw::BUILTIN_ISINF
            | crate::kw::BUILTIN_ISINFF
            | crate::kw::BUILTIN_ISINFL
            | crate::kw::BUILTIN_ISINF_SIGN
            | crate::kw::BUILTIN_ISFINITE
            | crate::kw::BUILTIN_ISNORMAL => Some((|| {
                // The `f` and `l` spellings ask the same question of the same
                // argument: `FpTest` dispatches on the operand's own type, so
                // the suffix carries no information the node needs. gcc has
                // both suffixed spellings of `isnan` and `isinf`, and code
                // that includes <math.h> without c17's own headers reaches for
                // them. It has no suffixed `isfinite` or `isnormal`, so
                // neither does this -- claiming a builtin gcc does not have
                // would make `__has_builtin` a worse answer than none.
                let test = match name_id {
                    crate::kw::BUILTIN_ISNAN
                    | crate::kw::BUILTIN_ISNANF
                    | crate::kw::BUILTIN_ISNANL => FpTest::IsNan,
                    crate::kw::BUILTIN_ISINF
                    | crate::kw::BUILTIN_ISINFF
                    | crate::kw::BUILTIN_ISINFL => FpTest::IsInf,
                    crate::kw::BUILTIN_ISINF_SIGN => FpTest::IsInfSign,
                    crate::kw::BUILTIN_ISFINITE => FpTest::IsFinite,
                    _ => FpTest::IsNormal,
                };
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::FpTest {
                        test,
                        arg: Box::new(arg),
                    },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_ISGREATER
            | crate::kw::BUILTIN_ISGREATEREQUAL
            | crate::kw::BUILTIN_ISLESS
            | crate::kw::BUILTIN_ISLESSEQUAL
            | crate::kw::BUILTIN_ISLESSGREATER
            | crate::kw::BUILTIN_ISUNORDERED => {
                let cmp = match name_id {
                    crate::kw::BUILTIN_ISGREATER => FpCompare::Greater,
                    crate::kw::BUILTIN_ISGREATEREQUAL => FpCompare::GreaterEqual,
                    crate::kw::BUILTIN_ISLESS => FpCompare::Less,
                    crate::kw::BUILTIN_ISLESSEQUAL => FpCompare::LessEqual,
                    crate::kw::BUILTIN_ISLESSGREATER => FpCompare::LessGreater,
                    _ => FpCompare::Unordered,
                };
                Some(self.parse_fp_compare(token_pos, cmp))
            }
            crate::kw::BUILTIN_FPCLASSIFY => Some((|| {
                // __builtin_fpclassify(nan, inf, normal, subnormal, zero, x)
                self.expect_special(b'(')?;
                let mut args = Vec::with_capacity(6);
                args.push(self.parse_assignment_expr()?);
                while self.is_special(b',') {
                    self.advance();
                    args.push(self.parse_assignment_expr()?);
                }
                self.expect_special(b')')?;
                if args.len() != 6 {
                    return Err(ParseError::new(
                        "__builtin_fpclassify expects five class codes and a value",
                        token_pos,
                    ));
                }
                let arg = args.pop().expect("checked length");
                Ok(Self::typed_expr(
                    ExprKind::FpClassify {
                        classes: args,
                        arg: Box::new(arg),
                    },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_SIGNBIT => Some((|| {
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                let raw = Self::typed_expr(
                    ExprKind::Signbit { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                );
                Ok(self.normalise_predicate(raw, token_pos))
            })()),
            crate::kw::BUILTIN_SIGNBITF => Some((|| {
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                let raw = Self::typed_expr(
                    ExprKind::Signbitf { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                );
                Ok(self.normalise_predicate(raw, token_pos))
            })()),
            crate::kw::BUILTIN_SIGNBITL => Some((|| {
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                // Same reason as `__builtin_fabsl`: the `Signbit64` emitter
                // calls `__signbit`, which takes a `double`, so it tested bit 63
                // of an x87 mantissa -- the explicit integer bit, set for every
                // normal value -- and answered "negative" for positive numbers.
                let ld = self.types.longdouble_id;
                let raw = self.libm_call("__signbitl", self.types.int_id, &[ld], arg, token_pos);
                Ok(self.normalise_predicate(raw, token_pos))
            })()),
            crate::kw::BUILTIN_CREAL
            | crate::kw::BUILTIN_CREALF
            | crate::kw::BUILTIN_CREALL
            | crate::kw::BUILTIN_CIMAG
            | crate::kw::BUILTIN_CIMAGF
            | crate::kw::BUILTIN_CIMAGL => Some((|| {
                // `creal`/`cimag` name the halves `__real__` and `__imag__`
                // already reach, so they lower to those rather than to a
                // library call. The suffix is not consulted: the operand's own
                // type gives the precision, and a mismatch there would be the
                // caller's bug, not something the spelling can fix.
                let op = matches!(
                    name_id,
                    crate::kw::BUILTIN_CREAL
                        | crate::kw::BUILTIN_CREALF
                        | crate::kw::BUILTIN_CREALL
                )
                .then_some(UnaryOp::Real)
                .unwrap_or(UnaryOp::Imag);
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                let arg_typ = arg.typ.unwrap_or(self.types.double_id);
                let base = self.types.complex_base(arg_typ);
                Ok(Self::typed_expr(
                    ExprKind::Unary {
                        op,
                        operand: Box::new(arg),
                    },
                    base,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CONJ | crate::kw::BUILTIN_CONJF | crate::kw::BUILTIN_CONJL => {
                Some((|| {
                    // conj(z) is z with the sign of its imaginary part flipped.
                    // Built from `__builtin_complex(__real__ z, -__imag__ z)`
                    // rather than a libm call: every piece already exists, and
                    // negating the imaginary half is exact at every precision,
                    // where a call would need -lm for nothing.
                    self.expect_special(b'(')?;
                    let arg = self.parse_assignment_expr()?;
                    self.expect_special(b')')?;
                    let arg_typ = arg.typ.unwrap_or(self.types.double_id);
                    let base = self.types.complex_base(arg_typ);
                    let complex_typ = self.types.make_complex(base);
                    let real = Self::typed_expr(
                        ExprKind::Unary {
                            op: UnaryOp::Real,
                            operand: Box::new(arg.clone()),
                        },
                        base,
                        token_pos,
                    );
                    let imag = Self::typed_expr(
                        ExprKind::Unary {
                            op: UnaryOp::Imag,
                            operand: Box::new(arg),
                        },
                        base,
                        token_pos,
                    );
                    let neg_imag = Self::typed_expr(
                        ExprKind::Unary {
                            op: UnaryOp::Neg,
                            operand: Box::new(imag),
                        },
                        base,
                        token_pos,
                    );
                    Ok(Self::typed_expr(
                        ExprKind::BuiltinComplex {
                            real: Box::new(real),
                            imag: Box::new(neg_imag),
                        },
                        complex_typ,
                        token_pos,
                    ))
                })())
            }
            crate::kw::BUILTIN_COMPLEX => Some((|| {
                // __builtin_complex(real, imag) - construct complex value
                self.expect_special(b'(')?;
                let real = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let imag = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                // Determine complex type from argument types
                let real_typ = real.typ.unwrap_or(self.types.double_id);
                let complex_typ = self.types.make_complex(real_typ);
                Ok(Self::typed_expr(
                    ExprKind::BuiltinComplex {
                        real: Box::new(real),
                        imag: Box::new(imag),
                    },
                    complex_typ,
                    token_pos,
                ))
            })()),
            _ => None,
        }
    }

    /// Optimiser hints, stack introspection, `setjmp`/`longjmp` and `offsetof`.
    fn parse_misc_builtin(
        &mut self,
        name_id: StringId,
        token_pos: Position,
    ) -> Option<ParseResult<Expr>> {
        match name_id {
            crate::kw::BUILTIN_UNREACHABLE => Some((|| {
                // __builtin_unreachable() - marks code as unreachable
                // Takes no arguments, returns void
                // Behavior is undefined if actually reached at runtime
                self.expect_special(b'(')?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Unreachable,
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CONSTANT_P => Some((|| {
                // __builtin_constant_p(expr) - returns 1 if expr is a constant, 0 otherwise
                // This is evaluated at compile time, not runtime
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                // Constant-ness, not integer-ness: `__builtin_constant_p(3.14)`
                // is 1 in gcc. The integer folder deliberately refuses a
                // floating literal, since 6.6 makes one an integer constant
                // expression only as the operand of a cast, so the floating
                // fold has to be asked as well.
                let is_constant = self.eval_const_expr(&arg).is_some()
                    || self
                        .eval_const_f64(crate::constexpr::ConstScope::Standard, &arg)
                        .is_some();
                // Answering 1 here is final -- nothing later makes a constant
                // unconstant. Answering 0 is not: gcc decides this *after*
                // optimization, so `int x = 42; __builtin_constant_p(x)` is 1
                // at `-O1` and above, and only propagation knows. What the
                // parser cannot fold is deferred rather than refused.
                let kind = if is_constant {
                    ExprKind::IntLit(1)
                } else {
                    ExprKind::ConstantP(Box::new(arg))
                };
                Ok(Self::typed_expr(kind, self.types.int_id, token_pos))
            })()),
            crate::kw::BUILTIN_EXPECT => Some((|| {
                // `__builtin_expect(expr, c)` is a branch-prediction hint, and
                // c17 does not predict branches -- so its value is `expr`.
                //
                // The hint is still a function-like call, though, and its
                // second argument is evaluated: gcc runs the side effects of
                // `__builtin_expect(cond, z++)`. Dropping it on the floor,
                // which is what this used to do, silently skipped them.
                //
                // A literal -- which is what `likely`/`unlikely` expand to
                // almost everywhere -- has nothing to evaluate, so it is
                // dropped as before rather than left as a dead operand in
                // every hot path.
                self.expect_special(b'(')?;
                let expr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let expected = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                if Self::is_literal_constant(&expected) {
                    return Ok(expr);
                }
                let typ = expr.typ.unwrap_or(self.types.int_id);
                let pos = expr.pos;
                Ok(Self::typed_expr(
                    ExprKind::Comma(vec![expected, expr]),
                    typ,
                    pos,
                ))
            })()),
            crate::kw::BUILTIN_ASSUME_ALIGNED => Some((|| {
                // __builtin_assume_aligned(ptr, align) or
                // __builtin_assume_aligned(ptr, align, offset)
                // Returns ptr, hints that ptr is aligned to align bytes
                // We just return ptr since we don't do alignment optimization
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let _align = self.parse_assignment_expr()?;
                // Optional third argument (offset)
                if self.peek_special() == Some(b',' as u32) {
                    self.expect_special(b',')?;
                    let _offset = self.parse_assignment_expr()?;
                }
                self.expect_special(b')')?;
                Ok(ptr)
            })()),
            crate::kw::BUILTIN_EXTRACT_RETURN_ADDR => Some((|| {
                // Identity on both targets c17 has. The builtin exists for
                // architectures that encode a flag in the return address --
                // ARM Thumb sets bit 0 -- and there is nothing to strip on
                // x86-64 or AArch64, which is exactly what gcc does there.
                // Returning the argument keeps it evaluated exactly once.
                self.expect_special(b'(')?;
                let addr = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(addr)
            })()),
            crate::kw::BUILTIN_CLEAR_CACHE => Some((|| {
                // `__builtin___clear_cache(begin, end)` -- make instructions
                // written as data visible to the fetcher. A JIT is wrong
                // without it on AArch64, where the caches are not coherent;
                // on x86-64 they are, and gcc expands it to nothing.
                //
                // Lowered to libgcc's `__clear_cache`, which every target
                // provides and which is the no-op on x86-64. One spelling,
                // both targets, and correct on the one where it matters.
                self.expect_special(b'(')?;
                let begin = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let end = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                let void_id = self.types.void_id;
                let void_ptr = self.types.void_ptr_id;
                let sym = self
                    .declare_libm_function("__clear_cache", void_id, &[void_ptr, void_ptr])
                    .ok_or_else(|| ParseError::new("cannot declare __clear_cache", token_pos))?;
                Ok(Self::typed_expr(
                    ExprKind::Call {
                        func: Box::new(Self::typed_expr(ExprKind::Ident(sym), void_id, token_pos)),
                        args: vec![begin, end],
                    },
                    void_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_PREFETCH => Some((|| {
                // __builtin_prefetch(addr) or
                // __builtin_prefetch(addr, rw) or
                // __builtin_prefetch(addr, rw, locality)
                // Prefetch data at addr into cache - no-op for correctness
                self.expect_special(b'(')?;
                let addr = self.parse_assignment_expr()?;
                // Optional rw argument (0=read, 1=write)
                if self.peek_special() == Some(b',' as u32) {
                    self.expect_special(b',')?;
                    let _rw = self.parse_assignment_expr()?;
                    // Optional locality argument (0-3)
                    if self.peek_special() == Some(b',' as u32) {
                        self.expect_special(b',')?;
                        let _locality = self.parse_assignment_expr()?;
                    }
                }
                self.expect_special(b')')?;
                // The prefetch itself emits nothing, but its address argument
                // is still an expression and C evaluates it. Discarding it
                // here lost whatever it did: `__builtin_prefetch((q = p))`
                // left `q` unassigned, and `&p[j = i]` left `j` unassigned.
                // gcc documents the address as evaluated and tests for it.
                //
                // The `rw` and locality arguments need no such care -- gcc
                // requires them to be compile-time constants, so there is
                // nothing in them to evaluate.
                //
                // A comma expression carries the address along and yields the
                // void result, which is what the builtin's type says.
                let void_id = self.types.void_id;
                let void_result = Self::typed_expr(ExprKind::IntLit(0), void_id, token_pos);
                Ok(Self::typed_expr(
                    ExprKind::Comma(vec![addr, void_result]),
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CLASSIFY_TYPE => Some((|| {
                // __builtin_classify_type(expr) -- a compile-time code for the
                // argument's type family. Like `sizeof`, the argument is not
                // evaluated; unlike `sizeof`, gcc takes an expression rather
                // than a type name.
                //
                // The codes are gcc's, and were read off gcc rather than from
                // its source: the conversions happen first, so a `char`, an
                // enumeration and a `_Bool` all answer 1, and an array, a
                // function and a string literal all answer 5 because they
                // decay. Only the families below are reachable from C.
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                let typ = arg.typ.unwrap_or(self.types.int_id);
                let code = if self.types.is_complex(typ) {
                    9
                } else {
                    match self.types.kind(typ) {
                        TypeKind::Void => 0,
                        TypeKind::Struct => 12,
                        TypeKind::Union => 13,
                        TypeKind::Pointer | TypeKind::Array | TypeKind::Function => 5,
                        k if self.types.is_float(typ) => {
                            let _ = k;
                            8
                        }
                        // Every remaining arithmetic type is an integer one by
                        // the time the conversions are done with it.
                        _ => 1,
                    }
                };
                Ok(Self::typed_expr(
                    ExprKind::IntLit(code),
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_TYPES_COMPATIBLE_P => Some((|| {
                // __builtin_types_compatible_p(type1, type2) - returns 1 if types are compatible
                // This is evaluated at compile time, ignoring top-level qualifiers
                self.expect_special(b'(')?;
                let type1 = self.parse_type_name()?;
                self.expect_special(b',')?;
                let type2 = self.parse_type_name()?;
                self.expect_special(b')')?;
                // Check type compatibility (ignoring qualifiers)
                let compatible = self.types.types_compatible(type1, type2);
                Ok(Self::typed_expr(
                    ExprKind::IntLit(if compatible { 1 } else { 0 }),
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_FRAME_ADDRESS => Some((|| {
                // __builtin_frame_address(level) - returns void*, address of frame at level
                // Level 0 is the current frame, 1 is the caller's frame, etc.
                let level = self.parse_frame_level("__builtin_frame_address")?;
                Ok(Self::typed_expr(
                    ExprKind::FrameAddress { level },
                    self.types.void_ptr_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_RETURN_ADDRESS => Some((|| {
                // __builtin_return_address(level) - returns void*, return address at level
                // Level 0 is the current function's return address
                let level = self.parse_frame_level("__builtin_return_address")?;
                Ok(Self::typed_expr(
                    ExprKind::ReturnAddress { level },
                    self.types.void_ptr_id,
                    token_pos,
                ))
            })()),
            crate::kw::SETJMP | crate::kw::SETJMP2 => Some((|| {
                // setjmp(env) - saves execution context, returns int
                // Returns 0 on direct call, non-zero when returning via longjmp
                self.expect_special(b'(')?;
                // `setjmp()` with no argument is a call to an unprototyped
                // function, which old code writes and gcc accepts. Reaching
                // for the argument unconditionally turned it into "unexpected
                // token in expression", a parse error where an arity
                // diagnostic belonged -- and there is nothing to save, so the
                // builtin cannot handle it either: hand it back as an
                // ordinary call.
                if self.is_special(b')') {
                    self.advance();
                    return self.unprototyped_call(name_id, token_pos);
                }
                let env = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Setjmp { env: Box::new(env) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::LONGJMP | crate::kw::LONGJMP2 => Some((|| {
                // longjmp(env, val) - restores execution context (never returns)
                // Causes corresponding setjmp to return val (or 1 if val == 0)
                self.expect_special(b'(')?;
                let env = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let val = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Longjmp {
                        env: Box::new(env),
                        val: Box::new(val),
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_OFFSETOF | crate::kw::OFFSETOF => Some((|| {
                // __builtin_offsetof(type, member-designator)
                // Returns the byte offset of a member within a struct/union
                // member-designator can be .field or [index] chains
                self.expect_special(b'(')?;
                // Parse the type name
                let type_id = self.parse_type_name()?;
                self.expect_special(b',')?;
                // Parse member-designator starting with field name (no dot prefix for first field)
                // Subsequent components use .field or [index] syntax
                let mut path = Vec::new();
                // Expect identifier for first member
                let first_field = self.expect_identifier()?;
                path.push(OffsetOfPath::Field(first_field));
                // Parse subsequent designators
                loop {
                    if self.is_special(b'.') {
                        self.advance();
                        let field = self.expect_identifier()?;
                        path.push(OffsetOfPath::Field(field));
                    } else if self.is_special(b'[') {
                        self.advance();
                        // Parse constant expression for index
                        let index_expr = self.parse_conditional_expr()?;
                        let index_pos = index_expr.pos;
                        self.expect_special(b']')?;
                        // Evaluate as constant - offsetof requires compile-time constant
                        let index_val = self.eval_const_expr(&index_expr).ok_or_else(|| {
                            ParseError::new(
                                "array index in offsetof must be a constant expression",
                                index_pos,
                            )
                        })?;
                        path.push(OffsetOfPath::Index(index_val as i64));
                    } else {
                        break;
                    }
                }
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::OffsetOf { type_id, path },
                    self.types.ulong_id, // size_t is typically unsigned long
                    token_pos,
                ))
            })()),
            // Atomic builtins (Clang __c11_atomic_* for C11 stdatomic.h)
            _ => None,
        }
    }

    /// `__c11_atomic_fetch_<op>(ptr, val, order)`: apply the operation and
    /// return the old value.  The result type is the pointed-to type.
    fn parse_atomic_fetch_op(
        &mut self,
        token_pos: Position,
        kind: fn(Box<Expr>, Box<Expr>, Box<Expr>) -> ExprKind,
    ) -> ParseResult<Expr> {
        self.expect_special(b'(')?;
        let ptr = self.parse_assignment_expr()?;
        self.expect_special(b',')?;
        let val = self.parse_assignment_expr()?;
        self.expect_special(b',')?;
        let order = self.parse_assignment_expr()?;
        self.expect_special(b')')?;
        let ptr_type = ptr.typ.unwrap_or(self.types.void_ptr_id);
        let result_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
        Ok(Self::typed_expr(
            kind(Box::new(ptr), Box::new(val), Box::new(order)),
            result_type,
            token_pos,
        ))
    }

    /// Clang's `__c11_atomic_*` family, behind C11 <stdatomic.h>.
    fn parse_atomic_builtin(
        &mut self,
        name_id: StringId,
        token_pos: Position,
    ) -> Option<ParseResult<Expr>> {
        match name_id {
            crate::kw::C11_ATOMIC_INIT => Some((|| {
                // __c11_atomic_init(ptr, val) - initialize atomic (no ordering)
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let val = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicInit {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_LOAD => Some((|| {
                // __c11_atomic_load(ptr, order) - returns *ptr atomically
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                // Result type is the pointed-to type
                let ptr_type = ptr.typ.unwrap_or(self.types.void_ptr_id);
                let result_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicLoad {
                        ptr: Box::new(ptr),
                        order: Box::new(order),
                    },
                    result_type,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_STORE => Some((|| {
                // __c11_atomic_store(ptr, val, order) - *ptr = val atomically
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let val = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicStore {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                        order: Box::new(order),
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_EXCHANGE => Some((|| {
                // __c11_atomic_exchange(ptr, val, order) - swap and return old
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let val = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                // Result type is the pointed-to type
                let ptr_type = ptr.typ.unwrap_or(self.types.void_ptr_id);
                let result_type = self.types.base_type(ptr_type).unwrap_or(self.types.int_id);
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicExchange {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                        order: Box::new(order),
                    },
                    result_type,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_COMPARE_EXCHANGE_STRONG => Some((|| {
                // __c11_atomic_compare_exchange_strong(ptr, expected, desired, succ, fail)
                // Note: fail_order is parsed but ignored (we use succ_order for both)
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let expected = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let desired = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let succ_order = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let _fail_order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                // Returns bool (_Bool)
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicCompareExchangeStrong {
                        ptr: Box::new(ptr),
                        expected: Box::new(expected),
                        desired: Box::new(desired),
                        succ_order: Box::new(succ_order),
                    },
                    self.types.bool_id,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_COMPARE_EXCHANGE_WEAK => Some((|| {
                // __c11_atomic_compare_exchange_weak(ptr, expected, desired, succ, fail)
                // Note: Implemented as strong (no spurious failures)
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let expected = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let desired = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let succ_order = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let _fail_order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                // Returns bool (_Bool)
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicCompareExchangeWeak {
                        ptr: Box::new(ptr),
                        expected: Box::new(expected),
                        desired: Box::new(desired),
                        succ_order: Box::new(succ_order),
                    },
                    self.types.bool_id,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_FETCH_ADD => {
                Some(self.parse_atomic_fetch_op(token_pos, |ptr, val, order| {
                    ExprKind::C11AtomicFetchAdd { ptr, val, order }
                }))
            }
            crate::kw::C11_ATOMIC_FETCH_SUB => {
                Some(self.parse_atomic_fetch_op(token_pos, |ptr, val, order| {
                    ExprKind::C11AtomicFetchSub { ptr, val, order }
                }))
            }
            crate::kw::C11_ATOMIC_FETCH_AND => {
                Some(self.parse_atomic_fetch_op(token_pos, |ptr, val, order| {
                    ExprKind::C11AtomicFetchAnd { ptr, val, order }
                }))
            }
            crate::kw::C11_ATOMIC_FETCH_OR => {
                Some(self.parse_atomic_fetch_op(token_pos, |ptr, val, order| {
                    ExprKind::C11AtomicFetchOr { ptr, val, order }
                }))
            }
            crate::kw::C11_ATOMIC_FETCH_XOR => {
                Some(self.parse_atomic_fetch_op(token_pos, |ptr, val, order| {
                    ExprKind::C11AtomicFetchXor { ptr, val, order }
                }))
            }
            crate::kw::C11_ATOMIC_THREAD_FENCE => Some((|| {
                // __c11_atomic_thread_fence(order) - memory fence
                self.expect_special(b'(')?;
                let order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicThreadFence {
                        order: Box::new(order),
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_SIGNAL_FENCE => Some((|| {
                // __c11_atomic_signal_fence(order) - compiler barrier
                self.expect_special(b'(')?;
                let order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicSignalFence {
                        order: Box::new(order),
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::SYNC_FETCH_AND_ADD => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Add, false, false))
            }
            crate::kw::SYNC_ADD_AND_FETCH => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Add, true, false))
            }
            crate::kw::ATOMIC_FETCH_ADD => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Add, false, true))
            }
            crate::kw::ATOMIC_ADD_FETCH => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Add, true, true))
            }
            crate::kw::SYNC_FETCH_AND_SUB => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Sub, false, false))
            }
            crate::kw::SYNC_SUB_AND_FETCH => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Sub, true, false))
            }
            crate::kw::ATOMIC_FETCH_SUB => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Sub, false, true))
            }
            crate::kw::ATOMIC_SUB_FETCH => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Sub, true, true))
            }
            crate::kw::SYNC_FETCH_AND_AND => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::And, false, false))
            }
            crate::kw::SYNC_AND_AND_FETCH => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::And, true, false))
            }
            crate::kw::ATOMIC_FETCH_AND => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::And, false, true))
            }
            crate::kw::ATOMIC_AND_FETCH => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::And, true, true))
            }
            crate::kw::SYNC_FETCH_AND_OR => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Or, false, false))
            }
            crate::kw::SYNC_OR_AND_FETCH => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Or, true, false))
            }
            crate::kw::ATOMIC_FETCH_OR => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Or, false, true))
            }
            crate::kw::ATOMIC_OR_FETCH => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Or, true, true))
            }
            crate::kw::SYNC_FETCH_AND_XOR => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Xor, false, false))
            }
            crate::kw::SYNC_XOR_AND_FETCH => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Xor, true, false))
            }
            crate::kw::ATOMIC_FETCH_XOR => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Xor, false, true))
            }
            crate::kw::ATOMIC_XOR_FETCH => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Xor, true, true))
            }
            crate::kw::SYNC_FETCH_AND_NAND => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Nand, false, false))
            }
            crate::kw::SYNC_NAND_AND_FETCH => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Nand, true, false))
            }
            crate::kw::ATOMIC_FETCH_NAND => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Nand, false, true))
            }
            crate::kw::ATOMIC_NAND_FETCH => {
                Some(self.parse_gnu_atomic_rmw(token_pos, GnuAtomicOp::Nand, true, true))
            }
            crate::kw::SYNC_BOOL_COMPARE_AND_SWAP => {
                Some(self.parse_gnu_atomic_cas(token_pos, false))
            }
            crate::kw::SYNC_VAL_COMPARE_AND_SWAP => {
                Some(self.parse_gnu_atomic_cas(token_pos, true))
            }
            crate::kw::SYNC_LOCK_TEST_AND_SET => Some((|| {
                // An acquire exchange. `__sync_*` predates the C11 orders and
                // this one is documented as acquire rather than sequentially
                // consistent; c17's exchange is sequentially consistent, which
                // is stronger and therefore correct.
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let val = self.parse_assignment_expr()?;
                self.skip_trailing_sync_args()?;
                let result_type = self.pointee_or_int(&ptr);
                let order = self.seq_cst_literal(token_pos);
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicExchange {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                        order: Box::new(order),
                    },
                    result_type,
                    token_pos,
                ))
            })()),
            crate::kw::SYNC_LOCK_RELEASE => Some((|| {
                // A release store of zero.
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.skip_trailing_sync_args()?;
                let zero = Self::typed_expr(ExprKind::IntLit(0), self.types.int_id, token_pos);
                let order = self.seq_cst_literal(token_pos);
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicStore {
                        ptr: Box::new(ptr),
                        val: Box::new(zero),
                        order: Box::new(order),
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::SYNC_SYNCHRONIZE => Some((|| {
                self.expect_special(b'(')?;
                self.expect_special(b')')?;
                let order = self.seq_cst_literal(token_pos);
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicThreadFence {
                        order: Box::new(order),
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::ATOMIC_LOAD_N => Some((|| {
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                let result_type = self.pointee_or_int(&ptr);
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicLoad {
                        ptr: Box::new(ptr),
                        order: Box::new(order),
                    },
                    result_type,
                    token_pos,
                ))
            })()),
            crate::kw::ATOMIC_STORE_N => Some((|| {
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let val = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicStore {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                        order: Box::new(order),
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::ATOMIC_EXCHANGE_N => Some((|| {
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let val = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                let result_type = self.pointee_or_int(&ptr);
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicExchange {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                        order: Box::new(order),
                    },
                    result_type,
                    token_pos,
                ))
            })()),
            crate::kw::ATOMIC_COMPARE_EXCHANGE_N => Some((|| {
                // (ptr, expected, desired, weak, success_order, failure_order)
                // `expected` is a pointer here, exactly as in the C11 builtin,
                // so the node is the same one.
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let expected = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let desired = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let weak = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let succ_order = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let _fail_order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                // c17 implements both as strong, so the flag chooses only
                // which node is built; a weak exchange that never fails
                // spuriously is a conforming weak exchange.
                let is_weak = self.eval_const_expr(&weak).is_some_and(|v| v != 0);
                let (ptr, expected, desired, succ_order) = (
                    Box::new(ptr),
                    Box::new(expected),
                    Box::new(desired),
                    Box::new(succ_order),
                );
                let kind = if is_weak {
                    ExprKind::C11AtomicCompareExchangeWeak {
                        ptr,
                        expected,
                        desired,
                        succ_order,
                    }
                } else {
                    ExprKind::C11AtomicCompareExchangeStrong {
                        ptr,
                        expected,
                        desired,
                        succ_order,
                    }
                };
                Ok(Self::typed_expr(kind, self.types.int_id, token_pos))
            })()),
            crate::kw::ATOMIC_TEST_AND_SET => Some((|| {
                // Exchange 1 into the byte and report whether it was already
                // set. gcc documents the object as being set to "some
                // non-zero value"; 1 is the one every target uses.
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                let result_type = self.pointee_or_int(&ptr);
                let one = Self::typed_expr(ExprKind::IntLit(1), self.types.int_id, token_pos);
                let swapped = Self::typed_expr(
                    ExprKind::C11AtomicExchange {
                        ptr: Box::new(ptr),
                        val: Box::new(one),
                        order: Box::new(order),
                    },
                    result_type,
                    token_pos,
                );
                let zero = Self::typed_expr(ExprKind::IntLit(0), self.types.int_id, token_pos);
                Ok(Self::typed_expr(
                    ExprKind::Binary {
                        op: BinaryOp::Ne,
                        left: Box::new(swapped),
                        right: Box::new(zero),
                    },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::ATOMIC_CLEAR => Some((|| {
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                let zero = Self::typed_expr(ExprKind::IntLit(0), self.types.int_id, token_pos);
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicStore {
                        ptr: Box::new(ptr),
                        val: Box::new(zero),
                        order: Box::new(order),
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::ATOMIC_THREAD_FENCE => Some((|| {
                self.expect_special(b'(')?;
                let order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicThreadFence {
                        order: Box::new(order),
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::ATOMIC_SIGNAL_FENCE => Some((|| {
                self.expect_special(b'(')?;
                let order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicSignalFence {
                        order: Box::new(order),
                    },
                    self.types.void_id,
                    token_pos,
                ))
            })()),
            crate::kw::ATOMIC_ALWAYS_LOCK_FREE | crate::kw::ATOMIC_IS_LOCK_FREE => Some((|| {
                // (size, ptr). Answered at parse time from the size alone,
                // which is what both spellings reduce to here: c17's atomics
                // are lock-free exactly at the machine integer widths, and an
                // over-aligned pointer cannot make a 16-byte object lock-free
                // when the target has no 16-byte atomic.
                self.expect_special(b'(')?;
                let size = self.parse_assignment_expr()?;
                if self.is_special(b',') {
                    self.advance();
                    let _ptr = self.parse_assignment_expr()?;
                }
                self.expect_special(b')')?;
                let lock_free = self
                    .eval_const_expr(&size)
                    .is_some_and(|n| matches!(n, 1 | 2 | 4 | 8));
                Ok(Self::typed_expr(
                    ExprKind::IntLit(i64::from(lock_free)),
                    self.types.bool_id,
                    token_pos,
                ))
            })(
            )),
            _ => None,
        }
    }

    /// A call to `name` with no arguments, when a builtin cannot take it.
    ///
    /// The name is declared as the unprototyped `int name()` if nothing has
    /// declared it, which is what an implicit declaration gives and what
    /// `check_call_arity` then declines to check.
    fn unprototyped_call(&mut self, name_id: StringId, pos: Position) -> ParseResult<Expr> {
        let sym = match self.symbols.lookup_id(name_id, Namespace::Ordinary) {
            Some(sym) => sym,
            None => {
                let int_id = self.types.int_id;
                let func_type = self.types.intern(Type {
                    kind: TypeKind::Function,
                    base: Some(int_id),
                    params: None,
                    ..Default::default()
                });
                let symbol = Symbol::function(name_id, func_type, self.symbols.depth());
                self.symbols
                    .declare(symbol)
                    .map_err(|_| ParseError::new("cannot declare an implicit function", pos))?
            }
        };
        let sym_typ = self.symbols.get(sym).typ;
        let ret = self.types.base_type(sym_typ).unwrap_or(self.types.int_id);
        Ok(Self::typed_expr(
            ExprKind::Call {
                func: Box::new(Self::typed_expr(ExprKind::Ident(sym), ret, pos)),
                args: Vec::new(),
            },
            ret,
            pos,
        ))
    }

    /// A pointer to `typ`.
    fn pointer_to(&mut self, typ: TypeId) -> TypeId {
        self.types.intern(Type {
            kind: TypeKind::Pointer,
            base: Some(typ),
            ..Default::default()
        })
    }

    /// How many arguments a libm entry point takes.
    fn libm_arity(name: &str) -> usize {
        let stem = name
            .strip_suffix('f')
            .or_else(|| name.strip_suffix('l'))
            .unwrap_or(name);
        match stem {
            "fma" => 3,
            "copysign" | "fmax" | "fmin" | "pow" | "fmod" | "atan2" | "hypot" | "fdim"
            | "remainder" | "nextafter" | "modf" | "frexp" | "ldexp" => 2,
            _ => 1,
        }
    }

    /// The real floating type a libm alias computes in, from its suffix.
    ///
    /// One table rather than a suffix test at each site: a `float` entry point
    /// takes and returns `float`, and getting that wrong does not fail to
    /// link -- it sends the argument at the wrong width and answers with
    /// whatever was in the register.
    fn libm_real_kind(name: &str) -> Option<LibmReal> {
        const UNARY: &[&str] = &[
            "sqrt",
            "cbrt",
            "ceil",
            "floor",
            "trunc",
            "round",
            "rint",
            "nearbyint",
            "sin",
            "cos",
            "tan",
            "asin",
            "acos",
            "atan",
            "sinh",
            "cosh",
            "tanh",
            "asinh",
            "acosh",
            "atanh",
            "exp",
            "exp2",
            "expm1",
            "log",
            "log2",
            "log10",
            "log1p",
            "logb",
            "tgamma",
            "lgamma",
            "erf",
            "erfc",
        ];
        const BINARY: &[&str] = &[
            "copysign",
            "fmax",
            "fmin",
            "pow",
            "fmod",
            "atan2",
            "hypot",
            "fdim",
            "remainder",
            "nextafter",
            "modf",
            "frexp",
            "ldexp",
        ];
        const TERNARY: &[&str] = &["fma"];

        let (stem, kind) = match name.strip_suffix('f') {
            Some(stem) => (stem, LibmReal::Float),
            None => match name.strip_suffix('l') {
                Some(stem) => (stem, LibmReal::LongDouble),
                None => (name, LibmReal::Double),
            },
        };
        // `lgamma` ends in `a`, but `logb`/`log` do not collide: the strip
        // above only fires on a real suffix because the stem is then checked
        // against the table.
        let known = |n: &str| UNARY.contains(&n) || BINARY.contains(&n) || TERNARY.contains(&n);
        if known(stem) {
            Some(kind)
        } else if known(name) {
            Some(LibmReal::Double)
        } else {
            None
        }
    }

    /// The pointee type of `ptr`, or `int` when it is not a pointer. The
    /// diagnostic for the non-pointer case comes from the argument check.
    fn pointee_or_int(&self, ptr: &Expr) -> TypeId {
        let ptr_type = ptr.typ.unwrap_or(self.types.void_ptr_id);
        self.types.base_type(ptr_type).unwrap_or(self.types.int_id)
    }

    /// The `memory_order_seq_cst` constant, for the `__sync_*` builtins, which
    /// predate the C11 orders and are all sequentially consistent.
    fn seq_cst_literal(&self, pos: Position) -> Expr {
        Self::typed_expr(
            ExprKind::IntLit(crate::ir::MemoryOrder::SeqCst as i64),
            self.types.int_id,
            pos,
        )
    }

    /// Consume the optional trailing arguments a `__sync_*` builtin accepts.
    ///
    /// gcc documents every one of them as taking "an optional list of
    /// variables protected by the memory barrier", which it then ignores. A
    /// call that passes them must still parse.
    fn skip_trailing_sync_args(&mut self) -> ParseResult<()> {
        while self.is_special(b',') {
            self.advance();
            let _ = self.parse_assignment_expr()?;
        }
        self.expect_special(b')')?;
        Ok(())
    }

    /// `__sync_fetch_and_op` / `__sync_op_and_fetch` and their `__atomic_`
    /// counterparts.
    ///
    /// The `__atomic_` forms carry an explicit memory order; the `__sync_`
    /// ones are sequentially consistent and instead accept the trailing
    /// variable list gcc ignores.
    fn parse_gnu_atomic_rmw(
        &mut self,
        token_pos: Position,
        op: GnuAtomicOp,
        returns_new: bool,
        has_order: bool,
    ) -> ParseResult<Expr> {
        self.expect_special(b'(')?;
        let ptr = self.parse_assignment_expr()?;
        self.expect_special(b',')?;
        let val = self.parse_assignment_expr()?;
        let order = if has_order {
            self.expect_special(b',')?;
            let o = self.parse_assignment_expr()?;
            self.expect_special(b')')?;
            o
        } else {
            self.skip_trailing_sync_args()?;
            self.seq_cst_literal(token_pos)
        };
        let result_type = self.pointee_or_int(&ptr);
        Ok(Self::typed_expr(
            ExprKind::GnuAtomicRmw {
                op,
                ptr: Box::new(ptr),
                val: Box::new(val),
                order: Box::new(order),
                returns_new,
            },
            result_type,
            token_pos,
        ))
    }

    /// `__sync_bool_compare_and_swap` and `__sync_val_compare_and_swap`.
    fn parse_gnu_atomic_cas(
        &mut self,
        token_pos: Position,
        returns_old: bool,
    ) -> ParseResult<Expr> {
        self.expect_special(b'(')?;
        let ptr = self.parse_assignment_expr()?;
        self.expect_special(b',')?;
        let expected = self.parse_assignment_expr()?;
        self.expect_special(b',')?;
        let desired = self.parse_assignment_expr()?;
        self.skip_trailing_sync_args()?;
        let result_type = if returns_old {
            self.pointee_or_int(&ptr)
        } else {
            self.types.int_id
        };
        Ok(Self::typed_expr(
            ExprKind::GnuAtomicCas {
                ptr: Box::new(ptr),
                expected: Box::new(expected),
                desired: Box::new(desired),
                returns_old,
            },
            result_type,
            token_pos,
        ))
    }

    /// `__builtin_object_size`, the _FORTIFY_SOURCE size query.
    fn parse_object_size_builtin(
        &mut self,
        name_id: StringId,
        token_pos: Position,
    ) -> Option<ParseResult<Expr>> {
        match name_id {
            crate::kw::BUILTIN_OBJECT_SIZE => Some((|| {
                // __builtin_object_size(ptr, type): how many bytes remain in
                // the object `ptr` points into, when that is known statically.
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let otype = self.parse_assignment_expr()?;
                self.expect_special(b')')?;

                // The type argument must be an integer constant 0..3. Bit 0
                // selects the closest surrounding subobject over the whole
                // object; bit 1 asks for a minimum rather than a maximum.
                let otype = self.eval_const_expr(&otype).unwrap_or(0).clamp(0, 3) as u32;

                let size = match self.object_extent(&ptr) {
                    // A statically known object has the same minimum and
                    // maximum size, so bit 1 does not change the answer.
                    Some(extent) => extent.remaining(otype & 1 != 0),
                    // Unknown: the documented answers are the ones that make a
                    // `_FORTIFY_SOURCE` check pass rather than fire, which is
                    // the largest value for a maximum and zero for a minimum.
                    None if otype & 2 != 0 => 0,
                    None => u64::MAX,
                };

                Ok(Self::typed_expr(
                    ExprKind::IntLit(size as i64),
                    self.types.ulong_id,
                    token_pos,
                ))
            })()),
            _ => None,
        }
    }

    /// A builtin that is nothing but the library function under a
    /// reserved name, including the fortified `__builtin___*_chk`
    /// family.  These ride a de-prefixing path rather than getting an
    /// `ExprKind` each: an expression node per builtin means a case in
    /// the linearizer and in both backends, for something that is
    /// already an ordinary call.
    fn parse_library_builtin(
        &mut self,
        name_id: StringId,
        token_pos: Position,
    ) -> Option<ParseResult<Expr>> {
        let name_str = self.idents.get_opt(name_id).unwrap_or("");
        // A builtin that is nothing but the library function under a
        // reserved name. gcc knows these intrinsically, and code that
        // uses them is usually inside the very header that declares
        // the real one, so the call is spelled `__builtin_` to avoid
        // depending on a declaration it is in the middle of making.
        //
        // They ride the same de-prefixing path as the `_chk` family
        // below rather than getting an `ExprKind` each: an expression
        // node per builtin means a case in the linearizer and in both
        // backends, for something that is already an ordinary call.
        if name_str.starts_with("__builtin___") || Self::is_library_builtin(name_id) {
            Some((|| {
                // Fortified builtins: __builtin___snprintf_chk etc.
                // Strip __builtin_ prefix → __snprintf_chk, which is a
                // real libc function (declared by macOS/glibc headers).
                // `__builtin_trap` has no same-named library entry
                // point; its contract is abnormal termination, which
                // is `abort`. Everything else keeps its own name.
                let real_name = match name_str {
                    "__builtin_trap" => "abort",
                    // gcc's own equality-only spelling of `memcmp`. There is
                    // no library entry point of that name; answering the
                    // ordering as well is a correct implementation of it.
                    "__builtin_memcmp_eq" => "memcmp",
                    _ => &name_str["__builtin_".len()..],
                };
                // Parse arguments first (must consume tokens regardless)
                self.expect_special(b'(')?;
                let mut args = Vec::new();
                if !self.is_special(b')') {
                    args.push(self.parse_assignment_expr()?);
                    while self.is_special(b',') {
                        self.advance();
                        args.push(self.parse_assignment_expr()?);
                    }
                }
                self.expect_special(b')')?;
                // Look up the real function by its de-prefixed name
                let real_name_id = self.idents.lookup(real_name);
                let symbol_id = real_name_id.and_then(|id| {
                    self.symbols
                        .lookup_id(id, crate::symbol::Namespace::Ordinary)
                });
                if let Some(symbol_id) = symbol_id {
                    let func_type = self.symbols.get(symbol_id).typ;
                    let ret_type = self.types.base_type(func_type).unwrap_or(self.types.int_id);
                    let func_expr =
                        Self::typed_expr(ExprKind::Ident(symbol_id), func_type, token_pos);
                    return Ok(Self::typed_expr(
                        ExprKind::Call {
                            func: Box::new(func_expr),
                            args,
                        },
                        ret_type,
                        token_pos,
                    ));
                }
                // Not declared. gcc knows these intrinsically and
                // glibc relies on that: `bits/string_fortified.h`
                // calls `__builtin___memcpy_chk` without ever
                // declaring `__memcpy_chk`. Synthesize the
                // declaration rather than failing.
                if let Some(symbol_id) = self
                    .chk_builtin_return_type(real_name)
                    .and_then(|ret| self.declare_chk_builtin(real_name, ret))
                {
                    let ret_type = self
                        .types
                        .base_type(self.symbols.get(symbol_id).typ)
                        .unwrap_or(self.types.int_id);
                    let func_type = self.symbols.get(symbol_id).typ;
                    let func_expr =
                        Self::typed_expr(ExprKind::Ident(symbol_id), func_type, token_pos);
                    return Ok(Self::typed_expr(
                        ExprKind::Call {
                            func: Box::new(func_expr),
                            args,
                        },
                        ret_type,
                        token_pos,
                    ));
                }
                diag::error_args(token_pos, "undeclared function '{0}'", &[real_name]);
                Ok(Self::typed_expr(
                    ExprKind::IntLit(0),
                    self.types.int_id,
                    token_pos,
                ))
            })())
        } else {
            None
        }
    }

    pub(super) fn parse_builtin_expr(
        &mut self,
        name_id: StringId,
        token_pos: Position,
    ) -> Option<ParseResult<Expr>> {
        // Each family answers `None` for a name it does not own.
        if let Some(result) = self.parse_varargs_builtin(name_id, token_pos) {
            return Some(result);
        }
        if let Some(result) = self.parse_bit_builtin(name_id, token_pos) {
            return Some(result);
        }
        if let Some(result) = self.parse_memory_builtin(name_id, token_pos) {
            return Some(result);
        }
        if let Some(result) = self.parse_float_builtin(name_id, token_pos) {
            return Some(result);
        }
        if let Some(result) = self.parse_misc_builtin(name_id, token_pos) {
            return Some(result);
        }
        if let Some(result) = self.parse_atomic_builtin(name_id, token_pos) {
            return Some(result);
        }
        if let Some(result) = self.parse_object_size_builtin(name_id, token_pos) {
            return Some(result);
        }
        self.parse_library_builtin(name_id, token_pos)
    }

    /// What is statically known about the object a pointer expression
    /// designates, for `__builtin_object_size`.
    ///
    /// Tracks both the whole object and the innermost aggregate containing the
    /// designated byte, because the builtin's type argument selects between
    /// them: `__builtin_object_size(s.arr, 0)` is everything left in `s`,
    /// while type 1 is everything left in `arr`.
    fn object_extent(&self, expr: &Expr) -> Option<ObjectExtent> {
        match &expr.kind {
            // An array name decays to a pointer to its first element, so it
            // designates the array itself. A pointer *variable* designates
            // whatever it was assigned, which is exactly what is not known.
            ExprKind::Ident(id) => {
                let typ = self.symbols.get(*id).typ;
                if self.types.kind(typ) != TypeKind::Array {
                    return None;
                }
                ObjectExtent::whole_of(self.byte_size(typ)?)
            }

            // A string literal is an array of its bytes plus the terminator.
            ExprKind::StringLit(s) => ObjectExtent::whole_of(s.chars().count() as u64 + 1),

            // `&lvalue` designates the lvalue, which may be a subobject.
            ExprKind::Unary {
                op: UnaryOp::AddrOf,
                operand,
            } => self.lvalue_extent(operand),

            // A cast does not move the pointer.
            ExprKind::Cast { expr, .. } => self.object_extent(expr),

            // Pointer arithmetic with a constant displacement.
            ExprKind::Binary {
                op: op @ (BinaryOp::Add | BinaryOp::Sub),
                left,
                right,
            } => {
                let base = self.object_extent(left)?;
                let elem = self.pointee_size(left)?;
                let n = self.eval_const_expr(right)?;
                let bytes = i128::from(elem).checked_mul(n)?;
                base.advance(if *op == BinaryOp::Sub { -bytes } else { bytes })
            }

            // Everything else -- a call, a dereference, an unknown pointer.
            _ => self.object_extent_of_lvalue_forms(expr),
        }
    }

    /// The extent of an lvalue: the object it names, and where inside its
    /// enclosing object it sits.
    fn lvalue_extent(&self, expr: &Expr) -> Option<ObjectExtent> {
        match &expr.kind {
            ExprKind::Ident(id) => {
                ObjectExtent::whole_of(self.byte_size(self.symbols.get(*id).typ)?)
            }

            ExprKind::Member { expr: base, member } => {
                let base_typ = self.lvalue_type(base)?;
                let info = self.types.find_member(base_typ, *member)?;
                let outer = self.lvalue_extent(base)?;
                outer.narrow(info.offset as u64, self.byte_size(info.typ)?)
            }

            // `a[i]` with a constant index, where `a` is an array we can see.
            ExprKind::Index { array, index } => {
                let n = self.eval_const_expr(index)?;
                let outer = self.object_extent(array)?;
                let elem = self.pointee_size(array)?;
                outer.advance(i128::from(elem).checked_mul(n)?)
            }

            _ => None,
        }
    }

    /// `Member` and `Index` reached without an `&`, i.e. an array-typed
    /// subobject that decayed to a pointer.
    fn object_extent_of_lvalue_forms(&self, expr: &Expr) -> Option<ObjectExtent> {
        match &expr.kind {
            ExprKind::Member { .. } | ExprKind::Index { .. } => {
                let typ = self.lvalue_type(expr)?;
                if self.types.kind(typ) != TypeKind::Array {
                    return None;
                }
                self.lvalue_extent(expr)
            }
            _ => None,
        }
    }

    /// The declared type of an lvalue expression, as far as it can be resolved
    /// from symbols and member lookups alone.
    fn lvalue_type(&self, expr: &Expr) -> Option<TypeId> {
        match &expr.kind {
            ExprKind::Ident(id) => Some(self.symbols.get(*id).typ),
            ExprKind::Member { expr, member } => {
                let base = self.lvalue_type(expr)?;
                Some(self.types.find_member(base, *member)?.typ)
            }
            ExprKind::Index { array, .. } => {
                let base = self.lvalue_type(array)?;
                self.types.get(base).base
            }
            ExprKind::Cast { cast_type, .. } => Some(*cast_type),
            _ => expr.typ,
        }
    }

    /// Size in bytes of a type whose size is known and non-zero.
    fn byte_size(&self, typ: TypeId) -> Option<u64> {
        let bytes = self.types.size_bytes(typ);
        if bytes == 0 {
            return None;
        }
        Some(bytes as u64)
    }

    /// Size of what `expr` points at, for scaling pointer arithmetic.
    fn pointee_size(&self, expr: &Expr) -> Option<u64> {
        let typ = self.lvalue_type(expr)?;
        let elem = self.types.get(typ).base?;
        self.byte_size(elem)
    }

    /// The return type of a `_chk` fortified libc entry point, if `name` is
    /// one c17 knows.
    ///
    /// The type matters more than it looks: these mostly return a pointer, and
    /// declaring one of them `int` truncates the returned address to 32 bits.
    /// `None` means "not a known `_chk` function", which stays an error.
    pub(crate) fn chk_builtin_return_type(&mut self, name: &str) -> Option<TypeId> {
        // The string family returns `char *`; the memory family returns
        // `void *`; the printf family returns `int`.
        match name {
            "__memcpy_chk" | "__memmove_chk" | "__mempcpy_chk" | "__memset_chk" => {
                Some(self.types.void_ptr_id)
            }
            "__strcpy_chk" | "__stpcpy_chk" | "__strncpy_chk" | "__stpncpy_chk"
            | "__strcat_chk" | "__strncat_chk" => {
                let char_id = self.types.char_id;
                Some(self.types.intern(Type {
                    kind: TypeKind::Pointer,
                    base: Some(char_id),
                    ..Default::default()
                }))
            }
            "__sprintf_chk" | "__snprintf_chk" | "__printf_chk" | "__fprintf_chk"
            | "__vsprintf_chk" | "__vsnprintf_chk" | "__vprintf_chk" | "__vfprintf_chk" => {
                Some(self.types.int_id)
            }
            // The library builtins, for the case where the header that would
            // declare them has not been included.
            "strlen" => Some(self.types.ulong_id),
            "strcmp" | "abs" | "ffs" | "ffsl" | "ffsll" | "memcmp" | "strncmp" | "printf"
            | "sprintf" | "snprintf" | "puts" | "putchar" | "printf_unlocked"
            | "fprintf_unlocked" | "fputs_unlocked" => Some(self.types.int_id),
            "labs" => Some(self.types.long_id),
            "llabs" => Some(self.types.longlong_id),
            _ if Self::libm_real_kind(name).is_some() => Some(match Self::libm_real_kind(name) {
                Some(LibmReal::Float) => self.types.float_id,
                Some(LibmReal::LongDouble) => self.types.longdouble_id,
                _ => self.types.double_id,
            }),
            "bcmp" | "strcasecmp" | "strncasecmp" => Some(self.types.int_id),
            "abort" | "exit" | "free" => Some(self.types.void_id),
            // The allocators and `mempcpy` return `void *`; the string family
            // returns `char *`. Answering `int` here would truncate the
            // returned address to 32 bits, which is the bug the `_chk` cases
            // above are commented for.
            "strndup" | "strdup" => {
                let char_id = self.types.char_id;
                Some(self.types.intern(Type {
                    kind: TypeKind::Pointer,
                    base: Some(char_id),
                    ..Default::default()
                }))
            }
            "malloc" | "calloc" | "realloc" | "mempcpy" | "memchr" | "alloca" => {
                Some(self.types.void_ptr_id)
            }
            // `bcopy` predates `memmove` and returns nothing; `index`/`rindex`
            // are the old spellings of `strchr`/`strrchr`.
            "bcopy" | "bzero" => Some(self.types.void_id),
            "imaxabs" => Some(self.types.long_id),
            "strcspn" | "strspn" => Some(self.types.ulong_id),
            "strcpy" | "strncpy" | "stpcpy" | "stpncpy" | "strcat" | "strncat" | "strchr"
            | "strrchr" | "strstr" | "index" | "rindex" | "strpbrk" => {
                let char_id = self.types.char_id;
                Some(self.types.intern(Type {
                    kind: TypeKind::Pointer,
                    base: Some(char_id),
                    ..Default::default()
                }))
            }
            _ => None,
        }
    }

    /// Builtins that are the library function of the same name.
    ///
    /// Keyed by identifier rather than by spelling so this list and the
    /// keyword table cannot drift apart -- naming them twice is how
    /// `__has_builtin` came to disagree with the parser before.
    fn is_library_builtin(name_id: StringId) -> bool {
        matches!(
            name_id,
            crate::kw::BUILTIN_STRLEN
                | crate::kw::BUILTIN_STRCMP
                | crate::kw::BUILTIN_ABS
                | crate::kw::BUILTIN_LABS
                | crate::kw::BUILTIN_LLABS
                | crate::kw::BUILTIN_FFS
                | crate::kw::BUILTIN_FFSL
                | crate::kw::BUILTIN_FFSLL
                | crate::kw::BUILTIN_SQRT
                | crate::kw::BUILTIN_COPYSIGN
                | crate::kw::BUILTIN_COPYSIGNF
                | crate::kw::BUILTIN_COPYSIGNL
                | crate::kw::BUILTIN_SQRTF
                | crate::kw::BUILTIN_SQRTL
                | crate::kw::BUILTIN_FMAX
                | crate::kw::BUILTIN_FMAXF
                | crate::kw::BUILTIN_FMAXL
                | crate::kw::BUILTIN_FMIN
                | crate::kw::BUILTIN_FMINF
                | crate::kw::BUILTIN_FMINL
                | crate::kw::BUILTIN_POW
                | crate::kw::BUILTIN_POWF
                | crate::kw::BUILTIN_POWL
                | crate::kw::BUILTIN_FMA
                | crate::kw::BUILTIN_FMAF
                | crate::kw::BUILTIN_FMAL
                | crate::kw::BUILTIN_BCMP
                | crate::kw::BUILTIN_BZERO
                | crate::kw::BUILTIN_STPNCPY
                | crate::kw::BUILTIN_CBRT
                | crate::kw::BUILTIN_CBRTF
                | crate::kw::BUILTIN_CBRTL
                | crate::kw::BUILTIN_CEIL
                | crate::kw::BUILTIN_CEILF
                | crate::kw::BUILTIN_CEILL
                | crate::kw::BUILTIN_FLOOR
                | crate::kw::BUILTIN_FLOORF
                | crate::kw::BUILTIN_FLOORL
                | crate::kw::BUILTIN_TRUNC
                | crate::kw::BUILTIN_TRUNCF
                | crate::kw::BUILTIN_TRUNCL
                | crate::kw::BUILTIN_ROUND
                | crate::kw::BUILTIN_ROUNDF
                | crate::kw::BUILTIN_ROUNDL
                | crate::kw::BUILTIN_RINT
                | crate::kw::BUILTIN_RINTF
                | crate::kw::BUILTIN_RINTL
                | crate::kw::BUILTIN_NEARBYINT
                | crate::kw::BUILTIN_NEARBYINTF
                | crate::kw::BUILTIN_NEARBYINTL
                | crate::kw::BUILTIN_SIN
                | crate::kw::BUILTIN_SINF
                | crate::kw::BUILTIN_SINL
                | crate::kw::BUILTIN_COS
                | crate::kw::BUILTIN_COSF
                | crate::kw::BUILTIN_COSL
                | crate::kw::BUILTIN_TAN
                | crate::kw::BUILTIN_TANF
                | crate::kw::BUILTIN_TANL
                | crate::kw::BUILTIN_ASIN
                | crate::kw::BUILTIN_ASINF
                | crate::kw::BUILTIN_ASINL
                | crate::kw::BUILTIN_ACOS
                | crate::kw::BUILTIN_ACOSF
                | crate::kw::BUILTIN_ACOSL
                | crate::kw::BUILTIN_ATAN
                | crate::kw::BUILTIN_ATANF
                | crate::kw::BUILTIN_ATANL
                | crate::kw::BUILTIN_SINH
                | crate::kw::BUILTIN_SINHF
                | crate::kw::BUILTIN_SINHL
                | crate::kw::BUILTIN_COSH
                | crate::kw::BUILTIN_COSHF
                | crate::kw::BUILTIN_COSHL
                | crate::kw::BUILTIN_TANH
                | crate::kw::BUILTIN_TANHF
                | crate::kw::BUILTIN_TANHL
                | crate::kw::BUILTIN_ASINH
                | crate::kw::BUILTIN_ASINHF
                | crate::kw::BUILTIN_ASINHL
                | crate::kw::BUILTIN_ACOSH
                | crate::kw::BUILTIN_ACOSHF
                | crate::kw::BUILTIN_ACOSHL
                | crate::kw::BUILTIN_ATANH
                | crate::kw::BUILTIN_ATANHF
                | crate::kw::BUILTIN_ATANHL
                | crate::kw::BUILTIN_EXP
                | crate::kw::BUILTIN_EXPF
                | crate::kw::BUILTIN_EXPL
                | crate::kw::BUILTIN_EXP2
                | crate::kw::BUILTIN_EXP2F
                | crate::kw::BUILTIN_EXP2L
                | crate::kw::BUILTIN_EXPM1
                | crate::kw::BUILTIN_EXPM1F
                | crate::kw::BUILTIN_EXPM1L
                | crate::kw::BUILTIN_LOG
                | crate::kw::BUILTIN_LOGF
                | crate::kw::BUILTIN_LOGL
                | crate::kw::BUILTIN_LOG2
                | crate::kw::BUILTIN_LOG2F
                | crate::kw::BUILTIN_LOG2L
                | crate::kw::BUILTIN_LOG10
                | crate::kw::BUILTIN_LOG10F
                | crate::kw::BUILTIN_LOG10L
                | crate::kw::BUILTIN_LOG1P
                | crate::kw::BUILTIN_LOG1PF
                | crate::kw::BUILTIN_LOG1PL
                | crate::kw::BUILTIN_LOGB
                | crate::kw::BUILTIN_LOGBF
                | crate::kw::BUILTIN_LOGBL
                | crate::kw::BUILTIN_TGAMMA
                | crate::kw::BUILTIN_TGAMMAF
                | crate::kw::BUILTIN_TGAMMAL
                | crate::kw::BUILTIN_LGAMMA
                | crate::kw::BUILTIN_LGAMMAF
                | crate::kw::BUILTIN_LGAMMAL
                | crate::kw::BUILTIN_ERF
                | crate::kw::BUILTIN_ERFF
                | crate::kw::BUILTIN_ERFL
                | crate::kw::BUILTIN_ERFC
                | crate::kw::BUILTIN_ERFCF
                | crate::kw::BUILTIN_ERFCL
                | crate::kw::BUILTIN_FMOD
                | crate::kw::BUILTIN_FMODF
                | crate::kw::BUILTIN_FMODL
                | crate::kw::BUILTIN_ATAN2
                | crate::kw::BUILTIN_ATAN2F
                | crate::kw::BUILTIN_ATAN2L
                | crate::kw::BUILTIN_HYPOT
                | crate::kw::BUILTIN_HYPOTF
                | crate::kw::BUILTIN_HYPOTL
                | crate::kw::BUILTIN_FDIM
                | crate::kw::BUILTIN_FDIMF
                | crate::kw::BUILTIN_FDIML
                | crate::kw::BUILTIN_REMAINDER
                | crate::kw::BUILTIN_REMAINDERF
                | crate::kw::BUILTIN_REMAINDERL
                | crate::kw::BUILTIN_NEXTAFTER
                | crate::kw::BUILTIN_NEXTAFTERF
                | crate::kw::BUILTIN_NEXTAFTERL
                | crate::kw::BUILTIN_MODF
                | crate::kw::BUILTIN_MODFF
                | crate::kw::BUILTIN_MODFL
                | crate::kw::BUILTIN_FREXP
                | crate::kw::BUILTIN_FREXPF
                | crate::kw::BUILTIN_FREXPL
                | crate::kw::BUILTIN_LDEXP
                | crate::kw::BUILTIN_LDEXPF
                | crate::kw::BUILTIN_LDEXPL
                | crate::kw::BUILTIN_STRCASECMP
                | crate::kw::BUILTIN_STRNCASECMP
                | crate::kw::BUILTIN_STRDUP
                | crate::kw::BUILTIN_STRNDUP
                | crate::kw::BUILTIN_MEMCMP_EQ
                | crate::kw::BUILTIN_TRAP
                | crate::kw::BUILTIN_ABORT
                | crate::kw::BUILTIN_EXIT
                | crate::kw::BUILTIN_PRINTF
                | crate::kw::BUILTIN_SPRINTF
                | crate::kw::BUILTIN_SNPRINTF
                | crate::kw::BUILTIN_PUTS
                | crate::kw::BUILTIN_MALLOC
                | crate::kw::BUILTIN_CALLOC
                | crate::kw::BUILTIN_REALLOC
                | crate::kw::BUILTIN_FREE
                | crate::kw::BUILTIN_MEMCMP
                | crate::kw::BUILTIN_MEMPCPY
                | crate::kw::BUILTIN_STRCPY
                | crate::kw::BUILTIN_STRNCPY
                | crate::kw::BUILTIN_STPCPY
                | crate::kw::BUILTIN_STRCAT
                | crate::kw::BUILTIN_STRNCAT
                | crate::kw::BUILTIN_STRNCMP
                | crate::kw::BUILTIN_STRCHR
                | crate::kw::BUILTIN_STRRCHR
                | crate::kw::BUILTIN_STRSTR
                | crate::kw::BUILTIN_IMAXABS
                | crate::kw::BUILTIN_MEMCHR
                | crate::kw::BUILTIN_BCOPY
                | crate::kw::BUILTIN_INDEX
                | crate::kw::BUILTIN_RINDEX
                | crate::kw::BUILTIN_PUTCHAR
                | crate::kw::BUILTIN_STRCSPN
                | crate::kw::BUILTIN_STRSPN
                | crate::kw::BUILTIN_STRPBRK
                | crate::kw::BUILTIN_PRINTF_UNLOCKED
                | crate::kw::BUILTIN_FPRINTF_UNLOCKED
                | crate::kw::BUILTIN_FPUTS_UNLOCKED
        )
    }

    /// Reduce a predicate to 0 or 1.
    ///
    /// C17 7.12.3.6 lets `signbit` answer with *any* nonzero value, and the
    /// library entry points take it literally -- `__signbitf` returns 8,
    /// `__signbit` 128 and `__signbitl` 512. Comparing against zero costs one
    /// instruction and gives gcc's 0/1.
    fn normalise_predicate(&mut self, raw: Expr, pos: Position) -> Expr {
        let zero = Self::typed_expr(ExprKind::IntLit(0), self.types.int_id, pos);
        Self::typed_expr(
            ExprKind::Binary {
                op: BinaryOp::Ne,
                left: Box::new(raw),
                right: Box::new(zero),
            },
            self.types.int_id,
            pos,
        )
    }

    /// Lower a one-argument math builtin to an ordinary call to the library
    /// function that implements it, declaring that function if the translation
    /// unit has not.
    ///
    /// Unlike `declare_chk_builtin`, the parameter types are *modelled*: that
    /// one spells every parameter `unsigned long` because a pointer, a size and
    /// a flag all classify the same way, which is false the moment an argument
    /// is a `long double`.
    ///
    /// Falls back to the argument unchanged if the name cannot be interned,
    /// which would mean `kw.rs` and this list had drifted apart.
    fn libm_call(
        &mut self,
        name: &str,
        ret_type: TypeId,
        params: &[TypeId],
        arg: Expr,
        pos: Position,
    ) -> Expr {
        let Some(symbol_id) = self.declare_libm_function(name, ret_type, params) else {
            return arg;
        };
        let func_type = self.symbols.get(symbol_id).typ;
        let func_expr = Self::typed_expr(ExprKind::Ident(symbol_id), func_type, pos);
        Self::typed_expr(
            ExprKind::Call {
                func: Box::new(func_expr),
                args: vec![arg],
            },
            ret_type,
            pos,
        )
    }

    /// Declare `name` with a real prototype, reusing any existing declaration.
    fn declare_libm_function(
        &mut self,
        name: &str,
        ret_type: TypeId,
        params: &[TypeId],
    ) -> Option<SymbolId> {
        let name_id = self.idents.lookup(name)?;
        if let Some(existing) = self.symbols.lookup_id(name_id, Namespace::Ordinary) {
            return Some(existing);
        }
        let func_type = self.types.intern(Type {
            kind: TypeKind::Function,
            base: Some(ret_type),
            params: Some(params.to_vec()),
            variadic: false,
            ..Default::default()
        });
        let sym = Symbol::function(name_id, func_type, 0);
        self.symbols.declare(sym).ok()
    }

    /// Declare a `_chk` entry point, so the call type-checks and returns a
    /// value of the right width.
    ///
    /// The fixed parameter count is modelled even though the types are not:
    /// Apple's arm64 passes every variadic argument on the stack while fixed
    /// ones stay in registers, so declaring these as variadic from argument
    /// zero -- which is what an empty parameter list means -- misplaces
    /// `__snprintf_chk`'s buffer, length, flag and size.
    fn declare_chk_builtin(&mut self, name: &str, ret_type: TypeId) -> Option<SymbolId> {
        // Pre-interned in `cc/kw.rs`; the identifier table is read-only here.
        let name_id = self.idents.lookup(name)?;

        // Each entry gives (fixed parameters before the `...`, whether a
        // `...` follows). The `v` forms take a `va_list` and are not variadic;
        // the memory ones take a fixed argument list outright.
        let (fixed, variadic) = match name {
            "__printf_chk" => (2, true),
            "__fprintf_chk" | "__sprintf_chk" => (3, true),
            "__snprintf_chk" => (5, true),
            "__vprintf_chk" => (3, false),
            "__vfprintf_chk" | "__vsprintf_chk" => (4, false),
            "__vsnprintf_chk" => (6, false),
            "__memset_chk" | "__strcpy_chk" | "__stpcpy_chk" | "__strcat_chk" => (3, false),
            "__memcpy_chk" | "__memmove_chk" | "__mempcpy_chk" | "__strncpy_chk"
            | "__stpncpy_chk" | "__strncat_chk" => (4, false),
            "strlen" | "abs" | "labs" | "llabs" | "ffs" | "ffsl" | "ffsll" => (1, false),
            "strcmp" | "bzero" | "strcasecmp" => (2, false),
            "bcmp" | "stpncpy" | "strncasecmp" => (3, false),
            // The libm entry points, from the one table that knows them.
            _ if Self::libm_real_kind(name).is_some() => (Self::libm_arity(name), false),
            "abort" => (0, false),
            "exit" | "puts" | "malloc" | "free" | "putchar" | "imaxabs" | "strdup" => (1, false),
            "strndup" => (2, false),
            "calloc" | "realloc" | "strcpy" | "stpcpy" | "strcat" | "strchr" | "strrchr"
            | "strstr" | "index" | "rindex" | "strpbrk" | "strcspn" | "strspn" => (2, false),
            "memcmp" | "mempcpy" | "strncpy" | "strncat" | "strncmp" | "memchr" | "bcopy" => {
                (3, false)
            }
            // The printf family is variadic after its format string. Getting
            // the fixed count right is what keeps the format argument in a
            // register on Apple arm64, where variadic arguments go on the
            // stack -- the same reason the `_chk` forms above are spelled out.
            "printf" | "printf_unlocked" => (1, true),
            // The stdio `_unlocked` forms. gcc has them, and the torture
            // suite's builtins/ tests supply the library side themselves --
            // glibc has no `printf_unlocked`, so gcc's own link fails without
            // that. Only the three a real corpus uses are here.
            "fprintf_unlocked" => (2, true),
            "fputs_unlocked" => (2, false),
            "sprintf" => (2, true),
            "snprintf" => (3, true),
            // An entry point this does not know is left as it was: variadic,
            // with nothing fixed.
            _ => (0, true),
        };
        // The types are not modelled -- only how many arguments are fixed --
        // so each is spelled as the widest integer the ABI passes in one
        // register, which a pointer, a size and a flag all classify as.
        //
        // A `double` does not. It is passed in an SSE register, so declaring
        // one of these as an integer sent the argument to the wrong register
        // file outright: `__builtin_sqrt(4.0)` read whatever was in xmm0 and
        // came back 0.0, and `__builtin_copysign(1.0, -1.0)` answered 1.0
        // because the sign argument never arrived. Both are silent wrong
        // answers -- the call links and runs. The library functions of the
        // same names are unaffected; this path is only taken when the header
        // that would declare them was not included.
        let param_typ = match Self::libm_real_kind(name) {
            Some(LibmReal::Float) => self.types.float_id,
            Some(LibmReal::Double) => self.types.double_id,
            Some(LibmReal::LongDouble) => self.types.longdouble_id,
            None => self.types.ulong_id,
        };
        let params = match name {
            // `modf`, `frexp` and `ldexp` do not take a list of one type: the
            // second parameter is a pointer or an `int`. Declaring one of
            // them uniformly sent that argument to the wrong register file,
            // which is the silent wrong answer the note above describes.
            "modf" | "modff" | "modfl" => vec![param_typ, self.pointer_to(param_typ)],
            "frexp" | "frexpf" | "frexpl" => {
                vec![param_typ, self.pointer_to(self.types.int_id)]
            }
            "ldexp" | "ldexpf" | "ldexpl" => vec![param_typ, self.types.int_id],
            _ => vec![param_typ; fixed],
        };

        let func_type = self.types.intern(Type {
            kind: TypeKind::Function,
            base: Some(ret_type),
            variadic,
            params: Some(params),
            ..Default::default()
        });
        let symbol = Symbol::function(name_id, func_type, self.symbols.depth());
        // A redeclaration can only mean the header did declare it after all,
        // in which case the existing symbol is the one to use.
        Some(self.symbols.declare(symbol).unwrap_or_else(|_| {
            self.symbols
                .lookup_id(name_id, Namespace::Ordinary)
                .expect("declare failed but no existing symbol")
        }))
    }
}
