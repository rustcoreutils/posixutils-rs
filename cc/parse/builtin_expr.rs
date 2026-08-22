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

use super::ast::{BinaryOp, CheckedOp, Expr, ExprKind, FpTest, OffsetOfPath, UnaryOp};
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

impl Parser<'_> {
    /// Whether a declaration in scope displaces the builtin meaning the parser
    /// would otherwise give `name_id`.
    ///
    /// `offsetof`, `alignof`, `setjmp` and `longjmp` are not C17 keywords --
    /// the first is a macro, the second a C23 spelling, the last two ordinary
    /// library functions -- so a program may use any of them as an identifier,
    /// and gcc accepts that. Recognising them ahead of ordinary lookup made
    /// them impossible to *use*: `int offsetof; offsetof = 1;` declared fine
    /// and then reported "expected '('".
    ///
    /// `setjmp` and `longjmp` are the exception, and only against a *function*
    /// declaration: `<setjmp.h>` declares exactly those, and they need code
    /// generation an ordinary call cannot produce. A declaration of any other
    /// kind -- a variable, a parameter, a typedef -- is unambiguously not that
    /// function.
    ///
    /// The reserved spellings (`__builtin_*`, `_Alignof`, `__alignof__`) are
    /// never displaced: C17 7.1.3 reserves them to the implementation in every
    /// scope, so a program that declares one has no claim on the name.
    pub(super) fn builtin_is_shadowed(&self, name_id: StringId) -> bool {
        let shadowed_by_any_decl = matches!(name_id, crate::kw::OFFSETOF | crate::kw::ALIGNOF_C23);
        let shadowable = shadowed_by_any_decl
            || matches!(
                name_id,
                crate::kw::SETJMP | crate::kw::SETJMP2 | crate::kw::LONGJMP | crate::kw::LONGJMP2
            );
        if !shadowable {
            return false;
        }

        let Some(symbol_id) = self.symbols.lookup_id(name_id, Namespace::Ordinary) else {
            return false;
        };
        shadowed_by_any_decl
            || self.types.kind(self.symbols.get(symbol_id).typ) != TypeKind::Function
    }

    /// Try to parse a builtin function expression.
    /// Returns `Some(result)` if `name_id` is a recognized builtin, `None` otherwise.
    pub(super) fn parse_builtin_expr(
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
            crate::kw::BUILTIN_BSWAP16 => Some((|| {
                // __builtin_bswap16(x) - returns uint16_t
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Bswap16 { arg: Box::new(arg) },
                    self.types.ushort_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_BSWAP32 => Some((|| {
                // __builtin_bswap32(x) - returns uint32_t
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Bswap32 { arg: Box::new(arg) },
                    self.types.uint_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_BSWAP64 => Some((|| {
                // __builtin_bswap64(x) - returns uint64_t
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Bswap64 { arg: Box::new(arg) },
                    self.types.ulonglong_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CTZ => Some((|| {
                // __builtin_ctz(x) - returns int, counts trailing zeros in unsigned int
                // Result is undefined if x is 0
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Ctz { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CTZL => Some((|| {
                // __builtin_ctzl(x) - returns int, counts trailing zeros in unsigned long
                // Result is undefined if x is 0
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Ctzl { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CTZLL => Some((|| {
                // __builtin_ctzll(x) - returns int, counts trailing zeros in unsigned long long
                // Result is undefined if x is 0
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Ctzll { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            // Checked arithmetic: compute exactly, store the wrapped
            // result, and answer whether wrapping lost anything.
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
                    },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CLZ => Some((|| {
                // __builtin_clz(x) - returns int, counts leading zeros in unsigned int
                // Result is undefined if x is 0
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Clz { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CLZL => Some((|| {
                // __builtin_clzl(x) - returns int, counts leading zeros in unsigned long
                // Result is undefined if x is 0
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Clzl { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CLZLL => Some((|| {
                // __builtin_clzll(x) - returns int, counts leading zeros in unsigned long long
                // Result is undefined if x is 0
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Clzll { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CLRSB => Some((|| {
                // __builtin_clrsb(x) - redundant sign bits in a signed int.
                // Defined for every input, so unlike `clz` there is no
                // undefined case to document.
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Clrsb { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CLRSBL => Some((|| {
                // __builtin_clrsbl(x) - redundant sign bits in a signed long.
                // Defined for every input, so unlike `clz` there is no
                // undefined case to document.
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Clrsbl { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_CLRSBLL => Some((|| {
                // __builtin_clrsbll(x) - redundant sign bits in a signed long long.
                // Defined for every input, so unlike `clz` there is no
                // undefined case to document.
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Clrsbll { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_POPCOUNT => Some((|| {
                // __builtin_popcount(x) - returns int, counts set bits in unsigned int
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Popcount { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_POPCOUNTL => Some((|| {
                // __builtin_popcountl(x) - returns int, counts set bits in unsigned long
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Popcountl { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_POPCOUNTLL => Some((|| {
                // __builtin_popcountll(x) - returns int, counts set bits in unsigned long long
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Popcountll { arg: Box::new(arg) },
                    self.types.int_id,
                    token_pos,
                ))
            })()),
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
            crate::kw::BUILTIN_ALLOCA => Some((|| {
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
            crate::kw::BUILTIN_FABS => Some((|| {
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Fabs { arg: Box::new(arg) },
                    self.types.double_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_FABSF => Some((|| {
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::Fabsf { arg: Box::new(arg) },
                    self.types.float_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_FABSL => Some((|| {
                self.expect_special(b'(')?;
                let arg = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                // Lowered as an ordinary call to `fabsl`, not as an opcode.
                // The opcode it used to build was `Fabs64`, whose emitter moves
                // its argument as a `double` and calls `fabs` -- so on x86-64 it
                // read the low eight bytes of an 80-bit x87 value, its mantissa,
                // and `__builtin_fabsl(-3.5L)` returned 2.5e-4932. A real call
                // gets the long-double ABI from the call path, which already
                // carries one for `__mulxc3`.
                let ld = self.types.longdouble_id;
                Ok(self.libm_call("fabsl", ld, &[ld], arg, token_pos))
            })()),
            // Signbit builtins - test sign bit of floats
            crate::kw::BUILTIN_ISNAN
            | crate::kw::BUILTIN_ISINF
            | crate::kw::BUILTIN_ISINF_SIGN
            | crate::kw::BUILTIN_ISFINITE
            | crate::kw::BUILTIN_ISNORMAL => Some((|| {
                let test = match name_id {
                    crate::kw::BUILTIN_ISNAN => FpTest::IsNan,
                    crate::kw::BUILTIN_ISINF => FpTest::IsInf,
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
                Ok(Self::typed_expr(
                    ExprKind::IntLit(if is_constant { 1 } else { 0 }),
                    self.types.int_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_EXPECT => Some((|| {
                // __builtin_expect(expr, c) - branch prediction hint
                // Returns expr, the second argument is the expected value (for optimization hints)
                // We just return expr since we don't do branch prediction optimization
                self.expect_special(b'(')?;
                let expr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let _expected = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(expr)
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
            crate::kw::BUILTIN_PREFETCH => Some((|| {
                // __builtin_prefetch(addr) or
                // __builtin_prefetch(addr, rw) or
                // __builtin_prefetch(addr, rw, locality)
                // Prefetch data at addr into cache - no-op for correctness
                self.expect_special(b'(')?;
                let _addr = self.parse_assignment_expr()?;
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
                // Returns void - just return a void expression
                Ok(Self::typed_expr(
                    ExprKind::IntLit(0),
                    self.types.void_id,
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
                // Returns NULL for invalid levels (beyond stack bounds)
                self.expect_special(b'(')?;
                let level = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::FrameAddress {
                        level: Box::new(level),
                    },
                    self.types.void_ptr_id,
                    token_pos,
                ))
            })()),
            crate::kw::BUILTIN_RETURN_ADDRESS => Some((|| {
                // __builtin_return_address(level) - returns void*, return address at level
                // Level 0 is the current function's return address
                // Returns NULL for invalid levels (beyond stack bounds)
                self.expect_special(b'(')?;
                let level = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                Ok(Self::typed_expr(
                    ExprKind::ReturnAddress {
                        level: Box::new(level),
                    },
                    self.types.void_ptr_id,
                    token_pos,
                ))
            })()),
            crate::kw::SETJMP | crate::kw::SETJMP2 => Some((|| {
                // setjmp(env) - saves execution context, returns int
                // Returns 0 on direct call, non-zero when returning via longjmp
                self.expect_special(b'(')?;
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
            // ================================================================
            // Atomic builtins (Clang __c11_atomic_* for C11 stdatomic.h)
            // ================================================================
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
            crate::kw::C11_ATOMIC_FETCH_ADD => Some((|| {
                // __c11_atomic_fetch_add(ptr, val, order) - add and return old
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
                    ExprKind::C11AtomicFetchAdd {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                        order: Box::new(order),
                    },
                    result_type,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_FETCH_SUB => Some((|| {
                // __c11_atomic_fetch_sub(ptr, val, order) - subtract and return old
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
                    ExprKind::C11AtomicFetchSub {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                        order: Box::new(order),
                    },
                    result_type,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_FETCH_AND => Some((|| {
                // __c11_atomic_fetch_and(ptr, val, order) - AND and return old
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
                    ExprKind::C11AtomicFetchAnd {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                        order: Box::new(order),
                    },
                    result_type,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_FETCH_OR => Some((|| {
                // __c11_atomic_fetch_or(ptr, val, order) - OR and return old
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
                    ExprKind::C11AtomicFetchOr {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                        order: Box::new(order),
                    },
                    result_type,
                    token_pos,
                ))
            })()),
            crate::kw::C11_ATOMIC_FETCH_XOR => Some((|| {
                // __c11_atomic_fetch_xor(ptr, val, order) - XOR and return old
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
                    ExprKind::C11AtomicFetchXor {
                        ptr: Box::new(ptr),
                        val: Box::new(val),
                        order: Box::new(order),
                    },
                    result_type,
                    token_pos,
                ))
            })()),
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
            _ => {
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
                            let ret_type =
                                self.types.base_type(func_type).unwrap_or(self.types.int_id);
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
        }
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
        let bits = self.types.size_bits(typ);
        if bits == 0 {
            return None;
        }
        Some(u64::from(bits) / 8)
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
    fn chk_builtin_return_type(&mut self, name: &str) -> Option<TypeId> {
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
            "strcmp" | "abs" | "ffs" | "ffsl" | "ffsll" => Some(self.types.int_id),
            "labs" => Some(self.types.long_id),
            "llabs" => Some(self.types.longlong_id),
            "sqrt" | "copysign" => Some(self.types.double_id),
            "abort" => Some(self.types.void_id),
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
                | crate::kw::BUILTIN_TRAP
        )
    }

    /// Declare a `_chk` entry point, so the call type-checks and returns a
    /// value of the right width.
    ///
    /// The fixed parameters are counted even though their types are not
    /// modelled: where an argument stops being fixed and starts being variadic
    /// is an ABI question, not only a type-checking one. Apple's arm64 passes
    /// every variadic argument on the stack while fixed ones stay in
    /// registers, so declaring these as variadic-from-argument-zero -- which
    /// is what an empty parameter list means -- put `__snprintf_chk`'s buffer,
    /// length, flag and size on the stack, where it does not look for them.
    /// Reduce a predicate to 0 or 1.
    ///
    /// C17 7.12.3.6 lets `signbit` answer with *any* nonzero value, and the
    /// library entry points take it literally -- `__signbitf` returns 8,
    /// `__signbit` 128 and `__signbitl` 512, each being the sign bit still in
    /// place. All conforming, none matching gcc's 0/1, and the three did not
    /// even agree with one another. Comparing against zero costs one
    /// instruction and removes a gratuitous difference.
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

    fn declare_chk_builtin(&mut self, name: &str, ret_type: TypeId) -> Option<SymbolId> {
        // Pre-interned in `cc/kw.rs`; the identifier table is read-only here.
        let name_id = self.idents.lookup(name)?;

        // (fixed parameters before the `...`, whether a `...` follows). The
        // `v` forms take a `va_list` and are not variadic; the memory ones
        // take a fixed argument list outright.
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
            "strlen" | "abs" | "labs" | "llabs" | "ffs" | "ffsl" | "ffsll" | "sqrt" => (1, false),
            "strcmp" | "copysign" => (2, false),
            "abort" => (0, false),
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
        let param_typ = match name {
            "sqrt" | "copysign" => self.types.double_id,
            _ => self.types.ulong_id,
        };
        let params = vec![param_typ; fixed];

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
