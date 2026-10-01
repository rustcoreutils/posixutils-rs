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
    BinaryOp, CalleeBinding, Expr, ExprKind, FpCompare, GnuAtomicOp, LibFn, OffsetOfPath, UnaryOp,
};
use super::builtin_args::ConstantArgument;
use super::library_builtin::LibraryBuiltin;
use super::parser::{ParseError, ParseResult, Parser};
use crate::diag;
use crate::float::{FloatVal, NanKind};
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
    pub(super) fn remaining(self, closest_subobject: bool) -> u64 {
        if closest_subobject {
            self.sub.saturating_sub(self.sub_offset)
        } else {
            self.whole.saturating_sub(self.offset)
        }
    }
}

impl Parser<'_> {
    /// Whether an expression is a literal constant, with nothing to evaluate.
    ///
    /// Used to decide whether a discarded operand can be dropped outright or
    /// has to be kept for its side effects. Deliberately conservative: it
    /// answers `true` only for the shapes that plainly compute nothing, so a
    /// wrong answer keeps a harmless dead operand rather than losing a side
    /// effect.
    pub(super) fn is_literal_constant(expr: &Expr) -> bool {
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
    /// The library builtins (`abs`, `fabs`, `floor`, ...) survive a function
    /// declaration only if it is compatible with the prototype their table
    /// gives (see `library_prototype_matches`), and the functions gcc expands
    /// late (`sqrt`, `floor`, `memcpy`, ...) not a definition of the function
    /// at all.
    ///
    /// The reserved spellings (`__builtin_*`, `_Alignof`, `__alignof__`) are
    /// never displaced: C17 7.1.3 reserves them to the implementation in every
    /// scope, so a program that declares one has no claim on the name.
    pub(super) fn builtin_is_shadowed(&self, name_id: StringId) -> bool {
        let shadowed_by_any_decl = matches!(name_id, crate::kw::OFFSETOF | crate::kw::ALIGNOF_C23);
        let library = LibraryBuiltin::by_bare_name(name_id);
        let shadowable = shadowed_by_any_decl
            || library.is_some()
            || matches!(
                name_id,
                crate::kw::SETJMP
                    | crate::kw::SETJMP2
                    | crate::kw::LONGJMP
                    | crate::kw::LONGJMP2
                    | crate::kw::ALLOCA
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
        // A definition in this translation unit is the function called; one
        // further down is found by the linearizer instead.
        if library.is_some_and(|lb| lb.is_displaced(&self.defined_functions)) {
            return true;
        }

        let Some(symbol_id) = self.symbols.lookup_id(name_id, Namespace::Ordinary) else {
            return false;
        };
        let typ = self.symbols.get(symbol_id).typ;
        shadowed_by_any_decl
            || self.types.kind(typ) != TypeKind::Function
            || library.is_some_and(|lb| !self.library_prototype_matches(lb, typ))
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

    /// One of the C99 7.12.14 relations: `__builtin_isgreater` and its five
    /// siblings, and C23's `__builtin_iseqsig`.
    ///
    /// C17 7.12.14p1 requires real floating arguments. gcc relaxes that to
    /// "both real, at least one of them floating": an integer operand beside a
    /// floating one is accepted, and two integers, a pointer, a complex value
    /// or a structure is the error reported below, in gcc's words.
    ///
    /// The operands go through the usual arithmetic conversions, as the
    /// relational operators they stand for do, so `isless(1, 2.0)` compares
    /// two `double`s rather than reading an `int` as one. The result is `int`,
    /// 0 or 1.
    ///
    /// The comparison itself is desugared in the linearizer: writing it out as
    /// `a < b` here would duplicate the operand expressions, and
    /// `isunordered(f(), g())` must call each function once.
    fn parse_fp_compare(
        &mut self,
        name_id: StringId,
        token_pos: Position,
        cmp: FpCompare,
    ) -> ParseResult<Expr> {
        self.expect_special(b'(')?;
        let lhs = self.parse_assignment_expr()?;
        self.expect_special(b',')?;
        let rhs = self.parse_assignment_expr()?;
        self.expect_special(b')')?;

        let common = match (lhs.typ, rhs.typ) {
            (Some(l), Some(r)) if self.fp_compare_operands_valid(l, r) => {
                self.usual_arithmetic_conversions(l, r)
            }
            (Some(_), Some(_)) => {
                let name = self.idents.get_opt(name_id).unwrap_or("").to_string();
                diag::error_args(
                    token_pos,
                    "non-floating-point arguments in call to function '{0}'",
                    &[&name],
                );
                self.types.double_id
            }
            // An operand whose type is unknown has been diagnosed already.
            (Some(t), None) | (None, Some(t)) if self.types.is_float(t) => t,
            _ => self.types.double_id,
        };

        let lhs = self.convert_operand(lhs, common);
        let rhs = self.convert_operand(rhs, common);
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

    /// Whether `l` and `r` may be the operands of an fp comparison builtin:
    /// both real (integer or real floating), and at least one floating.
    fn fp_compare_operands_valid(&self, l: TypeId, r: TypeId) -> bool {
        let t = &self.types;
        // `is_integer` asks only the kind, so `_Complex int` and a vector of
        // `int` would pass it; neither is real.
        let real =
            |id| !t.is_complex(id) && !t.is_vector(id) && (t.is_integer(id) || t.is_float(id));
        real(l) && real(r) && (t.is_float(l) || t.is_float(r))
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
                if !self.enclosing_function.forwarding {
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

            crate::kw::BUILTIN_VA_START | crate::kw::BUILTIN_MS_VA_START => Some((|| {
                // __builtin_va_start(ap, last_param), or the Microsoft
                // flavour of it, which only an `ms_abi` function may use.
                let wants = if name_id == crate::kw::BUILTIN_MS_VA_START {
                    crate::abi::CallingConv::Win64
                } else {
                    crate::abi::CallingConv::C
                };
                self.expect_special(b'(')?;
                let ap = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                // Second arg is a parameter name
                let last_param_pos = self.current_pos();
                let last_param = self.expect_identifier()?;
                self.expect_special(b')')?;
                if !self.check_va_start(last_param, token_pos, last_param_pos)
                    || !self.check_va_start_convention(wants, &ap, token_pos)
                {
                    return Ok(self.diagnosed_call(self.types.void_id, token_pos));
                }
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
                // Second arg is a type, whose extents the value carries.
                let type_pos = self.current_pos();
                let (arg_type, dims) = self.parse_type_name_vm()?;
                self.expect_special(b')')?;
                if !self.check_va_arg_type(arg_type, dims.len(), type_pos) {
                    return Ok(self.diagnosed_call(self.types.int_id, token_pos));
                }
                let value = Self::typed_expr(
                    ExprKind::VaArg {
                        ap: Box::new(ap),
                        arg_type,
                    },
                    arg_type,
                    token_pos,
                );
                Ok(self.with_type_name_extents(dims, value))
            })()),
            crate::kw::BUILTIN_VA_END | crate::kw::BUILTIN_MS_VA_END => Some((|| {
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
            crate::kw::BUILTIN_VA_COPY | crate::kw::BUILTIN_MS_VA_COPY => Some((|| {
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

    /// gcc's rules for `__builtin_va_start(ap, last)`: the enclosing
    /// function must be variadic (an error), and `last` should be its last
    /// named parameter (a warning: `last` locates nothing here, the ABI's
    /// register save area does). `false` once the error is reported.
    fn check_va_start(&self, last: StringId, pos: Position, last_pos: Position) -> bool {
        let function = self.enclosing_function;
        if !function.variadic {
            diag::error(
                pos,
                &gettext("'va_start' used in function with fixed arguments"),
            );
            return false;
        }
        if function.last_param != Some(last) && diag::warning_group_enabled("varargs") {
            diag::warning(
                last_pos,
                &gettext("second parameter of 'va_start' not last named argument"),
            );
        }
        true
    }

    /// Each `va_start` belongs to one convention: `__builtin_va_start` lays
    /// out a System V `va_list` from the register save area, and
    /// `__builtin_ms_va_start` points a `__builtin_ms_va_list` at the stacked
    /// arguments of an `ms_abi` function. Either in the other kind of function
    /// is an error, as clang makes it -- gcc accepts both and emits code that
    /// reads the wrong frame -- and so is the Microsoft one on a list that is
    /// not a `__builtin_ms_va_list`. `false` once one is reported.
    fn check_va_start_convention(
        &self,
        wants: crate::abi::CallingConv,
        ap: &Expr,
        pos: Position,
    ) -> bool {
        use crate::abi::CallingConv;
        let message = match (wants, self.enclosing_function.conv) {
            (CallingConv::C, CallingConv::Win64) => "'va_start' used in Win64 ABI function",
            (CallingConv::Win64, CallingConv::C) => {
                "'__builtin_ms_va_start' used in System V ABI function"
            }
            (CallingConv::Win64, _) if !ap.typ.is_some_and(|t| self.types.is_ms_va_list(t)) => {
                "first argument to '__builtin_ms_va_start' not of type '__builtin_ms_va_list'"
            }
            _ => return true,
        };
        diag::error(pos, message);
        false
    }

    /// gcc's rules for `__builtin_va_arg(ap, type)`: the type must be a
    /// complete object type (C17 7.16.1.1p2), so an incomplete or function
    /// type is an error; one the default argument promotions change can
    /// never match what a caller passed, a warning. `false` once the error is
    /// reported. Types are named unqualified, as gcc names them.
    fn check_va_arg_type(&mut self, typ: TypeId, extents: usize, pos: Position) -> bool {
        let typ = self.types.unqualified(typ);
        let named = self.types.format_type(typ, Some(self.idents));
        if self.types.kind(typ) == TypeKind::Function {
            diag::error_args(
                pos,
                "second argument to 'va_arg' is a function type '{0}'",
                &[&named],
            );
            return false;
        }
        if self.type_name_is_incomplete(typ, extents) {
            diag::error_args(
                pos,
                "second argument to 'va_arg' is of incomplete type '{0}'",
                &[&named],
            );
            return false;
        }
        let promoted = self.types.default_argument_promote(typ);
        let promotes = matches!(
            self.types.kind(typ),
            TypeKind::Bool | TypeKind::Char | TypeKind::Short | TypeKind::Float
        );
        if promotes && promoted != typ {
            let promoted = self.types.format_type(promoted, Some(self.idents));
            diag::warning_args(
                pos,
                "'{0}' is promoted to '{1}' when passed through '...'",
                &[&named, &promoted],
            );
        }
        true
    }

    /// `__builtin_choose_expr`.
    fn parse_choose_expr(&mut self, name_id: StringId) -> Option<ParseResult<Expr>> {
        match name_id {
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

    /// `alloca`, bare or reserved.
    fn parse_memory_builtin(
        &mut self,
        name_id: StringId,
        token_pos: Position,
    ) -> Option<ParseResult<Expr>> {
        match name_id {
            crate::kw::BUILTIN_ALLOCA | crate::kw::ALLOCA => Some((|| {
                // gcc's prototype is `void *(size_t)`, for either spelling.
                use super::library_builtin::ProtoType::{SizeT, VoidPtr};
                let args = self.parse_prototyped_builtin(name_id, VoidPtr, &[SizeT], false)?;
                let Some(size) = args.and_then(|args| args.into_iter().next()) else {
                    return Ok(self.diagnosed_call(self.types.void_ptr_id, token_pos));
                };
                Ok(Self::typed_expr(
                    ExprKind::Alloca {
                        size: Box::new(size),
                    },
                    self.types.void_ptr_id,
                    token_pos,
                ))
            })()),
            _ => None,
        }
    }

    /// Floating-point constants, classification and sign tests.
    fn parse_float_builtin(
        &mut self,
        name_id: StringId,
        token_pos: Position,
    ) -> Option<ParseResult<Expr>> {
        if let Some(&(_, value, suffix)) = FLOAT_CONSTANT_BUILTINS
            .iter()
            .find(|(id, _, _)| *id == name_id)
        {
            return Some(self.parse_float_constant_builtin(name_id, value, suffix, token_pos));
        }
        match name_id {
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
            crate::kw::BUILTIN_ISGREATER
            | crate::kw::BUILTIN_ISGREATEREQUAL
            | crate::kw::BUILTIN_ISLESS
            | crate::kw::BUILTIN_ISLESSEQUAL
            | crate::kw::BUILTIN_ISLESSGREATER
            | crate::kw::BUILTIN_ISUNORDERED
            | crate::kw::BUILTIN_ISEQSIG => {
                let cmp = match name_id {
                    crate::kw::BUILTIN_ISGREATER => FpCompare::Greater,
                    crate::kw::BUILTIN_ISGREATEREQUAL => FpCompare::GreaterEqual,
                    crate::kw::BUILTIN_ISLESS => FpCompare::Less,
                    crate::kw::BUILTIN_ISLESSEQUAL => FpCompare::LessEqual,
                    crate::kw::BUILTIN_ISLESSGREATER => FpCompare::LessGreater,
                    crate::kw::BUILTIN_ISEQSIG => FpCompare::Equal,
                    _ => FpCompare::Unordered,
                };
                Some(self.parse_fp_compare(name_id, token_pos, cmp))
            }
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
                    || crate::constexpr::eval_float(
                        self,
                        crate::constexpr::ConstScope::Standard,
                        &arg,
                    )
                    .is_some();
                // Answering 1 here is final -- nothing later makes a constant
                // unconstant. Answering 0 is not: gcc decides this *after*
                // optimization, so `int x = 42; __builtin_constant_p(x)` is 1
                // at `-O1` and above, and only propagation knows. What the
                // parser cannot fold is deferred rather than refused.
                //
                // At `-O0` there is no optimization to wait for, and gcc
                // answers 0 on the spot: the answer is then a constant, and
                // `if (__builtin_constant_p(n))` drops its arm as any other
                // constant condition does (gcc.c-torture 20030330-1).
                let kind = if is_constant {
                    ExprKind::IntLit(1)
                } else if !self.library_call_policy.optimizing {
                    ExprKind::IntLit(0)
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
            crate::kw::BUILTIN_ASSUME_ALIGNED => Some(self.parse_assume_aligned(token_pos)),
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
                        binding: CalleeBinding::Library,
                        known: None,
                    },
                    void_id,
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
                self.expect_special(b'(')?;
                let ptr = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let expected = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let desired = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let succ_order = self.parse_assignment_expr()?;
                self.expect_special(b',')?;
                let fail_order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                // Returns bool (_Bool)
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicCompareExchangeStrong {
                        ptr: Box::new(ptr),
                        expected: Box::new(expected),
                        desired: Box::new(desired),
                        succ_order: Box::new(succ_order),
                        fail_order: Box::new(fail_order),
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
                let fail_order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                // Returns bool (_Bool)
                Ok(Self::typed_expr(
                    ExprKind::C11AtomicCompareExchangeWeak {
                        ptr: Box::new(ptr),
                        expected: Box::new(expected),
                        desired: Box::new(desired),
                        succ_order: Box::new(succ_order),
                        fail_order: Box::new(fail_order),
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
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Add, false, false))
            }
            crate::kw::SYNC_ADD_AND_FETCH => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Add, true, false))
            }
            crate::kw::ATOMIC_FETCH_ADD => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Add, false, true))
            }
            crate::kw::ATOMIC_ADD_FETCH => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Add, true, true))
            }
            crate::kw::SYNC_FETCH_AND_SUB => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Sub, false, false))
            }
            crate::kw::SYNC_SUB_AND_FETCH => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Sub, true, false))
            }
            crate::kw::ATOMIC_FETCH_SUB => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Sub, false, true))
            }
            crate::kw::ATOMIC_SUB_FETCH => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Sub, true, true))
            }
            crate::kw::SYNC_FETCH_AND_AND => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::And, false, false))
            }
            crate::kw::SYNC_AND_AND_FETCH => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::And, true, false))
            }
            crate::kw::ATOMIC_FETCH_AND => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::And, false, true))
            }
            crate::kw::ATOMIC_AND_FETCH => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::And, true, true))
            }
            crate::kw::SYNC_FETCH_AND_OR => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Or, false, false))
            }
            crate::kw::SYNC_OR_AND_FETCH => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Or, true, false))
            }
            crate::kw::ATOMIC_FETCH_OR => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Or, false, true))
            }
            crate::kw::ATOMIC_OR_FETCH => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Or, true, true))
            }
            crate::kw::SYNC_FETCH_AND_XOR => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Xor, false, false))
            }
            crate::kw::SYNC_XOR_AND_FETCH => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Xor, true, false))
            }
            crate::kw::ATOMIC_FETCH_XOR => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Xor, false, true))
            }
            crate::kw::ATOMIC_XOR_FETCH => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Xor, true, true))
            }
            crate::kw::SYNC_FETCH_AND_NAND => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Nand, false, false))
            }
            crate::kw::SYNC_NAND_AND_FETCH => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Nand, true, false))
            }
            crate::kw::ATOMIC_FETCH_NAND => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Nand, false, true))
            }
            crate::kw::ATOMIC_NAND_FETCH => {
                Some(self.parse_gnu_atomic_rmw(name_id, token_pos, GnuAtomicOp::Nand, true, true))
            }
            crate::kw::SYNC_BOOL_COMPARE_AND_SWAP => {
                Some(self.parse_gnu_atomic_cas(name_id, token_pos, false))
            }
            crate::kw::SYNC_VAL_COMPARE_AND_SWAP => {
                Some(self.parse_gnu_atomic_cas(name_id, token_pos, true))
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
                if !self.check_atomic_object(&ptr, name_id, false) {
                    return Ok(self.diagnosed_call(result_type, token_pos));
                }
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
                if !self.check_atomic_object(&ptr, name_id, false) {
                    return Ok(self.diagnosed_call(self.types.void_id, token_pos));
                }
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
                if !self.check_atomic_object(&ptr, name_id, false) {
                    return Ok(self.diagnosed_call(result_type, token_pos));
                }
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
                if !self.check_atomic_object(&ptr, name_id, false) {
                    return Ok(self.diagnosed_call(self.types.void_id, token_pos));
                }
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
                if !self.check_atomic_object(&ptr, name_id, false) {
                    return Ok(self.diagnosed_call(result_type, token_pos));
                }
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
                let fail_order = self.parse_assignment_expr()?;
                self.expect_special(b')')?;
                if !self.check_atomic_object(&ptr, name_id, false) {
                    return Ok(self.diagnosed_call(self.types.int_id, token_pos));
                }
                // c17 implements both as strong, so the flag chooses only
                // which node is built; a weak exchange that never fails
                // spuriously is a conforming weak exchange.
                let is_weak = self.eval_const_expr(&weak).is_some_and(|v| v != 0);
                let (ptr, expected, desired, succ_order, fail_order) = (
                    Box::new(ptr),
                    Box::new(expected),
                    Box::new(desired),
                    Box::new(succ_order),
                    Box::new(fail_order),
                );
                let kind = if is_weak {
                    ExprKind::C11AtomicCompareExchangeWeak {
                        ptr,
                        expected,
                        desired,
                        succ_order,
                        fail_order,
                    }
                } else {
                    ExprKind::C11AtomicCompareExchangeStrong {
                        ptr,
                        expected,
                        desired,
                        succ_order,
                        fail_order,
                    }
                };
                Ok(Self::typed_expr(kind, self.types.int_id, token_pos))
            })()),
            crate::kw::ATOMIC_TEST_AND_SET | crate::kw::ATOMIC_CLEAR => {
                Some(self.parse_atomic_flag_op(name_id, token_pos))
            }
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
            crate::kw::ATOMIC_ALWAYS_LOCK_FREE | crate::kw::ATOMIC_IS_LOCK_FREE => {
                Some(self.parse_lock_free_query(name_id, token_pos))
            }
            _ => None,
        }
    }

    /// `__atomic_test_and_set(ptr, order)` and `__atomic_clear(ptr, order)`,
    /// which gcc declares `bool (volatile void *, int)` and
    /// `void (volatile void *, int)`.
    ///
    /// Each works on the one byte at `ptr`, whatever it points to: the
    /// test-and-set exchanges 1 into it and reports whether it was already
    /// set (gcc documents "some non-zero value"; 1 is the one every target
    /// uses), the clear stores 0 into it.
    fn parse_atomic_flag_op(&mut self, name_id: StringId, pos: Position) -> ParseResult<Expr> {
        use super::library_builtin::ProtoType::{Bool, Int, Void, VolatileVoidPtr};
        let set = name_id == crate::kw::ATOMIC_TEST_AND_SET;
        let (ret, result) = if set {
            (Bool, self.types.int_id)
        } else {
            (Void, self.types.void_id)
        };
        let args = self.parse_prototyped_builtin(name_id, ret, &[VolatileVoidPtr, Int], false)?;
        let Some([ptr, order]) = args.and_then(|args| <[Expr; 2]>::try_from(args).ok()) else {
            return Ok(self.diagnosed_call(result, pos));
        };
        let uchar = self.types.uchar_id;
        let flag = self.types.intern(Type::pointer(uchar));
        let (ptr, order) = (Box::new(self.convert_operand(ptr, flag)), Box::new(order));
        let value = Self::typed_expr(ExprKind::IntLit(i64::from(set)), uchar, pos);
        if !set {
            let store = ExprKind::C11AtomicStore {
                ptr,
                val: Box::new(value),
                order,
            };
            return Ok(Self::typed_expr(store, result, pos));
        }
        let exchange = ExprKind::C11AtomicExchange {
            ptr,
            val: Box::new(value),
            order,
        };
        let swapped = Self::typed_expr(exchange, uchar, pos);
        let zero = Self::typed_expr(ExprKind::IntLit(0), uchar, pos);
        let was_set = ExprKind::Binary {
            op: BinaryOp::Ne,
            left: Box::new(swapped),
            right: Box::new(zero),
        };
        Ok(Self::typed_expr(was_set, result, pos))
    }

    /// `__atomic_always_lock_free(size, ptr)` and `__atomic_is_lock_free`,
    /// which gcc declares `bool (size_t, const volatile void *)`.
    ///
    /// Answered at parse time from the size alone, which is what both
    /// reduce to here: c17's atomics are lock-free exactly at the machine
    /// integer widths, and an over-aligned pointer cannot make a 16-byte
    /// object lock-free when the target has no 16-byte atomic. The `always`
    /// form is a constant, so its size must be one.
    fn parse_lock_free_query(&mut self, name_id: StringId, pos: Position) -> ParseResult<Expr> {
        use super::library_builtin::ProtoType::{Bool, ConstVolatileVoidPtr, SizeT};
        let bool_id = self.types.bool_id;
        let params = [SizeT, ConstVolatileVoidPtr];
        let args = self.parse_prototyped_builtin(name_id, Bool, &params, false)?;
        let Some(size) = args.and_then(|args| args.into_iter().next()) else {
            return Ok(self.diagnosed_call(bool_id, pos));
        };
        let size = match self.constant_argument(&size, i128::MIN..=i128::MAX) {
            ConstantArgument::InRange(n) => Some(n),
            _ if name_id == crate::kw::ATOMIC_ALWAYS_LOCK_FREE => {
                diag::error_args(
                    size.pos,
                    "non-constant argument 1 to '{0}'",
                    &["__atomic_always_lock_free"],
                );
                return Ok(self.diagnosed_call(bool_id, pos));
            }
            _ => None,
        };
        let lock_free = size
            .and_then(|n| u64::try_from(n).ok())
            .is_some_and(crate::target::atomic_is_lock_free);
        Ok(Self::typed_expr(
            ExprKind::IntLit(i64::from(lock_free)),
            bool_id,
            pos,
        ))
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
                binding: CalleeBinding::Declared,
                known: None,
            },
            ret,
            pos,
        ))
    }

    /// Whether `ptr` may be the object operand of the `__atomic_*` or
    /// `__sync_*` builtin `name`, which is overloaded on it: gcc requires a
    /// pointer to an integer or a pointer, of a size an atomic operation
    /// has -- and, for an `arithmetic` operation, not to a `_Bool`. Reported
    /// in gcc's words when it is not.
    fn check_atomic_object(&mut self, ptr: &Expr, name: StringId, arithmetic: bool) -> bool {
        let Some(typ) = ptr.typ else {
            return true;
        };
        let typ = self.decayed_type(typ);
        let pointee = (self.types.kind(typ) == TypeKind::Pointer)
            .then(|| self.types.base_type(typ))
            .flatten();
        let valid = pointee.is_some_and(|p| {
            let kind = self.types.kind(p);
            let operand = (self.is_integral(p) && !(arithmetic && kind == TypeKind::Bool))
                || kind == TypeKind::Pointer;
            operand && matches!(self.types.size_bytes(p), 1 | 2 | 4 | 8 | 16)
        });
        if !valid {
            let type_name = self.types.format_type(typ, Some(self.idents));
            let name = self.idents.get_opt(name).unwrap_or("");
            diag::error_args(
                ptr.pos,
                "operand type '{0}' is incompatible with argument 1 of '{1}'",
                &[&type_name, name],
            );
        }
        valid
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
        name_id: StringId,
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
        if !self.check_atomic_object(&ptr, name_id, true) {
            return Ok(self.diagnosed_call(result_type, token_pos));
        }
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
        name_id: StringId,
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
        if !self.check_atomic_object(&ptr, name_id, false) {
            return Ok(self.diagnosed_call(result_type, token_pos));
        }
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

    /// The floating type `suffix` names, or `None` where the target has no
    /// such type: `_Float128` is `__float128`, which macOS lacks.
    fn float_suffix_type(&self, suffix: FloatSuffix) -> Option<TypeId> {
        let t = &self.types;
        Some(match suffix {
            FloatSuffix::Double | FloatSuffix::F64 => t.double_id,
            FloatSuffix::Float | FloatSuffix::F32 => t.float_id,
            FloatSuffix::LongDouble => t.longdouble_id,
            FloatSuffix::F16 => t.float16_id,
            FloatSuffix::F128 if t.has_float128() => t.float128_id,
            FloatSuffix::F128 => return None,
        })
    }

    /// An infinity or NaN builtin ([`FLOAT_CONSTANT_BUILTINS`]): a constant
    /// of the type its suffix names.
    ///
    /// A NaN's string argument, when it is a literal that parses as gcc
    /// parses it (see [`nan_payload`]), is the payload -- quiet for `nan`,
    /// signalling for `nans`. Anything else is not folded, which is gcc's
    /// behaviour too: the `nan` forms become a call to the library function
    /// of the same name (`nanf16` and so on), which reads the string at run
    /// time, and the `nans` forms, which have no library function, are an
    /// error here where gcc's is a link failure.
    fn parse_float_constant_builtin(
        &mut self,
        name_id: StringId,
        value: FloatConstant,
        suffix: FloatSuffix,
        token_pos: Position,
    ) -> ParseResult<Expr> {
        let call_pos = self.current_pos();
        self.expect_special(b'(')?;
        let arg = match value {
            FloatConstant::Infinity => None,
            FloatConstant::Nan(_) => Some(self.parse_assignment_expr()?),
        };
        self.expect_special(b')')?;

        let Some(typ) = self.float_suffix_type(suffix) else {
            let name = self.idents.get(name_id).to_string();
            return Err(ParseError::new(
                format!("'{name}' is not supported on this target"),
                token_pos,
            ));
        };
        let fmt = self
            .types
            .fp_format(typ)
            .expect("a floating constant builtin's type is a floating type");
        let constant = |v| Ok(Self::typed_expr(ExprKind::FloatLit(v), typ, token_pos));
        let (FloatConstant::Nan(kind), Some(arg)) = (value, arg) else {
            return constant(FloatVal::infinity(false));
        };
        let payload = match &arg.kind {
            ExprKind::StringLit(s) => nan_payload(crate::token::lexer::payload_bytes(s)),
            _ => None,
        };
        match (payload, kind) {
            (Some(payload), _) => constant(FloatVal::nan_with_payload(fmt, payload, kind)),
            (None, NanKind::Quiet) => {
                let library = suffix.nan_library_function();
                Ok(self.call_library_function(&library, name_id, vec![arg], call_pos, token_pos))
            }
            (None, NanKind::Signalling) => {
                diag::error_args(
                    arg.pos,
                    "the argument of '__builtin_nans{0}' is not a string literal naming a NaN payload",
                    &[suffix.text()],
                );
                constant(FloatVal::nan())
            }
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
                let call_pos = self.current_pos();
                self.expect_special(b'(')?;
                let args = self.parse_argument_list()?;
                self.expect_special(b')')?;
                Ok(self.call_library_function(real_name, name_id, args, call_pos, token_pos))
            })())
        } else {
            None
        }
    }

    /// A call to the library function `real_name` with `args`, for the
    /// builtin `spelled` that stands for it. Diagnostics name `spelled`, as
    /// gcc's do.
    pub(super) fn call_library_function(
        &mut self,
        real_name: &str,
        spelled: StringId,
        args: Vec<Expr>,
        call_pos: Position,
        token_pos: Position,
    ) -> Expr {
        // Look up the real function by its name. A declaration in scope is
        // checked against exactly as an ordinary call to it would be.
        let real_name_id = self.idents.lookup(real_name);
        let row = real_name_id.and_then(LibraryBuiltin::by_bare_name);
        let symbol_id = real_name_id.and_then(|id| {
            self.symbols
                .lookup_id(id, crate::symbol::Namespace::Ordinary)
        });
        if let Some(symbol_id) = symbol_id {
            let func_expr = self.library_callee(symbol_id, token_pos);
            let func_type = self.resolved_function_type(&func_expr);
            self.check_call(func_type, Some(spelled), &args, call_pos);
            // The reserved spelling means the library function whatever the
            // program says about its bare name -- unless what it declared is
            // a different function altogether, whose arguments a fold would
            // misread.
            let typ = self.symbols.get(symbol_id).typ;
            let known = row
                .filter(|lb| self.library_prototype_matches(lb, typ))
                .and_then(LibraryBuiltin::called);
            return self.library_call(func_expr, args, known, token_pos);
        }
        // Not declared. gcc knows these intrinsically: a program may call
        // `__builtin_puts` without `<stdio.h>`, and glibc's
        // `bits/string_fortified.h` calls `__builtin___memcpy_chk` without
        // ever declaring `__memcpy_chk`. The function is declared with the
        // table's prototype, and the call checked against it.
        if let Some(lb) = row {
            if let Some(symbol_id) = self.declare_known_library_function(lb) {
                let func_expr = self.library_callee(symbol_id, token_pos);
                let func_type = self.resolved_function_type(&func_expr);
                self.check_call(func_type, Some(spelled), &args, call_pos);
                return self.library_call(func_expr, args, lb.called(), token_pos);
            }
        }
        diag::error_args(token_pos, "undeclared function '{0}'", &[real_name]);
        Self::typed_expr(ExprKind::IntLit(0), self.types.int_id, token_pos)
    }

    /// A function designator for `symbol_id`, typed with its declared type.
    fn library_callee(&self, symbol_id: SymbolId, pos: Position) -> Expr {
        let func_type = self.symbols.get(symbol_id).typ;
        Self::typed_expr(ExprKind::Ident(symbol_id), func_type, pos)
    }

    /// A call through `func` that reaches the library's function, never an
    /// inline definition of the same name (see `CalleeBinding::Library`),
    /// and is `known` to call that library function.
    fn library_call(
        &self,
        func: Expr,
        args: Vec<Expr>,
        known: Option<LibFn>,
        pos: Position,
    ) -> Expr {
        let ret_type = func
            .typ
            .and_then(|t| self.types.base_type(t))
            .unwrap_or(self.types.int_id);
        self.fold_zero_length_compare(Self::typed_expr(
            ExprKind::Call {
                func: Box::new(func),
                args,
                binding: CalleeBinding::Library,
                known,
            },
            ret_type,
            pos,
        ))
    }

    pub(super) fn parse_builtin_expr(
        &mut self,
        name_id: StringId,
        token_pos: Position,
    ) -> Option<ParseResult<Expr>> {
        // Each family answers `None` for a name it does not own.
        if let Some(result) = self.parse_library_builtin_call(name_id, token_pos) {
            return Some(result);
        }
        if let Some(result) = self.parse_varargs_builtin(name_id, token_pos) {
            return Some(result);
        }
        if let Some(result) = self.parse_bit_builtin(name_id, token_pos) {
            return Some(result);
        }
        if let Some(result) = self.parse_choose_expr(name_id) {
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
        if let Some(result) = self.parse_generic_builtin(name_id, token_pos) {
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
    pub(super) fn object_extent(&self, expr: &Expr) -> Option<ObjectExtent> {
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

    /// The return type the prototype table gives the library function
    /// `name`, if it has a row.
    pub(super) fn library_return_type(&self, name: &str) -> Option<TypeId> {
        let lb = LibraryBuiltin::by_bare_name(self.idents.lookup(name)?)?;
        Some(lb.return_type(self.types))
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
                | crate::kw::BUILTIN_FMAXL
                | crate::kw::BUILTIN_FMINL
                | crate::kw::BUILTIN_POW
                | crate::kw::BUILTIN_POWF
                | crate::kw::BUILTIN_POWL
                | crate::kw::BUILTIN_FMAL
                | crate::kw::BUILTIN_BCMP
                | crate::kw::BUILTIN_BZERO
                | crate::kw::BUILTIN_STPNCPY
                | crate::kw::BUILTIN_CBRT
                | crate::kw::BUILTIN_CBRTF
                | crate::kw::BUILTIN_CBRTL
                | crate::kw::BUILTIN_CEILL
                | crate::kw::BUILTIN_FLOORL
                | crate::kw::BUILTIN_TRUNCL
                | crate::kw::BUILTIN_ROUNDL
                | crate::kw::BUILTIN_RINTL
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
                | crate::kw::BUILTIN_STRCPY
                | crate::kw::BUILTIN_STRNCPY
                | crate::kw::BUILTIN_STPCPY
                | crate::kw::BUILTIN_STRCAT
                | crate::kw::BUILTIN_STRNCAT
                | crate::kw::BUILTIN_STRNCMP
                | crate::kw::BUILTIN_STRCHR
                | crate::kw::BUILTIN_STRRCHR
                | crate::kw::BUILTIN_STRSTR
                | crate::kw::BUILTIN_MEMCHR
                | crate::kw::BUILTIN_INDEX
                | crate::kw::BUILTIN_RINDEX
                | crate::kw::BUILTIN_PUTCHAR
                | crate::kw::BUILTIN_STRCSPN
                | crate::kw::BUILTIN_STRSPN
                | crate::kw::BUILTIN_STRPBRK
                | crate::kw::BUILTIN_PRINTF_UNLOCKED
                | crate::kw::BUILTIN_FPRINTF_UNLOCKED
                | crate::kw::BUILTIN_FPRINTF
                | crate::kw::BUILTIN_FPUTS
                | crate::kw::BUILTIN_FPUTC
                | crate::kw::BUILTIN_FWRITE
                | crate::kw::BUILTIN_FPUTS_UNLOCKED
        )
    }

    /// Lower a math builtin to an ordinary call to the library function
    /// `name_id` that implements it, declaring that function if the
    /// translation unit has not. `args` are already converted to `params`.
    ///
    /// A zero of the return type stands in if the name cannot be declared,
    /// which the symbol table has already diagnosed.
    pub(super) fn libm_call(
        &mut self,
        name_id: StringId,
        ret_type: TypeId,
        params: &[TypeId],
        args: Vec<Expr>,
        pos: Position,
    ) -> Expr {
        let func_type = self
            .types
            .intern(Type::function(ret_type, params.to_vec(), false, false));
        let Some(symbol_id) = self.declare_library_function_id(name_id, func_type) else {
            let zero = Self::typed_expr(ExprKind::IntLit(0), self.types.int_id, pos);
            return self.convert_operand(zero, ret_type);
        };
        let func_type = self.symbols.get(symbol_id).typ;
        let func_expr = Self::typed_expr(ExprKind::Ident(symbol_id), func_type, pos);
        Self::typed_expr(
            ExprKind::Call {
                func: Box::new(func_expr),
                args,
                binding: CalleeBinding::Library,
                known: None,
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
        let func_type = self
            .types
            .intern(Type::function(ret_type, params.to_vec(), false, false));
        self.declare_library_function_id(name_id, func_type)
    }

    /// Declare the library function `name_id` as of the function type
    /// `func_type`, reusing any existing declaration.
    pub(super) fn declare_library_function_id(
        &mut self,
        name_id: StringId,
        func_type: TypeId,
    ) -> Option<SymbolId> {
        if let Some(existing) = self.symbols.lookup_id(name_id, Namespace::Ordinary) {
            // What the name already means may not be a function at all --
            // `int puts;` binds a name C17 7.1.3 reserves to an object. There
            // is then no library function here to call, and handing back the
            // object's symbol makes a call through its storage. This is the
            // same test `builtin_is_shadowed` ends with.
            //
            // Declining is what every caller expects: the libm path
            // substitutes a zero, `__builtin___clear_cache` raises a parse
            // error, and a `__builtin_X` call falls through to `_chk`
            // synthesis and then a diagnostic.
            if self.types.kind(self.symbols.get(existing).typ) != TypeKind::Function {
                return None;
            }
            return Some(existing);
        }
        let sym = Symbol::function(name_id, func_type, 0);
        self.symbols.declare(sym).ok()
    }
}

/// The floating type a builtin's suffix names: `__builtin_inf` is a
/// `double`, `__builtin_inff16` a `_Float16`. `_Float32` and `_Float64` are
/// `float` and `double` in c17, not types of their own.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FloatSuffix {
    Double,
    Float,
    LongDouble,
    F16,
    F32,
    F64,
    F128,
}

impl FloatSuffix {
    /// How the suffix is spelled at the end of a function's name.
    fn text(self) -> &'static str {
        match self {
            FloatSuffix::Double => "",
            FloatSuffix::Float => "f",
            FloatSuffix::LongDouble => "l",
            FloatSuffix::F16 => "f16",
            FloatSuffix::F32 => "f32",
            FloatSuffix::F64 => "f64",
            FloatSuffix::F128 => "f128",
        }
    }

    /// The library function a quiet NaN builtin of this suffix calls for a
    /// string it cannot fold, as gcc's does.
    fn nan_library_function(self) -> String {
        format!("nan{}", self.text())
    }
}

/// The value an infinity or NaN builtin produces.
#[derive(Debug, Clone, Copy)]
enum FloatConstant {
    /// `__builtin_inf` and `__builtin_huge_val`, which are the same thing
    /// on every IEEE format.
    Infinity,
    /// `__builtin_nan` (quiet) and `__builtin_nans` (signalling), whose
    /// string argument names the payload.
    Nan(NanKind),
}

/// Every infinity and NaN builtin: what it produces, and in which type.
#[rustfmt::skip]
const FLOAT_CONSTANT_BUILTINS: &[(StringId, FloatConstant, FloatSuffix)] = {
    use crate::kw::*;
    use FloatConstant::{Infinity as Inf, Nan};
    use FloatSuffix::*;
    const QUIET: FloatConstant = Nan(NanKind::Quiet);
    const SIGNALLING: FloatConstant = Nan(NanKind::Signalling);
    &[
        (BUILTIN_INF,           Inf,        Double),
        (BUILTIN_INFF,          Inf,        Float),
        (BUILTIN_INFL,          Inf,        LongDouble),
        (BUILTIN_INFF16,        Inf,        F16),
        (BUILTIN_INFF32,        Inf,        F32),
        (BUILTIN_INFF64,        Inf,        F64),
        (BUILTIN_INFF128,       Inf,        F128),
        (BUILTIN_HUGE_VAL,      Inf,        Double),
        (BUILTIN_HUGE_VALF,     Inf,        Float),
        (BUILTIN_HUGE_VALL,     Inf,        LongDouble),
        (BUILTIN_HUGE_VALF16,   Inf,        F16),
        (BUILTIN_HUGE_VALF32,   Inf,        F32),
        (BUILTIN_HUGE_VALF64,   Inf,        F64),
        (BUILTIN_HUGE_VALF128,  Inf,        F128),
        (BUILTIN_NAN,           QUIET,      Double),
        (BUILTIN_NANF,          QUIET,      Float),
        (BUILTIN_NANL,          QUIET,      LongDouble),
        (BUILTIN_NANF16,        QUIET,      F16),
        (BUILTIN_NANF32,        QUIET,      F32),
        (BUILTIN_NANF64,        QUIET,      F64),
        (BUILTIN_NANF128,       QUIET,      F128),
        (BUILTIN_NANS,          SIGNALLING, Double),
        (BUILTIN_NANSF,         SIGNALLING, Float),
        (BUILTIN_NANSL,         SIGNALLING, LongDouble),
        (BUILTIN_NANSF16,       SIGNALLING, F16),
        (BUILTIN_NANSF32,       SIGNALLING, F32),
        (BUILTIN_NANSF64,       SIGNALLING, F64),
        (BUILTIN_NANSF128,      SIGNALLING, F128),
    ]
};

/// The payload a `__builtin_nan` string names, or `None` if the string is
/// not one gcc folds.
///
/// Parsed as gcc's `real_nan` parses it, which is `strtoull` with base 0 and
/// no overflow check: leading white space, an optional sign that is then
/// ignored, `0x` for hexadecimal or a leading `0` for octal, and digits that
/// must run to the end of the string. An empty string, and a bare `0x`, are
/// zero. Digits past the 128th bit wrap, which drops only bits that no
/// format's payload has room for. A C string ends at its first NUL.
fn nan_payload(bytes: impl Iterator<Item = u8>) -> Option<u128> {
    let s: Vec<u8> = bytes.take_while(|&b| b != 0).collect();
    let mut rest = s.as_slice();
    while let [b' ' | b'\t' | b'\n' | b'\x0b' | b'\x0c' | b'\r', tail @ ..] = rest {
        rest = tail;
    }
    if let [b'-' | b'+', tail @ ..] = rest {
        rest = tail;
    }
    let base = match rest {
        [b'0', b'x' | b'X', tail @ ..] => {
            rest = tail;
            16
        }
        [b'0', tail @ ..] => {
            rest = tail;
            8
        }
        _ => 10,
    };
    let mut value: u128 = 0;
    for &c in rest {
        let digit = char::from(c).to_digit(16).filter(|&d| d < base)?;
        value = value
            .wrapping_mul(u128::from(base))
            .wrapping_add(u128::from(digit));
    }
    Some(value)
}
