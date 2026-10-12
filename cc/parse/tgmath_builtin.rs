//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__builtin_tgmath(functions..., arguments...)`, gcc 8's builtin for
// <tgmath.h>: the call of whichever function the arguments pick by C2x's
// type-generic rules (7.27p3), each argument expanded and evaluated once.
// gcc 13's semantics and diagnostics.
//

use super::ast::Expr;
use super::builtin_args::BuiltinArgs;
use super::parser::{ParseResult, Parser};
use crate::diag;
use crate::token::lexer::Position;
use crate::types::{FloatClass, TypeId, TypeKind};

impl Parser<'_> {
    /// `__builtin_tgmath( ... )`, its name already consumed.
    ///
    /// The functions come first: as many as leave, after them, one argument
    /// per parameter of the first. A parameter is generic where the
    /// functions' types for it differ, and each function's generic type is
    /// its type there. The type the arguments for the generic parameters
    /// ask for is their usual arithmetic conversion, an integer counting as
    /// `double`, made complex where any of them is; a real one may also
    /// pick a complex function, where no function is real. What is called
    /// is that function with the arguments, as an ordinary call.
    pub(super) fn parse_tgmath(&mut self, pos: Position) -> ParseResult<Expr> {
        let BuiltinArgs { args, call_pos } = self.parse_builtin_call_args()?;
        let int = self.types.int_id;
        let refused = |p: &mut Self| Ok(p.diagnosed_call(int, pos));
        let too_few = |p: &mut Self| {
            diag::error_args(
                call_pos,
                "too few arguments to '{0}'",
                &["__builtin_tgmath"],
            );
            refused(p)
        };
        let Some(first) = args.first() else {
            return too_few(self);
        };
        let Some(arity) = self.tgmath_function(first, 1) else {
            return refused(self);
        };
        let arity = arity.len();
        if args.len() < arity + 2 {
            return too_few(self);
        }
        let count = args.len() - arity;
        let mut params = Vec::with_capacity(count);
        for (i, f) in args[..count].iter().enumerate() {
            let Some(p) = self.tgmath_function(f, i + 1) else {
                return refused(self);
            };
            if p.len() != arity {
                diag::error_args(
                    f.pos,
                    "argument {0} of '{1}' has wrong number of arguments",
                    &[&(i + 1).to_string(), "__builtin_tgmath"],
                );
                return refused(self);
            }
            params.push(p);
        }
        let generic: Vec<usize> = (0..arity)
            .filter(|&k| {
                params
                    .iter()
                    .any(|p| !self.types.types_compatible(p[k], params[0][k]))
            })
            .collect();
        let Some(&first_generic) = generic.first() else {
            diag::error_args(
                call_pos,
                "function arguments of '{0}' all have the same type",
                &["__builtin_tgmath"],
            );
            return refused(self);
        };
        let generic_type: Vec<TypeId> = params.iter().map(|p| p[first_generic]).collect();
        for (i, a) in generic_type.iter().enumerate() {
            if generic_type[..i]
                .iter()
                .any(|b| self.types.types_compatible(*a, *b))
            {
                diag::error_args(
                    args[i].pos,
                    "duplicate type-generic parameter type for function argument {0} of '{1}'",
                    &[&(i + 1).to_string(), "__builtin_tgmath"],
                );
                return refused(self);
            }
        }

        let call_args = &args[count..];
        for &k in &generic {
            let arg = &call_args[k];
            let typ = arg.typ.unwrap_or(int);
            let arithmetic = self.types.is_arithmetic(typ) && !self.types.is_vector(typ);
            if !arithmetic {
                diag::error_args(
                    arg.pos,
                    "invalid type of argument {0} of type-generic function",
                    &[&(k + 1).to_string()],
                );
                return refused(self);
            }
        }
        let bases: Vec<TypeId> = generic
            .iter()
            .map(|&k| {
                let typ = call_args[k].typ.unwrap_or(int);
                self.types.complex_base(typ)
            })
            .collect();
        let complex = generic
            .iter()
            .any(|&k| self.types.is_complex(call_args[k].typ.unwrap_or(int)));
        // C2x 7.27p3: an integer counts as `double`, but as `_Float32x`
        // where another argument is of a `_FloatNx` type, real or complex.
        let extended = bases.iter().any(|&b| {
            self.types.is_float(b) && self.types.get(b).float_class == FloatClass::Extended
        });
        let integer_as = if extended {
            self.types.floating(TypeKind::Double, FloatClass::Extended)
        } else {
            self.types.double_id
        };
        let mut real: Option<TypeId> = None;
        for &base in &bases {
            let this = if self.types.is_float(base) {
                base
            } else {
                integer_as
            };
            real = Some(match real {
                Some(r) => self.usual_arithmetic_conversions(r, this),
                None => this,
            });
        }
        let real = real.expect("a generic parameter has an argument");
        let wanted = |p: &mut Self, typ: TypeId| {
            generic_type
                .iter()
                .position(|&g| p.types.types_compatible(g, typ))
        };
        let complex_of = self.types.make_complex(real);
        let pick = if complex {
            wanted(self, complex_of)
        } else if self.tgmath_narrowing(&args[..count]) {
            wanted(self, real).or_else(|| self.tgmath_first_holding(&generic_type, real))
        } else {
            let any_real = generic_type.iter().any(|&g| !self.types.is_complex(g));
            if any_real {
                wanted(self, real)
            } else {
                wanted(self, complex_of)
            }
        };
        let Some(pick) = pick else {
            diag::error(call_pos, "no matching function for type-generic call");
            return refused(self);
        };
        let mut args = args;
        let call_args = args.split_off(count);
        let callee = args.swap_remove(pick);
        Ok(self.checked_call(callee, call_args, call_pos, pos))
    }

    /// Whether the functions round their result to a narrower type (C2x
    /// 7.12.14, `fadd`, `f32xsqrt`): every one returns the same real
    /// floating type, so the result type is no generic parameter. Such a
    /// call may reach a function whose parameter type is not the arguments'
    /// type but holds all its values.
    fn tgmath_narrowing(&self, functions: &[Expr]) -> bool {
        let returns: Vec<TypeId> = functions
            .iter()
            .filter_map(|f| self.resolved_function_type(f))
            .map(|func| self.types.base_type(func).unwrap_or(func))
            .collect();
        let Some(&first) = returns.first() else {
            return false;
        };
        self.types.is_float(first)
            && !self.types.is_complex(first)
            && returns
                .iter()
                .all(|&r| self.types.types_compatible(r, first))
    }

    /// For a narrowing call whose arguments' type `real` no function takes:
    /// the first function, in the order given, whose generic parameter type
    /// holds every value of `real` -- gcc's pick, so that `f32xsqrt` of a
    /// `double` is `f32xsqrtf64x`, the first listed, not `f32xsqrtf64`.
    fn tgmath_first_holding(&self, generic_type: &[TypeId], real: TypeId) -> Option<usize> {
        let want = self.types.fp_format(real)?;
        generic_type.iter().position(|&g| {
            !self.types.is_complex(g)
                && self.types.fp_format(g).is_some_and(|have| have.holds(want))
        })
    }

    /// The parameter types of function argument `n` (1-based) of a
    /// `__builtin_tgmath`, or `None` once it is reported as no function
    /// pointer, one without a prototype, or a variadic one.
    fn tgmath_function(&self, f: &Expr, n: usize) -> Option<Vec<TypeId>> {
        let name = "__builtin_tgmath";
        let n_str = n.to_string();
        let Some(func) = self.resolved_function_type(f) else {
            diag::error_args(
                f.pos,
                "argument {0} of '{1}' is not a function pointer",
                &[&n_str, name],
            );
            return None;
        };
        let t = self.types.get(func);
        let Some(params) = t.params.clone() else {
            diag::error_args(
                f.pos,
                "argument {0} of '{1}' is unprototyped",
                &[&n_str, name],
            );
            return None;
        };
        if t.variadic {
            diag::error_args(
                f.pos,
                "argument {0} of '{1}' has variable arguments",
                &[&n_str, name],
            );
            return None;
        }
        Some(params)
    }
}
