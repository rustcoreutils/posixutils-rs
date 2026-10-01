//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The argument rules builtins share
//
// A builtin gcc declares with a prototype is checked as a call through it
// (`parse_prototyped_builtin`). One that is type-generic -- `isnan`,
// `__builtin_add_overflow`, the atomics -- has rules of its own, but the
// same few recur: a count, a floating or integral argument, an integer
// constant in a range. Each is written once here, in gcc's words, and the
// builtin's family applies it where that family is parsed.
//

use super::ast::{Expr, ExprKind};
use super::library_builtin::ProtoType;
use super::parser::{ParseResult, Parser};
use crate::diag;
use crate::strings::StringId;
use crate::token::lexer::Position;
use crate::types::{Type, TypeId};
use std::ops::RangeInclusive;

/// What an argument that must be an integer constant turned out to be.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ConstantArgument {
    /// An integer constant expression whose value is in the range asked for.
    InRange(i128),
    /// An integer constant expression outside it.
    OutOfRange(i128),
    /// Anything else, a floating constant included.
    NotConstant,
}

/// A builtin's argument list, once parsed: the arguments, and where the
/// call's `(` stood -- the position a count diagnostic points at.
pub(super) struct BuiltinArgs {
    pub(super) args: Vec<Expr>,
    pub(super) call_pos: Position,
}

impl Parser<'_> {
    /// `( arguments )`.
    pub(super) fn parse_builtin_call_args(&mut self) -> ParseResult<BuiltinArgs> {
        let call_pos = self.current_pos();
        self.expect_special(b'(')?;
        let args = self.parse_argument_list()?;
        self.expect_special(b')')?;
        Ok(BuiltinArgs { args, call_pos })
    }

    /// The arguments of a type-generic builtin `name`, which takes from `min`
    /// to `max` of them; `None` once a wrong count has been reported.
    pub(super) fn parse_generic_builtin_args(
        &mut self,
        name: StringId,
        min: usize,
        max: usize,
    ) -> ParseResult<Option<Vec<Expr>>> {
        let BuiltinArgs { args, call_pos } = self.parse_builtin_call_args()?;
        let counted = self.check_argument_count(Some(name), args.len(), min, Some(max), call_pos);
        Ok(counted.then_some(args))
    }

    /// The function type `ret (params[, ...])`.
    pub(super) fn builtin_prototype(
        &mut self,
        ret: ProtoType,
        params: &[ProtoType],
        variadic: bool,
    ) -> TypeId {
        let ret = ret.id(self.types);
        let params = params.iter().map(|p| p.id(self.types)).collect();
        self.types
            .intern(Type::function(ret, params, variadic, false))
    }

    /// The arguments of a call to the builtin `name`, which gcc declares as
    /// `func_type`: checked as a call through that prototype is, and each
    /// one a parameter receives converted to its type (C17 6.5.2.2p7). Any
    /// past the parameters are left as written. `None` once the call has
    /// been reported.
    pub(super) fn check_prototyped_builtin(
        &mut self,
        name: StringId,
        func_type: TypeId,
        args: Vec<Expr>,
        call_pos: Position,
    ) -> Option<Vec<Expr>> {
        if !self.check_call(Some(func_type), Some(name), &args, call_pos) {
            return None;
        }
        let params = self.types.get(func_type).params.clone().unwrap_or_default();
        let mut args = args.into_iter();
        let mut converted: Vec<Expr> = params
            .iter()
            .zip(args.by_ref())
            .map(|(&param, arg)| self.convert_operand(arg, param))
            .collect();
        converted.extend(args);
        Some(converted)
    }

    /// `( arguments )` of the builtin `name`, checked and converted through
    /// the prototype `ret name(params[, ...])`; see
    /// [`Self::check_prototyped_builtin`].
    pub(super) fn parse_prototyped_builtin(
        &mut self,
        name: StringId,
        ret: ProtoType,
        params: &[ProtoType],
        variadic: bool,
    ) -> ParseResult<Option<Vec<Expr>>> {
        let BuiltinArgs { args, call_pos } = self.parse_builtin_call_args()?;
        let func_type = self.builtin_prototype(ret, params, variadic);
        Ok(self.check_prototyped_builtin(name, func_type, args, call_pos))
    }

    /// What stands for a call already reported as an error: a zero of the
    /// type the call has, so that the enclosing expression still parses and
    /// types, and nothing is asked of an argument that cannot give it.
    pub(super) fn diagnosed_call(&mut self, typ: TypeId, pos: Position) -> Expr {
        let zero = Self::typed_expr(ExprKind::IntLit(0), self.types.int_id, pos);
        self.convert_operand(zero, typ)
    }

    /// Whether `typ` is what gcc calls integral: an integer type, `_Bool` or
    /// an enumeration, and neither complex nor a vector.
    pub(super) fn is_integral(&self, typ: TypeId) -> bool {
        let t = &self.types;
        t.is_integer(typ) && !t.is_complex(typ) && !t.is_vector(typ)
    }

    /// `arg` as an integer constant in `range`. Only an integer constant
    /// expression (C17 6.6p6) is one: a floating constant is not, nor is a
    /// `const` object.
    pub(super) fn constant_argument(
        &self,
        arg: &Expr,
        range: RangeInclusive<i128>,
    ) -> ConstantArgument {
        let value = arg
            .typ
            .filter(|&t| self.is_integral(t))
            .and_then(|_| self.eval_const_expr(arg));
        match value {
            Some(v) if range.contains(&v) => ConstantArgument::InRange(v),
            Some(v) => ConstantArgument::OutOfRange(v),
            None => ConstantArgument::NotConstant,
        }
    }

    /// Whether `arg`, an argument of the type-generic builtin `callee`, has
    /// a real floating type, as gcc requires of the classification
    /// builtins; reported in gcc's words when it does not.
    pub(super) fn require_floating_argument(&self, arg: &Expr, callee: StringId) -> bool {
        // An argument whose type is unknown was reported already.
        if arg.typ.is_none_or(|t| self.types.is_float(t)) {
            return true;
        }
        let name = self.idents.get_opt(callee).unwrap_or("");
        diag::error_args(
            arg.pos,
            "non-floating-point argument in call to function '{0}'",
            &[name],
        );
        false
    }
}
