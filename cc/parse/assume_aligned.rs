//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__builtin_assume_aligned`
//
// gcc declares it `void *__builtin_assume_aligned(const void *, size_t, ...)`,
// so a call is checked and converted as a call through that prototype is
// (C17 6.5.2.2p2, p7): every argument is evaluated, and the result is the
// pointer as a `void *` -- whatever its pointee's qualifiers were. gcc adds
// two rules of its own: at most one argument follows the alignment, and that
// misalignment has integer type.
//
// c17 makes no use of the alignment, so the call computes nothing beyond
// its arguments' side effects.
//

use super::ast::{Expr, ExprKind};
use super::library_builtin::ProtoType;
use super::parser::{ParseResult, Parser};
use crate::diag;
use crate::kw;
use crate::token::lexer::Position;
use crate::types::{Type, TypeId};

/// The arguments gcc accepts: the pointer, the alignment and the
/// misalignment.
const MAX_ARGS: usize = 3;

impl Parser<'_> {
    /// A call to `__builtin_assume_aligned`, from its argument list on.
    pub(super) fn parse_assume_aligned(&mut self, pos: Position) -> ParseResult<Expr> {
        let call_pos = self.current_pos();
        self.expect_special(b'(')?;
        let mut args = self.parse_argument_list()?;
        self.expect_special(b')')?;

        let void_ptr = self.types.void_ptr_id;
        let params = vec![
            ProtoType::ConstVoidPtr.id(self.types),
            ProtoType::SizeT.id(self.types),
        ];
        let func_type = self
            .types
            .intern(Type::function(void_ptr, params, true, false));
        let name = Some(kw::BUILTIN_ASSUME_ALIGNED);
        let prototyped = self.check_call(Some(func_type), name, &args, call_pos);
        let extra = self.check_assume_aligned_extra(&args, call_pos);
        if !(prototyped && extra) {
            // Diagnosed already. A null `void *` stands in, so the enclosing
            // expression still parses and types.
            let zero = Self::typed_expr(ExprKind::IntLit(0), self.types.int_id, pos);
            return Ok(self.convert_operand(zero, void_ptr));
        }
        let ptr = args.remove(0);
        let ptr = self.convert_operand(ptr, void_ptr);
        Ok(Self::assume_aligned_value(ptr, args, void_ptr, pos))
    }

    /// gcc's rules past the prototype: no argument after the misalignment,
    /// which is an integer. `false` when either was reported.
    fn check_assume_aligned_extra(&self, args: &[Expr], call_pos: Position) -> bool {
        let name = "__builtin_assume_aligned";
        if args.len() > MAX_ARGS {
            diag::error_args(call_pos, "too many arguments to function '{0}'", &[name]);
            return false;
        }
        let Some(misalign) = args.get(MAX_ARGS - 1) else {
            return true;
        };
        if misalign.typ.is_some_and(|t| self.types.is_integer(t)) {
            return true;
        }
        diag::error_args(
            misalign.pos,
            "non-integer argument 3 in call to function '{0}'",
            &[name],
        );
        false
    }

    /// The value of the call: `ptr`, once the alignment and misalignment
    /// have been evaluated. A literal has nothing to evaluate, so the usual
    /// `__builtin_assume_aligned(p, 16)` is `ptr` alone.
    fn assume_aligned_value(ptr: Expr, rest: Vec<Expr>, void_ptr: TypeId, pos: Position) -> Expr {
        let mut parts: Vec<Expr> = rest
            .into_iter()
            .filter(|arg| !Self::is_literal_constant(arg))
            .collect();
        if parts.is_empty() {
            return ptr;
        }
        parts.push(ptr);
        Self::typed_expr(ExprKind::Comma(parts), void_ptr, pos)
    }
}
