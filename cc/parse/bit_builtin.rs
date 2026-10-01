//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The bit builtins: byte swaps, bit counts and `ffs`
//
// gcc declares each with a prototype -- `int __builtin_ctz(unsigned int)`,
// `uint16_t __builtin_bswap16(uint16_t)` -- so a call to one is checked and
// its argument converted exactly as for a call through that prototype
// (C17 6.5.2.2p2, p7): `__builtin_ctz(8.0)` counts the zeros of 8u. The table
// below is the one place those prototypes are written.
//

use super::ast::{BinaryOp, Expr, ExprKind};
use super::builtin_args::BuiltinArgs;
use super::library_builtin::ProtoType;
use super::parser::{ParseResult, Parser};
use crate::ir::constfold::{eval_bit_op, BitOp};
use crate::kw;
use crate::strings::StringId;
use crate::token::lexer::Position;
use crate::types::TypeId;

/// What a call to a bit builtin becomes, once its argument is converted.
#[derive(Clone, Copy)]
enum BitEval {
    /// The operation node of the argument.
    Node(fn(Box<Expr>) -> ExprKind),
    /// The low bit of the population-count node of the argument. One node,
    /// so `__builtin_parity(f())` calls `f` once.
    Parity(fn(Box<Expr>) -> ExprKind),
    /// A call to the C library function of this name, which performs this
    /// operation -- folded instead when the argument is a constant.
    Library(&'static str, BitOp),
}

/// A bit builtin and its prototype: `ret name(param)`.
struct BitBuiltin {
    name: StringId,
    ret: ProtoType,
    param: ProtoType,
    eval: BitEval,
}

const fn row(name: StringId, ret: ProtoType, param: ProtoType, eval: BitEval) -> BitBuiltin {
    BitBuiltin {
        name,
        ret,
        param,
        eval,
    }
}

/// Every bit builtin, with gcc's prototype for it.
///
/// `uint64_t` is spelled `unsigned long long`, the type the byte swap's
/// result has always had here; it has the width of `unsigned long` on every
/// target c17 has.
#[rustfmt::skip]
static BIT_BUILTINS: &[BitBuiltin] = {
    use BitEval::*;
    use ProtoType::*;
    &[
        //  name                     returns    parameter  evaluates
        row(kw::BUILTIN_BSWAP16,     UShort,    UShort,    Node(|arg| ExprKind::Bswap16 { arg })),
        row(kw::BUILTIN_BSWAP32,     UInt,      UInt,      Node(|arg| ExprKind::Bswap32 { arg })),
        row(kw::BUILTIN_BSWAP64,     ULongLong, ULongLong, Node(|arg| ExprKind::Bswap64 { arg })),
        row(kw::BUILTIN_CTZ,         Int,       UInt,      Node(|arg| ExprKind::Ctz { arg })),
        row(kw::BUILTIN_CTZL,        Int,       ULong,     Node(|arg| ExprKind::Ctzl { arg })),
        row(kw::BUILTIN_CTZLL,       Int,       ULongLong, Node(|arg| ExprKind::Ctzll { arg })),
        row(kw::BUILTIN_CLZ,         Int,       UInt,      Node(|arg| ExprKind::Clz { arg })),
        row(kw::BUILTIN_CLZL,        Int,       ULong,     Node(|arg| ExprKind::Clzl { arg })),
        row(kw::BUILTIN_CLZLL,       Int,       ULongLong, Node(|arg| ExprKind::Clzll { arg })),
        row(kw::BUILTIN_CLRSB,       Int,       Int,       Node(|arg| ExprKind::Clrsb { arg })),
        row(kw::BUILTIN_CLRSBL,      Int,       Long,      Node(|arg| ExprKind::Clrsbl { arg })),
        row(kw::BUILTIN_CLRSBLL,     Int,       LongLong,  Node(|arg| ExprKind::Clrsbll { arg })),
        row(kw::BUILTIN_POPCOUNT,    Int,       UInt,      Node(|arg| ExprKind::Popcount { arg })),
        row(kw::BUILTIN_POPCOUNTL,   Int,       ULong,     Node(|arg| ExprKind::Popcountl { arg })),
        row(kw::BUILTIN_POPCOUNTLL,  Int,       ULongLong, Node(|arg| ExprKind::Popcountll { arg })),
        row(kw::BUILTIN_PARITY,      Int,       UInt,      Parity(|arg| ExprKind::Popcount { arg })),
        row(kw::BUILTIN_PARITYL,     Int,       ULong,     Parity(|arg| ExprKind::Popcountl { arg })),
        row(kw::BUILTIN_PARITYLL,    Int,       ULongLong, Parity(|arg| ExprKind::Popcountll { arg })),
        row(kw::BUILTIN_FFS,         Int,       Int,       Library("ffs", BitOp::Ffs)),
        row(kw::BUILTIN_FFSL,        Int,       Long,      Library("ffsl", BitOp::Ffs)),
        row(kw::BUILTIN_FFSLL,       Int,       LongLong,  Library("ffsll", BitOp::Ffs)),
    ]
};

impl Parser<'_> {
    /// A call to the bit builtin `name_id` spells, if it spells one.
    pub(super) fn parse_bit_builtin(
        &mut self,
        name_id: StringId,
        pos: Position,
    ) -> Option<ParseResult<Expr>> {
        let row = BIT_BUILTINS.iter().find(|row| row.name == name_id)?;
        Some(self.parse_checked_bit_call(row, pos))
    }

    /// The argument list of a call to `row`, checked as an ordinary call
    /// through its prototype is, and converted to the parameter type.
    fn parse_checked_bit_call(&mut self, row: &BitBuiltin, pos: Position) -> ParseResult<Expr> {
        let BuiltinArgs { args, call_pos } = self.parse_builtin_call_args()?;
        let func_type = self.builtin_prototype(row.ret, &[row.param], false);
        let arg = self
            .check_prototyped_builtin(row.name, func_type, args, call_pos)
            .and_then(|args| args.into_iter().next());
        let Some(arg) = arg else {
            let ret = row.ret.id(self.types);
            return Ok(self.diagnosed_call(ret, pos));
        };
        Ok(self.evaluate_bit_builtin(row, arg, func_type, call_pos, pos))
    }

    /// The expression a call to `row`, of prototype `func_type`, of the
    /// converted `arg` stands for.
    fn evaluate_bit_builtin(
        &mut self,
        row: &BitBuiltin,
        arg: Expr,
        func_type: TypeId,
        call_pos: Position,
        pos: Position,
    ) -> Expr {
        let ret = row.ret.id(self.types);
        match row.eval {
            BitEval::Node(node) => Self::typed_expr(node(Box::new(arg)), ret, pos),
            BitEval::Parity(count) => {
                let count = Self::typed_expr(count(Box::new(arg)), ret, pos);
                let one = Self::typed_expr(ExprKind::IntLit(1), ret, pos);
                let parity = ExprKind::Binary {
                    op: BinaryOp::BitAnd,
                    left: Box::new(count),
                    right: Box::new(one),
                };
                Self::typed_expr(parity, ret, pos)
            }
            BitEval::Library(name, op) => {
                // gcc computes these inline, so one of a constant is an
                // integer constant expression there and makes no call.
                if let Some(value) = self.eval_const_expr(&arg) {
                    let width = self.types.size_bits(row.param.id(self.types));
                    let folded = eval_bit_op(op, width, value) as i64;
                    return Self::typed_expr(ExprKind::IntLit(folded), ret, pos);
                }
                // Declared with this prototype unless the program declared
                // it already, so the call passes the converted argument the
                // way the library function receives it.
                if let Some(bare) = self.idents.lookup(name) {
                    self.declare_library_function_id(bare, func_type);
                }
                self.call_library_function(name, row.name, vec![arg], call_pos, pos)
            }
        }
    }
}
