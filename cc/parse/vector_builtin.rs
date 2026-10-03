//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's builtins over GNU vectors -- `__builtin_shuffle`,
// `__builtin_shufflevector` and `__builtin_convertvector` -- checked as gcc
// checks them and in gcc's words.
//

use super::ast::{Expr, ExprKind, ShuffleSelector};
use super::parser::{ParseResult, Parser};
use crate::diag;
use crate::kw;
use crate::token::lexer::Position;
use crate::types::TypeId;
use gettextrs::gettext;

impl Parser<'_> {
    /// A call to one of the vector builtins, if `name_id` spells one.
    pub(super) fn parse_vector_builtin(
        &mut self,
        name_id: crate::strings::StringId,
        pos: Position,
    ) -> Option<ParseResult<Expr>> {
        match name_id {
            kw::BUILTIN_SHUFFLE => Some(self.parse_builtin_shuffle(pos)),
            kw::BUILTIN_SHUFFLEVECTOR => Some(self.parse_builtin_shufflevector(pos)),
            kw::BUILTIN_CONVERTVECTOR => Some(self.parse_builtin_convertvector(pos)),
            _ => None,
        }
    }

    /// The lvalue-converted vector type of `e`, or `None`.
    fn vector_operand(&mut self, e: &Expr) -> Option<TypeId> {
        let t = self.lvalue_converted_type(e.typ?);
        self.types.is_vector(t).then_some(t)
    }

    /// `__builtin_shuffle(a, mask)` or `__builtin_shuffle(a, b, mask)`: a
    /// vector of `a`'s type whose lane `k` is lane `mask[k]` -- modulo the
    /// lanes `a` (and `b`) hold -- of `a` followed by `b`.
    fn parse_builtin_shuffle(&mut self, pos: Position) -> ParseResult<Expr> {
        let Some(mut args) = self.parse_generic_builtin_args(kw::BUILTIN_SHUFFLE, 2, 3)? else {
            return Ok(self.diagnosed_call(self.types.int_id, pos));
        };
        let mask = args.pop().expect("two or three arguments");
        let second = (args.len() == 2).then(|| args.pop().expect("three arguments"));
        let first = args.pop().expect("an operand");
        let types: Vec<Option<TypeId>> = [Some(&first), second.as_ref(), Some(&mask)]
            .into_iter()
            .flatten()
            .map(|e| self.vector_operand(e))
            .collect();
        let fault = if types.iter().any(Option::is_none) {
            Some("'__builtin_shuffle' arguments must be vectors".to_string())
        } else {
            let first_t = types[0].expect("checked");
            let mask_t = types[types.len() - 1].expect("checked");
            let (lane, count) = self.types.vector_lanes(first_t).expect("a vector");
            let (mlane, mcount) = self.types.vector_lanes(mask_t).expect("a vector");
            if second.is_some()
                && !self
                    .types
                    .types_compatible(first_t, types[1].expect("checked"))
            {
                Some("'__builtin_shuffle' argument vectors must be of the same type".to_string())
            } else if !self.types.is_integer(mlane) {
                Some("'__builtin_shuffle' last argument must be an integer vector".to_string())
            } else if count != mcount {
                Some(
                    "'__builtin_shuffle' number of elements of the argument vector(s) and \
                     the mask vector should be the same"
                        .to_string(),
                )
            } else if self.types.size_bits(lane) != self.types.size_bits(mlane) {
                Some(
                    "'__builtin_shuffle' argument vector(s) inner type must have the same \
                     size as inner type of the mask"
                        .to_string(),
                )
            } else {
                None
            }
        };
        if let Some(msg) = fault {
            diag::error(pos, &gettext(msg));
            return Ok(self.diagnosed_call(self.types.int_id, pos));
        }
        let result = self.types.unqualified(types[0].expect("checked"));
        let shuffle = ExprKind::VectorShuffle {
            first: Box::new(first),
            second: second.map(Box::new),
            selector: ShuffleSelector::Mask(Box::new(mask)),
        };
        Ok(Self::typed_expr(shuffle, result, pos))
    }

    /// `__builtin_shufflevector(a, b, i...)`: a vector of the operands'
    /// element type, one lane per constant index -- a power of two of them
    /// -- each naming a lane of `a` followed by `b`, or `-1` for any value.
    fn parse_builtin_shufflevector(&mut self, pos: Position) -> ParseResult<Expr> {
        let name = kw::BUILTIN_SHUFFLEVECTOR;
        let Some(mut args) = self.parse_generic_builtin_args(name, 3, usize::MAX)? else {
            return Ok(self.diagnosed_call(self.types.int_id, pos));
        };
        let index_exprs = args.split_off(2);
        let second = args.pop().expect("two operands");
        let first = args.pop().expect("two operands");
        let (Some(a), Some(b)) = (self.vector_operand(&first), self.vector_operand(&second)) else {
            diag::error(
                pos,
                &gettext("'__builtin_shufflevector' arguments must be vectors"),
            );
            return Ok(self.diagnosed_call(self.types.int_id, pos));
        };
        let (lane, na) = self.types.vector_lanes(a).expect("a vector");
        let (lane_b, nb) = self.types.vector_lanes(b).expect("a vector");
        if !self.types.types_compatible(lane, lane_b) {
            diag::error(
                pos,
                &gettext(
                    "'__builtin_shufflevector' argument vectors must have the same element type",
                ),
            );
            return Ok(self.diagnosed_call(self.types.int_id, pos));
        }
        let mut indices = Vec::with_capacity(index_exprs.len());
        for e in &index_exprs {
            match self.eval_const_expr(e) {
                Some(-1) => indices.push(None),
                Some(i) if (0..(na + nb) as i128).contains(&i) => indices.push(Some(i as u32)),
                _ => {
                    let spelled = self.index_spelling(e);
                    diag::error_args(
                        e.pos,
                        "invalid element index '{0}' to '__builtin_shufflevector'",
                        &[&spelled],
                    );
                    return Ok(self.diagnosed_call(self.types.int_id, pos));
                }
            }
        }
        if !indices.len().is_power_of_two() {
            diag::error(
                pos,
                &gettext(
                    "'__builtin_shufflevector' must specify a result with a power of two \
                     number of elements",
                ),
            );
            return Ok(self.diagnosed_call(self.types.int_id, pos));
        }
        let lane = self.types.unqualified(lane);
        let result = self.types.vector_of(lane, indices.len(), None);
        let shuffle = ExprKind::VectorShuffle {
            first: Box::new(first),
            second: Some(Box::new(second)),
            selector: ShuffleSelector::Indices(indices),
        };
        Ok(Self::typed_expr(shuffle, result, pos))
    }

    /// `__builtin_convertvector(v, T)`: each lane of `v` converted, as a
    /// cast converts it, to the lane type of the vector type `T`, which has
    /// as many lanes.
    fn parse_builtin_convertvector(&mut self, pos: Position) -> ParseResult<Expr> {
        self.expect_special(b'(')?;
        let value = self.parse_assignment_expr()?;
        self.expect_special(b',')?;
        let target = self.parse_type_name()?;
        self.expect_special(b')')?;
        let Some(from) = self.vector_operand(&value) else {
            diag::error(
                pos,
                &gettext(
                    "'__builtin_convertvector' first argument must be an integer or \
                     floating vector",
                ),
            );
            return Ok(self.diagnosed_call(self.types.int_id, pos));
        };
        let Some((_, to_count)) = self.types.vector_lanes(target) else {
            diag::error(
                pos,
                &gettext(
                    "'__builtin_convertvector' second argument must be an integer or \
                     floating vector type",
                ),
            );
            return Ok(self.diagnosed_call(self.types.int_id, pos));
        };
        let (_, from_count) = self.types.vector_lanes(from).expect("a vector");
        if from_count != to_count {
            diag::error(
                pos,
                &gettext(
                    "'__builtin_convertvector' number of elements of the first argument \
                     vector and the second argument vector type should be the same",
                ),
            );
            return Ok(self.diagnosed_call(self.types.int_id, pos));
        }
        let result = self.types.unqualified(target);
        let convert = ExprKind::ConvertVector {
            value: Box::new(value),
        };
        Ok(Self::typed_expr(convert, result, pos))
    }

    /// How a `__builtin_shufflevector` index is named in a diagnostic: its
    /// value, or the name it was written as.
    fn index_spelling(&self, e: &Expr) -> String {
        if let Some(v) = self.eval_const_expr(e) {
            return v.to_string();
        }
        match &e.kind {
            ExprKind::Ident(symbol) => self.idents.get(self.symbols.get(*symbol).name).to_string(),
            _ => String::from("..."),
        }
    }
}
