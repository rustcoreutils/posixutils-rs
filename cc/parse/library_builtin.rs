//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Library functions c17 knows by prototype, and calls to them
//
// C17 7.1.4p1 lets an implementation evaluate a library function in place,
// but what the program wrote is still a call: its arguments are checked and
// converted against the prototype (6.5.2.2p2, p7) and its result is a value
// (6.5.2.2p5). So a call to one of these is parsed as a call -- the ordinary
// argument list, the ordinary checks -- and only then handed to the lowering
// the table names for it.
//

use super::ast::{Expr, ExprKind, InlineLibraryFn};
use super::parser::{ParseResult, Parser};
use crate::kw;
use crate::strings::StringId;
use crate::token::lexer::Position;
use crate::types::{Type, TypeId, TypeTable};

/// A type in one of the prototypes below.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ProtoType {
    Int,
    Long,
    LongLong,
    Float,
    Double,
    LongDouble,
    ComplexFloat,
    ComplexDouble,
    ComplexLongDouble,
}

impl ProtoType {
    fn id(self, t: &TypeTable) -> TypeId {
        match self {
            ProtoType::Int => t.int_id,
            ProtoType::Long => t.long_id,
            ProtoType::LongLong => t.longlong_id,
            ProtoType::Float => t.float_id,
            ProtoType::Double => t.double_id,
            ProtoType::LongDouble => t.longdouble_id,
            ProtoType::ComplexFloat => t.complex_float_id,
            ProtoType::ComplexDouble => t.complex_double_id,
            ProtoType::ComplexLongDouble => t.complex_longdouble_id,
        }
    }
}

/// How a call to a library builtin is evaluated, once its argument has been
/// checked.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Lowering {
    /// Computed in place, as an [`ExprKind::InlineLibraryCall`].
    InPlace(InlineLibraryFn),
    /// An ordinary call, to the function's `float` form `narrow` when the
    /// argument is a `float`.
    ///
    /// `(float)floor((double)x)` is `floorf(x)` exactly: the result is an
    /// integer no greater in magnitude than `x`, so a value representable as
    /// a `float` stays representable, and converting it up to `double` and
    /// back changes nothing. The condition is on the **argument** type and
    /// not the result -- `double q(float a) { return floor(a); }` narrows
    /// too, because the narrowing happens before the widening.
    ///
    /// Only the exactly-rounding functions qualify, which is why they are
    /// enumerated rather than derived. `sin` and `log` are not among them:
    /// `sinf(x)` and `(float)sin((double)x)` differ in the last bit for some
    /// `x`, and narrowing one is a wrong answer rather than a faster one.
    NarrowingCall { narrow: &'static str },
}

/// Which spelling of a library builtin named it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Spelling {
    /// The library's own name (`abs`): the program's to displace, and a
    /// builtin only where it is being called.
    Bare,
    /// The `__builtin_` name, reserved to the implementation in every scope.
    Reserved,
}

/// A library function c17 knows by prototype. Each takes one argument.
#[derive(Debug)]
pub(super) struct LibraryBuiltin {
    /// The library's name for the function, which is also its bare keyword.
    bare: StringId,
    /// The `__builtin_` spelling, when this table is what that spelling
    /// means. The floor family's reserved spellings are plain library
    /// aliases instead (see `parse_library_builtin`).
    reserved: Option<StringId>,
    ret: ProtoType,
    param: ProtoType,
    lowering: Lowering,
}

const fn entry(
    bare: StringId,
    reserved: Option<StringId>,
    ret: ProtoType,
    param: ProtoType,
    lowering: Lowering,
) -> LibraryBuiltin {
    LibraryBuiltin {
        bare,
        reserved,
        ret,
        param,
        lowering,
    }
}

/// Every library builtin, with its prototype and how it is evaluated.
///
/// `intmax_t` is `long` on every target c17 has, as
/// `chk_builtin_return_type` also answers.
#[rustfmt::skip]
static LIBRARY_BUILTINS: &[LibraryBuiltin] = {
    use InlineLibraryFn as F;
    use Lowering::{InPlace, NarrowingCall};
    use ProtoType::*;
    &[
        //    bare           reserved                   returns            parameter          lowering
        entry(kw::ABS,       Some(kw::BUILTIN_ABS),     Int,               Int,               InPlace(F::IntAbs)),
        entry(kw::LABS,      Some(kw::BUILTIN_LABS),    Long,              Long,              InPlace(F::IntAbs)),
        entry(kw::LLABS,     Some(kw::BUILTIN_LLABS),   LongLong,          LongLong,          InPlace(F::IntAbs)),
        entry(kw::IMAXABS,   Some(kw::BUILTIN_IMAXABS), Long,              Long,              InPlace(F::IntAbs)),
        entry(kw::FABS,      Some(kw::BUILTIN_FABS),    Double,            Double,            InPlace(F::Fabs)),
        entry(kw::FABSF,     Some(kw::BUILTIN_FABSF),   Float,             Float,             InPlace(F::Fabs)),
        entry(kw::FABSL,     Some(kw::BUILTIN_FABSL),   LongDouble,        LongDouble,        InPlace(F::Fabs)),
        entry(kw::FLOOR,     None,                      Double,            Double,            NarrowingCall { narrow: "floorf" }),
        entry(kw::CEIL,      None,                      Double,            Double,            NarrowingCall { narrow: "ceilf" }),
        entry(kw::TRUNC,     None,                      Double,            Double,            NarrowingCall { narrow: "truncf" }),
        entry(kw::ROUND,     None,                      Double,            Double,            NarrowingCall { narrow: "roundf" }),
        entry(kw::RINT,      None,                      Double,            Double,            NarrowingCall { narrow: "rintf" }),
        entry(kw::NEARBYINT, None,                      Double,            Double,            NarrowingCall { narrow: "nearbyintf" }),
        entry(kw::CREAL,     Some(kw::BUILTIN_CREAL),   Double,            ComplexDouble,     InPlace(F::ComplexReal)),
        entry(kw::CREALF,    Some(kw::BUILTIN_CREALF),  Float,             ComplexFloat,      InPlace(F::ComplexReal)),
        entry(kw::CREALL,    Some(kw::BUILTIN_CREALL),  LongDouble,        ComplexLongDouble, InPlace(F::ComplexReal)),
        entry(kw::CIMAG,     Some(kw::BUILTIN_CIMAG),   Double,            ComplexDouble,     InPlace(F::ComplexImag)),
        entry(kw::CIMAGF,    Some(kw::BUILTIN_CIMAGF),  Float,             ComplexFloat,      InPlace(F::ComplexImag)),
        entry(kw::CIMAGL,    Some(kw::BUILTIN_CIMAGL),  LongDouble,        ComplexLongDouble, InPlace(F::ComplexImag)),
        entry(kw::CONJ,      Some(kw::BUILTIN_CONJ),    ComplexDouble,     ComplexDouble,     InPlace(F::Conjugate)),
        entry(kw::CONJF,     Some(kw::BUILTIN_CONJF),   ComplexFloat,      ComplexFloat,      InPlace(F::Conjugate)),
        entry(kw::CONJL,     Some(kw::BUILTIN_CONJL),   ComplexLongDouble, ComplexLongDouble, InPlace(F::Conjugate)),
    ]
};

impl LibraryBuiltin {
    /// The library builtin `name_id` spells, and which spelling it is.
    fn lookup(name_id: StringId) -> Option<(&'static LibraryBuiltin, Spelling)> {
        LIBRARY_BUILTINS.iter().find_map(|lb| {
            if lb.bare == name_id {
                Some((lb, Spelling::Bare))
            } else if lb.reserved == Some(name_id) {
                Some((lb, Spelling::Reserved))
            } else {
                None
            }
        })
    }

    /// The library builtin whose bare name is `name_id`.
    pub(super) fn by_bare_name(name_id: StringId) -> Option<&'static LibraryBuiltin> {
        LIBRARY_BUILTINS.iter().find(|lb| lb.bare == name_id)
    }
}

impl Parser<'_> {
    /// Whether the function type `typ` declared for `lb`'s bare name is
    /// compatible with the library prototype.
    ///
    /// gcc's rule: a declaration with an incompatible type ("conflicting types
    /// for built-in function") makes the name an ordinary function, while the
    /// compatible one `<stdlib.h>` or `<math.h>` writes keeps the builtin. An
    /// unprototyped declaration says nothing about the parameters, so only
    /// its return type is compared; a prototype must have exactly the one
    /// parameter, whose qualifiers do not count (C17 6.7.6.3p15 -- which
    /// `types_compatible` already ignores at the top level).
    pub(super) fn library_prototype_matches(&self, lb: &LibraryBuiltin, typ: TypeId) -> bool {
        let (ret, param) = (lb.ret.id(self.types), lb.param.id(self.types));
        let decl = self.types.get(typ);
        let ret_ok = decl
            .base
            .is_some_and(|base| self.types.types_compatible(base, ret));
        let params_ok = match &decl.params {
            None => true,
            Some(params) => {
                !decl.variadic
                    && matches!(params.as_slice(), [p] if self.types.types_compatible(*p, param))
            }
        };
        ret_ok && params_ok
    }

    /// A call to a library builtin, if `name_id` spells one and -- for a bare
    /// spelling -- is being called. `None` leaves the name to the rest of
    /// builtin dispatch and then to the ordinary identifier path, which is
    /// what makes `double (*p)(double) = fabs;` name the library function
    /// rather than report a missing `(`. A `__builtin_` spelling is not an
    /// object, so demanding the `(` is the right diagnostic for it.
    ///
    /// Whether a declaration has displaced a bare spelling was settled before
    /// this is reached (see `builtin_is_shadowed`).
    pub(super) fn parse_library_builtin_call(
        &mut self,
        name_id: StringId,
        pos: Position,
    ) -> Option<ParseResult<Expr>> {
        let (lb, spelling) = LibraryBuiltin::lookup(name_id)?;
        if spelling == Spelling::Bare && !self.is_special(b'(') {
            return None;
        }
        Some(self.parse_checked_library_call(lb, pos))
    }

    /// The argument list of a call to `lb`, checked as an ordinary call to a
    /// function of its prototype is checked, then lowered.
    fn parse_checked_library_call(
        &mut self,
        lb: &LibraryBuiltin,
        pos: Position,
    ) -> ParseResult<Expr> {
        let call_pos = self.current_pos();
        self.expect_special(b'(')?;
        let args = self.parse_argument_list()?;
        self.expect_special(b')')?;

        let (ret, param) = (lb.ret.id(self.types), lb.param.id(self.types));
        let func_type = self
            .types
            .intern(Type::function(ret, vec![param], false, false));
        let sound = self.check_call(Some(func_type), &args, call_pos);
        match <[Expr; 1]>::try_from(args) {
            Ok([arg]) if sound => Ok(self.lower_library_call(lb, arg, pos)),
            // Diagnosed already. A zero of the return type stands in for the
            // call, so the enclosing expression still parses and types, and
            // no conversion is asked of an argument that has none.
            _ => {
                let zero = Self::typed_expr(ExprKind::IntLit(0), self.types.int_id, pos);
                Ok(self.convert_operand(zero, ret))
            }
        }
    }

    /// Evaluate a checked call to `lb` of `arg` as the table says to.
    fn lower_library_call(&mut self, lb: &LibraryBuiltin, arg: Expr, pos: Position) -> Expr {
        let (ret, param) = (lb.ret.id(self.types), lb.param.id(self.types));
        let name = self.idents.get_opt(lb.bare).unwrap_or("");
        match lb.lowering {
            Lowering::InPlace(func) => {
                let arg = self.convert_operand(arg, param);
                Self::typed_expr(
                    ExprKind::InlineLibraryCall {
                        func,
                        arg: Box::new(arg),
                    },
                    ret,
                    pos,
                )
            }
            Lowering::NarrowingCall { narrow } if self.is_binary32(&arg) => {
                let f = self.types.float_id;
                self.libm_call(narrow, f, &[f], arg, pos)
            }
            Lowering::NarrowingCall { .. } => {
                let arg = self.convert_operand(arg, param);
                self.libm_call(name, ret, &[param], arg, pos)
            }
        }
    }

    /// Whether `e` is a value in the IEEE single format.
    fn is_binary32(&self, e: &Expr) -> bool {
        e.typ
            .and_then(|t| self.types.fp_format(t))
            .is_some_and(|fmt| fmt == crate::float::FpFormat::Binary32)
    }
}
