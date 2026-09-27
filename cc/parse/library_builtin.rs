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

use super::ast::{Expr, ExprKind, InlineLibraryFn, MathErrno, MemoryFn};
use super::parser::{ParseResult, Parser};
use crate::float::IntegralRounding;
use crate::kw;
use crate::strings::StringId;
use crate::token::lexer::Position;
use crate::types::{Type, TypeId, TypeTable};

/// What the command line says about evaluating library calls in place.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LibraryCallPolicy {
    /// `-O1` and above. At `-O0` gcc calls the library for a libm function
    /// named by its own spelling, and computes in place only one named
    /// `__builtin_*`; c17 does the same.
    pub optimizing: bool,
    /// `-fmath-errno`, gcc's default: a domain error sets `errno`, so a
    /// function that has one keeps a call for the arguments that raise it.
    pub math_errno: bool,
}

impl Default for LibraryCallPolicy {
    /// gcc's defaults once optimizing: in place, with `errno` kept.
    fn default() -> Self {
        LibraryCallPolicy {
            optimizing: true,
            math_errno: true,
        }
    }
}

impl LibraryCallPolicy {
    /// `func` as the table names it, with `-fno-math-errno` applied: a
    /// function that would report a domain error through `errno` does not.
    fn applied_to(self, func: InlineLibraryFn) -> InlineLibraryFn {
        match func {
            InlineLibraryFn::Sqrt(MathErrno::Set) if !self.math_errno => {
                InlineLibraryFn::Sqrt(MathErrno::Ignored)
            }
            _ => func,
        }
    }

    /// Whether a call to `func`, written with `spelling`, is computed in
    /// place rather than called.
    ///
    /// Always once optimizing. At `-O0` gcc calls the library for the bare
    /// spelling of a libm function, and for any spelling of one that must
    /// set `errno`, since only its optimizer can split a call off for the
    /// arguments that set it. The magnitudes, `copysign` and the complex
    /// accessors are bit operations and moves, and are in place at every
    /// level -- as `abs` and `fabs` are in gcc, which turns them into
    /// operators before anything else sees them. The block memory functions
    /// are their IR operation at every level too: whether one becomes loads
    /// and stores or the library call is decided on the IR
    /// (`ir::memexpand`), where a length that only inlining makes constant
    /// can still be seen.
    fn in_place(self, func: InlineLibraryFn, spelling: Spelling) -> bool {
        if self.optimizing {
            return true;
        }
        match func {
            InlineLibraryFn::IntAbs
            | InlineLibraryFn::Fabs
            | InlineLibraryFn::CopySign
            | InlineLibraryFn::ComplexReal
            | InlineLibraryFn::ComplexImag
            | InlineLibraryFn::Conjugate
            | InlineLibraryFn::Memory(_) => true,
            InlineLibraryFn::Sqrt(MathErrno::Set) => false,
            InlineLibraryFn::Sqrt(MathErrno::Ignored)
            | InlineLibraryFn::RoundToIntegral(_)
            | InlineLibraryFn::FMin
            | InlineLibraryFn::FMax
            | InlineLibraryFn::Fma => spelling == Spelling::Reserved,
        }
    }
}

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
    VoidPtr,
    ConstVoidPtr,
    /// `size_t`, which is `unsigned long` on every target c17 has.
    SizeT,
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
            ProtoType::VoidPtr => t.void_ptr_id,
            ProtoType::ConstVoidPtr => t.const_void_ptr_id,
            ProtoType::SizeT => t.ulong_id,
        }
    }
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

/// A library function c17 knows by prototype.
#[derive(Debug)]
pub(super) struct LibraryBuiltin {
    /// The library's name for the function, which is also its bare keyword.
    bare: StringId,
    /// The `__builtin_` spelling.
    reserved: StringId,
    ret: ProtoType,
    params: &'static [ProtoType],
    /// What a call computes, once its arguments have been checked.
    func: InlineLibraryFn,
}

const fn entry(
    bare: StringId,
    reserved: StringId,
    ret: ProtoType,
    params: &'static [ProtoType],
    func: InlineLibraryFn,
) -> LibraryBuiltin {
    LibraryBuiltin {
        bare,
        reserved,
        ret,
        params,
        func,
    }
}

/// Every library builtin, with its prototype and how it is evaluated.
///
/// `intmax_t` is `long` on every target c17 has, as
/// `chk_builtin_return_type` also answers.
#[rustfmt::skip]
static LIBRARY_BUILTINS: &[LibraryBuiltin] = {
    use InlineLibraryFn as F;
    use IntegralRounding as R;
    use MemoryFn as M;
    use ProtoType::*;
    const SQRT: F = F::Sqrt(MathErrno::Set);
    &[
        //    bare            reserved                 returns            parameters                        computes
        entry(kw::ABS,        kw::BUILTIN_ABS,         Int,               &[Int],                           F::IntAbs),
        entry(kw::LABS,       kw::BUILTIN_LABS,        Long,              &[Long],                          F::IntAbs),
        entry(kw::LLABS,      kw::BUILTIN_LLABS,       LongLong,          &[LongLong],                      F::IntAbs),
        entry(kw::IMAXABS,    kw::BUILTIN_IMAXABS,     Long,              &[Long],                          F::IntAbs),
        entry(kw::FABS,       kw::BUILTIN_FABS,        Double,            &[Double],                        F::Fabs),
        entry(kw::FABSF,      kw::BUILTIN_FABSF,       Float,             &[Float],                         F::Fabs),
        entry(kw::FABSL,      kw::BUILTIN_FABSL,       LongDouble,        &[LongDouble],                    F::Fabs),
        entry(kw::COPYSIGN,   kw::BUILTIN_COPYSIGN,    Double,            &[Double, Double],                F::CopySign),
        entry(kw::COPYSIGNF,  kw::BUILTIN_COPYSIGNF,   Float,             &[Float, Float],                  F::CopySign),
        entry(kw::COPYSIGNL,  kw::BUILTIN_COPYSIGNL,   LongDouble,        &[LongDouble, LongDouble],        F::CopySign),
        entry(kw::SQRT,       kw::BUILTIN_SQRT,        Double,            &[Double],                        SQRT),
        entry(kw::SQRTF,      kw::BUILTIN_SQRTF,       Float,             &[Float],                         SQRT),
        entry(kw::SQRTL,      kw::BUILTIN_SQRTL,       LongDouble,        &[LongDouble],                    SQRT),
        entry(kw::FLOOR,      kw::BUILTIN_FLOOR,       Double,            &[Double],                        F::RoundToIntegral(R::Floor)),
        entry(kw::FLOORF,     kw::BUILTIN_FLOORF,      Float,             &[Float],                         F::RoundToIntegral(R::Floor)),
        entry(kw::CEIL,       kw::BUILTIN_CEIL,        Double,            &[Double],                        F::RoundToIntegral(R::Ceil)),
        entry(kw::CEILF,      kw::BUILTIN_CEILF,       Float,             &[Float],                         F::RoundToIntegral(R::Ceil)),
        entry(kw::TRUNC,      kw::BUILTIN_TRUNC,       Double,            &[Double],                        F::RoundToIntegral(R::Trunc)),
        entry(kw::TRUNCF,     kw::BUILTIN_TRUNCF,      Float,             &[Float],                         F::RoundToIntegral(R::Trunc)),
        entry(kw::ROUND,      kw::BUILTIN_ROUND,       Double,            &[Double],                        F::RoundToIntegral(R::Round)),
        entry(kw::ROUNDF,     kw::BUILTIN_ROUNDF,      Float,             &[Float],                         F::RoundToIntegral(R::Round)),
        entry(kw::RINT,       kw::BUILTIN_RINT,        Double,            &[Double],                        F::RoundToIntegral(R::Rint)),
        entry(kw::RINTF,      kw::BUILTIN_RINTF,       Float,             &[Float],                         F::RoundToIntegral(R::Rint)),
        entry(kw::NEARBYINT,  kw::BUILTIN_NEARBYINT,   Double,            &[Double],                        F::RoundToIntegral(R::NearbyInt)),
        entry(kw::NEARBYINTF, kw::BUILTIN_NEARBYINTF,  Float,             &[Float],                         F::RoundToIntegral(R::NearbyInt)),
        entry(kw::FMIN,       kw::BUILTIN_FMIN,        Double,            &[Double, Double],                F::FMin),
        entry(kw::FMINF,      kw::BUILTIN_FMINF,       Float,             &[Float, Float],                  F::FMin),
        entry(kw::FMAX,       kw::BUILTIN_FMAX,        Double,            &[Double, Double],                F::FMax),
        entry(kw::FMAXF,      kw::BUILTIN_FMAXF,       Float,             &[Float, Float],                  F::FMax),
        entry(kw::FMA,        kw::BUILTIN_FMA,         Double,            &[Double, Double, Double],        F::Fma),
        entry(kw::FMAF,       kw::BUILTIN_FMAF,        Float,             &[Float, Float, Float],           F::Fma),
        entry(kw::CREAL,      kw::BUILTIN_CREAL,       Double,            &[ComplexDouble],                 F::ComplexReal),
        entry(kw::CREALF,     kw::BUILTIN_CREALF,      Float,             &[ComplexFloat],                  F::ComplexReal),
        entry(kw::CREALL,     kw::BUILTIN_CREALL,      LongDouble,        &[ComplexLongDouble],             F::ComplexReal),
        entry(kw::CIMAG,      kw::BUILTIN_CIMAG,       Double,            &[ComplexDouble],                 F::ComplexImag),
        entry(kw::CIMAGF,     kw::BUILTIN_CIMAGF,      Float,             &[ComplexFloat],                  F::ComplexImag),
        entry(kw::CIMAGL,     kw::BUILTIN_CIMAGL,      LongDouble,        &[ComplexLongDouble],             F::ComplexImag),
        entry(kw::CONJ,       kw::BUILTIN_CONJ,        ComplexDouble,     &[ComplexDouble],                 F::Conjugate),
        entry(kw::CONJF,      kw::BUILTIN_CONJF,       ComplexFloat,      &[ComplexFloat],                  F::Conjugate),
        entry(kw::CONJL,      kw::BUILTIN_CONJL,       ComplexLongDouble, &[ComplexLongDouble],             F::Conjugate),
        entry(kw::MEMCPY,     kw::BUILTIN_MEMCPY,      VoidPtr,           &[VoidPtr, ConstVoidPtr, SizeT],  F::Memory(M::Copy)),
        entry(kw::MEMSET,     kw::BUILTIN_MEMSET,      VoidPtr,           &[VoidPtr, Int, SizeT],           F::Memory(M::Set)),
        entry(kw::MEMMOVE,    kw::BUILTIN_MEMMOVE,     VoidPtr,           &[VoidPtr, ConstVoidPtr, SizeT],  F::Memory(M::Move)),
    ]
};

impl LibraryBuiltin {
    /// The library builtin `name_id` spells, and which spelling it is.
    fn lookup(name_id: StringId) -> Option<(&'static LibraryBuiltin, Spelling)> {
        LIBRARY_BUILTINS.iter().find_map(|lb| {
            if lb.bare == name_id {
                Some((lb, Spelling::Bare))
            } else if lb.reserved == name_id {
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

    /// See [`InlineLibraryFn::yields_to_a_definition`].
    pub(super) fn yields_to_a_definition(&self) -> bool {
        self.func.yields_to_a_definition()
    }

    /// The `float` function this `double` one narrows to for a `float`
    /// argument ([`InlineLibraryFn::narrows_exactly`]): `floorf` for
    /// `floor`. The condition is on the argument's type, not the result's --
    /// `double q(float a) { return floor(a); }` narrows too, because the
    /// narrowing happens before the widening.
    fn float_form(&self) -> Option<&'static LibraryBuiltin> {
        if !self.func.narrows_exactly() || self.ret != ProtoType::Double {
            return None;
        }
        LIBRARY_BUILTINS
            .iter()
            .find(|lb| lb.func == self.func && lb.ret == ProtoType::Float)
    }
}

impl Parser<'_> {
    /// Whether the function type `typ` declared for `lb`'s bare name is
    /// compatible with the library prototype.
    ///
    /// gcc's rule: a declaration with an incompatible type ("conflicting types
    /// for built-in function") makes the name an ordinary function, while the
    /// compatible one `<stdlib.h>`, `<math.h>` or `<string.h>` writes keeps
    /// the builtin. An unprototyped declaration says nothing about the
    /// parameters, so only its return type is compared; a prototype must have
    /// exactly the library's parameters, whose qualifiers do not count (C17
    /// 6.7.6.3p15 -- which `types_compatible` already ignores at the top
    /// level, so a `restrict` is no obstacle).
    pub(super) fn library_prototype_matches(&self, lb: &LibraryBuiltin, typ: TypeId) -> bool {
        let ret = lb.ret.id(self.types);
        let decl = self.types.get(typ);
        let ret_ok = decl
            .base
            .is_some_and(|base| self.types.types_compatible(base, ret));
        let params_ok = match &decl.params {
            None => true,
            Some(params) => {
                !decl.variadic
                    && params.len() == lb.params.len()
                    && params
                        .iter()
                        .zip(lb.params)
                        .all(|(&p, want)| self.types.types_compatible(p, want.id(self.types)))
            }
        };
        ret_ok && params_ok
    }

    /// `lb`'s parameter types.
    fn library_params(&self, lb: &LibraryBuiltin) -> Vec<TypeId> {
        lb.params.iter().map(|p| p.id(self.types)).collect()
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
        Some(self.parse_checked_library_call(lb, spelling, pos))
    }

    /// The argument list of a call to `lb`, checked as an ordinary call to a
    /// function of its prototype is checked, then lowered.
    fn parse_checked_library_call(
        &mut self,
        lb: &LibraryBuiltin,
        spelling: Spelling,
        pos: Position,
    ) -> ParseResult<Expr> {
        let call_pos = self.current_pos();
        self.expect_special(b'(')?;
        let args = self.parse_argument_list()?;
        self.expect_special(b')')?;

        let ret = lb.ret.id(self.types);
        let params = self.library_params(lb);
        let func_type = self.types.intern(Type::function(ret, params, false, false));
        let sound = self.check_call(Some(func_type), &args, call_pos);
        if sound && args.len() == lb.params.len() {
            Ok(self.lower_library_call(lb, spelling, args, pos))
        } else {
            // Diagnosed already. A zero of the return type stands in for the
            // call, so the enclosing expression still parses and types, and
            // no conversion is asked of an argument that has none.
            let zero = Self::typed_expr(ExprKind::IntLit(0), self.types.int_id, pos);
            Ok(self.convert_operand(zero, ret))
        }
    }

    /// Evaluate a checked call to `lb` of `args`, one for each of its
    /// parameters, as the table says to -- and as the command line allows
    /// (see [`LibraryCallPolicy`]).
    fn lower_library_call(
        &mut self,
        lb: &LibraryBuiltin,
        spelling: Spelling,
        args: Vec<Expr>,
        pos: Position,
    ) -> Expr {
        let ret = lb.ret.id(self.types);
        if let (Some(narrow), [arg]) = (lb.float_form(), args.as_slice()) {
            if self.is_binary32(arg) {
                // Computed at `float`, and the exact answer widened back: the
                // call is still a `double` (C17 6.5.2.2p5).
                let value = self.lower_library_call(narrow, spelling, args, pos);
                return self.convert_operand(value, ret);
            }
        }
        let params = self.library_params(lb);
        let func = self.library_call_policy.applied_to(lb.func);
        let mut args: Vec<Expr> = args
            .into_iter()
            .zip(&params)
            .map(|(arg, &param)| self.convert_operand(arg, param))
            .collect();
        if let (InlineLibraryFn::Memory(_), Some(n)) = (func, args.last_mut()) {
            self.fold_constant_length(n);
        }
        if !self.library_call_policy.in_place(func, spelling) {
            return self.libm_call(lb.bare, ret, &params, args, pos);
        }
        let name = lb.bare;
        Self::typed_expr(ExprKind::InlineLibraryCall { func, args, name }, ret, pos)
    }

    /// A block memory function's length, folded to a `size_t` literal when
    /// it is a constant, conversion and all, so that the IR receives the
    /// constant itself: at `-O0` nothing folds a conversion later, and
    /// `ir::memexpand` expands only a length it can see is one.
    fn fold_constant_length(&self, n: &mut Expr) {
        if let (Some(v), Some(typ)) = (self.eval_const_expr(n), n.typ) {
            // The bits of a `size_t`, as an unsigned literal carries them.
            *n = Self::typed_expr(ExprKind::IntLit(v as i64), typ, n.pos);
        }
    }

    /// Whether `e` is a value in the IEEE single format.
    fn is_binary32(&self, e: &Expr) -> bool {
        e.typ
            .and_then(|t| self.types.fp_format(t))
            .is_some_and(|fmt| fmt == crate::float::FpFormat::Binary32)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Each entry declares as many parameters as its function consumes
    /// arguments: the call is checked against the one and lowered by the
    /// other.
    #[test]
    fn every_entry_takes_what_its_lowering_consumes() {
        for lb in LIBRARY_BUILTINS {
            assert_eq!(lb.params.len(), lb.func.arity(), "{lb:?}");
        }
    }

    /// Every `double` function that narrows has its `float` form in the
    /// table, taking a `float`; nothing else narrows.
    #[test]
    fn every_narrowing_function_has_its_float_form() {
        for lb in LIBRARY_BUILTINS {
            let narrow = lb.float_form();
            let should = lb.func.narrows_exactly() && lb.ret == ProtoType::Double;
            assert_eq!(narrow.is_some(), should, "{lb:?}");
            if let Some(n) = narrow {
                assert_eq!(n.params, &[ProtoType::Float], "{lb:?}");
            }
        }
        let floor = LibraryBuiltin::by_bare_name(kw::FLOOR).unwrap();
        assert_eq!(floor.float_form().map(|n| n.bare), Some(kw::FLOORF));
    }
}
