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

/// A block memory function of `<string.h>`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MemoryFn {
    /// `void *memcpy(void *restrict, const void *restrict, size_t)`
    Copy,
    /// `void *memset(void *, int, size_t)`
    Set,
    /// `void *memmove(void *, const void *, size_t)`
    Move,
}

/// How a call to a library builtin is evaluated, once its arguments have
/// been checked.
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
    /// A block memory function, as its [`ExprKind`] node. Whether it becomes
    /// a call or loads and stores is decided on the IR (`ir::memexpand`),
    /// where a length that only inlining makes constant can still be seen.
    Memory(MemoryFn),
}

impl Lowering {
    /// Whether this translation unit's own definition of the name displaces
    /// the builtin.
    ///
    /// gcc's answer differs by function, and this follows it. A `memcpy`
    /// defined here is the one called, at every level -- which is what
    /// glibc's fortify headers depend on: an `always_inline` `gnu_inline`
    /// `memcpy` wrapper that checks the object size before it copies. An
    /// `abs` defined here is folded past all the same (gcc.c-torture
    /// `execute/20021127-1`), since defining a reserved library name is
    /// undefined (C17 7.1.3p2) and the value cannot differ.
    fn displaced_by_definition(self) -> bool {
        matches!(self, Lowering::Memory(_))
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
    /// The `__builtin_` spelling, when this table is what that spelling
    /// means. The floor family's reserved spellings are plain library
    /// aliases instead (see `parse_library_builtin`).
    reserved: Option<StringId>,
    ret: ProtoType,
    params: &'static [ProtoType],
    lowering: Lowering,
}

const fn entry(
    bare: StringId,
    reserved: Option<StringId>,
    ret: ProtoType,
    params: &'static [ProtoType],
    lowering: Lowering,
) -> LibraryBuiltin {
    LibraryBuiltin {
        bare,
        reserved,
        ret,
        params,
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
    use Lowering::{InPlace, Memory, NarrowingCall};
    use MemoryFn as M;
    use ProtoType::*;
    &[
        //    bare           reserved                     returns            parameters                        lowering
        entry(kw::ABS,       Some(kw::BUILTIN_ABS),       Int,               &[Int],                           InPlace(F::IntAbs)),
        entry(kw::LABS,      Some(kw::BUILTIN_LABS),      Long,              &[Long],                          InPlace(F::IntAbs)),
        entry(kw::LLABS,     Some(kw::BUILTIN_LLABS),     LongLong,          &[LongLong],                      InPlace(F::IntAbs)),
        entry(kw::IMAXABS,   Some(kw::BUILTIN_IMAXABS),   Long,              &[Long],                          InPlace(F::IntAbs)),
        entry(kw::FABS,      Some(kw::BUILTIN_FABS),      Double,            &[Double],                        InPlace(F::Fabs)),
        entry(kw::FABSF,     Some(kw::BUILTIN_FABSF),     Float,             &[Float],                         InPlace(F::Fabs)),
        entry(kw::FABSL,     Some(kw::BUILTIN_FABSL),     LongDouble,        &[LongDouble],                    InPlace(F::Fabs)),
        entry(kw::COPYSIGN,  Some(kw::BUILTIN_COPYSIGN),  Double,            &[Double, Double],                InPlace(F::CopySign)),
        entry(kw::COPYSIGNF, Some(kw::BUILTIN_COPYSIGNF), Float,             &[Float, Float],                  InPlace(F::CopySign)),
        entry(kw::COPYSIGNL, Some(kw::BUILTIN_COPYSIGNL), LongDouble,        &[LongDouble, LongDouble],        InPlace(F::CopySign)),
        entry(kw::FLOOR,     None,                        Double,            &[Double],                        NarrowingCall { narrow: "floorf" }),
        entry(kw::CEIL,      None,                        Double,            &[Double],                        NarrowingCall { narrow: "ceilf" }),
        entry(kw::TRUNC,     None,                        Double,            &[Double],                        NarrowingCall { narrow: "truncf" }),
        entry(kw::ROUND,     None,                        Double,            &[Double],                        NarrowingCall { narrow: "roundf" }),
        entry(kw::RINT,      None,                        Double,            &[Double],                        NarrowingCall { narrow: "rintf" }),
        entry(kw::NEARBYINT, None,                        Double,            &[Double],                        NarrowingCall { narrow: "nearbyintf" }),
        entry(kw::CREAL,     Some(kw::BUILTIN_CREAL),     Double,            &[ComplexDouble],                 InPlace(F::ComplexReal)),
        entry(kw::CREALF,    Some(kw::BUILTIN_CREALF),    Float,             &[ComplexFloat],                  InPlace(F::ComplexReal)),
        entry(kw::CREALL,    Some(kw::BUILTIN_CREALL),    LongDouble,        &[ComplexLongDouble],             InPlace(F::ComplexReal)),
        entry(kw::CIMAG,     Some(kw::BUILTIN_CIMAG),     Double,            &[ComplexDouble],                 InPlace(F::ComplexImag)),
        entry(kw::CIMAGF,    Some(kw::BUILTIN_CIMAGF),    Float,             &[ComplexFloat],                  InPlace(F::ComplexImag)),
        entry(kw::CIMAGL,    Some(kw::BUILTIN_CIMAGL),    LongDouble,        &[ComplexLongDouble],             InPlace(F::ComplexImag)),
        entry(kw::CONJ,      Some(kw::BUILTIN_CONJ),      ComplexDouble,     &[ComplexDouble],                 InPlace(F::Conjugate)),
        entry(kw::CONJF,     Some(kw::BUILTIN_CONJF),     ComplexFloat,      &[ComplexFloat],                  InPlace(F::Conjugate)),
        entry(kw::CONJL,     Some(kw::BUILTIN_CONJL),     ComplexLongDouble, &[ComplexLongDouble],             InPlace(F::Conjugate)),
        entry(kw::MEMCPY,    Some(kw::BUILTIN_MEMCPY),    VoidPtr,           &[VoidPtr, ConstVoidPtr, SizeT],  Memory(M::Copy)),
        entry(kw::MEMSET,    Some(kw::BUILTIN_MEMSET),    VoidPtr,           &[VoidPtr, Int, SizeT],           Memory(M::Set)),
        entry(kw::MEMMOVE,   Some(kw::BUILTIN_MEMMOVE),   VoidPtr,           &[VoidPtr, ConstVoidPtr, SizeT],  Memory(M::Move)),
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

    /// Whether a definition of the bare name in this translation unit
    /// displaces the builtin (see `Lowering::displaced_by_definition`).
    pub(super) fn displaced_by_definition(&self) -> bool {
        self.lowering.displaced_by_definition()
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

        let ret = lb.ret.id(self.types);
        let params = self.library_params(lb);
        let func_type = self.types.intern(Type::function(ret, params, false, false));
        let sound = self.check_call(Some(func_type), &args, call_pos);
        if sound && args.len() == lb.params.len() {
            Ok(self.lower_library_call(lb, args, pos))
        } else {
            // Diagnosed already. A zero of the return type stands in for the
            // call, so the enclosing expression still parses and types, and
            // no conversion is asked of an argument that has none.
            let zero = Self::typed_expr(ExprKind::IntLit(0), self.types.int_id, pos);
            Ok(self.convert_operand(zero, ret))
        }
    }

    /// Evaluate a checked call to `lb` of `args`, one for each of its
    /// parameters, as the table says to.
    fn lower_library_call(&mut self, lb: &LibraryBuiltin, args: Vec<Expr>, pos: Position) -> Expr {
        let ret = lb.ret.id(self.types);
        let params = self.library_params(lb);
        match lb.lowering {
            Lowering::InPlace(func) => {
                let args = args
                    .into_iter()
                    .zip(params)
                    .map(|(arg, param)| self.convert_operand(arg, param))
                    .collect();
                Self::typed_expr(ExprKind::InlineLibraryCall { func, args }, ret, pos)
            }
            Lowering::NarrowingCall { narrow } => {
                let (Ok([arg]), &[param]) = (<[Expr; 1]>::try_from(args), params.as_slice()) else {
                    unreachable!("the table gives a narrowing call one parameter");
                };
                if self.is_binary32(&arg) {
                    let f = self.types.float_id;
                    self.libm_call(narrow, f, &[f], arg, pos)
                } else {
                    let name = self.idents.get_opt(lb.bare).unwrap_or("");
                    let arg = self.convert_operand(arg, param);
                    self.libm_call(name, ret, &[param], arg, pos)
                }
            }
            Lowering::Memory(func) => {
                let Ok(args) = <[Expr; 3]>::try_from(args) else {
                    unreachable!("the table gives a memory function three parameters");
                };
                self.lower_memory_call(func, args, &params, ret, pos)
            }
        }
    }

    /// A checked call to a block memory function, each argument converted to
    /// its parameter's type as the prototype would.
    fn lower_memory_call(
        &mut self,
        func: MemoryFn,
        [dest, second, n]: [Expr; 3],
        params: &[TypeId],
        ret: TypeId,
        pos: Position,
    ) -> Expr {
        let dest = Box::new(self.convert_operand(dest, params[0]));
        let second = Box::new(self.convert_operand(second, params[1]));
        // A constant length is folded here, conversion and all, so the IR
        // receives the constant itself: at -O0 nothing folds a conversion
        // later, and `ir::memexpand` expands only a length it can see is one.
        let n = self.convert_operand(n, params[2]);
        let n = Box::new(match self.eval_const_expr(&n) {
            // The bits of a `size_t`, as an unsigned literal carries them.
            Some(v) => Self::typed_expr(ExprKind::IntLit(v as i64), params[2], n.pos),
            None => n,
        });
        let kind = match func {
            MemoryFn::Copy => ExprKind::Memcpy {
                dest,
                src: second,
                n,
            },
            MemoryFn::Set => ExprKind::Memset { dest, c: second, n },
            MemoryFn::Move => ExprKind::Memmove {
                dest,
                src: second,
                n,
            },
        };
        Self::typed_expr(kind, ret, pos)
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

    /// Each entry declares as many parameters as its lowering consumes
    /// arguments: the call is checked against the one and lowered by the
    /// other.
    #[test]
    fn every_entry_takes_what_its_lowering_consumes() {
        for lb in LIBRARY_BUILTINS {
            let want = match lb.lowering {
                Lowering::InPlace(func) => func.arity(),
                Lowering::NarrowingCall { .. } => 1,
                Lowering::Memory(_) => 3,
            };
            assert_eq!(lb.params.len(), want, "{lb:?}");
        }
    }
}
