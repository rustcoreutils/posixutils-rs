//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The builtins gcc checks by rules of their own
//
// The classification builtins, `__builtin_complex`, checked arithmetic,
// `__builtin_object_size` and `__builtin_prefetch` take arguments no
// ordinary prototype describes: any floating type, any integer type, an
// integer constant in a range. gcc checks each by a rule of its own, and so
// does c17 -- in gcc's words, with the shared rules of `builtin_args.rs`.
// Where gcc does give a prototype (`__builtin_isnanf`, `__builtin_sadd_overflow`,
// the pointer of `__builtin_object_size`), the call is checked through it.
//

use super::ast::{CheckedOp, Expr, ExprKind, FpTest};
use super::builtin_args::{BuiltinArgs, ConstantArgument};
use super::library_builtin::ProtoType;
use super::parser::{ParseResult, Parser};
use crate::diag;
use crate::kw;
use crate::strings::StringId;
use crate::token::lexer::Position;
use crate::types::{Type, TypeId, TypeKind, TypeModifiers};
use gettextrs::gettext;

/// A classification builtin: the test, and the parameter type gcc declares
/// for a suffixed spelling. The others are type-generic: any real floating
/// argument, tested at its own type.
#[rustfmt::skip]
const FP_TESTS: &[(StringId, FpTest, Option<ProtoType>)] = {
    use FpTest::*;
    use ProtoType::{Float, LongDouble};
    &[
        (kw::BUILTIN_ISNAN,       IsNan,     None),
        (kw::BUILTIN_ISNANF,      IsNan,     Some(Float)),
        (kw::BUILTIN_ISNANL,      IsNan,     Some(LongDouble)),
        (kw::BUILTIN_ISINF,       IsInf,     None),
        (kw::BUILTIN_ISINFF,      IsInf,     Some(Float)),
        (kw::BUILTIN_ISINFL,      IsInf,     Some(LongDouble)),
        (kw::BUILTIN_ISINF_SIGN,  IsInfSign, None),
        (kw::BUILTIN_ISFINITE,    IsFinite,  None),
        (kw::BUILTIN_ISNORMAL,    IsNormal,  None),
        (kw::BUILTIN_SIGNBIT,     SignBit,   None),
        (kw::BUILTIN_SIGNBITF,    SignBit,   Some(Float)),
        (kw::BUILTIN_SIGNBITL,    SignBit,   Some(LongDouble)),
    ]
};

/// How a checked-arithmetic builtin names its result.
#[derive(Clone, Copy)]
enum CheckedForm {
    /// `__builtin_add_overflow(a, b, &r)`: any integers, stored through a
    /// pointer to any integer.
    Store,
    /// `__builtin_add_overflow_p(a, b, (T)0)`: the result type named by a
    /// value, nothing stored.
    Predicate,
    /// `__builtin_sadd_overflow(a, b, &r)`: gcc's prototype, every operand
    /// of this type.
    Typed(ProtoType),
}

/// Every checked-arithmetic builtin.
#[rustfmt::skip]
const CHECKED_ARITH: &[(StringId, CheckedOp, CheckedForm)] = {
    use kw::*;
    use CheckedForm::*;
    use CheckedOp::*;
    use ProtoType::*;
    &[
        (BUILTIN_ADD_OVERFLOW,    Add, Store),
        (BUILTIN_SUB_OVERFLOW,    Sub, Store),
        (BUILTIN_MUL_OVERFLOW,    Mul, Store),
        (BUILTIN_ADD_OVERFLOW_P,  Add, Predicate),
        (BUILTIN_SUB_OVERFLOW_P,  Sub, Predicate),
        (BUILTIN_MUL_OVERFLOW_P,  Mul, Predicate),
        (BUILTIN_SADD_OVERFLOW,   Add, Typed(Int)),
        (BUILTIN_SADDL_OVERFLOW,  Add, Typed(Long)),
        (BUILTIN_SADDLL_OVERFLOW, Add, Typed(LongLong)),
        (BUILTIN_UADD_OVERFLOW,   Add, Typed(UInt)),
        (BUILTIN_UADDL_OVERFLOW,  Add, Typed(ULong)),
        (BUILTIN_UADDLL_OVERFLOW, Add, Typed(ULongLong)),
        (BUILTIN_SSUB_OVERFLOW,   Sub, Typed(Int)),
        (BUILTIN_SSUBL_OVERFLOW,  Sub, Typed(Long)),
        (BUILTIN_SSUBLL_OVERFLOW, Sub, Typed(LongLong)),
        (BUILTIN_USUB_OVERFLOW,   Sub, Typed(UInt)),
        (BUILTIN_USUBL_OVERFLOW,  Sub, Typed(ULong)),
        (BUILTIN_USUBLL_OVERFLOW, Sub, Typed(ULongLong)),
        (BUILTIN_SMUL_OVERFLOW,   Mul, Typed(Int)),
        (BUILTIN_SMULL_OVERFLOW,  Mul, Typed(Long)),
        (BUILTIN_SMULLL_OVERFLOW, Mul, Typed(LongLong)),
        (BUILTIN_UMUL_OVERFLOW,   Mul, Typed(UInt)),
        (BUILTIN_UMULL_OVERFLOW,  Mul, Typed(ULong)),
        (BUILTIN_UMULLL_OVERFLOW, Mul, Typed(ULongLong)),
    ]
};

/// `__builtin_prefetch`'s hints after the address: the values each may
/// take, and gcc's words for one that is not a constant (an error) and one
/// out of range (a warning; zero is used).
const PREFETCH_HINTS: [(i128, &str, &str); 2] = [
    (
        1,
        "second argument to '__builtin_prefetch' must be a constant",
        "invalid second argument to '__builtin_prefetch'; using zero",
    ),
    (
        3,
        "third argument to '__builtin_prefetch' must be a constant",
        "invalid third argument to '__builtin_prefetch'; using zero",
    ),
];

impl Parser<'_> {
    /// A call to one of the builtins this module checks, if `name_id`
    /// spells one.
    pub(super) fn parse_generic_builtin(
        &mut self,
        name_id: StringId,
        pos: Position,
    ) -> Option<ParseResult<Expr>> {
        if let Some(call) = self.parse_vector_builtin(name_id, pos) {
            return Some(call);
        }
        if let Some(&(_, test, param)) = FP_TESTS.iter().find(|row| row.0 == name_id) {
            return Some(self.parse_fp_test(name_id, test, param, pos));
        }
        if let Some(&(_, op, form)) = CHECKED_ARITH.iter().find(|row| row.0 == name_id) {
            return Some(self.parse_checked_arith(name_id, op, form, pos));
        }
        match name_id {
            kw::BUILTIN_FPCLASSIFY => Some(self.parse_fpclassify(pos)),
            kw::BUILTIN_COMPLEX => Some(self.parse_builtin_complex(pos)),
            kw::BUILTIN_OBJECT_SIZE | kw::BUILTIN_DYNAMIC_OBJECT_SIZE => {
                Some(self.parse_object_size(name_id, pos))
            }
            kw::BUILTIN_PREFETCH => Some(self.parse_prefetch(pos)),
            _ => None,
        }
    }

    /// A classification builtin. The `f` and `l` spellings convert their
    /// argument to their parameter type; the others test any real floating
    /// argument at its own type, and reject anything else.
    fn parse_fp_test(
        &mut self,
        name: StringId,
        test: FpTest,
        param: Option<ProtoType>,
        pos: Position,
    ) -> ParseResult<Expr> {
        let int = self.types.int_id;
        let arg = match param {
            Some(p) => self.parse_prototyped_builtin(name, ProtoType::Int, &[p], false)?,
            None => self
                .parse_generic_builtin_args(name, 1, 1)?
                .filter(|args| self.require_floating_argument(&args[0], name)),
        };
        let Some(arg) = arg.and_then(|args| args.into_iter().next()) else {
            return Ok(self.diagnosed_call(int, pos));
        };
        if test == FpTest::SignBit && param.is_none() {
            return Ok(self.type_generic_signbit(arg, pos));
        }
        let test = ExprKind::FpTest {
            test,
            arg: Box::new(arg),
        };
        Ok(Self::typed_expr(test, int, pos))
    }

    /// `__builtin_signbit`, which gcc makes type-generic: glibc's `signbit`
    /// macro hands it every floating type, not only `double`.
    ///
    /// The sign is read at the argument's own width. The two types c17 has
    /// no bit test for are converted on the way, which only ever widens or
    /// keeps the sign: a `_Float16` to `float`, a `__float128` to
    /// `long double`.
    ///
    /// C17 7.12.3.6 asks only for nonzero when the sign is set, and gcc's own
    /// answer is not one value: 1 for a constant, and at run time the bit in
    /// place -- `INT_MIN` for a `float`, 512 for an x87 `long double`. 1 at
    /// every width and level is the one of its answers that is consistent.
    fn type_generic_signbit(&mut self, arg: Expr, pos: Position) -> Expr {
        let typ = match arg.typ.map(|t| self.types.kind(t)) {
            Some(TypeKind::LongDouble | TypeKind::Float128) => self.types.longdouble_id,
            Some(TypeKind::Float | TypeKind::Float16) => self.types.float_id,
            _ => self.types.double_id,
        };
        let arg = self.convert_operand(arg, typ);
        let test = ExprKind::FpTest {
            test: FpTest::SignBit,
            arg: Box::new(arg),
        };
        Self::typed_expr(test, self.types.int_id, pos)
    }

    /// `__builtin_fpclassify(nan, inf, normal, subnormal, zero, x)`.
    ///
    /// gcc declares it `int (int, int, int, int, int, ...)`: the five class
    /// codes convert to `int`, and must then be constants; `x` is any real
    /// floating value.
    fn parse_fpclassify(&mut self, pos: Position) -> ParseResult<Expr> {
        let name = kw::BUILTIN_FPCLASSIFY;
        let int = self.types.int_id;
        let BuiltinArgs { args, call_pos } = self.parse_builtin_call_args()?;
        let args = if self.check_argument_count(Some(name), args.len(), 6, Some(6), call_pos) {
            let func_type = self.builtin_prototype(ProtoType::Int, &[ProtoType::Int; 5], true);
            self.check_prototyped_builtin(name, func_type, args, call_pos)
        } else {
            None
        };
        let Some(mut classes) = args.filter(|args| self.fpclassify_args_valid(args)) else {
            return Ok(self.diagnosed_call(int, pos));
        };
        let arg = classes.pop().expect("six arguments, counted");
        let classify = ExprKind::FpClassify {
            classes,
            arg: Box::new(arg),
        };
        Ok(Self::typed_expr(classify, int, pos))
    }

    /// gcc's rules for `__builtin_fpclassify`'s six converted arguments.
    fn fpclassify_args_valid(&self, args: &[Expr]) -> bool {
        let name = kw::BUILTIN_FPCLASSIFY;
        let (value, classes) = args.split_last().expect("six arguments, counted");
        for (i, class) in classes.iter().enumerate() {
            if self.constant_argument(class, i128::MIN..=i128::MAX) == ConstantArgument::NotConstant
            {
                diag::error_args(
                    class.pos,
                    "non-const integer argument {0} in call to function '{1}'",
                    &[&(i + 1).to_string(), "__builtin_fpclassify"],
                );
                return false;
            }
        }
        self.require_floating_argument(value, name)
    }

    /// `__builtin_complex(re, im)`: a complex value of the operands' type,
    /// which must be one real floating type.
    fn parse_builtin_complex(&mut self, pos: Position) -> ParseResult<Expr> {
        let args = self.parse_generic_builtin_args(kw::BUILTIN_COMPLEX, 2, 2)?;
        let Some([real, imag]) = args
            .filter(|args| self.complex_operands_valid(&args[0], &args[1]))
            .and_then(|args| <[Expr; 2]>::try_from(args).ok())
        else {
            let complex = self.types.complex_double_id;
            return Ok(self.diagnosed_call(complex, pos));
        };
        let real_typ = real.typ.unwrap_or(self.types.double_id);
        let real_typ = self.types.unqualified(real_typ);
        let complex_typ = self.types.make_complex(real_typ);
        let complex = ExprKind::BuiltinComplex {
            real: Box::new(real),
            imag: Box::new(imag),
        };
        Ok(Self::typed_expr(complex, complex_typ, pos))
    }

    /// gcc's rules for `__builtin_complex`: each operand real floating, and
    /// both of one type, qualifiers aside.
    fn complex_operands_valid(&mut self, real: &Expr, imag: &Expr) -> bool {
        let floating = |e: &Expr| e.typ.is_none_or(|t| self.types.is_float(t));
        if !(floating(real) && floating(imag)) {
            let msg = "'__builtin_complex' operand not of real binary floating-point type";
            diag::error(real.pos, &gettext(msg));
            return false;
        }
        let (Some(r), Some(i)) = (real.typ, imag.typ) else {
            return true;
        };
        if self.types.unqualified(r) == self.types.unqualified(i) {
            return true;
        }
        let msg = "'__builtin_complex' operands of different types";
        diag::error(real.pos, &gettext(msg));
        false
    }

    /// A checked-arithmetic builtin: compute exactly, store (or not) the
    /// wrapped result, and answer whether wrapping lost anything.
    fn parse_checked_arith(
        &mut self,
        name: StringId,
        op: CheckedOp,
        form: CheckedForm,
        pos: Position,
    ) -> ParseResult<Expr> {
        let int = self.types.int_id;
        let args = match form {
            CheckedForm::Typed(operand) => self.parse_typed_checked_args(name, operand)?,
            CheckedForm::Store | CheckedForm::Predicate => self
                .parse_generic_builtin_args(name, 3, 3)?
                .filter(|args| self.checked_args_valid(name, args, form)),
        };
        let Some([a, b, res]) = args.and_then(|args| <[Expr; 3]>::try_from(args).ok()) else {
            return Ok(self.diagnosed_call(int, pos));
        };
        // The `_p` form's result is evaluated, as gcc evaluates it: only its
        // value is unused.
        let checked = ExprKind::CheckedArith {
            op,
            a: Box::new(a),
            b: Box::new(b),
            res: Box::new(res),
            store: !matches!(form, CheckedForm::Predicate),
        };
        Ok(Self::typed_expr(checked, int, pos))
    }

    /// The arguments of a typed checked-arithmetic builtin, through gcc's
    /// prototype `bool (T, T, T *)`.
    fn parse_typed_checked_args(
        &mut self,
        name: StringId,
        operand: ProtoType,
    ) -> ParseResult<Option<Vec<Expr>>> {
        let BuiltinArgs { args, call_pos } = self.parse_builtin_call_args()?;
        let operand = operand.id(self.types);
        let result = self.types.intern(Type::pointer(operand));
        let int = self.types.int_id;
        let params = vec![operand, operand, result];
        let func_type = self.types.intern(Type::function(int, params, false, false));
        Ok(self.check_prototyped_builtin(name, func_type, args, call_pos))
    }

    /// gcc's rules for the type-generic checked arithmetic: integral
    /// operands, and a result that is, or points to, an integer that is
    /// neither `_Bool` nor an enumeration -- and, stored through, not
    /// `const`. The first argument at fault is reported.
    fn checked_args_valid(&mut self, name: StringId, args: &[Expr], form: CheckedForm) -> bool {
        let callee = self.idents.get_opt(name).unwrap_or("").to_string();
        for (n, arg) in args[..2].iter().enumerate() {
            if arg.typ.is_some_and(|t| !self.is_integral(t)) {
                diag::error_args(
                    arg.pos,
                    "argument {0} in call to function '{1}' does not have integral type",
                    &[&(n + 1).to_string(), &callee],
                );
                return false;
            }
        }
        let Some(typ) = args[2].typ else {
            return true;
        };
        let fault = match form {
            CheckedForm::Predicate => self.checked_predicate_fault(typ),
            _ => self.checked_result_fault(typ),
        };
        let Some(msg) = fault else {
            return true;
        };
        let typ = self.decayed_type(typ);
        let type_name = self.types.format_type(typ, Some(self.idents));
        diag::error_args(args[2].pos, msg, &[&callee, &type_name]);
        false
    }

    /// What is wrong with `typ` as the pointer a checked-arithmetic result
    /// is stored through, in gcc's words, if anything.
    fn checked_result_fault(&mut self, typ: TypeId) -> Option<&'static str> {
        let typ = self.decayed_type(typ);
        let pointee = (self.types.kind(typ) == TypeKind::Pointer)
            .then(|| self.types.base_type(typ))
            .flatten()
            .filter(|&t| self.is_integral(t));
        let Some(pointee) = pointee else {
            return Some(
                "argument 3 in call to function '{0}' does not have pointer to integral type",
            );
        };
        match self.types.kind(pointee) {
            TypeKind::Bool => {
                Some("argument 3 in call to function '{0}' has pointer to boolean type")
            }
            TypeKind::Enum => {
                Some("argument 3 in call to function '{0}' has pointer to enumerated type")
            }
            _ if self.types.modifiers(pointee).contains(TypeModifiers::CONST) => {
                Some("argument 3 in call to function '{0}' has pointer to 'const' type ('{1}')")
            }
            _ => None,
        }
    }

    /// What is wrong with `typ` as the type a `_p` checked-arithmetic
    /// builtin computes in, in gcc's words, if anything.
    fn checked_predicate_fault(&self, typ: TypeId) -> Option<&'static str> {
        if !self.is_integral(typ) {
            return Some("argument 3 in call to function '{0}' does not have integral type");
        }
        match self.types.kind(typ) {
            TypeKind::Bool => Some("argument 3 in call to function '{0}' has boolean type"),
            TypeKind::Enum => Some("argument 3 in call to function '{0}' has enumerated type"),
            _ => None,
        }
    }

    /// `__builtin_object_size(ptr, type)`: how many bytes remain in the
    /// object `ptr` points into, when that is known statically.
    ///
    /// `__builtin_dynamic_object_size` takes the same arguments and may also
    /// answer with a size known only at run time. Every answer the static
    /// form gives is a correct one for it -- run-time sizes are a refinement
    /// gcc offers, not a different meaning -- so it is the same builtin.
    ///
    /// gcc declares it `size_t (const void *, int)`; the type must then be
    /// an integer constant from 0 to 3. Bit 0 selects the closest
    /// surrounding subobject over the whole object; bit 1 asks for a minimum
    /// rather than a maximum.
    fn parse_object_size(&mut self, name: StringId, pos: Position) -> ParseResult<Expr> {
        use ProtoType::{ConstVoidPtr, Int, SizeT};
        let size_t = self.types.ulong_id;
        let args = self.parse_prototyped_builtin(name, SizeT, &[ConstVoidPtr, Int], false)?;
        let Some([ptr, otype]) = args.and_then(|args| <[Expr; 2]>::try_from(args).ok()) else {
            return Ok(self.diagnosed_call(size_t, pos));
        };
        let ConstantArgument::InRange(otype) = self.constant_argument(&otype, 0..=3) else {
            diag::error_args(
                otype.pos,
                "last argument of '{0}' is not integer constant between 0 and 3",
                &["__builtin_object_size"],
            );
            return Ok(self.diagnosed_call(size_t, pos));
        };
        let size = match self.object_extent(&ptr) {
            // A statically known object has the same minimum and maximum
            // size, so bit 1 does not change the answer.
            Some(extent) => extent.remaining(otype & 1 != 0),
            // Unknown: the documented answers are the ones that make a
            // `_FORTIFY_SOURCE` check pass rather than fire, which is the
            // largest value for a maximum and zero for a minimum.
            None if otype & 2 != 0 => 0,
            None => u64::MAX,
        };
        Ok(Self::typed_expr(ExprKind::IntLit(size as i64), size_t, pos))
    }

    /// `__builtin_prefetch(addr[, rw[, locality]])`, which gcc declares
    /// `void (const void *, ...)`.
    ///
    /// The prefetch itself emits nothing, but its address is still an
    /// expression and C evaluates it: `__builtin_prefetch((q = p))` assigns
    /// `q`. The hints are integer constants, so there is nothing in them to
    /// evaluate; one out of range is a warning, and zero is used.
    fn parse_prefetch(&mut self, pos: Position) -> ParseResult<Expr> {
        use ProtoType::{ConstVoidPtr, Void};
        let name = kw::BUILTIN_PREFETCH;
        let void = self.types.void_id;
        let args = self
            .parse_prototyped_builtin(name, Void, &[ConstVoidPtr], true)?
            .filter(|args| self.prefetch_hints_valid(&args[1..]));
        let Some(addr) = args.and_then(|args| args.into_iter().next()) else {
            return Ok(self.diagnosed_call(void, pos));
        };
        // A comma expression carries the address along and yields the void
        // result, which is what the builtin's type says.
        let void_result = Self::typed_expr(ExprKind::IntLit(0), void, pos);
        Ok(Self::typed_expr(
            ExprKind::Comma(vec![addr, void_result]),
            void,
            pos,
        ))
    }

    /// Whether `hints`, the arguments after a prefetch's address, are the
    /// constants gcc requires. Any past the locality are not looked at, as
    /// gcc does not look at them.
    fn prefetch_hints_valid(&self, hints: &[Expr]) -> bool {
        let mut valid = true;
        for (hint, &(max, not_constant, out_of_range)) in hints.iter().zip(&PREFETCH_HINTS) {
            match self.constant_argument(hint, 0..=max) {
                ConstantArgument::InRange(_) => {}
                ConstantArgument::OutOfRange(_) => {
                    diag::warning(hint.pos, &gettext(out_of_range));
                }
                ConstantArgument::NotConstant => {
                    diag::error(hint.pos, &gettext(not_constant));
                    valid = false;
                }
            }
        }
        valid
    }
}
