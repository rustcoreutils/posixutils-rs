//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Expression constraint checking: call arguments, assignment compatibility
// and lvalue requirements
//

use super::ast::{BinaryOp, Expr, ExprKind, UnaryOp};
use super::operand_rule::{
    binary_operand_verdict, Operand, OperandClass, OperandVerdict, UnaryOperator,
};
use super::parser::Parser;
use crate::diag;
use crate::strings::StringId;
use crate::symbol::SymbolKind;
use crate::token::lexer::Position;
use crate::types::{AssignFault, TypeId, TypeKind, TypeModifiers};
use gettextrs::gettext;

/// Where a value is converted as if by assignment (C17 6.5.16.1). The
/// constraints are the same in each; only the wording of a fault differs.
#[derive(Clone, Copy, PartialEq, Eq)]
enum ConversionSite {
    /// `=` (6.5.16.1) and the store-back of a compound assignment.
    Assignment,
    /// The initializer of a scalar (6.7.9p11).
    Initialization,
    /// A `return` statement's value (6.8.6.4p3).
    Return,
}

impl Parser<'_> {
    /// Check a call's arguments against the function type it calls: their
    /// number, then each one's type (C17 6.5.2.2p2).
    ///
    /// `func_type` is the called function's type, or `None` when the callee
    /// has none -- an object that is not a function, already diagnosed by
    /// `check_callable`. Every call is checked here: the ordinary postfix
    /// call, a builtin that stands for a library function, and a
    /// `__builtin_` alias of one, so each is told exactly what the others are.
    ///
    /// Done at parse time rather than in the linearizer: the same `TypeId` is
    /// available here, but the positions are far better — `call_pos` and every
    /// argument's own position are live, whereas by linearization the only
    /// position left points at whichever sub-expression was lowered last.
    ///
    /// `callee` is the name the call was spelled with, when it was spelled
    /// with one: an argument diagnostic names it, as gcc's does.
    ///
    /// Answers whether the call is sound: `false` once either check reported
    /// an error, so a caller that goes on to compute the call in place knows
    /// not to convert an argument that cannot be converted.
    pub(super) fn check_call(
        &mut self,
        func_type: Option<TypeId>,
        callee: Option<StringId>,
        args: &[Expr],
        call_pos: Position,
    ) -> bool {
        let arity_ok = self.check_call_arity(func_type, callee, args, call_pos);
        let types_ok = self.check_argument_types(func_type, callee, args);
        arity_ok && types_ok
    }

    /// The name a call through `callee` is spelled with: a function or a
    /// pointer to one called by its identifier.
    pub(super) fn callee_name(&self, callee: &Expr) -> Option<StringId> {
        match callee.kind {
            ExprKind::Ident(symbol) => Some(self.symbols.get(symbol).name),
            _ => None,
        }
    }

    /// The number of arguments against the prototype. `false` when that was
    /// reported as an error.
    fn check_call_arity(
        &self,
        func_type: Option<TypeId>,
        callee: Option<StringId>,
        args: &[Expr],
        call_pos: Position,
    ) -> bool {
        let Some(func_type) = func_type else {
            return true;
        };

        // `params == None` means no prototype is visible: `int f();`, a K&R
        // identifier list -- neither of which C17 6.5.2.2p1 permits a check
        // against -- or an undeclared callee, which already produced its own
        // diagnostic and carries a dummy `int` type.
        let ft = self.types.get(func_type);
        let Some(params) = ft.params.as_ref() else {
            return true;
        };
        let max = (!ft.variadic).then_some(params.len());
        self.check_argument_count(callee, args.len(), params.len(), max, call_pos)
    }

    /// Whether `given` arguments are at least `min` and at most `max` (no
    /// limit when `None`), reporting the call to `callee` in gcc's words
    /// when they are not. Every call's count is judged here: an ordinary
    /// call's against its prototype, and a builtin's against the counts it
    /// takes.
    pub(super) fn check_argument_count(
        &self,
        callee: Option<StringId>,
        given: usize,
        min: usize,
        max: Option<usize>,
        call_pos: Position,
    ) -> bool {
        let fewer = given < min;
        if !fewer && max.is_none_or(|max| given <= max) {
            return true;
        }
        let callee = callee.and_then(|id| self.idents.get_opt(id));
        match (fewer, callee) {
            (true, Some(f)) => {
                diag::error_args(call_pos, "too few arguments to function '{0}'", &[f])
            }
            (true, None) => diag::error(call_pos, &gettext("too few arguments to function")),
            (false, Some(f)) => {
                diag::error_args(call_pos, "too many arguments to function '{0}'", &[f])
            }
            (false, None) => diag::error(call_pos, &gettext("too many arguments to function")),
        }
        false
    }

    /// Check each argument against its parameter's type. `false` when any
    /// was reported as an error.
    ///
    /// C17 6.5.2.2p2 requires an argument to be assignable to its parameter,
    /// so the constraints are the assignment ones and this asks the same
    /// question -- only the wording differs, naming the position gcc names.
    ///
    /// Arguments past a prototype's fixed parameters get the default argument
    /// promotions instead (p7), and an unprototyped callee has nothing to
    /// check against, so both are skipped.
    fn check_argument_types(
        &mut self,
        func_type: Option<TypeId>,
        callee: Option<StringId>,
        args: &[Expr],
    ) -> bool {
        let mut sound = true;
        for arg in args {
            // An argument is a value (C17 6.5.2.2p4), which a void
            // expression is not -- under a prototype or not.
            // `__builtin_va_arg_pack()` stands for the caller's arguments.
            if !matches!(arg.kind, ExprKind::VaArgPack)
                && arg
                    .typ
                    .is_some_and(|t| self.types.kind(t) == TypeKind::Void)
            {
                diag::error(arg.pos, &gettext("invalid use of void expression"));
                sound = false;
            }
        }
        let Some(func_type) = func_type else {
            return sound;
        };
        let Some(params) = self.types.get(func_type).params.clone() else {
            return sound;
        };
        for (i, (arg, &param)) in args.iter().zip(params.iter()).enumerate() {
            let (Some(a), param) = (arg.typ, param) else {
                continue;
            };
            let a = self.decayed_type(a);
            let param = self.decayed_type(param);
            // C17 6.5.2.2p4 assigns the argument to the parameter, which
            // needs an object of the parameter's type; a prototype may name an
            // incomplete one, but a call cannot be made through it.
            if self.type_name_is_incomplete(param) && self.types.kind(param) != TypeKind::Void {
                let n = (i + 1).to_string();
                diag::error_args(arg.pos, "type of formal parameter {0} is incomplete", &[&n]);
                sound = false;
                continue;
            }
            let Some(fault) =
                self.types
                    .assignment_fault(param, a, self.is_null_pointer_constant(arg))
            else {
                continue;
            };
            // glibc declares the socket calls with a union parameter carrying
            // __attribute__((transparent_union)), which lets a caller pass any
            // one of its member types -- `sendto(..., SAS2SA(&addr), ...)`
            // hands a `struct sockaddr *` to a `__CONST_SOCKADDR_ARG`.
            //
            // The attribute is now recorded, so this asks about the union in
            // hand rather than waving every union parameter through. Assignment
            // and `return` stay strict: the attribute governs calls only.
            if self.argument_matches_union_member(param, a) {
                continue;
            }
            let n = (i + 1).to_string();
            let callee = callee.and_then(|id| self.idents.get_opt(id));
            if fault == AssignFault::FunctionPointerVoid {
                match callee {
                    Some(f) => diag::pedwarn_args(
                        arg.pos,
                        "ISO C forbids passing argument {0} of '{1}' between function pointer and 'void *'",
                        &[&n, f],
                    ),
                    None => diag::pedwarn_args(
                        arg.pos,
                        "ISO C forbids passing argument {0} between function pointer and 'void *'",
                        &[&n],
                    ),
                }
                continue;
            }
            let (p_name, a_name) = (
                self.types.format_type(param, Some(self.idents)),
                self.types.format_type(a, Some(self.idents)),
            );
            Self::report_argument_fault(fault, arg.pos, &n, callee, &p_name, &a_name);
            if fault.is_error() {
                sound = false;
            }
        }
        sound
    }

    /// Report argument `n`, of type `a_name`, that `fault` keeps from being
    /// assigned to its parameter of type `p_name`. The callee is named when
    /// the call spelled one, as gcc names it.
    fn report_argument_fault(
        fault: AssignFault,
        pos: Position,
        n: &str,
        callee: Option<&str>,
        p_name: &str,
        a_name: &str,
    ) {
        match (fault.is_error(), callee) {
            (true, Some(f)) => diag::error_args(
                pos,
                "incompatible type for argument {0} of '{1}': expected '{2}', got '{3}'",
                &[n, f, p_name, a_name],
            ),
            (true, None) => diag::error_args(
                pos,
                "incompatible type for argument {0}: expected '{1}', got '{2}'",
                &[n, p_name, a_name],
            ),
            (false, Some(f)) => diag::pedwarn_default_args(
                pos,
                "passing argument {0} of '{1}' as '{2}' from '{3}' {4}",
                &[n, f, p_name, a_name, fault.describe()],
            ),
            (false, None) => diag::pedwarn_default_args(
                pos,
                "passing argument {0} as '{1}' from '{2}' {3}",
                &[n, p_name, a_name, fault.describe()],
            ),
        }
    }

    /// C17 6.5.3.2p2: the operand of unary `*` shall have pointer type. A
    /// *function designator* is deliberately allowed: `(*f)()` and even
    /// `(***f)()` are ordinary idioms gcc accepts, `*f` on a function being a
    /// no-op, and the result type computed just above already models that.
    pub(super) fn check_dereferenceable(&self, operand: &Expr, pos: Position) {
        let Some(typ) = operand.typ else {
            return;
        };
        if matches!(
            self.types.kind(typ),
            TypeKind::Pointer | TypeKind::Array | TypeKind::Function
        ) {
            return;
        }
        let named = self.types.format_type(typ, Some(self.idents));
        diag::error_args(
            pos,
            "invalid type argument of unary '*' (have '{0}')",
            &[&named],
        );
    }

    /// C17 6.5.2.2p1: the expression before `(` shall be a function, or a
    /// pointer to one. A pointer to a function is the ordinary spelling and a
    /// pointer to a pointer to one is reached through `*`, so both are
    /// accepted.
    pub(super) fn check_callable(&self, callee: &Expr, pos: Position) {
        let Some(typ) = callee.typ else {
            return;
        };
        let is_callable = match self.types.kind(typ) {
            TypeKind::Function => true,
            TypeKind::Pointer => self
                .types
                .base_type(typ)
                .is_some_and(|t| self.types.kind(t) == TypeKind::Function),
            _ => false,
        };
        if is_callable {
            return;
        }
        let named = self.types.format_type(typ, Some(self.idents));
        diag::error_args(
            pos,
            "called object is not a function or function pointer: '{0}'",
            &[&named],
        );
    }

    pub(super) fn check_subscript(&self, base: &Expr, index: &Expr, pos: Position) {
        let (Some(b), Some(i)) = (base.typ, index.typ) else {
            return;
        };
        let points = |t: TypeId| matches!(self.types.kind(t), TypeKind::Pointer | TypeKind::Array);
        let integral = |t: TypeId| self.types.is_integer(t);
        // Symmetric, because `a[i]` is defined as `*(a + i)`: `0[arr]` is
        // legal C. But exactly one side may be the pointer -- `p[q]` with two
        // pointers has nothing to scale by.
        if (points(b) && integral(i)) || (points(i) && integral(b)) {
            // 6.5.2.1p1 wants a pointer to a complete object type. gcc's
            // arithmetic on a pointer to a function stops short of indexing
            // one, and so does c17.
            let pointer = if points(b) { b } else { i };
            let to_function = self.types.kind(pointer) == TypeKind::Pointer
                && self
                    .types
                    .base_type(pointer)
                    .is_some_and(|t| self.types.kind(t) == TypeKind::Function);
            if to_function {
                diag::error(pos, &gettext("subscripted value is pointer to function"));
            } else if self.types.kind(pointer) == TypeKind::Pointer {
                self.check_pointer_steps(pointer, pos);
            }
            return;
        }
        if points(b) || points(i) {
            diag::error(pos, &gettext("array subscript is not an integer"));
        } else {
            diag::error(
                pos,
                &gettext("subscripted value is neither array nor pointer"),
            );
        }
    }

    /// Reject a `void` operand where a value is required (C17 6.5.6p2 for the
    /// binary operators, 6.5.15p3 for the conditional).
    ///
    /// gcc words this the same way for every such context, and the wording is
    /// the useful part: the problem is not the type but that there is no value
    /// at all.
    pub(super) fn check_not_void(&self, operand: &Expr, pos: Position) -> bool {
        let is_void = operand
            .typ
            .is_some_and(|t| self.types.kind(t) == TypeKind::Void);
        if is_void {
            diag::error(pos, &gettext("void value not ignored as it ought to be"));
        }
        is_void
    }

    /// Check the operand of a unary operator against the type its operator
    /// requires (C17 6.5.3.3p1, 6.5.2.4p1, 6.5.3.1p1).
    ///
    /// Answers whether the operand passed, so the caller can leave the
    /// result untyped after an error and spare the expressions around it a
    /// second report of the same mistake. A vector operand passes: the
    /// vector path has its own message. An operand with no type has been
    /// diagnosed already.
    pub(super) fn check_unary_operand(
        &mut self,
        op: UnaryOperator,
        operand: &Expr,
        pos: Position,
    ) -> bool {
        let Some(t) = operand.typ else { return true };
        if self.types.is_vector(t) {
            return self.check_vector_unary(op, t, pos);
        }
        if self.types.kind(t) == TypeKind::Void {
            diag::error(pos, &gettext("invalid use of void expression"));
            return false;
        }
        let t = self.decayed_type(t);
        if op.operand_class().admits(self.types, t) {
            let steps = matches!(op, UnaryOperator::Increment | UnaryOperator::Decrement);
            return !steps || self.check_pointer_steps(t, pos);
        }
        diag::error_args(pos, "wrong type argument to {0}", &[op.name()]);
        false
    }

    /// C17 6.5.6p2, 6.5.2.4p1: pointer arithmetic needs a pointer to a
    /// complete object type, whose size is the step. A pointer to an
    /// incomplete structure, union or enumeration was stepped by zero, and
    /// `p - q` divided by it. (`void *` and function pointers stay gcc's
    /// extension, stepping by one.) Answers whether `typ` may be stepped.
    pub(super) fn check_pointer_steps(&self, typ: TypeId, pos: Position) -> bool {
        if self.types.kind(typ) != TypeKind::Pointer {
            return true;
        }
        let Some(pointee) = self.types.base_type(typ) else {
            return true;
        };
        // An array of unknown size has no size to step by; a variable length
        // one does, at run time, and stepping over it is C. gcc words the
        // former its own way for a subscript and for `+`.
        if self.types.is_incomplete_array(pointee) {
            diag::error(
                pos,
                &gettext("invalid use of array with unspecified bounds"),
            );
            return false;
        }
        let incomplete = matches!(
            self.types.kind(pointee),
            TypeKind::Struct | TypeKind::Union | TypeKind::Enum
        ) && !self.types.is_composite_complete(pointee);
        if incomplete {
            diag::error(pos, &gettext("arithmetic on pointer to an incomplete type"));
        }
        !incomplete
    }

    /// Check an expression whose truth is tested -- the first operand of
    /// `?:` (C17 6.5.15p2), the left operand of `&&` and `||`, and the
    /// controlling expression of `if` and the loops (6.8.4.1p1, 6.8.5p2) --
    /// for the scalar type each requires. Worded as gcc words it.
    pub(super) fn check_truth_value(&mut self, cond: &Expr) -> bool {
        let Some(t) = cond.typ else { return true };
        if self.check_not_void(cond, cond.pos) || self.check_not_vector_operand(cond) {
            return false;
        }
        let t = self.decayed_type(t);
        if OperandClass::Scalar.admits(self.types, t) {
            return true;
        }
        let what = match self.types.kind(t) {
            TypeKind::Union => "union".to_string(),
            TypeKind::Struct => "struct".to_string(),
            _ => self.types.format_type(t, Some(self.idents)),
        };
        diag::error_args(
            cond.pos,
            "used {0} type value where scalar is required",
            &[&what],
        );
        false
    }

    /// Check a binary operator's operands (C17 6.5.5p2 through 6.5.14p2),
    /// and a compound assignment's (6.5.16.2p1-2) through the operator it
    /// applies.
    ///
    /// Answers whether the operation has a type: `false` after an error, so
    /// the caller leaves the result untyped and an enclosing assignment or
    /// operator does not report the same mistake again. gcc's warnings for
    /// a pointer compared with an integer or with an unrelated pointer leave
    /// the operation intact.
    pub(super) fn check_binary_operands(
        &mut self,
        op: BinaryOp,
        left: &Expr,
        right: &Expr,
        pos: Position,
    ) -> bool {
        // `&&` and `||` test their left operand for truth, as `if` would;
        // gcc reports that test and the right operand independently.
        let logical = matches!(op, BinaryOp::LogAnd | BinaryOp::LogOr);
        let left_ok = if logical {
            self.check_truth_value(left)
        } else {
            self.check_has_value(left)
        };
        let right_ok = if logical {
            !self.check_not_vector_operand(right)
        } else {
            true
        };
        if !(self.check_has_value(right) && left_ok && right_ok) {
            return false;
        }
        let (Some(lt), Some(rt)) = (left.typ, right.typ) else {
            return true;
        };
        if self.types.is_vector(lt) || self.types.is_vector(rt) {
            let types = (
                self.lvalue_converted_type(lt),
                self.lvalue_converted_type(rt),
            );
            return self.check_vector_operands(op, left, right, types, pos);
        }
        let (lt, rt) = (self.decayed_type(lt), self.decayed_type(rt));
        let l = self.binary_operand(op, left, lt, rt);
        let r = self.binary_operand(op, right, rt, lt);
        match binary_operand_verdict(self.types, op, l, r) {
            OperandVerdict::Valid if matches!(op, BinaryOp::Add | BinaryOp::Sub) => {
                self.check_pointer_steps(lt, pos) && self.check_pointer_steps(rt, pos)
            }
            OperandVerdict::Valid => true,
            OperandVerdict::Invalid => {
                // `&&` and `||` have tested the left operand for truth by
                // now, and gcc names it by the `int` that test yields.
                let lt = if logical { self.types.int_id } else { lt };
                self.report_invalid_operands(op, lt, rt, pos);
                false
            }
            OperandVerdict::PointerInteger => {
                diag::pedwarn_default(pos, &gettext("comparison between pointer and integer"));
                true
            }
            OperandVerdict::DistinctPointers => {
                diag::pedwarn_default(
                    pos,
                    &gettext("comparison of distinct pointer types lacks a cast"),
                );
                true
            }
            OperandVerdict::FunctionPointerVoid => {
                diag::pedwarn(
                    pos,
                    &gettext("ISO C forbids comparison of 'void *' with function pointer"),
                );
                true
            }
        }
    }

    /// C17 6.5.5-6.5.14 require operands with a value: `f() + 1`, where `f`
    /// returns void, has none.
    fn check_has_value(&self, operand: &Expr) -> bool {
        !self.check_not_void(operand, operand.pos)
    }

    /// Report a vector where a scalar is tested for truth -- a condition, or
    /// an operand of `&&` or `||` -- which gcc's C does not take.
    pub(super) fn check_not_vector_operand(&self, e: &Expr) -> bool {
        let is_vector = e.typ.is_some_and(|t| self.types.is_vector(t));
        if is_vector {
            diag::error(e.pos, &gettext("used vector type where scalar is required"));
        }
        is_vector
    }

    /// One operand of `op`, with its decayed type `typ`, beside an operand of
    /// type `other`. Whether it is a null pointer constant matters only to a
    /// comparison with a pointer, and costs a constant evaluation, so it is
    /// asked only then.
    fn binary_operand(&self, op: BinaryOp, e: &Expr, typ: TypeId, other: TypeId) -> Operand {
        let null_constant = op.is_comparison()
            && self.types.kind(other) == TypeKind::Pointer
            && self.is_null_pointer_constant(e);
        Operand { typ, null_constant }
    }

    /// gcc's "invalid operands to binary +", naming each operand by the type
    /// the operator would have seen: decayed, unqualified, and promoted.
    fn report_invalid_operands(
        &mut self,
        op: BinaryOp,
        left: TypeId,
        right: TypeId,
        pos: Position,
    ) {
        let names = [left, right].map(|t| {
            let t = self.operand_value_type(t);
            self.types.format_type(t, Some(self.idents))
        });
        diag::error_args(
            pos,
            "invalid operands to binary {0} (have '{1}' and '{2}')",
            &[op.spelling(), &names[0], &names[1]],
        );
    }

    /// The type of an operand's value as an operator computes with it: an
    /// integer narrower than `int`, or an enumeration, after the integer
    /// promotions (C17 6.3.1.1p2).
    fn operand_value_type(&mut self, typ: TypeId) -> TypeId {
        let t = self.types.unqualified(typ);
        if self.types.is_integer(t) && !self.types.is_complex(t) {
            self.types.integer_promote(t)
        } else {
            t
        }
    }

    /// Does this argument's type match some member of a
    /// `__attribute__((transparent_union))` parameter?
    ///
    /// C17 has no such rule; the attribute is a gcc extension that glibc's
    /// socket declarations depend on. An ordinary union parameter is checked
    /// like any other aggregate, which is what 6.5.2.2p2 requires.
    fn argument_matches_union_member(&self, param: TypeId, arg: TypeId) -> bool {
        if self.types.transparent_union_first_member(param).is_none() {
            return false;
        }
        let Some(comp) = self.types.composite(param) else {
            return false;
        };
        comp.members
            .iter()
            .any(|m| self.types.assignment_fault(m.typ, arg, false).is_none())
    }

    /// The function type a callee expression resolves to, through a function
    /// pointer if need be.
    pub(super) fn resolved_function_type(&self, callee: &Expr) -> Option<TypeId> {
        callee.typ.and_then(|t| self.types.callee_function_type(t))
    }

    /// Report a simple assignment whose value cannot be converted to the
    /// target's type (C17 6.5.16.1).
    ///
    /// Compound assignment is deliberately not routed here: 6.5.16.2 has its
    /// own, looser constraints, under which `p += 1` is ordinary pointer
    /// arithmetic rather than an integer assigned to a pointer.
    pub(super) fn check_assignment_types(&mut self, target: &Expr, value: &Expr, pos: Position) {
        let (Some(t), Some(v)) = (target.typ, value.typ) else {
            return;
        };
        let null_constant = self.is_null_pointer_constant(value);
        self.check_assignment_conversion(ConversionSite::Assignment, t, v, null_constant, pos);
    }

    /// Report a compound assignment whose operands the operator rejects
    /// (C17 6.5.16.2p1-2), or whose result cannot be stored back.
    ///
    /// `E1 op= E2` computes `E1 op E2` and assigns it, so it is checked as
    /// exactly that: the operator's own constraints first, then the
    /// conversion of the operation's result to the target's type -- which is
    /// how gcc finds `i += p` (an `int *` stored to an `int`) worth a warning
    /// while `p += 1` is ordinary pointer arithmetic.
    pub(super) fn check_compound_assignment(
        &mut self,
        op: BinaryOp,
        target: &Expr,
        value: &Expr,
        pos: Position,
    ) {
        if !self.check_binary_operands(op, target, value, pos) {
            return;
        }
        let Some(t) = target.typ else { return };
        let left = self.lvalue_converted_type(t);
        let result = self.binary_result_type(op, left, value);
        self.check_assignment_conversion(ConversionSite::Assignment, t, result, false, pos);
    }

    /// Check a `return` statement against the enclosing function's declared
    /// return type (C17 6.8.6.4). `pos` is the `return` keyword's.
    ///
    /// p1 forbids returning a *value* from a void function. An expression of
    /// type `void` has none, so `return f();` where `f` returns void -- the
    /// ordinary tail-call wrapper, which gcc and Clang both accept -- is not a
    /// violation. p3 converts a returned value "as if by assignment", so the
    /// simple-assignment constraints govern it, asked in exactly the same
    /// way as for `=`.
    ///
    /// A value-ness mismatch is an error, as the constraint makes it. gcc
    /// warns and compiles, and code that does this is old rather than clever
    /// -- so `-fpermissive`, which already relaxes implicit `int` and
    /// implicit function declarations for exactly that reason, relaxes these
    /// too. The value is discarded either way, and a missing one leaves the
    /// returned value indeterminate, which is what gcc's program does as well.
    pub(super) fn check_return(&mut self, value: Option<&Expr>, pos: Position) {
        let Some(declared) = self.enclosing_function.return_type else {
            return;
        };
        let returns_void = self.types.kind(declared) == TypeKind::Void;
        match value {
            None if !returns_void => diag::permissive_error(
                pos,
                &gettext("'return' with no value in a function returning non-void"),
            ),
            Some(e) if returns_void => {
                if e.typ.is_some_and(|t| self.types.kind(t) != TypeKind::Void) {
                    diag::permissive_error(
                        e.pos,
                        &gettext("'return' with a value in a function returning void"),
                    );
                }
            }
            Some(e) => {
                if let Some(v) = e.typ {
                    let null_constant = self.is_null_pointer_constant(e);
                    self.check_assignment_conversion(
                        ConversionSite::Return,
                        declared,
                        v,
                        null_constant,
                        e.pos,
                    );
                }
            }
            None => {}
        }
    }

    /// Report a value of type `v` that cannot be converted to the type `t`
    /// as if by assignment (C17 6.5.16.1p1), in the words `site` calls for.
    /// `null_constant` says whether the value is a null pointer constant.
    fn check_assignment_conversion(
        &mut self,
        site: ConversionSite,
        t: TypeId,
        v: TypeId,
        null_constant: bool,
        pos: Position,
    ) {
        let t = self.decayed_type(t);
        let v = self.decayed_type(v);
        let Some(fault) = self.types.assignment_fault(t, v, null_constant) else {
            return;
        };
        if fault == AssignFault::FunctionPointerVoid {
            let msg = match site {
                ConversionSite::Assignment => {
                    gettext("ISO C forbids assignment between function pointer and 'void *'")
                }
                ConversionSite::Initialization => {
                    gettext("ISO C forbids initialization between function pointer and 'void *'")
                }
                ConversionSite::Return => {
                    gettext("ISO C forbids return between function pointer and 'void *'")
                }
            };
            diag::pedwarn(pos, &msg);
            return;
        }
        if fault.is_error() {
            // An aggregate initialized from something that is not a
            // compatible aggregate is "invalid initializer" in gcc, not a
            // type mismatch -- and the wording is the useful part, being what
            // a user searches for. gcc says so even of a `void` value.
            if site == ConversionSite::Initialization
                && matches!(self.types.kind(t), TypeKind::Struct | TypeKind::Union)
            {
                diag::error(pos, &gettext("invalid initializer"));
                return;
            }
            // gcc words this one case differently, and it is the clearer
            // phrasing: the problem is not the types but that there is no
            // value at all.
            if self.types.kind(v) == TypeKind::Void {
                diag::error(pos, &gettext("void value not ignored as it ought to be"));
                return;
            }
        }
        let (t_name, v_name) = (
            self.types.format_type(t, Some(self.idents)),
            self.types.format_type(v, Some(self.idents)),
        );
        let template = match (site, fault.is_error()) {
            (ConversionSite::Assignment, true) => {
                "incompatible types when assigning to type '{0}' from type '{1}'"
            }
            (ConversionSite::Initialization, true) => {
                "incompatible types when initializing type '{0}' using type '{1}'"
            }
            (ConversionSite::Return, true) => {
                "incompatible types when returning type '{1}' but '{0}' was expected"
            }
            (ConversionSite::Assignment, false) => "assignment to '{0}' from '{1}' {2}",
            (ConversionSite::Initialization, false) => "initialization of '{0}' from '{1}' {2}",
            (ConversionSite::Return, false) => {
                "returning '{1}' from a function with return type '{0}' {2}"
            }
        };
        let args: &[&str] = &[&t_name, &v_name, fault.describe()];
        if fault.is_error() {
            diag::error_args(pos, template, args);
        } else {
            diag::pedwarn_default_args(pos, template, args);
        }
    }

    /// C17 6.7.9p11: the initializer for a scalar shall satisfy the
    /// constraints of simple assignment; p13 and p14 confine an aggregate to a
    /// brace-enclosed list, or a string literal for a character array.
    ///
    /// The severities come from `AssignFault::is_error`, so they are gcc's
    /// without a table to maintain: incompatible types are an error, the
    /// pointer/integer conversions a warning.
    pub(crate) fn check_initializer_types(&mut self, target: TypeId, init: &Expr) {
        // A brace-enclosed list has its own rules, checked elsewhere.
        if matches!(init.kind, ExprKind::InitList { .. }) {
            return;
        }
        let Some(v) = init.typ else {
            return;
        };

        // 6.7.9p14/p15: an array may be initialized by a string literal, with
        // or without braces, but only one whose element type it matches. Any
        // other array needs a list. A vector takes a vector value.
        if self.types.kind(target) == TypeKind::Array && !self.types.is_vector(target) {
            // A compound literal of the same array type initializes an array,
            // which gcc accepts and `ast_init_to_ir` already lowers -- only
            // this check stood in the way, having been written when a string
            // literal was the one non-braced initializer an array could take.
            if matches!(init.kind, ExprKind::CompoundLiteral { .. })
                && init
                    .typ
                    .is_some_and(|t| self.types.types_compatible(t, target))
            {
                return;
            }
            if !self.string_literal_suits_array(target, init) {
                diag::error(init.pos, &gettext("invalid initializer"));
            } else {
                self.check_string_fits_array(target, init);
            }
            return;
        }

        let null_constant = self.is_null_pointer_constant(init);
        self.check_assignment_conversion(
            ConversionSite::Initialization,
            target,
            v,
            null_constant,
            init.pos,
        );
    }

    /// C11 6.5.2.3p5: naming a member of an atomic structure or union is
    /// undefined behaviour -- the access reads or writes part of an object
    /// whose atomicity covers the whole of it, so the lock the type promises
    /// is not taken.
    ///
    /// A warning rather than a rejection, because the standard makes it
    /// undefined rather than a constraint violation, and gcc warns. c17 said
    /// nothing at all, so the one operation `_Atomic` exists to prevent was
    /// the one it did not mention.
    ///
    /// The atomicity of the *object* is what counts, not the member's:
    /// `struct { _Atomic int a; } s; s.a` is an ordinary access to an atomic
    /// member and is silent, while `_Atomic struct S s; s.a` is not.
    pub(super) fn warn_atomic_member_access(
        &self,
        object: TypeId,
        member: StringId,
        pos: Position,
    ) {
        if !self.types.is_atomic(object) {
            return;
        }
        let what = match self.types.kind(object) {
            TypeKind::Struct => "structure",
            TypeKind::Union => "union",
            _ => return,
        };
        let name = self
            .idents
            .get_opt(member)
            .unwrap_or("<unknown>")
            .to_string();
        diag::warning_args(
            pos,
            "accessing a member '{0}' of an atomic {1}",
            &[&name, what],
        );
    }

    /// May this string literal initialize this array (C17 6.7.9p14, p15)?
    ///
    /// p14 gives a *character* array the narrow literal -- and "character
    /// type" is all three of `char`, `signed char` and `unsigned char`, which
    /// `TypeKind::Char` covers because signedness is a modifier. `u8"..."` is
    /// narrow too: 6.4.5p6 gives it type `char[]`.
    ///
    /// p15 is stricter. A wide literal needs an element type *compatible* with
    /// its own, so `int a[] = L"ab";` is legal on a target where `wchar_t` is
    /// `int` while `unsigned a[] = L"ab";` is not -- a distinction gcc makes
    /// and one that "is it a character type?" cannot. Comparing the kind and
    /// the signedness gets it exactly, and both are unaffected by a qualifier,
    /// so `const char a[] = "hi";` still passes.
    ///
    /// Returns false for a non-string initializer too: an array has no other
    /// unbraced form.
    /// C17 6.7.9p2, p14: a string literal may fill an array exactly -- its
    /// terminating null then has no room, and is dropped -- but may not
    /// overrun it. gcc warns and truncates, and so does c17.
    fn check_string_fits_array(&self, target: TypeId, init: &Expr) {
        let Some(capacity) = self.types.array_extent(target).known().filter(|&n| n > 0) else {
            return;
        };
        let units = match &init.kind {
            ExprKind::StringLit(bytes) => bytes.chars().count(),
            ExprKind::WideStringLit(units) | ExprKind::Utf32StringLit(units) => units.len(),
            ExprKind::Utf16StringLit(units) => units.len(),
            _ => return,
        };
        if units > capacity {
            let elem = self.types.base_type(target).unwrap_or(self.types.char_id);
            let named = self.types.format_type(elem, Some(self.idents));
            diag::pedwarn_default_args(
                init.pos,
                "initializer-string for array of '{0}' is too long",
                &[&named],
            );
        }
    }

    fn string_literal_suits_array(&mut self, target: TypeId, init: &Expr) -> bool {
        let narrow = matches!(init.kind, ExprKind::StringLit(_));
        let wide = matches!(
            init.kind,
            ExprKind::WideStringLit(_) | ExprKind::Utf16StringLit(_) | ExprKind::Utf32StringLit(_)
        );
        let Some(elem) = self.types.base_type(target) else {
            return false;
        };

        if narrow {
            return self.types.kind(elem) == TypeKind::Char;
        }
        if !wide {
            return false;
        }
        // The literal's own element type -- `int` for `L""`, `unsigned short`
        // for `u""`, `unsigned int` for `U""` -- is already on its type.
        let Some(lit_elem) = init.typ.and_then(|t| self.types.base_type(t)) else {
            return false;
        };
        self.types.kind(elem) == self.types.kind(lit_elem)
            && self.types.is_unsigned(elem) == self.types.is_unsigned(lit_elem)
    }

    /// Is this expression a null pointer constant (C17 6.3.2.3p3) -- an
    /// integer constant expression with the value 0, or such an expression
    /// cast to `void *`?
    ///
    /// Only a cast to `void *` itself qualifies: `(char *)0` is a null
    /// *pointer* of type `char *`, which converts to an `int *` no more than
    /// any other `char *` does, and `(const void *)0` keeps its qualifier.
    pub(super) fn is_null_pointer_constant(&self, expr: &Expr) -> bool {
        let inner = match &expr.kind {
            ExprKind::Cast {
                cast_type,
                expr: inner,
            } if self.is_unqualified_void_pointer(*cast_type) => inner,
            _ => expr,
        };
        self.types
            .is_integer(inner.typ.unwrap_or(self.types.int_id))
            && self.eval_const_expr(inner) == Some(0)
            && !self.evaluates_a_pointer(inner)
    }

    /// Is `typ` exactly `void *`, the one type a null pointer constant may
    /// be cast to and remain one?
    fn is_unqualified_void_pointer(&self, typ: TypeId) -> bool {
        self.types.kind(typ) == TypeKind::Pointer
            && self.types.base_type(typ).is_some_and(|base| {
                self.types.kind(base) == TypeKind::Void && self.types.qualifiers(base).is_empty()
            })
    }

    /// Does evaluating `expr` involve a pointer, an array or a function?
    ///
    /// An integer constant expression may cast only arithmetic types to
    /// integer types (C17 6.6p6), so `(int)(char *)0` folds to zero but is no
    /// integer constant expression, and so no null pointer constant. The
    /// operand of `sizeof`, `_Alignof`, `__builtin_constant_p` and
    /// `__builtin_object_size` is not evaluated, and may be anything.
    fn evaluates_a_pointer(&self, expr: &Expr) -> bool {
        if matches!(
            expr.kind,
            ExprKind::SizeofExpr(_)
                | ExprKind::AlignofExpr(_)
                | ExprKind::ConstantP(_)
                | ExprKind::ObjectSize { .. }
        ) {
            return false;
        }
        expr.typ.is_some_and(|t| {
            matches!(
                self.types.kind(t),
                TypeKind::Pointer | TypeKind::Array | TypeKind::Function
            )
        }) || expr
            .operands()
            .into_iter()
            .any(|e| self.evaluates_a_pointer(e))
    }

    /// Does this expression designate an object (C17 6.3.2.1p1)?
    ///
    /// Only the shapes that can name storage: an object identifier, a
    /// dereference, a subscript, a member of an lvalue, a member through a
    /// pointer, a compound literal, or a string literal. Everything else --
    /// the result of arithmetic, a call, a cast, a conditional, a comma -- is
    /// a value, not a place.
    pub(super) fn is_lvalue(&self, expr: &Expr) -> bool {
        match &expr.kind {
            ExprKind::Ident(symbol_id) => {
                // A function designator and an enum constant are not objects.
                !matches!(
                    self.symbols.get(*symbol_id).kind,
                    SymbolKind::Function | SymbolKind::EnumConstant
                )
            }
            ExprKind::Unary { op, operand } => match op {
                UnaryOp::Deref => true,
                // `__real__ x` and `__imag__ x` are lvalues exactly when their
                // operand is, which is what gcc documents.
                UnaryOp::Real | UnaryOp::Imag => self.is_lvalue(operand),
                _ => false,
            },
            ExprKind::Index { .. } | ExprKind::Arrow { .. } => true,
            // `s.x` designates an object only when `s` does: `f().x` is a
            // member of a returned value, and has nowhere to live.
            ExprKind::Member { expr, .. } => self.is_lvalue(expr),
            // `__func__` is a `static const char` array (C17 6.4.2.2p1).
            ExprKind::CompoundLiteral { .. }
            | ExprKind::FuncName
            | ExprKind::StringLit(_)
            | ExprKind::WideStringLit(_)
            | ExprKind::Utf16StringLit(_)
            | ExprKind::Utf32StringLit(_) => true,
            // A GNU statement expression is an lvalue exactly when the
            // expression it ends with is one, which is what gcc documents.
            ExprKind::StmtExpr { result, .. } => self.is_lvalue(result),
            // A compound literal stays one when its type-name carried
            // extents.
            ExprKind::VmTypeName { expr, .. } => self.is_lvalue(expr),
            _ => false,
        }
    }

    /// The member `expr` designates when it is a bit-field: `s.m` or `p->m`
    /// where `m` was declared with a width.
    pub(crate) fn bit_field_designated(&self, expr: &Expr) -> Option<StringId> {
        let (aggregate, member) = match &expr.kind {
            ExprKind::Member { expr, member } => (expr.typ?, *member),
            // A pointer's pointee, or an array's element: the array decays.
            ExprKind::Arrow { expr, member } => (self.types.base_type(expr.typ?)?, *member),
            _ => return None,
        };
        self.types
            .find_member(aggregate, member)
            .and_then(|m| m.bit_width)
            .map(|_| member)
    }

    /// Report an operand of unary `&` that has no address (C17 6.5.3.2p1):
    /// one that is neither a function designator nor an lvalue, such as
    /// `&(i + 1)` or `&creal(z)` -- a call's result is a value, even when the
    /// call is evaluated in place.
    ///
    /// `register` is the other case, and the one that bites: the storage
    /// class is a hint the compiler may ignore, but taking the address is
    /// still a constraint violation, and a program that does it is relying on
    /// the hint being ignored.
    pub(super) fn check_addressable(&self, operand: &Expr, pos: Position) {
        let designates_function = operand
            .typ
            .is_some_and(|t| self.types.kind(t) == TypeKind::Function);
        if !designates_function && !self.is_lvalue(operand) {
            diag::error_args(pos, "lvalue required as {0}", &["unary '&' operand"]);
            return;
        }
        // C17 6.5.3.2p1: never a bit-field, which has no address.
        if let Some(member) = self.bit_field_designated(operand) {
            diag::error_args(
                pos,
                "cannot take address of bit-field '{0}'",
                &[self.str(member)],
            );
            return;
        }
        if self.check_reverse_order_address(operand, pos) {
            return;
        }
        // The address of a member is the address of the object it is in, so
        // `&s.a` of a `register` structure asks for the register's.
        let mut root = operand;
        while let ExprKind::Member { expr, .. } = &root.kind {
            root = expr;
        }
        let ExprKind::Ident(symbol_id) = &root.kind else {
            return;
        };
        let sym = self.symbols.get(*symbol_id);
        if !matches!(sym.kind, SymbolKind::Variable | SymbolKind::Parameter) {
            return;
        }
        if self
            .types
            .modifiers(sym.typ)
            .contains(TypeModifiers::REGISTER)
        {
            let name = self.str(sym.name).to_string();
            diag::error_args(
                pos,
                "address of register variable '{0}' requested",
                &[&name],
            );
        }
    }

    /// gcc's restrictions on the address of an object stored in reverse
    /// byte order (`scalar_storage_order`), which a pointer type cannot
    /// carry: a scalar's address is an error, and an array of them draws a
    /// warning, since gcc lets one be taken for a block copy. A struct or
    /// union's own address is fine. True when the address was refused.
    fn check_reverse_order_address(&self, operand: &Expr, pos: Position) -> bool {
        let Some(typ) = operand.typ else {
            return false;
        };
        if self.types.reverses_storage(typ) {
            diag::error(
                pos,
                &gettext("cannot take address of scalar with reverse storage order"),
            );
            return true;
        }
        let is_array = self.types.kind(typ) == TypeKind::Array && !self.types.is_vector(typ);
        if is_array
            && self
                .types
                .reverses_storage(self.types.innermost_element(typ))
        {
            diag::group_warning(
                "scalar-storage-order",
                pos,
                &gettext("address of array with reverse scalar storage order requested"),
            );
        }
        false
    }

    /// Is `expr` a member access naming an `_Atomic` scalar stored in reverse
    /// byte order?
    fn is_reverse_atomic_member(&self, expr: &Expr) -> bool {
        matches!(expr.kind, ExprKind::Member { .. } | ExprKind::Arrow { .. })
            && expr
                .typ
                .is_some_and(|t| self.types.is_atomic(t) && self.types.reverses_storage(t))
    }

    /// Record a member access just built, if it names an `_Atomic` scalar of
    /// a `scalar_storage_order` structure stored in reverse order.
    ///
    /// Every access to such an object is an atomic operation on its address,
    /// which a reversed scalar does not have, so gcc refuses each one: a read,
    /// a write, a compound assignment, `++` and `--` -- even in an operand
    /// that is never evaluated, `sizeof (s.a + 1)`. What it allows is the
    /// member as the *whole* operand of `sizeof`, `_Alignof`, `typeof` or a
    /// `_Generic` controlling expression, which read nothing, and in an
    /// association or `__builtin_choose_expr` arm that is not selected.
    ///
    /// Postfix parsing cannot know which of these it is in, so the access is
    /// held here until those operators have had the chance to clear it
    /// ([`Self::exempt_reverse_atomic_operand`],
    /// [`Self::take_reverse_atomic_members`]), and whatever remains is
    /// reported by [`Self::report_reverse_atomic_members`].
    ///
    /// An element of an `_Atomic` array member is not held: gcc reads and
    /// writes it as an ordinary reversed scalar.
    pub(super) fn note_reverse_atomic_member(&mut self, expr: &Expr) {
        if self.is_reverse_atomic_member(expr) {
            self.reverse_atomic_members.push(expr.pos);
        }
    }

    /// `operand` is the whole operand of an operator that does not access
    /// it; if it is a member [`Self::note_reverse_atomic_member`] held, it
    /// was the last one held, and is no access after all.
    pub(super) fn exempt_reverse_atomic_operand(&mut self, operand: &Expr) {
        if self.is_reverse_atomic_member(operand)
            && self.reverse_atomic_members.last() == Some(&operand.pos)
        {
            self.reverse_atomic_members.pop();
        }
    }

    /// How many members are held, to pass to
    /// [`Self::take_reverse_atomic_members`] once an operand is parsed.
    pub(super) fn reverse_atomic_mark(&self) -> usize {
        self.reverse_atomic_members.len()
    }

    /// The members held since `mark`, removed: an operand that is not
    /// evaluated, or one parsed and then abandoned, gives them up, and a
    /// `_Generic` association puts its own back once it is selected.
    pub(super) fn take_reverse_atomic_members(&mut self, mark: usize) -> Vec<Position> {
        let mark = mark.min(self.reverse_atomic_members.len());
        self.reverse_atomic_members.split_off(mark)
    }

    /// Report every member access still held: each one reads or writes the
    /// object. A tentative parse that was rewound may have held one twice.
    pub(super) fn report_reverse_atomic_members(&mut self) {
        let mut held = std::mem::take(&mut self.reverse_atomic_members);
        held.dedup();
        for pos in held {
            diag::error(
                pos,
                &gettext("cannot take address of scalar with reverse storage order"),
            );
        }
    }

    /// Report a target that cannot be assigned to or stepped (C17 6.5.16p2,
    /// 6.5.3.1p1). `verb` names the operator for the message, matching what
    /// gcc says so that the two agree on the wording users search for.
    pub(super) fn check_modifiable_lvalue(&self, target: &Expr, verb: &str, pos: Position) {
        if !self.is_lvalue(target) {
            diag::error_args(pos, "lvalue required as {0}", &[verb]);
            return;
        }
        // An array is an lvalue but never a modifiable one: it has no
        // assignment operator, only its elements do. A vector has one.
        if let Some(typ) = target.typ {
            if self.types.kind(typ) == TypeKind::Array && !self.types.is_vector(typ) {
                diag::error(pos, &gettext("assignment to expression with array type"));
            }
        }
    }

    pub(super) fn check_const_assignment(&self, target: &Expr, pos: Position) {
        // Check for assignment through pointer to const first: *p where p is const T*
        if let ExprKind::Unary {
            op: UnaryOp::Deref,
            operand,
        } = &target.kind
        {
            if let Some(ptr_type_id) = operand.typ {
                if let Some(base_type_id) = self.types.base_type(ptr_type_id) {
                    if self
                        .types
                        .modifiers(base_type_id)
                        .contains(TypeModifiers::CONST)
                    {
                        diag::error(pos, &gettext("assignment of read-only location"));
                        return; // Don't duplicate with the general const check
                    }
                }
            }
        }

        // Check if target type has CONST modifier (direct const variable)
        if let Some(typ_id) = target.typ {
            if self.types.modifiers(typ_id).contains(TypeModifiers::CONST) {
                // Get variable name if it's an identifier
                let var_name = match &target.kind {
                    ExprKind::Ident(symbol_id) => {
                        let name = self.symbols.get(*symbol_id).name;
                        format!(" '{}'", self.str(name))
                    }
                    _ => String::new(),
                };
                diag::error_args(
                    pos,
                    "assignment of read-only variable{0}",
                    &[&var_name.to_string()],
                );
                return;
            }
            // C17 6.3.2.1p1: a structure or union with a `const` member,
            // at any depth, is not a modifiable lvalue as a whole -- the
            // assignment would write the member.
            if self.has_read_only_member(typ_id) {
                diag::error(
                    pos,
                    &gettext("assignment of a structure or union with a read-only member"),
                );
            }
        }
    }

    /// Whether a structure or union has a `const`-qualified member,
    /// directly, inside an array member, or inside a nested aggregate.
    fn has_read_only_member(&self, typ: TypeId) -> bool {
        if !matches!(self.types.kind(typ), TypeKind::Struct | TypeKind::Union) {
            return false;
        }
        let Some(composite) = self.types.composite(typ) else {
            return false;
        };
        composite.members.iter().any(|m| {
            let mut t = m.typ;
            while self.types.kind(t) == TypeKind::Array {
                match self.types.base_type(t) {
                    Some(elem) => t = elem,
                    None => break,
                }
            }
            self.types.modifiers(t).contains(TypeModifiers::CONST) || self.has_read_only_member(t)
        })
    }
}
