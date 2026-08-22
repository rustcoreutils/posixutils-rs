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

use super::ast::{Expr, ExprKind, UnaryOp};
use super::parser::Parser;
use crate::diag;
use crate::strings::StringId;
use crate::symbol::SymbolKind;
use crate::token::lexer::Position;
use crate::types::{AssignFault, TypeId, TypeKind, TypeModifiers};
use gettextrs::gettext;

impl Parser<'_> {
    /// Check a call against the callee's prototype (C99 6.5.2.2p2).
    ///
    /// Done at parse time rather than in the linearizer: the same `TypeId` is
    /// available here, but the positions are far better — `call_pos` and every
    /// argument's own position are live, whereas by linearization the only
    /// position left points at whichever sub-expression was lowered last.
    pub(super) fn check_call_arity(&self, callee: &Expr, args: &[Expr], call_pos: Position) {
        // Resolve through a function pointer, as the return-type logic does.
        let Some(func_type) = self.resolved_function_type(callee) else {
            return;
        };

        // `params == None` means no prototype is visible: `int f();`, a K&R
        // identifier list -- neither of which C17 6.5.2.2p1 permits a check
        // against -- or an undeclared callee, which already produced its own
        // diagnostic and carries a dummy `int` type.
        let ft = self.types.get(func_type);
        let Some(params) = ft.params.as_ref() else {
            return;
        };
        let required = params.len();

        let variadic = ft.variadic;

        let wrong = if variadic {
            args.len() < required
        } else {
            args.len() != required
        };
        if !wrong {
            return;
        }

        let expected = if variadic {
            format!("at least {}", required)
        } else {
            required.to_string()
        };
        // The singular/plural split is baked into the English sentence, so no
        // amount of substitution fixes it from outside -- both forms have to be
        // msgids.
        diag::error_plural(
            call_pos,
            "call expects {0} argument, but {1} given",
            "call expects {0} arguments, but {1} given",
            required,
            &[&expected, &args.len().to_string()],
        );
    }

    /// Check each argument against its parameter's type.
    ///
    /// C17 6.5.2.2p2 requires an argument to be assignable to its parameter,
    /// so the constraints are the assignment ones and this asks the same
    /// question -- only the wording differs, naming the position gcc names.
    ///
    /// Arguments past a prototype's fixed parameters get the default argument
    /// promotions instead (p7), and an unprototyped callee has nothing to
    /// check against, so both are skipped.
    pub(super) fn check_argument_types(&mut self, callee: &Expr, args: &[Expr]) {
        let Some(func_type) = self.resolved_function_type(callee) else {
            return;
        };
        let Some(params) = self.types.get(func_type).params.clone() else {
            return;
        };
        for (i, (arg, &param)) in args.iter().zip(params.iter()).enumerate() {
            let (Some(a), param) = (arg.typ, param) else {
                continue;
            };
            let a = self.decayed_type(a);
            let param = self.decayed_type(param);
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
            if fault == AssignFault::FunctionPointerVoid {
                if diag::warning_group_enabled(crate::types::FUNCTION_POINTER_CONV) {
                    diag::warning_args(
                        arg.pos,
                        "ISO C forbids passing argument {0} between function pointer and 'void *'",
                        &[&n],
                    );
                }
                continue;
            }
            let (p_name, a_name) = (
                self.types.format_type(param, Some(self.idents)),
                self.types.format_type(a, Some(self.idents)),
            );
            if fault.is_error() {
                diag::error_args(
                    arg.pos,
                    "incompatible type for argument {0}: expected '{1}', got '{2}'",
                    &[&n, &p_name, &a_name],
                );
            } else {
                diag::warning_args(
                    arg.pos,
                    "passing argument {0} as '{1}' from '{2}' {3}",
                    &[&n, &p_name, &a_name, fault.describe()],
                );
            }
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
    fn resolved_function_type(&self, callee: &Expr) -> Option<TypeId> {
        callee.typ.and_then(|t| match self.types.kind(t) {
            TypeKind::Function => Some(t),
            TypeKind::Pointer => self
                .types
                .base_type(t)
                .filter(|&b| self.types.kind(b) == TypeKind::Function),
            _ => None,
        })
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
        let t = self.decayed_type(t);
        let v = self.decayed_type(v);
        let Some(fault) = self
            .types
            .assignment_fault(t, v, self.is_null_pointer_constant(value))
        else {
            return;
        };
        if fault == AssignFault::FunctionPointerVoid {
            if diag::warning_group_enabled(crate::types::FUNCTION_POINTER_CONV) {
                diag::warning(
                    pos,
                    &gettext("ISO C forbids assignment between function pointer and 'void *'"),
                );
            }
            return;
        }
        let (t_name, v_name) = (
            self.types.format_type(t, Some(self.idents)),
            self.types.format_type(v, Some(self.idents)),
        );
        if fault.is_error() {
            // gcc words this one case differently, and it is the clearer
            // phrasing: the problem is not the types but that there is no
            // value at all.
            if self.types.kind(v) == TypeKind::Void {
                diag::error(pos, &gettext("void value not ignored as it ought to be"));
                return;
            }
            diag::error_args(
                pos,
                "incompatible types when assigning to type '{0}' from type '{1}'",
                &[&t_name, &v_name],
            );
        } else {
            diag::warning_args(
                pos,
                "assignment to '{0}' from '{1}' {2}",
                &[&t_name, &v_name, fault.describe()],
            );
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
        // other array needs a list.
        if self.types.kind(target) == TypeKind::Array {
            if !self.string_literal_suits_array(target, init) {
                diag::error(init.pos, &gettext("invalid initializer"));
            }
            return;
        }

        let t = self.decayed_type(target);
        let v = self.decayed_type(v);
        let Some(fault) = self
            .types
            .assignment_fault(t, v, self.is_null_pointer_constant(init))
        else {
            return;
        };

        // An aggregate initialized from something that is not a compatible
        // aggregate is "invalid initializer" in gcc, not a type mismatch --
        // and the wording is the useful part, being what a user searches for.
        if fault.is_error() && matches!(self.types.kind(t), TypeKind::Struct | TypeKind::Union) {
            diag::error(init.pos, &gettext("invalid initializer"));
            return;
        }

        if fault == AssignFault::FunctionPointerVoid {
            if diag::warning_group_enabled(crate::types::FUNCTION_POINTER_CONV) {
                diag::warning(
                    init.pos,
                    &gettext("ISO C forbids initialization between function pointer and 'void *'"),
                );
            }
            return;
        }

        let (t_name, v_name) = (
            self.types.format_type(t, Some(self.idents)),
            self.types.format_type(v, Some(self.idents)),
        );
        if fault.is_error() {
            if self.types.kind(v) == TypeKind::Void {
                diag::error(
                    init.pos,
                    &gettext("void value not ignored as it ought to be"),
                );
                return;
            }
            diag::error_args(
                init.pos,
                "incompatible types when initializing type '{0}' using type '{1}'",
                &[&t_name, &v_name],
            );
        } else {
            diag::warning_args(
                init.pos,
                "initialization of '{0}' from '{1}' {2}",
                &[&t_name, &v_name, fault.describe()],
            );
        }
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
        if !self.types.modifiers(object).contains(TypeModifiers::ATOMIC) {
            return;
        }
        let what = match self.types.kind(self.resolve_struct_type(object)) {
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
    /// integer constant expression with the value 0, possibly cast to
    /// `void *`?
    fn is_null_pointer_constant(&self, expr: &Expr) -> bool {
        let inner = match &expr.kind {
            ExprKind::Cast { expr: inner, .. } => inner,
            _ => expr,
        };
        self.types.kind(inner.typ.unwrap_or(self.types.int_id)) != TypeKind::Pointer
            && self.eval_const_expr(inner) == Some(0)
    }

    /// Does this expression designate an object (C17 6.3.2.1p1)?
    ///
    /// Only the shapes that can name storage: an object identifier, a
    /// dereference, a subscript, a member of an lvalue, a member through a
    /// pointer, a compound literal, or a string literal. Everything else --
    /// the result of arithmetic, a call, a cast, a conditional, a comma -- is
    /// a value, not a place.
    fn is_lvalue(&self, expr: &Expr) -> bool {
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
            ExprKind::CompoundLiteral { .. }
            | ExprKind::StringLit(_)
            | ExprKind::WideStringLit(_)
            | ExprKind::Utf16StringLit(_)
            | ExprKind::Utf32StringLit(_) => true,
            // A GNU statement expression is an lvalue exactly when the
            // expression it ends with is one, which is what gcc documents.
            ExprKind::StmtExpr { result, .. } => self.is_lvalue(result),
            _ => false,
        }
    }

    /// Report an operand of unary `&` that has no address (C17 6.5.3.2p1).
    ///
    /// `register` is the case that bites: the storage class is a hint the
    /// compiler may ignore, but taking the address is still a constraint
    /// violation, and a program that does it is relying on the hint being
    /// ignored.
    pub(super) fn check_addressable(&self, operand: &Expr, pos: Position) {
        let ExprKind::Ident(symbol_id) = &operand.kind else {
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

    /// Report a target that cannot be assigned to or stepped (C17 6.5.16p2,
    /// 6.5.3.1p1). `verb` names the operator for the message, matching what
    /// gcc says so that the two agree on the wording users search for.
    pub(super) fn check_modifiable_lvalue(&self, target: &Expr, verb: &str, pos: Position) {
        if !self.is_lvalue(target) {
            diag::error_args(pos, "lvalue required as {0}", &[verb]);
            return;
        }
        // An array is an lvalue but never a modifiable one: it has no
        // assignment operator, only its elements do.
        if let Some(typ) = target.typ {
            if self.types.kind(typ) == TypeKind::Array {
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
            }
        }
    }
}
