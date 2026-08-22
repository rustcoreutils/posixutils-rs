//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Declarators and parameter lists (C17 6.7.6)
//

use super::ast::Expr;
use super::parser::{DeclaratorName, ParameterList, ParseError, ParseResult, Parser, RawParam};
use crate::diag;
use crate::strings::StringId;
use crate::symbol::Symbol;
use crate::token::lexer::{Position, SpecialToken, TokenType};
use crate::types::{Type, TypeId, TypeKind, TypeModifiers};
use gettextrs::gettext;

const DEFAULT_PARAM_CAPACITY: usize = 8;

/// Result of parsing a declarator: (name, type, VLA expressions, raw function parameters)
type DeclaratorResult = (StringId, TypeId, Vec<Expr>, Option<Vec<RawParam>>);

/// Function parameter type info: (type IDs, is_variadic)
/// The signature a function declarator's `( ... )` suffix contributes.
///
/// `prototyped` is false for `()` and for a K&R identifier list, which C17
/// 6.7.6.3p14 makes a different type from `(void)` rather than a shorter one.
struct FuncSignature {
    param_types: Vec<TypeId>,
    variadic: bool,
    prototyped: bool,
}

impl FuncSignature {
    /// The function type this signature makes of a return type.
    fn into_type(self, return_type: TypeId) -> Type {
        if self.prototyped {
            Type::function(return_type, self.param_types, self.variadic, false)
        } else {
            Type::function_no_prototype(return_type, false)
        }
    }
}

impl Parser<'_> {
    /// Consume the type-qualifier run after a `*` in a declarator.
    ///
    /// C17 6.7.6.1 lets any type qualifier appear there, `_Atomic` included:
    /// `int *_Atomic p;` declares an atomic pointer to int. Three copies of
    /// this loop had drifted apart and only the one in `parse_declarator`
    /// listed `_Atomic`, so the same declaration parsed inside a function and
    /// failed at file scope -- where `_Atomic` fell through to the name
    /// position and was taken as the identifier instead.
    pub(super) fn parse_pointer_qualifiers(&mut self) -> TypeModifiers {
        let mut modifiers = TypeModifiers::empty();
        while self.peek() == TokenType::Ident {
            let Some(name_id) = self.get_ident_id(self.current()) else {
                break;
            };
            match name_id {
                crate::kw::CONST => modifiers |= TypeModifiers::CONST,
                crate::kw::VOLATILE => modifiers |= TypeModifiers::VOLATILE,
                crate::kw::RESTRICT => modifiers |= TypeModifiers::RESTRICT,
                crate::kw::ATOMIC => modifiers |= TypeModifiers::ATOMIC,
                _ if super::is_nullability_qualifier(name_id) => {}
                _ => break,
            }
            self.advance();
        }
        modifiers
    }

    /// Parse a declarator (name and type modifiers)
    ///
    /// C declarators are parsed "inside-out". For example, `int (*p)[3]`:
    /// - `int` is the base type
    /// - `(*p)` means p is a pointer
    /// - `[3]` after the parens means "to array of 3"
    ///   So p is "pointer to array of 3 ints"
    ///
    /// Returns: (name, type, VLA size expressions, function parameters if declarator is function)
    /// The function parameters include names for use in function definitions.
    pub(crate) fn parse_declarator(
        &mut self,
        base_type_id: TypeId,
        name: DeclaratorName,
    ) -> ParseResult<DeclaratorResult> {
        // Collect pointer modifiers (they bind tighter than array/function)
        let mut pointer_modifiers: Vec<TypeModifiers> = Vec::new();
        while self.is_special(b'*') {
            self.advance();
            let mut ptr_modifiers = TypeModifiers::empty();

            ptr_modifiers |= self.parse_pointer_qualifiers();
            pointer_modifiers.push(ptr_modifiers);
        }

        // Check for parenthesized declarator: int (*p)[3]
        // The paren comes AFTER pointers, e.g. int *(*p)[3] = pointer to (pointer to array of 3 ints)
        let (name, inner_type_id, inner_func_params) = if self.is_special(b'(') {
            // Check if this looks like a function parameter list or a grouped declarator
            // A grouped declarator will have * or identifier immediately after (
            let saved_pos = self.pos;
            self.advance(); // consume '('

            let is_grouped = self.is_grouped_declarator();

            if is_grouped {
                // For int (*p)[3]: we're now at *p), base_type is int

                // Note: We ignore any VLA expression from inner declarators - VLAs would be
                // in the outer array dimensions, not inner pointer/grouped declarators
                let (inner_name, inner_decl_type_id, _inner_vla, inner_func_params) =
                    self.parse_declarator(self.types.void_id, name)?;
                self.expect_special(b')')?;

                (inner_name, Some(inner_decl_type_id), inner_func_params)
            } else {
                // The `(` opens a parameter list, so this declarator is
                // abstract: `int (size_t)` names a function type, and there is
                // no identifier to find. Rewind to the `(` and let the
                // function-suffix loop below consume the parameter list.
                self.pos = saved_pos;
                (StringId::EMPTY, None, None)
            }
        } else if self.peek() == TokenType::Ident {
            (self.expect_declarator_name()?, None, None)
        } else if name == DeclaratorName::Optional {
            // An abstract declarator has no identifier by construction
            // (C17 6.7.7): `void (*)(int)`, or a parameter written as a bare
            // type. Whether that is allowed is the caller's question, not a
            // guess from the next token -- this used to be a whitelist of
            // `)`, `[`, `(`, `,`, so a type-name ending in anything else was
            // rejected outright. `_Generic(1, int: 11)` ends its type-name at
            // a `:`, which was not on the list.
            (StringId::EMPTY, None, None)
        } else {
            (self.expect_declarator_name()?, None, None)
        };

        // Handle array declarators - collect all dimensions first
        // Also track VLA expressions (non-constant size) for each dimension
        let mut dimensions: Vec<(Option<usize>, Position)> = Vec::new();
        let mut vla_exprs: Vec<Expr> = Vec::new();
        while self.is_special(b'[') {
            let dim_pos = self.current_pos();
            self.advance();

            // Parse optional qualifiers and static (C99 6.7.5.3)
            // These are valid in function parameter array declarators
            while self.peek() == TokenType::Ident {
                if let Some(name_id) = self.get_ident_id(self.current()) {
                    match name_id {
                        // C17 6.7.6.2: the array declarator of a parameter
                        // takes a type-qualifier list, which includes
                        // `_Atomic`, and optionally `static`.
                        crate::kw::STATIC
                        | crate::kw::CONST
                        | crate::kw::VOLATILE
                        | crate::kw::RESTRICT
                        | crate::kw::ATOMIC => {
                            self.advance();
                        }
                        _ => break,
                    }
                } else {
                    break;
                }
            }

            // Check for [*] VLA unspecified size (C99 6.7.5.2)
            // This is used in function prototypes: void f(int n, int arr[*])
            let size = if self.is_special(b']') {
                None
            } else if self.is_special(b'*') {
                // Check if it's [*] (VLA star) or just a multiplication expression
                let saved_pos = self.pos;
                self.advance();
                if self.is_special(b']') {
                    // It's [*] - VLA with unspecified size
                    None
                } else {
                    // It's an expression starting with * (e.g., [*ptr])
                    self.pos = saved_pos;
                    let size_pos = self.current_pos();
                    let expr = self.parse_assignment_expr()?;
                    match self.eval_const_expr(&expr) {
                        Some(n) if n >= 0 => Some(n as usize),
                        Some(_) => {
                            return Err(ParseError::new(
                                "size of array is negative".to_string(),
                                size_pos,
                            ));
                        }
                        None => {
                            self.check_array_size_type(&expr, size_pos)?;
                            vla_exprs.push(expr);
                            None
                        }
                    }
                }
            } else {
                // Parse constant expression for array size (C99 6.7.5.2)
                let size_pos = self.current_pos();
                let expr = self.parse_assignment_expr()?;
                // Evaluate as integer constant expression
                match self.eval_const_expr(&expr) {
                    Some(n) if n >= 0 => Some(n as usize),
                    // C17 6.7.6.2p1: the size shall be greater than zero.
                    // This used to become `None`, i.e. an incomplete array,
                    // so `int a[-1]` was accepted and silently sized zero.
                    Some(_) => {
                        // gcc distinguishes the two, and the abstract case is
                        // the one a type-name reaches: `sizeof(char[-1])`
                        // says "unnamed", `char a[-1];` names `a`.
                        return Err(ParseError::new(
                            if name == StringId::EMPTY {
                                "size of unnamed array is negative".to_string()
                            } else {
                                format!("size of array '{}' is negative", self.idents.get(name))
                            },
                            size_pos,
                        ));
                    }
                    None => {
                        // Non-constant (VLA) - save expression for VLA handling
                        self.check_array_size_type(&expr, size_pos)?;
                        vla_exprs.push(expr);
                        None
                    }
                }
            };
            self.expect_special(b']')?;
            dimensions.push((size, dim_pos));
        }

        // Handle function declarators: void (*fp)(int, char)
        // This parses the parameter list after a grouped declarator
        // We keep both the TypeIds (for building the type) and raw params (for function defs)
        let (func_params, full_func_params): (Option<FuncSignature>, Option<Vec<RawParam>>) =
            if self.is_special(b'(') {
                self.advance();
                let list = self.parse_parameter_list()?;
                self.expect_special(b')')?;
                let param_types: Vec<TypeId> = list.params.iter().map(|p| p.typ).collect();
                (
                    Some(FuncSignature {
                        param_types,
                        variadic: list.variadic,
                        prototyped: list.prototyped,
                    }),
                    Some(list.params),
                )
            } else {
                (None, None)
            };

        // Build the type from the base type
        let mut result_type_id = base_type_id;

        if let Some(inner_tid) = inner_type_id {
            // Grouped declarator: int (*p)[3] or void (*fp)(int) or int *(*q)[3]
            // Outer pointers (before parens) apply to the base type first
            // Then arrays/functions in suffix are applied
            // Finally we substitute into the inner declarator

            // Apply any outer pointers (before the parens) to base type FIRST
            // For struct node *(*fp)(int): base is struct node, outer * -> Pointer(struct node)
            // For int *(*q)[3]: base is int, outer * -> Pointer(int)
            // Note: Forward iteration is correct - qualifiers after each * apply to that pointer level
            for modifiers in pointer_modifiers.into_iter() {
                let ptr_type = Type {
                    kind: TypeKind::Pointer,
                    modifiers,
                    base: Some(result_type_id),
                    ..Default::default()
                };
                result_type_id = self.types.intern(ptr_type);
            }

            // Apply function parameters to (possibly pointer-modified) base type
            // For struct node *(*fp)(int): result is Pointer(struct node)
            //   -> Function(Pointer(struct node), [int])
            if let Some(sig) = func_params {
                let func_type = sig.into_type(result_type_id);
                result_type_id = self.types.intern(func_type);
            }

            // Apply array dimensions to result type
            // For int *(*q)[3]: result is Pointer(int) -> Array(3, Pointer(int))
            for (size, pos) in dimensions.into_iter().rev() {
                result_type_id = self.derive_array_type(result_type_id, size, pos)?;
            }

            // Substitute into inner declarator
            // For int (*p)[3]: inner_decl is Pointer(Void), result_type is Array(3, int)
            // -> Pointer(Array(3, int))
            // For struct node *(*fp)(int): inner_decl is Pointer(Void),
            //   result_type is Function(Pointer(struct node), [int])
            // -> Pointer(Function(Pointer(struct node), [int]))
            result_type_id = self.substitute_base_type(inner_tid, result_type_id);
        } else {
            // Simple declarator: char *arr[3]
            // Pointers bind tighter than arrays: *arr[3] = array of pointers

            // Apply pointer modifiers to base type first
            // Note: Forward iteration is correct - qualifiers after each * apply to that pointer level
            for modifiers in pointer_modifiers.into_iter() {
                let ptr_type = Type {
                    kind: TypeKind::Pointer,
                    modifiers,
                    base: Some(result_type_id),
                    ..Default::default()
                };
                result_type_id = self.types.intern(ptr_type);
            }

            // Then apply array dimensions
            // For char *arr[3]: result_type is char*, suffix [3] -> Array(3, char*)
            for (size, pos) in dimensions.into_iter().rev() {
                result_type_id = self.derive_array_type(result_type_id, size, pos)?;
            }

            // Apply function parameters if present (for function declarators)
            // For int get_op(int which): base is int, suffix (int) -> Function(int, [int])
            // This is needed for nested declarators like int (*get_op(int))(int, int)
            if let Some(sig) = func_params {
                let func_type = sig.into_type(result_type_id);
                result_type_id = self.types.intern(func_type);
            }
        }

        // Determine which function parameters to return:
        // - For grouped declarator with inner function params: return inner_func_params
        //   (e.g., int (*get_op(int which))(int) - inner has params)
        // - For grouped declarator without inner params: return full_func_params
        //   (e.g., int (name)(int lhs, int rhs) - parenthesized name followed by params)
        // - For simple declarator with function: return full_func_params
        let returned_func_params = if inner_func_params.is_some() {
            inner_func_params
        } else {
            full_func_params
        };

        // Propagate storage class modifiers from base type to derived type
        // For "extern int *p", the EXTERN should be on the pointer type, not just int
        let storage_class_mask = TypeModifiers::EXTERN
            | TypeModifiers::STATIC
            | TypeModifiers::TYPEDEF
            | TypeModifiers::REGISTER
            | TypeModifiers::AUTO
            | TypeModifiers::THREAD_LOCAL;
        let base_storage_class = self.types.modifiers(base_type_id) & storage_class_mask;
        if !base_storage_class.is_empty() && result_type_id != base_type_id {
            // Add storage class modifiers to the result type
            let mut result_type = self.types.get(result_type_id).clone();
            result_type.modifiers |= base_storage_class;
            result_type_id = self.types.intern(result_type);
        }

        Ok((name, result_type_id, vla_exprs, returned_func_params))
    }

    /// Substitute the actual base type into a declarator parsed with a placeholder
    /// For int (*p)[3]: inner_decl is Pointer(Void), actual_base is Array(3, int)
    /// Result should be Pointer(Array(3, int))
    fn substitute_base_type(&mut self, decl_type_id: TypeId, actual_base_id: TypeId) -> TypeId {
        let decl_type = self.types.get(decl_type_id);
        match decl_type.kind {
            TypeKind::Void => actual_base_id,
            TypeKind::Pointer => {
                let inner_base_id = decl_type.base.unwrap();
                let decl_modifiers = decl_type.modifiers;
                let new_base_id = self.substitute_base_type(inner_base_id, actual_base_id);
                let ptr_type = Type {
                    kind: TypeKind::Pointer,
                    modifiers: decl_modifiers,
                    base: Some(new_base_id),
                    ..Default::default()
                };
                self.types.intern(ptr_type)
            }
            TypeKind::Array => {
                let inner_base_id = decl_type.base.unwrap();
                let decl_modifiers = decl_type.modifiers;
                let decl_array_size = decl_type.array_size;
                let new_base_id = self.substitute_base_type(inner_base_id, actual_base_id);
                let arr_type = Type {
                    kind: TypeKind::Array,
                    modifiers: decl_modifiers,
                    base: Some(new_base_id),
                    array_size: decl_array_size,
                    ..Default::default()
                };
                self.types.intern(arr_type)
            }
            TypeKind::Function => {
                // For function declarators like int (*get_op(int))(int, int)
                // The inner declarator is Function(Pointer(Void), [int])
                // We need to substitute Void with the actual return type
                let inner_base_id = decl_type.base.unwrap(); // return type (placeholder)
                let decl_params = decl_type.params.clone();
                let decl_variadic = decl_type.variadic;
                let decl_noreturn = decl_type.noreturn;
                let new_ret_id = self.substitute_base_type(inner_base_id, actual_base_id);
                let func_type = Type {
                    kind: TypeKind::Function,
                    base: Some(new_ret_id),
                    params: decl_params,
                    variadic: decl_variadic,
                    noreturn: decl_noreturn,
                    ..Default::default()
                };
                self.types.intern(func_type)
            }
            _ => decl_type_id, // Other types don't need substitution
        }
    }

    /// Parameters are declared in a temporary scope during parsing so that
    /// VLA sizes like `arr[n]` can reference earlier parameters like `n`.
    /// The scope is exited at the end; callers re-declare parameters as needed.
    pub(crate) fn parse_parameter_list(&mut self) -> ParseResult<ParameterList> {
        let mut params: Vec<RawParam> = Vec::with_capacity(DEFAULT_PARAM_CAPACITY);
        let mut variadic = false;
        let mut prototyped = true;

        // Enter a temporary scope for parameter parsing (C99 6.9.1p9)
        // This allows VLA sizes to reference earlier parameters
        self.symbols.enter_scope();

        // `()` -- an empty identifier list, which is not a prototype.
        if self.is_special(b')') {
            self.symbols.leave_scope();
            return Ok(ParameterList {
                params,
                variadic,
                prototyped: false,
            });
        }

        // Check for (void)
        if self.peek() == TokenType::Ident {
            if let Some(name_id) = self.get_ident_id(self.current()) {
                if name_id == crate::kw::VOID {
                    let saved_pos = self.pos;
                    self.advance();
                    if self.is_special(b')') {
                        self.symbols.leave_scope();
                        return Ok(ParameterList {
                            params,
                            variadic,
                            prototyped: true,
                        });
                    }
                    // Not just void, backtrack
                    self.pos = saved_pos;
                }
            }
        }

        loop {
            // Check for ellipsis
            if self.is_special_token(SpecialToken::Ellipsis) {
                // ISO C requires at least one named parameter before '...'
                // GCC/Clang emit a warning with -Wstrict-prototypes
                if params.is_empty() {
                    diag::warning(
                        self.current_pos(),
                        &gettext("ISO C requires a named argument before '...'"),
                    );
                }
                self.advance();
                variadic = true;
                break;
            }

            // Parse parameter type
            let param_type = self.parse_type_specifier()?;
            // An identifier list -- `int f(a, b) int a, b;` -- is not a
            // prototype (C17 6.7.6.3p14), and it is exactly the case where the
            // specifier parser supplied an implicit `int` without consuming an
            // identifier. The choice is all-or-nothing across the list, so the
            // first parameter settles it.
            if params.is_empty() && !variadic {
                prototyped = self.saw_explicit_type;
            }
            // For struct/union types with tags, use existing TypeId to preserve forward declarations
            let base_type_id = self.intern_type_with_tag(&param_type);

            // Use parse_declarator to handle all declarator forms including:
            // - Simple pointers: void *, int *
            // - Grouped declarators: void (*)(int), int (*)[10]
            // - Arrays: int arr[], int arr[10]
            // Note: parse_declarator returns (name, type, vla_sizes)
            let (param_name, mut typ_id, vla_sizes, _func_params) =
                self.parse_declarator(base_type_id, DeclaratorName::Optional)?;

            // Skip any __attribute__ after parameter declarator
            self.skip_extensions();

            // A parameter's type attributes are the parameter's, and are
            // applied before the array-to-pointer adjustment below so a mode
            // names the declared type rather than the adjusted one. Nothing
            // consumed them here, so `int x __attribute__((mode(QI)))` was
            // silently an `int`.
            typ_id = self.apply_pending_type_attrs(typ_id);

            // C99 6.7.5.3: Array and function parameters are adjusted to pointers
            // - Array T[] becomes pointer to T
            // - Function type becomes pointer to function type
            let typ = self.types.get(typ_id);
            if typ.kind == TypeKind::Array {
                // Convert array to pointer to element type
                let element_type = typ.base.unwrap_or(self.types.void_id);
                let ptr_type = Type {
                    kind: TypeKind::Pointer,
                    base: Some(element_type),
                    ..Default::default()
                };
                typ_id = self.types.intern(ptr_type);
            } else if typ.kind == TypeKind::Function {
                // Convert function type to pointer to function type
                // e.g., `int fn(int)` becomes `int (*)(int)`
                let ptr_type = Type {
                    kind: TypeKind::Pointer,
                    base: Some(typ_id),
                    ..Default::default()
                };
                typ_id = self.types.intern(ptr_type);
            }

            let name_opt = if param_name == StringId::EMPTY {
                None
            } else {
                Some(param_name)
            };

            // Keep the run-time dimensions of a variably-modified element
            // type. Every other declarator path already carries `vla_sizes`;
            // dropping them here is what left `int a[n][m]` indexing with a
            // row stride of zero.
            //
            // `vla_sizes` runs outermost-first over the whole declarator,
            // while only the element type matters after the array-to-pointer
            // adjustment. The outermost dimension is the one that may be
            // absent (`int a[][m]`), and C17 6.7.6.2p1 requires every later
            // dimension to be present, so the element type's variable
            // dimensions are exactly the trailing entries.
            let elem_typ = self.types.get(typ_id).base;
            let vm_dims = match elem_typ {
                Some(elem) => {
                    let want = self.types.unsized_array_levels(elem);
                    let skip = vla_sizes.len().saturating_sub(want);
                    vla_sizes[skip..].to_vec()
                }
                None => Vec::new(),
            };

            // C17 6.7.6.3p10: `void` may appear as a parameter only as the
            // unnamed sole item in the list -- and then it means the function
            // takes none. The literal `(void)` is recognised earlier, on the
            // token; this is the same thing reached through a typedef, as in
            // `typedef void V; int f(V);`, which must not become a
            // one-parameter prototype.
            if self.types.kind(typ_id) == TypeKind::Void {
                if name_opt.is_none() && params.is_empty() && self.is_special(b')') {
                    self.symbols.leave_scope();
                    return Ok(ParameterList {
                        params,
                        variadic,
                        prototyped: true,
                    });
                }
                diag::warning_args(
                    self.current_pos(),
                    "parameter {0} has void type",
                    &[&(params.len() + 1).to_string()],
                );
            }

            params.push(RawParam {
                name: name_opt,
                typ: typ_id,
                vm_dims,
                symbol: None,
            });

            // Declare parameter in temporary scope so later params can reference it
            // (C99 6.9.1p9: parameters are in scope for VLA sizes)
            if let Some(name) = name_opt {
                let sym = Symbol::parameter(name, typ_id, self.symbols.depth())
                    .with_variably_modified_array(!vla_sizes.is_empty());
                // 6.9.1p5: no two parameters may share a name. `declare`
                // reports it and the `Err` was dropped, leaving the second
                // parameter with no symbol at all -- so the function compiled
                // and every use of the name reached the first one.
                match self.symbols.declare(sym) {
                    Ok(sym_id) => {
                        if let Some(last) = params.last_mut() {
                            last.symbol = Some(sym_id);
                        }
                    }
                    Err(_) => {
                        let spelled = self.idents.get_opt(name).unwrap_or("").to_string();
                        diag::error_args(
                            self.current_pos(),
                            "redefinition of parameter '{0}'",
                            &[&spelled],
                        );
                    }
                }
            }

            if self.is_special(b',') {
                self.advance();
                // C17 6.7.6.3: a parameter-type-list is a comma-separated list
                // of parameter declarations, optionally followed by `, ...`.
                // Nothing else may follow the comma. Falling through here let
                // `parse_type_specifier` supply an implicit `int`, so
                // `void g(int, );` silently declared `void(int, int)` -- and
                // once call arity was checked, the *correct* call `g(1)`
                // became the one rejected. (C23 permits the trailing comma;
                // this compiler is C17.)
                if self.is_special(b')') {
                    return Err(ParseError::new(
                        "expected a declaration specifier or '...' after ','".to_string(),
                        self.current_pos(),
                    ));
                }
            } else {
                break;
            }
        }

        // Leave temporary parameter scope
        self.symbols.leave_scope();

        Ok(ParameterList {
            params,
            variadic,
            prototyped,
        })
    }
}
