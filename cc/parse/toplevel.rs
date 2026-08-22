//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Translation-unit and external-declaration parsing: function definitions,
// file-scope declarators and _Static_assert
//

use super::ast::{
    Declaration, ExternalDecl, FunctionDef, InitDeclarator, Parameter, Stmt, TranslationUnit,
};
use super::parser::{DeclaratorName, ParseError, ParseResult, Parser};
use crate::diag;
use crate::strings::StringId;
use crate::symbol::{Namespace, Symbol};
use crate::token::lexer::{payload_text, Position, TokenType, TokenValue};
use crate::types::{Type, TypeId, TypeKind, TypeModifiers};
use gettextrs::gettext;

/// The declaration specifiers shared by every declarator in one declaration.
///
/// `parse_remaining_declarators` already took these as five positional
/// parameters; naming them keeps the grouped-declarator helper to four.
struct DeclSpecs {
    pos: Position,
    modifiers: TypeModifiers,
    storage_class: TypeModifiers,
    is_typedef: bool,
}

impl Parser<'_> {
    pub fn parse_translation_unit(&mut self) -> ParseResult<TranslationUnit> {
        let mut tu = TranslationUnit::default();

        self.skip_stream_tokens();

        while !self.is_eof() {
            // Try to determine if this is a function definition or a declaration
            // Both start with type specifier + declarator
            let external_decl = self.parse_external_decl()?;
            tu.add(external_decl);
        }

        self.check_deferred_incomplete_definitions();

        Ok(tu)
    }

    /// C17 6.7p7: an object's type must be complete where the object is
    /// *defined*. At file scope that cannot be judged where the declaration
    /// appears, because 6.9.2p3 lets a tentative definition be completed later
    /// in the translation unit -- `struct U; struct U u; struct U { int a; };`
    /// is legal, and forward-declare-then-complete is everywhere in CPython and
    /// glibc. So file-scope definitions are collected as they are parsed and
    /// judged here, when nothing more can complete them.
    fn check_deferred_incomplete_definitions(&mut self) {
        for (typ, pos) in std::mem::take(&mut self.tentative_definitions) {
            // Ask the *tag*, not the recorded id. A qualified spelling --
            // `volatile struct S` -- is interned as a fresh type carrying a
            // clone of the tag's composite data as it stood at the time
            // (`intern_type_with_tag`), and `complete_struct` only ever
            // mutates the tag's own entry. So the recorded id is a frozen
            // `is_complete: false` that completing the tag never updates, and
            // `struct S; volatile struct S vs; struct S { int a; };` was
            // rejected although the tag is complete.
            let typ = self.resolve_struct_type(typ);
            if self.types.is_composite_complete(typ) {
                continue;
            }
            let named = self.types.format_type(typ, Some(self.idents));
            diag::error_args(
                pos,
                "storage size of an object of type '{0}' is not known",
                &[&named],
            );
        }
    }

    /// Check if current token is _Static_assert or static_assert
    pub(super) fn is_static_assert(&self) -> bool {
        if self.peek() != TokenType::Ident {
            return false;
        }
        if let Some(id) = self.get_ident_id(self.current()) {
            crate::kw::has_tag(id, crate::kw::ASSERT_KW)
        } else {
            false
        }
    }

    /// Parse _Static_assert(constant-expression, string-literal);
    /// C11: _Static_assert(expr, msg)
    /// C23: static_assert(expr) or static_assert(expr, msg)
    pub(super) fn parse_static_assert(&mut self) -> ParseResult<()> {
        let pos = self.current_pos();
        self.advance(); // consume _Static_assert / static_assert
        self.expect_special(b'(')?;

        // Parse constant expression
        let expr = self.parse_conditional_expr()?;

        // Evaluate the constant expression
        let value = self.eval_const_expr(&expr);

        // Check for optional message (C23 allows omitting it)
        let message = if self.is_special(b',') {
            self.advance(); // consume ','
                            // Expect string literal
            if self.peek() != TokenType::String {
                return Err(ParseError::new(
                    "expected string literal in _Static_assert",
                    self.current_pos(),
                ));
            }
            let msg = if let TokenValue::String(s) = &self.current().value {
                payload_text(s)
            } else {
                String::new()
            };
            self.advance(); // consume string
            msg
        } else {
            // C23: no message provided
            String::new()
        };

        self.expect_special(b')')?;
        self.expect_special(b';')?;

        // Check if assertion failed
        if let Some(v) = value {
            if v == 0 {
                // Assertion failed
                let msg = if message.is_empty() {
                    "static assertion failed".to_string()
                } else {
                    format!("static assertion failed: {}", message)
                };
                return Err(ParseError::new(msg, pos));
            }
        } else {
            // Could not evaluate at compile time
            return Err(ParseError::new(
                "_Static_assert expression is not a constant expression",
                pos,
            ));
        }

        Ok(())
    }

    /// Parse a function body, recording whether the forwarding builtins may
    /// appear in it.
    ///
    /// `__builtin_va_arg_pack()` names the caller's variadic arguments, so it
    /// needs the enclosing function to be variadic (there are arguments) and
    /// `always_inline` (there is a known caller to take them from).
    fn parse_forwarding_body(
        &mut self,
        attrs: &crate::parse::ast::FunctionAttrs,
        is_variadic: bool,
    ) -> ParseResult<Stmt> {
        let outer = self.in_forwarding_function;
        self.in_forwarding_function = is_variadic && attrs.always_inline;
        let body = self.parse_block_stmt_no_scope();
        self.in_forwarding_function = outer;
        body
    }

    fn accumulate_fn_attrs(&mut self, name: StringId) -> crate::parse::ast::FunctionAttrs {
        let pending = self.pending_fn_attrs.clone();
        let seen = self.declared_fn_attrs.entry(name).or_default();
        seen.merge(&pending);
        seen.clone()
    }

    /// Parse an external declaration (function definition or declaration)
    /// A grouped declarator at file scope: `void (*fp)(int)`, `int (*arr)[10]`,
    /// or `typedef int (name)(params)`.
    ///
    /// Returns `Ok(None)`, with the parser position restored, when what follows
    /// is not one.
    ///
    /// `merge_alignas_into_typedef` records an asymmetry that predates this
    /// helper and is preserved rather than levelled: the call before the
    /// pointer loop merged `pending_alignas` into a typedef's type and the call
    /// after it did not, so `typedef int *(name)(void)` carrying an
    /// `_Alignas`/`aligned` attribute dropped the alignment where
    /// `typedef int (name)(void)` kept it. Which of the two is right is a
    /// separate question from moving the code.
    fn parse_grouped_declarator_decl(
        &mut self,
        specs: &DeclSpecs,
        base: TypeId,
        merge_alignas_into_typedef: bool,
    ) -> ParseResult<Option<ExternalDecl>> {
        if self.is_special(b'(') {
            let saved_pos = self.pos;
            self.advance(); // consume '('
            if self.is_grouped_declarator() {
                // This is a grouped declarator - use parse_declarator
                self.pos = saved_pos; // restore position before '('
                let (name, mut typ, vla_sizes, decl_func_params) =
                    self.parse_declarator(base, DeclaratorName::Required)?;

                // C99 6.7.5.2: VLAs must have block scope
                if !vla_sizes.is_empty() {
                    return Err(ParseError::new(
                        "variable length arrays cannot have file scope".to_string(),
                        self.current_pos(),
                    ));
                }

                // Parse any __attribute__ after declarator
                let attrs = self.parse_attributes();
                let calling_conv = attrs.calling_conv().unwrap_or_default();
                let fn_attrs = attrs.function_attrs();
                self.pending_fn_attrs.merge(&fn_attrs);
                // Symbol attributes too. Only the function half was collected
                // here, so `weak` written after a function declarator was
                // dropped -- and a weak *declaration* is the whole point of
                // the attribute.
                self.merge_symbol_attrs(&attrs);
                let all_fn_attrs = if self.types.kind(typ) == TypeKind::Function {
                    self.accumulate_fn_attrs(name)
                } else {
                    Default::default()
                };

                // Check if this is a function definition (function type followed by '{')
                // This handles cases like: int (*get_op(int which))(int, int) { ... }
                if self.types.kind(typ) == TypeKind::Function && self.is_special(b'{') {
                    // Get the function's return type
                    // Storage class (static, inline) comes from base_type, not the function type
                    let func_type = self.types.get(typ);
                    let return_type = func_type.base.unwrap();
                    let is_variadic_fn = func_type.variadic;
                    let is_static = specs.modifiers.contains(TypeModifiers::STATIC);
                    let is_inline = specs.modifiers.contains(TypeModifiers::INLINE);

                    // Add function to symbol table
                    self.check_redeclaration(name, typ, specs.pos);
                    let func_sym = Symbol::function(name, typ, self.symbols.depth());
                    let _ = self.symbols.declare(func_sym);
                    // Definitions bind a fresh symbol, so this path needs the
                    // accumulated facts settled onto it exactly as the other
                    // two function-definition paths do -- without it, this
                    // definition's C99 6.7.4p6 inline classification is
                    // computed from declarations it never saw.
                    self.settle_declaration_facts(name, specs.storage_class);

                    // Get raw parameters - use decl_func_params which has names
                    let raw_params = decl_func_params.unwrap_or_default();

                    // Enter function scope for parameters
                    self.symbols.enter_scope();

                    // Bind parameters in function scope and create Parameter structs
                    let mut params = Vec::with_capacity(raw_params.len());
                    for raw in &raw_params {
                        let symbol_id = raw.symbol.map(|id| self.symbols.redeclare(id, raw.typ));
                        params.push(Parameter {
                            symbol: symbol_id,
                            typ: raw.typ,
                            vm_dims: raw.vm_dims.clone(),
                        });
                    }

                    // Parse body without creating another scope
                    let body = self.parse_forwarding_body(&all_fn_attrs, is_variadic_fn)?;

                    // Leave function scope
                    self.symbols.leave_scope();

                    return Ok(Some(ExternalDecl::FunctionDef(FunctionDef {
                        return_type,
                        name,
                        params,
                        body,
                        pos: specs.pos,
                        is_static,
                        is_inline,
                        calling_conv,
                        attrs: all_fn_attrs,
                    })));
                }

                // Handle initializer (for declarations, not function definitions)
                let init = if self.is_special(b'=') {
                    if specs.is_typedef {
                        return Err(ParseError::new(
                            "typedef cannot have initializer",
                            self.current_pos(),
                        ));
                    }
                    self.advance();
                    Some(self.parse_initializer()?)
                } else {
                    None
                };

                self.skip_extensions();
                self.expect_special(b';')?;

                // Validate explicit alignment (C11 6.7.5: >= natural alignment)
                typ = self.apply_pending_type_attrs(typ);
                let validated_align = self.validated_explicit_align(typ)?;

                // Add to symbol table and capture SymbolId
                // C allows multiple declarations of the same variable at file scope
                let symbol_id = if specs.is_typedef {
                    // A mode replaces the type; alignment then attaches to
                    // whatever the type ended up being.
                    typ = self.apply_pending_type_attrs(typ);
                    // Apply __attribute__((aligned(N))) to typedef type
                    if merge_alignas_into_typedef {
                        if let Some(align) = self.pending_alignas {
                            let mut aligned_type = self.types.get(typ).clone();
                            aligned_type.explicit_align =
                                Some(aligned_type.explicit_align.map_or(align, |e| e.max(align)));
                            typ = self.types.intern(aligned_type);
                        }
                    }
                    self.check_typedef_redefinition(name, typ, specs.pos);
                    let sym = Symbol::typedef(name, typ, self.symbols.depth());
                    self.symbols
                        .declare(sym)
                        .ok()
                        .or_else(|| self.symbols.lookup_id(name, Namespace::Ordinary))
                } else {
                    self.check_redeclaration(name, typ, specs.pos);
                    let var_sym = Symbol::variable(name, typ, self.symbols.depth())
                        .with_align(validated_align);
                    self.symbols
                        .declare(var_sym)
                        .ok()
                        .or_else(|| self.symbols.lookup_id(name, Namespace::Ordinary))
                };

                let symbol = symbol_id.expect("declaration must have symbol");
                self.settle_declaration_facts(name, specs.storage_class);
                return Ok(Some(ExternalDecl::Declaration(Declaration {
                    declarators: vec![InitDeclarator {
                        symbol_attrs: std::mem::take(&mut self.pending_symbol_attrs),
                        symbol,
                        typ,
                        storage_class: specs.storage_class,
                        init,
                        vla_sizes: vec![],
                        explicit_align: validated_align,
                        pos: specs.pos,
                    }],
                })));
            }
            // Not a grouped declarator, restore position
            self.pos = saved_pos;
        }
        Ok(None)
    }

    /// Clear the `pending_*` state a previous declaration may have left
    /// behind.
    ///
    /// `skip_extensions` collects an asm label wherever it runs, which is most
    /// places a type specifier can appear, but only a file-scope declarator
    /// claims one -- so a label written on a block-scope declaration, on a
    /// struct definition, or in a shape this parser does not model stays
    /// pending and would be claimed by whatever is declared next. The symptom
    /// is an unrelated global emitted under someone else's assembler name.
    /// The symbol and function attributes carry the same hazard: a variable
    /// after a `section(...)` function was emitted into that function's
    /// section, and the conflicting "ax"/"aw" flags made the assembler reject
    /// the file outright.
    fn reset_pending_declaration_state(&mut self) {
        // Clear pending alignment from previous declaration
        self.pending_alignas = None;
        // A mode that no declarator consumed belongs to no later declaration:
        // leaving it set applied it to whatever came next.
        self.pending_mode = None;
        self.pending_transparent_union = None;
        self.pending_fn_attrs = Default::default();
        // And any asm label the previous declaration left behind.
        //
        // `skip_extensions` collects one wherever it runs, which is most
        // places a type specifier can appear, but only a file-scope declarator
        // claims one -- so a label written on a block-scope declaration, on a
        // struct definition, or in a shape this parser does not model at all
        // stays pending and is claimed by whatever is declared next. The
        // symptom is an unrelated global emitted under someone else's
        // assembler name.
        self.pending_asm_label = None;
        // Likewise the symbol attributes. A function *definition* takes its
        // attributes through `pending_fn_attrs` and leaves these behind, so the
        // next declaration to build an `InitDeclarator` claimed them: the
        // variable after a `section(...)` function was emitted into that
        // function's section, and the conflicting "ax"/"aw" flags made the
        // assembler reject the file outright.
        self.pending_symbol_attrs = Default::default();

        // Check for _Static_assert first (C11)
    }

    /// A file-scope function declarator: `int f(int)` as a declaration, a
    /// definition with a body, or a K&R definition whose parameter
    /// declarations follow the parenthesised identifier list.
    ///
    /// Returns `Ok(None)` when the name is not followed by `(`.
    fn parse_function_declarator_decl(
        &mut self,
        specs: &DeclSpecs,
        base_type: &Type,
        base_type_id: TypeId,
        name: StringId,
        typ_id: TypeId,
    ) -> ParseResult<Option<ExternalDecl>> {
        if self.is_special(b'(') {
            // Could be function definition or declaration
            self.advance();
            let param_list = self.parse_parameter_list()?;
            let (variadic, prototyped) = (param_list.variadic, param_list.prototyped);
            let mut params = param_list.params;
            self.expect_special(b')')?;

            // Parse __attribute__ after parameter list (e.g., __attribute__((noreturn)))
            let attrs = self.parse_attributes();
            let fn_attrs = attrs.function_attrs();
            self.pending_fn_attrs.merge(&fn_attrs);
            self.merge_symbol_attrs(&attrs);
            let all_fn_attrs = self.accumulate_fn_attrs(name);
            // noreturn can come from __attribute__((noreturn)) or _Noreturn keyword in base type
            let typ_from_table = self.types.get(typ_id);
            let is_noreturn =
                attrs.has_noreturn() || typ_from_table.modifiers.contains(TypeModifiers::NORETURN);
            // Extract calling convention from attributes
            let calling_conv = attrs.calling_conv().unwrap_or_default();

            // K&R (old-style) parameter declarations: type declarations between ) and {
            // e.g., int add(a, b) int a; int b; { ... }
            // The parameter list already parsed bare names with implicit int.
            // Now parse explicit type declarations and update parameter types.
            if self.is_declaration_start() && !self.is_special(b'{') {
                while self.is_declaration_start() {
                    let knr_type = self.parse_type_specifier()?;
                    let knr_base_id = self.intern_type_with_tag(&knr_type);
                    loop {
                        let (decl_name, mut decl_typ, _vla, _fparams) =
                            self.parse_declarator(knr_base_id, DeclaratorName::Required)?;
                        // C99 6.7.5.3: array/function params adjusted to pointers
                        let typ = self.types.get(decl_typ);
                        if typ.kind == TypeKind::Array {
                            let elem = typ.base.unwrap_or(self.types.void_id);
                            decl_typ = self.types.intern(Type {
                                kind: TypeKind::Pointer,
                                base: Some(elem),
                                ..Default::default()
                            });
                        } else if typ.kind == TypeKind::Function {
                            decl_typ = self.types.intern(Type {
                                kind: TypeKind::Pointer,
                                base: Some(decl_typ),
                                ..Default::default()
                            });
                        }
                        // Update matching parameter type
                        if decl_name != StringId::EMPTY {
                            for param in &mut params {
                                if param.name == Some(decl_name) {
                                    param.typ = decl_typ;
                                }
                            }
                        }
                        if self.is_special(b',') {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    self.expect_special(b';')?;
                }
            }

            if self.is_special(b'{') {
                // Function definition
                // Use specs.storage_class extracted from original base_type at line 5926,
                // not from typ_id which may have lost storage class for tagged structs
                let is_static = specs.storage_class.contains(TypeModifiers::STATIC);
                let is_inline = specs.storage_class.contains(TypeModifiers::INLINE);

                // Add function to symbol table so it can be called by other functions
                let param_type_ids: Vec<TypeId> = params.iter().map(|p| p.typ).collect();
                let func_type = if prototyped {
                    Type::function(typ_id, param_type_ids.clone(), variadic, is_noreturn)
                } else {
                    Type::function_no_prototype(typ_id, is_noreturn)
                };
                // An old-style declarator has no `...` to be variadic with.
                let is_variadic_fn = prototyped && variadic;
                let func_type_id = self.types.intern(func_type);
                self.check_redeclaration(name, func_type_id, specs.pos);
                let func_sym = Symbol::function(name, func_type_id, self.symbols.depth());
                let _ = self.symbols.declare(func_sym);
                self.settle_declaration_facts(name, specs.storage_class);

                // Enter function scope for parameters
                self.symbols.enter_scope();

                // Bind parameters in function scope and create Parameter structs
                let mut final_params = Vec::with_capacity(params.len());
                for raw in &params {
                    let symbol_id = raw.symbol.map(|id| self.symbols.redeclare(id, raw.typ));
                    final_params.push(Parameter {
                        symbol: symbol_id,
                        typ: raw.typ,
                        vm_dims: raw.vm_dims.clone(),
                    });
                }

                // Parse body without creating another scope
                let body = self.parse_forwarding_body(&all_fn_attrs, is_variadic_fn)?;

                // Leave function scope
                self.symbols.leave_scope();

                return Ok(Some(ExternalDecl::FunctionDef(FunctionDef {
                    return_type: typ_id,
                    name,
                    params: final_params,
                    body,
                    pos: specs.pos,
                    is_static,
                    is_inline,
                    calling_conv,
                    attrs: all_fn_attrs,
                })));
            } else {
                // Function declaration
                // Skip __asm("...") symbol aliasing which can appear after function declarator
                self.skip_extensions();
                // ...but not necessarily the end of the declaration. C17 6.7
                // lets an init-declarator-list hold any declarators, function
                // ones included, so `int f(int), g(int);` and sparse's
                // `static struct symbol *base_type(...), *do_expression(...);`
                // are ordinary declarations. Demanding `;` here rejected every
                // list whose first declarator was a function.
                let more_declarators = self.is_special(b',');
                if !more_declarators {
                    self.expect_special(b';')?;
                }
                let param_type_ids: Vec<TypeId> = params.iter().map(|p| p.typ).collect();
                let func_type = if prototyped {
                    Type::function(typ_id, param_type_ids, variadic, is_noreturn)
                } else {
                    Type::function_no_prototype(typ_id, is_noreturn)
                };
                let func_type_id = self.types.intern(func_type);
                // Add to symbol table and capture SymbolId
                // C allows multiple declarations of the same function
                let symbol_id = if specs.is_typedef {
                    // Function type typedef: typedef void my_func(void);
                    self.check_typedef_redefinition(name, func_type_id, specs.pos);
                    let sym = Symbol::typedef(name, func_type_id, self.symbols.depth());
                    self.symbols
                        .declare(sym)
                        .ok()
                        .or_else(|| self.symbols.lookup_id(name, Namespace::Ordinary))
                } else {
                    // Function declaration: add so the variadic flag is available when called
                    self.check_redeclaration(name, func_type_id, specs.pos);
                    let func_sym = Symbol::function(name, func_type_id, self.symbols.depth());
                    self.symbols
                        .declare(func_sym)
                        .ok()
                        .or_else(|| self.symbols.lookup_id(name, Namespace::Ordinary))
                };
                let symbol = symbol_id.expect("function declaration must have symbol");
                self.settle_declaration_facts(name, specs.storage_class);
                let mut first_declarator = vec![InitDeclarator {
                    symbol_attrs: std::mem::take(&mut self.pending_symbol_attrs),
                    symbol,
                    typ: func_type_id,
                    storage_class: specs.storage_class,
                    init: None,
                    vla_sizes: vec![],
                    explicit_align: None, // Functions don't have _Alignas
                    pos: specs.pos,
                }];
                if more_declarators {
                    self.parse_remaining_declarators(
                        base_type,
                        base_type_id,
                        specs.is_typedef,
                        specs.storage_class,
                        specs.pos,
                        &mut first_declarator,
                    )?;
                    self.expect_special(b';')?;
                    self.pending_alignas = None;
                    self.pending_vm_typedef_dims = None;
                    self.pending_mode = None;
                    self.pending_transparent_union = None;
                }
                return Ok(Some(ExternalDecl::Declaration(Declaration {
                    declarators: first_declarator,
                })));
            }
        }
        Ok(None)
    }

    pub(crate) fn parse_external_decl(&mut self) -> ParseResult<ExternalDecl> {
        self.reset_pending_declaration_state();

        if self.is_static_assert() {
            self.parse_static_assert()?;
            // Return empty declaration - static_assert produces nothing
            return Ok(ExternalDecl::Declaration(Declaration {
                declarators: vec![],
            }));
        }

        // A stray `;` at file scope is an empty declaration. C17 6.7p2 makes it
        // a constraint violation, but GCC and Clang accept it by default (they
        // warn only under -pedantic) and it is common in real source: any
        // function-like macro that expands to nothing and is invoked with a
        // trailing semicolon produces one. CPython's `_Py_DECLARE_STR()` is
        // exactly that. Consume it before reaching the type specifier, which
        // would otherwise report a spurious "type specifier missing".
        if self.is_special(b';') {
            self.advance();
            return Ok(ExternalDecl::Declaration(Declaration {
                declarators: vec![],
            }));
        }

        let decl_pos = self.current_pos();
        // Parse type specifier
        let base_type = self.parse_type_specifier()?;
        // A declaration that stops right here declares nothing, and that --
        // not a missing type specifier -- is what to report. The `;` arms
        // below do it.
        if !self.is_special(b';') {
            self.check_implicit_int(decl_pos);
        }
        // Skip __attribute__ between type and declarator (GCC extension)
        self.skip_extensions();
        // Check modifiers before interning (storage class specifiers)
        let is_typedef = base_type.modifiers.contains(TypeModifiers::TYPEDEF);
        // Extract storage class specifiers (not stored in type system)
        let storage_class_mask = TypeModifiers::EXTERN
            | TypeModifiers::STATIC
            | TypeModifiers::THREAD_LOCAL
            | TypeModifiers::TYPEDEF
            | TypeModifiers::AUTO
            | TypeModifiers::REGISTER
            // `inline` is a function specifier rather than a storage class,
            // but it travels with them here: leaving it out made
            // `FunctionDef::is_inline` false for every ordinary definition,
            // and with it every consumer downstream.
            | TypeModifiers::INLINE;
        let storage_class = base_type.modifiers & storage_class_mask;
        // For struct/union types with tags, use existing TypeId to preserve forward declarations
        let base_type_id = self.intern_type_with_tag(&base_type);
        let specs = DeclSpecs {
            pos: decl_pos,
            modifiers: base_type.modifiers,
            storage_class,
            is_typedef,
        };

        // Check for standalone type definition (e.g., "enum Color { ... };")
        // This happens when a composite type is defined but no variables are declared
        if self.is_special(b';') {
            self.advance();
            self.check_declares_something(decl_pos, &base_type);
            // Return empty declaration - the type was already registered in parse_*_specifier
            return Ok(ExternalDecl::Declaration(Declaration {
                declarators: vec![],
            }));
        }

        // Check for grouped declarator: void (*fp)(int), int (*arr)[10], or typedef int (name)(params)
        if let Some(decl) = self.parse_grouped_declarator_decl(&specs, base_type_id, true)? {
            return Ok(decl);
        }

        // Handle pointer with qualifiers (const, volatile, restrict)
        let mut typ_id = base_type_id;
        while self.is_special(b'*') {
            self.advance();
            let mut ptr_modifiers = TypeModifiers::empty();

            ptr_modifiers |= self.parse_pointer_qualifiers();

            let ptr_type = Type {
                kind: TypeKind::Pointer,
                modifiers: ptr_modifiers,
                base: Some(typ_id),
                ..Default::default()
            };
            typ_id = self.types.intern(ptr_type);
        }

        // Propagate storage class modifiers from base type to derived pointer type
        // For "extern int *p", the EXTERN should be on the pointer type, not just int
        if typ_id != base_type_id {
            let storage_class_mask = TypeModifiers::EXTERN
                | TypeModifiers::STATIC
                | TypeModifiers::TYPEDEF
                | TypeModifiers::REGISTER
                | TypeModifiers::AUTO
                | TypeModifiers::THREAD_LOCAL
                // Without this a pointer-returning `inline` function loses the
                // bit twice over -- `memcpy` returning `void *` is exactly the
                // shape glibc uses.
                | TypeModifiers::INLINE;
            let base_storage_class = self.types.modifiers(base_type_id) & storage_class_mask;
            if !base_storage_class.is_empty() {
                let mut typ = self.types.get(typ_id).clone();
                typ.modifiers |= base_storage_class;
                typ_id = self.types.intern(typ);
            }
        }

        // Skip __attribute__ after pointer declarator (GCC extension)
        // Handles: void * __attribute__((malloc)) func(...)
        self.skip_extensions();

        // Check again for grouped declarator after pointer modifiers: char *(*fp)(int)
        // Also handles: char *(name)(params) for function type
        if let Some(decl) = self.parse_grouped_declarator_decl(&specs, typ_id, false)? {
            return Ok(decl);
        }

        // Parse name
        let name = self.expect_declarator_name()?;

        // Check for function definition vs declaration
        if let Some(decl) =
            self.parse_function_declarator_decl(&specs, &base_type, base_type_id, name, typ_id)?
        {
            return Ok(decl);
        }

        // Variable/typedef declaration
        let mut declarators = Vec::new();

        // Handle array - collect dimensions first, build type from right to left
        //
        // This is the first declarator of a file-scope declaration, which has
        // its own dimension loop rather than going through `parse_declarator`.
        // That is why the three "variable length arrays cannot have file
        // scope" checks elsewhere in this function never saw a plain
        // `int bad[n];`: they guard the grouped-declarator, K&R and
        // second-declarator paths. Here every unusable size folded to
        // `unwrap_or(0)`, so the declaration was accepted and sized zero.
        let mut var_type_id = typ_id;
        let mut dimensions: Vec<(Option<usize>, Position)> = Vec::new();
        while self.is_special(b'[') {
            let dim_pos = self.current_pos();
            self.advance();
            let size = if self.is_special(b']') {
                // No size given at all: an incomplete type, which is a
                // tentative definition at file scope and legal.
                None
            } else {
                // Parse constant expression for array size (C99 6.7.5.2)
                let size_pos = self.current_pos();
                let arr_expr = self.parse_assignment_expr()?;
                // Evaluate as integer constant expression
                match self.eval_const_expr(&arr_expr) {
                    Some(n) if n >= 0 => Some(n as usize),
                    // C17 6.7.6.2p1: the size shall be greater than zero. Zero
                    // itself is a GNU extension gcc accepts, so only a
                    // negative size is refused here.
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
                    // A size expression that is not a constant makes the type
                    // variably modified, which 6.7.6.2p2 confines to block
                    // scope.
                    None => {
                        self.check_array_size_type(&arr_expr, size_pos)?;
                        return Err(ParseError::new(
                            "variable length arrays cannot have file scope".to_string(),
                            size_pos,
                        ));
                    }
                }
            };
            self.expect_special(b']')?;
            dimensions.push((size, dim_pos));
        }
        // Build type from right to left (innermost dimension first).
        //
        // An absent extent stays `None`, as `parse_declarator` records it.
        // Collapsing it to `Some(0)` here made `int a[];` indistinguishable
        // from the GNU zero-length `int a[0];`, so the two declarator paths
        // disagreed about what "incomplete" looks like and `sizeof a` could
        // not tell them apart.
        for (size, pos) in dimensions.into_iter().rev() {
            var_type_id = self.derive_array_type(var_type_id, size, pos)?;
        }

        // Propagate storage class modifiers from base type to derived array type
        // For "typedef int arr[10]", the TYPEDEF should be on the array type, not just int
        if var_type_id != typ_id {
            let storage_class_mask = TypeModifiers::EXTERN
                | TypeModifiers::STATIC
                | TypeModifiers::TYPEDEF
                | TypeModifiers::REGISTER
                | TypeModifiers::AUTO
                | TypeModifiers::THREAD_LOCAL
                // Without this a pointer-returning `inline` function loses the
                // bit twice over -- `memcpy` returning `void *` is exactly the
                // shape glibc uses.
                | TypeModifiers::INLINE;
            let base_storage_class = self.types.modifiers(typ_id) & storage_class_mask;
            if !base_storage_class.is_empty() {
                let mut var_type = self.types.get(var_type_id).clone();
                var_type.modifiers |= base_storage_class;
                var_type_id = self.types.intern(var_type);
            }
        }

        // Skip any __attribute__ after variable name/array declarator
        self.skip_extensions_after_declarator();

        // Validate explicit alignment (C11 6.7.5: >= natural alignment)
        var_type_id = self.apply_pending_type_attrs(var_type_id);
        let validated_align = self.validated_explicit_align(var_type_id)?;

        // 6.7p7 for a file-scope *definition*. Judged at end of translation
        // unit, since 6.9.2p3 lets a tentative definition be completed later.
        // `extern` and `typedef` define nothing, and a function is not an
        // object.
        if !is_typedef {
            // An array's *element* type has to be complete where the array is
            // declared, because the stride is what forms the type -- so this
            // holds even for `extern`, and even when the tag is completed
            // further down, both of which gcc rejects.
            let elem = self.types.array_element_deep(var_type_id);
            if elem != var_type_id {
                if !self.types.is_composite_complete(elem) {
                    let named = self.types.format_type(elem, Some(self.idents));
                    diag::error_args(
                        self.current_pos(),
                        "array type has incomplete element type '{0}'",
                        &[&named],
                    );
                }
            } else if !base_type.modifiers.contains(TypeModifiers::EXTERN)
                && !self.types.is_composite_complete(var_type_id)
            {
                // A *definition* needs a size. Only this half gets the
                // tentative-definition grace of 6.9.2p3.
                self.tentative_definitions
                    .push((var_type_id, self.current_pos()));
            }
        }

        // Bind variable to symbol table BEFORE parsing initializer.
        // This ensures the variable is in scope for self-referential initializers.
        // Per C99 6.2.1p7: "Any other identifier has scope that begins just
        // after the completion of its declarator."
        // For typedefs, we bind AFTER (since typedef initializers are forbidden anyway).
        let mut symbol = if is_typedef {
            None // Will be bound after initializer parsing
        } else {
            // Add global variable to symbol table so it can be referenced in initializer
            self.check_redeclaration(name, var_type_id, self.current_pos());
            let var_sym = Symbol::variable(name, var_type_id, self.symbols.depth())
                .with_align(validated_align);
            Some(match self.symbols.declare(var_sym) {
                Ok(id) => id,
                Err(_) => {
                    // Extern declaration of existing variable - reuse existing symbol
                    let existing = self
                        .symbols
                        .lookup_id(name, Namespace::Ordinary)
                        .expect("redeclaration should find existing symbol");
                    // 6.2.7p4: the composite of two compatible array types has
                    // whichever extent is known, so a later declaration
                    // completes an earlier `extern int a[];`. Keeping the first
                    // type left the object incomplete for good, and `sizeof a`
                    // after `int a[4];` was refused.
                    if self
                        .types
                        .unsized_array_levels(self.symbols.get(existing).typ)
                        > 0
                        && self.types.unsized_array_levels(var_type_id) == 0
                    {
                        self.symbols.get_mut(existing).typ = var_type_id;
                    }
                    existing
                }
            })
        };

        // Handle initializer (not allowed for typedef)
        let init = if self.is_special(b'=') {
            if is_typedef {
                return Err(ParseError::new(
                    "typedef cannot have initializer",
                    self.current_pos(),
                ));
            }
            self.advance();
            Some(self.parse_initializer()?)
        } else {
            None
        };

        // For incomplete array types, infer size from initializer
        if let Some(ref init_expr) = init {
            let old_type = var_type_id;
            var_type_id = self.infer_array_size_from_init(var_type_id, init_expr);
            // 6.7.9p5 again, at file scope: the declaration *is* a
            // definition with external linkage, so `extern` adds nothing
            // and gcc warns rather than rejecting.
            if base_type.modifiers.contains(TypeModifiers::EXTERN) {
                diag::warning(
                    init_expr.pos,
                    &gettext("'extern' variable has an initializer"),
                );
            }
            self.check_excess_initializers(var_type_id, init_expr);
            self.check_initializer_types(var_type_id, init_expr);

            // If the type changed (array size was inferred), update the symbol's type
            // This is needed because the symbol was already added before parsing the initializer
            if var_type_id != old_type {
                if let Some(sym_id) = symbol {
                    self.symbols.get_mut(sym_id).typ = var_type_id;
                }
            }
        }

        // Bind typedef to symbol table (after parsing initializer, which is forbidden anyway)
        if is_typedef {
            var_type_id = self.apply_pending_type_attrs(var_type_id);
            // Apply __attribute__((aligned(N))) to typedef type
            if let Some(align) = self.pending_alignas {
                let mut aligned_type = self.types.get(var_type_id).clone();
                aligned_type.explicit_align =
                    Some(aligned_type.explicit_align.map_or(align, |e| e.max(align)));
                var_type_id = self.types.intern(aligned_type);
            }
            self.check_typedef_redefinition(name, var_type_id, self.current_pos());
            let sym = Symbol::typedef(name, var_type_id, self.symbols.depth());
            symbol = Some(match self.symbols.declare(sym) {
                Ok(id) => id,
                Err(_) => self
                    .symbols
                    .lookup_id(name, Namespace::Ordinary)
                    .expect("redeclaration should find existing symbol"),
            });
        }

        let symbol = symbol.expect("symbol should be bound");
        self.settle_declaration_facts(name, storage_class);
        declarators.push(InitDeclarator {
            symbol_attrs: std::mem::take(&mut self.pending_symbol_attrs),
            symbol,
            typ: var_type_id,
            storage_class,
            init,
            vla_sizes: vec![],
            explicit_align: validated_align,
            pos: decl_pos,
        });

        // Handle additional declarators
        self.parse_remaining_declarators(
            &base_type,
            base_type_id,
            is_typedef,
            storage_class,
            decl_pos,
            &mut declarators,
        )?;

        self.expect_special(b';')?;

        // Clear pending alignment after declaration
        self.pending_alignas = None;
        // Belongs to the declaration whose specifiers named the typedef, and
        // to no later one.
        self.pending_vm_typedef_dims = None;
        // A mode that no declarator consumed belongs to no later declaration:
        // leaving it set applied it to whatever came next.
        self.pending_mode = None;
        self.pending_transparent_union = None;

        Ok(ExternalDecl::Declaration(Declaration { declarators }))
    }

    /// Parse the declarators after the first in a file-scope declaration.
    ///
    /// `int a, b;` and `int f(int), g(int);` are the same grammar -- C17 6.7's
    /// *declaration-specifiers init-declarator-list* -- and a function
    /// declarator is an ordinary member of that list.
    ///
    /// Takes `base_type_id` rather than the running `typ_id`: any `*` before
    /// the first declarator belongs to that declarator alone, so in
    /// `int *f(int), g(int)` the `g` is an `int`, not an `int *`.
    fn parse_remaining_declarators(
        &mut self,
        base_type: &Type,
        base_type_id: TypeId,
        is_typedef: bool,
        storage_class: TypeModifiers,
        decl_pos: Position,
        declarators: &mut Vec<InitDeclarator>,
    ) -> ParseResult<()> {
        while self.is_special(b',') {
            self.advance();
            let next_decl_pos = self.current_pos();
            let (decl_name, mut decl_type, vla_sizes, _decl_func_params) =
                self.parse_declarator(base_type_id, DeclaratorName::Required)?;

            // An attribute may follow any declarator, not just the first.
            // Without this the second one was a syntax error -- `int a, b
            // __attribute__((unused));` did not compile at file scope, while
            // the same line inside a function did, because the block-scope
            // loop has always parsed attributes after every declarator.
            self.skip_extensions_after_declarator();

            // C99 6.7.5.2: VLAs must have block scope
            if !vla_sizes.is_empty() {
                return Err(ParseError::new(
                    "variable length arrays cannot have file scope".to_string(),
                    self.current_pos(),
                ));
            }

            // Validate explicit alignment for this declarator's type (C11 6.7.5)
            decl_type = self.apply_pending_type_attrs(decl_type);
            let decl_validated_align = self.validated_explicit_align(decl_type)?;

            // Bind variable to symbol table BEFORE parsing initializer (C99 6.2.1p7)
            let mut decl_symbol = if is_typedef {
                None // Will be bound after initializer parsing
            } else {
                self.check_redeclaration(decl_name, decl_type, decl_pos);
                let var_sym = self.declared_symbol(decl_name, decl_type, decl_validated_align);
                Some(match self.symbols.declare(var_sym) {
                    Ok(id) => id,
                    Err(_) => self
                        .symbols
                        .lookup_id(decl_name, Namespace::Ordinary)
                        .expect("redeclaration should find existing symbol"),
                })
            };

            let decl_init = if self.is_special(b'=') {
                if is_typedef {
                    return Err(ParseError::new(
                        "typedef cannot have initializer",
                        self.current_pos(),
                    ));
                }
                self.advance();
                Some(self.parse_initializer()?)
            } else {
                None
            };

            // For incomplete array types, infer size from initializer
            if let Some(ref init_expr) = decl_init {
                let old_type = decl_type;
                decl_type = self.infer_array_size_from_init(decl_type, init_expr);
                // 6.7.9p5 again, at file scope: the declaration *is* a
                // definition with external linkage, so `extern` adds nothing
                // and gcc warns rather than rejecting.
                if base_type.modifiers.contains(TypeModifiers::EXTERN) {
                    diag::warning(
                        init_expr.pos,
                        &gettext("'extern' variable has an initializer"),
                    );
                }
                self.check_excess_initializers(decl_type, init_expr);
                self.check_initializer_types(decl_type, init_expr);

                // If the type changed (array size was inferred), update the symbol's type
                // This is needed because the symbol was already added before parsing the initializer
                if decl_type != old_type {
                    if let Some(sym_id) = decl_symbol {
                        self.symbols.get_mut(sym_id).typ = decl_type;
                    }
                }
            }

            // Bind typedef to symbol table (after parsing initializer, which is forbidden anyway)
            if is_typedef {
                self.check_typedef_redefinition(decl_name, decl_type, decl_pos);
                let sym = Symbol::typedef(decl_name, decl_type, self.symbols.depth());
                decl_symbol = Some(match self.symbols.declare(sym) {
                    Ok(id) => id,
                    Err(_) => self
                        .symbols
                        .lookup_id(decl_name, Namespace::Ordinary)
                        .expect("redeclaration should find existing symbol"),
                });
            }

            self.settle_declaration_facts(decl_name, storage_class);
            declarators.push(InitDeclarator {
                symbol_attrs: std::mem::take(&mut self.pending_symbol_attrs),
                symbol: decl_symbol.expect("symbol should be bound"),
                typ: decl_type,
                storage_class,
                init: decl_init,
                vla_sizes: vec![],
                explicit_align: decl_validated_align,
                pos: next_decl_pos,
            });
        }
        Ok(())
    }
}
