//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Translation-unit and external-declaration parsing: function definitions
// and _Static_assert
//

use super::ast::{
    ExternalDecl, FunctionAttrs, FunctionDef, ParamStyle, Parameter, Stmt, TranslationUnit,
};
use super::bind::{DeclScope, DeclSpecs};
use super::declaration::SpecContext;
use super::parser::{
    DeclaratorContext, EnclosingFunction, ParseError, ParseResult, ParsedDeclarator, Parser,
    RawParam,
};
use crate::diag;
use crate::strings::StringId;
use crate::symbol::Symbol;
use crate::token::lexer::{payload_text, Position, TokenType, TokenValue};
use crate::types::{Type, TypeId, TypeKind, TypeModifiers};

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
        self.current_ident()
            .is_some_and(|id| crate::kw::has_tag(id, crate::kw::ASSERT_KW))
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

    /// Parse a function body, recording what the variadic builtins in it
    /// need to know of the function (see [`EnclosingFunction`]).
    fn parse_function_body(
        &mut self,
        attrs: &FunctionAttrs,
        typ: TypeId,
        last_param: Option<StringId>,
    ) -> ParseResult<Stmt> {
        let func = self.types.get(typ);
        let is_variadic = func.variadic;
        let outer = std::mem::replace(
            &mut self.enclosing_function,
            EnclosingFunction {
                variadic: is_variadic,
                forwarding: is_variadic && attrs.always_inline,
                last_param,
                conv: func.conv,
                return_type: func.base,
            },
        );
        let body = self.parse_block_stmt_no_scope();
        self.enclosing_function = outer;
        body
    }

    /// Record the attributes pending for a function declarator under its name.
    ///
    /// A function's attributes are gathered across all of its declarations
    /// and read back by name -- by its definition, by `__alignof__`, and by
    /// the type of a later declaration, which is what makes a prototype's
    /// `noreturn` reach a call through a redeclaration.
    pub(super) fn accumulate_fn_attrs(&mut self, name: StringId) -> FunctionAttrs {
        // An `aligned` written before the declaration specifiers or after the
        // declarator arrives on the object channels, since the parser cannot
        // yet tell it is declaring a function; the `_Alignas` keyword on the
        // same channel has already been refused by `reject_alignas_in`. Fold
        // both in here, where it is known to be one.
        let declared_align = self
            .pending_alignas
            .max(self.pending_declarator_align.take());
        self.pending_fn_attrs.align = self.pending_fn_attrs.align.max(declared_align);
        let pending = self.pending_fn_attrs.clone();
        let seen = self.declared_fn_attrs.entry(name).or_default();
        seen.merge(&pending);
        seen.clone()
    }

    /// Clear the `pending_*` state a previous declaration may have left
    /// behind, at either scope: a declaration inside a function body
    /// otherwise started from the enclosing definition's attributes, so a
    /// local `void m(void);` inherited its `noinline` or `aligned`.
    pub(super) fn reset_pending_declaration_state(&mut self) {
        // Clear pending alignment from previous declaration
        self.pending_alignas = None;
        self.pending_alignas_kw = None;
        // A mode that no declarator consumed belongs to no later declaration:
        // leaving it set applied it to whatever came next.
        self.pending_mode = None;
        self.pending_transparent_union = None;
        self.pending_packed = false;
        self.pending_fn_attrs = Default::default();
        self.pending_calling_conv = None;
        // And any asm label the previous declaration left behind.
        //
        // `skip_extensions` collects one wherever it runs, which is most
        // places a type specifier can appear, but only a declarator with
        // linkage claims one -- so a label written on a struct definition, or
        // in a shape this parser does not model at all, stays pending and is
        // claimed by whatever is declared next. The symptom is an unrelated
        // global emitted under someone else's assembler name.
        self.pending_asm_label = None;
        // Likewise the symbol attributes. A function *definition* takes its
        // attributes through `pending_fn_attrs` and leaves these behind, so the
        // next declaration to build an `InitDeclarator` claimed them: the
        // variable after a `section(...)` function was emitted into that
        // function's section, and the conflicting "ax"/"aw" flags made the
        // assembler reject the file outright.
        self.pending_symbol_attrs = Default::default();
    }

    pub(crate) fn parse_external_decl(&mut self) -> ParseResult<ExternalDecl> {
        if self.at_attribute_declaration() {
            return self.parse_file_attribute_declaration();
        }
        self.parse_declaration(DeclScope::File)
    }

    /// Define the function the first declarator of an external declaration
    /// declares: its K&R declaration list if it has one, then its body.
    ///
    /// `typ` is the declarator's function type, already carrying `noreturn`;
    /// `attrs` the function's attributes accumulated over every declaration of
    /// its name.
    pub(super) fn define_function(
        &mut self,
        specs: &DeclSpecs,
        name: StringId,
        pos: Position,
        typ: TypeId,
        params: Option<Vec<RawParam>>,
        attrs: FunctionAttrs,
    ) -> ParseResult<FunctionDef> {
        let mut params = params.unwrap_or_default();
        let typ = self.parse_old_style_parameters(typ, &mut params)?;
        let func = self.types.get(typ);
        // An identifier list records no parameter types (C17 6.7.6.3p14).
        let param_style = if func.params.is_some() {
            ParamStyle::Prototype
        } else {
            ParamStyle::IdentifierList
        };
        let return_type = func.base.expect("a function type has a return type");

        self.check_redeclaration(name, typ, pos);
        let _ = self
            .symbols
            .declare(Symbol::function(name, typ, self.symbols.depth()));
        // A weak definition may be replaced at link time, so gcc leaves the
        // builtin in place of it; so does this.
        if !attrs.symbol.weak {
            self.defined_functions.insert(name);
        }
        // A definition binds a fresh symbol, so the facts accumulated over
        // every declaration of the name are settled onto it -- without them
        // its C99 6.7.4p6 inline classification is computed from declarations
        // it never saw.
        self.settle_declaration_facts(name, specs.storage_class);

        // Bind the parameters in the function scope: each re-declares the
        // symbol its parameter list made, since that is what any variably
        // modified extent in a later parameter already resolved against.
        self.symbols.enter_scope();
        let last_param = params
            .last()
            .and_then(|raw| raw.symbol)
            .map(|id| self.symbols.get(id).name);
        let params = params
            .iter()
            .map(|raw| Parameter {
                symbol: raw.symbol.map(|id| self.symbols.redeclare(id, raw.typ)),
                typ: raw.typ,
                vm_dims: raw.vm_dims.clone(),
                discarded_dims: raw.discarded_dims.clone(),
            })
            .collect();
        // Parse body without creating another scope
        let body = self.parse_function_body(&attrs, typ, last_param)?;
        self.symbols.leave_scope();

        Ok(FunctionDef {
            return_type,
            name,
            params,
            param_style,
            body,
            pos: specs.pos,
            is_static: specs.storage_class.contains(TypeModifiers::STATIC),
            is_inline: specs.storage_class.contains(TypeModifiers::INLINE),
            calling_conv: self.types.get(typ).conv,
            attrs,
        })
    }

    /// K&R (old-style) parameter declarations, between the declarator and the
    /// body: `int add(a, b) int a; int b; { ... }`.
    ///
    /// The identifier list gave every name an implicit `int`; the declarations
    /// give them their real types. A prototype's type records its parameter
    /// types, so it is rebuilt from the patched ones; an identifier list's
    /// records none (C17 6.7.6.3p14).
    fn parse_old_style_parameters(
        &mut self,
        typ: TypeId,
        params: &mut [RawParam],
    ) -> ParseResult<TypeId> {
        let mut declared = false;
        while self.is_declaration_start() {
            declared = true;
            let knr_pos = self.current_pos();
            let knr_type = self
                .parse_declaration_specifiers(SpecContext::Declaration)?
                .ty;
            let knr_base_id = self.intern_type_with_tag(&knr_type);
            loop {
                let ParsedDeclarator {
                    name: decl_name,
                    typ: mut decl_typ,
                    ..
                } = self.parse_declarator(knr_base_id, DeclaratorContext::OldStyleParameter)?;
                self.check_parameter_specifiers(knr_type.modifiers, decl_name, knr_pos);
                self.check_not_vector_value(Some(decl_typ), self.current_pos());
                // C99 6.7.5.3: array/function params adjusted to pointers
                let adjusted = self.types.get(decl_typ);
                if adjusted.kind == TypeKind::Array {
                    let elem = adjusted.base.unwrap_or(self.types.void_id);
                    decl_typ = self.types.intern(Type {
                        kind: TypeKind::Pointer,
                        base: Some(elem),
                        ..Default::default()
                    });
                } else if adjusted.kind == TypeKind::Function {
                    decl_typ = self.types.intern(Type {
                        kind: TypeKind::Pointer,
                        base: Some(decl_typ),
                        ..Default::default()
                    });
                }
                // The identifier list gave this name an implicit `int`; its
                // real type arrives here, so this is the only place the
                // stack-slot bound can be asked of it.
                // `parse_parameter_list_inner` never saw it.
                self.check_stack_object_size(decl_typ, self.current_pos(), "a by-value parameter")?;
                for param in params.iter_mut() {
                    if param.name == Some(decl_name) {
                        param.typ = decl_typ;
                    }
                }
                if !self.is_special(b',') {
                    break;
                }
                self.advance();
            }
            self.expect_special(b';')?;
        }

        let mut func = self.types.get(typ).clone();
        if !declared || func.params.is_none() {
            return Ok(typ);
        }
        func.params = Some(params.iter().map(|p| p.typ).collect());
        Ok(self.types.intern(func))
    }
}
