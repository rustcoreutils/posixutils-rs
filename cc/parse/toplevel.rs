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
    ExprKind, ExternalDecl, FunctionAttrs, FunctionDef, ParamStyle, Parameter, Stmt,
    TranslationUnit,
};
use super::bind::{DeclScope, DeclSpecs};
use super::declaration::{Redeclared, SpecContext};
use super::linkage::Declared;
use super::parser::{
    DeclaratorContext, EnclosingFunction, ParseError, ParseResult, ParsedDeclarator, Parser,
    RawParam,
};
use crate::diag;
use crate::strings::StringId;
use crate::symbol::{Symbol, SymbolId};
use crate::token::lexer::{payload_text, Position, TokenType};
use crate::types::{Type, TypeId, TypeKind, TypeModifiers};
use gettextrs::gettext;
use std::collections::HashMap;

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
        self.complete_tentative_arrays(&mut tu);

        Ok(tu)
    }

    /// C17 6.9.2p2: an array defined without its extent -- `static int a[];`
    /// -- may be completed by a later declaration in the unit, and only one
    /// still incomplete at its end is an array of one element, which gcc
    /// assumes with a warning. The warning is given here, at the object's
    /// last definition, since the declaration itself cannot know what
    /// follows it.
    ///
    /// Every declarator of the object then takes its final type, since any
    /// of them may be the one whose storage is emitted: `int a[]; int a[3];`
    /// reserved the first declaration's size, and `a[2]` overlapped the next
    /// object.
    fn complete_tentative_arrays(&mut self, tu: &mut TranslationUnit) {
        let recorded = std::mem::take(&mut self.tentative_arrays);
        if recorded.is_empty() {
            return;
        }
        // Each object once, in declaration order, at its last definition.
        let mut last: HashMap<SymbolId, Position> = HashMap::new();
        let mut order: Vec<SymbolId> = Vec::new();
        for (id, pos) in recorded {
            if last.insert(id, pos).is_none() {
                order.push(id);
            }
        }
        for &id in &order {
            let typ = self.symbols.get(id).typ;
            if self.types.unsized_array_levels(typ) == 0 {
                continue;
            }
            let name = self.symbols.get(id).name;
            let spelled = self.idents.get_opt(name).unwrap_or("");
            // A block-scope `extern` may have given the object its extent.
            let completed = match self.linked_type(name) {
                Some((linked, late)) if self.types.unsized_array_levels(linked) == 0 => {
                    if late && self.types.get(linked).array_size != Some(1) {
                        diag::error_args(
                            last[&id],
                            "type of array '{0}' completed incompatibly with implicit initialization",
                            &[spelled],
                        );
                    }
                    self.types.without_decl_specifiers(linked)
                }
                _ => {
                    diag::warning_args(
                        last[&id],
                        "array '{0}' assumed to have one element",
                        &[spelled],
                    );
                    let elem = self.types.base_type(typ).unwrap_or(self.types.int_id);
                    self.types.intern(Type::array(elem, 1))
                }
            };
            self.symbols.get_mut(id).typ = completed;
        }
        for item in &mut tu.items {
            let ExternalDecl::Declaration(decl) = item else {
                continue;
            };
            for d in &mut decl.declarators {
                if last.contains_key(&d.symbol) && self.types.unsized_array_levels(d.typ) > 0 {
                    d.typ = self.symbols.get(d.symbol).typ;
                }
            }
        }
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
            // A qualified spelling -- `volatile struct S` -- is its own
            // `TypeId`, completed along with the tag (`complete_struct`), so
            // `struct S; volatile struct S vs; struct S { int a; };` is
            // complete by now.
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
                            // A string literal, which translation phase 6 has already made of
                            // any adjacent ones: `"a" "b"` is one literal, as the
                            // `BUILD_BUG_ON`-style macros that paste a message together rely
                            // on. Any encoding prefix is allowed; only a narrow message is
                            // shown.
            if !matches!(
                self.peek(),
                TokenType::String
                    | TokenType::WideString
                    | TokenType::Utf16String
                    | TokenType::Utf32String
            ) {
                return Err(ParseError::new(
                    "expected string literal in _Static_assert",
                    self.current_pos(),
                ));
            }
            match self.parse_string_literal_run()?.kind {
                ExprKind::StringLit(bytes) => payload_text(&bytes),
                _ => String::new(),
            }
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
        name: StringId,
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
                name: Some(name),
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
        self.check_parameters_complete(&params, pos);
        self.check_identifier_list_against_prototype(name, typ, &params, pos);
        let func = self.types.get(typ);
        // An identifier list records no parameter types (C17 6.7.6.3p14).
        let param_style = if func.params.is_some() {
            ParamStyle::Prototype
        } else {
            ParamStyle::IdentifierList
        };
        let return_type = func.base.expect("a function type has a return type");
        // C17 6.9.1p3: a definition returns `void` or a complete object
        // type, since its `return` makes one.
        if self.types.kind(return_type) != TypeKind::Void
            && self.type_name_is_incomplete(return_type, 0)
        {
            diag::error(pos, &gettext("return type is an incomplete type"));
        }

        let form = match param_style {
            ParamStyle::Prototype => Redeclared::Declaration,
            ParamStyle::IdentifierList => Redeclared::IdentifierListDefinition,
        };
        self.check_redeclaration(name, typ, pos, form);
        // A GNU inline-only body -- `extern inline` under `gnu_inline`
        // semantics -- emits nothing, so a real definition may join it.
        let gnu_inline = attrs.gnu_inline || crate::builtins::gnu89_inline();
        let inline_only = gnu_inline
            && specs
                .storage_class
                .contains(TypeModifiers::EXTERN | TypeModifiers::INLINE);
        let linkage = self.declare_linkage(Declared {
            name,
            typ,
            pos,
            storage: specs.storage_class,
            scope: DeclScope::File,
            defines: true,
            inline_only,
        });
        let _ = self
            .symbols
            .declare(Symbol::function(name, typ, self.symbols.depth()).with_linkage(linkage));
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
        let body = self.parse_function_body(name, &attrs, typ, last_param)?;
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

    /// C17 6.9.1p7: in a definition, each parameter has a complete object
    /// type after adjustment, since the function body has an object for it.
    /// A prototype that is not a definition may name an incomplete type.
    fn check_parameters_complete(&self, params: &[RawParam], pos: Position) {
        for (i, raw) in params.iter().enumerate() {
            if self.types.kind(raw.typ) == TypeKind::Void
                || !self.type_name_is_incomplete(raw.typ, 0)
            {
                continue;
            }
            let n = (i + 1).to_string();
            let name = raw
                .name
                .and_then(|id| self.idents.get_opt(id))
                .unwrap_or("");
            diag::error_args(
                pos,
                "parameter {0} ('{1}') has incomplete type",
                &[&n, name],
            );
        }
    }

    /// C17 6.7.6.3p15, 6.9.1p7: a definition with an identifier list after a
    /// prototype takes as many parameters as the prototype, each of a type
    /// compatible with the prototype's once the default argument promotions
    /// are applied -- `int f(int); int f(a) double a; { ... }` called `f`
    /// with an `int` and read a `double`. gcc's errors.
    fn check_identifier_list_against_prototype(
        &mut self,
        name: StringId,
        typ: TypeId,
        params: &[RawParam],
        pos: Position,
    ) {
        if self.types.get(typ).params.is_some() {
            return;
        }
        let Some(prior) = self
            .symbols
            .lookup(name, crate::symbol::Namespace::Ordinary)
        else {
            return;
        };
        let Some(proto) = self.types.get(prior.typ).params.clone() else {
            return;
        };
        if proto.len() != params.len() {
            diag::error(pos, &gettext("number of arguments doesn't match prototype"));
            return;
        }
        for (declared, raw) in proto.iter().zip(params) {
            let declared = self.types.decayed(*declared);
            let promoted = self.types.default_argument_promote(raw.typ);
            if self.types.types_compatible(declared, raw.typ)
                || self.types.types_compatible(declared, promoted)
            {
                continue;
            }
            let spelled = raw
                .name
                .and_then(|n| self.idents.get_opt(n))
                .unwrap_or("")
                .to_string();
            diag::error_args(pos, "argument '{0}' doesn't match prototype", &[&spelled]);
        }
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
        // C17 6.9.1p6: each declaration declares an identifier in the list,
        // once, and every identifier in the list is declared.
        let identifier_list = self.types.get(typ).params.is_none();
        let mut seen: Vec<StringId> = Vec::new();
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
                if identifier_list {
                    let spelled = self.idents.get_opt(decl_name).unwrap_or("").to_string();
                    if !params.iter().any(|p| p.name == Some(decl_name)) {
                        diag::error_args(
                            knr_pos,
                            "declaration for parameter '{0}' but no such parameter",
                            &[&spelled],
                        );
                    } else if seen.contains(&decl_name) {
                        diag::error_args(knr_pos, "redefinition of parameter '{0}'", &[&spelled]);
                    } else if self.types.kind(decl_typ) == TypeKind::Void {
                        diag::error_args(
                            knr_pos,
                            "parameter '{0}' declared with void type",
                            &[&spelled],
                        );
                    }
                    seen.push(decl_name);
                }
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

        // An identifier the declarations left out is `int`, the C89 rule
        // gcc keeps with a warning. C17 6.9.1p6 requires the declaration,
        // but as a semantic rule, not a constraint.
        if identifier_list {
            for param in params.iter() {
                let Some(name) = param.name.filter(|n| !seen.contains(n)) else {
                    continue;
                };
                let spelled = self.idents.get_opt(name).unwrap_or("").to_string();
                diag::warning(
                    self.current_pos(),
                    &format!("type of '{spelled}' defaults to 'int'"),
                );
            }
        }

        let mut func = self.types.get(typ).clone();
        if !declared || func.params.is_none() {
            return Ok(typ);
        }
        func.params = Some(params.iter().map(|p| p.typ).collect());
        Ok(self.types.intern(func))
    }
}
