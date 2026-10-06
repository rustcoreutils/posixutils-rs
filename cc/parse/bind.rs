//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Declarations (C17 6.7): the one path from a declaration's specifiers and
// declarators to the symbols they bind, at file scope and block scope alike
//

use super::ast::{
    Declaration, Expr, ExprKind, ExternalDecl, FunctionAttrs, InitDeclarator, UnaryOp,
};
use super::attribute::SpecifierAttrs;
use super::declaration::{Redeclared, SpecContext};
use super::linkage::Declared;
use super::parser::{DeclaratorContext, ParseError, ParseResult, ParsedDeclarator, Parser};
use crate::diag;
use crate::strings::StringId;
use crate::symbol::{Namespace, Symbol, SymbolId};
use crate::token::lexer::Position;
use crate::types::{Type, TypeId, TypeKind, TypeModifiers};
use gettextrs::gettext;

/// Where a declaration is written, which settles several of its rules.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum DeclScope {
    /// An external declaration (C17 6.9): a function may be defined here, an
    /// object definition may be tentative (6.9.2p3), and nothing may be
    /// variably modified (6.7.6.2p2).
    File,
    /// Inside a function body. `for_init` is the first clause of a `for`,
    /// which 6.8.5p3 limits to objects with `auto` or `register` storage.
    Block { for_init: bool },
}

/// The declaration specifiers every declarator of one declaration shares.
pub(super) struct DeclSpecs {
    /// Where the declaration starts, which is a function definition's
    /// position.
    pub(super) pos: Position,
    /// The specified type, still carrying the storage-class and function
    /// specifiers.
    ty: Type,
    /// The type every declarator derives from, or `__auto_type`.
    base: DeclBase,
    /// The storage-class specifiers and `inline` ([`Type::STORAGE_CLASS`]),
    /// which the declaration records rather than the type.
    pub(super) storage_class: TypeModifiers,
    /// The attributes written among the specifiers, which every declarator
    /// gets.
    attrs: SpecifierAttrs,
    /// The extents of the variably modified levels the specifiers introduced
    /// -- through a variably modified typedef name or `typeof(int[n])` --
    /// which are the innermost levels of every declarator's type.
    vm_dims: Vec<Expr>,
}

/// The type a declaration's declarators derive from.
#[derive(Clone, Copy)]
enum DeclBase {
    /// The type the specifiers name, interned.
    Given(TypeId),
    /// `__auto_type`, written at `pos`: the declaration's one declarator
    /// takes the type of its initializer ([`Parser::infer_auto_type`]).
    /// `placeholder` is the specifiers' type, `int` standing in for the data
    /// type; the declarator is parsed against it, so one that derives
    /// anything from it is told from a plain identifier.
    Inferred { pos: Position, placeholder: TypeId },
}

impl DeclBase {
    /// The type the declarator is parsed against.
    fn parse_against(self) -> TypeId {
        match self {
            DeclBase::Given(typ) => typ,
            DeclBase::Inferred { placeholder, .. } => placeholder,
        }
    }
}

impl DeclSpecs {
    fn is_typedef(&self) -> bool {
        self.storage_class.contains(TypeModifiers::TYPEDEF)
    }

    fn is_extern(&self) -> bool {
        self.storage_class.contains(TypeModifiers::EXTERN)
    }
}

/// What binding one declarator produced.
enum Bound {
    Decl(InitDeclarator),
    /// Only ever the first declarator at file scope: `int f(void) { .. }`.
    FuncDef(super::ast::FunctionDef),
    /// A block-scope redeclaration the symbol table refused, which binds
    /// nothing new.
    Nothing,
}

/// A `cleanup(func)` written on the declarator of `var`, whose type is `typ`.
struct Cleanup {
    func: SymbolId,
    var: SymbolId,
    typ: TypeId,
    pos: Position,
}

/// No automatic storage duration, so no stack slot to size.
const NO_AUTO_DURATION: TypeModifiers = TypeModifiers::STATIC
    .union(TypeModifiers::EXTERN)
    .union(TypeModifiers::THREAD_LOCAL)
    .union(TypeModifiers::TYPEDEF);

impl Parser<'_> {
    /// Parse one declaration and bind every name it declares -- at file
    /// scope, possibly a function definition.
    ///
    /// The one declaration parser. File scope used to have five hand-written
    /// binders -- the first plain declarator, a grouped one, a function
    /// declarator, the declarators after the first -- and block scope a sixth,
    /// and each check lived in some of them: a later declarator skipped the
    /// incomplete-element check, a grouped one never inferred an array size
    /// from its initializer, only the first function declarator carried
    /// `noreturn` into its type. Every declarator of every declaration now
    /// takes [`Self::bind_declarator`].
    pub(super) fn parse_declaration(&mut self, scope: DeclScope) -> ParseResult<ExternalDecl> {
        let nothing = || {
            Ok(ExternalDecl::Declaration(Declaration {
                declarators: vec![],
            }))
        };
        self.reset_pending_declaration_state();

        if self.is_static_assert() {
            self.parse_static_assert()?;
            return nothing();
        }

        // A stray `;` is an empty declaration. C17 6.7p2 makes it a
        // constraint violation, but GCC and Clang accept it by default (they
        // warn only under -pedantic) and it is common in real source: any
        // function-like macro that expands to nothing and is invoked with a
        // trailing semicolon produces one. CPython's `_Py_DECLARE_STR()` is
        // exactly that. Consume it before reaching the type specifier, which
        // would otherwise report a spurious "type specifier missing".
        if self.is_special(b';') {
            self.advance();
            return nothing();
        }

        let mut specs = self.parse_decl_specs(scope)?;
        if scope == (DeclScope::Block { for_init: true }) {
            self.check_for_init_declares_no_tag(&specs);
        }
        let mut declarators = Vec::new();
        if let DeclScope::Block { .. } = scope {
            // At file scope a variably modified specifier is refused with the
            // declarator, so only a block binds its extents.
            let dims = std::mem::take(&mut specs.vm_dims);
            specs.vm_dims = self.bind_specifier_extents(
                dims,
                specs.base.parse_against(),
                specs.pos,
                &mut declarators,
            );
        }

        // `struct point { int x; };` -- a tag, and no declarators.
        if self.is_special(b';') {
            match specs.base {
                DeclBase::Inferred { pos, .. } => {
                    diag::error(pos, &gettext("'__auto_type' in empty declaration"));
                }
                DeclBase::Given(_) => self.check_declares_something(specs.pos, &specs.ty),
            }
        } else {
            let mut first = true;
            loop {
                let d = self
                    .parse_declarator(specs.base.parse_against(), DeclaratorContext::Declaration)?;
                // An attribute may follow any declarator, and so may an asm
                // label.
                self.skip_extensions_after_declarator();
                match self.bind_declarator(&specs, d, scope, first)? {
                    Bound::FuncDef(def) => return Ok(ExternalDecl::FunctionDef(def)),
                    Bound::Decl(d) => declarators.push(d),
                    Bound::Nothing => {}
                }
                if !self.is_special(b',') {
                    break;
                }
                self.advance();
                first = false;
                self.begin_declarator(&specs.attrs);
                // An attribute may also come *before* a declarator in the
                // list -- `int a, __attribute__((unused)) b;` -- where it
                // belongs to that declarator.
                self.skip_extensions_after_declarator();
            }
        }

        self.expect_special(b';')?;
        self.end_declaration();
        Ok(ExternalDecl::Declaration(Declaration { declarators }))
    }

    /// C17 6.8.5p3: the declaration in a `for` declares objects only, so a
    /// tag it declares -- `for (struct S { int a; } s;;)` -- is refused. The
    /// `for` has a scope of its own, so a tag in it was declared right here.
    fn check_for_init_declares_no_tag(&self, specs: &DeclSpecs) {
        let keyword = match specs.ty.kind {
            TypeKind::Struct => "struct",
            TypeKind::Union => "union",
            TypeKind::Enum => "enum",
            _ => return,
        };
        let Some(tag) = specs.ty.composite.as_ref().and_then(|c| c.tag) else {
            return;
        };
        if self.symbols.lookup_tag_in_current_scope(tag).is_some() {
            diag::error_args(
                specs.pos,
                "'{0} {1}' declared in 'for' loop initial declaration",
                &[keyword, self.idents.get_opt(tag).unwrap_or("")],
            );
        }
    }

    /// Parse the declaration specifiers and check what they may combine.
    fn parse_decl_specs(&mut self, scope: DeclScope) -> ParseResult<DeclSpecs> {
        let pos = self.current_pos();
        let parsed = self.parse_declaration_specifiers(SpecContext::Declaration)?;
        // A declaration that stops right here declares nothing, and that --
        // not a missing type specifier -- is what to report.
        if !self.is_special(b';') {
            self.check_implicit_int(parsed.explicit, pos);
        }
        // Skip __attribute__ between type and declarator (GCC extension)
        self.skip_extensions();

        let ty = parsed.ty;
        self.check_storage_class_combination(ty.modifiers, scope)?;
        // C11 6.7.5p2 -- both facts are already in the specifier modifiers.
        if ty.modifiers.contains(TypeModifiers::TYPEDEF) {
            self.reject_alignas_in("a typedef");
        } else if ty.modifiers.contains(TypeModifiers::REGISTER) {
            self.reject_alignas_in("an object with register storage");
        }
        // For struct/union types with tags, use existing TypeId to preserve
        // forward declarations
        let interned = self.intern_type_with_tag(&ty);
        let base = match parsed.inferred {
            Some(pos) => DeclBase::Inferred {
                pos,
                placeholder: interned,
            },
            None => DeclBase::Given(interned),
        };
        Ok(DeclSpecs {
            pos,
            storage_class: ty.modifiers & Type::STORAGE_CLASS,
            base,
            attrs: self.specifier_attrs(),
            vm_dims: parsed.vm_dims,
            ty,
        })
    }

    /// The storage classes a declaration may not combine, whatever its scope.
    fn check_storage_class_combination(
        &self,
        modifiers: TypeModifiers,
        scope: DeclScope,
    ) -> ParseResult<()> {
        // C17 6.8.5p3: the declaration in a `for` declares objects with
        // `auto` or `register` storage only.
        if scope == (DeclScope::Block { for_init: true }) {
            for (bit, what) in [
                (TypeModifiers::STATIC, "static"),
                (TypeModifiers::EXTERN, "extern"),
                (TypeModifiers::THREAD_LOCAL, "thread-local"),
            ] {
                if modifiers.contains(bit) {
                    return Err(ParseError::new(
                        format!("declaration of {what} variable in for loop initial declaration"),
                        self.current_pos(),
                    ));
                }
            }
        }
        // C11 6.7.1p2: _Thread_local shall not appear in a declaration with
        // auto or register -- nor with typedef, which is a storage class too.
        if modifiers.contains(TypeModifiers::THREAD_LOCAL) {
            for (bit, what) in [
                (TypeModifiers::AUTO, "auto"),
                (TypeModifiers::REGISTER, "register"),
                (TypeModifiers::TYPEDEF, "typedef"),
            ] {
                if modifiers.contains(bit) {
                    return Err(ParseError::new(
                        format!("_Thread_local cannot be combined with {what}"),
                        self.current_pos(),
                    ));
                }
            }
        }
        Ok(())
    }

    /// Clear what only the declaration just finished could claim. A mode, a
    /// `transparent_union` or an alignment that no declarator consumed
    /// belongs to no later declaration: leaving it set applied it to
    /// whatever came next.
    fn end_declaration(&mut self) {
        self.pending_alignas = None;
        self.pending_alignas_kw = None;
        self.pending_mode = None;
        self.pending_transparent_union = None;
        self.pending_packed = false;
    }

    /// Bind one declarator of a declaration: the rules of C17 6.7 in one
    /// order, whichever position the declarator holds.
    fn bind_declarator(
        &mut self,
        specs: &DeclSpecs,
        d: ParsedDeclarator,
        scope: DeclScope,
        first: bool,
    ) -> ParseResult<Bound> {
        let ParsedDeclarator {
            name,
            pos,
            mut typ,
            mut vla,
            vla_pos,
            params,
            ..
        } = d;
        let is_typedef = specs.is_typedef();

        // `__auto_type`: the initializer is parsed first, for its type.
        let mut inferred_init = None;
        if let DeclBase::Inferred {
            pos: auto_pos,
            placeholder,
        } = specs.base
        {
            let (inferred, init) =
                self.infer_auto_type(specs, auto_pos, typ == placeholder, first)?;
            typ = inferred;
            inferred_init = Some(init);
        }

        // The specifiers' extents are the innermost levels, after the
        // declarator's own -- the ordering `try_parse_type_name_vm` applies
        // to a type-name's declarator and specifier levels.
        vla.extend(specs.vm_dims.iter().cloned());
        // C17 6.7.6.2p2: an ordinary identifier with a variably modified
        // type has block scope. So has a typedef of one (6.7.7p3).
        if scope == DeclScope::File && !vla.is_empty() {
            return Err(ParseError::new(
                "variable length arrays cannot have file scope".to_string(),
                vla_pos.unwrap_or(specs.pos),
            ));
        }

        let is_fn = self.types.kind(typ) == TypeKind::Function;
        self.check_derived_types(typ, name, pos);
        if !is_typedef {
            self.check_storage_for_declarator(specs, name, is_fn, scope, pos);
        }
        let fn_attrs = if is_fn {
            self.function_declarator_attrs(specs, name, &mut typ)
        } else {
            None
        };

        // A mode or vector size replaces the type; alignment then attaches to
        // whatever the type ended up being.
        typ = self.apply_pending_type_attrs(typ);
        let align = self.validated_explicit_align(typ)?;

        // A function definition is the only declarator of an external
        // declaration, and its body -- or a K&R declaration list -- follows
        // the declarator directly (C17 6.9.1).
        if let Some(attrs) = &fn_attrs {
            let body_follows = self.is_special(b'{') || self.is_declaration_start();
            if scope == DeclScope::File && first && body_follows {
                // C17 6.9.1p2: the declarator itself gives a definition its
                // function type; one that came from a typedef has no
                // parameter list for the body to name.
                if params.is_none() {
                    diag::error(
                        pos,
                        &gettext("function definition declared through a typedef"),
                    );
                }
                // A definition's parameters are in its block's scope, not a
                // prototype's, so `[*]` has no business there (6.7.6.2p4).
                if let Some(star) = params.as_ref().and_then(|list| list.star) {
                    diag::error(
                        star,
                        &gettext("'[*]' not allowed in other than function prototype scope"),
                    );
                }
                let params = params.map(|list| list.params);
                let def = self.define_function(specs, name, pos, typ, params, attrs.clone())?;
                return Ok(Bound::FuncDef(def));
            }
        }

        let (symbol, init) = if is_typedef {
            if scope == (DeclScope::Block { for_init: true }) {
                diag::error_args(
                    pos,
                    "declaration of non-variable '{0}' in 'for' loop initial declaration",
                    &[self.idents.get_opt(name).unwrap_or("")],
                );
            }
            typ = self.align_typedef_type(typ, align);
            (self.bind_typedef_name(scope, name, pos, typ, &vla)?, None)
        } else {
            // C17 6.2.7p4: two declarations of one object with linkage
            // describe it by their composite type.
            typ = self.composite_with_prior_declaration(name, typ, specs.storage_class);
            self.check_redeclaration(name, typ, pos, Redeclared::Declaration);
            let linkage = self.declare_linkage(Declared {
                name,
                typ,
                pos,
                storage: specs.storage_class,
                scope,
                // An initializer makes an object's declaration a definition
                // (6.9.2p1); one without is at most a tentative definition.
                defines: !is_fn && self.is_special(b'='),
                gnu_extern_inline: fn_attrs
                    .as_ref()
                    .is_some_and(|attrs| attrs.gnu_inline_only(specs.storage_class)),
            });
            let sym = self
                .declared_symbol(name, typ, align)
                .with_variably_modified_array(!vla.is_empty())
                .with_linkage(linkage);
            // Bound before the initializer: C99 6.2.1p7 starts the scope just
            // after the declarator, so `int *p = sizeof *p ...` sees `p`.
            let symbol = self.declare_in(scope, sym, name);
            let init = match inferred_init {
                Some(init) => Some(self.settle_initializer(specs, scope, &mut typ, symbol, init)),
                None => {
                    self.parse_declarator_initializer(specs, scope, name, pos, &mut typ, symbol)?
                }
            };
            if !is_fn && !specs.is_extern() {
                self.check_object_complete(scope, name, typ, &vla, pos);
            }
            // A tentative definition of an array without its extent, which a
            // later declaration in the unit may still supply (6.9.2p2).
            if scope == DeclScope::File
                && init.is_none()
                && !specs.is_extern()
                && self.types.kind(typ) == TypeKind::Array
                && self.types.unsized_array_levels(typ) > 0
            {
                if let Some(id) = symbol {
                    self.tentative_arrays.push((id, pos));
                }
            }
            if !is_fn && !vla.is_empty() {
                self.check_variably_modified_storage(specs.storage_class, name, typ, pos);
            }
            (symbol, init)
        };

        match scope {
            DeclScope::File => self.settle_declaration_facts(name, specs.storage_class),
            // A block-scope declaration with linkage names the same object or
            // function a file-scope one would, under the same assembler name.
            DeclScope::Block { .. } if is_fn || specs.is_extern() => self.settle_asm_label(name),
            // One without names nothing an asm label could rename, and must
            // not hand its label to the next declarator of the list.
            DeclScope::Block { .. } => self.pending_asm_label = None,
        }

        let Some(symbol) = symbol else {
            return Ok(Bound::Nothing);
        };
        // Checked after the initializer, which is what can still infer the
        // extent of `char a[] = { .. }`. `register` does not exempt an array:
        // it still has automatic storage duration.
        if matches!(scope, DeclScope::Block { .. })
            && !specs.storage_class.intersects(NO_AUTO_DURATION)
            && vla.is_empty()
            && !is_fn
        {
            self.check_stack_object_size(typ, pos, "an automatic object")?;
        }

        // The memory effect is consumed whatever is declared, so the next
        // declarator does not inherit it; a function takes the effect every
        // declaration of its name has promised.
        let pending_effect = self.take_pending_fn_effect();
        let mut symbol_attrs = std::mem::take(&mut self.pending_symbol_attrs);
        if !is_fn {
            Self::drop_ifunc(&mut symbol_attrs, pos);
        }
        let cleanup = symbol_attrs.cleanup.take().and_then(|func| {
            let var = Cleanup {
                func,
                var: symbol,
                typ,
                pos,
            };
            self.declarator_cleanup(var, specs.storage_class, scope, is_fn)
        });
        Ok(Bound::Decl(InitDeclarator {
            symbol_attrs,
            fn_effect: fn_attrs.map_or(pending_effect, |a| a.effect),
            cleanup,
            symbol,
            typ,
            storage_class: specs.storage_class,
            init,
            vla_sizes: vla,
            // C11 6.7.5p2: a function has no alignment specifier.
            explicit_align: if is_fn { None } else { align },
            pos,
        }))
    }

    /// The call `__attribute__((cleanup(fn)))` asks for on the declarator
    /// `var` describes, with storage class `storage` at `scope`.
    ///
    /// Only a variable with automatic storage has a scope to leave. gcc
    /// ignores the attribute with a warning on a typedef, a function, a
    /// `static` and anything at file scope -- and in silence on a
    /// block-scope `extern`, which this matches.
    ///
    /// The call is built here, where it is checked as any call is: an
    /// argument the function's prototype cannot take draws the usual
    /// diagnostic, and taking the address of a `register` variable is the
    /// usual error.
    fn declarator_cleanup(
        &mut self,
        var: Cleanup,
        storage: TypeModifiers,
        scope: DeclScope,
        is_fn: bool,
    ) -> Option<Expr> {
        let automatic = matches!(scope, DeclScope::Block { .. })
            && !is_fn
            && !storage.intersects(NO_AUTO_DURATION);
        if automatic {
            return Some(self.cleanup_call(var));
        }
        let block_extern = matches!(scope, DeclScope::Block { .. })
            && !is_fn
            && storage.contains(TypeModifiers::EXTERN);
        if !block_extern {
            Self::warn_cleanup_ignored(var.pos);
        }
        None
    }

    /// `func(&var)`, checked as the call it is.
    fn cleanup_call(&mut self, c: Cleanup) -> Expr {
        let func_typ = self.symbols.get(c.func).typ;
        let callee = Self::typed_expr(ExprKind::Ident(c.func), func_typ, c.pos);
        let operand = Self::typed_expr(ExprKind::Ident(c.var), c.typ, c.pos);
        self.check_addressable(&operand, c.pos);
        let ptr = self.types.pointer_to(c.typ);
        let arg = Self::typed_expr(
            ExprKind::Unary {
                op: UnaryOp::AddrOf,
                operand: Box::new(operand),
            },
            ptr,
            c.pos,
        );
        self.checked_call(callee, vec![arg], c.pos, c.pos)
    }

    /// The storage-class and function specifiers a declarator of this kind,
    /// at this scope, may not have.
    fn check_storage_for_declarator(
        &self,
        specs: &DeclSpecs,
        name: StringId,
        is_fn: bool,
        scope: DeclScope,
        pos: Position,
    ) {
        let storage = specs.storage_class;
        let spelled = self.idents.get_opt(name).unwrap_or("");
        let file = scope == DeclScope::File;
        if is_fn {
            // 6.7.1p7 (block-scope `static` is reported with linkage).
            if storage.intersects(
                TypeModifiers::THREAD_LOCAL | TypeModifiers::AUTO | TypeModifiers::REGISTER,
            ) {
                diag::error_args(pos, "invalid storage class for function '{0}'", &[spelled]);
            }
            // 6.7.4p4: no function specifier on `main` in a hosted program.
            if file && spelled == "main" && storage.contains(TypeModifiers::INLINE) {
                diag::warning(pos, &gettext("cannot inline function 'main'"));
            }
            return;
        }
        // 6.7.4p1: the function specifiers are for functions only. gcc warns.
        if storage.contains(TypeModifiers::INLINE) {
            diag::warning_args(pos, "variable '{0}' declared 'inline'", &[spelled]);
        }
        if specs.ty.modifiers.contains(TypeModifiers::NORETURN) {
            diag::warning_args(pos, "variable '{0}' declared '_Noreturn'", &[spelled]);
        }
        if file {
            // 6.9p2: no `auto` or `register` on an external declaration --
            // except gcc's global register variable, `register int r
            // __asm__("r14");`, which names its register.
            if storage.contains(TypeModifiers::AUTO) {
                diag::error_args(
                    pos,
                    "file-scope declaration of '{0}' specifies 'auto'",
                    &[spelled],
                );
            }
            if storage.contains(TypeModifiers::REGISTER) && self.pending_asm_label.is_none() {
                diag::error_args(pos, "register name not specified for '{0}'", &[spelled]);
            }
        } else if storage.contains(TypeModifiers::THREAD_LOCAL)
            && !storage.intersects(TypeModifiers::STATIC | TypeModifiers::EXTERN)
        {
            // 6.7.1p3: at block scope `_Thread_local` needs `static` or
            // `extern`; alone it was silently an automatic variable.
            diag::error_args(
                pos,
                "function-scope '{0}' implicitly auto and declared '_Thread_local'",
                &[spelled],
            );
        }
    }

    /// Report what C forbids in the type a declarator derived: a function
    /// returning an array or a function (C17 6.7.6.3p1), and `restrict` on
    /// anything but a pointer to an object type (6.7.3p2). The parameter
    /// types of a function type are walked too, so `void f(restrict int x)`
    /// is caught with its function.
    pub(super) fn check_derived_types(&self, typ: TypeId, name: StringId, pos: Position) {
        let spelled = self.idents.get_opt(name).unwrap_or("type name");
        let t = self.types.get(typ);
        if t.modifiers.contains(TypeModifiers::RESTRICT) {
            let restrictable = t.kind == TypeKind::Pointer
                && t.base
                    .is_some_and(|b| self.types.kind(b) != TypeKind::Function);
            if !restrictable {
                diag::error(pos, &gettext("invalid use of 'restrict'"));
            }
        }
        match t.kind {
            TypeKind::Function => {
                let ret = t.base.expect("a function type has a return type");
                let params = t.params.clone().unwrap_or_default();
                match self.types.kind(ret) {
                    // A GNU vector is a value, and may be returned.
                    TypeKind::Array if self.types.is_vector(ret) => {}
                    TypeKind::Array => diag::error_args(
                        pos,
                        "'{0}' declared as function returning an array",
                        &[spelled],
                    ),
                    TypeKind::Function => diag::error_args(
                        pos,
                        "'{0}' declared as function returning a function",
                        &[spelled],
                    ),
                    _ => self.check_derived_types(ret, name, pos),
                }
                for param in params {
                    self.check_derived_types(param, StringId::EMPTY, pos);
                }
            }
            TypeKind::Pointer | TypeKind::Array => {
                if let Some(base) = t.base {
                    self.check_derived_types(base, name, pos);
                }
            }
            _ => {}
        }
    }

    /// Bind a typedef name. A typedef takes no initializer, and one of a
    /// variably modified type remembers how many extents it carries, so a use
    /// can name each of them: they cannot be recovered from the type --
    /// `int[n]`, `int[m]` and `int[]` all intern to one `TypeId`.
    fn bind_typedef_name(
        &mut self,
        scope: DeclScope,
        name: StringId,
        pos: Position,
        typ: TypeId,
        extents: &[Expr],
    ) -> ParseResult<Option<SymbolId>> {
        if self.is_special(b'=') {
            return Err(ParseError::new(
                "typedef cannot have initializer",
                self.current_pos(),
            ));
        }
        // 6.7p3: a typedef name may be redefined in its scope only to the
        // same type, which is never the case for a variably modified one --
        // its extents are evaluated anew each time.
        let existing_here = self
            .symbols
            .lookup_id(name, Namespace::Ordinary)
            .filter(|&id| {
                let s = self.symbols.get(id);
                s.is_typedef() && s.scope_depth == self.symbols.depth()
            });
        if let Some(id) = existing_here {
            if !extents.is_empty() || self.vm_typedefs.contains_key(&id) {
                diag::error_args(
                    pos,
                    "redefinition of typedef '{0}' with variably modified type",
                    &[self.idents.get_opt(name).unwrap_or("")],
                );
            }
        }
        self.check_typedef_redefinition(name, typ, pos);
        let sym = Symbol::typedef(name, typ, self.symbols.depth());
        let symbol = self.declare_in(scope, sym, name);
        if let Some(id) = symbol.filter(|_| !extents.is_empty()) {
            self.vm_typedefs.insert(id, extents.len() as u32);
        }
        Ok(symbol)
    }

    /// The attributes of a function declarator, and its type made to carry
    /// `noreturn` when anything said so.
    ///
    /// Answers the attributes accumulated under the function's name, or
    /// `None` for a typedef, which names a type and no function.
    fn function_declarator_attrs(
        &mut self,
        specs: &DeclSpecs,
        name: StringId,
        typ: &mut TypeId,
    ) -> Option<FunctionAttrs> {
        let attrs = if specs.is_typedef() {
            None
        } else {
            // C11 6.7.5p2: not on a function. A pointer to function is an
            // object and stays legal, so this asks the finished type.
            self.reject_alignas_in("a function");
            Some(self.accumulate_fn_attrs(name))
        };
        // `noreturn` belongs to the function *type*, since that is what a
        // call site reads: `_Noreturn` among the specifiers, or the attribute
        // on this declarator or on any earlier declaration of the name.
        let noreturn = specs.ty.modifiers.contains(TypeModifiers::NORETURN)
            || attrs
                .as_ref()
                .map_or(self.pending_fn_attrs.noreturn, |a| a.noreturn);
        if noreturn && !self.types.get(*typ).noreturn {
            let mut func = self.types.get(*typ).clone();
            func.noreturn = true;
            *typ = self.types.intern(func);
        }
        attrs
    }

    /// Declare `sym`, or answer the symbol an earlier declaration of the name
    /// already bound in this scope.
    ///
    /// C allows any number of file-scope declarations of one object or
    /// function, so there the existing symbol is reused -- and a declaration
    /// that knows an array's extent completes an earlier `extern int a[];`
    /// (6.2.7p4). A block-scope repeat binds nothing.
    fn declare_in(&mut self, scope: DeclScope, sym: Symbol, name: StringId) -> Option<SymbolId> {
        let typ = sym.typ;
        let is_typedef = sym.is_typedef();
        if let Ok(id) = self.symbols.declare(sym) {
            return Some(id);
        }
        if scope != DeclScope::File {
            return None;
        }
        let existing = self
            .symbols
            .lookup_id(name, Namespace::Ordinary)
            .expect("redeclaration should find existing symbol");
        if !is_typedef
            && self
                .types
                .unsized_array_levels(self.symbols.get(existing).typ)
                > 0
            && self.types.unsized_array_levels(typ) == 0
        {
            self.symbols.get_mut(existing).typ = typ;
        }
        Some(existing)
    }

    /// Parse the initializer after a declarator, if one follows, and let it
    /// complete the declared type.
    fn parse_declarator_initializer(
        &mut self,
        specs: &DeclSpecs,
        scope: DeclScope,
        name: StringId,
        pos: Position,
        typ: &mut TypeId,
        symbol: Option<SymbolId>,
    ) -> ParseResult<Option<Expr>> {
        if !self.is_special(b'=') {
            return Ok(None);
        }
        if self.types.kind(*typ) == TypeKind::Function {
            return Err(ParseError::new(
                format!(
                    "function '{}' is initialized like a variable",
                    self.idents.get(name)
                ),
                pos,
            ));
        }
        self.advance();
        let init = self.parse_initializer()?;
        Ok(Some(
            self.settle_initializer(specs, scope, typ, symbol, init),
        ))
    }

    /// Check a declarator's initializer against the declared type, and let it
    /// complete that type.
    fn settle_initializer(
        &mut self,
        specs: &DeclSpecs,
        scope: DeclScope,
        typ: &mut TypeId,
        symbol: Option<SymbolId>,
        mut init: Expr,
    ) -> Expr {
        self.walk_initializer(*typ, &mut init);

        // 6.7.9p5: an identifier declared `extern` at block scope has
        // linkage, so it refers to a definition elsewhere and cannot carry
        // one here. At *file* scope the same spelling is a definition with
        // external linkage, which gcc only warns about.
        if specs.is_extern() {
            let msg = gettext("'extern' variable has an initializer");
            match scope {
                DeclScope::File => diag::warning(init.pos, &msg),
                DeclScope::Block { .. } => diag::error(init.pos, &msg),
            }
        }

        // For incomplete array types, infer size from initializer
        let sized = self.infer_array_size_from_init(*typ, &init);
        self.check_excess_initializers(sized, &init);
        self.check_initializer_types(sized, &init);
        // The symbol was bound before the initializer was parsed, so it
        // still has the incomplete type.
        if sized != *typ {
            *typ = sized;
            if let Some(id) = symbol {
                self.symbols.get_mut(id).typ = sized;
            }
        }
        init
    }

    /// The type an `__auto_type` declarator takes, and the initializer it
    /// takes it from (gcc's extension).
    ///
    /// The declarator must be the declaration's only one, a plain identifier
    /// (`plain`), and initialized: gcc's diagnostics for each, in its order.
    /// The initializer is an expression, parsed here, ahead of the
    /// declaration: the name is not yet in scope inside it, so
    /// `__auto_type x = x;` names an outer `x` or none, as in gcc. Its type is
    /// lvalue-converted -- an array or a function decays, and top-level
    /// qualifiers drop (C17 6.3.2.1) -- and then takes the qualifiers written
    /// with `__auto_type`.
    fn infer_auto_type(
        &mut self,
        specs: &DeclSpecs,
        auto_pos: Position,
        plain: bool,
        first: bool,
    ) -> ParseResult<(TypeId, Expr)> {
        if !first {
            return Err(ParseError::new(
                gettext("'__auto_type' may only be used with a single declarator"),
                auto_pos,
            ));
        }
        if !plain {
            return Err(ParseError::new(
                gettext("'__auto_type' requires a plain identifier as declarator"),
                auto_pos,
            ));
        }
        if !self.is_special(b'=') {
            return Err(ParseError::new(
                gettext("'__auto_type' requires an initialized data declaration"),
                auto_pos,
            ));
        }
        if specs.is_typedef() {
            return Err(ParseError::new(
                "typedef cannot have initializer",
                self.current_pos(),
            ));
        }
        self.advance(); // consume '='
        let init = self.parse_assignment_expr()?;
        if self.bit_field_designated(&init).is_some() {
            diag::error(
                auto_pos,
                &gettext("'__auto_type' used with a bit-field initializer"),
            );
        }
        let value = self.lvalue_converted_type(init.typ.unwrap_or(self.types.int_id));
        let quals = specs.ty.modifiers & Type::QUALIFIERS;
        Ok((self.types.qualified_with(value, quals), init))
    }

    /// C17 6.7p7: an object's type must be complete where it is defined.
    ///
    /// At file scope that cannot be judged here, because 6.9.2p3 lets a
    /// tentative definition be completed later in the translation unit, so
    /// the object is recorded and judged at its end. In a block nothing later
    /// can supply a size -- a tag completed further down the block is a
    /// different declaration -- and an array must have its extent, from its
    /// declarator or its initializer.
    fn check_object_complete(
        &mut self,
        scope: DeclScope,
        name: StringId,
        typ: TypeId,
        vla: &[Expr],
        pos: Position,
    ) {
        let is_array = self.types.kind(typ) == TypeKind::Array;
        match scope {
            DeclScope::File => {
                if !is_array && !self.types.is_composite_complete(typ) {
                    self.tentative_definitions.push((typ, pos));
                }
            }
            DeclScope::Block { .. } if is_array => {
                if self.types.get(typ).array_size.is_none() && vla.is_empty() {
                    diag::error_args(pos, "array size missing in '{0}'", &[self.idents.get(name)]);
                }
            }
            DeclScope::Block { .. } => {
                if !self.types.is_composite_complete(typ) {
                    let named = self.types.format_type(typ, Some(self.idents));
                    diag::error_args(
                        pos,
                        "storage size of an object of type '{0}' is not known",
                        &[&named],
                    );
                }
            }
        }
    }

    /// C17 6.7.6.2p2 for a block-scope object of variably modified type: it
    /// may have no linkage, and a variable length array may not have static
    /// or thread storage duration. Either one has storage laid out at compile
    /// time, and an extent known only at run time gave it none.
    fn check_variably_modified_storage(
        &mut self,
        storage_class: TypeModifiers,
        name: StringId,
        typ: TypeId,
        pos: Position,
    ) {
        if storage_class.contains(TypeModifiers::EXTERN) {
            diag::error(
                pos,
                &gettext("object with variably modified type must have no linkage"),
            );
        } else if self.types.kind(typ) == TypeKind::Array
            && storage_class.intersects(TypeModifiers::STATIC | TypeModifiers::THREAD_LOCAL)
        {
            diag::error_args(
                pos,
                "storage size of '{0}' isn't constant",
                &[self.idents.get(name)],
            );
        }
    }
}
