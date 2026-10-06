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
use super::declaration::SpecContext;
use super::parser::{
    DeclaratorContext, ParameterList, ParseError, ParseResult, ParsedDeclarator, Parser, RawParam,
};
use crate::diag;
use crate::strings::StringId;
use crate::symbol::Symbol;
use crate::token::lexer::{Position, SpecialToken, TokenType};
use crate::types::{Type, TypeId, TypeKind, TypeModifiers};
use gettextrs::gettext;

const DEFAULT_PARAM_CAPACITY: usize = 8;

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

/// What one `[ ]` of an array declarator says about the array's extent.
#[derive(Clone, Copy)]
enum Extent {
    /// An integer constant expression.
    Constant(usize),
    /// A variable length array's size expression, or `[*]`: complete, but
    /// known only at run time.
    Runtime,
    /// `[]`: the array type is incomplete (C17 6.7.6.2p4).
    Absent,
}

impl Extent {
    /// The extent the array type records; a run-time one is carried apart.
    fn size(self) -> Option<usize> {
        match self {
            Self::Constant(n) => Some(n),
            Self::Runtime | Self::Absent => None,
        }
    }
}

/// Why a type cannot be an array's element type (C17 6.7.6.2p1), as gcc
/// words it.
enum ElementDefect {
    /// An incomplete object type.
    Incomplete,
    /// `void` or a function type: "array of voids", "array of functions".
    Of(&'static str),
}

/// The identifier a declarator declares and where it is written, which is
/// how gcc names and places a diagnostic about the declared type.
#[derive(Clone, Copy)]
struct DeclSite {
    name: StringId,
    name_pos: Position,
}

impl Parser<'_> {
    /// Consume the type-qualifier run after a `*` in a declarator.
    ///
    /// C17 6.7.6.1 lets any type qualifier appear there, `_Atomic` included:
    /// `int *_Atomic p;` declares an atomic pointer to int. One implementation
    /// for every declarator path, so file scope and block scope agree; missing
    /// `_Atomic` here lets it fall through to the name position and be taken
    /// as the identifier.
    pub(super) fn parse_pointer_qualifiers(&mut self) -> TypeModifiers {
        let mut modifiers = TypeModifiers::empty();
        while let Some(name_id) = self.current_ident() {
            // Every spelling of the three CV qualifiers, from the one shared
            // answer.
            if let Some(m) = super::cv_qualifier_modifier(name_id) {
                modifiers |= m;
                self.advance();
                continue;
            }
            match name_id {
                crate::kw::ATOMIC => modifiers |= TypeModifiers::ATOMIC,
                _ if super::is_nullability_qualifier(name_id) => {}
                // An attribute may sit between two `*`s -- `int *
                // __attribute__((aligned(16))) *p;` -- where it qualifies the
                // pointer being declared. Breaking out here left it for the
                // declarator, which at file scope was a parse error and at
                // block scope was worse: `expect_declarator_name` does not
                // treat `__attribute__` as reserved, so it became the
                // declared object's *name*.
                _ if self.is_attribute_keyword() => {
                    self.skip_extensions();
                    continue;
                }
                _ => break,
            }
            self.advance();
        }
        modifiers
    }

    /// Whether the `(` just consumed groups a declarator, looking past any
    /// attribute lists at its start without consuming them.
    ///
    /// Those attributes are what decides nothing: `(__attribute__((x)) *p)`
    /// groups a pointer declarator, while `(__attribute__((mode(SI))) int f)`
    /// opens a parameter list whose first parameter carries them, and only
    /// the token after them tells the two apart.
    fn is_grouped_declarator_after_attributes(&mut self) -> bool {
        let start = self.pos;
        while self.is_attribute_keyword() {
            self.advance();
            let mut depth = 0usize;
            loop {
                if self.is_special(b'(') {
                    depth += 1;
                } else if self.is_special(b')') {
                    depth = depth.saturating_sub(1);
                } else if self.peek() == TokenType::StreamEnd {
                    break;
                }
                self.advance();
                if depth == 0 {
                    break;
                }
            }
        }
        let grouped = self.is_grouped_declarator();
        self.pos = start;
        grouped
    }

    /// Parse a declarator (name and type modifiers)
    ///
    /// C declarators are parsed "inside-out". For example, `int (*p)[3]`:
    /// - `int` is the base type
    /// - `(*p)` means p is a pointer
    /// - `[3]` after the parens means "to array of 3"
    ///   So p is "pointer to array of 3 ints"
    ///
    /// The one declarator parser: every declaration, member, parameter and
    /// type-name reaches its declarator through here, and `ctx` says which it
    /// is. The function parameters it returns include names, for a function
    /// definition to bind.
    pub(crate) fn parse_declarator(
        &mut self,
        base_type_id: TypeId,
        ctx: DeclaratorContext,
    ) -> ParseResult<ParsedDeclarator> {
        self.parse_declarator_level(base_type_id, ctx, false)
    }

    /// One level of [`Self::parse_declarator`]. A grouped declarator's inner
    /// level is parsed over a `void` placeholder (`placeholder`) that the
    /// outer level substitutes once its own suffix is known, so an array
    /// directly over the placeholder has its element checked there.
    fn parse_declarator_level(
        &mut self,
        base_type_id: TypeId,
        ctx: DeclaratorContext,
        placeholder: bool,
    ) -> ParseResult<ParsedDeclarator> {
        let start = self.current_pos();
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
        let mut name_pos = self.current_pos();
        let mut group_attributed = false;
        let (name, inner) = if self.is_special(b'(') {
            // Check if this looks like a function parameter list or a grouped declarator
            // A grouped declarator will have * or identifier immediately after (
            let saved_pos = self.pos;
            self.advance(); // consume '('

            if self.is_grouped_declarator_after_attributes() {
                // An attribute opening the group -- `long (__attribute__
                // ((ms_abi)) *fp)(long)`, the spelling every Windows
                // calling-convention macro expands to -- belongs to this
                // declaration.
                let group_start = self.pos;
                self.skip_extensions();
                group_attributed = self.pos != group_start;
                // For int (*p)[3]: we're now at *p), base_type is int
                let inner = self.parse_declarator_level(self.types.void_id, ctx, true)?;
                self.expect_special(b')')?;
                name_pos = inner.name_pos;
                (inner.name, Some(inner))
            } else {
                // The `(` opens a parameter list, so this declarator is
                // abstract: `int (size_t)` names a function type, and there is
                // no identifier to find. Rewind to the `(` and let the
                // function-suffix loop below consume the parameter list.
                self.pos = saved_pos;
                if ctx.requires_name() {
                    // A declaration has to declare something: `int (void);`
                    // is the abstract form where only a declarator may go.
                    (self.expect_declarator_name()?, None)
                } else {
                    (StringId::EMPTY, None)
                }
            }
        } else if self.peek() == TokenType::Ident || ctx.requires_name() {
            (self.expect_declarator_name()?, None)
        } else {
            // An abstract declarator has no identifier by construction
            // (C17 6.7.7): `void (*)(int)`, or a parameter written as a bare
            // type. Whether that is allowed is the caller's question, not a
            // guess from the next token: any token may follow, and
            // `_Generic(1, int: 11)` ends its type-name at a `:`.
            (StringId::EMPTY, None)
        };

        // The inner declarator is the outer level, so its extents come first:
        // `int (*p[n])` is an array of `n` pointers. Dropping them left that
        // array incomplete.
        let mut vla_pos = inner.as_ref().and_then(|i| i.vla_pos);
        let mut vla_exprs: Vec<Expr> = inner.as_ref().map(|i| i.vla.clone()).unwrap_or_default();

        // Only the parameter's own array type, the first `[]` written after a
        // plain name, may carry `static` or qualifiers: `int (*p)[static 3]`
        // and `int a[3][const 4]` qualify something that is not adjusted to a
        // pointer. A name in parentheses, `int (a)[static 3]`, is still a
        // plain name; one grouped with an attribute is not, to gcc.
        let plain_inner = inner.as_ref().is_none_or(|i| i.plain_name) && !group_attributed;

        // Handle array declarators - collect all dimensions first
        let mut dimensions: Vec<(Extent, Position)> = Vec::new();
        while self.is_special(b'[') {
            let dim_pos = self.current_pos();
            self.advance();
            let outermost = dimensions.is_empty() && plain_inner;
            let extent = self.parse_array_extent(
                name,
                dim_pos,
                ctx,
                outermost,
                &mut vla_exprs,
                &mut vla_pos,
            )?;
            self.expect_special(b']')?;
            dimensions.push((extent, dim_pos));
        }

        // Handle function declarators: void (*fp)(int, char)
        // This parses the parameter list after a grouped declarator
        // We keep both the TypeIds (for building the type) and raw params (for function defs)
        let (func_params, full_func_params): (Option<FuncSignature>, Option<ParameterList>) =
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
                    Some(list),
                )
            } else {
                (None, None)
            };

        let plain_name = plain_inner
            && pointer_modifiers.is_empty()
            && dimensions.is_empty()
            && func_params.is_none();

        // Build the type from the base type
        let mut result_type_id = base_type_id;

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

        // Outer pointers (before any parens) apply to the base type first.
        // A suffix then reads left to right from the identifier, so
        // `a[2](void)` is an array of functions: the function suffix after
        // the extents forms the element type they are derived over.
        //
        // For struct node *(*fp)(int): Pointer(struct node)
        //   -> Function(Pointer(struct node), [int])
        if let Some(sig) = func_params {
            let func_type = sig.into_type(result_type_id);
            result_type_id = self.types.intern(func_type);
        }
        // An abstract declarator is placed at its start, as gcc places it.
        if name == StringId::EMPTY {
            name_pos = start;
        }
        let site = DeclSite { name, name_pos };
        let over_placeholder = placeholder && result_type_id == base_type_id;
        let outer_absent = matches!(dimensions.first(), Some((Extent::Absent, _)));
        result_type_id =
            self.derive_suffix_arrays(result_type_id, dimensions, site, over_placeholder)?;

        if let Some(inner) = &inner {
            // Grouped declarator: int (*p)[3] or void (*fp)(int) or int *(*q)[3].
            // Substitute the result type into the inner declarator: for
            // `int (*p)[3]`, Pointer(Void) and Array(3, int) compose into
            // Pointer(Array(3, int)).
            if self.array_over_placeholder(inner.typ) {
                self.check_array_element(result_type_id, outer_absent, site);
            }
            result_type_id = self.substitute_base_type(inner.typ, result_type_id);
        }

        // The inner declarator's parameters win when it has them, as in
        // `int (*get_op(int which))(int)`; otherwise the outer suffix's do.
        let params = match inner {
            Some(ParsedDeclarator {
                params: Some(params),
                ..
            }) => Some(params),
            _ => full_func_params,
        };

        Ok(ParsedDeclarator {
            name,
            pos: start,
            name_pos,
            typ: self.carry_storage_class(base_type_id, result_type_id),
            vla: vla_exprs,
            vla_pos,
            params,
            plain_name,
        })
    }

    /// The extent inside one `[ ]` of an array declarator, after the `[`.
    ///
    /// A run-time extent's expression is appended to `vla`.
    fn parse_array_extent(
        &mut self,
        name: StringId,
        dim_pos: Position,
        ctx: DeclaratorContext,
        outermost: bool,
        vla: &mut Vec<Expr>,
        vla_pos: &mut Option<Position>,
    ) -> ParseResult<Extent> {
        // C17 6.7.6.2p1: the optional type qualifiers and `static` belong to
        // the declaration of a function parameter -- `_Atomic` among them.
        let mut qualified = false;
        while let Some(name_id) = self.current_ident() {
            match name_id {
                crate::kw::STATIC | crate::kw::ATOMIC => {}
                _ if super::cv_qualifier_modifier(name_id).is_some() => {}
                _ => break,
            }
            qualified = true;
            self.advance();
        }
        if qualified && !(ctx.is_parameter() && outermost) {
            diag::error(
                dim_pos,
                "static or type qualifiers in non-parameter array declarator",
            );
        }

        if self.is_special(b']') {
            return Ok(Extent::Absent);
        }
        // `[*]`: a variable length array of unspecified size, which only a
        // prototype can declare (C17 6.7.6.2p4). `[*p]` is an expression.
        if self.is_special(b'*') && self.next_token_is_special(b']') {
            self.advance();
            if !ctx.is_prototype_scope() {
                diag::error(
                    dim_pos,
                    "'[*]' not allowed in other than function prototype scope",
                );
            } else {
                // Allowed only if no body follows this list, which is not
                // known yet.
                self.star_in_params.get_or_insert(dim_pos);
            }
            return Ok(Extent::Runtime);
        }

        // Parse constant expression for array size (C99 6.7.5.2)
        let size_pos = self.current_pos();
        let expr = self.parse_assignment_expr()?;
        match self.eval_const_expr(&expr) {
            Some(n) if n >= 0 => Ok(Extent::Constant(n as usize)),
            // C17 6.7.6.2p1: the size shall be greater than zero. Zero itself
            // is a GNU extension gcc accepts, so only a negative size is
            // refused here.
            Some(_) => {
                // gcc distinguishes the two, and the abstract case is the one
                // a type-name reaches: `sizeof(char[-1])` says "unnamed",
                // `char a[-1];` names `a`.
                Err(ParseError::new(
                    if name == StringId::EMPTY {
                        "size of unnamed array is negative".to_string()
                    } else {
                        format!("size of array '{}' is negative", self.idents.get(name))
                    },
                    size_pos,
                ))
            }
            None => {
                // Non-constant (VLA) - save expression for VLA handling
                self.check_array_size_type(&expr, size_pos)?;
                vla.push(expr);
                vla_pos.get_or_insert(size_pos);
                Ok(Extent::Runtime)
            }
        }
    }

    /// Derive a declarator's array suffix over `elem`, the innermost extent
    /// first, checking each level's element as it is formed. `over_placeholder`
    /// says `elem` is a grouped inner level's placeholder, whose real type --
    /// and so its check -- is the outer level's.
    fn derive_suffix_arrays(
        &mut self,
        elem: TypeId,
        dimensions: Vec<(Extent, Position)>,
        site: DeclSite,
        over_placeholder: bool,
    ) -> ParseResult<TypeId> {
        let mut typ = elem;
        let mut elem_absent = false;
        for (i, (extent, pos)) in dimensions.into_iter().rev().enumerate() {
            if i > 0 || !over_placeholder {
                self.check_array_element(typ, elem_absent, site);
            }
            typ = self.derive_array_type(typ, extent.size(), pos)?;
            elem_absent = matches!(extent, Extent::Absent);
        }
        Ok(typ)
    }

    /// Whether a grouped inner declarator's type puts an array directly over
    /// its placeholder, as `(a[2])` does and `(*a[2])` does not.
    fn array_over_placeholder(&self, inner: TypeId) -> bool {
        let mut cur = inner;
        let mut over_array = false;
        while let TypeKind::Pointer | TypeKind::Array | TypeKind::Function = self.types.kind(cur) {
            over_array = self.types.kind(cur) == TypeKind::Array;
            match self.types.base_type(cur) {
                Some(base) => cur = base,
                None => break,
            }
        }
        over_array
    }

    /// C17 6.7.6.2p1 (constraint): an array's element type shall be neither
    /// incomplete nor a function type. The element's size is the array's
    /// stride, so this holds wherever an array type is formed -- an `extern`
    /// declaration, a typedef, a parameter that adjusts to a pointer, a
    /// pointer to the array, a type-name -- and gcc does not wait for a tag
    /// completed further down. `elem_absent` says `elem` is an array whose
    /// own extent was written `[]`. Reported, not fatal: the type is still
    /// formed, so the declaration binds and nothing cascades. An abstract
    /// declarator's `site` is its start, where gcc points too.
    fn check_array_element(&self, elem: TypeId, elem_absent: bool, site: DeclSite) {
        let Some(defect) = self.array_element_defect(elem, elem_absent) else {
            return;
        };
        let msg = match defect {
            ElementDefect::Incomplete => format!(
                "array type has incomplete element type '{}'",
                self.types.format_type(elem, Some(self.idents))
            ),
            ElementDefect::Of(what) if site.name == StringId::EMPTY => {
                format!("declaration of type name as array of {what}")
            }
            ElementDefect::Of(what) => format!(
                "declaration of '{}' as array of {what}",
                self.idents.get(site.name)
            ),
        };
        diag::error(site.name_pos, &msg);
    }

    /// What, if anything, keeps `elem` from being an array's element type.
    fn array_element_defect(&self, elem: TypeId, elem_absent: bool) -> Option<ElementDefect> {
        match self.types.kind(elem) {
            TypeKind::Void => Some(ElementDefect::Of("voids")),
            TypeKind::Function => Some(ElementDefect::Of("functions")),
            TypeKind::Array => elem_absent.then_some(ElementDefect::Incomplete),
            _ => self
                .type_name_is_incomplete(elem, 0)
                .then_some(ElementDefect::Incomplete),
        }
    }

    /// Carry the specifiers' storage class onto a type derived from them.
    ///
    /// Storage class is not part of a type, but the declaration binders read
    /// it off the declarator's type -- `extern int *p` needs `EXTERN` on the
    /// pointer, `typedef int a[3]` `TYPEDEF` on the array. A function
    /// declarator carries it on its *return* type instead: that is the type a
    /// function definition is emitted from, and the linearizer reads `extern`
    /// there. The function type itself stays bare.
    fn carry_storage_class(&mut self, base: TypeId, derived: TypeId) -> TypeId {
        let storage = self.types.modifiers(base) & Type::STORAGE_CLASS;
        if storage.is_empty() || derived == base {
            return derived;
        }
        let mut typ = self.types.get(derived).clone();
        if typ.kind == TypeKind::Function {
            let ret = typ.base.expect("a function type has a return type");
            typ.base = Some(self.carry_storage_class(base, ret));
        } else {
            typ.modifiers |= storage;
        }
        self.types.intern(typ)
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
                let decl_conv = decl_type.conv;
                let new_ret_id = self.substitute_base_type(inner_base_id, actual_base_id);
                let func_type = Type {
                    kind: TypeKind::Function,
                    base: Some(new_ret_id),
                    params: decl_params,
                    variadic: decl_variadic,
                    noreturn: decl_noreturn,
                    conv: decl_conv,
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
    /// Parse `( parameters )`.
    ///
    /// The enclosing declaration's alignment specifier is hidden for the
    /// duration and restored afterwards, however the inner parse exits. Without
    /// that, `_Alignas(64) void (*fp)(int);` -- a legal pointer-to-function
    /// *object* -- saw the outer `_Alignas` while parsing `(int)` and reported
    /// it as an alignment on a parameter.
    ///
    /// The parameter scope is bracketed here for the same reason, and so are
    /// the pending function and symbol attributes: an attribute written on a
    /// parameter is the parameter's, and `void f(void (*cb)(void)
    /// __attribute__((noreturn)));` does not make `f` noreturn.
    pub(crate) fn parse_parameter_list(&mut self) -> ParseResult<ParameterList> {
        let saved_align = self.pending_alignas.take();
        let saved_align_kw = self.pending_alignas_kw.take();
        let saved_fn_attrs = std::mem::take(&mut self.pending_fn_attrs);
        let saved_symbol_attrs = std::mem::take(&mut self.pending_symbol_attrs);
        let saved_calling_conv = self.pending_calling_conv.take();
        // The parameter scope is opened and closed here rather than inside, so
        // that it is balanced however the inner parse exits. It used to be left
        // open on the `?` paths and on the trailing-comma `return Err`.
        self.symbols.enter_scope();
        self.param_list_depth += 1;
        // A nested list -- a parameter that is a pointer to a function --
        // keeps its `[*]` to itself.
        let enclosing_star = self.star_in_params.take();
        let result = self.parse_parameter_list_inner().map(|mut list| {
            list.star = self.star_in_params;
            list
        });
        self.star_in_params = enclosing_star;
        self.param_list_depth -= 1;
        self.symbols.leave_scope();
        self.pending_alignas = saved_align;
        self.pending_alignas_kw = saved_align_kw;
        self.pending_fn_attrs = saved_fn_attrs;
        self.pending_symbol_attrs = saved_symbol_attrs;
        self.pending_calling_conv = saved_calling_conv;
        result
    }

    fn parse_parameter_list_inner(&mut self) -> ParseResult<ParameterList> {
        let mut params: Vec<RawParam> = Vec::with_capacity(DEFAULT_PARAM_CAPACITY);
        let mut variadic = false;
        let mut prototyped = true;

        // Enter a temporary scope for parameter parsing (C99 6.9.1p9)
        // This allows VLA sizes to reference earlier parameters

        // `()` -- an empty identifier list, which is not a prototype.
        if self.is_special(b')') {
            return Ok(ParameterList {
                params,
                variadic,
                prototyped: false,
                star: None,
            });
        }

        // Check for (void)
        if self.is_keyword(crate::kw::VOID) {
            let saved_pos = self.pos;
            self.advance();
            if self.is_special(b')') {
                return Ok(ParameterList {
                    params,
                    variadic,
                    prototyped: true,
                    star: None,
                });
            }
            // Not just void, backtrack
            self.pos = saved_pos;
        }

        loop {
            // Check for ellipsis
            if self.is_special_token(SpecialToken::Ellipsis) {
                // C17 6.7.6p1's grammar has a parameter before `, ...`, but
                // gcc accepts `int f(...)` as an extension (C23 adopted it)
                // and objects only under `-pedantic`.
                if params.is_empty() {
                    diag::pedwarn(
                        self.current_pos(),
                        &gettext("ISO C requires a named argument before '...'"),
                    );
                }
                self.advance();
                variadic = true;
                break;
            }

            // Parse parameter type
            let param_pos = self.current_pos();
            let param_specs = self.parse_declaration_specifiers(SpecContext::Parameter)?;
            let param_type = param_specs.ty;
            // C11 6.7.5p2: not on a parameter.
            self.reject_alignas_in("a parameter");
            // An identifier list -- `int f(a, b) int a, b;` -- is not a
            // prototype (C17 6.7.6.3p14), and it is exactly the case where the
            // specifier parser supplied an implicit `int` without consuming an
            // identifier. The choice is all-or-nothing across the list, so the
            // first parameter settles it.
            if params.is_empty() && !variadic {
                prototyped = param_specs.explicit;
            }
            // For struct/union types with tags, use existing TypeId to preserve forward declarations
            let base_type_id = self.intern_type_with_tag(&param_type);

            // Use parse_declarator to handle all declarator forms including:
            // - Simple pointers: void *, int *
            // - Grouped declarators: void (*)(int), int (*)[10]
            // - Arrays: int arr[], int arr[10]
            let ParsedDeclarator {
                name: param_name,
                typ: mut typ_id,
                vla: mut vla_sizes,
                ..
            } = self.parse_declarator(base_type_id, DeclaratorContext::Parameter)?;
            // The specifiers' extents are the innermost levels, as in any
            // other declaration: `typeof(a) b` beside `int (*a)[n]`.
            vla_sizes.extend(param_specs.vm_dims);
            self.check_parameter_specifiers(param_type.modifiers, param_name, param_pos);

            // Skip any __attribute__ after parameter declarator
            self.skip_extensions();
            self.drop_pending_cleanup(param_pos);

            // A parameter's type attributes are the parameter's, and are
            // applied before the array-to-pointer adjustment below so a mode
            // names the declared type rather than the adjusted one. Nothing
            // consumed them here, so `int x __attribute__((mode(QI)))` was
            // silently an `int`.
            typ_id = self.apply_pending_type_attrs(typ_id);

            // A vector parameter is passed by value, and is no array to be
            // adjusted below.
            self.check_not_vector_value(Some(typ_id), self.current_pos());

            // C99 6.7.5.3: Array and function parameters are adjusted to pointers
            // - Array T[] becomes pointer to T
            // - Function type becomes pointer to function type
            let typ = self.types.get(typ_id);
            if typ.kind == TypeKind::Array && !self.types.is_vector(typ_id) {
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

            // A by-value parameter that runs out of registers gets a
            // stacked-argument slot, addressed exactly as a local is, so the
            // same bound applies. Asked *after* the 6.7.5.3 adjustment above,
            // which is what keeps `int f(char a[3000000000])` legal: that
            // parameter is a `char *`.
            //
            // Asked of a prototype as well as a definition, because neither
            // backend can call or define such a function, so the declaration is
            // the earliest honest place to say so -- and it is the one place
            // every prototyped parameter list, named or not, at any scope,
            // passes through. gcc reaches the same conclusion later, as
            // "sorry, unimplemented: passing too large argument on stack".
            self.check_stack_object_size(typ_id, self.current_pos(), "a by-value parameter")?;

            let name_opt = if param_name == StringId::EMPTY {
                None
            } else {
                Some(param_name)
            };

            // Keep the run-time dimensions of a variably-modified element
            // type; without them `int a[n][m]` indexes with a row stride of
            // zero.
            //
            // `vla_sizes` runs outermost-first over the whole declarator,
            // while only the element type matters after the array-to-pointer
            // adjustment. The outermost dimension is the one that may be
            // absent (`int a[][m]`), and C17 6.7.6.2p1 requires every later
            // dimension to be present, so the element type's variable
            // dimensions are exactly the trailing entries.
            let elem_typ = self.types.get(typ_id).base;
            let (vm_dims, discarded_dims) = match elem_typ {
                Some(elem) => {
                    let want = self.types.unsized_array_levels(elem);
                    let skip = vla_sizes.len().saturating_sub(want);
                    // The leading entries are the dimensions the
                    // array-to-pointer adjustment removes. They are still
                    // evaluated on entry, so they are kept for their side
                    // effects -- see `Parameter::discarded_dims`.
                    (vla_sizes[skip..].to_vec(), vla_sizes[..skip].to_vec())
                }
                None => (Vec::new(), vla_sizes.clone()),
            };

            // C17 6.7.6.3p10: `void` may appear as a parameter only as the
            // unnamed sole item in the list -- and then it means the function
            // takes none. The literal `(void)` is recognised earlier, on the
            // token; this is the same thing reached through a typedef, as in
            // `typedef void V; int f(V);`, which must not become a
            // one-parameter prototype.
            if self.types.kind(typ_id) == TypeKind::Void {
                if name_opt.is_none() && params.is_empty() && self.is_special(b')') {
                    // ... and unqualified (6.7.6.3p10): `void f(const void)`.
                    if self.types.modifiers(typ_id).intersects(Type::QUALIFIERS) {
                        diag::error(
                            self.current_pos(),
                            &gettext("'void' as only parameter may not be qualified"),
                        );
                    }
                    return Ok(ParameterList {
                        params,
                        variadic,
                        prototyped: true,
                        star: None,
                    });
                }
                // An unnamed `void` among other parameters is no parameter
                // at all, which gcc rejects; a named one it only warns about.
                if name_opt.is_none() {
                    diag::error(
                        self.current_pos(),
                        &gettext("'void' must be the only parameter"),
                    );
                } else {
                    diag::warning_args(
                        self.current_pos(),
                        "parameter {0} has void type",
                        &[&(params.len() + 1).to_string()],
                    );
                }
            }

            params.push(RawParam {
                name: name_opt,
                typ: typ_id,
                vm_dims,
                discarded_dims,
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
                // Nothing else may follow the comma: falling through would let
                // the specifier parser supply an implicit `int` and make
                // `void g(int, );` a two-parameter prototype. (C23 permits the
                // trailing comma; this compiler is C17.)
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

        Ok(ParameterList {
            params,
            variadic,
            prototyped,
            star: None,
        })
    }
}
