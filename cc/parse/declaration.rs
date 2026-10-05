//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Declaration parsing: declaration specifiers, type specifiers, initializer
// checking and redeclaration compatibility
//

use super::ast::{
    Declaration, Designator, Expr, ExprKind, ExternalDecl, InitDeclarator, InitElement, MemEffect,
};
use super::bind::DeclScope;
use super::parser::{ParseError, ParseResult, Parser};
use crate::diag;
use crate::strings::StringId;
use crate::symbol::{Namespace, Symbol, SymbolId, SymbolKind};
use crate::token::lexer::Position;
use crate::types::{FloatClass, Type, TypeId, TypeKind, TypeModifiers, TypeTable};
use gettextrs::gettext;

/// C17 6.7.2p2: the type specifiers must together name one of a
/// fixed list of combinations, so they are tallied, not accumulated.
/// `short`/`long`/`signed`/`unsigned` are counted; they qualify a
/// data type rather than naming one.
#[derive(Default)]
struct SpecifierTally<'a> {
    /// Data-type specifiers, in source order, under their canonical spelling
    /// -- or, for a typedef name or `typeof`, as written. More than one is
    /// always a constraint violation.
    data_types: Vec<(&'a str, Position)>,
    /// Where `_Complex` appeared, if it did. It names no data type of its
    /// own, but it is a type specifier, so a typedef name after it is the
    /// declarator.
    complex: Option<Position>,
    short_count: u32,
    long_count: u32,
    signed_count: u32,
    unsigned_count: u32,
    /// Where the most recent `short`/`long`/`signed`/`unsigned` appeared, for
    /// the diagnostic that reports it against an incompatible data type.
    last_size: Option<(&'static str, Position)>,
    last_sign: Option<(&'static str, Position)>,
    /// Storage-class specifiers, in source order. 6.7.1p2 permits at most one
    /// -- `_Thread_local` excepted, which may accompany `static` or `extern`.
    storage_classes: Vec<(&'static str, Position)>,
}

impl<'a> SpecifierTally<'a> {
    /// Whether an *alias* type specifier appearing now is the name of what is
    /// being declared rather than a second data type.
    ///
    /// A C library may define one of these as a typedef of its own: glibc's
    /// <bits/floatn-common.h> has `typedef float _Float32;` whenever the
    /// compiler does not claim native support. Once a data type has been
    /// given, `_Float32` is the declarator -- the same rule `typeof` needs --
    /// and without it that typedef is two data types and an error.
    fn alias_is_declarator_name(&self) -> bool {
        !self.data_types.is_empty()
    }

    fn note_data_type(&mut self, name: &'a str, pos: Position) {
        self.data_types.push((name, pos));
    }

    /// Whether any type specifier at all has been given -- a data type, a
    /// size, a signedness or `_Complex`. A typedef name is a type specifier
    /// only where none has (6.7.2p2 lists no combination containing one), so
    /// in `unsigned T x;` the `T` is the declarator, not the type; asking
    /// only whether a data type had been seen took it as the type.
    fn has_type_specifier(&self) -> bool {
        !self.data_types.is_empty()
            || self.complex.is_some()
            || self.short_count + self.long_count + self.signed_count + self.unsigned_count > 0
    }

    fn note_size(&mut self, name: &'static str, pos: Position) {
        self.last_size = Some((name, pos));
        if name == "short" {
            self.short_count += 1;
        } else {
            self.long_count += 1;
        }
    }

    /// `_Thread_local` is deliberately not recorded: 6.7.1p2 lets it appear
    /// with `static` or `extern`, and gcc accepts both orders, so counting it
    /// would reject `static _Thread_local int x;`.
    fn note_storage_class(&mut self, name: &'static str, pos: Position) {
        self.storage_classes.push((name, pos));
    }

    fn note_sign(&mut self, name: &'static str, pos: Position) {
        self.last_sign = Some((name, pos));
        if name == "signed" {
            self.signed_count += 1;
        } else {
            self.unsigned_count += 1;
        }
    }

    /// Report every way this specifier list fails C17 6.7.2p2.
    ///
    /// Reporting rather than returning an error: a constraint violation needs a
    /// diagnostic (C17 5.1.1.3), and the parser recovers with the type it had
    /// already built, so one bad declaration does not cascade.
    fn check(&self) {
        if let Some((second, pos)) = self.storage_classes.get(1) {
            let first = self.storage_classes[0].0;
            if first == *second {
                diag::error_args(*pos, "duplicate '{0}'", &[second]);
            } else {
                diag::error_args(
                    *pos,
                    "multiple storage classes in declaration specifiers ('{0}' and '{1}')",
                    &[first, second],
                );
            }
            return;
        }

        if let Some((second, pos)) = self.data_types.get(1) {
            diag::error_args(
                *pos,
                "two or more data types in declaration specifiers ('{0}' and '{1}')",
                &[self.data_types[0].0, second],
            );
            return;
        }

        if self.signed_count > 0 && self.unsigned_count > 0 {
            if let Some((_, pos)) = self.last_sign {
                diag::error(
                    pos,
                    "both 'signed' and 'unsigned' in declaration specifiers",
                );
            }
        } else if self.signed_count > 1 || self.unsigned_count > 1 {
            if let Some((name, pos)) = self.last_sign {
                diag::error_args(pos, "duplicate '{0}'", &[name]);
            }
        }

        if self.short_count > 0 && self.long_count > 0 {
            if let Some((_, pos)) = self.last_size {
                diag::error(pos, "both 'short' and 'long' in declaration specifiers");
            }
        } else if self.short_count > 1 {
            if let Some((_, pos)) = self.last_size {
                diag::error(pos, "duplicate 'short'");
            }
        } else if self.long_count > 2 {
            if let Some((_, pos)) = self.last_size {
                diag::error(pos, "'long long long' is too long for c17");
            }
        }

        let data_type = self.data_types.first().map(|(name, _)| *name);

        // `_Complex` qualifies a floating type (C17 6.7.2p2), or an integer
        // one as gcc's extension. Anything else -- a typedef name, `typeof`,
        // a tag, `_Bool`, `void` -- is no complex type, and the keyword was
        // silently dropped: `typedef double ty; ty _Complex z;` made `z` a
        // plain `double`.
        if let (Some(pos), Some(data)) = (self.complex, data_type) {
            let base_ok = matches!(
                data,
                "float"
                    | "double"
                    | "char"
                    | "int"
                    | "__int128"
                    | "_Float16"
                    | "_Float32"
                    | "_Float64"
                    | "_Float32x"
                    | "_Float64x"
                    | "__float128"
            );
            if !base_ok {
                diag::error_args(
                    pos,
                    "both '_Complex' and '{0}' in declaration specifiers",
                    &[data],
                );
            }
        }

        // `short` and `long` pair with `int`; `long` alone also pairs with
        // `double`. Nothing else.
        let size_ok = match data_type {
            None | Some("int") => true,
            Some("double") => self.short_count == 0 && self.long_count == 1,
            _ => false,
        };
        if !size_ok {
            if let (Some((size, pos)), Some(data)) = (self.last_size, data_type) {
                diag::error_args(
                    pos,
                    "both '{0}' and '{1}' in declaration specifiers",
                    &[size, data],
                );
            }
        }

        // `signed` and `unsigned` pair with the integer types only.
        let sign_ok = matches!(
            data_type,
            None | Some("int") | Some("char") | Some("__int128")
        );
        if !sign_ok {
            if let (Some((sign, pos)), Some(data)) = (self.last_sign, data_type) {
                diag::error_args(
                    pos,
                    "both '{0}' and '{1}' in declaration specifiers",
                    &[sign, data],
                );
            }
        }
    }
}

/// Where a list of declaration specifiers is written, which settles the
/// specifiers it may hold.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum SpecContext {
    /// A declaration, including a parameter declaration (C17 6.7p1): every
    /// specifier. A parameter may carry no storage class but `register`
    /// (6.7.6.3p2), but that diagnostic names the parameter, so it is made
    /// once the declarator has supplied the name
    /// ([`Parser::check_parameter_specifiers`]).
    Declaration,
    /// A structure or union member (6.7.2.1p1): a specifier-qualifier list,
    /// which admits an alignment specifier but no storage class or function
    /// specifier.
    Member,
    /// A type-name (6.7.7p1): a specifier-qualifier list, and no alignment
    /// specifier either (6.7.5p2).
    TypeName,
}

/// The declaration a redeclaration check is for.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum Redeclared {
    /// Any declaration, or a definition with a prototype.
    Declaration,
    /// A function definition with an identifier list, `int f(c) char c;`.
    IdentifierListDefinition,
}

/// The type a list of declaration specifiers names, and what came with it.
pub(crate) struct DeclSpecifiers {
    /// The type, still carrying the storage-class and function specifiers,
    /// which a declaration takes off its modifiers.
    pub(crate) ty: Type,
    /// `ty`'s interned id, when the specifiers resolved to an existing type
    /// -- a typedef name or `typeof` -- and added nothing to it. A type-name
    /// answers with that id rather than interning a copy: a composite type is
    /// never deduplicated, so the copy would be a distinct type.
    pub(crate) id: Option<TypeId>,
    /// Whether a type specifier was given, rather than `int` defaulted.
    ///
    /// C99 removed implicit int, but defaulting is still the right recovery,
    /// so a declaration decides whether the omission is an error at its own
    /// position: a K&R identifier list and an abstract parameter reach here
    /// legitimately with no specifier, and a declaration that stops at `;`
    /// has a different fault to report.
    pub(crate) explicit: bool,
    /// The extents of the variably modified array levels the specifiers
    /// introduced -- through a variably modified typedef name, or through
    /// `typeof(int[n])` or `typeof(v)` -- outermost-first. They cannot ride on the type:
    /// `int[n]`, `int[m]` and `int[]` all intern to one `TypeId`.
    pub(crate) vm_dims: Vec<Expr>,
}

/// A type specifier that names a complete type by itself.
enum Resolved {
    /// An existing type: a typedef name or `typeof`.
    Id(TypeId),
    /// A type the specifier built: `_Atomic(..)`, or a struct, union or enum
    /// specifier.
    Built(Type),
}

/// The modifier bits `short`, `long`, `signed`, `unsigned` and `_Complex`
/// record. Beside a resolved type they are a 6.7.2p2 violation the tally has
/// already reported, and are dropped rather than applied to it.
const SIZE_SIGN_COMPLEX: TypeModifiers = TypeModifiers::SIGNED
    .union(TypeModifiers::UNSIGNED)
    .union(TypeModifiers::SHORT)
    .union(TypeModifiers::LONG)
    .union(TypeModifiers::LONGLONG)
    .union(TypeModifiers::COMPLEX);

impl Parser<'_> {
    /// Parse a block-scope declaration and bind every name it declares.
    pub(super) fn parse_declaration_and_bind(&mut self) -> ParseResult<Declaration> {
        self.parse_block_declaration(false)
    }

    /// Parse the declaration in the first clause of a `for`, which may not
    /// name a storage class but `auto` or `register` (C17 6.8.5p3).
    pub(super) fn parse_for_init_declaration_and_bind(&mut self) -> ParseResult<Declaration> {
        self.parse_block_declaration(true)
    }

    fn parse_block_declaration(&mut self, for_init: bool) -> ParseResult<Declaration> {
        match self.parse_declaration(DeclScope::Block { for_init })? {
            ExternalDecl::Declaration(decl) => Ok(decl),
            ExternalDecl::FunctionDef(_) => {
                unreachable!("a function definition is recognised only at file scope")
            }
        }
    }

    /// The element count an array takes from a string-literal initializer:
    /// the characters plus the terminating null.
    ///
    /// One C byte per `char`, so count chars rather than bytes: `len()` is the
    /// UTF-8 encoded length, which over-counts every byte at or above 0x80 --
    /// `char a[] = "\x80";` came out as 3 bytes, not 2. `char16_t`/`char32_t`
    /// literals already carry code units, so their unit count is the element
    /// count; the *byte* size follows from the element type.
    pub(crate) fn string_initializer_len(&self, init: &Expr) -> Option<usize> {
        match &init.kind {
            ExprKind::StringLit(s) => Some(s.chars().count() + 1),
            ExprKind::Utf16StringLit(units) => Some(units.len() + 1),
            ExprKind::WideStringLit(units) | ExprKind::Utf32StringLit(units) => {
                Some(units.len() + 1)
            }
            _ => None,
        }
    }

    /// The string literal inside `{ ... }`, when that is what the braces hold.
    ///
    /// C17 6.7.9p14 lets the string literal initializing a character array be
    /// enclosed in braces, so `char b[] = {"hi"};` declares `char[3]` and
    /// copies the characters in. Counting the initializer list instead gives
    /// an array of one element holding a pointer's worth of nothing.
    ///
    /// Gated on the element type, because the same shape means something else
    /// one level up: `const char *p[] = {"aa", "bbb"}` is an array of two
    /// pointers, and `char names[3][4] = {"Sun"}` an array of arrays.
    pub(crate) fn braced_string_initializer<'a>(
        &self,
        elem_type: TypeId,
        elements: &'a [InitElement],
    ) -> Option<&'a Expr> {
        if !self.types.is_integer(elem_type) {
            return None;
        }
        let [only] = elements else { return None };
        if !only.designators.is_empty() {
            return None;
        }
        matches!(
            only.value.kind,
            ExprKind::StringLit(_)
                | ExprKind::WideStringLit(_)
                | ExprKind::Utf16StringLit(_)
                | ExprKind::Utf32StringLit(_)
        )
        .then_some(only.value.as_ref())
    }

    /// Walk the initializer `init` for an object of type `typ`, in place:
    ///
    /// - C17 6.7.9p7: each designator names a member of, or an index into,
    ///   the current object -- `.y` in an initializer for a struct with no
    ///   `y` is a constraint violation, which was dropped in silence and its
    ///   value with it.
    /// - 6.7.9p11: a scalar's initializer is an expression, optionally in
    ///   braces; the braces come off here, so nothing downstream meets a
    ///   brace list for a scalar. More than one level is gcc's warning --
    ///   and was an internal error for an automatic object's member.
    ///
    /// Nested lists are followed wherever the subobject they initialize is
    /// known: through a designator, or by position until brace elision makes
    /// the position unclear.
    pub(crate) fn walk_initializer(&mut self, typ: TypeId, init: &mut Expr) {
        if self.types.is_scalar(typ) {
            self.strip_scalar_braces(init, 0);
        } else if let ExprKind::InitList { elements } = &mut init.kind {
            // `char s[4] = {"abc"}` is the string in optional braces
            // (6.7.9p14), checked as the string -- not as a pointer stored
            // into `s[0]`, which was a warning gcc does not give.
            let braced_string = (self.types.kind(typ) == TypeKind::Array)
                .then(|| self.types.base_type(typ))
                .flatten()
                .and_then(|elem| self.braced_string_initializer(elem, elements))
                .cloned();
            match braced_string {
                Some(string) => self.check_initializer_types(typ, &string),
                None => self.walk_initializer_elements(typ, elements),
            }
        }
    }

    /// Replace a braced initializer for a scalar with the expression it
    /// holds; `levels` is how many braces enclose `init` already.
    fn strip_scalar_braces(&self, init: &mut Expr, levels: u32) {
        let ExprKind::InitList { elements } = &mut init.kind else {
            return;
        };
        // `{}` is gcc's zero initializer, and a designator in a scalar's list
        // is diagnosed where the list is checked; both stay lists.
        if elements.is_empty() || !elements[0].designators.is_empty() {
            return;
        }
        if levels > 0 {
            diag::warning(init.pos, &gettext("braces around scalar initializer"));
        }
        // Only the first value initializes the scalar (6.7.9p11).
        if elements.len() > 1 {
            diag::warning(init.pos, &gettext("excess elements in scalar initializer"));
        }
        let mut first = elements.swap_remove(0).value;
        self.strip_scalar_braces(&mut first, levels + 1);
        *init = *first;
    }

    /// [`Self::walk_initializer`] for the elements of a brace list
    /// initializing an aggregate of type `typ`.
    pub(crate) fn walk_initializer_elements(&mut self, typ: TypeId, elements: &mut [InitElement]) {
        // Positional elements after a designator chain continue inside it
        // (6.7.9p17); spelling out where they land lets the one-level cursor
        // below -- and everything downstream -- place them.
        crate::parse::ast::spell_out_designator_continuations(self.types, elements, typ);
        let kind = self.types.kind(typ);
        let members: Vec<TypeId> = match kind {
            // A union's positional initializer is for its first named member.
            TypeKind::Struct | TypeKind::Union => self
                .types
                .composite(typ)
                .map(|c| {
                    c.members
                        .iter()
                        .filter(|m| m.name != StringId::EMPTY || m.bit_width.is_none())
                        .map(|m| m.typ)
                        .take(if kind == TypeKind::Union {
                            1
                        } else {
                            usize::MAX
                        })
                        .collect()
                })
                .unwrap_or_default(),
            _ => Vec::new(),
        };
        // The position of the next positional element: an index into
        // `members` for a struct, or `None` once it can no longer be known.
        let mut next: Option<usize> = Some(0);
        let mut idx = 0;
        while idx < elements.len() {
            let element = &elements[idx];
            let sub = if element.designators.is_empty() {
                let sub = match kind {
                    TypeKind::Array => self.types.base_type(typ),
                    TypeKind::Struct | TypeKind::Union => {
                        next.and_then(|i| members.get(i).copied())
                    }
                    _ => None,
                };
                next = next.map(|i| i + 1);
                sub
            } else {
                let resolved =
                    self.designated_subobject(typ, &element.designators, element.value.pos);
                // After `.m = v` the next positional element is the member
                // after `m`, which only a direct member pins down.
                next = match element.designators.first() {
                    Some(Designator::Field(name)) if kind == TypeKind::Struct => self
                        .types
                        .composite(typ)
                        .and_then(|c| {
                            c.members
                                .iter()
                                .filter(|m| m.name != StringId::EMPTY || m.bit_width.is_none())
                                .position(|m| m.name == *name)
                        })
                        .map(|i| i + 1),
                    // An anonymous member, by its place in the member list.
                    Some(Designator::Member(position)) if kind == TypeKind::Struct => {
                        self.types.composite(typ).map(|c| {
                            c.members[..(*position + 1).min(c.members.len())]
                                .iter()
                                .filter(|m| m.is_initializable())
                                .count()
                        })
                    }
                    _ => None,
                };
                resolved
            };
            // A non-list value for an aggregate subobject is brace elision
            // (6.7.9p20): it and the elements after it that the subobject
            // takes are walked as the subobject's own list would be, so a
            // designator inside a braced element among them is checked
            // against the subobject it lands in.
            if let Some(t) = sub
                .filter(|&t| crate::parse::ast::is_brace_elision_candidate(self.types, element, t))
            {
                let span = crate::parse::ast::brace_elision_span(self.types, elements, idx, t);
                // The first element's designators chose `t`; inside `t`'s list
                // the element is positional.
                let chosen = std::mem::take(&mut elements[idx].designators);
                self.walk_initializer_elements(t, &mut elements[span.clone()]);
                elements[idx].designators = chosen;
                idx = span.end;
                continue;
            }
            let element = &mut elements[idx];
            idx += 1;
            if let Some(sub) = sub {
                self.walk_initializer(sub, &mut element.value);
                // A nested list is counted against its own subobject, and a
                // scalar's value converted to it as by assignment (6.7.9p11),
                // as the outermost initializer is against the object.
                self.check_excess_initializers(sub, &element.value);
                // So is a string literal's fit to the array it initializes
                // (6.7.9p14-15) -- the one unbraced form an array takes here.
                let unbraced_array = self.types.kind(sub) == TypeKind::Array
                    && !matches!(element.value.kind, ExprKind::InitList { .. });
                if self.types.is_scalar(sub) || unbraced_array {
                    self.check_initializer_types(sub, &element.value);
                }
            }
        }
    }

    /// The subobject a designator chain names in an object of type `typ`,
    /// reporting the first designator that names nothing.
    fn designated_subobject(
        &self,
        typ: TypeId,
        designators: &[Designator],
        pos: Position,
    ) -> Option<TypeId> {
        let mut current = typ;
        for designator in designators {
            current = match designator {
                Designator::Field(name) => {
                    if !matches!(self.types.kind(current), TypeKind::Struct | TypeKind::Union) {
                        diag::error(
                            pos,
                            &gettext("field name not in record or union initializer"),
                        );
                        return None;
                    }
                    match self.types.find_member(current, *name) {
                        Some(member) => member.typ,
                        None => {
                            let named = self.types.format_type(current, Some(self.idents));
                            let spelled = self.idents.get_opt(*name).unwrap_or("");
                            diag::error_args(
                                pos,
                                "'{0}' has no member named '{1}'",
                                &[&named, spelled],
                            );
                            return None;
                        }
                    }
                }
                Designator::Index(_) | Designator::IndexRange(..) => {
                    if self.types.kind(current) != TypeKind::Array {
                        diag::error(pos, &gettext("array index in non-array initializer"));
                        return None;
                    }
                    self.types.base_type(current)?
                }
                // Only the parser writes one, and only for a member that is
                // there.
                Designator::Member(position) => {
                    self.types.composite(current)?.members.get(*position)?.typ
                }
            };
        }
        Some(current)
    }

    /// Report an array designator that addresses past the end of its array.
    ///
    /// Only the outermost list is walked, and only when the bound is known: an
    /// array sized *by* this initializer cannot overflow it, and a designator
    /// inside a nested list addresses a different object than `typ`.
    fn check_designator_bounds(&self, typ: TypeId, elements: &[InitElement]) {
        if self.types.kind(typ) != TypeKind::Array {
            return;
        }
        let Some(capacity) = self.types.array_size(typ).filter(|&n| n > 0) else {
            return;
        };
        for element in elements {
            let Some(designator) = element.designators.first() else {
                continue;
            };
            let end = match designator {
                Designator::Index(i) => *i,
                Designator::IndexRange(_, hi) => *hi,
                Designator::Field(_) | Designator::Member(_) => continue,
            };
            if end >= capacity as i64 {
                diag::error_args(
                    element.value.pos,
                    "array index in initializer exceeds array bounds ({0} >= {1})",
                    &[&end.to_string(), &capacity.to_string()],
                );
                return;
            }
        }
    }

    /// Report an initializer list with more elements than the object it
    /// initializes can hold (C17 6.7.9p2).
    ///
    /// Brace elision lets an aggregate member consume several consecutive
    /// elements -- `struct P p[2] = {1,2,3,4}` fills two two-field structs,
    /// and `union { struct { char s[4]; int n; } in; } u = {"ab", 4}` puts
    /// both into the union's one member -- so the list is walked subobject
    /// by subobject rather than counted. A designator moves the walk to the
    /// subobject it names, and the elements after it continue from there
    /// (6.7.9p17). An array with no bound, or a flexible array member, takes
    /// every element left.
    pub(super) fn check_excess_initializers(&self, typ: TypeId, init: &Expr) {
        let ExprKind::InitList { elements } = &init.kind else {
            return;
        };
        // A designator's position can itself be out of range, and nothing
        // checked that anywhere: `int a[4] = {[10] = 7};` compiled and wrote
        // past the array. GCC rejects it. Ranges make it easy to write by
        // accident, so the bound is checked here where the array's size is
        // known.
        self.check_designator_bounds(typ, elements);

        let message = match self.types.kind(typ) {
            // An absent or zero size is an array whose bound came from this
            // very initializer, so it cannot overflow.
            TypeKind::Array if self.types.array_size(typ).is_some_and(|n| n > 0) => {
                "excess elements in array initializer"
            }
            TypeKind::Struct
                if self
                    .types
                    .composite(typ)
                    .is_some_and(|c| !c.members.is_empty()) =>
            {
                "excess elements in struct initializer"
            }
            TypeKind::Union => "excess elements in union initializer",
            // A scalar's braces are gone by now: `strip_scalar_braces`
            // warned about any excess as it took them off.
            _ => return,
        };
        // The elements the subobjects take, each one or -- by brace
        // elision -- as many as its own subobjects take (6.7.9p20).
        if crate::parse::ast::initializer_list_end(self.types, elements, typ) < elements.len() {
            diag::warning(init.pos, &gettext(message));
        }
    }

    pub(super) fn infer_array_size_from_init(&mut self, typ: TypeId, init: &Expr) -> TypeId {
        if self.types.kind(typ) != TypeKind::Array {
            return typ;
        }

        let array_size = self.types.get(typ).array_size;
        // Check if array size is incomplete (0 or None)
        if array_size != Some(0) && array_size.is_some() {
            return typ;
        }

        // `{"hi"}` initializes the array with the string, not with one
        // element (C17 6.7.9p14), so look through the braces first.
        let elem_type = self.types.base_type(typ).unwrap_or(self.types.int_id);
        let init = match &init.kind {
            ExprKind::InitList { elements } => self
                .braced_string_initializer(elem_type, elements)
                .unwrap_or(init),
            _ => init,
        };

        let new_size = match &init.kind {
            ExprKind::InitList { elements } => {
                Some(self.array_size_from_elements(elements, elem_type))
            }
            // A compound literal of array type already has its size, worked
            // out from its own braces. Without this the declared array stayed
            // incomplete and `sizeof` on it failed, although the initializer
            // said exactly how long it was.
            ExprKind::CompoundLiteral { typ: lit_typ, .. } => self.types.get(*lit_typ).array_size,
            _ => self.string_initializer_len(init),
        };

        if let Some(size) = new_size {
            // Update type with actual size from initializer
            // Preserve modifiers (like const, static)
            let modifiers = self.types.modifiers(typ);
            let mut arr_type = Type::array(elem_type, size);
            arr_type.modifiers = modifiers;
            self.types.intern(arr_type)
        } else {
            typ
        }
    }

    /// C17 6.7.6.2p1: an array's size expression shall have integer type.
    /// Without it the non-constant fallback takes over and `int a[1.5];`
    /// parses as a variable length array sized from a `double`.
    pub(super) fn check_array_size_type(&self, expr: &Expr, pos: Position) -> ParseResult<()> {
        match expr.typ {
            Some(t) if !self.types.is_integer(t) => Err(ParseError::new(
                "size of array has non-integer type".to_string(),
                pos,
            )),
            _ => Ok(()),
        }
    }

    /// Derive an array type, refusing an extent past
    /// [`TypeTable::max_object_bytes`] (`PTRDIFF_MAX`), gcc's bound too.
    /// Enforced here, where the element type is known, so an outer dimension
    /// is measured against an inner one already within the bound.
    pub(super) fn derive_array_type(
        &mut self,
        elem: TypeId,
        size: Option<usize>,
        pos: Position,
    ) -> Result<TypeId, ParseError> {
        if let Some(count) = size {
            let total = (count as u128) * (self.types.size_bytes(elem) as u128);
            let max = self.types.max_object_bytes();
            if total > max as u128 {
                return Err(ParseError::new(
                    format!(
                        "size of array is too large: it exceeds the maximum \
                         object size of {max} bytes"
                    ),
                    pos,
                ));
            }
        }
        Ok(self.types.intern(Type {
            kind: TypeKind::Array,
            base: Some(elem),
            array_size: size,
            ..Default::default()
        }))
    }

    /// Refuse an object the backend cannot give a stack slot.
    ///
    /// C17 puts no limit here; this one is c17's. Both backends address a local
    /// and a stacked argument by a signed 32-bit displacement from the frame
    /// register, so an object past [`TypeTable::MAX_STACK_OBJECT_BYTES`] has no
    /// slot to be given -- and every size conversion in `arch/` and `abi/` was
    /// a bare `as i32` that wrapped, so `char a[3000000000];` in a function got
    /// an eight-byte slot and a `subq $32, %rsp` frame with no diagnostic.
    ///
    /// Asked only of an object with *automatic* storage duration and of a
    /// by-value parameter type. A static or file-scope object of the same size
    /// is addressed symbolically and works, so widening this to every
    /// declaration would make it a second, tighter `max_object_bytes` and would
    /// reject what `diagnostics_largest_describable_object_is_accepted`
    /// requires.
    ///
    /// A variable length array is not asked: its extent is a run-time value and
    /// there is nothing to compare. Neither is `alloca`, for the same reason.
    /// `crate::abi::slot_bytes` is the backstop for what this cannot see --
    /// those two, a compound literal's anonymous local, and the `__sret` local
    /// a call to a function returning a large aggregate allocates.
    pub(super) fn check_stack_object_size(
        &self,
        typ: TypeId,
        pos: Position,
        what: &str,
    ) -> Result<(), ParseError> {
        let bytes = self.types.size_bytes(typ);
        if bytes <= TypeTable::MAX_STACK_OBJECT_BYTES {
            return Ok(());
        }
        Err(ParseError::new(
            format!(
                "size of {what} of type '{}' is {bytes} bytes, which exceeds \
                 the maximum stack object size of {} bytes",
                self.types.format_type(typ, Some(self.idents)),
                TypeTable::MAX_STACK_OBJECT_BYTES,
            ),
            pos,
        ))
    }

    pub(crate) fn array_size_from_elements(
        &self,
        elements: &[InitElement],
        elem_type: TypeId,
    ) -> usize {
        let mut max_index: i64 = -1;
        let mut current_index: i64 = 0;
        let mut idx = 0usize;

        while idx < elements.len() {
            let element = &elements[idx];
            let (_, last, designated) =
                crate::parse::ast::array_slot(&element.designators, &mut current_index);
            max_index = max_index.max(last);

            // An element takes several list elements for this one slot when
            // it elides braces into it, or continues a designator chain into
            // it -- `[1].x = 1, 2` gives `[1]` the 2 as well.
            let rest = designated.map_or(&[][..], |pos| &element.designators[pos + 1..]);
            idx =
                crate::parse::ast::designated_span(self.types, elements, idx, elem_type, rest).end;
        }

        if max_index < 0 {
            0
        } else {
            (max_index + 1) as usize
        }
    }
}

impl<'a> Parser<'a> {
    /// Parse a list of declaration specifiers (C17 6.7p1) -- or, in a member
    /// declaration or a type-name, a specifier-qualifier list.
    ///
    /// The one specifier loop. There used to be two, one for declarations and
    /// one for type-names, and they drifted: the type-name copy kept no
    /// [`SpecifierTally`], dropped the qualifiers written before `typeof`,
    /// never checked `_Atomic` on an array, and answered "not a type" after
    /// consuming tokens.
    ///
    /// 6.7p1 lets the specifiers come in any order, so a type specifier that
    /// names a complete type by itself -- `typeof`, `_Atomic(..)`, a struct,
    /// union or enum specifier, a typedef name -- is recorded and the loop
    /// goes on: `typeof(int) const x;` and `enum E { A } static e;` read as
    /// written. Returning as soon as one was parsed left the specifier after
    /// it to be read as the declarator's name.
    ///
    /// A type-name's caller decides beforehand, from the current token alone,
    /// whether one begins here ([`Self::starts_type_name`]). Past that point
    /// the list is committed and every fault is reported.
    pub(super) fn parse_declaration_specifiers(
        &mut self,
        ctx: SpecContext,
    ) -> ParseResult<DeclSpecifiers> {
        let start = self.current_pos();
        // The spellings the tally records outlive this borrow of `self`.
        let idents: &'a crate::strings::StringTable = self.idents;
        let mut modifiers = TypeModifiers::empty();
        let mut base_kind: Option<TypeKind> = None;
        // Which `_FloatN`/`_FloatNx` name, if any, gave `base_kind`.
        let mut float_class = FloatClass::Standard;
        let mut resolved: Option<Resolved> = None;
        let mut vm_dims = Vec::new();
        // Where `_Atomic` was written, for the constraint checked at the end.
        let mut atomic_pos: Option<Position> = None;
        // C17 6.7.2p2 admits only a fixed list of specifier combinations. The
        // loop below merely overwrites `base_kind`, so the specifiers are
        // recorded here and checked once the list is complete.
        let mut tally = SpecifierTally::default();

        // Skip any leading __attribute__
        self.skip_extensions();

        while let Some(name_id) = self.current_ident() {
            let pos = self.current_pos();
            // Every spelling of `const`, `volatile` and `restrict`, from the
            // one shared answer.
            if let Some(m) = super::cv_qualifier_modifier(name_id) {
                self.advance();
                modifiers |= m;
                continue;
            }
            match name_id {
                // An attribute can sit anywhere among the specifiers and goes
                // to the pending slots; a type-name has set the enclosing
                // declaration's aside ([`Self::try_parse_type_name_vm`]).
                crate::kw::GNU_ATTRIBUTE | crate::kw::GNU_ATTRIBUTE2 => {
                    self.skip_extensions();
                    continue;
                }
                crate::kw::STATIC
                | crate::kw::EXTERN
                | crate::kw::REGISTER
                | crate::kw::AUTO
                | crate::kw::TYPEDEF => {
                    if self.reject_outside_declaration(ctx) {
                        continue;
                    }
                    let (name, m) = match name_id {
                        crate::kw::STATIC => ("static", TypeModifiers::STATIC),
                        crate::kw::EXTERN => ("extern", TypeModifiers::EXTERN),
                        crate::kw::REGISTER => ("register", TypeModifiers::REGISTER),
                        crate::kw::AUTO => ("auto", TypeModifiers::AUTO),
                        _ => ("typedef", TypeModifiers::TYPEDEF),
                    };
                    tally.note_storage_class(name, pos);
                    self.advance();
                    modifiers |= m;
                }
                crate::kw::THREAD_LOCAL | crate::kw::GNU_THREAD => {
                    if self.reject_outside_declaration(ctx) {
                        continue;
                    }
                    self.advance();
                    modifiers |= TypeModifiers::THREAD_LOCAL;
                }
                crate::kw::INLINE | crate::kw::GNU_INLINE2 | crate::kw::GNU_INLINE => {
                    if self.reject_outside_declaration(ctx) {
                        continue;
                    }
                    self.advance();
                    modifiers |= TypeModifiers::INLINE;
                }
                crate::kw::NORETURN | crate::kw::GNU_NORETURN => {
                    if self.reject_outside_declaration(ctx) {
                        continue;
                    }
                    self.advance();
                    modifiers |= TypeModifiers::NORETURN;
                }
                crate::kw::SIGNED => {
                    tally.note_sign("signed", pos);
                    self.advance();
                    modifiers |= TypeModifiers::SIGNED;
                }
                crate::kw::UNSIGNED => {
                    tally.note_sign("unsigned", pos);
                    self.advance();
                    modifiers |= TypeModifiers::UNSIGNED;
                }
                crate::kw::COMPLEX | crate::kw::GNU_COMPLEX | crate::kw::GNU_COMPLEX2 => {
                    tally.complex = Some(pos);
                    self.advance();
                    modifiers |= TypeModifiers::COMPLEX;
                }
                // After a type specifier and before what can only follow a
                // declarator's name, it *is* the name, as a typedef name would
                // be: `int _Imaginary;` is a keyword misused, which the
                // declarator reports, not a type c17 lacks. A type-name has
                // no declarator name, so there it is always the specifier.
                crate::kw::IMAGINARY
                    if !matches!(ctx, SpecContext::TypeName)
                        && tally.has_type_specifier()
                        && b";,=[():".iter().any(|&c| self.next_token_is_special(c)) =>
                {
                    break;
                }
                // Imaginary types belong to Annex G, which binds only an
                // implementation that defines `__STDC_IEC_559_COMPLEX__`, and
                // c17 does not; without them `_Imaginary` is no permitted type
                // specifier (C17 6.7.2p2), which needs a diagnostic. One, and
                // then the declaration goes on as if `_Complex` had been
                // written, so nothing after it reports the same mistake again.
                crate::kw::IMAGINARY => {
                    diag::error(pos, "imaginary types are not supported");
                    tally.complex = Some(pos);
                    self.advance();
                    modifiers |= TypeModifiers::COMPLEX;
                }
                // `_Atomic` immediately followed by `(` is the type specifier
                // `_Atomic(type-name)` (C17 6.7.2.4p4); anywhere else it is
                // the qualifier.
                crate::kw::ATOMIC => {
                    atomic_pos.get_or_insert(pos);
                    if !self.next_token_is_open_paren() {
                        self.advance();
                        modifiers |= TypeModifiers::ATOMIC;
                        continue;
                    }
                    self.advance(); // consume `_Atomic`
                    self.advance(); // consume '('
                    let Some(inner) = self.try_parse_type_name() else {
                        return Err(ParseError::new(
                            "expected type-name in _Atomic(...)",
                            self.current_pos(),
                        ));
                    };
                    self.expect_special(b')')?;
                    // C17 6.7.2.4p3: nor an atomic or qualified type.
                    if self.types.modifiers(inner).intersects(
                        TypeModifiers::CONST
                            | TypeModifiers::VOLATILE
                            | TypeModifiers::RESTRICT
                            | TypeModifiers::ATOMIC,
                    ) {
                        diag::error(pos, &gettext("'_Atomic' applied to a qualified type"));
                    }
                    tally.note_data_type("_Atomic", pos);
                    let mut atomic = self.types.get(inner).clone();
                    atomic.modifiers |= TypeModifiers::ATOMIC;
                    resolved = Some(Resolved::Built(atomic));
                }
                crate::kw::ALIGNAS => {
                    self.parse_alignas_specifier()?;
                    // C11 6.7.5p2 lists no type-name among the places an
                    // alignment specifier may appear.
                    if ctx == SpecContext::TypeName {
                        self.reject_alignas_in("a type name");
                    }
                }
                crate::kw::SHORT => {
                    tally.note_size("short", pos);
                    self.advance();
                    modifiers |= TypeModifiers::SHORT;
                    // C17 6.7.2p2 lists the declaration specifiers as a set,
                    // so `int short` names what `short int` names. A tally
                    // that had already seen `int` must still take the size:
                    // testing only `is_none()` left the kind at `Int` and
                    // gave `int short` four bytes.
                    if base_kind.is_none() || base_kind == Some(TypeKind::Int) {
                        base_kind = Some(TypeKind::Short);
                    }
                }
                crate::kw::LONG => {
                    tally.note_size("long", pos);
                    self.advance();
                    if modifiers.contains(TypeModifiers::LONG) {
                        modifiers |= TypeModifiers::LONGLONG;
                        base_kind = Some(TypeKind::LongLong);
                    } else {
                        modifiers |= TypeModifiers::LONG;
                        // long double case
                        if base_kind == Some(TypeKind::Double) {
                            base_kind = Some(TypeKind::LongDouble);
                        } else if base_kind.is_none() || base_kind == Some(TypeKind::Int) {
                            // `int long` is `long int`; see the SHORT arm.
                            base_kind = Some(TypeKind::Long);
                        }
                    }
                }
                crate::kw::VOID => {
                    tally.note_data_type("void", pos);
                    self.advance();
                    base_kind = Some(TypeKind::Void);
                }
                crate::kw::CHAR => {
                    tally.note_data_type("char", pos);
                    self.advance();
                    base_kind = Some(TypeKind::Char);
                }
                crate::kw::INT => {
                    tally.note_data_type("int", pos);
                    self.advance();
                    if base_kind.is_none()
                        || !matches!(
                            base_kind,
                            Some(TypeKind::Short) | Some(TypeKind::Long) | Some(TypeKind::LongLong)
                        )
                    {
                        base_kind = Some(TypeKind::Int);
                    }
                }
                crate::kw::FLOAT => {
                    tally.note_data_type("float", pos);
                    self.advance();
                    base_kind = Some(TypeKind::Float);
                }
                crate::kw::DOUBLE => {
                    tally.note_data_type("double", pos);
                    self.advance();
                    // Handle long double
                    if modifiers.contains(TypeModifiers::LONG) {
                        base_kind = Some(TypeKind::LongDouble);
                    } else {
                        base_kind = Some(TypeKind::Double);
                    }
                }
                crate::kw::FLOAT16 => {
                    if tally.alias_is_declarator_name() {
                        break;
                    }
                    tally.note_data_type("_Float16", pos);
                    self.advance();
                    base_kind = Some(TypeKind::Float16);
                }
                // C23's interchange and extended types (TS 18661-3): the
                // format of a standard type, under a name of their own --
                // see `FloatClass`.
                crate::kw::FLOAT32
                | crate::kw::FLOAT64
                | crate::kw::FLOAT32X
                | crate::kw::FLOAT64X => {
                    if tally.alias_is_declarator_name() {
                        break;
                    }
                    let (spelled, kind, class) = match name_id {
                        crate::kw::FLOAT32 => {
                            ("_Float32", TypeKind::Float, FloatClass::Interchange)
                        }
                        crate::kw::FLOAT64 => {
                            ("_Float64", TypeKind::Double, FloatClass::Interchange)
                        }
                        crate::kw::FLOAT32X => {
                            ("_Float32x", TypeKind::Double, FloatClass::Extended)
                        }
                        _ => ("_Float64x", TypeKind::LongDouble, FloatClass::Extended),
                    };
                    if kind == TypeKind::LongDouble && !self.types.has_float64x() {
                        return Err(ParseError::new(
                            "_Float64x is not supported on this target",
                            pos,
                        ));
                    }
                    tally.note_data_type(spelled, pos);
                    self.advance();
                    base_kind = Some(kind);
                    float_class = class;
                }
                crate::kw::FLOAT128 | crate::kw::FLOAT128_ALIAS => {
                    // IEEE binary128. `_Float128` is the C23/TS 18661-3
                    // spelling and `__float128` the GCC one; on a target whose
                    // long double is already binary128 glibc typedefs the
                    // former to `long double`, so it takes the same
                    // yield-to-the-declarator rule as the other aliases.
                    if tally.alias_is_declarator_name() {
                        break;
                    }
                    if !self.types.has_float128() {
                        return Err(ParseError::new(
                            "__float128 is not supported on this target",
                            pos,
                        ));
                    }
                    tally.note_data_type("__float128", pos);
                    self.advance();
                    base_kind = Some(TypeKind::Float128);
                }
                crate::kw::BOOL => {
                    tally.note_data_type("_Bool", pos);
                    self.advance();
                    base_kind = Some(TypeKind::Bool);
                }
                crate::kw::INT128 => {
                    tally.note_data_type("__int128", pos);
                    self.advance();
                    base_kind = Some(TypeKind::Int128);
                }
                crate::kw::INT128_T => {
                    if tally.alias_is_declarator_name() {
                        break;
                    }
                    tally.note_data_type("__int128", pos);
                    self.advance();
                    base_kind = Some(TypeKind::Int128);
                }
                crate::kw::UINT128_T => {
                    if tally.alias_is_declarator_name() {
                        break;
                    }
                    tally.note_data_type("__int128", pos);
                    self.advance();
                    modifiers |= TypeModifiers::UNSIGNED;
                    base_kind = Some(TypeKind::Int128);
                }
                crate::kw::BUILTIN_VA_LIST => {
                    tally.note_data_type("__builtin_va_list", pos);
                    self.advance();
                    base_kind = Some(TypeKind::VaList);
                }
                crate::kw::BUILTIN_MS_VA_LIST => {
                    // A name gcc knows only on x86-64: anywhere else it names
                    // no type, and is diagnosed as gcc diagnoses it.
                    if !crate::kw::exists_on(name_id, self.types.target().arch) {
                        diag::error(pos, "unknown type name '__builtin_ms_va_list'");
                    }
                    tally.note_data_type("__builtin_ms_va_list", pos);
                    self.advance();
                    resolved = Some(Resolved::Id(self.types.ms_va_list_id));
                }
                crate::kw::TYPEOF | crate::kw::GNU_TYPEOF | crate::kw::GNU_TYPEOF2 => {
                    // `typeof` always takes a parenthesized operand, so
                    // without one this is not a type specifier -- it is the
                    // declarator's name. gcc accepts `int typeof;` in C17,
                    // because `typeof` is a GNU extension rather than a C17
                    // keyword; consuming it here and then demanding `(` made
                    // c17 reject the declaration outright.
                    if !self.next_token_is_open_paren() {
                        break;
                    }
                    self.advance(); // consume typeof
                    self.advance(); // consume '('

                    // typeof can take either a type name or an expression;
                    // try the type name first, keeping any variably modified
                    // extents it found: `typeof(int[n])` is a complete type
                    // whose size is `n * sizeof(int)`, and dropping them left
                    // it indistinguishable from `int[]` -- in a declaration as
                    // much as in `sizeof`.
                    let (typ, dims) = match self.try_parse_type_name_vm() {
                        Some(named) => named,
                        None => {
                            let expr = self.parse_expression()?;
                            let dims = self.typeof_object_extents(&expr);
                            (expr.typ.unwrap_or(self.types.int_id), dims)
                        }
                    };
                    self.expect_special(b')')?;
                    tally.note_data_type(idents.get(name_id), pos);

                    // The operand's declaration contributes its type and
                    // qualifiers, never its storage class: `static int g;
                    // typeof(g) c;` declares an automatic `c`.
                    resolved = Some(Resolved::Id(self.types.without_decl_specifiers(typ)));
                    vm_dims = dims;
                }
                crate::kw::ENUM | crate::kw::STRUCT | crate::kw::UNION => {
                    // Nothing written ahead of it, so a `;` right after makes
                    // this the declaration `struct S;` (C17 6.7.2.3p7). A
                    // qualifier or storage class ahead of it makes an empty
                    // declaration that redeclares nothing.
                    let alone = ctx == SpecContext::Declaration
                        && modifiers.is_empty()
                        && !tally.has_type_specifier();
                    tally.note_data_type(idents.get(name_id), pos);
                    let tag_start = self.pos;
                    let parsed = if name_id == crate::kw::ENUM {
                        self.parse_enum_specifier()
                    } else {
                        self.parse_struct_or_union_specifier(name_id == crate::kw::UNION, alone)
                    };
                    resolved = Some(match parsed {
                        Ok(typ) => Resolved::Built(typ),
                        // A type-name reports the fault where it arose and
                        // steps over the specifier, so the declarator after it
                        // still parses: in `*(struct { char x[n]; } *)p` the
                        // cast is still a cast to a pointer.
                        Err(e) if ctx == SpecContext::TypeName => {
                            diag::error(e.pos, &e.message);
                            self.skip_failed_tag_specifier(tag_start);
                            Resolved::Id(self.types.int_id)
                        }
                        Err(e) => return Err(e),
                    });
                }
                _ => {
                    // A typedef name is a type specifier only where no other
                    // has been given; after one it is the declarator's name.
                    if !tally.has_type_specifier() {
                        if let Some((sym, typedef_type_id)) =
                            self.symbols.lookup_typedef_symbol(name_id)
                        {
                            self.advance();
                            tally.note_data_type(idents.get(name_id), pos);
                            // A variably modified typedef carries extents the
                            // `TypeId` cannot: hand the declarator list the
                            // names of the ones this typedef already evaluated
                            // (C17 6.7.7p3), not its size expressions.
                            vm_dims = self.vm_typedef_extents(sym).unwrap_or_default();
                            resolved = Some(Resolved::Id(typedef_type_id));
                            continue;
                        }
                    }
                    break;
                }
            }
        }

        tally.check();
        let explicit = tally.has_type_specifier();

        let (ty, id) = match resolved {
            Some(resolved) => {
                let added = modifiers.difference(SIZE_SIGN_COMPLEX);
                match resolved {
                    Resolved::Id(rid) => {
                        let named = self.types.get(rid);
                        let quals = added & Type::QUALIFIERS;
                        if named.kind == TypeKind::Array && !quals.is_empty() {
                            // C17 6.7.3p10: a qualifier on an array typedef
                            // qualifies its elements, as `const int a[3]`
                            // does -- on the array itself it was ignored, so
                            // `const A x` was writable and `x[0]` an `int`.
                            let qualified = self.types.qualified_with(rid, quals);
                            let mut ty = self.types.get(qualified).clone();
                            ty.modifiers.remove(TypeModifiers::TYPEDEF);
                            ty.modifiers |= added.difference(Type::QUALIFIERS);
                            (ty, None)
                        } else if added.is_empty()
                            && !named.modifiers.contains(TypeModifiers::TYPEDEF)
                        {
                            (named.clone(), Some(rid))
                        } else {
                            // Drop the TYPEDEF bit either way. It records how
                            // the name was *declared*, not anything about the
                            // type, and leaving it on made a typedef's type
                            // differ from the type it aliases -- so
                            // `__builtin_types_compatible_p(int, MyInt)`
                            // answered 0.
                            let mut ty = named.clone();
                            ty.modifiers.remove(TypeModifiers::TYPEDEF);
                            ty.modifiers |= added;
                            (ty, None)
                        }
                    }
                    Resolved::Built(mut ty) => {
                        ty.modifiers |= added;
                        (ty, None)
                    }
                }
            }
            None => {
                // `_Complex` with no base type is `_Complex double`, which is
                // what gcc gives it. C17 requires a base (6.7.2p2 lists only
                // the three floating spellings), so this is the GNU reading;
                // and now that c17 has `_Complex int`, defaulting to `int` like
                // everything else would have made `_Complex v;` an eight-byte
                // integer pair rather than gcc's sixteen-byte double one.
                //
                // Only a *bare* `_Complex`: a signedness modifier names an
                // integer base of its own -- `_Complex unsigned` is `_Complex
                // unsigned int`, eight bytes, not sixteen.
                let kind = match base_kind {
                    Some(k) => k,
                    None if modifiers.contains(TypeModifiers::COMPLEX)
                        && !modifiers
                            .intersects(TypeModifiers::SIGNED | TypeModifiers::UNSIGNED) =>
                    {
                        TypeKind::Double
                    }
                    None => TypeKind::Int,
                };
                (
                    Type {
                        float_class,
                        ..Type::with_modifiers(kind, modifiers)
                    },
                    None,
                )
            }
        };

        // C17 6.7.2.4p3 and 6.7.3p3: neither `_Atomic(T)` nor the `_Atomic`
        // qualifier may name an array or function type. The qualifier reaches
        // one only through a typedef -- `_Atomic int a[4]` is an array *of*
        // atomic ints, and perfectly legal. Asked once, of the finished type:
        // the specifier arm and a wrapper around the loop each asked before,
        // and `_Atomic(int[3]) v;` drew the error twice.
        if let Some(at) = atomic_pos {
            let what = match ty.kind {
                TypeKind::Array => Some("an array"),
                TypeKind::Function => Some("a function"),
                _ => None,
            };
            if let Some(what) = what {
                diag::error_args(at, "'_Atomic' cannot be applied to {0} type", &[what]);
            }
        }

        // A member or a type-name has no `;`-only form and no identifier
        // list: nothing but a missing type specifier leaves it without one.
        if ctx != SpecContext::Declaration {
            self.check_implicit_int(explicit, start);
        }

        // A `vector_size` among the specifiers -- written before the type or
        // inside it -- makes the type they name a vector, for every
        // declarator, as gcc reads it. Held for the declarator, it reached
        // the first one only, and a function's own type rather than its
        // return type.
        let (ty, id) = if self.pending_vector_size.is_some() {
            let outer = ty.modifiers & (Type::DECL_SPECIFIERS | Type::QUALIFIERS);
            let mut elem = ty;
            elem.modifiers.remove(outer);
            let elem = self.types.intern(elem);
            // A mode written with it names the element's width, as in
            // `int __attribute__((mode(SI), vector_size(8)))`.
            let elem = self.apply_pending_mode(elem);
            let vector = self.apply_pending_vector_size(elem);
            let mut ty = self.types.get(vector).clone();
            ty.modifiers |= outer;
            (ty, None)
        } else {
            (ty, id)
        };

        Ok(DeclSpecifiers {
            ty,
            id,
            explicit,
            vm_dims,
        })
    }

    /// Refuse a storage-class or function specifier outside a declaration:
    /// a specifier-qualifier list admits neither (C17 6.7.2.1p1, 6.7.7p1).
    /// Reported in gcc's words and skipped, so the list recovers as if it
    /// had not been written. Answers whether it did.
    fn reject_outside_declaration(&mut self, ctx: SpecContext) -> bool {
        if ctx == SpecContext::Declaration {
            return false;
        }
        let spelled = self.current_ident().map_or("", |id| self.idents.get(id));
        diag::error_args(
            self.current_pos(),
            "expected specifier-qualifier-list before '{0}'",
            &[spelled],
        );
        self.advance();
        true
    }

    /// C17 6.7.6.3p2: the only storage-class specifier a parameter
    /// declaration may carry is `register`. `spec_modifiers` are the
    /// specifiers' modifiers, and `name` the parameter's, `EMPTY` for an
    /// abstract one. A function specifier is gcc's warning, not an error.
    pub(super) fn check_parameter_specifiers(
        &self,
        spec_modifiers: TypeModifiers,
        name: StringId,
        pos: Position,
    ) {
        const NOT_REGISTER: TypeModifiers = TypeModifiers::STATIC
            .union(TypeModifiers::EXTERN)
            .union(TypeModifiers::AUTO)
            .union(TypeModifiers::TYPEDEF)
            .union(TypeModifiers::THREAD_LOCAL);
        let spelled = self.str(name);
        if spec_modifiers.intersects(NOT_REGISTER) {
            if spelled.is_empty() {
                diag::error(pos, "storage class specified for unnamed parameter");
            } else {
                diag::error_args(
                    pos,
                    "storage class specified for parameter '{0}'",
                    &[spelled],
                );
            }
        }
        for (bit, specifier) in [
            (TypeModifiers::INLINE, "inline"),
            (TypeModifiers::NORETURN, "_Noreturn"),
        ] {
            if !spec_modifiers.contains(bit) {
                continue;
            }
            if spelled.is_empty() {
                diag::warning_args(pos, "unnamed parameter declared '{0}'", &[specifier]);
            } else {
                diag::warning_args(pos, "parameter '{0}' declared '{1}'", &[spelled, specifier]);
            }
        }
    }
}

impl Parser<'_> {
    /// Evaluate the extents a declaration's specifiers introduced once, for
    /// the whole declaration, and answer the expressions each declarator's
    /// levels should name.
    ///
    /// The declaration specifiers are evaluated once per declaration, however
    /// many declarators share them: in `typeof(int[n++]) a, b;` gcc increments
    /// `n` once and gives `a` and `b` one extent. Copying `n++` into every
    /// declarator evaluated it once each, so `b` came out a different size
    /// from `a`. A variably modified typedef name already has this shape --
    /// its extents were evaluated at the typedef, and a use names them
    /// ([`ExprKind::VmTypedefExtent`]) -- so `typeof`'s size expressions are
    /// given the same one: an unnamed typedef declared ahead of the
    /// declarators evaluates them, and every declarator names its extents.
    ///
    /// Extents that already name evaluated ones -- a typedef name, `typeof`
    /// of one, or `typeof` of a variably modified object -- are answered as
    /// they are. The last carries its operand for evaluation, and each
    /// declarator then evaluates it: gcc steps `i` twice in
    /// `typeof(p[i++]) a, b;`.
    pub(super) fn bind_specifier_extents(
        &mut self,
        dims: Vec<Expr>,
        spec_type: TypeId,
        pos: Position,
        declarators: &mut Vec<InitDeclarator>,
    ) -> Vec<Expr> {
        if dims.iter().all(names_recorded_extent) {
            return dims;
        }
        let typ = self.types.without_decl_specifiers(spec_type);
        let mut sym = Symbol::typedef(StringId::EMPTY, typ, self.symbols.depth());
        // Unnamed, so never a redefinition of another one in this scope.
        sym.defined = false;
        let Ok(id) = self.symbols.declare(sym) else {
            return dims;
        };
        self.vm_typedefs.insert(id, dims.len() as u32);
        declarators.push(InitDeclarator {
            symbol_attrs: Default::default(),
            fn_effect: MemEffect::Unknown,
            cleanup: None,
            symbol: id,
            typ,
            storage_class: TypeModifiers::TYPEDEF,
            init: None,
            vla_sizes: dims,
            explicit_align: None,
            pos,
        });
        self.vm_typedef_extents(id).unwrap_or_default()
    }

    /// The extents `typeof(expr)` names when `expr`'s type is variably
    /// modified: an array's variable levels, or a pointer's pointee's -- the
    /// levels a declarator's size expressions describe -- each read from what
    /// the declaration of the object `expr` is rooted in recorded
    /// ([`ExprKind::VmObjectExtent`]); see [`super::ast::vm_extent_count`].
    ///
    /// The operand is evaluated only when its type is variably modified (C23
    /// 6.7.3.6), which the first extent carries out; a bare identifier has
    /// nothing to evaluate.
    fn typeof_object_extents(&self, expr: &Expr) -> Vec<Expr> {
        let levels = super::ast::vm_extent_count(self.types, self.symbols, expr);
        if levels == 0 {
            return Vec::new();
        }
        let ulong = Some(self.types.ulong_id);
        let mut dims: Vec<Expr> = (0..levels as u32)
            .map(|level| Expr {
                kind: ExprKind::VmObjectExtent(Box::new(expr.clone()), level),
                typ: ulong,
                pos: expr.pos,
                bitfield_bits: None,
            })
            .collect();
        if !matches!(expr.kind, ExprKind::Ident(_)) {
            let first = dims.remove(0);
            dims.insert(
                0,
                Expr {
                    kind: ExprKind::Comma(vec![expr.clone(), first]),
                    typ: ulong,
                    pos: expr.pos,
                    bitfield_bits: None,
                },
            );
        }
        dims
    }

    /// The extents of the variably modified typedef `sym`, as expressions that
    /// name what its declaration already evaluated.
    ///
    /// None for an ordinary typedef, which is nearly all of them.
    pub(crate) fn vm_typedef_extents(&self, sym: SymbolId) -> Option<Vec<Expr>> {
        let levels = *self.vm_typedefs.get(&sym)?;
        Some(
            (0..levels)
                .map(|level| Expr {
                    kind: ExprKind::VmTypedefExtent(sym, level),
                    typ: Some(self.types.ulong_id),
                    pos: self.current_pos(),
                    bitfield_bits: None,
                })
                .collect(),
        )
    }

    /// `_Alignas(type-name)` or `_Alignas(constant-expression)`, folded into
    /// the pending alignment slot.
    fn parse_alignas_specifier(&mut self) -> ParseResult<()> {
        let alignas_pos = self.current_pos();
        self.advance();
        self.expect_special(b'(')?;
        let align = if let Some(type_id) = self.try_parse_type_name() {
            self.types.alignment(type_id) as i128
        } else {
            let expr = self.parse_expression()?;
            match self.eval_const_expr(&expr) {
                Some(align) => align,
                None => {
                    diag::error(
                        alignas_pos,
                        &gettext("requested alignment is not an integer constant"),
                    );
                    0
                }
            }
        };
        self.expect_special(b')')?;

        // C11 6.7.5p6: `_Alignas(0)` has no effect.
        if align == 0 {
            return Ok(());
        }
        let align = match u32::try_from(align) {
            Ok(align) if align.is_power_of_two() => align,
            _ => {
                return Err(ParseError::new(
                    format!("_Alignas({}) must be a power of 2", align),
                    alignas_pos,
                ))
            }
        };
        // C17 6.7.5p4: an alignment the implementation supports -- the same
        // ceiling as the `aligned` attribute's.
        if i128::from(align) > super::attribute::MAX_ATTR_ALIGN {
            diag::error_args(
                alignas_pos,
                "requested alignment '{0}' exceeds object file maximum {1}",
                &[
                    &align.to_string(),
                    &super::attribute::MAX_ATTR_ALIGN.to_string(),
                ],
            );
            return Ok(());
        }
        // Several may appear; the strictest wins (C11 6.7.5).
        self.pending_alignas = Some(match self.pending_alignas {
            Some(existing) => existing.max(align),
            None => align,
        });
        // Record the spelling: 6.7.5p5 constrains the keyword, not the
        // `aligned` attribute that shares the slot.
        self.pending_alignas_kw.get_or_insert(alignas_pos);
        Ok(())
    }

    /// Diagnose a redeclaration whose type conflicts with the one already in
    /// scope (C17 6.7p4: all declarations of the same object or function shall
    /// specify compatible types). Two guards keep legal code legal: only a
    /// repeat *in the same scope* is a redeclaration, so shadowing survives,
    /// and the declaration-only modifiers come off first, so `extern int x;`
    /// followed by `int x = 5;` is one type, not two.
    ///
    /// `form` says whether the new one is a function definition with an
    /// identifier list, which gcc holds to a looser rule.
    pub(super) fn check_redeclaration(
        &mut self,
        name: StringId,
        new_type: TypeId,
        pos: Position,
        form: Redeclared,
    ) {
        let Some(existing_id) = self.symbols.lookup_id(name, Namespace::Ordinary) else {
            return;
        };
        let existing = self.symbols.get(existing_id);
        // An enumerator shares the ordinary name space with a variable, so
        // declaring one over the other is 6.7p3 and gcc's own wording says
        // so. The reverse direction -- an enumerator over a variable -- is
        // caught where enumerators are bound. Without this arm `enum A { Z };
        // int Z;` compiled and the two names collided silently.
        if existing.kind == SymbolKind::EnumConstant && existing.scope_depth == self.symbols.depth()
        {
            let spelled = self.idents.get_opt(name).unwrap_or("").to_string();
            diag::error_args(
                pos,
                "'{0}' redeclared as a different kind of symbol",
                &[&spelled],
            );
            return;
        }
        // A typedef name is in the ordinary name space too (C17 6.2.3), so an
        // object or function of the same name in the same scope is a second
        // kind of symbol. `typedef int T; T T;` compiled: the specifier loop
        // takes the first `T` as the type and the second as the declarator,
        // which is right, and nothing then noticed the collision.
        if existing.is_typedef() && existing.scope_depth == self.symbols.depth() {
            let spelled = self.idents.get_opt(name).unwrap_or("").to_string();
            diag::error_args(
                pos,
                "'{0}' redeclared as a different kind of symbol",
                &[&spelled],
            );
            return;
        }
        // Typedefs have their own check; a tag is a different namespace.
        if !matches!(
            existing.kind,
            SymbolKind::Variable | SymbolKind::Function | SymbolKind::Parameter
        ) {
            return;
        }
        if existing.scope_depth != self.symbols.depth() {
            return;
        }
        let old_kind = existing.kind;
        let old_type = existing.typ;
        let old_type = self.types.without_decl_specifiers(old_type);
        let new_type = self.types.without_decl_specifiers(new_type);
        if self.redeclaration_compatible(old_type, new_type, form) {
            return;
        }

        let spelled = self.idents.get_opt(name).unwrap_or("").to_string();
        // gcc distinguishes these, and the distinction is the useful part: a
        // function becoming an object is a different mistake from a function
        // changing its signature.
        let old_is_func =
            self.types.kind(old_type) == TypeKind::Function || old_kind == SymbolKind::Function;
        let new_is_func = self.types.kind(new_type) == TypeKind::Function;
        if old_is_func != new_is_func {
            diag::error_args(
                pos,
                "'{0}' redeclared as a different kind of symbol",
                &[&spelled],
            );
            return;
        }
        diag::error_args(
            pos,
            "conflicting types for '{0}': '{1}' then '{2}'",
            &[
                &spelled,
                &self.types.format_type(old_type, Some(self.idents)),
                &self.types.format_type(new_type, Some(self.idents)),
            ],
        );
    }

    /// Are these two declarations of one name compatible (C17 6.2.7)?
    ///
    /// Ordinary type compatibility -- which already pairs a function type
    /// without a prototype against one with a prototype, so `int f(); int
    /// f(int);` is a composite type and `int f(); int f(char);` a conflict --
    /// and a zero extent as an unknown one.
    ///
    /// A definition with an identifier list after a prototype is held only
    /// to its return type. 6.2.7p3 would compare each prototype parameter
    /// with the promoted type of its identifier, but gcc accepts `int f(char);
    /// int f(c) char c; { ... }` and objects only under `-pedantic`.
    pub(super) fn redeclaration_compatible(
        &self,
        old: TypeId,
        new: TypeId,
        form: Redeclared,
    ) -> bool {
        if self.types.types_compatible(old, new) {
            return true;
        }
        let (o, n) = (self.types.get(old), self.types.get(new));

        // 6.2.7p3: an array of unknown size is compatible with a sized array
        // of the same element type -- the composite takes the known size. That
        // is how `extern int a[]; int a[3];` completes a declaration.
        //
        // "Unknown" is spelled two ways here, absent and zero, because the
        // declarator paths do not agree on which; that also means a genuine
        // `int a[0]` (the GNU zero-length array) is accepted against any size.
        // Under-diagnosing that is the safe direction.
        if o.kind == TypeKind::Array && n.kind == TypeKind::Array {
            let size_unknown = |sz: Option<usize>| matches!(sz, None | Some(0));
            if size_unknown(o.array_size) || size_unknown(n.array_size) {
                return match (o.base, n.base) {
                    (Some(a), Some(b)) => self.types.types_compatible(a, b),
                    _ => false,
                };
            }
        }

        if form != Redeclared::IdentifierListDefinition
            || o.kind != TypeKind::Function
            || o.params.is_none()
        {
            return false;
        }
        match (o.base, n.base) {
            (Some(a), Some(b)) => self.types.types_compatible(a, b),
            _ => false,
        }
    }

    /// Diagnose redefining a typedef name with an incompatible type.
    ///
    /// C11/C17 6.7p3 legalized redefining a typedef, but only to a *compatible*
    /// type. Every `declare()` caller discards `SymbolError::Redefinition` and
    /// reuses the existing symbol, so an incompatible redefinition silently
    /// kept the first type — strictly worse than C89, where any redefinition
    /// was flagged.
    pub(super) fn check_typedef_redefinition(
        &mut self,
        name: StringId,
        new_type: TypeId,
        pos: Position,
    ) {
        let Some(existing_id) = self.symbols.lookup_id(name, Namespace::Ordinary) else {
            return;
        };
        let existing = self.symbols.get(existing_id);
        // The reverse of the typedef arm in `check_redeclaration`: an object,
        // function or enumerator already holds the name in this scope.
        if !existing.is_typedef() {
            if existing.scope_depth == self.symbols.depth()
                && matches!(
                    existing.kind,
                    SymbolKind::Variable
                        | SymbolKind::Function
                        | SymbolKind::Parameter
                        | SymbolKind::EnumConstant
                )
            {
                let spelled = self.idents.get_opt(name).unwrap_or("").to_string();
                diag::error_args(
                    pos,
                    "'{0}' redeclared as a different kind of symbol",
                    &[&spelled],
                );
            }
            return;
        }
        // 6.7p3 governs a *repeat* declaration, which means the same scope.
        // `lookup_id` answers with the innermost visible binding from any
        // enclosing scope, so without this a block-scope `typedef double T;`
        // shadowing a file-scope `typedef int T;` — perfectly legal, and what
        // shadowing is for — was reported as an incompatible redefinition and
        // failed the translation unit.
        if existing.scope_depth != self.symbols.depth() {
            return;
        }
        let old_type = existing.typ;
        // Compare the types the two names denote, not how they were spelled.
        // A typedef's recorded type may still carry the TYPEDEF bit and the
        // storage class from its declaration, and glibc reaches most of these
        // names through a second typedef (`typedef __int16_t int16_t;`), so
        // comparing raw modifiers reports two identical `short`s as different.
        let old_type = self.types.without_decl_specifiers(old_type);
        let new_type = self.types.without_decl_specifiers(new_type);
        if self.types.types_same(old_type, new_type) {
            return;
        }
        let spelled = self.idents.get_opt(name).unwrap_or("").to_string();
        diag::error_args(
            pos,
            "typedef '{0}' redefined with a different type ('{1}' then '{2}')",
            &[
                &spelled.to_string(),
                &self.types.format_type(old_type, Some(self.idents)),
                &self.types.format_type(new_type, Some(self.idents)),
            ],
        );
    }

    /// Diagnose a declaration that named no type (C99 removed implicit int;
    /// 6.7.2p2 makes "at least one type specifier shall be given" a
    /// constraint). `explicit` is [`DeclSpecifiers::explicit`]; call at a site
    /// where a type is genuinely required.
    pub(super) fn check_implicit_int(&self, explicit: bool, pos: Position) {
        if !explicit {
            // `-fpermissive` downgrades this to a warning. The recovery below
            // is the same either way -- the type defaults to `int` -- so the
            // flag changes only whether the translation unit is rejected.
            let msg = gettext("type specifier missing; implicit 'int' was removed in C99");
            if diag::permissive() {
                diag::warning(pos, &msg);
            } else {
                diag::error(pos, &msg);
            }
            // The caller keeps the defaulted `int` and carries on: the
            // declarator that follows is usually well-formed, and one
            // diagnostic per declaration reads better than a cascade.
        }
    }

    /// The specifier a declaration led with, for the diagnostic below.
    ///
    /// Ordered so the one a reader would blame comes first: a storage class
    /// is more surprising in an empty declaration than a bare qualifier.
    fn leading_specifier_name(modifiers: TypeModifiers) -> Option<&'static str> {
        const SPELLINGS: &[(TypeModifiers, &str)] = &[
            (TypeModifiers::TYPEDEF, "typedef"),
            (TypeModifiers::EXTERN, "extern"),
            (TypeModifiers::STATIC, "static"),
            (TypeModifiers::REGISTER, "register"),
            (TypeModifiers::AUTO, "auto"),
            (TypeModifiers::THREAD_LOCAL, "_Thread_local"),
            (TypeModifiers::INLINE, "inline"),
            (TypeModifiers::CONST, "const"),
            (TypeModifiers::VOLATILE, "volatile"),
        ];
        SPELLINGS
            .iter()
            .find(|(m, _)| modifiers.contains(*m))
            .map(|(_, name)| *name)
    }

    /// Diagnose a declaration that stops at `;` having declared nothing.
    ///
    /// C17 6.7p2 requires a declaration to declare a declarator, a tag, or the
    /// members of an enumeration. `struct S;` and `enum E { A };` declare a
    /// tag and are the reason this arm exists at all; `int;`, `static;` and
    /// `int register;` declare nothing whatsoever and were accepted silently.
    ///
    /// Reported rather than warned: the constraint is violated, and a
    /// declaration that declares nothing is always a typo or a stray token.
    /// (gcc errors on `register`/`inline` here and warns on the rest; both are
    /// conforming, since 6.7p2 asks only for a diagnostic.)
    pub(super) fn check_declares_something(&mut self, pos: Position, base_type: &Type) {
        // A tag -- declared or defined -- is the thing this declaration form
        // exists to express, so it always counts. A structure or union with
        // no tag declares nothing it could be named by again (an enumeration
        // still declares its constants), which gcc warns about.
        if matches!(
            base_type.kind,
            TypeKind::Struct | TypeKind::Union | TypeKind::Enum
        ) {
            let untagged = base_type
                .composite
                .as_ref()
                .is_some_and(|c| c.tag.is_none());
            if untagged && base_type.kind != TypeKind::Enum {
                diag::warning(
                    pos,
                    &gettext("unnamed struct/union that defines no instances"),
                );
            }
            return;
        }

        match Self::leading_specifier_name(base_type.modifiers) {
            Some(spec) => diag::error_args(pos, "'{0}' in empty declaration", &[spec]),
            None => diag::error(pos, &gettext("declaration declares nothing")),
        }
    }
}

/// Whether `dim` names an extent some declaration already recorded -- a
/// typedef's, or an object's, possibly after evaluating `typeof`'s operand --
/// rather than a size expression still to evaluate.
fn names_recorded_extent(dim: &Expr) -> bool {
    match &dim.kind {
        ExprKind::VmTypedefExtent(..) | ExprKind::VmObjectExtent(..) => true,
        ExprKind::Comma(items) => items.last().is_some_and(names_recorded_extent),
        _ => false,
    }
}
