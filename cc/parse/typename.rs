//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Type names and specifier-qualifier lists (C17 6.7.7), as they appear in
// casts, sizeof, _Alignof, compound literals and _Generic
//

use super::ast::Expr;
use super::declaration::SpecifierTally;
use super::parser::{DeclaratorName, ParseError, ParseResult, Parser};
use crate::strings::StringId;
use crate::symbol::Symbol;
use crate::token::lexer::TokenType;
use crate::types::{Type, TypeId, TypeKind, TypeModifiers};

impl Parser<'_> {
    /// Check if identifier is a type-starting keyword (for cast/sizeof
    /// disambiguation).
    ///
    /// Any type *qualifier* can begin a type name -- C17 6.7.7 makes a
    /// type-name a specifier-qualifier-list, in either order -- and a
    /// qualifier at the head of a parenthesised construct cannot be anything
    /// else, so admitting them here does not make a cast ambiguous with an
    /// expression. Only `const`, `volatile` and `_Atomic` carried
    /// `TYPE_KEYWORD` in the table, so `sizeof(__const int)` and
    /// `(__const int *)p` were rejected while `sizeof(const int)` was fine.
    pub(crate) fn is_type_keyword(id: crate::strings::StringId) -> bool {
        crate::kw::has_tag(id, crate::kw::TYPE_KEYWORD)
            || crate::kw::has_tag(id, crate::kw::QUALIFIER)
    }

    /// Consume type qualifiers (const, volatile, restrict)
    /// Used for qualifiers after '*' in pointers or after struct/union/enum types
    /// Returns the modifiers that were consumed
    /// Consume trailing declaration specifiers after a struct, union or enum
    /// specifier: qualifiers *and* storage classes.
    ///
    /// C17 6.7p1 makes declaration-specifiers a sequence of storage-class
    /// specifiers, type specifiers, qualifiers and the rest **in any order**,
    /// so `struct { int a; } static g = {1};` is well-formed. Only the
    /// qualifiers were consumed here, and the storage class was then read as
    /// the declarator's name -- the declaration failed with
    /// "expected ';', found identifier 'g'".
    pub(crate) fn consume_trailing_specifiers(
        &mut self,
        tally: &mut SpecifierTally,
    ) -> ParseResult<TypeModifiers> {
        let mut mods = self.consume_type_qualifiers();
        loop {
            if self.peek() != TokenType::Ident {
                break;
            }
            let Some(name_id) = self.get_ident_id(self.current()) else {
                break;
            };
            // Everything C17 6.7p1 lets follow a completed type specifier. The
            // list was five storage classes long, which made
            // `struct S { int x; } extern __thread a;` stop at `__thread` and
            // read it as the declarator's name -- a silent misparse, not a
            // diagnostic. It also bypassed the tally, so two storage classes
            // here went unreported where two before the `struct` did not.
            let m = match name_id {
                crate::kw::STATIC => {
                    tally.note_storage_class("static", self.current_pos());
                    TypeModifiers::STATIC
                }
                crate::kw::EXTERN => {
                    tally.note_storage_class("extern", self.current_pos());
                    TypeModifiers::EXTERN
                }
                crate::kw::REGISTER => {
                    tally.note_storage_class("register", self.current_pos());
                    TypeModifiers::REGISTER
                }
                crate::kw::AUTO => {
                    tally.note_storage_class("auto", self.current_pos());
                    TypeModifiers::AUTO
                }
                crate::kw::TYPEDEF => {
                    tally.note_storage_class("typedef", self.current_pos());
                    TypeModifiers::TYPEDEF
                }
                crate::kw::THREAD_LOCAL | crate::kw::GNU_THREAD => TypeModifiers::THREAD_LOCAL,
                crate::kw::INLINE | crate::kw::GNU_INLINE | crate::kw::GNU_INLINE2 => {
                    TypeModifiers::INLINE
                }
                crate::kw::NORETURN | crate::kw::GNU_NORETURN => TypeModifiers::NORETURN,
                crate::kw::ALIGNAS => {
                    self.parse_alignas_specifier()?;
                    mods |= self.consume_type_qualifiers();
                    continue;
                }
                _ => break,
            };
            self.advance();
            mods |= m;
            // A storage class may itself be followed by more qualifiers.
            mods |= self.consume_type_qualifiers();
        }
        tally.check();
        Ok(mods)
    }

    pub(crate) fn consume_type_qualifiers(&mut self) -> TypeModifiers {
        let mut mods = TypeModifiers::empty();
        while self.peek() == TokenType::Ident {
            let name_id = match self.get_ident_id(self.current()) {
                Some(id) => id,
                None => break,
            };
            match name_id {
                crate::kw::ATOMIC => {
                    self.advance();
                    mods |= TypeModifiers::ATOMIC;
                }
                // Every spelling, from the one shared answer.
                _ if let Some(m) = super::cv_qualifier_modifier(name_id) => {
                    self.advance();
                    mods |= m;
                }
                _ if super::is_nullability_qualifier(name_id) => {
                    self.advance();
                }
                _ => break,
            }
        }
        mods
    }

    /// Apply the qualifiers written after a tag reference, plus `leading` --
    /// the ones already collected before it. Both halves matter: `const` and
    /// `volatile` the back end does not act on, but `_Atomic` decides both the
    /// access and the alignment.
    fn apply_trailing_qualifiers_with(
        &mut self,
        base_type: TypeId,
        leading: TypeModifiers,
    ) -> TypeId {
        let mods = leading | self.consume_type_qualifiers();
        if mods.is_empty() {
            base_type
        } else {
            let mut qualified_type = self.types.get(base_type).clone();
            qualified_type.modifiers |= mods;
            self.types.intern(qualified_type)
        }
    }

    /// Parse a type name (required, returns error if not a type)
    pub(super) fn parse_type_name(&mut self) -> ParseResult<TypeId> {
        self.try_parse_type_name()
            .ok_or_else(|| ParseError::new("expected type name".to_string(), self.current_pos()))
    }

    /// Parse a type-name: a specifier-qualifier list followed by an optional
    /// abstract declarator (C17 6.7.7). Speculative -- the caller uses it to
    /// tell `(type)expr` from `(expr)`, so a failure rewinds and answers
    /// `None` rather than reporting anything. The abstract declarator goes
    /// through `parse_declarator`, the same parser every other declarator
    /// uses: an abstract declarator is just a declarator whose identifier is
    /// absent, which it already represents as `StringId::EMPTY`.
    pub(crate) fn try_parse_type_name(&mut self) -> Option<TypeId> {
        self.try_parse_type_name_vm().map(|(typ, _dims)| typ)
    }

    /// A type-name together with the size expressions of its variably-modified
    /// array levels, outermost-first.
    ///
    /// Only `sizeof` needs the expressions. C17 6.5.3.4p2 evaluates the
    /// operand of `sizeof` when the type is a variable length array, and the
    /// size cannot be recovered afterwards: `int[n]`, `int[m]` and `int[]` all
    /// intern to one `TypeId`. Every other caller wants the type alone and
    /// uses [`Self::try_parse_type_name`].
    pub(crate) fn try_parse_type_name_vm(&mut self) -> Option<(TypeId, Vec<Expr>)> {
        let saved_pos = self.pos;
        let (base, spec_dims) = self.try_parse_specifier_qualifier_list()?;

        match self.parse_declarator(base, DeclaratorName::Optional) {
            // An abstract declarator names nothing. A name here means this was
            // never a type-name, so let the caller try it as an expression.
            Ok((name, typ, vla, _params)) if name == StringId::EMPTY => {
                // Declarator levels are outermost, specifier levels innermost:
                // in `typeof(int[n])[3]` the constant 3 is the outer extent and
                // `n` the inner one. Concatenated the other way round,
                // `int[3][n]` and `int[n][3]` would come out the same size --
                // right for one shape and wrong for another.
                let mut dims = vla;
                dims.extend(spec_dims);
                Some((typ, dims))
            }
            // A *named* declarator means this was never a type-name -- `(x)`
            // in a cast position, say. Rewind and let the caller read it as
            // an expression.
            Ok(_) => {
                self.pos = saved_pos;
                None
            }
            // The specifier-qualifier list parsed, so this *is* a type-name
            // and the declarator after it is simply invalid. Report the
            // declarator's own error instead of rewinding: rewinding discards
            // it and lets the caller re-read the tokens as an expression,
            // which diagnoses neither problem -- `sizeof(char[-1])` would draw
            // "undeclared identifier 'char'" where gcc says "size of unnamed
            // array is negative".
            Err(e) => {
                crate::diag::error(e.pos, &e.message);
                self.resync_to_enclosing_paren();
                Some((self.types.int_id, Vec::new()))
            }
        }
    }

    /// After a committed type-name error, skip to the `)` that closes the
    /// construct the caller opened, so one bad declarator draws one
    /// diagnostic rather than cascading into "expected ')'".
    ///
    /// Every caller of `try_parse_type_name_vm` is positioned just inside a
    /// `(` -- `sizeof(`, `_Alignof(`, `_Atomic(`, `typeof(`, a cast, a
    /// compound literal -- so the token that ends the construct is the first
    /// `)` not nested inside a bracket or paren opened after this point.
    ///
    /// The cursor usually sits *inside* an unclosed `[` when this is called,
    /// the array bound being where the declarator failed, so a closer with no
    /// opener is one of the caller's and must not drive the depth negative.
    fn resync_to_enclosing_paren(&mut self) {
        let mut depth = 0i32;
        while !self.is_eof() {
            if self.is_special(b'(') || self.is_special(b'[') {
                depth += 1;
            } else if self.is_special(b']') {
                depth = (depth - 1).max(0);
            } else if self.is_special(b')') {
                if depth == 0 {
                    return;
                }
                depth -= 1;
            }
            self.advance();
        }
    }

    /// The specifier-qualifier list of a type-name, without its declarator.
    ///
    /// Returns the extent expressions of any variably modified array level the
    /// list itself introduced, which only `typeof(type-name)` can do. They
    /// cannot ride on the `TypeId`: `int[]`, `int[n]` and `int[m]` all intern
    /// to one type, so dropping them here is what made `sizeof(typeof(int[n]))`
    /// answer 0.
    fn try_parse_specifier_qualifier_list(&mut self) -> Option<(TypeId, Vec<Expr>)> {
        if self.peek() != TokenType::Ident {
            return None;
        }

        // Check if this looks like a type name (keyword or typedef)
        let name_id = self.get_ident_id(self.current())?;
        if !Self::is_type_keyword(name_id) && self.symbols.lookup_typedef(name_id).is_none() {
            // Not a type keyword and not a typedef
            return None;
        }

        // Parse type specifiers (similar to parse_type_specifier)
        let mut modifiers = TypeModifiers::empty();
        let mut base_kind: Option<TypeKind> = None;
        let mut parsed_something = false;
        // Track typedef type separately - we continue parsing after a typedef
        // to collect trailing qualifiers like "z_word_t const"
        let mut typedef_base: Option<TypeId> = None;
        // The extents of a variably modified typedef, when the name resolves
        // to one. Kept beside the type because they cannot live in it.
        let mut typedef_dims: Option<Vec<Expr>> = None;

        loop {
            if self.peek() != TokenType::Ident {
                break;
            }

            let name_id = match self.get_ident_id(self.current()) {
                Some(id) => id,
                None => break,
            };
            match name_id {
                // Every spelling of `const`, `volatile` and `restrict`, from
                // the one shared answer. `_Atomic` has its own arm below
                // because it is also a type specifier.
                _ if let Some(m) = super::cv_qualifier_modifier(name_id) => {
                    self.advance();
                    modifiers |= m;
                    parsed_something = true;
                }
                crate::kw::SIGNED => {
                    self.advance();
                    modifiers |= TypeModifiers::SIGNED;
                    parsed_something = true;
                }
                crate::kw::UNSIGNED => {
                    self.advance();
                    modifiers |= TypeModifiers::UNSIGNED;
                    parsed_something = true;
                }
                crate::kw::COMPLEX | crate::kw::GNU_COMPLEX | crate::kw::GNU_COMPLEX2 => {
                    self.advance();
                    modifiers |= TypeModifiers::COMPLEX;
                    parsed_something = true;
                }
                crate::kw::ATOMIC => {
                    self.advance();
                    // _Atomic can be:
                    // 1. Type specifier: _Atomic(type-name)
                    // 2. Type qualifier: _Atomic (without parens)
                    if self.is_special(b'(') {
                        // Type specifier form: _Atomic(type-name)
                        self.advance(); // consume '('
                        let inner_type = self.try_parse_type_name()?;
                        if !self.is_special(b')') {
                            return None;
                        }
                        self.advance(); // consume ')'
                        let inner = self.types.get(inner_type).clone();
                        let result = Type {
                            modifiers: modifiers | inner.modifiers | TypeModifiers::ATOMIC,
                            ..inner
                        };
                        let result_id = self.types.intern(result);
                        return Some((result_id, Vec::new()));
                    } else {
                        // Qualifier form: just _Atomic
                        modifiers |= TypeModifiers::ATOMIC;
                    }
                    parsed_something = true;
                }
                crate::kw::SHORT => {
                    self.advance();
                    modifiers |= TypeModifiers::SHORT;
                    // A specifier list is a set (C17 6.7.2p2), so `int short`
                    // is `short int`. This tally is a second copy of the one
                    // in `parser.rs`; both had to learn the same thing.
                    if base_kind.is_none() || base_kind == Some(TypeKind::Int) {
                        base_kind = Some(TypeKind::Short);
                    }
                    parsed_something = true;
                }
                crate::kw::LONG => {
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
                    parsed_something = true;
                }
                crate::kw::VOID => {
                    self.advance();
                    base_kind = Some(TypeKind::Void);
                    parsed_something = true;
                }
                crate::kw::CHAR => {
                    self.advance();
                    base_kind = Some(TypeKind::Char);
                    parsed_something = true;
                }
                crate::kw::INT => {
                    self.advance();
                    if base_kind.is_none()
                        || !matches!(
                            base_kind,
                            Some(TypeKind::Short) | Some(TypeKind::Long) | Some(TypeKind::LongLong)
                        )
                    {
                        base_kind = Some(TypeKind::Int);
                    }
                    parsed_something = true;
                }
                crate::kw::FLOAT => {
                    self.advance();
                    base_kind = Some(TypeKind::Float);
                    parsed_something = true;
                }
                crate::kw::DOUBLE => {
                    self.advance();
                    // Handle long double
                    if modifiers.contains(TypeModifiers::LONG) {
                        base_kind = Some(TypeKind::LongDouble);
                    } else {
                        base_kind = Some(TypeKind::Double);
                    }
                    parsed_something = true;
                }
                crate::kw::FLOAT16 => {
                    self.advance();
                    base_kind = Some(TypeKind::Float16);
                    parsed_something = true;
                }
                crate::kw::FLOAT32 => {
                    // _Float32 is an alias for float (TS 18661-3 / C23)
                    self.advance();
                    base_kind = Some(TypeKind::Float);
                    parsed_something = true;
                }
                crate::kw::FLOAT64 => {
                    // _Float64 is an alias for double (TS 18661-3 / C23)
                    self.advance();
                    base_kind = Some(TypeKind::Double);
                    parsed_something = true;
                }
                crate::kw::FLOAT128 | crate::kw::FLOAT128_ALIAS => {
                    // IEEE binary128; see the declaration parser's arm.
                    if !self.types.has_float128() {
                        break;
                    }
                    self.advance();
                    base_kind = Some(TypeKind::Float128);
                    parsed_something = true;
                }
                crate::kw::BOOL => {
                    self.advance();
                    base_kind = Some(TypeKind::Bool);
                    parsed_something = true;
                }
                crate::kw::INT128 => {
                    self.advance();
                    base_kind = Some(TypeKind::Int128);
                    parsed_something = true;
                }
                crate::kw::INT128_T => {
                    self.advance();
                    base_kind = Some(TypeKind::Int128);
                    parsed_something = true;
                }
                crate::kw::UINT128_T => {
                    self.advance();
                    modifiers |= TypeModifiers::UNSIGNED;
                    base_kind = Some(TypeKind::Int128);
                    parsed_something = true;
                }
                crate::kw::BUILTIN_VA_LIST => {
                    self.advance();
                    base_kind = Some(TypeKind::VaList);
                    parsed_something = true;
                }
                crate::kw::TYPEOF | crate::kw::GNU_TYPEOF | crate::kw::GNU_TYPEOF2 => {
                    self.advance(); // consume typeof
                    if !self.is_special(b'(') {
                        return None;
                    }
                    self.advance(); // consume '('

                    // typeof can take either a type name or an expression.
                    // Try type name first, keeping any variably modified
                    // extents it found: `typeof(int[n])` is a complete type
                    // whose size is `n * sizeof(int)`, and dropping them left
                    // it indistinguishable from `int[]`.
                    if let Some((typ, dims)) = self.try_parse_type_name_vm() {
                        if !self.is_special(b')') {
                            return None;
                        }
                        self.advance(); // consume ')'
                        return Some((typ, dims));
                    }

                    // Not a type name, try expression
                    let expr = match self.parse_expression() {
                        Ok(e) => e,
                        Err(_) => return None,
                    };
                    if !self.is_special(b')') {
                        return None;
                    }
                    self.advance(); // consume ')'

                    let expr_type = expr.typ.unwrap_or(self.types.int_id);
                    return Some((expr_type, Vec::new()));
                }
                crate::kw::STRUCT => {
                    self.advance(); // consume 'struct'
                                    // For struct tag reference, look up directly in symbol table
                    if let Some(tag_name) = self.get_ident_id(self.current()) {
                        if !self.is_special(b'{') {
                            // This is a tag reference (e.g., "struct Point*")
                            self.advance(); // consume tag name
                            if let Some(existing) = self.symbols.lookup_tag(tag_name) {
                                return Some((
                                    self.apply_trailing_qualifiers_with(existing.typ, modifiers),
                                    Vec::new(),
                                ));
                            }
                            // Tag not found - create incomplete struct type and register it
                            // This ensures that when the struct is later defined, we can update
                            // this same TypeId rather than creating a new one
                            let mut incomplete = Type::incomplete_struct(tag_name);
                            incomplete.modifiers |= self.consume_type_qualifiers();
                            let result_id = self.types.intern(incomplete);
                            let sym = Symbol::tag(tag_name, result_id, self.symbols.depth());
                            let _ = self.symbols.declare(sym);
                            return Some((result_id, Vec::new()));
                        }
                    }
                    // Fall back to full struct parsing for definitions
                    self.pos -= 1;
                    if let Ok(struct_type) = self.parse_struct_or_union_specifier(false) {
                        let mut typ = struct_type;
                        typ.modifiers |= modifiers | self.consume_type_qualifiers();
                        return Some((self.types.intern(typ), Vec::new()));
                    }
                    return None;
                }
                crate::kw::UNION => {
                    self.advance(); // consume 'union'
                                    // For union tag reference, look up directly in symbol table
                    if let Some(tag_name) = self.get_ident_id(self.current()) {
                        if !self.is_special(b'{') {
                            // This is a tag reference
                            self.advance(); // consume tag name
                            if let Some(existing) = self.symbols.lookup_tag(tag_name) {
                                return Some((
                                    self.apply_trailing_qualifiers_with(existing.typ, modifiers),
                                    Vec::new(),
                                ));
                            }
                            // Tag not found - create incomplete union type and register it
                            // This ensures that when the union is later defined, we can update
                            // this same TypeId rather than creating a new one
                            let mut incomplete = Type::incomplete_union(tag_name);
                            incomplete.modifiers |= self.consume_type_qualifiers();
                            let result_id = self.types.intern(incomplete);
                            let sym = Symbol::tag(tag_name, result_id, self.symbols.depth());
                            let _ = self.symbols.declare(sym);
                            return Some((result_id, Vec::new()));
                        }
                    }
                    // Fall back to full union parsing for definitions
                    self.pos -= 1;
                    if let Ok(union_type) = self.parse_struct_or_union_specifier(true) {
                        let mut typ = union_type;
                        typ.modifiers |= modifiers | self.consume_type_qualifiers();
                        return Some((self.types.intern(typ), Vec::new()));
                    }
                    return None;
                }
                crate::kw::ENUM => {
                    if let Ok(enum_type) = self.parse_enum_specifier() {
                        let mut typ = enum_type;
                        typ.modifiers |= modifiers | self.consume_type_qualifiers();
                        return Some((self.types.intern(typ), Vec::new()));
                    }
                    return None;
                }
                _ => {
                    // Check if it's a typedef name
                    // Only consume if we haven't already seen a base type or typedef
                    if base_kind.is_none() && typedef_base.is_none() {
                        if let Some((sym, typedef_type_id)) =
                            self.symbols.lookup_typedef_symbol(name_id)
                        {
                            self.advance();
                            // A variably modified typedef's extents cannot be
                            // recovered from its `TypeId`, so `sizeof(T)` is
                            // handed the ones its declaration evaluated -- the
                            // same channel a type-name's own `[n]` levels use
                            // (C17 6.7.7p3).
                            typedef_dims = self.vm_typedef_extents(sym);
                            // Save the typedef type and continue looping to collect trailing
                            // qualifiers (e.g., "z_word_t const" where const comes after typedef)
                            typedef_base = Some(typedef_type_id);
                            parsed_something = true;
                            continue;
                        }
                    }
                    break;
                }
            }
        }

        if !parsed_something {
            return None;
        }

        // Get the base type - either from typedef or from built-in type specifiers
        let result_id = if let Some(typedef_type_id) = typedef_base {
            // Drop the TYPEDEF bit either way. It records how the name was
            // *declared*, not anything about the type, and leaving it on made a
            // typedef's type differ from the type it aliases -- so
            // `__builtin_types_compatible_p(int, MyInt)` answered 0, and
            // `_Generic` could neither match nor reject a typedef'd
            // association. The trailing-modifier path already cleared it; the
            // bare path did not.
            let typedef_type = self.types.get(typedef_type_id);
            if !modifiers.is_empty() || typedef_type.modifiers.contains(TypeModifiers::TYPEDEF) {
                let mut result = typedef_type.clone();
                result.modifiers &= !TypeModifiers::TYPEDEF;
                result.modifiers |= modifiers;
                self.types.intern(result)
            } else {
                typedef_type_id
            }
        } else {
            // If we only have modifiers like `unsigned` without a base type,
            // default to int -- except for a bare `_Complex`, which gcc reads
            // as `_Complex double`. The declaration parser has the same rule,
            // and this is the other of the two specifier tallies: without it
            // `sizeof(_Complex)` answered 8 (a `_Complex int`) where
            // `_Complex v;` was correctly 16.
            let kind = match base_kind {
                Some(k) => k,
                // Only a *bare* `_Complex`. A signedness modifier names an
                // integer base of its own -- `_Complex unsigned` is
                // `_Complex unsigned int`, eight bytes, not sixteen.
                None if modifiers.contains(TypeModifiers::COMPLEX)
                    && !modifiers.intersects(TypeModifiers::SIGNED | TypeModifiers::UNSIGNED) =>
                {
                    TypeKind::Double
                }
                None => TypeKind::Int,
            };
            let typ = Type::with_modifiers(kind, modifiers);
            self.types.intern(typ)
        };

        Some((result_id, typedef_dims.unwrap_or_default()))
    }
}
