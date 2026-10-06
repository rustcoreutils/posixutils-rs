//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// struct, union and enum specifiers, and the bit-field constraints
//

use super::attribute::AttributeList;
use super::declaration::SpecContext;
use super::parser::{DeclaratorContext, ParseError, ParseResult, ParsedDeclarator, Parser};
use crate::diag;
use crate::strings::StringId;
use crate::symbol::{Namespace, Symbol, SymbolId};
use crate::target::ByteOrder;
use crate::token::lexer::{Position, TokenType, TokenValue};
use crate::token::preprocess::StorageOrderPragma;
use crate::types::{
    CompositeType, EnumConstant, MemberAlign, StructMember, Type, TypeId, TypeKind, TypeModifiers,
};
use gettextrs::{gettext, gettext_args};

const DEFAULT_MEMBER_CAPACITY: usize = 16;
const DEFAULT_ENUM_CAPACITY: usize = 16;

impl Parser<'_> {
    /// The integer type an enumerated type is compatible with, and its size.
    ///
    /// C17 6.7.2.2p4 requires it to represent every member; the choice among
    /// the types that do is implementation-defined. gcc's choice, which this
    /// matches: the first of `int` and the 64-bit type that holds every
    /// member, unsigned when no member is negative. A `packed` enum starts
    /// the search at the 1-byte type instead, which makes it the smallest
    /// integer type that holds its members. The choice is recorded as the
    /// enum's size and `UNSIGNED` modifier, which
    /// [`crate::types::TypeTable::enum_compatible_type`] reads back.
    fn enum_underlying_type(
        &mut self,
        constants: &[EnumConstant],
        pos: Position,
        packed: bool,
    ) -> (TypeId, usize) {
        let Some(min) = constants.iter().map(|c| c.value).min() else {
            return (self.types.int_id, 4);
        };
        let max = constants.iter().map(|c| c.value).max().unwrap_or(0);

        // 6.7.2.2p4 leaves the choice to the implementation, requiring only a
        // type that represents every member. gcc's choice is unsigned whenever
        // no enumerator is negative, and it is observable -- `(enum E)-1 > 0`
        // is true there and was false here. Preferring `int` for a small
        // non-negative list also made an enum bit-field read back negative:
        // `enum E { A, B, C, D }; struct { enum E e:2; }` holding `D` gave -1
        // where gcc gives 3, because the field's signedness follows the type's.
        let unsigned = min >= 0;
        let t = &self.types;
        let candidates = if unsigned {
            [
                (t.uchar_id, 1),
                (t.ushort_id, 2),
                (t.uint_id, 4),
                (t.ulong_id, 8),
            ]
        } else {
            [
                (t.schar_id, 1),
                (t.short_id, 2),
                (t.int_id, 4),
                (t.long_id, 8),
            ]
        };
        let first = if packed { 0 } else { 2 };
        let fits = |size: usize| {
            let bits = 8 * size as u32;
            if unsigned {
                max < 1i128 << bits
            } else {
                min >= -(1i128 << (bits - 1)) && max < 1i128 << (bits - 1)
            }
        };
        if let Some(&chosen) = candidates[first..].iter().find(|&&(_, size)| fits(size)) {
            return chosen;
        }
        // Reachable: nothing clamps an enumerator to 64 bits and the folder
        // computes in `i128`, so `enum E { A = 1 << 64 };` arrives here, as
        // does a list spanning below `LONG_MIN` and above `LONG_MAX`. gcc
        // warns and carries on, folding the shift to 0 because it truncates
        // to the expression's type, which c17's folders do not do. Say what
        // is wrong rather than picking a type and truncating silently.
        diag::error(
            pos,
            &gettext("no integer type can represent all values of this enumeration"),
        );
        candidates[3]
    }

    /// The type an enumeration constant has once its enumeration is complete.
    ///
    /// C17 6.4.4.3p2 makes it `int`. gcc keeps `int` for a member that fits,
    /// except that every member of an enumeration wider than `int` takes the
    /// enumeration's type -- `enum L { L0 = -1, L1 = 0x100000000 }` makes
    /// `sizeof(L0)` 8 -- and a member that fits nowhere else takes it too. A
    /// `packed` enumeration narrower than `int` leaves its members `int`.
    fn enumerator_type(&self, value: i128, underlying: TypeId, size: usize) -> TypeId {
        if size <= 4 && i32::try_from(value).is_ok() {
            self.types.int_id
        } else {
            underlying
        }
    }

    /// Parse an enum specifier
    /// enum-specifier: 'enum' identifier? '{' enumerator-list? '}' | 'enum' identifier
    pub(crate) fn parse_enum_specifier(&mut self) -> ParseResult<Type> {
        let enum_pos = self.current_pos();
        self.advance(); // consume 'enum'

        // gcc reads an enum's attributes between `enum` and the tag and after
        // the closing brace -- not between the tag and `{`, which it rejects.
        // Of them only `packed` means anything; gcc ignores `aligned` on an
        // enum, and on a declaration that is not a definition.
        let (early_attrs, tag) = self.parse_attributed_tag()?;

        // Check for definition vs forward reference
        if self.is_special(b'{') {
            self.advance(); // consume '{'

            let mut constants = Vec::with_capacity(DEFAULT_ENUM_CAPACITY);
            let mut next_value = 0i128;
            // The enumerators bind before the enum's own width is known --
            // `enum { A = 1, B = A + 1 }` reads A while the list is still
            // being parsed -- so they are declared with a provisional type
            // and re-typed once every value has been seen.
            let mut constant_syms: Vec<SymbolId> = Vec::new();

            while !self.is_special(b'}') && !self.is_eof() {
                let name_pos = self.current_pos();
                let name = self.expect_identifier()?;

                let value = if self.is_special(b'=') {
                    self.advance();
                    let value_pos = self.current_pos();
                    let expr = self.parse_conditional_expr()?;
                    // Evaluate constant expression
                    let v = self.eval_const_expr(&expr).ok_or_else(|| {
                        ParseError::new(
                            gettext_args(
                                "enumerator value for '{0}' is not an integer constant",
                                &[self.idents.get_opt(name).unwrap_or("")],
                            ),
                            name_pos,
                        )
                    })?;
                    // C17 6.7.2.2p2 requires an enumerator to be
                    // representable as `int`, so exceeding it is a constraint
                    // violation. gcc widens the enumerated type rather than
                    // rejecting, and says so only under `-pedantic`; so does
                    // c17.
                    if v < i32::MIN as i128 || v > i32::MAX as i128 {
                        diag::pedwarn(
                            value_pos,
                            &gettext("ISO C restricts enumerator values to range of 'int'"),
                        );
                    }
                    v
                } else {
                    next_value
                };

                constants.push(EnumConstant { name, value });
                next_value = value + 1;

                // Register enum constant in symbol table (Ordinary namespace)
                let sym =
                    Symbol::enum_constant(name, value, self.types.int_id, self.symbols.depth());
                // 6.7p3: an enumerator shares the ordinary name space, so a
                // repeat -- of another enumerator or of a variable -- is a
                // constraint violation. `declare` already detects it; the
                // `Err` was being dropped, so `enum A { X }; enum B { X };`
                // compiled and the second `X` silently kept the first's value.
                match self.symbols.declare(sym) {
                    Ok(id) => constant_syms.push(id),
                    Err(_) => {
                        let spelled = self.idents.get_opt(name).unwrap_or("").to_string();
                        let existing = self.symbols.lookup(name, Namespace::Ordinary);
                        let redeclared = existing.is_some_and(|e| !e.is_enum_constant());
                        if redeclared {
                            diag::error_args(
                                enum_pos,
                                "'{0}' redeclared as a different kind of symbol",
                                &[&spelled],
                            );
                        } else {
                            diag::error_args(
                                enum_pos,
                                "redeclaration of enumerator '{0}'",
                                &[&spelled],
                            );
                        }
                    }
                }

                if self.is_special(b',') {
                    self.advance();
                    // Allow trailing comma before '}'
                    if self.is_special(b'}') {
                        break;
                    }
                } else {
                    break;
                }
            }

            // C17 6.7.2.2p1: the enumerator list is not optional. gcc and
            // clang both reject an empty one; it is not a GNU extension.
            if constants.is_empty() {
                diag::error(self.current_pos(), &gettext("empty enum is invalid"));
            }

            self.expect_special(b'}')?;
            let packed = early_attrs.has_packed() || self.parse_attributes().has_packed();

            // C17 6.7.2.2p4: the enumerated type is compatible with some
            // integer type capable of representing every member. Pick it, and
            // give the enumerators their final type.
            let (underlying, size) = self.enum_underlying_type(&constants, enum_pos, packed);
            for &sym_id in &constant_syms {
                let sym = self.symbols.get(sym_id);
                let value = sym.enum_value.unwrap_or(0);
                self.symbols.get_mut(sym_id).typ = self.enumerator_type(value, underlying, size);
            }

            let composite = CompositeType {
                tag,
                members: Vec::new(),
                enum_constants: constants,
                size,
                align: size,
                member_align: size,
                is_complete: true,
                transparent: false,
                anon_id: tag.is_none().then(|| self.types.fresh_anon_id()),
                tag_type: None,
            };

            let unsigned = self.types.is_unsigned(underlying);

            // A forward reference to the tag made an incomplete enum; the
            // definition completes that same type, as a struct's does.
            if let Some(redefined) = tag.and_then(|t| self.reused_tag(t, TypeKind::Enum)) {
                return Ok(self.types.get(redefined).clone());
            }
            if let Some(existing) = tag.and_then(|t| self.symbols.lookup_tag_in_current_scope(t)) {
                let existing_typ = existing.typ;
                let forward = self.types.get(existing_typ);
                if forward.kind == TypeKind::Enum
                    && forward.composite.as_ref().is_some_and(|c| !c.is_complete)
                {
                    self.types.complete_enum(existing_typ, composite, unsigned);
                    return Ok(self.types.get(existing_typ).clone());
                }
            }

            self.warn_tag_in_parameter_list("enum", tag);
            let mut enum_type = Type::enum_type(composite);
            // C17 6.7.2.2p4: the enumerated type shall represent every member.
            // `enum_underlying_type` picks a type that does, but the enum's own
            // type carried no signedness, so an *object* of it was loaded and
            // compared as signed even where the underlying type is unsigned:
            // `enum E { BIG = 0x80000000u }; enum E e = BIG;` read back
            // -2147483648 and `e < 0` was true, while the constant `BIG` was
            // correct all along.
            if unsigned {
                enum_type.modifiers |= TypeModifiers::UNSIGNED;
            }

            // Register tag if present, and answer with the interned type,
            // which carries the tag's identity.
            if let Some(tag_name) = tag {
                let enum_type_id = self.types.intern(enum_type);
                let sym = Symbol::tag(tag_name, enum_type_id, self.symbols.depth());
                let _ = self.symbols.declare(sym);
                return Ok(self.types.get(enum_type_id).clone());
            }

            Ok(enum_type)
        } else {
            // Forward reference - look up existing tag
            if let Some(tag_name) = tag {
                // Look up or create incomplete type
                if let Some(existing) = self.symbols.lookup_tag(tag_name) {
                    let existing = existing.typ;
                    self.check_tag_kind(tag_name, existing, TypeKind::Enum);
                    Ok(self.types.get(existing).clone())
                } else {
                    // Registered, so that the definition completes this
                    // same type rather than making another.
                    let typ_id = self.types.intern(Type::incomplete_enum(tag_name));
                    let sym = Symbol::tag(tag_name, typ_id, self.symbols.depth());
                    let _ = self.symbols.declare(sym);
                    Ok(self.types.get(typ_id).clone())
                }
            } else {
                Err(ParseError::new(
                    "expected enum definition or tag name",
                    self.current_pos(),
                ))
            }
        }
    }

    /// A tag first declared in a parameter list has prototype scope (C17
    /// 6.2.1p4), so no other declaration can ever name the same type -- a
    /// definition of the function that repeats `struct S *` has a different
    /// `struct S`. gcc warns, and so does c17.
    fn warn_tag_in_parameter_list(&self, keyword: &str, tag: Option<StringId>) {
        if self.param_list_depth == 0 {
            return;
        }
        let pos = self.current_pos();
        match tag.and_then(|t| self.idents.get_opt(t)) {
            Some(name) => diag::warning_args(
                pos,
                "'{0} {1}' declared inside parameter list will not be visible outside of this definition or declaration",
                &[keyword, name],
            ),
            None => diag::warning_args(
                pos,
                "anonymous {0} declared inside parameter list will not be visible outside of this definition or declaration",
                &[keyword],
            ),
        }
    }

    /// Every name `members` puts in a structure's name space: its named
    /// members, and those of its anonymous structure and union members.
    fn member_names(&self, members: &[StructMember]) -> Vec<StringId> {
        let mut names = Vec::new();
        for m in members {
            if m.name != StringId::EMPTY {
                names.push(m.name);
            } else if m.bit_width.is_none() {
                if let Some(c) = self.types.composite(m.typ) {
                    names.extend(self.member_names(&c.members));
                }
            }
        }
        names
    }

    /// For the definition of a tag of `kind` in this scope: the type to answer
    /// instead, when an earlier declaration of the tag here forbids it --
    /// one of another kind (C17 6.7.2.3p2), or one already defined
    /// (6.7.2.3p1). `None` when the definition may go ahead.
    fn reused_tag(&self, tag: StringId, kind: TypeKind) -> Option<TypeId> {
        let existing = self.symbols.lookup_tag_in_current_scope(tag)?.typ;
        if !self.check_tag_kind(tag, existing, kind) {
            return Some(existing);
        }
        let complete = self
            .types
            .get(existing)
            .composite
            .as_ref()
            .is_some_and(|c| c.is_complete);
        if !complete {
            return None;
        }
        let keyword = match kind {
            TypeKind::Union => "union",
            TypeKind::Enum => "enum",
            _ => "struct",
        };
        let spelled = self.idents.get_opt(tag).unwrap_or("");
        diag::error_args(
            self.current_pos(),
            "redefinition of '{0} {1}'",
            &[keyword, spelled],
        );
        Some(existing)
    }

    /// C17 6.7.2.3p2: a tag names one kind of type. `struct S` and `union
    /// S` in one scope are a constraint violation, reported here; answers
    /// whether `existing` has the `kind` asked for.
    fn check_tag_kind(&self, tag: StringId, existing: TypeId, kind: TypeKind) -> bool {
        if self.types.kind(existing) == kind {
            return true;
        }
        let spelled = self.idents.get_opt(tag).unwrap_or("");
        diag::error_args(
            self.current_pos(),
            "'{0}' defined as wrong kind of tag",
            &[spelled],
        );
        false
    }

    /// The attributes written between a tag keyword and its tag, and the
    /// tag: `struct __attribute__((packed)) S`, `enum __attribute__((packed))
    /// E`, or with no tag at all.
    fn parse_attributed_tag(&mut self) -> ParseResult<(AttributeList, Option<StringId>)> {
        let attrs = self.parse_attributes();
        let tag = if self.peek() == TokenType::Ident && !self.is_special(b'{') {
            Some(self.expect_identifier()?)
        } else {
            None
        };
        Ok((attrs, tag))
    }

    /// Parse a struct or union specifier
    /// struct-or-union-specifier: ('struct'|'union') identifier? '{' struct-declaration-list? '}'
    ///                          | ('struct'|'union') identifier
    ///
    /// `alone`: nothing precedes the specifier in its declaration, so if a
    /// `;` follows it, it is the declaration `struct S;`.
    pub(crate) fn parse_struct_or_union_specifier(
        &mut self,
        is_union: bool,
        alone: bool,
    ) -> ParseResult<Type> {
        let specifier_pos = self.current_pos();
        self.advance(); // consume 'struct' or 'union'

        // Attributes between 'struct' and the tag name
        // (e.g., struct __attribute__((packed)) tagname { ... })
        let (early_attrs, tag) = self.parse_attributed_tag()?;
        let mut is_packed = early_attrs.has_packed();
        // `transparent_union` is collected at the same positions as
        // `packed`, for the same reason: gcc accepts it at any of them.
        let mut is_transparent = early_attrs.has_transparent_union();
        // Track struct-level aligned attribute (max across all positions)
        let mut struct_align: Option<u32> = early_attrs.get_alignment();
        // `scalar_storage_order`, accepted at the same three positions; the
        // last one written decides, as in gcc.
        let mut storage_order = early_attrs.storage_order(specifier_pos);

        // Parse __attribute__ after tag name but before '{'
        let pre_attrs = self.parse_attributes();
        is_packed = is_packed || pre_attrs.has_packed();
        is_transparent = is_transparent || pre_attrs.has_transparent_union();
        if let Some(a) = pre_attrs.get_alignment() {
            struct_align = Some(struct_align.map_or(a, |e| e.max(a)));
        }
        storage_order = pre_attrs.storage_order(specifier_pos).or(storage_order);

        // Check for definition vs forward reference
        if self.is_special(b'{') {
            self.advance(); // consume '{'

            // The members are declarations of their own, parsed through the
            // same attribute slots as the declaration this specifier begins.
            // Take that declaration's aside, or `_Alignas(16) struct { char a;
            // } x;` aligns the member `a` and leaves `x` unaligned.
            let outer = self.take_pending_decl_attrs();
            let members = self.parse_member_list();
            self.restore_pending_decl_attrs(outer);
            let mut members = members?;

            self.expect_special(b'}')?;

            // Parse trailing __attribute__ (e.g., __attribute__((packed)))
            let attrs = self.parse_attributes();
            is_packed = is_packed || attrs.has_packed();
            is_transparent = is_transparent || attrs.has_transparent_union();
            if let Some(a) = attrs.get_alignment() {
                struct_align = Some(struct_align.map_or(a, |e| e.max(a)));
            }
            storage_order = attrs.storage_order(specifier_pos).or(storage_order);

            self.check_flexible_array_members(&members, is_union);
            self.apply_storage_order(&mut members, storage_order);

            // Compute layout. `__attribute__((packed))` on the struct or union
            // is `packed` on every member, which is how gcc defines it; a
            // `#pragma pack(n)` in force caps every member at n.
            if is_packed {
                for member in &mut members {
                    member.align.packed = true;
                }
            }
            let pack_cap = self.current_pack();
            let (size, mut align) = if is_union {
                self.types.compute_union_layout(&mut members, pack_cap)
            } else {
                self.types.compute_struct_layout(&mut members, pack_cap)
            };
            self.check_wide_bitfields_have_a_carrier(&members);

            // What the members alone require, kept before the attribute below
            // overwrites it. AAPCS64 derives an argument's alignment from the
            // members and ignores the type's own attribute, and a `#pragma
            // pack` cap folded in here is recorded nowhere else.
            let member_align = align;

            // Apply struct-level aligned attribute (raises alignment, never lowers)
            if let Some(sa) = struct_align {
                if sa as usize > align {
                    align = sa as usize;
                }
            }

            // Re-pad size to new alignment. Saturating: the layout answers
            // `usize::MAX` for a member list too large to describe, and
            // rounding that must not wrap it small before the check below.
            let size = size.checked_next_multiple_of(align).unwrap_or(usize::MAX);

            // The same bound `derive_array_type` enforces: a member list can
            // reach it even when no single member does.
            let max = self.types.max_object_bytes();
            if size > max {
                let keyword = if is_union { "union" } else { "struct" };
                let name = tag
                    .and_then(|t| self.idents.get_opt(t))
                    .unwrap_or("<anonymous>");
                return Err(ParseError::new(
                    format!(
                        "type '{keyword} {name}' is too large: its size exceeds \
                         the maximum object size of {max} bytes"
                    ),
                    specifier_pos,
                ));
            }

            if is_transparent && !is_union {
                self.warn_transparent_union_ignored(specifier_pos);
            }

            let composite = CompositeType {
                tag,
                members,
                enum_constants: Vec::new(),
                size,
                align,
                member_align,
                is_complete: true,
                transparent: is_transparent && is_union,
                anon_id: tag.is_none().then(|| self.types.fresh_anon_id()),
                tag_type: None,
            };

            // Check if there's an existing forward declaration that we should complete
            let kind = if is_union {
                TypeKind::Union
            } else {
                TypeKind::Struct
            };
            if let Some(redefined) = tag.and_then(|t| self.reused_tag(t, kind)) {
                return Ok(self.types.get(redefined).clone());
            }
            if let Some(tag_name) = tag {
                if let Some(existing) = self.symbols.lookup_tag_in_current_scope(tag_name) {
                    // Complete the existing forward-declared type in place
                    // This ensures all pointers to the incomplete type now see the complete type
                    let existing_typ = existing.typ;
                    let existing_type = self.types.get(existing_typ);
                    if existing_type
                        .composite
                        .as_ref()
                        .is_some_and(|c| !c.is_complete)
                    {
                        self.types.complete_struct(existing_typ, composite);
                        return Ok(self.types.get(existing_typ).clone());
                    }
                }
            }

            // No existing forward declaration - create new type
            self.warn_tag_in_parameter_list(if is_union { "union" } else { "struct" }, tag);
            let struct_type = if is_union {
                Type::union_type(composite)
            } else {
                Type::struct_type(composite)
            };

            // Register tag if present, and answer with the interned type,
            // which carries the tag's identity.
            if let Some(tag_name) = tag {
                let typ_id = self.types.intern(struct_type);
                let sym = Symbol::tag(tag_name, typ_id, self.symbols.depth());
                let _ = self.symbols.declare(sym);
                return Ok(self.types.get(typ_id).clone());
            }

            Ok(struct_type)
        } else {
            // A reference, not a definition: the type already exists, so an
            // `aligned` written on it is the declaration's -- gcc aligns `x` in
            // `struct S __attribute__((aligned(32))) x;`, and a type-name's
            // type in `_Alignof(struct S __attribute__((aligned(32))))`.
            if let Some(align) = struct_align {
                self.raise_pending_alignas(align);
            }
            if let Some(tag_name) = tag {
                // C17 6.7.2.3p7: `struct S;` by itself declares the tag in
                // this scope, a new incomplete type that hides any `struct S`
                // of an enclosing one. Every other reference names the tag
                // that is visible.
                let existing = if alone && self.is_special(b';') {
                    self.symbols.lookup_tag_in_current_scope(tag_name)
                } else {
                    self.symbols.lookup_tag(tag_name)
                };
                if let Some(existing) = existing {
                    let existing = existing.typ;
                    let kind = if is_union {
                        TypeKind::Union
                    } else {
                        TypeKind::Struct
                    };
                    self.check_tag_kind(tag_name, existing, kind);
                    Ok(self.in_written_storage_order(existing, storage_order))
                } else {
                    // Create new incomplete type and register it in symbol table
                    // This ensures that when the type is completed later, we can update
                    // this same TypeId rather than creating a new one
                    let keyword = if is_union { "union" } else { "struct" };
                    self.warn_tag_in_parameter_list(keyword, Some(tag_name));
                    let incomplete_type = if is_union {
                        Type::incomplete_union(tag_name)
                    } else {
                        Type::incomplete_struct(tag_name)
                    };
                    let typ_id = self.types.intern(incomplete_type);
                    let sym = Symbol::tag(tag_name, typ_id, self.symbols.depth());
                    let _ = self.symbols.declare(sym);
                    Ok(self.types.get(typ_id).clone())
                }
            } else {
                Err(ParseError::new(
                    "expected struct/union definition or tag name",
                    self.current_pos(),
                ))
            }
        }
    }

    /// The member declarations of a struct or union definition, from just
    /// after its `{` to just before its `}`.
    fn parse_member_list(&mut self) -> ParseResult<Vec<StructMember>> {
        let mut members = Vec::with_capacity(DEFAULT_MEMBER_CAPACITY);

        while !self.is_special(b'}') && !self.is_eof() {
            // A stray `;` -- `int a;;` or a member list that opens with one.
            // The grammar has no empty member declaration, but gcc accepts it
            // (warning only under -pedantic), and <linux/nfc.h> has one.
            if self.is_special(b';') {
                self.advance();
                continue;
            }

            // Check for _Static_assert in struct (C11 6.7.2.1p1)
            if self.is_static_assert() {
                self.parse_static_assert()?;
                continue;
            }

            // An `_Alignas` keyword is the declaration's that wrote it, and
            // the bit-field check below must not see the previous member's.
            self.pending_alignas_kw = None;

            // Parse member declaration
            let specs_start = self.pos;
            let member_specs = self.parse_declaration_specifiers(SpecContext::Member)?;
            // Whether the specifiers spell out a structure or union -- the
            // only thing that can make an anonymous member -- rather than
            // naming one through a typedef.
            let spelled_tag = self.tokens[specs_start..self.pos].iter().any(|t| {
                matches!(&t.value, TokenValue::Ident(id) if *id == crate::kw::STRUCT || *id == crate::kw::UNION)
            });
            let member_base_type = &member_specs.ty;
            let is_struct_or_union =
                matches!(member_base_type.kind, TypeKind::Struct | TypeKind::Union);
            // For struct/union types with tags, use the existing TypeId from symbol table
            // to ensure forward-declared types are properly linked
            let member_base_type_id = self.intern_type_with_tag(member_base_type);

            // Skip any __attribute__ after type specifier (before member name)
            self.skip_extensions();
            // An alignment or `packed` among the specifiers is the whole
            // declaration's, and reaches every declarator in it:
            // `__attribute__((aligned(8))) int b, c;` aligns both.
            let specifier_align = self.take_member_align();

            // C11 anonymous struct/union members: "struct { ... };" or "union { ... };"
            // These have no declarator name, just end with ';'
            if is_struct_or_union && self.is_special(b';') {
                // C17 6.7.2.1p13: only a specifier with no tag makes an
                // anonymous member. `struct T;` or `struct T { ... };` here
                // declares the tag and nothing else, as gcc reads it --
                // taking it as a member made `struct A { struct A; }`
                // contain itself.
                // A typedef name for one is not a specifier that makes an
                // anonymous member either (6.7.2.1p13 asks for a
                // struct-or-union-specifier): `T;` declares nothing, and
                // taking it as a member changed the layout gcc gives.
                let tagged = self
                    .types
                    .get(member_base_type_id)
                    .composite
                    .as_ref()
                    .is_some_and(|c| c.tag.is_some());
                if tagged || !spelled_tag {
                    diag::warning(
                        self.current_pos(),
                        &gettext("declaration does not declare anything"),
                    );
                    self.advance(); // consume ';'
                    continue;
                }
                // The anonymous member's own members join this name space
                // (C17 6.7.2.1p13), so they may not repeat one already here.
                let existing = self.member_names(&members);
                let added = self
                    .types
                    .composite(member_base_type_id)
                    .map(|c| self.member_names(&c.members))
                    .unwrap_or_default();
                for name in added.iter().filter(|n| existing.contains(n)) {
                    let spelled = self.idents.get_opt(*name).unwrap_or("").to_string();
                    diag::error_args(self.current_pos(), "duplicate member '{0}'", &[&spelled]);
                }
                members.push(StructMember {
                    name: StringId::EMPTY,
                    typ: member_base_type_id,
                    offset: 0,
                    bit_offset: None,
                    bit_width: None,
                    access_bytes: None,
                    // gcc takes an alignment written here and ignores
                    // `packed`, silently: the anonymous member keeps its
                    // type's alignment.
                    align: MemberAlign {
                        packed: false,
                        ..specifier_align
                    },
                });
                self.advance(); // consume ';'
                continue;
            }

            loop {
                // An unnamed bit-field, first in the list or after a comma:
                // `int a : 1, : 2, b : 3;` and `unsigned char :1, :1, x:1;`
                if self.is_special(b':') {
                    // Unnamed bitfield: parse width only
                    self.advance(); // consume ':'
                    let width = self.parse_bitfield_width()?;
                    self.validate_bitfield(member_base_type_id, width, false)?;
                    // Attributes after the width are this field's, as after
                    // a named one: `int :5 __attribute__((aligned(8)))`
                    // places it at the next 8-byte boundary under gcc, and
                    // `packed` packs it.
                    self.skip_extensions();
                    let typ = self.apply_pending_type_attrs(member_base_type_id);
                    let align = specifier_align.merge(self.take_member_align());

                    members.push(StructMember {
                        name: StringId::EMPTY,
                        typ,
                        offset: 0,
                        bit_offset: None,
                        bit_width: Some(width),
                        access_bytes: None,
                        align,
                    });

                    if self.is_special(b',') {
                        self.advance();
                        continue;
                    } else {
                        break;
                    }
                }

                // VLAs are not allowed in struct members
                let ParsedDeclarator {
                    name,
                    typ,
                    vla,
                    pos: member_pos,
                    ..
                } = self.parse_declarator(member_base_type_id, DeclaratorContext::Declaration)?;

                // C99 6.7.5.2: VLAs cannot be members of structures or unions
                if !vla.is_empty() {
                    return Err(ParseError::new(
                        "variable length arrays cannot be structure or union members".to_string(),
                        self.current_pos(),
                    ));
                }

                // 6.7.2.1p9 says the same of a member that reached its
                // variably modified type through its specifiers -- a
                // typedef, or `typeof(int[n])` -- the declarator having
                // written no `[n]`, so the check above sees nothing. Without
                // this the member looked like a flexible array (its extent is
                // absent) and drew a diagnostic about that instead. gcc's
                // wording.
                if !member_specs.vm_dims.is_empty() {
                    return Err(ParseError::new(
                        "a member of a structure or union cannot have a variably modified type"
                            .to_string(),
                        self.current_pos(),
                    ));
                }

                // Check for bitfield: name : width
                let bit_width = if self.is_special(b':') {
                    self.advance(); // consume ':'
                    let width = self.parse_bitfield_width()?;
                    // Validate bitfield type and width (this is a named bitfield)
                    self.validate_bitfield(typ, width, true)?;
                    Some(width)
                } else {
                    None
                };

                // Skip any __attribute__ after member declaration
                self.skip_extensions();
                self.drop_pending_cleanup(member_pos);

                // A member's type attributes are the member's: consuming
                // them here sizes the member, and keeps a `mode(M)` from
                // staying pending for the enclosing declaration.
                let typ = self.apply_pending_type_attrs(typ);

                // What this declarator adds to the specifiers' alignment.
                let member_align = specifier_align.merge(self.take_member_align());
                // C11 6.7.5p5: the `_Alignas` keyword may not weaken a
                // member's alignment either; the `aligned` attribute may.
                if let (Some(written), Some(_)) = (member_align.written, self.pending_alignas_kw) {
                    let natural = self.types.natural_alignment(typ) as u32;
                    if written < natural {
                        let spelled = self.idents.get_opt(name).unwrap_or("").to_string();
                        diag::error_args(
                            self.current_pos(),
                            "'_Alignas' specifiers cannot reduce alignment of '{0}'",
                            &[&spelled],
                        );
                    }
                }

                // C17 6.7.2.1p3: no member of incomplete or function type,
                // the flexible array member excepted. A member of the type
                // being defined is the same violation -- the tag is
                // incomplete until its `}` -- and one that got through made
                // a type that contains itself, which every walk over its
                // members followed forever.
                if !self.check_member_type(name, typ) {
                    if self.is_special(b',') {
                        self.advance();
                        continue;
                    }
                    break;
                }

                // C17 6.7.2.1p2: members share one name space, so a
                // repeated name is a constraint violation. Unnamed members
                // -- anonymous struct/union members and unnamed bitfields
                // -- all carry the empty name and are not repeats of each
                // other.
                if name != StringId::EMPTY && self.member_names(&members).contains(&name) {
                    let spelled = self.idents.get_opt(name).unwrap_or("").to_string();
                    diag::error_args(self.current_pos(), "duplicate member '{0}'", &[&spelled]);
                }

                members.push(StructMember {
                    name,
                    typ,
                    offset: 0, // Computed later
                    bit_offset: None,
                    bit_width,
                    access_bytes: None,
                    align: member_align,
                });

                if self.is_special(b',') {
                    self.advance();
                } else {
                    break;
                }
            }

            // C17 6.7.2.1 requires the `;`. gcc accepts a member list
            // whose last declaration lacks one, with a warning that
            // `-pedantic-errors` makes an error -- there is nothing
            // ambiguous about `struct S { int a; int b }`, the `}` says the
            // list ended.
            if self.is_special(b'}') {
                diag::pedwarn_default(
                    self.current_pos(),
                    &gettext("no semicolon at end of struct or union"),
                );
            } else {
                self.expect_special(b';')?;
            }
        }
        Ok(members)
    }

    /// Whether a member may have type `typ` (C17 6.7.2.1p3), diagnosing it
    /// if not. An array passes: its declarator already refused an incomplete
    /// element type, and whether an array of unknown size is a valid flexible
    /// array member is `check_flexible_array_members`' question.
    fn check_member_type(&self, name: StringId, typ: TypeId) -> bool {
        let spelled = self.idents.get_opt(name).unwrap_or("").to_string();
        let message = match self.types.kind(typ) {
            TypeKind::Function => "field '{0}' declared as a function",
            // An array's element type was checked by its declarator.
            TypeKind::Array => return true,
            _ if self.type_name_is_incomplete(typ) => "field '{0}' has incomplete type",
            _ => return true,
        };
        diag::error_args(self.current_pos(), message, &[&spelled]);
        false
    }

    /// The alignment and `packed` written since the last call, consumed: the
    /// member's half of [`crate::types::TypeTable::member_alignment`].
    fn take_member_align(&mut self) -> MemberAlign {
        MemberAlign {
            written: self.pending_alignas.take(),
            packed: std::mem::take(&mut self.pending_packed),
        }
    }

    /// Parse a bitfield width (constant expression after ':')
    fn parse_bitfield_width(&mut self) -> ParseResult<u32> {
        // C11 6.7.5p2: not on a bit-field.
        self.reject_alignas_in("a bit-field");
        let expr = self.parse_conditional_expr()?;
        match self.eval_const_expr(&expr) {
            Some(val) if val >= 0 => Ok(val as u32),
            Some(_) => Err(ParseError::new(
                "bitfield width must be non-negative",
                self.current_pos(),
            )),
            None => Err(ParseError::new(
                "bitfield width must be a constant expression",
                self.current_pos(),
            )),
        }
    }

    /// A bit-field wider than 64 bits needs a whole 16-byte storage unit.
    ///
    /// `emit_bitfield_load`/`_store` reach the 128-bit carrier only for an
    /// access span of exactly one addressable unit. A *packed* field gets a
    /// span of just the bytes its own bits touch, which sends it to the
    /// byte-wise path -- and that assembles into a 64-bit carrier, so it
    /// cannot represent the value. gcc packs these; c17 refuses them, which
    /// is a narrower divergence than the width cap this replaced.
    ///
    /// Checked after layout because packing is what decides the span, and
    /// packing is applied there.
    fn check_wide_bitfields_have_a_carrier(&self, members: &[StructMember]) {
        for m in members {
            let Some(width) = m.bit_width else { continue };
            // An unnamed field is never accessed, so needs no carrier: on an
            // ABI where it is padding its span is only the bytes it touches.
            if width <= 64 || m.access_bytes == Some(16) || m.is_unnamed_bitfield() {
                continue;
            }
            diag::error_args(
                self.current_pos(),
                "bit-field of width {0} needs an unpacked 16-byte storage unit",
                &[&width.to_string()],
            );
        }
    }

    /// Give the members of the struct or union being defined the storage
    /// order its `scalar_storage_order` attribute names, or failing that the
    /// one `#pragma scalar_storage_order` has in force.
    ///
    /// Only an order that differs from the target's changes anything, and
    /// only for the members gcc counts as scalars -- see
    /// [`crate::types::TypeTable::in_reverse_storage`]. The order is a
    /// property of the member types from here on: everything that reads or
    /// writes a member learns it from the type it accesses the member at.
    fn apply_storage_order(&mut self, members: &mut [StructMember], written: Option<ByteOrder>) {
        let order = match written {
            Some(order) => order,
            None => match self.current_storage_order() {
                StorageOrderPragma::Order(order) => order,
                StorageOrderPragma::Default => return,
            },
        };
        if order == self.types.target().byte_order() {
            return;
        }
        for member in members {
            member.typ = self.types.in_reverse_storage(member.typ);
        }
    }

    /// The struct or union `typ` as a reference to it with a
    /// `scalar_storage_order` attribute sees it: gcc's variant of the type
    /// in that order -- `typedef struct S __attribute__((...)) BE;` -- which
    /// has the same members and layout and is not compatible with `typ` when
    /// the order differs. Without an attribute, or with the order the type
    /// already has, the type itself.
    fn in_written_storage_order(&mut self, typ: TypeId, written: Option<ByteOrder>) -> Type {
        let mut variant = self.types.get(typ).clone();
        let Some(order) = written else {
            return variant;
        };
        let reverse = order != self.types.target().byte_order();
        let Some(composite) = variant.composite.as_deref_mut() else {
            return variant;
        };
        let mut changed = false;
        for member in &mut composite.members {
            let typ = if reverse {
                self.types.in_reverse_storage(member.typ)
            } else {
                self.types.in_native_storage(member.typ)
            };
            changed |= typ != member.typ;
            member.typ = typ;
        }
        if changed {
            // A type of its own, not a copy of the tag's: interning a copy
            // answers with the tag's `TypeId`, which has the other order.
            composite.tag_type = None;
        }
        variant
    }

    fn check_flexible_array_members(&self, members: &[StructMember], is_union: bool) {
        let Some(first) = members
            .iter()
            .position(|m| self.types.is_flexible_array_member(m))
        else {
            return;
        };
        let pos = self.current_pos();

        if is_union {
            diag::error(pos, &gettext("flexible array member in union"));
            return;
        }
        if first + 1 != members.len() {
            diag::error(pos, &gettext("flexible array member not at end of struct"));
            return;
        }
        // A named member has to precede it: the array is a tail on something,
        // and a struct that is nothing but a tail has no size to speak of. An
        // anonymous structure or union counts -- its members are members of
        // this struct (C17 6.7.2.1p13), as <linux/bpf.h> relies on -- and only
        // an unnamed bit-field, which is padding, does not.
        if members
            .iter()
            .take(first)
            .all(StructMember::is_unnamed_bitfield)
        {
            diag::error(
                pos,
                &gettext("flexible array member in a struct with no named members"),
            );
        }
    }

    fn validate_bitfield(&self, typ_id: TypeId, width: u32, is_named: bool) -> ParseResult<()> {
        // C17 6.7.2.1p5 allows `_Bool`, `signed int`, `unsigned int`, and "some
        // other implementation-defined type". gcc's set is every integer type,
        // enumerations included, and real headers lean on `enum E e : 2;`
        // heavily -- a hand-written list of `TypeKind`s omitted `Enum` and
        // rejected all of them. `is_integer` is that set, and already covers
        // every kind the list named.
        if !self.types.is_integer(typ_id) {
            return Err(ParseError::new(
                "bitfield must have integer type",
                self.current_pos(),
            ));
        }

        // C99: Zero-width bitfield with a name is an error
        // (zero-width unnamed bitfields are allowed for alignment)
        if width == 0 && is_named {
            return Err(ParseError::new(
                "named bit-field has zero width",
                self.current_pos(),
            ));
        }

        // C17 6.7.2.1p5: not an atomic type, which no bit-field can be
        // accessed as.
        if self.types.modifiers(typ_id).contains(TypeModifiers::ATOMIC) {
            return Err(ParseError::new(
                "bit-field has atomic type",
                self.current_pos(),
            ));
        }

        // Check that width doesn't exceed type size: the type's width in
        // bits (6.7.2.1p4), which for `_Bool` is one, not its eight-bit size.
        let max_width = if self.types.kind(typ_id) == TypeKind::Bool {
            1
        } else {
            self.types.size_bits(typ_id)
        };
        if width > max_width {
            return Err(ParseError::new(
                format!("bitfield width {} exceeds type size {}", width, max_width),
                self.current_pos(),
            ));
        }

        // A width above 64 needs a 16-byte carrier, which exists only for a
        // field the layout gives a whole `__int128` storage unit. Whether it
        // got one is not knowable here -- packing decides it, and packing is
        // applied at layout -- so the check lives in
        // `check_wide_bitfields_have_a_carrier`, once `access_bytes` is known.
        // The `width > max_width` test above still refuses `__int128 f:129`.

        // Warning: one-bit signed bitfield has dubious values
        // (can only hold -1 or 0 in 2's complement, or 0/-0 in other representations).
        // `_Bool` needs no exemption here: it is an unsigned type, which
        // `is_unsigned` now reports, so `_Bool f:1` is an ordinary flag.
        if width == 1 && !self.types.is_unsigned(typ_id) {
            diag::warning(
                self.current_pos(),
                &gettext("single-bit signed bit-field has dubious values"),
            );
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::super::test_parser::parse_tu;
    use crate::types::TypeModifiers;

    /// A forward reference to an enum tag is the type its definition later
    /// completes: the typedef made through it is the tag's own type, of the
    /// enum's size and signedness.
    #[test]
    fn a_forward_enum_is_completed_in_place() {
        let (_tu, types, strings, symbols) =
            parse_tu("typedef enum foo E; enum foo { a, b = 0x80000000u };").unwrap();
        let tag = symbols.lookup_tag(strings.lookup("foo").unwrap()).unwrap();
        let e = symbols
            .lookup_typedef(strings.lookup("E").unwrap())
            .unwrap();
        assert_eq!(e, tag.typ);
        assert_eq!(types.size_bits(e), 32);
        assert!(types.is_unsigned(e));
    }

    /// A qualified forward reference is a copy of the tag's type carrying its
    /// qualifiers, and the definition completes the copy along with the tag:
    /// it takes the enum's size and signedness and keeps its own `const`.
    #[test]
    fn a_qualified_forward_enum_is_completed_with_its_tag() {
        let (_tu, types, strings, symbols) = parse_tu(
            "enum big; typedef const enum big CB; enum big { n = -1, l = 0x7fffffffffLL };\n\
             enum pos; typedef volatile enum pos VP; enum pos { h = 0x80000000u };\n\
             struct S; typedef const struct S CS; struct S { long a, b; };",
        )
        .unwrap();
        let typedef = |name: &str| {
            symbols
                .lookup_typedef(strings.lookup(name).unwrap())
                .unwrap()
        };
        let (cb, vp, cs) = (typedef("CB"), typedef("VP"), typedef("CS"));
        assert_eq!(types.size_bits(cb), 64);
        assert!(!types.is_unsigned(cb));
        assert_eq!(types.qualifiers(cb), TypeModifiers::CONST);
        assert_eq!(types.size_bits(vp), 32);
        assert!(types.is_unsigned(vp));
        assert_eq!(types.qualifiers(vp), TypeModifiers::VOLATILE);
        assert!(types.is_composite_complete(cs));
        assert_eq!(types.size_bytes(cs), 16);
        assert_eq!(types.qualifiers(cs), TypeModifiers::CONST);
    }

    /// A tag defined in an inner scope is a new type: it leaves the outer
    /// forward declaration incomplete, for the outer definition to complete.
    #[test]
    fn an_inner_definition_does_not_complete_the_outer_tag() {
        let (_tu, types, strings, symbols) = parse_tu(
            "struct S; typedef const struct S CS;\n\
             void f(void) { struct S { int a; } s; }\n\
             struct S { long x, y; };",
        )
        .unwrap();
        let cs = symbols
            .lookup_typedef(strings.lookup("CS").unwrap())
            .unwrap();
        assert_eq!(types.size_bytes(cs), 16);
    }

    /// The types of the variables declared at the top of function `func`'s
    /// body, by name.
    fn locals_of(
        tu: &crate::parse::ast::TranslationUnit,
        strings: &crate::strings::StringTable,
        symbols: &crate::symbol::SymbolTable,
        func: &str,
    ) -> Vec<(String, crate::types::TypeId)> {
        use crate::parse::ast::{BlockItem, ExternalDecl, Stmt};
        let f = tu
            .items
            .iter()
            .find_map(|item| match item {
                ExternalDecl::FunctionDef(f) if strings.get(f.name) == func => Some(f),
                _ => None,
            })
            .unwrap();
        let Stmt::Block(items) = &f.body else {
            panic!("expected a block");
        };
        items
            .iter()
            .filter_map(|item| match item {
                BlockItem::Declaration(decl) => Some(decl),
                BlockItem::Statement(_) => None,
            })
            .flat_map(|decl| decl.declarators.iter())
            .map(|d| (strings.get(symbols.get(d.symbol).name).to_string(), d.typ))
            .collect()
    }

    /// A typedef name names the tag that was visible where the typedef was
    /// declared, even in a block that has given the tag's name to a new type.
    #[test]
    fn a_typedef_keeps_its_tag_under_an_inner_one() {
        let (tu, types, strings, symbols) = parse_tu(
            "struct S { int a; }; typedef struct S TS; typedef const struct S CTS;\n\
             void f(void) { struct S { char c[50]; }; TS x; CTS y; struct S z; }",
        )
        .unwrap();
        let outer = symbols
            .lookup_tag(strings.lookup("S").unwrap())
            .unwrap()
            .typ;
        let locals = locals_of(&tu, &strings, &symbols, "f");
        let typ = |name: &str| locals.iter().find(|(n, _)| n == name).unwrap().1;
        assert_eq!(typ("x"), outer);
        assert_eq!(types.size_bytes(typ("y")), 4);
        assert_eq!(types.qualifiers(typ("y")), TypeModifiers::CONST);
        assert_eq!(types.size_bytes(typ("z")), 50);
    }

    /// `struct S;` by itself declares a new tag in its block, which the
    /// block's definition completes; with a qualifier ahead of it, it names
    /// the visible tag and declares nothing.
    #[test]
    fn a_bare_tag_declaration_hides_the_outer_tag() {
        let (tu, types, strings, symbols) = parse_tu(
            "struct S { int a; };\n\
             void f(void) { struct S; struct S *p; struct S { char c[50]; } *q; }\n\
             void g(void) { const struct S; struct S *p; }",
        )
        .unwrap();
        let outer = symbols
            .lookup_tag(strings.lookup("S").unwrap())
            .unwrap()
            .typ;
        let pointee = |func: &str, name: &str| {
            let locals = locals_of(&tu, &strings, &symbols, func);
            let p = locals.iter().find(|(n, _)| n == name).unwrap().1;
            types.base_type(p).unwrap()
        };
        assert_ne!(pointee("f", "p"), outer);
        assert_eq!(pointee("f", "p"), pointee("f", "q"));
        assert_eq!(types.size_bytes(pointee("f", "p")), 50);
        assert_eq!(pointee("g", "p"), outer);
    }
}
