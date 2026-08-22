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

use super::parser::{DeclaratorName, ParseError, ParseResult, Parser};
use crate::diag;
use crate::strings::StringId;
use crate::symbol::{Namespace, Symbol, SymbolId};
use crate::token::lexer::{Position, TokenType};
use crate::types::{
    CompositeType, EnumConstant, StructMember, Type, TypeId, TypeKind, TypeModifiers, TypeTable,
};
use gettextrs::gettext;

const DEFAULT_MEMBER_CAPACITY: usize = 16;
const DEFAULT_ENUM_CAPACITY: usize = 16;

impl Parser<'_> {
    /// The integer type an enumerated type is compatible with, and its size.
    ///
    /// C17 6.7.2.2p4 requires it to represent every member; the choice among
    /// the types that do is implementation-defined. Narrowest wins, signed
    /// before unsigned at each width, so an enum whose members all fit in
    /// `int` is exactly the four signed bytes it has always been -- the
    /// widening only happens where the alternative was a wrong value.
    fn enum_underlying_type(
        &mut self,
        constants: &[EnumConstant],
        pos: Position,
    ) -> (TypeId, usize) {
        let Some(min) = constants.iter().map(|c| c.value).min() else {
            return (self.types.int_id, 4);
        };
        let max = constants.iter().map(|c| c.value).max().unwrap_or(0);

        let fits = |lo: i128, hi: i128| min >= lo && max <= hi;
        // 6.7.2.2p4 leaves the choice to the implementation, requiring only a
        // type that represents every member. gcc's choice is unsigned whenever
        // no enumerator is negative, and it is observable -- `(enum E)-1 > 0`
        // is true there and was false here. Preferring `int` for a small
        // non-negative list also made an enum bit-field read back negative:
        // `enum E { A, B, C, D }; struct { enum E e:2; }` holding `D` gave -1
        // where gcc gives 3, because the field's signedness follows the type's.
        if min >= 0 {
            if fits(0, u32::MAX as i128) {
                (self.types.uint_id, 4)
            } else if fits(0, u64::MAX as i128) {
                (self.types.ulong_id, 8)
            } else {
                // Not unreachable, as this once claimed: nothing clamps an
                // enumerator to 64 bits and the folder computes in `i128`, so
                // `enum E { A = 1 << 64 };` reached here and **panicked the
                // compiler**. gcc warns and carries on, folding the shift to 0
                // because it truncates to the expression's type -- which c17's
                // folders do not yet do (see #C115). Until they do, say what is
                // wrong rather than aborting: a compiler diagnoses.
                diag::error(
                    pos,
                    &gettext("no integer type can represent all values of this enumeration"),
                );
                (self.types.ulong_id, 8)
            }
        } else if fits(i32::MIN as i128, i32::MAX as i128) {
            (self.types.int_id, 4)
        } else if fits(i64::MIN as i128, i64::MAX as i128) {
            (self.types.long_id, 8)
        } else {
            // A list spanning below i64::MIN and above i64::MAX has no
            // integer type that holds both ends. Say so rather than picking
            // one and truncating the other.
            diag::error(
                pos,
                &gettext("no integer type can represent all values of this enumeration"),
            );
            (self.types.long_id, 8)
        }
    }

    /// Parse an enum specifier
    /// enum-specifier: 'enum' identifier? '{' enumerator-list? '}' | 'enum' identifier
    pub(crate) fn parse_enum_specifier(&mut self) -> ParseResult<Type> {
        let enum_pos = self.current_pos();
        self.advance(); // consume 'enum'

        // Optional tag name
        let tag = if self.peek() == TokenType::Ident && !self.is_special(b'{') {
            Some(self.expect_identifier()?)
        } else {
            None
        };

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
                let name = self.expect_identifier()?;

                let value = if self.is_special(b'=') {
                    self.advance();
                    let value_pos = self.current_pos();
                    let expr = self.parse_conditional_expr()?;
                    // Evaluate constant expression
                    let v = self.eval_const_expr(&expr).ok_or_else(|| {
                        ParseError::new("enum value must be constant", self.current_pos())
                    })?;
                    // C17 6.7.2.2p2 requires an enumerator to be
                    // representable as `int`, so exceeding it is a constraint
                    // violation and 5.1.1.3 requires it be diagnosed. gcc
                    // widens the enumerated type rather than rejecting, and
                    // so does c17 -- but not in silence, which is how
                    // `X = 5000000000` used to become 705032704.
                    if v < i32::MIN as i128 || v > i32::MAX as i128 {
                        diag::warning_args(
                            value_pos,
                            "enumerator value {0} is outside the range of 'int'",
                            &[&v.to_string()],
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

            // Empty enum definition is a GNU extension, warn with -Wpedantic
            if constants.is_empty() {
                diag::warning(
                    self.current_pos(),
                    &gettext("empty enum definition is a GNU extension"),
                );
            }

            self.expect_special(b'}')?;

            // C17 6.7.2.2p4: the enumerated type is compatible with some
            // integer type capable of representing every member. Pick the
            // narrowest that is, preferring signed at each width, and give
            // the enumerators that type -- for an enum that fits in `int`,
            // which is nearly all of them, this is the `int` it always was.
            let (underlying, size) = self.enum_underlying_type(&constants, enum_pos);
            for &sym_id in &constant_syms {
                self.symbols.get_mut(sym_id).typ = underlying;
            }

            let composite = CompositeType {
                tag,
                members: Vec::new(),
                enum_constants: constants,
                size,
                align: size,
                is_complete: true,
                transparent: false,
            };

            let mut enum_type = Type::enum_type(composite);
            // C17 6.7.2.2p4: the enumerated type shall represent every member.
            // `enum_underlying_type` picks a type that does, but the enum's own
            // type carried no signedness, so an *object* of it was loaded and
            // compared as signed even where the underlying type is unsigned:
            // `enum E { BIG = 0x80000000u }; enum E e = BIG;` read back
            // -2147483648 and `e < 0` was true, while the constant `BIG` was
            // correct all along.
            if self.types.is_unsigned(underlying) {
                enum_type.modifiers |= TypeModifiers::UNSIGNED;
            }

            // Register tag if present
            if let Some(tag_name) = tag {
                let enum_type_id = self.types.intern(enum_type.clone());
                let sym = Symbol::tag(tag_name, enum_type_id, self.symbols.depth());
                let _ = self.symbols.declare(sym);
            }

            Ok(enum_type)
        } else {
            // Forward reference - look up existing tag
            if let Some(tag_name) = tag {
                // Look up or create incomplete type
                if let Some(existing) = self.symbols.lookup_tag(tag_name) {
                    // Return a clone of the underlying type
                    Ok(self.types.get(existing.typ).clone())
                } else {
                    Ok(Type::incomplete_enum(tag_name))
                }
            } else {
                Err(ParseError::new(
                    "expected enum definition or tag name",
                    self.current_pos(),
                ))
            }
        }
    }

    /// Parse a struct or union specifier
    /// struct-or-union-specifier: ('struct'|'union') identifier? '{' struct-declaration-list? '}'
    ///                          | ('struct'|'union') identifier
    pub(crate) fn parse_struct_or_union_specifier(&mut self, is_union: bool) -> ParseResult<Type> {
        let specifier_pos = self.current_pos();
        self.advance(); // consume 'struct' or 'union'

        // Parse __attribute__ between 'struct' keyword and tag name
        // (e.g., struct __attribute__((packed)) tagname { ... })
        let early_attrs = self.parse_attributes();
        let mut is_packed = early_attrs
            .attrs
            .iter()
            .any(|a| a.name == "packed" || a.name == "__packed__");
        // `transparent_union` is collected at the same four positions as
        // `packed`, for the same reason: gcc accepts it at any of them.
        let mut is_transparent = early_attrs.has_transparent_union();
        // Track struct-level aligned attribute (max across all positions)
        let mut struct_align: Option<u32> = early_attrs.get_alignment();

        // Optional tag name
        let tag = if self.peek() == TokenType::Ident && !self.is_special(b'{') {
            if !self.is_attribute_keyword() {
                Some(self.expect_identifier()?)
            } else {
                let mid_attrs = self.parse_attributes();
                is_packed = is_packed
                    || mid_attrs
                        .attrs
                        .iter()
                        .any(|a| a.name == "packed" || a.name == "__packed__");
                is_transparent = is_transparent || mid_attrs.has_transparent_union();
                if let Some(a) = mid_attrs.get_alignment() {
                    struct_align = Some(struct_align.map_or(a, |e| e.max(a)));
                }
                if self.peek() == TokenType::Ident && !self.is_special(b'{') {
                    Some(self.expect_identifier()?)
                } else {
                    None
                }
            }
        } else {
            None
        };

        // Parse __attribute__ after tag name but before '{'
        let pre_attrs = self.parse_attributes();
        is_packed = is_packed
            || pre_attrs
                .attrs
                .iter()
                .any(|a| a.name == "packed" || a.name == "__packed__");
        is_transparent = is_transparent || pre_attrs.has_transparent_union();
        if let Some(a) = pre_attrs.get_alignment() {
            struct_align = Some(struct_align.map_or(a, |e| e.max(a)));
        }

        // Check for definition vs forward reference
        if self.is_special(b'{') {
            self.advance(); // consume '{'

            let mut members = Vec::with_capacity(DEFAULT_MEMBER_CAPACITY);

            while !self.is_special(b'}') && !self.is_eof() {
                // Check for _Static_assert in struct (C11 6.7.2.1p1)
                if self.is_static_assert() {
                    self.parse_static_assert()?;
                    continue;
                }

                // Parse member declaration
                let member_base_type = self.parse_type_specifier()?;
                let is_struct_or_union =
                    matches!(member_base_type.kind, TypeKind::Struct | TypeKind::Union);
                // For struct/union types with tags, use the existing TypeId from symbol table
                // to ensure forward-declared types are properly linked
                let member_base_type_id = self.intern_type_with_tag(&member_base_type);

                // Skip any __attribute__ after type specifier (before member name)
                self.skip_extensions();

                // C11 anonymous struct/union members: "struct { ... };" or "union { ... };"
                // These have no declarator name, just end with ';'
                if is_struct_or_union && self.is_special(b';') {
                    members.push(StructMember {
                        name: StringId::EMPTY,
                        typ: member_base_type_id,
                        offset: 0,
                        bit_offset: None,
                        bit_width: None,
                        access_bytes: None,
                        explicit_align: None, // anonymous members
                    });
                    self.advance(); // consume ';'
                    continue;
                }

                // Check for unnamed bitfield (starts with ':')
                if self.is_special(b':') {
                    // Unnamed bitfield: parse width only
                    self.advance(); // consume ':'
                    let width = self.parse_bitfield_width()?;
                    // An unnamed bit-field is still a bit-field: its type has
                    // to be one a bit-field may have, and its width has to fit.
                    // Neither unnamed site validated anything, so
                    // `struct { float : 3; }` was accepted.
                    self.validate_bitfield(member_base_type_id, width, false)?;

                    members.push(StructMember {
                        name: StringId::EMPTY,
                        typ: member_base_type_id,
                        offset: 0,
                        bit_offset: None,
                        bit_width: Some(width),
                        access_bytes: None,
                        explicit_align: None, // bitfields don't support _Alignas
                    });

                    self.expect_special(b';')?;
                    continue;
                }

                loop {
                    // Check for unnamed bitfield (can appear after ',' too)
                    // e.g., "int a : 1, : 2, b : 3;"
                    if self.is_special(b':') {
                        // Unnamed bitfield: parse width only
                        self.advance(); // consume ':'
                        let width = self.parse_bitfield_width()?;
                        self.validate_bitfield(member_base_type_id, width, false)?;

                        members.push(StructMember {
                            name: StringId::EMPTY,
                            typ: member_base_type_id,
                            offset: 0,
                            bit_offset: None,
                            bit_width: Some(width),
                            access_bytes: None,
                            explicit_align: None, // bitfields don't support _Alignas
                        });

                        if self.is_special(b',') {
                            self.advance();
                            continue;
                        } else {
                            break;
                        }
                    }

                    // VLAs are not allowed in struct members
                    let (name, typ, vla_sizes, _func_params) =
                        self.parse_declarator(member_base_type_id, DeclaratorName::Required)?;

                    // C99 6.7.5.2: VLAs cannot be members of structures or unions
                    if !vla_sizes.is_empty() {
                        return Err(ParseError::new(
                            "variable length arrays cannot be structure or union members"
                                .to_string(),
                            self.current_pos(),
                        ));
                    }

                    // 6.7.2.1p9 says the same of a member that reached its
                    // variably modified type through a typedef, which is the
                    // only other way in -- the declarator wrote no `[n]`, so
                    // the check above sees nothing. Without this the member
                    // looked like a flexible array (its extent is absent) and
                    // drew a diagnostic about that instead. gcc's wording.
                    if self.pending_vm_typedef_dims.is_some() {
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

                    // A member's type attributes are the member's. Nothing
                    // consumed them here, so `__attribute__((mode(M)))` on a
                    // member did two wrong things at once: it did not size the
                    // member, and it stayed pending, so the next declarator to
                    // consume one -- the enclosing declaration's -- took it
                    // instead. `struct Big { int arr[100]; int x
                    // __attribute__((mode(QI))); } b;` gave `sizeof b == 1`
                    // where gcc gives 404. The eight declarator sites all do
                    // this; the member loop was the gap.
                    let typ = self.apply_pending_type_attrs(typ);

                    // Capture any pending _Alignas from type specifier
                    let member_align = self.pending_alignas.take();

                    // C17 6.7.2.1p2: members share one name space, so a
                    // repeated name is a constraint violation. Unnamed members
                    // -- anonymous struct/union members and unnamed bitfields
                    // -- all carry the empty name and are not repeats of each
                    // other.
                    if name != StringId::EMPTY && members.iter().any(|m| m.name == name) {
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
                        explicit_align: member_align,
                    });

                    if self.is_special(b',') {
                        self.advance();
                    } else {
                        break;
                    }
                }

                self.expect_special(b';')?;
            }

            self.expect_special(b'}')?;

            // Parse trailing __attribute__ (e.g., __attribute__((packed)))
            let attrs = self.parse_attributes();
            is_packed = is_packed
                || attrs
                    .attrs
                    .iter()
                    .any(|a| a.name == "packed" || a.name == "__packed__");
            is_transparent = is_transparent || attrs.has_transparent_union();
            if let Some(a) = attrs.get_alignment() {
                struct_align = Some(struct_align.map_or(a, |e| e.max(a)));
            }

            self.check_flexible_array_members(&members, is_union);

            // Compute layout. `__attribute__((packed))` is a cap of 1; a
            // `#pragma pack(n)` in force is a cap of n. Where both apply the
            // tighter one wins, which is what gcc does.
            let pragma_cap = self.current_pack();
            let pack_cap = match (is_packed, pragma_cap) {
                (true, Some(n)) => Some(n.min(1)),
                (true, None) => Some(1),
                (false, cap) => cap,
            };
            let (size, mut align) = if is_union {
                self.types.compute_union_layout(&mut members, pack_cap)
            } else {
                self.types.compute_struct_layout(&mut members, pack_cap)
            };
            self.check_wide_bitfields_have_a_carrier(&members);

            // Apply struct-level aligned attribute (raises alignment, never lowers)
            if let Some(sa) = struct_align {
                if sa as usize > align {
                    align = sa as usize;
                }
            }

            // Re-pad size to new alignment
            let size = if align > 1 {
                (size + align - 1) & !(align - 1)
            } else {
                size
            };

            // The same bound `derive_array_type` enforces: a member list can
            // reach it even when no single member does.
            if size > TypeTable::MAX_OBJECT_BYTES {
                return Err(ParseError::new(
                    format!(
                        "size of {} exceeds the maximum object size of {} bytes",
                        if is_union { "union" } else { "struct" },
                        TypeTable::MAX_OBJECT_BYTES
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
                is_complete: true,
                transparent: is_transparent && is_union,
            };

            // Check if there's an existing forward declaration that we should complete
            if let Some(tag_name) = tag {
                if let Some(existing) = self.symbols.lookup_tag(tag_name) {
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
            let struct_type = if is_union {
                Type::union_type(composite)
            } else {
                Type::struct_type(composite)
            };

            // Register tag if present
            if let Some(tag_name) = tag {
                let typ_id = self.types.intern(struct_type.clone());
                let sym = Symbol::tag(tag_name, typ_id, self.symbols.depth());
                let _ = self.symbols.declare(sym);
            }

            Ok(struct_type)
        } else {
            // Forward reference
            if let Some(tag_name) = tag {
                // Look up existing tag
                if let Some(existing) = self.symbols.lookup_tag(tag_name) {
                    Ok(self.types.get(existing.typ).clone())
                } else {
                    // Create new incomplete type and register it in symbol table
                    // This ensures that when the type is completed later, we can update
                    // this same TypeId rather than creating a new one
                    let incomplete_type = if is_union {
                        Type::incomplete_union(tag_name)
                    } else {
                        Type::incomplete_struct(tag_name)
                    };
                    let typ_id = self.types.intern(incomplete_type.clone());
                    let sym = Symbol::tag(tag_name, typ_id, self.symbols.depth());
                    let _ = self.symbols.declare(sym);
                    Ok(incomplete_type)
                }
            } else {
                Err(ParseError::new(
                    "expected struct/union definition or tag name",
                    self.current_pos(),
                ))
            }
        }
    }

    /// Parse a bitfield width (constant expression after ':')
    fn parse_bitfield_width(&mut self) -> ParseResult<u32> {
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
            if width <= 64 || m.access_bytes == Some(16) {
                continue;
            }
            diag::error_args(
                self.current_pos(),
                "bit-field of width {0} needs an unpacked 16-byte storage unit",
                &[&width.to_string()],
            );
        }
    }

    fn check_flexible_array_members(&self, members: &[StructMember], is_union: bool) {
        let is_flexible =
            |m: &StructMember| m.bit_width.is_none() && self.types.unsized_array_levels(m.typ) > 0;

        let Some(first) = members.iter().position(is_flexible) else {
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
        // and a struct that is nothing but a tail has no size to speak of.
        if members
            .iter()
            .take(first)
            .all(|m| m.name == StringId::EMPTY)
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

        // Check that width doesn't exceed type size
        let max_width = self.types.size_bits(typ_id);
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
