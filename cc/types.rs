//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Compositional type model with interning for efficient comparison.
//

use crate::abi::CallingConv;
use crate::float::{ComplexRoutineFormat, FpFormat};
use crate::strings::{StringId, StringTable as IdentTable};
use crate::target::{Arch, CharSignedness, IntType, Os, Target};
use std::collections::HashMap;
use std::fmt;

// Type ID - Unique identifier for interned types

/// A unique identifier for an interned type (like IdentTable for strings)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct TypeId(pub u32);

impl TypeId {
    /// Invalid/uninitialized type ID
    pub const INVALID: TypeId = TypeId(u32::MAX);

    /// Check if this is a valid type ID
    #[cfg(test)]
    pub fn is_valid(&self) -> bool {
        self.0 != u32::MAX
    }
}

// Composite Type Components

/// A struct/union member
#[derive(Debug, Clone, PartialEq)]
pub struct StructMember {
    /// Member name (StringId::EMPTY for unnamed bitfields)
    pub name: StringId,
    /// Member type (interned TypeId)
    pub typ: TypeId,
    /// Byte offset within struct (0 for unions, offset of storage unit for bitfields)
    pub offset: usize,
    /// For a bit-field: bit offset within its access span (0 = LSB)
    pub bit_offset: Option<u32>,
    /// For bitfields: bit width
    pub bit_width: Option<u32>,
    /// For a bit-field: how many bytes of the **access span** beginning at
    /// `offset` the field is read and written through. The field occupies bits
    /// `[bit_offset, bit_offset + bit_width)` of that span, and two invariants
    /// hold: `bit_offset + bit_width <= access_bytes * 8`, and
    /// `offset + access_bytes <= sizeof(the enclosing aggregate)`.
    ///
    /// Unpacked, the span is the naturally-aligned `sizeof(T)` window the field
    /// provably sits inside and `offset` is that window's base. Packed, it is
    /// exactly the bytes the field's own bits touch, `offset` is the first of
    /// them and `bit_offset < 8` -- a span that need not be a power of two, and
    /// need not be aligned. The name was `storage_unit_size`, which was only
    /// ever true of the unpacked case.
    pub access_bytes: Option<u32>,
    /// What the member's own declaration says about its alignment. See
    /// [`TypeTable::member_alignment`] for how it combines with the type's.
    pub align: MemberAlign,
}

/// What a member's declaration says about its alignment, apart from what its
/// type says.
///
/// Both halves are written on the member -- in its specifiers, which give them
/// to every declarator of the declaration, or after one declarator, which
/// gives them to that one -- and a struct-level `packed` is `packed` on every
/// member, which is how gcc defines it.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct MemberAlign {
    /// `_Alignas(n)` or `__attribute__((aligned(n)))`: raises the member's
    /// alignment, and never lowers it.
    pub written: Option<u32>,
    /// `__attribute__((packed))`: drops the alignment the member's type
    /// demands to one byte, and packs a bit-field to the bit.
    pub packed: bool,
}

impl MemberAlign {
    /// No alignment written and not packed: the type's own alignment.
    pub const NATURAL: MemberAlign = MemberAlign {
        written: None,
        packed: false,
    };

    /// Both declarations' alignments at once: the larger written alignment,
    /// and packed if either is.
    pub fn merge(self, other: MemberAlign) -> MemberAlign {
        MemberAlign {
            written: self.written.max(other.written),
            packed: self.packed || other.packed,
        }
    }
}

/// Where a bit-field lies: the access span its bits are read and written
/// through, and the bits within it. See [`StructMember::access_bytes`] for
/// the span's contract.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Bitfield {
    /// Byte offset of the access span from the base of the object.
    pub offset: usize,
    /// The field's first bit within the span.
    pub bit_offset: u32,
    /// The field's width in bits.
    pub bit_width: u32,
    /// The span's length in bytes.
    pub access_bytes: u32,
}

impl Bitfield {
    /// The placement the three member fields describe, when they describe
    /// one: a bit-field of non-zero width.
    pub fn from_parts(
        offset: usize,
        bit_offset: Option<u32>,
        bit_width: Option<u32>,
        access_bytes: Option<u32>,
    ) -> Option<Bitfield> {
        Some(Bitfield {
            offset,
            bit_offset: bit_offset?,
            bit_width: bit_width.filter(|&w| w > 0)?,
            access_bytes: access_bytes?,
        })
    }

    /// The bytes the field's own bits occupy, from the object's base --
    /// narrower than the access span, which covers bytes other members own:
    /// what decides whether two initializers describe overlapping storage.
    pub fn own_bytes(&self) -> std::ops::Range<usize> {
        own_bit_bytes(self.offset, self.bit_offset, self.bit_width)
    }
}

/// The bytes bits `[bit_offset, bit_offset + bit_width)` of the span at
/// `offset` occupy: at least one, so that a zero-width field still names a
/// place.
pub fn own_bit_bytes(offset: usize, bit_offset: u32, bit_width: u32) -> std::ops::Range<usize> {
    let start = offset + (bit_offset / 8) as usize;
    let end = offset + (bit_offset + bit_width).div_ceil(8) as usize;
    start..end.max(start + 1)
}

impl StructMember {
    /// This member's placement, if it is a bit-field of non-zero width.
    pub fn bitfield(&self) -> Option<Bitfield> {
        Bitfield::from_parts(
            self.offset,
            self.bit_offset,
            self.bit_width,
            self.access_bytes,
        )
    }

    /// Does an initializer reach this member?
    ///
    /// Everything but an unnamed bit-field, which is padding rather than a
    /// member (C17 6.7.2.1p12) and is skipped by positional initialization
    /// (6.7.9p9). An anonymous structure or union *is* reached: it is the
    /// member its own members live in (6.7.2.1p13).
    pub fn is_initializable(&self) -> bool {
        self.name != StringId::EMPTY || self.bit_width.is_none()
    }
}

/// Information about a struct/union member lookup
#[derive(Debug, Clone, Copy)]
pub struct MemberInfo {
    /// Byte offset within struct
    pub offset: usize,
    /// Member type (interned TypeId)
    pub typ: TypeId,
    /// For a bit-field: bit offset within the access span
    pub bit_offset: Option<u32>,
    /// For a bit-field: bit width
    pub bit_width: Option<u32>,
    /// For a bit-field: the access span in bytes. See [`StructMember`] for the
    /// contract -- it is not always a storage unit, and not always a power of
    /// two.
    pub access_bytes: Option<u32>,
    /// The qualifiers of the anonymous structures and unions the lookup
    /// passed through to reach the member, as [`Type::MEMBER_QUALIFIERS`]
    /// selects them.
    ///
    /// `struct { volatile struct { int a; }; } s;` makes `s.a` a member of a
    /// volatile object (C17 6.7.2.1p13 makes it a member of `s`, and it lives
    /// inside the anonymous one), so `s.a` is volatile although neither `s`
    /// nor `a` was declared so. `typ` is still the declared type; apply these
    /// through [`TypeTable::subobject_type`].
    pub quals: TypeModifiers,
}

impl MemberInfo {
    /// This member's placement, if it is a bit-field of non-zero width.
    pub fn bitfield(&self) -> Option<Bitfield> {
        Bitfield::from_parts(
            self.offset,
            self.bit_offset,
            self.bit_width,
            self.access_bytes,
        )
    }

    /// A stand-in for a member lookup that failed, occupying the whole object
    /// at offset 0 with type `typ`.
    ///
    /// Only reachable once the parser has already reported the unknown
    /// member, so what it answers never reaches an object file; it exists so
    /// the linearizer can keep walking rather than panic.
    pub fn standing_in(typ: TypeId) -> MemberInfo {
        MemberInfo {
            offset: 0,
            typ,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            quals: TypeModifiers::empty(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumConstant {
    /// Constant name (interned StringId)
    pub name: StringId,
    /// Constant value.
    ///
    /// Held at 128 bits so nothing wraps on the way in: an enumerator may
    /// need `unsigned long`, whose upper half does not fit a signed 64-bit
    /// slot, and truncating here is the defect this width exists to prevent.
    pub value: i128,
}

/// Composite type definition (struct, union, or enum)
#[derive(Debug, Clone, PartialEq)]
pub struct CompositeType {
    /// Tag name (e.g., "point" in "struct point") - None for anonymous
    pub tag: Option<StringId>,
    /// Members for struct/union
    pub members: Vec<StructMember>,
    /// Constants for enum
    pub enum_constants: Vec<EnumConstant>,
    /// Total size in bytes
    pub size: usize,
    /// Alignment requirement in bytes
    pub align: usize,
    /// The alignment the **members** require, before a struct-level
    /// `__attribute__((aligned(N)))` raises `align` above it.
    ///
    /// Not "natural alignment": `natural_alignment()` answers `align` for a
    /// composite and so cannot tell an over-aligned struct from a naturally
    /// aligned one. This can, and AAPCS64 needs it -- that ABI derives an
    /// argument's alignment from the members and ignores the type's own
    /// attribute, so passing a 32-byte-aligned struct must not pad the
    /// argument area to 32.
    ///
    /// Recorded rather than recomputed because `__attribute__((packed))` is
    /// consumed at parse time as a pack cap and never stored: walking the
    /// members of a packed struct would answer 16 where the truth is 1.
    /// `compute_struct_layout` already returns exactly this number.
    pub member_align: usize,
    /// False for forward declarations
    pub is_complete: bool,
    /// `__attribute__((transparent_union))`: an argument matching **any**
    /// member's type may be passed to a parameter of this union, and the
    /// union is passed as its first member would be. Unions only; gcc's
    /// attribute governs calls, so assignment and `return` stay strict.
    pub transparent: bool,
    /// Which definition a tagless struct, union or enum came from; `None` for
    /// a tagged or synthesized one.
    ///
    /// C17 6.7.2.3p5: each struct-or-union specifier with a member list
    /// declares a *distinct* type, so two tagless definitions with the same
    /// members are still two incompatible types. With nothing but the members
    /// to compare, `struct { long a, b; } x; struct { long a, b; } y; x = y;`
    /// was accepted, where gcc rejects it. A qualified variant of the type
    /// clones the composite, and so keeps the identity.
    pub anon_id: Option<u32>,
    /// For a tagged struct, union or enum: the tag's own `TypeId`, the type
    /// its first declaration created and its definition completes in place.
    ///
    /// The type's identity, carried by every copy of the composite. A
    /// qualified reference -- `const enum E`, `volatile struct S` -- is a
    /// separate `TypeId` holding a clone of this composite, and so is the
    /// `Type` a typedef name or `typeof` hands a declaration; each says which
    /// tag it is through this, never through its tag's *name*, which an
    /// inner scope may have given to a different type. [`TypeTable::intern`]
    /// records every copy of an incomplete tag, and the definition completes
    /// the copies with the tag: a `typedef const enum E CE;` ahead of the
    /// list has the enum's size and signedness, and a `const enum E *`
    /// parameter declared before it is the same type as one declared after.
    /// Set by `intern`; `None` for a tagless composite, whose identity is
    /// [`Self::anon_id`].
    pub tag_type: Option<TypeId>,
}

impl CompositeType {
    /// Create a new empty composite type (forward declaration)
    pub fn incomplete(tag: Option<StringId>) -> Self {
        Self {
            tag,
            members: Vec::new(),
            enum_constants: Vec::new(),
            size: 0,
            align: 1,
            member_align: 1,
            is_complete: false,
            transparent: false,
            anon_id: None,
            tag_type: None,
        }
    }

    // NOTE: compute_struct_layout and compute_union_layout have been moved to TypeTable
    // since they require access to member type sizes via TypeId lookup.
}

// Type Modifiers

bitflags::bitflags! {
    /// Type modifiers (storage class, qualifiers, signedness)
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
    pub struct TypeModifiers: u32 {
        // Storage class specifiers
        const STATIC   = 1 << 0;
        const EXTERN   = 1 << 1;
        const REGISTER = 1 << 2;
        const AUTO     = 1 << 3;
        const TYPEDEF  = 1 << 13;  // Not a storage class semantically, but syntactically

        // Type qualifiers
        const CONST    = 1 << 4;
        const VOLATILE = 1 << 5;
        const RESTRICT = 1 << 6;

        // Signedness
        const SIGNED   = 1 << 7;
        const UNSIGNED = 1 << 8;

        // Size modifiers
        const SHORT    = 1 << 9;
        const LONG     = 1 << 10;
        const LONGLONG = 1 << 11;

        // Inline
        const INLINE   = 1 << 12;

        // Function specifier: noreturn (_Noreturn keyword, C11)
        const NORETURN = 1 << 14;

        // C99 complex type specifier
        const COMPLEX = 1 << 15;

        // C11 atomic type qualifier
        const ATOMIC = 1 << 16;

        // C11 thread-local storage specifier
        const THREAD_LOCAL = 1 << 17;

        // A GNU `vector_size` type. c17 lays one out as an array of its
        // elements, and this is what tells it apart from a real array, which
        // decays where a vector is a value.
        const VECTOR = 1 << 18;

        // `__builtin_ms_va_list`: a `char *` that `__builtin_va_arg` walks
        // by the Microsoft x64 convention. gcc makes it a variant of `char *`
        // -- assignable to and from one without a diagnostic -- that is
        // still the one pointer `va_arg` walks as a list, and this bit is
        // that variant.
        const MS_VA_LIST = 1 << 19;

        // The result of a vector comparison. gcc makes it "opaque": the
        // signed integer vector of its shape to everything but assignment,
        // where any vector of integer lanes of that shape takes it -- so
        // `unsigned_v = a < b` is valid where `unsigned_v = signed_v` is not.
        const VECTOR_MASK = 1 << 20;
    }
}

// Type Kinds

/// Basic type kinds for C99 types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeKind {
    // Basic types
    Void,
    Bool,
    Char,
    Short,
    Int,
    Long,
    LongLong,
    /// __int128 / __uint128_t - 128-bit integer (GCC/Clang extension)
    Int128,
    Float,
    Double,
    LongDouble,
    /// _Float16 - IEEE 754 binary16 (half precision)
    /// TS 18661-3 / C23 interchange type
    Float16,
    /// __float128 / _Float128 - IEEE 754 binary128 (quad precision).
    ///
    /// Distinct from `LongDouble` even where the two share a format: on
    /// aarch64 Linux `long double` *is* binary128, but on x86-64 it is the
    /// x87 80-bit format and binary128 is a separate type with its own ABI.
    /// Keeping them apart is what lets one lowering serve both.
    Float128,

    // Derived types
    Pointer,
    Array,
    Function,

    // Composite types (for future expansion)
    Struct,
    Union,
    Enum,

    // Compiler builtin types
    /// __builtin_va_list - platform-specific variadic argument list type
    /// x86-64: 24-byte struct (1 element array of struct with 4 fields)
    /// aarch64-linux: 32-byte struct
    /// aarch64-macos: char* (8 bytes, same as pointer)
    VaList,
}

impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeKind::Void => write!(f, "void"),
            TypeKind::Bool => write!(f, "_Bool"),
            TypeKind::Char => write!(f, "char"),
            TypeKind::Short => write!(f, "short"),
            TypeKind::Int => write!(f, "int"),
            TypeKind::Long => write!(f, "long"),
            TypeKind::LongLong => write!(f, "long long"),
            TypeKind::Int128 => write!(f, "__int128"),
            TypeKind::Float => write!(f, "float"),
            TypeKind::Double => write!(f, "double"),
            TypeKind::LongDouble => write!(f, "long double"),
            TypeKind::Float16 => write!(f, "_Float16"),
            TypeKind::Float128 => write!(f, "__float128"),
            TypeKind::Pointer => write!(f, "pointer"),
            TypeKind::Array => write!(f, "array"),
            TypeKind::Function => write!(f, "function"),
            TypeKind::Struct => write!(f, "struct"),
            TypeKind::Union => write!(f, "union"),
            TypeKind::Enum => write!(f, "enum"),
            TypeKind::VaList => write!(f, "__builtin_va_list"),
        }
    }
}

/// Which of C23's families of binary floating types (TS 18661-3) a
/// floating type of kind `Float`, `Double` or `LongDouble` belongs to.
///
/// `_Float32` has `float`'s format and is still a different type: not
/// compatible with it, a separate `_Generic` association, never promoted
/// through `...`. glibc's <math.h> lists `float:` and `_Float32:` in one
/// `_Generic` once the compiler claims gcc 7, so treating the names as
/// aliases rejects the header. The kind stays the format's, so everything
/// below the type system -- layout, ABI, code generation -- sees one type.
/// `_Float16` and `_Float128` have kinds of their own and stay `Standard`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum FloatClass {
    /// `float`, `double`, `long double`.
    #[default]
    Standard,
    /// `_Float32` and `_Float64`.
    Interchange,
    /// `_Float32x` and `_Float64x`.
    Extended,
}

impl FloatClass {
    /// The `_FloatN`/`_FloatNx` keyword for a type of this class and `kind`,
    /// or `None` for the standard types, which their kind spells.
    pub fn keyword(self, kind: TypeKind) -> Option<&'static str> {
        match (self, kind) {
            (FloatClass::Interchange, TypeKind::Float) => Some("_Float32"),
            (FloatClass::Interchange, TypeKind::Double) => Some("_Float64"),
            (FloatClass::Extended, TypeKind::Double) => Some("_Float32x"),
            (FloatClass::Extended, TypeKind::LongDouble) => Some("_Float64x"),
            _ => None,
        }
    }

    /// C23 6.3.1.8p1: of two types with the same format, an interchange type
    /// wins over a standard one, which wins over an extended one -- gcc's
    /// reading: `_Float32 + float` is `_Float32`, `_Float32x + double` is
    /// `double`.
    fn preference(self) -> u8 {
        match self {
            FloatClass::Interchange => 2,
            FloatClass::Standard => 1,
            FloatClass::Extended => 0,
        }
    }
}

// Type Representation

/// A C type (compositional structure)
///
/// Types are built compositionally using TypeId references:
/// - `int *p` -> Pointer { base: TypeId(int) }
/// - `int arr[10]` -> Array { base: TypeId(int), size: 10 }
/// - `int (*fp)(int)` -> Pointer { base: Function { return: TypeId(int), params: [TypeId(int)] } }
///
/// All nested types are referenced by TypeId, which are looked up in a TypeTable.
#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    /// The kind of type
    pub kind: TypeKind,

    /// Type modifiers (const, volatile, signed, unsigned, etc.)
    pub modifiers: TypeModifiers,

    /// Base type for pointers, arrays, and function return types (interned TypeId)
    pub base: Option<TypeId>,

    /// Array size (for arrays)
    pub array_size: Option<usize>,

    /// Function parameter types (interned TypeIds)
    pub params: Option<Vec<TypeId>>,

    /// Is this function variadic? (for functions)
    pub variadic: bool,

    /// Is this function noreturn? (for functions)
    /// Set via __attribute__((noreturn)) or _Noreturn keyword
    pub noreturn: bool,

    /// The calling convention of a function type: `__attribute__((ms_abi))`
    /// makes it [`CallingConv::Win64`]. Part of the type, so two function
    /// types differing only here are distinct and incompatible.
    pub conv: CallingConv,

    /// Composite type data (for struct, union, enum)
    pub composite: Option<Box<CompositeType>>,

    /// Explicit alignment from __attribute__((aligned(N))) on typedef.
    /// When set, overrides the natural alignment returned by alignment().
    pub explicit_align: Option<u32>,

    /// For a floating kind, which `_FloatN`/`_FloatNx` name, if any, this
    /// type is; see [`FloatClass`].
    pub float_class: FloatClass,
}

impl Default for Type {
    fn default() -> Self {
        Self {
            kind: TypeKind::Int,
            modifiers: TypeModifiers::empty(),
            base: None,
            array_size: None,
            params: None,
            variadic: false,
            noreturn: false,
            conv: CallingConv::C,
            composite: None,
            explicit_align: None,
            float_class: FloatClass::Standard,
        }
    }
}

impl Type {
    pub fn basic(kind: TypeKind) -> Self {
        Self {
            kind,
            ..Default::default()
        }
    }

    pub fn with_modifiers(kind: TypeKind, modifiers: TypeModifiers) -> Self {
        Self {
            kind,
            modifiers,
            ..Default::default()
        }
    }

    /// Create a pointer type (base type is a TypeId)
    pub fn pointer(base: TypeId) -> Self {
        Self {
            kind: TypeKind::Pointer,
            base: Some(base),
            ..Default::default()
        }
    }

    /// Create an array type (element type is a TypeId)
    pub fn array(base: TypeId, size: usize) -> Self {
        Self {
            kind: TypeKind::Array,
            base: Some(base),
            array_size: Some(size),
            ..Default::default()
        }
    }

    /// Create a function type (return type and param types are TypeIds)
    pub fn function(
        return_type: TypeId,
        params: Vec<TypeId>,
        variadic: bool,
        noreturn: bool,
    ) -> Self {
        Self {
            kind: TypeKind::Function,
            base: Some(return_type),
            params: Some(params),
            variadic,
            noreturn,
            ..Default::default()
        }
    }

    /// Create a function type whose declarator supplied no prototype: `int
    /// f();`, or a K&R identifier list.
    ///
    /// C17 6.7.6.3p14 draws the line here, not at "zero parameters": an empty
    /// identifier list says nothing about the parameters, while `(void)` says
    /// there are none. `params: None` is that distinction, and callers must
    /// read it as "unknown" rather than "empty" -- no arity check is permitted
    /// against it (6.5.2.2p1), and one against `(void)` is required.
    pub fn function_no_prototype(return_type: TypeId, noreturn: bool) -> Self {
        Self {
            kind: TypeKind::Function,
            base: Some(return_type),
            params: None,
            variadic: false,
            noreturn,
            ..Default::default()
        }
    }

    pub fn struct_type(composite: CompositeType) -> Self {
        Self {
            kind: TypeKind::Struct,
            composite: Some(Box::new(composite)),
            ..Default::default()
        }
    }

    pub fn union_type(composite: CompositeType) -> Self {
        Self {
            kind: TypeKind::Union,
            composite: Some(Box::new(composite)),
            ..Default::default()
        }
    }

    pub fn enum_type(composite: CompositeType) -> Self {
        Self {
            kind: TypeKind::Enum,
            composite: Some(Box::new(composite)),
            ..Default::default()
        }
    }

    /// Create an incomplete (forward-declared) struct type
    pub fn incomplete_struct(tag: StringId) -> Self {
        Self::struct_type(CompositeType::incomplete(Some(tag)))
    }

    /// Create an incomplete (forward-declared) union type
    pub fn incomplete_union(tag: StringId) -> Self {
        Self::union_type(CompositeType::incomplete(Some(tag)))
    }

    /// Create an incomplete (forward-declared) enum type
    pub fn incomplete_enum(tag: StringId) -> Self {
        Self::enum_type(CompositeType::incomplete(Some(tag)))
    }

    /// Modifiers that describe a *declaration* rather than a type.
    ///
    /// C17 6.7.1p1 keeps storage-class specifiers out of the type, and the
    /// function specifiers of 6.7.4 likewise qualify the declaration. They ride
    /// along on `Type` because the parser collects all specifiers into one bag,
    /// and they leak into places that only ever wanted the type: the return
    /// type of `static int f(void)` carried `STATIC`, so a call to it was not
    /// compatible with `int`. Invisible to `sizeof`, fatal to any comparison.
    pub const DECL_SPECIFIERS: TypeModifiers = Self::STORAGE_CLASS.union(TypeModifiers::NORETURN);

    /// The type qualifiers of C17 6.7.3p1.
    ///
    /// These are the bits a *type* carries at its top level, as opposed to the
    /// declaration specifiers above: `const int` and `int` are two types, while
    /// `static int` and `int` are one. Four copies of this list had been
    /// spelled out inline -- in `compatible_ignoring_base`, in `compatible`, in
    /// `assign_fault`'s pointer rule and in the parser's
    /// `lvalue_converted_type` -- which is three chances for the set to drift.
    pub const QUALIFIERS: TypeModifiers = TypeModifiers::CONST
        .union(TypeModifiers::VOLATILE)
        .union(TypeModifiers::RESTRICT)
        .union(TypeModifiers::ATOMIC);

    /// The qualifiers a member inherits from the object that holds it.
    ///
    /// C17 6.5.2.3p3/p4 gives `s.m` the "so-qualified version" of the member's
    /// type, which is the member's type plus the qualifiers of `s`. Not all
    /// four travel:
    ///
    /// * `_Atomic` does not. A member of an `_Atomic` struct is not itself
    ///   atomic -- there is no lock-free way to read one out of an atomic
    ///   object -- and gcc does not make it so. Reaching into one at all is
    ///   what `Parser::warn_atomic_member_access` already reports.
    /// * `restrict` cannot: 6.7.3p2 admits it only on a pointer to an object
    ///   type, so a composite never carries it in the first place.
    pub const MEMBER_QUALIFIERS: TypeModifiers =
        TypeModifiers::CONST.union(TypeModifiers::VOLATILE);

    /// The storage-class specifiers of C17 6.7.1, and `inline`.
    ///
    /// What a declaration records as its storage class, and what a declarator
    /// carries from its specifiers onto the type it derives. `inline` is a
    /// function specifier rather than a storage class, but it travels with
    /// them: without it `FunctionDef::is_inline` was false for every ordinary
    /// definition, and a pointer-returning `inline` function -- `memcpy` is
    /// exactly that shape in glibc -- lost the bit.
    pub const STORAGE_CLASS: TypeModifiers = TypeModifiers::STATIC
        .union(TypeModifiers::EXTERN)
        .union(TypeModifiers::REGISTER)
        .union(TypeModifiers::AUTO)
        .union(TypeModifiers::TYPEDEF)
        .union(TypeModifiers::THREAD_LOCAL)
        .union(TypeModifiers::INLINE);

    /// Check if two types are compatible (for __builtin_types_compatible_p)
    /// This ignores top-level qualifiers (const, volatile, restrict) and the
    /// declaration specifiers above, but otherwise requires types to be
    /// identical.
    /// Note: Different enum types are NOT compatible, even if they have
    /// the same underlying integer type.
    ///
    /// With TypeId interning, base types are compared by TypeId equality.
    /// For full recursive comparison, use TypeTable::types_compatible().
    fn compatible_ignoring_base(&self, other: &Type) -> bool {
        // Compare kinds first, and `float` is not `_Float32`.
        if self.kind != other.kind || self.float_class != other.float_class {
            return false;
        }

        // `signed` is redundant on every integer type except char, where
        // `signed char`, `unsigned char` and plain `char` really are three
        // distinct types (C17 6.2.5p15). Elsewhere `signed short` and `short`
        // name the same type, and treating the bit as significant made two
        // spellings of int16_t look incompatible.
        let redundant_signed = if self.kind == TypeKind::Char {
            TypeModifiers::empty()
        } else {
            TypeModifiers::SIGNED
        };
        // `short`, `long` and `long long` are redundant for the same reason,
        // and one step further: the size they name is already the `kind` this
        // function compared first. Two types with the same kind cannot differ
        // in size, so the bits distinguish nothing -- while the canonical
        // interned types carry none of them and a parsed specifier list
        // carries all of them. That mismatch is why `_Generic(1L, long: ...)`
        // matched no association and why a diagnostic could print both sides
        // as `long` while calling them incompatible.
        const REDUNDANT_SIZE: TypeModifiers = TypeModifiers::SHORT
            .union(TypeModifiers::LONG)
            .union(TypeModifiers::LONGLONG);
        // `__builtin_ms_va_list` is a `char *` to everything but `va_arg`,
        // and a vector comparison's mask the vector of its shape.
        let ignored = Self::QUALIFIERS
            .union(redundant_signed)
            .union(REDUNDANT_SIZE)
            .union(Self::DECL_SPECIFIERS)
            .union(TypeModifiers::MS_VA_LIST)
            .union(TypeModifiers::VECTOR_MASK);

        // Compare modifiers (ignoring top-level qualifiers)
        let self_mods = self.modifiers.difference(ignored);
        let other_mods = other.modifiers.difference(ignored);
        if self_mods != other_mods {
            return false;
        }

        // Compare array sizes.
        //
        // C17 6.7.6.2p6: two array types are compatible if their element types
        // are, and *both* have a constant size, in which case the sizes must
        // agree. A side with no extent -- an incomplete `int[]`, or a variably
        // modified `int[n]`, which are the same thing to the type table --
        // imposes no size requirement, so it is compatible with either.
        //
        // Requiring equality made a legal call diagnosed: `int a[n][m]` as a
        // parameter decays to `int (*)[m]`, whose pointee has no extent, and
        // passing an ordinary `int m[2][2]` gave "passing argument 3 as
        // 'int[]*' from 'int[2]*' incompatible pointer type" where gcc is
        // silent even under -Wall.
        match (self.array_size, other.array_size) {
            (Some(a), Some(b)) if a != b => return false,
            _ => {}
        }

        // Compare variadic flag. A function type without a prototype says
        // nothing about its parameters, so the caller settles one against a
        // prototype.
        if self.params.is_some() && other.params.is_some() && self.variadic != other.variadic {
            return false;
        }

        // A function type's calling convention is part of it: gcc calls
        // `long (*)(long)` and `long (__attribute__((ms_abi)) *)(long)`
        // incompatible, and a redeclaration that changes it conflicting.
        if self.conv != other.conv {
            return false;
        }

        // The base is compared by the caller, which can recurse. Comparing it
        // here by TypeId called two structurally identical types different
        // whenever a declaration specifier had settled on an inner type:
        // `static char *x[]` records STATIC on the element, so assigning it to
        // a plain `char **` field reported "'char**' from 'char**'" -- a
        // complaint about two spellings of one type. Storage class is a
        // property of a declaration, never of a type.

        // Only the *shape* of two prototypes is settled here: how many
        // parameters each has. The parameter types themselves are compared by
        // the caller, which can recurse -- comparing them by TypeId called two
        // identical `void *(Parser *)` different whenever a declaration
        // specifier had settled on an inner type, which is 19 lines of
        // CPython's generated parser. So is a prototype against a function
        // type without one.
        if let (Some(a), Some(b)) = (&self.params, &other.params) {
            if a.len() != b.len() {
                return false;
            }
        }

        // Compare composite types (struct, union, enum)
        match (&self.composite, &other.composite) {
            (Some(a), Some(b)) => {
                // A tag names the type. C17 6.2.7p1: completing a
                // forward-declared struct does not create a second type, so
                // `struct S;` and the later `struct S { int x; }` are one --
                // but they are two `CompositeType` values, one incomplete with
                // no members, and comparing those structurally called every
                // function declared before the definition and defined after it
                // a conflicting redeclaration.
                //
                // Different enum types stay incompatible even with the same
                // underlying type, because their tags differ.
                match (a.tag, b.tag) {
                    // One side still incomplete: this is a tag meeting its own
                    // definition, so the tag is all there is to go on.
                    (Some(x), Some(y)) if !a.is_complete || !b.is_complete => x == y,
                    // Both complete: the tag is necessary but not sufficient,
                    // because a nested scope may declare a different type under
                    // the same name. Comparing what they contain tells them
                    // apart, and this predicate feeds accept/reject decisions
                    // now, not only diagnostics.
                    (Some(x), Some(y)) => x == y && a == b,
                    // Anonymous composites have nothing to name them, so they
                    // are compared by what they contain.
                    _ => a == b,
                }
            }
            (None, None) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Print modifiers
        if self.modifiers.contains(TypeModifiers::CONST) {
            write!(f, "const ")?;
        }
        if self.modifiers.contains(TypeModifiers::VOLATILE) {
            write!(f, "volatile ")?;
        }
        if self.modifiers.contains(TypeModifiers::ATOMIC) {
            write!(f, "_Atomic ")?;
        }
        if self.modifiers.contains(TypeModifiers::UNSIGNED) {
            write!(f, "unsigned ")?;
        } else if self.modifiers.contains(TypeModifiers::SIGNED) && self.kind == TypeKind::Char {
            write!(f, "signed ")?;
        }

        // Note: With TypeId, we can't recursively print base types without TypeTable access
        // For debugging, we just show the TypeId value
        match self.kind {
            TypeKind::Pointer => {
                if let Some(base) = self.base {
                    write!(f, "T{}*", base.0)
                } else {
                    write!(f, "*")
                }
            }
            TypeKind::Array => {
                if let Some(base) = self.base {
                    if let Some(size) = self.array_size {
                        write!(f, "T{}[{}]", base.0, size)
                    } else {
                        write!(f, "T{}[]", base.0)
                    }
                } else {
                    write!(f, "[]")
                }
            }
            TypeKind::Function => {
                if let Some(ret) = self.base {
                    write!(f, "T{}(", ret.0)?;
                    if let Some(params) = &self.params {
                        for (i, param) in params.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "T{}", param.0)?;
                        }
                        if self.variadic {
                            if !params.is_empty() {
                                write!(f, ", ")?;
                            }
                            write!(f, "...")?;
                        }
                    }
                    write!(f, ")")
                } else {
                    write!(f, "()")
                }
            }
            _ => write!(f, "{}", self.kind),
        }
    }
}

// Type Table - Interned type storage and query methods

/// Key for type lookup/deduplication (hashable representation)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TypeKey {
    /// Basic type: kind + modifiers + floating class
    Basic(TypeKind, u32, FloatClass),
    /// Pointer to interned type
    Pointer(TypeId, u32), // base_id, modifiers
    /// Array of interned type
    Array(TypeId, Option<usize>, u32), // base_id, size, modifiers
    /// Function type
    Function {
        ret: TypeId,
        /// `None` for a declarator with no prototype, which is a *different*
        /// type from one taking no parameters (C17 6.7.6.3p14). Flattening the
        /// two with `unwrap_or_default` interned `int f()` and `int f(void)`
        /// as the same `TypeId`, so no later pass could tell them apart.
        params: Option<Vec<TypeId>>,
        variadic: bool,
        noreturn: bool,
        conv: CallingConv,
        modifiers: u32,
    },
}

/// Why a pair of operands fails the simple-assignment constraints of C17
/// 6.5.16.1p1.
///
/// The split into fatal and non-fatal follows gcc: a conversion that does not
/// exist at all is an error, while one that exists but is almost certainly a
/// mistake is a warning. Matching that split is what lets code which builds
/// today keep building.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignFault {
    /// No conversion exists: a pointer against a floating type, an aggregate
    /// against anything but a compatible aggregate, or a `void` value.
    Incompatible,
    /// A pointer where an integer belongs, with no cast.
    IntegerFromPointer,
    /// An integer where a pointer belongs, with no cast. Kept apart from
    /// `IntegerFromPointer` because the two read in opposite directions and a
    /// single message for both names the wrong conversion half the time.
    PointerFromInteger,
    /// Two pointers whose referenced types are not compatible.
    PointerMismatch,
    /// The target's referenced type drops a qualifier the source's had.
    QualifierDiscard,
    /// A function pointer against `void *`. The 6.5.16.1p1 carve-out is for a
    /// pointer to an *object* type, so this is a constraint violation -- but
    /// one POSIX requires to work, since `dlsym` returns `void *` and every
    /// caller assigns it to a function pointer.
    FunctionPointerVoid,
}

/// The `-Wno-<name>` group the function-pointer/`void *` warnings belong to.
///
/// Named rather than fatal because gcc accepts the conversion in silence and
/// only `-pedantic` objects; diagnosing it at all is stricter than the
/// compiler c17 matches by policy, so it has to be silenceable by anyone who
/// calls `dlsym` in a loop.
pub const FUNCTION_POINTER_CONV: &str = "function-pointer-conv";

impl AssignFault {
    /// Whether this fault is fatal. Only a missing conversion is.
    pub fn is_error(self) -> bool {
        self == AssignFault::Incompatible
    }

    /// The clause naming what went wrong, as gcc words it.
    pub fn describe(self) -> &'static str {
        match self {
            AssignFault::Incompatible => "incompatible types",
            AssignFault::IntegerFromPointer => "makes integer from pointer without a cast",
            AssignFault::PointerFromInteger => "makes pointer from integer without a cast",
            AssignFault::PointerMismatch => "incompatible pointer type",
            AssignFault::QualifierDiscard => "discards a qualifier from the pointer target type",
            AssignFault::FunctionPointerVoid => "converts between a function pointer and 'void *'",
        }
    }
}

/// Whether a comparison treats the outermost `const`/`volatile`/`restrict`/
/// `_Atomic` as part of the type.
///
/// They are not part of it for a whole object -- `const int` and `int` are the
/// same type to `__builtin_types_compatible_p` -- but they are for anything a
/// pointer or array refers to (C17 6.7.6.1p2), and are not for a parameter
/// (6.7.6.3p15).
#[derive(Clone, Copy, PartialEq, Eq)]
enum TopLevelQualifiers {
    Ignored,
    Significant,
}

/// Whether a comparison lets an enumerated type match its integer type.
///
/// For compatibility it does: C17 6.7.2.2p4 makes every enumerated type
/// compatible with one integer type. A typedef, though, may be redefined only
/// to denote the *same* type (6.7p3), and an enum is not the same type as the
/// integer it is compatible with -- gcc rejects `typedef enum E T; typedef
/// unsigned T;`.
#[derive(Clone, Copy, PartialEq, Eq)]
enum EnumMatch {
    Integer,
    Distinct,
}

const DEFAULT_TYPE_TABLE_CAPACITY: usize = 65536;

/// Type table - stores all types and provides ID-based lookup
/// Pattern follows IdentTable in token/lexer.rs
pub struct TypeTable {
    /// All interned types (indexed by TypeId)
    types: Vec<Type>,
    /// Lookup map for deduplication
    lookup: HashMap<TypeKey, TypeId>,
    /// The last identity [`TypeTable::fresh_anon_id`] handed out.
    next_anon_id: u32,
    /// The qualified copies of each incomplete tagged type, by the tag's own
    /// `TypeId`; see [`CompositeType::tag_type`].
    forward_copies: HashMap<TypeId, Vec<TypeId>>,
    /// Pointer size in bits (target-dependent, defaults to 64 for LP64)
    pointer_width: u32,
    /// Target architecture for runtime type size calculations
    target_arch: Arch,
    /// Target OS for runtime type size calculations
    target_os: Os,
    /// Plain `char`'s signedness on the target (C17 6.2.5p15), as
    /// [`CharSignedness::of`] decides it. Copied from `Target` at construction
    /// rather than recomputed, because the table cannot reconstruct the
    /// *requested* target -- `has_float128` rebuilds one with
    /// `..Target::host()` and would answer for the host instead.
    plain_char: CharSignedness,

    // Pre-computed common type IDs for fast access
    pub void_id: TypeId,
    pub bool_id: TypeId,
    pub char_id: TypeId,
    pub schar_id: TypeId,
    pub uchar_id: TypeId,
    pub short_id: TypeId,
    pub ushort_id: TypeId,
    pub int_id: TypeId,
    pub uint_id: TypeId,
    pub long_id: TypeId,
    pub ulong_id: TypeId,
    pub longlong_id: TypeId,
    pub ulonglong_id: TypeId,
    pub int128_id: TypeId,
    pub uint128_id: TypeId,
    pub float_id: TypeId,
    pub double_id: TypeId,
    pub longdouble_id: TypeId,
    pub float16_id: TypeId,
    pub float128_id: TypeId,
    /// `wchar_t`, `char16_t` and `char32_t`: the types of the `L`, `u` and
    /// `U` prefixed literals, as [`Target`] decides them. Each is one of the
    /// integer types above, not a type of its own.
    pub wchar_id: TypeId,
    pub char16_id: TypeId,
    pub char32_id: TypeId,
    /// Every arithmetic base type paired with its `_Complex` counterpart.
    ///
    /// [`Self::make_complex`] takes `&self`, so it cannot intern on demand,
    /// and the named `complex_*_id` fields cover only the five floating bases.
    /// GNU complex integers need thirteen more, and naming each of them would
    /// spread the same lookup over eighteen fields.
    complex_of: std::collections::HashMap<TypeId, TypeId>,
    pub complex_float_id: TypeId,
    pub complex_double_id: TypeId,
    pub complex_longdouble_id: TypeId,
    pub complex_float16_id: TypeId,
    pub complex_float128_id: TypeId,
    pub void_ptr_id: TypeId,
    /// `const void *`, the source operand of `memcpy` and `memmove`.
    pub const_void_ptr_id: TypeId,
    pub char_ptr_id: TypeId,
    /// `const char *`, the string operand of `<string.h>` and `<stdio.h>`.
    pub const_char_ptr_id: TypeId,
    /// `volatile void *`, the flag operand of `__atomic_test_and_set`.
    pub volatile_void_ptr_id: TypeId,
    /// `const volatile void *`, the object operand of the lock-free queries.
    pub const_volatile_void_ptr_id: TypeId,
    /// `int *`, `float *`, `double *` and `long double *`: the out
    /// parameters of `frexp` and `modf`.
    pub int_ptr_id: TypeId,
    pub float_ptr_id: TypeId,
    pub double_ptr_id: TypeId,
    pub longdouble_ptr_id: TypeId,
    /// `__builtin_va_list`, the last parameter of `vprintf` and its siblings.
    pub va_list_id: TypeId,
    /// `__builtin_ms_va_list`: the `char *` variant
    /// [`TypeModifiers::MS_VA_LIST`] marks.
    pub ms_va_list_id: TypeId,
}

/// Parenthesize a declarator that has reached a `*` before an array or
/// function suffix is appended, because a pointer binds looser than either.
/// `int (*)[8]` is a pointer to an array; `int *[8]` is an array of pointers.
fn parenthesize_if_pointer(decl: String) -> String {
    if decl.contains('*') {
        format!("({})", decl)
    } else {
        decl
    }
}

impl TypeTable {
    /// Create a new type table with common types pre-interned
    pub fn new(target: &Target) -> Self {
        let mut table = Self {
            types: Vec::with_capacity(DEFAULT_TYPE_TABLE_CAPACITY),
            lookup: HashMap::with_capacity(DEFAULT_TYPE_TABLE_CAPACITY),
            next_anon_id: 0,
            forward_copies: HashMap::new(),
            pointer_width: target.pointer_width,
            target_arch: target.arch,
            target_os: target.os,
            plain_char: target.plain_char,
            void_id: TypeId::INVALID,
            bool_id: TypeId::INVALID,
            char_id: TypeId::INVALID,
            schar_id: TypeId::INVALID,
            uchar_id: TypeId::INVALID,
            short_id: TypeId::INVALID,
            ushort_id: TypeId::INVALID,
            int_id: TypeId::INVALID,
            uint_id: TypeId::INVALID,
            long_id: TypeId::INVALID,
            ulong_id: TypeId::INVALID,
            longlong_id: TypeId::INVALID,
            ulonglong_id: TypeId::INVALID,
            int128_id: TypeId::INVALID,
            uint128_id: TypeId::INVALID,
            float_id: TypeId::INVALID,
            double_id: TypeId::INVALID,
            longdouble_id: TypeId::INVALID,
            float16_id: TypeId::INVALID,
            float128_id: TypeId::INVALID,
            wchar_id: TypeId::INVALID,
            char16_id: TypeId::INVALID,
            char32_id: TypeId::INVALID,
            complex_of: std::collections::HashMap::new(),
            complex_float_id: TypeId::INVALID,
            complex_double_id: TypeId::INVALID,
            complex_longdouble_id: TypeId::INVALID,
            complex_float16_id: TypeId::INVALID,
            complex_float128_id: TypeId::INVALID,
            void_ptr_id: TypeId::INVALID,
            const_void_ptr_id: TypeId::INVALID,
            char_ptr_id: TypeId::INVALID,
            const_char_ptr_id: TypeId::INVALID,
            volatile_void_ptr_id: TypeId::INVALID,
            const_volatile_void_ptr_id: TypeId::INVALID,
            int_ptr_id: TypeId::INVALID,
            float_ptr_id: TypeId::INVALID,
            double_ptr_id: TypeId::INVALID,
            longdouble_ptr_id: TypeId::INVALID,
            va_list_id: TypeId::INVALID,
            ms_va_list_id: TypeId::INVALID,
        };

        // Pre-intern common basic types
        table.void_id = table.intern(Type::basic(TypeKind::Void));
        table.bool_id = table.intern(Type::basic(TypeKind::Bool));
        table.char_id = table.intern(Type::basic(TypeKind::Char));
        table.schar_id = table.intern(Type::with_modifiers(TypeKind::Char, TypeModifiers::SIGNED));
        table.uchar_id = table.intern(Type::with_modifiers(
            TypeKind::Char,
            TypeModifiers::UNSIGNED,
        ));
        table.short_id = table.intern(Type::basic(TypeKind::Short));
        table.ushort_id = table.intern(Type::with_modifiers(
            TypeKind::Short,
            TypeModifiers::UNSIGNED,
        ));
        table.int_id = table.intern(Type::basic(TypeKind::Int));
        table.uint_id = table.intern(Type::with_modifiers(TypeKind::Int, TypeModifiers::UNSIGNED));
        table.long_id = table.intern(Type::basic(TypeKind::Long));
        table.ulong_id = table.intern(Type::with_modifiers(
            TypeKind::Long,
            TypeModifiers::UNSIGNED,
        ));
        table.longlong_id = table.intern(Type::basic(TypeKind::LongLong));
        table.ulonglong_id = table.intern(Type::with_modifiers(
            TypeKind::LongLong,
            TypeModifiers::UNSIGNED,
        ));
        table.int128_id = table.intern(Type::basic(TypeKind::Int128));
        table.uint128_id = table.intern(Type::with_modifiers(
            TypeKind::Int128,
            TypeModifiers::UNSIGNED,
        ));
        table.float_id = table.intern(Type::basic(TypeKind::Float));
        table.double_id = table.intern(Type::basic(TypeKind::Double));
        table.longdouble_id = table.intern(Type::basic(TypeKind::LongDouble));
        table.float16_id = table.intern(Type::basic(TypeKind::Float16));
        table.float128_id = table.intern(Type::basic(TypeKind::Float128));
        table.wchar_id = table.int_type_id(target.wchar_type());
        table.char16_id = table.int_type_id(target.char16_type());
        table.char32_id = table.int_type_id(target.char32_type());

        // Pre-intern complex types
        table.complex_float_id = table.intern(Type::with_modifiers(
            TypeKind::Float,
            TypeModifiers::COMPLEX,
        ));
        table.complex_double_id = table.intern(Type::with_modifiers(
            TypeKind::Double,
            TypeModifiers::COMPLEX,
        ));
        table.complex_longdouble_id = table.intern(Type::with_modifiers(
            TypeKind::LongDouble,
            TypeModifiers::COMPLEX,
        ));
        table.complex_float16_id = table.intern(Type::with_modifiers(
            TypeKind::Float16,
            TypeModifiers::COMPLEX,
        ));
        table.complex_float128_id = table.intern(Type::with_modifiers(
            TypeKind::Float128,
            TypeModifiers::COMPLEX,
        ));

        // Pre-intern the GNU complex integer types and record every
        // base/complex pair, so `make_complex` and `complex_base` are exact
        // inverses for both families.
        let bases = [
            table.char_id,
            table.schar_id,
            table.uchar_id,
            table.short_id,
            table.ushort_id,
            table.int_id,
            table.uint_id,
            table.long_id,
            table.ulong_id,
            table.longlong_id,
            table.ulonglong_id,
            table.int128_id,
            table.uint128_id,
        ];
        for base in bases {
            let typ = table.get(base);
            let cplx = table.intern(Type::with_modifiers(
                typ.kind,
                typ.modifiers | TypeModifiers::COMPLEX,
            ));
            table.complex_of.insert(base, cplx);
        }
        // `_Float32`, `_Float64`, `_Float32x` and `_Float64x`, real and
        // complex, so `floating` can answer through `&self`.
        for (kind, class) in [
            (TypeKind::Float, FloatClass::Interchange),
            (TypeKind::Double, FloatClass::Interchange),
            (TypeKind::Double, FloatClass::Extended),
            (TypeKind::LongDouble, FloatClass::Extended),
        ] {
            let real = table.intern(Type {
                float_class: class,
                ..Type::basic(kind)
            });
            let cplx = table.intern(Type {
                float_class: class,
                ..Type::with_modifiers(kind, TypeModifiers::COMPLEX)
            });
            table.complex_of.insert(real, cplx);
        }
        for (real, cplx) in [
            (table.float_id, table.complex_float_id),
            (table.double_id, table.complex_double_id),
            (table.longdouble_id, table.complex_longdouble_id),
            (table.float16_id, table.complex_float16_id),
            (table.float128_id, table.complex_float128_id),
        ] {
            table.complex_of.insert(real, cplx);
        }

        // Pre-intern common pointer types
        table.void_ptr_id = table.intern(Type::pointer(table.void_id));
        let const_void = table.intern(Type::with_modifiers(TypeKind::Void, TypeModifiers::CONST));
        table.const_void_ptr_id = table.intern(Type::pointer(const_void));
        table.char_ptr_id = table.intern(Type::pointer(table.char_id));
        let const_char = table.intern(Type::with_modifiers(TypeKind::Char, TypeModifiers::CONST));
        table.const_char_ptr_id = table.intern(Type::pointer(const_char));
        let volatile_void = table.intern(Type::with_modifiers(
            TypeKind::Void,
            TypeModifiers::VOLATILE,
        ));
        table.volatile_void_ptr_id = table.intern(Type::pointer(volatile_void));
        let const_volatile_void = table.intern(Type::with_modifiers(
            TypeKind::Void,
            TypeModifiers::CONST | TypeModifiers::VOLATILE,
        ));
        table.const_volatile_void_ptr_id = table.intern(Type::pointer(const_volatile_void));
        table.int_ptr_id = table.intern(Type::pointer(table.int_id));
        table.float_ptr_id = table.intern(Type::pointer(table.float_id));
        table.double_ptr_id = table.intern(Type::pointer(table.double_id));
        table.longdouble_ptr_id = table.intern(Type::pointer(table.longdouble_id));
        table.va_list_id = table.intern(Type::basic(TypeKind::VaList));
        table.ms_va_list_id = table.intern(Type {
            modifiers: TypeModifiers::MS_VA_LIST,
            ..Type::pointer(table.char_id)
        });

        table
    }

    /// The type a [`Target`]'s ABI choice names.
    pub fn int_type_id(&self, t: IntType) -> TypeId {
        match t {
            IntType::SChar => self.schar_id,
            IntType::UChar => self.uchar_id,
            IntType::Short => self.short_id,
            IntType::UShort => self.ushort_id,
            IntType::Int => self.int_id,
            IntType::UInt => self.uint_id,
            IntType::Long => self.long_id,
            IntType::ULong => self.ulong_id,
            IntType::LongLong => self.longlong_id,
            IntType::ULongLong => self.ulonglong_id,
        }
    }

    /// The unsigned integer type `bytes` wide, for moving an object's bits
    /// as a value; `None` for a width with no such type.
    pub fn unsigned_of_size(&self, bytes: usize) -> Option<TypeId> {
        match bytes {
            1 => Some(self.uchar_id),
            2 => Some(self.ushort_id),
            4 => Some(self.uint_id),
            8 => Some(self.ulong_id),
            _ => None,
        }
    }

    /// A fresh identity for a tagless composite definition; see
    /// [`CompositeType::anon_id`].
    pub fn fresh_anon_id(&mut self) -> u32 {
        self.next_anon_id += 1;
        self.next_anon_id
    }

    /// Intern a type, returning its unique ID.
    /// Deduplicates equivalent types (same ID for equivalent types).
    pub fn intern(&mut self, mut typ: Type) -> TypeId {
        // Try to create a key for deduplication
        if let Some(key) = self.make_key(&typ) {
            if let Some(&existing_id) = self.lookup.get(&key) {
                return existing_id;
            }
            let id = TypeId(self.types.len() as u32);
            self.types.push(typ);
            self.lookup.insert(key, id);
            id
        } else {
            // Types with composite data (structs) are not deduplicated
            let id = TypeId(self.types.len() as u32);
            self.note_tag_type(&mut typ, id);
            self.types.push(typ);
            id
        }
    }

    /// Tie a tagged type being interned as `id` to the tag it stands for:
    /// the first one interned is the tag's own type, and every later one is a
    /// copy of it. A copy of an incomplete tag is recorded, so that the tag's
    /// definition completes it too. See [`CompositeType::tag_type`].
    fn note_tag_type(&mut self, typ: &mut Type, id: TypeId) {
        let Some(composite) = typ.composite.as_deref_mut() else {
            return;
        };
        if composite.tag.is_none() {
            return;
        }
        let tag_type = *composite.tag_type.get_or_insert(id);
        if tag_type != id && !composite.is_complete {
            self.forward_copies.entry(tag_type).or_default().push(id);
        }
    }

    /// Create lookup key for deduplication (None for non-deduplicatable types)
    fn make_key(&self, typ: &Type) -> Option<TypeKey> {
        // Don't deduplicate types with composite data (structs/unions/enums have identity)
        if typ.composite.is_some() {
            return None;
        }
        // Don't deduplicate types with explicit alignment (typedef aligned types are unique)
        if typ.explicit_align.is_some() {
            return None;
        }

        match typ.kind {
            TypeKind::Pointer => {
                let base = typ.base?;
                Some(TypeKey::Pointer(base, typ.modifiers.bits()))
            }
            TypeKind::Array => {
                let base = typ.base?;
                Some(TypeKey::Array(base, typ.array_size, typ.modifiers.bits()))
            }
            TypeKind::Function => {
                let ret = typ.base?;
                Some(TypeKey::Function {
                    ret,
                    params: typ.params.clone(),
                    variadic: typ.variadic,
                    noreturn: typ.noreturn,
                    conv: typ.conv,
                    modifiers: typ.modifiers.bits(),
                })
            }
            _ => Some(TypeKey::Basic(
                typ.kind,
                typ.modifiers.bits(),
                typ.float_class,
            )),
        }
    }

    /// Get a type by ID (returns reference)
    pub fn get(&self, id: TypeId) -> &Type {
        &self.types[id.0 as usize]
    }

    /// Complete an incomplete struct/union type with its full definition.
    /// This updates the type in place so that all existing pointers to
    /// the incomplete type will now see the complete type.
    pub fn complete_struct(&mut self, id: TypeId, composite: CompositeType) {
        debug_assert!(
            matches!(self.kind(id), TypeKind::Struct | TypeKind::Union),
            "complete_struct called on non-struct/union type"
        );
        self.complete_tag(id, composite, TypeModifiers::empty());
    }

    /// Complete a forward-declared enum with its definition, in place, as
    /// [`Self::complete_struct`] does a struct: every typedef, member and
    /// pointee declared through the forward reference sees the enum's size,
    /// and its signedness when the underlying type is unsigned.
    pub fn complete_enum(&mut self, id: TypeId, composite: CompositeType, unsigned: bool) {
        debug_assert!(
            self.kind(id) == TypeKind::Enum,
            "complete_enum called on a non-enum type"
        );
        let sign = if unsigned {
            TypeModifiers::UNSIGNED
        } else {
            TypeModifiers::empty()
        };
        self.complete_tag(id, composite, sign);
    }

    /// Give the tag's type `id`, and every qualified copy of it, the
    /// definition's composite and `added` modifiers. Each copy keeps its own
    /// qualifiers.
    fn complete_tag(&mut self, id: TypeId, composite: CompositeType, added: TypeModifiers) {
        let tag_type = self.composite(id).and_then(|c| c.tag_type).unwrap_or(id);
        let copies = self.forward_copies.remove(&tag_type).unwrap_or_default();
        let composite = CompositeType {
            tag_type: Some(tag_type),
            ..composite
        };
        for copy in std::iter::once(tag_type).chain(copies) {
            let typ = &mut self.types[copy.0 as usize];
            typ.composite = Some(Box::new(composite.clone()));
            typ.modifiers |= added;
        }
    }

    // Type query methods (moved from Type to TypeTable)

    pub fn kind(&self, id: TypeId) -> TypeKind {
        self.get(id).kind
    }

    pub fn modifiers(&self, id: TypeId) -> TypeModifiers {
        self.get(id).modifiers
    }

    /// Get the base type ID (for pointers, arrays, functions)
    pub fn base_type(&self, id: TypeId) -> Option<TypeId> {
        self.get(id).base
    }

    /// The type `id` denotes, without the [`Type::DECL_SPECIFIERS`] of the
    /// declaration it was read from.
    ///
    /// The parser records a declaration's storage class on its base type and
    /// copies it onto the declarator's derived type, so `static int *p` is a
    /// `static` pointer to a `static int`, and the bits sit at every level a
    /// later derivation can reach: an element (`a[i]`), a pointee (`*p`), a
    /// return type. Anything that takes the type of an existing declaration
    /// -- `typeof`, a redeclaration check -- must come through here, or it
    /// inherits the other declaration's storage class along with its type.
    pub fn without_decl_specifiers(&mut self, id: TypeId) -> TypeId {
        let t = self.get(id);
        let base = t.base;
        let params = t.params.clone();
        let new_base = base.map(|b| self.without_decl_specifiers(b));
        let new_params = params.as_ref().map(|ps| {
            ps.iter()
                .map(|&p| self.without_decl_specifiers(p))
                .collect::<Vec<_>>()
        });
        let t = self.get(id);
        if !t.modifiers.intersects(Type::DECL_SPECIFIERS)
            && new_base == base
            && new_params == params
        {
            return id;
        }
        let mut stripped = t.clone();
        stripped.modifiers.remove(Type::DECL_SPECIFIERS);
        stripped.base = new_base;
        stripped.params = new_params;
        self.intern(stripped)
    }

    /// Look up an existing pointer type to the given base type
    /// Returns void_ptr_id if not found (since all pointers are same size)
    pub fn pointer_to(&self, base: TypeId) -> TypeId {
        // Look for an existing pointer to this base type
        let key = TypeKey::Pointer(base, 0); // No modifiers
        if let Some(&id) = self.lookup.get(&key) {
            return id;
        }
        // All pointers are same size, so void* works as fallback
        self.void_ptr_id
    }

    // Type-shape accessors

    pub fn array_size(&self, id: TypeId) -> Option<usize> {
        self.get(id).array_size
    }

    /// Has this struct or union been defined, as opposed to merely declared?
    ///
    /// A forward declaration interns a composite marked incomplete, so a
    /// `struct U;` never followed by a definition answers false here and its
    /// size may not be taken. Any other kind answers true: the question is
    /// only meaningful for a composite.
    pub fn is_composite_complete(&self, id: TypeId) -> bool {
        self.get(id)
            .composite
            .as_deref()
            .is_none_or(|c| c.is_complete)
    }

    /// Whether `member` is a flexible array member: an array with no bound,
    /// which C17 6.7.2.1p18 allows only as the last member of a structure.
    pub fn is_flexible_array_member(&self, member: &StructMember) -> bool {
        member.bit_width.is_none() && self.unsized_array_levels(member.typ) > 0
    }

    /// Whether an object of type `id` ends in storage with no bound: it is an
    /// array of unknown size, or a structure whose flexible array member is
    /// last.
    pub fn has_unbounded_tail(&self, id: TypeId) -> bool {
        match self.kind(id) {
            TypeKind::Array => self.get(id).array_size.is_none(),
            TypeKind::Struct => self
                .get(id)
                .composite
                .as_ref()
                .and_then(|c| c.members.last())
                .is_some_and(|m| self.is_flexible_array_member(m)),
            _ => false,
        }
    }

    /// How many array levels of `id`, outermost-first, have no extent.
    ///
    /// The type table cannot tell a variably-modified array from an incomplete
    /// one: `int[n]`, `int[m]` and `int[]` all intern to the same `TypeId`,
    /// because the key holds `array_size`, which is `None` for all three. So
    /// this counts the levels that *need* a size expression supplied from
    /// outside, which is exactly what `record_vm_extents` consumes one
    /// expression for -- and therefore also the count that says whether a
    /// given list of expressions describes the whole type or only part of it.
    ///
    /// Stops at the first non-array level, so a pointer to a variably-modified
    /// array counts zero: its size is the pointer's.
    pub fn unsized_array_levels(&self, id: TypeId) -> usize {
        let mut levels = 0;
        let mut cur = id;
        while self.kind(cur) == TypeKind::Array {
            let typ = self.get(cur);
            if typ.array_size.is_none() {
                levels += 1;
            }
            match typ.base {
                Some(base) => cur = base,
                None => break,
            }
        }
        levels
    }

    // The two below are read by `parse::test_parser` and by nothing in the
    // compiler, which reaches a signature through `Type::func_params` and
    // `Type::variadic` directly. They stay gated so that stays visible.

    /// Get function parameters
    #[cfg(test)]
    pub fn params(&self, id: TypeId) -> Option<&Vec<TypeId>> {
        self.get(id).params.as_ref()
    }

    #[cfg(test)]
    pub fn is_variadic(&self, id: TypeId) -> bool {
        self.get(id).variadic
    }

    /// Get composite type data (for struct/union/enum)
    pub fn composite(&self, id: TypeId) -> Option<&CompositeType> {
        self.get(id).composite.as_deref()
    }

    /// Mark a union `__attribute__((transparent_union))`.
    ///
    /// In place, like `complete_struct`: glibc spells the attribute on a
    /// typedef of an *anonymous* union, so re-interning a clone would hand
    /// back a fresh `TypeId` that every existing reference would then have to
    /// match structurally. Composites are never deduplicated, so the identity
    /// this preserves is the one the rest of the compiler already relies on.
    pub fn set_transparent_union(&mut self, id: TypeId) {
        if let Some(c) = self.types[id.0 as usize].composite.as_deref_mut() {
            c.transparent = true;
        }
    }

    /// The type an argument to a `transparent_union` parameter is really
    /// passed as: its **first declared member**.
    ///
    /// `None` for anything that is not a transparent union, which is what
    /// makes this usable as the guard at every call site.
    ///
    /// Unnamed zero-width bit-fields are skipped. They are not declared
    /// members in the sense gcc's rule means, and `CompositeType::members` is
    /// unfiltered, so one would otherwise decide the whole ABI of the union.
    pub fn transparent_union_first_member(&self, id: TypeId) -> Option<TypeId> {
        if self.kind(id) != TypeKind::Union {
            return None;
        }
        let comp = self.composite(id)?;
        if !comp.transparent {
            return None;
        }
        comp.members
            .iter()
            .find(|m| m.bit_width != Some(0))
            .map(|m| m.typ)
    }

    /// The member a GNU cast to union type `id` initializes from an operand
    /// of type `operand`, which the caller has already lvalue-converted.
    ///
    /// gcc selects the first member whose type is compatible with the
    /// operand's, qualifiers ignored. Nothing converts the operand to find a
    /// match: a `long` selects no `int` member. A bit-field never matches,
    /// and neither can an unnamed member, which no designator could name.
    /// `None` when nothing matches, or `id` is not a union.
    pub fn union_member_for_cast(&self, id: TypeId, operand: TypeId) -> Option<StringId> {
        if self.kind(id) != TypeKind::Union {
            return None;
        }
        self.composite(id)?
            .members
            .iter()
            .filter(|m| m.bit_width.is_none() && m.name != StringId::EMPTY)
            .find(|m| self.types_compatible(m.typ, operand))
            .map(|m| m.name)
    }

    /// Format a type for display (with recursive base type printing).
    ///
    /// Used by diagnostics that have to name the types they are complaining
    /// about -- an assignment constraint violation is unreadable without them.
    pub fn format_type(&self, id: TypeId, idents: Option<&IdentTable>) -> String {
        self.format_declarator(id, String::new(), idents)
    }

    /// Spell `id` in declarator form, wrapping `decl` -- the declarator built
    /// so far, read outward from where the name would stand.
    ///
    /// A C type reads inside-out, so a pointer binds looser than an array or
    /// function suffix: a declarator that has reached a `*` is parenthesized
    /// before a suffix is appended to it. gcc spells a pointer to `int[8]` as
    /// `int (*)[8]`; built left to right it would come out `int[8] *`, which
    /// reads as "array of pointers" -- the other type entirely.
    fn format_declarator(&self, id: TypeId, decl: String, idents: Option<&IdentTable>) -> String {
        let typ = self.get(id);

        match typ.kind {
            TypeKind::Pointer if typ.modifiers.contains(TypeModifiers::MS_VA_LIST) => {
                let mut name = String::from("__builtin_ms_va_list");
                if !decl.is_empty() {
                    name.push(' ');
                    name.push_str(&decl);
                }
                name
            }

            TypeKind::Pointer => {
                // Qualifiers on the pointer belong after the star -- the
                // `const` in `char *const` qualifies the pointer, not the char.
                let mut inner = String::from("*");
                if typ.modifiers.contains(TypeModifiers::CONST) {
                    inner.push_str(" const");
                }
                if typ.modifiers.contains(TypeModifiers::VOLATILE) {
                    inner.push_str(" volatile");
                }
                if !decl.is_empty() {
                    // `**`, never `* *`; but `*const p` needs its space.
                    if !inner.ends_with('*') {
                        inner.push(' ');
                    }
                    inner.push_str(&decl);
                }
                match typ.base {
                    Some(base) => self.format_declarator(base, inner, idents),
                    None => inner,
                }
            }

            // gcc's spelling, which names what the type is rather than how
            // it is laid out.
            TypeKind::Array if typ.modifiers.contains(TypeModifiers::VECTOR) => {
                let mut name = String::new();
                if typ.modifiers.contains(TypeModifiers::CONST) {
                    name.push_str("const ");
                }
                if typ.modifiers.contains(TypeModifiers::VOLATILE) {
                    name.push_str("volatile ");
                }
                let elem = typ.base.map(|b| self.format_type(b, idents));
                name.push_str(&format!(
                    "__vector({}) {}",
                    typ.array_size.unwrap_or(0),
                    elem.unwrap_or_default()
                ));
                if !decl.is_empty() {
                    name.push(' ');
                    name.push_str(&decl);
                }
                name
            }

            TypeKind::Array => {
                let mut extents = parenthesize_if_pointer(decl);
                let mut cur = id;
                let element = loop {
                    let t = self.get(cur);
                    if t.kind != TypeKind::Array {
                        break Some(cur);
                    }
                    // Outermost extent first, as the declaration writes it.
                    match t.array_size {
                        Some(size) => extents.push_str(&format!("[{}]", size)),
                        None => extents.push_str("[]"),
                    }
                    match t.base {
                        Some(base) => cur = base,
                        None => break None,
                    }
                };
                match element {
                    Some(element) => self.format_declarator(element, extents, idents),
                    None => extents,
                }
            }

            TypeKind::Function => {
                // gcc's spelling of a convention other than the default,
                // inside the declarator: `long (__attribute__((ms_abi)) *)(long)`.
                let decl = match typ.conv {
                    CallingConv::C => decl,
                    CallingConv::Win64 if decl.is_empty() => " __attribute__((ms_abi))".to_string(),
                    CallingConv::Win64 => format!("__attribute__((ms_abi)) {decl}"),
                };
                let mut sig = parenthesize_if_pointer(decl);
                sig.push('(');
                match &typ.params {
                    // 6.7.6.3p14 puts the line at "prototype or not", not at
                    // "zero parameters": `(void)` says there are none, an empty
                    // identifier list says nothing at all. They are distinct
                    // types, so a diagnostic must spell them apart.
                    Some(params) if params.is_empty() && !typ.variadic => {
                        sig.push_str("void");
                    }
                    Some(params) => {
                        for (i, &param) in params.iter().enumerate() {
                            if i > 0 {
                                sig.push_str(", ");
                            }
                            sig.push_str(&self.format_type(param, idents));
                        }
                        if typ.variadic {
                            if !params.is_empty() {
                                sig.push_str(", ");
                            }
                            sig.push_str("...");
                        }
                    }
                    None => {}
                }
                sig.push(')');
                match typ.base {
                    Some(ret) => self.format_declarator(ret, sig, idents),
                    None => sig,
                }
            }

            _ => {
                let mut result = String::new();
                if typ.modifiers.contains(TypeModifiers::CONST) {
                    result.push_str("const ");
                }
                if typ.modifiers.contains(TypeModifiers::VOLATILE) {
                    result.push_str("volatile ");
                }
                // Spelling, not signedness: a diagnostic must name the type the
                // source wrote. Plain `char` is an unsigned type on aarch64
                // Linux and is still `char` here.
                if self.spelled_unsigned(id) {
                    result.push_str("unsigned ");
                } else if typ.modifiers.contains(TypeModifiers::SIGNED)
                    && typ.kind == TypeKind::Char
                {
                    result.push_str("signed ");
                }

                match typ.kind {
                    TypeKind::Struct | TypeKind::Union | TypeKind::Enum => {
                        result.push_str(&typ.kind.to_string());
                        // The tag is what tells one struct from another, and an
                        // assignment diagnostic naming neither is unreadable.
                        if let (Some(comp), Some(idents)) = (typ.composite.as_deref(), idents) {
                            if let Some(tag) = comp.tag {
                                result.push(' ');
                                result.push_str(idents.get(tag));
                            }
                        }
                    }
                    _ => match typ.float_class.keyword(typ.kind) {
                        Some(keyword) => result.push_str(keyword),
                        None => result.push_str(&typ.kind.to_string()),
                    },
                }

                // `_Complex` is a modifier, not a kind, so the kind name alone
                // spells `double _Complex` as plain `double` -- and this is
                // what diagnostics name types with. Passing a `double
                // _Complex` where an `int *` was wanted reported "got
                // 'double'", naming a type the source never wrote and one that
                // is a different size.
                if typ.modifiers.contains(TypeModifiers::COMPLEX) {
                    result.push_str(" _Complex");
                }

                if !decl.is_empty() {
                    // A space only where a pointer is involved: gcc writes
                    // `int *`, `int (*)[8]` and `int (*)(void)` with one, and
                    // `int[4][8]` and `int()` without. The latter is also what
                    // cflow's worked EXAMPLE in POSIX prints.
                    if decl.contains('*') {
                        result.push(' ');
                    }
                    result.push_str(&decl);
                }
                result
            }
        }
    }

    // Production methods (used by compiler proper)

    /// Check if type is an integer type
    pub fn is_integer(&self, id: TypeId) -> bool {
        matches!(
            self.get(id).kind,
            TypeKind::Bool
                | TypeKind::Char
                | TypeKind::Short
                | TypeKind::Int
                | TypeKind::Long
                | TypeKind::LongLong
                | TypeKind::Int128
                | TypeKind::Enum
        )
    }

    /// Check if type is a floating point type (not complex)
    pub fn is_float(&self, id: TypeId) -> bool {
        let typ = self.get(id);
        matches!(
            typ.kind,
            TypeKind::Float
                | TypeKind::Double
                | TypeKind::LongDouble
                | TypeKind::Float16
                | TypeKind::Float128
        ) && !typ.modifiers.contains(TypeModifiers::COMPLEX)
    }

    /// Is this a GNU `vector_size` type?
    pub fn is_vector(&self, id: TypeId) -> bool {
        self.get(id).modifiers.contains(TypeModifiers::VECTOR)
    }

    /// A GNU vector of `count` elements of type `elem`.
    ///
    /// It aligns to its width rounded up to a power of two, capped at
    /// sixteen -- which is what GCC does by default on both targets c17
    /// supports. A 32-byte vector therefore aligns to 16, not 32; only
    /// `-mavx2` raises the cap, and c17 does not model that. Measured
    /// against gcc across widths 4 through 128 rather than assumed, because
    /// "aligns to its own width" is the obvious rule and is wrong. An
    /// `aligned(n)` written alongside, `align`, takes precedence, which is
    /// what `<link.h>` does: `__vector_size__(32), __aligned__(16)`.
    pub fn vector_of(&mut self, elem: TypeId, count: usize, align: Option<u32>) -> TypeId {
        const MAX_VECTOR_ALIGN: usize = 16;
        let bytes = self.size_bytes(elem) * count;
        let natural = bytes.next_power_of_two().min(MAX_VECTOR_ALIGN) as u32;
        self.intern(Type {
            kind: TypeKind::Array,
            base: Some(elem),
            array_size: Some(count),
            modifiers: TypeModifiers::VECTOR,
            explicit_align: Some(align.unwrap_or(natural)),
            ..Default::default()
        })
    }

    /// The element type and the number of elements of a vector type, or
    /// `None` for any other type.
    pub fn vector_lanes(&self, id: TypeId) -> Option<(TypeId, usize)> {
        if !self.is_vector(id) {
            return None;
        }
        let typ = self.get(id);
        Some((typ.base?, typ.array_size?))
    }

    /// The type of a comparison of two vectors of type `id`: as many lanes,
    /// each a signed integer as wide as `id`'s, holding -1 for true and 0 for
    /// false -- gcc's, which names an eight-byte lane `long`.
    pub fn vector_mask_type(&mut self, id: TypeId) -> TypeId {
        let Some((elem, count)) = self.vector_lanes(id) else {
            return self.int_id;
        };
        let lane = match self.size_bytes(elem) {
            1 => self.schar_id,
            2 => self.short_id,
            4 => self.int_id,
            8 => self.long_id,
            _ => self.int128_id,
        };
        let mask = self.vector_of(lane, count, None);
        let mut typ = self.get(mask).clone();
        typ.modifiers |= TypeModifiers::VECTOR_MASK;
        self.intern(typ)
    }

    /// Is this the result type of a vector comparison? See
    /// [`TypeModifiers::VECTOR_MASK`].
    pub fn is_vector_mask(&self, id: TypeId) -> bool {
        self.get(id).modifiers.contains(TypeModifiers::VECTOR_MASK)
    }

    /// Whether a vector comparison's mask of type `mask` assigns to the
    /// vector type `target`: integer lanes, as many and as wide.
    fn mask_assigns_to(&self, mask: TypeId, target: TypeId) -> bool {
        let (Some((ml, mn)), Some((tl, tn))) = (self.vector_lanes(mask), self.vector_lanes(target))
        else {
            return false;
        };
        self.is_vector_mask(mask)
            && mn == tn
            && self.is_integer(tl)
            && self.size_bits(ml) == self.size_bits(tl)
    }

    /// Is this `__builtin_ms_va_list`, the one pointer `__builtin_va_arg`
    /// walks by the Microsoft x64 convention?
    pub fn is_ms_va_list(&self, id: TypeId) -> bool {
        let typ = self.get(id);
        typ.kind == TypeKind::Pointer && typ.modifiers.contains(TypeModifiers::MS_VA_LIST)
    }

    /// Check if type is a complex floating point type
    pub fn is_complex(&self, id: TypeId) -> bool {
        self.get(id).modifiers.contains(TypeModifiers::COMPLEX)
    }

    /// Get the base float type for a complex type (e.g., double for double _Complex)
    /// Returns the same type if not complex
    pub fn complex_base(&self, id: TypeId) -> TypeId {
        if !self.is_complex(id) {
            return id;
        }
        self.canonical_arithmetic_base(id).unwrap_or(id)
    }

    /// The pre-interned, unqualified type of the same kind and signedness, or
    /// `None` for a kind that has no such counterpart.
    ///
    /// Keyed on the *kind*, not on the id: `const long double` and a
    /// `long double` reached through a typedef are different `TypeId`s than
    /// `longdouble_id`, and an exact-id lookup misses both. That is how
    /// `__builtin_complex(7.0L, 8.0L)` came out `_Complex double` -- the
    /// lookup missed and took the fallback -- and built a 16-byte value for a
    /// 32-byte type.
    fn canonical_arithmetic_base(&self, id: TypeId) -> Option<TypeId> {
        let unsigned = self.is_unsigned(id);
        let typ = self.get(id);
        Some(match typ.kind {
            TypeKind::Float | TypeKind::Double | TypeKind::LongDouble => {
                self.floating(typ.kind, typ.float_class)
            }
            TypeKind::Float16 => self.float16_id,
            TypeKind::Float128 => self.float128_id,
            // GNU complex integers. Answering `id` here -- the complex type
            // itself -- made the base twice as wide as the half it describes,
            // so the two halves were read from the same address and the
            // imaginary store overran the object.
            // `char` asks the *modifiers*, not `is_unsigned`: plain `char` is
            // a third type distinct from both `signed char` and
            // `unsigned char`, and its signedness is the target's choice. On
            // a target where it is unsigned, `is_unsigned` would send plain
            // `char` to `uchar_id` and the round trip would no longer land
            // back where it started.
            TypeKind::Char => {
                let mods = self.get(id).modifiers;
                if mods.contains(TypeModifiers::UNSIGNED) {
                    self.uchar_id
                } else if mods.contains(TypeModifiers::SIGNED) {
                    self.schar_id
                } else {
                    self.char_id
                }
            }
            TypeKind::Short if unsigned => self.ushort_id,
            TypeKind::Short => self.short_id,
            TypeKind::Int if unsigned => self.uint_id,
            TypeKind::Int => self.int_id,
            TypeKind::Long if unsigned => self.ulong_id,
            TypeKind::Long => self.long_id,
            TypeKind::LongLong if unsigned => self.ulonglong_id,
            TypeKind::LongLong => self.longlong_id,
            TypeKind::Int128 if unsigned => self.uint128_id,
            TypeKind::Int128 => self.int128_id,
            _ => return None,
        })
    }

    /// Whether this is a complex type whose halves are floating-point.
    ///
    /// The backends want this, not [`Self::is_complex`]: what they mean by
    /// "complex" is "arrives in SSE registers", and a GNU `_Complex int`
    /// arrives in general ones. The linearizer wants `is_complex`, because
    /// what *it* means is "two halves at offsets, addressed rather than held",
    /// which is true of both families.
    pub fn is_complex_float(&self, id: TypeId) -> bool {
        self.is_complex(id) && !self.is_integer(self.complex_base(id))
    }

    /// Whether a complex type's two halves are integers rather than floats.
    ///
    /// The complex emit paths are written once and pick their opcodes from
    /// this: `_Complex int` addition is `Add`, not `FAdd`. Asking
    /// `is_complex` alone cannot tell them apart, and asking the base's kind
    /// at each site invited the two to disagree.
    pub fn is_complex_integer(&self, id: TypeId) -> bool {
        self.is_complex(id) && self.is_integer(self.complex_base(id))
    }

    /// The binary format a floating type is held and computed in on this
    /// target, or `None` if the type is not a floating one.
    ///
    /// `long double` is three different formats across the supported targets
    /// and two of them are 128 bits wide, so this is the only sound way to ask
    /// -- the width does not distinguish x87's 80-bit format from binary128.
    /// A complex type answers for its base, which is the format each of its
    /// two halves has.
    pub fn fp_format(&self, id: TypeId) -> Option<FpFormat> {
        Some(match self.get(id).kind {
            TypeKind::Float16 => FpFormat::Binary16,
            TypeKind::Float => FpFormat::Binary32,
            TypeKind::Double => FpFormat::Binary64,
            TypeKind::Float128 => FpFormat::Binary128,
            TypeKind::LongDouble => match (self.target_arch, self.target_os) {
                (Arch::Aarch64, Os::MacOS) => FpFormat::Binary64,
                (Arch::Aarch64, _) => FpFormat::Binary128,
                _ => FpFormat::X87Extended,
            },
            _ => return None,
        })
    }

    /// The real type a floating complex `*` or `/` whose halves have type
    /// `base` is computed in, and the libgcc routine format that computes it;
    /// `None` if `base` is not a floating type.
    ///
    /// The type is `base` itself wherever `base`'s format has a routine, and
    /// otherwise the type the operands are widened to: `float`, for a
    /// `_Float16` base (see [`FpFormat::complex_routine_format`]).
    pub fn complex_routine_type(&self, base: TypeId) -> Option<(TypeId, ComplexRoutineFormat)> {
        let fmt = self.fp_format(base)?;
        let routine = fmt.complex_routine_format();
        let typ = if routine.format() == fmt {
            base
        } else {
            match routine {
                ComplexRoutineFormat::Binary32 => self.float_id,
                ComplexRoutineFormat::Binary64 => self.double_id,
                ComplexRoutineFormat::X87Extended => self.longdouble_id,
                ComplexRoutineFormat::Binary128 => self.float128_id,
            }
        };
        Some((typ, routine))
    }

    /// Get the complex type for a float base type (e.g., double → double _Complex)
    pub fn make_complex(&self, id: TypeId) -> TypeId {
        if self.is_complex(id) {
            return id;
        }
        // Through the canonical base, so a qualified or typedef'd type finds
        // its counterpart: the table is keyed by the pre-interned ids. The
        // fallback is for the non-arithmetic types that have no complex
        // counterpart at all and should never be asked.
        self.canonical_arithmetic_base(id)
            .and_then(|canon| self.complex_of.get(&canon).copied())
            .unwrap_or(self.complex_double_id)
    }

    /// Check if type is an arithmetic type (integer, float, or complex)
    pub fn is_arithmetic(&self, id: TypeId) -> bool {
        self.is_integer(id) || self.is_float(id) || self.is_complex(id)
    }

    /// How a type participates in an assignment.
    ///
    /// C17 6.3.2.1p3-4 converts an array to a pointer to its element and a
    /// function to a pointer to itself before anything else looks at it.
    /// Reported as a kind plus the referenced type rather than by interning a
    /// pointer, so a caller holding the table immutably can still ask.
    fn as_assigned(&self, id: TypeId) -> (TypeKind, Option<TypeId>) {
        if let Some(pointee) = self.decay_pointee(id) {
            return (TypeKind::Pointer, Some(pointee));
        }
        match self.kind(id) {
            TypeKind::Pointer => (TypeKind::Pointer, self.base_type(id)),
            other => (other, None),
        }
    }

    /// Check a value against the type it is being assigned to (C17
    /// 6.5.16.1p1). `None` means the assignment is allowed.
    ///
    /// The same constraints govern `return` and argument passing, which the
    /// standard defines as conversion "as if by assignment", so all three
    /// contexts ask this and differ only in how they word the answer.
    pub fn assignment_fault(
        &self,
        target: TypeId,
        value: TypeId,
        value_is_null_constant: bool,
    ) -> Option<AssignFault> {
        let (t_kind, t_pointee) = self.as_assigned(target);
        let (v_kind, v_pointee) = self.as_assigned(value);

        // A `void` expression has no value to assign.
        if v_kind == TypeKind::Void {
            return Some(AssignFault::Incompatible);
        }
        // Nothing is known about the target -- an earlier diagnostic already
        // fired, or this is a context the checker does not model.
        if t_kind == TypeKind::Void {
            return None;
        }

        let t_ptr = t_kind == TypeKind::Pointer;
        let v_ptr = v_kind == TypeKind::Pointer;
        let t_agg = matches!(t_kind, TypeKind::Struct | TypeKind::Union) || self.is_vector(target);
        let v_agg = matches!(v_kind, TypeKind::Struct | TypeKind::Union) || self.is_vector(value);

        // An aggregate assigns only from a compatible aggregate. Compared
        // through `types_compatible` rather than by TypeId, because struct and
        // union types are deliberately not deduplicated -- they have identity.
        if t_agg || v_agg {
            let ok = t_agg
                && v_agg
                && (self.types_compatible(target, value) || self.mask_assigns_to(value, target));
            return (!ok).then_some(AssignFault::Incompatible);
        }

        // A null pointer constant assigns to any pointer (6.5.16.1p1), and
        // `(void *)0` is one (6.3.2.3p3) -- so this has to be asked before the
        // pointer-to-pointer rules, not after them. It was read only in the
        // `t_ptr && !v_ptr` branch below, which a null constant that had
        // already decayed to `void *` never reaches. With glibc's
        // `#define NULL ((void *)0)` that made every `fp = NULL` on a function
        // pointer a "ISO C forbids ... function pointer and 'void *'"
        // diagnostic, and broke any -Werror build using function pointers.
        if value_is_null_constant && t_ptr {
            return None;
        }

        if t_ptr && v_ptr {
            let (Some(t_pointee), Some(v_pointee)) = (t_pointee, v_pointee) else {
                return None;
            };
            // `void *` converts to and from any object pointer (6.5.16.1p1).
            //
            // "Object" is the whole of it: the carve-out does not reach a
            // function pointer, which is why `FP fp = dlsym(h, "x");` is a
            // constraint violation. It stays a warning because POSIX requires
            // that exact line to work (XSH `dlsym`) and gcc accepts it in
            // silence, objecting only under `-pedantic`.
            let (t_void, v_void) = (
                self.kind(t_pointee) == TypeKind::Void,
                self.kind(v_pointee) == TypeKind::Void,
            );
            let (t_fn, v_fn) = (
                self.kind(t_pointee) == TypeKind::Function,
                self.kind(v_pointee) == TypeKind::Function,
            );
            if (t_void && v_fn) || (v_void && t_fn) {
                return Some(AssignFault::FunctionPointerVoid);
            }
            if !(t_void || v_void) && !self.types_compatible(t_pointee, v_pointee) {
                return Some(AssignFault::PointerMismatch);
            }
            // Compatible targets, or one of them `void`, but the assignment
            // must not silently gain write access: the target's qualifiers
            // have to include the source's (6.5.16.1p1 says so of both
            // cases), so `void *v = (const int *)p` is diagnosed too.
            let t_quals = self.qualifiers(t_pointee);
            let v_quals = self.qualifiers(v_pointee);
            return (!t_quals.contains(v_quals)).then_some(AssignFault::QualifierDiscard);
        }

        if t_ptr {
            // A null pointer constant is spelled as an integer, and is allowed.
            if value_is_null_constant {
                return None;
            }
            return Some(if self.is_integer(value) {
                AssignFault::PointerFromInteger
            } else {
                AssignFault::Incompatible
            });
        }

        if v_ptr {
            // `_Bool b = p` is explicitly permitted: it asks whether the
            // pointer is null.
            if t_kind == TypeKind::Bool {
                return None;
            }
            return Some(if self.is_integer(target) {
                AssignFault::IntegerFromPointer
            } else {
                AssignFault::Incompatible
            });
        }

        None
    }

    /// The type an array or function has in an expression: C17 6.3.2.1p3-4
    /// converts an array to a pointer to its first element and a function to a
    /// pointer to itself. Every other type is its own.
    pub fn decayed(&mut self, typ: TypeId) -> TypeId {
        match self.decay_pointee(typ) {
            Some(pointee) => self.intern(Type::pointer(pointee)),
            None => typ,
        }
    }

    /// [`Self::decayed`] for a caller holding the table immutably, as the
    /// linearizer does: the pointer type when it has been interned, `void *`
    /// otherwise. Either has the kind, width and signedness of the pointer,
    /// which is everything a conversion of the value reads.
    ///
    /// Typed as itself, a function has no width and an array has its whole
    /// size, so a value converted from that type was converted from the
    /// wrong width -- `(long)f` kept 32 bits of the address.
    pub fn decayed_value(&self, typ: TypeId) -> TypeId {
        match self.decay_pointee(typ) {
            Some(pointee) => self.pointer_to(pointee),
            None => typ,
        }
    }

    /// Whether a value of this type is the address it decays to: an array or
    /// a function.
    pub fn decays(&self, typ: TypeId) -> bool {
        self.decay_pointee(typ).is_some()
    }

    /// What one step of pointer arithmetic on a value of type `typ` spans
    /// (C17 6.5.6p8-9): the type a pointer references, or the one an array or
    /// a function decays to a pointer to. `None` for a type that is not a
    /// pointer once decayed.
    ///
    /// Not [`Self::base_type`], which answers a function type's *return*
    /// type: `f + 1` stepped by `sizeof(int)` in a static initializer.
    pub fn arithmetic_pointee(&self, typ: TypeId) -> Option<TypeId> {
        match self.kind(typ) {
            TypeKind::Pointer => self.base_type(typ),
            _ => self.decay_pointee(typ),
        }
    }

    /// The function type a call through a callee of type `typ` calls: `typ`
    /// itself, or the pointed-to type for a call through a pointer (C17
    /// 6.5.2.2p1 allows either). `None` when that is not a function type --
    /// an implicit declaration, a diagnosed expression.
    pub fn callee_function_type(&self, typ: TypeId) -> Option<TypeId> {
        let func = match self.kind(typ) {
            TypeKind::Pointer => self.base_type(typ).unwrap_or(typ),
            _ => typ,
        };
        (self.kind(func) == TypeKind::Function).then_some(func)
    }

    /// What an array or a function decays to a pointer *to*: the element
    /// type, or the function type itself. `None` for a type that does not
    /// decay. The one statement of C17 6.3.2.1p3-4 that the decays above
    /// and assignment checking all read.
    fn decay_pointee(&self, typ: TypeId) -> Option<TypeId> {
        match self.kind(typ) {
            // A vector is a value, laid out as an array but never decaying.
            TypeKind::Array if self.is_vector(typ) => None,
            TypeKind::Array => Some(self.base_type(typ).unwrap_or(self.char_id)),
            TypeKind::Function => Some(typ),
            _ => None,
        }
    }

    /// Whether a value of this type is made of members at offsets: a struct,
    /// union or array, or a complex number (its real and imaginary halves).
    ///
    /// `kind()` alone cannot answer this, because a complex type carries its
    /// *base's* kind -- `double _Complex` answers `TypeKind::Double` -- so a
    /// site that asks only for `Struct | Union | Array` takes a complex value
    /// for a scalar of its base type. The calling conventions lay a complex
    /// value out as the equivalent two-member struct (a two-element HFA on
    /// AAPCS64, its eightbytes classified as a struct's on System V), so
    /// anything that follows the ABI's classification wants both here.
    pub fn is_aggregate_or_complex(&self, id: TypeId) -> bool {
        self.is_complex(id)
            || matches!(
                self.kind(id),
                TypeKind::Struct | TypeKind::Union | TypeKind::Array
            )
    }

    /// A plain 128-bit integer, and not a complex one.
    ///
    /// `kind()` answers a complex type's *base* kind, exactly as it does for
    /// the aggregates above, so `_Complex __int128` satisfies a bare
    /// `kind(id) == TypeKind::Int128` as well. The two want opposite
    /// treatment: a bare `__int128` is a value for two consecutive registers,
    /// while `_Complex __int128` is a thirty-two byte composite that AAPCS64
    /// and System V both pass by reference. A back end reaching for the
    /// register pair has to ask this.
    pub fn is_plain_int128(&self, id: TypeId) -> bool {
        self.kind(id) == TypeKind::Int128 && !self.is_complex(id)
    }

    /// Check if type is a scalar type (arithmetic or pointer)
    pub fn is_scalar(&self, id: TypeId) -> bool {
        self.is_arithmetic(id) || self.get(id).kind == TypeKind::Pointer
    }

    /// Does this type, or anything inside it, carry `volatile`?
    ///
    /// A qualifier on a *member* makes that member's storage volatile (C17
    /// 6.7.3p7), but the object holding it is not itself volatile-qualified:
    /// `struct { volatile int v; int n; } s;` answers no to
    /// `modifiers(id).contains(VOLATILE)`, because that reports what was
    /// written on the struct. An optimizer asking whether it may move or drop
    /// an access to `s` has to ask this instead -- `loadfwd` forwarded a load
    /// of such a struct across a second copy of it, and `dse` would delete a
    /// store to one.
    ///
    /// A pointer is not followed: `volatile int *p` makes `*p` volatile, not
    /// `p`, and stopping there is also what keeps this finite, since a type
    /// can only contain itself through a pointer. An incomplete composite has
    /// no members to prove otherwise and answers yes, which is the answer
    /// that forbids rather than permits.
    pub fn contains_volatile(&self, id: TypeId) -> bool {
        if self.modifiers(id).contains(TypeModifiers::VOLATILE) {
            return true;
        }
        match self.kind(id) {
            TypeKind::Array => self
                .base_type(id)
                .is_some_and(|elem| self.contains_volatile(elem)),
            TypeKind::Struct | TypeKind::Union => match self.composite(id) {
                Some(c) if c.is_complete => c.members.iter().any(|m| self.contains_volatile(m.typ)),
                // An incomplete composite lists no members, which is not the
                // same as having none.
                _ => true,
            },
            _ => false,
        }
    }

    /// Is `id` `_Atomic`-qualified (C17 6.7.3)?
    pub fn is_atomic(&self, id: TypeId) -> bool {
        self.modifiers(id).contains(TypeModifiers::ATOMIC)
    }

    /// The top-level type qualifiers of `id` (C17 6.7.3p1).
    ///
    /// Only the qualifiers: `modifiers` answers with the declaration
    /// specifiers and the size and sign spellings mixed in, and every caller
    /// that wanted "is this `const`?" had to mask them off itself.
    pub fn qualifiers(&self, id: TypeId) -> TypeModifiers {
        self.modifiers(id) & Type::QUALIFIERS
    }

    /// The version of `id` qualified with `quals` -- the "so-qualified
    /// version" of C17 6.5.2.3p3/p4.
    ///
    /// This is what a member access yields: `s.m` has the member's type plus
    /// the qualifiers of `s`, so a member of a `volatile` object is volatile
    /// and a member of a `const` object is not assignable. Without it a
    /// `volatile struct` read as an ordinary struct and DCE deleted the load.
    ///
    /// Qualifiers outside [`Type::QUALIFIERS`] are ignored, and a type that
    /// already carries all of them is returned unchanged -- so the common case
    /// of an unqualified object interns nothing. A composite is not
    /// deduplicated by [`Self::intern`] (it has identity), so qualifying one
    /// hands back a fresh `TypeId` each time; that is sound because
    /// compatibility of composites is decided by tag and members rather than
    /// by id, and it is rare enough not to matter -- only an access to a
    /// member of aggregate type, through a qualified object, reaches it.
    pub fn qualified_with(&mut self, id: TypeId, quals: TypeModifiers) -> TypeId {
        let add = quals & Type::QUALIFIERS;
        if add.is_empty() {
            return id;
        }
        // C17 6.7.3p10: where an array type is qualified, the *element type* is
        // so-qualified and the array is not. That is also how a declaration
        // records it -- `const int a[4]` puts the `const` on the element -- so
        // qualifying the array instead would leave `cs.arr[0]` an ordinary
        // `int`, which the subscript reads from the element type, and a write
        // to it would be accepted.
        // A vector is no array here: it is qualified itself, as gcc does.
        if self.kind(id) == TypeKind::Array && !self.is_vector(id) {
            let Some(elem) = self.base_type(id) else {
                return id;
            };
            let qualified_elem = self.qualified_with(elem, add);
            if qualified_elem == elem {
                return id;
            }
            let mut array = self.get(id).clone();
            array.base = Some(qualified_elem);
            return self.intern(array);
        }
        if self.modifiers(id).contains(add) {
            return id;
        }
        let mut qualified = self.get(id).clone();
        qualified.modifiers |= add;
        self.intern(qualified)
    }

    /// The unqualified version of `id` (C17 6.3.2.1p2).
    ///
    /// Lvalue conversion drops the qualifiers, so this is the type of the
    /// *value* an lvalue yields: `volatile int v; v + 0` has type `int`, and
    /// nothing downstream may conclude from the sum's type that the addition
    /// touched a volatile object.
    pub fn unqualified(&mut self, id: TypeId) -> TypeId {
        if self.qualifiers(id).is_empty() {
            return id;
        }
        let mut unqualified = self.get(id).clone();
        unqualified.modifiers.remove(Type::QUALIFIERS);
        self.intern(unqualified)
    }

    /// How many scalar initializers it takes to fill this type.
    ///
    /// This is the measure brace elision runs on (C17 6.7.9p20): a brace-less
    /// initializer for an aggregate member consumes exactly this many elements
    /// from the enclosing list. The linearizer places values by it and the
    /// parser sizes incomplete arrays by it, so it lives here rather than in
    /// either -- when only the linearizer knew the rule, `int a[][2] =
    /// {1,2,3,4}` was stored as two rows and sized as four.
    pub fn count_scalar_fields(&self, id: TypeId) -> usize {
        match self.kind(id) {
            TypeKind::Array => {
                let elem_type = self.base_type(id).unwrap_or(self.int_id);
                let count = self.get(id).array_size.unwrap_or(0);
                count * self.count_scalar_fields(elem_type)
            }
            TypeKind::Struct => {
                if let Some(composite) = self.get(id).composite.as_ref() {
                    composite
                        .members
                        .iter()
                        .filter(|m| m.is_initializable())
                        .map(|m| self.count_scalar_fields(m.typ))
                        .sum()
                } else {
                    1
                }
            }
            TypeKind::Union => {
                // A union's initializer initializes its first member (C17
                // 6.7.9p17) -- which may be an anonymous aggregate, so the
                // test is the one positional initialization uses, not "has a
                // name": asking for a name counted `q` in
                // `union { struct { int a, b; }; long q; }` while the
                // initializer walk filled `a` and `b`.
                if let Some(composite) = self.get(id).composite.as_ref() {
                    composite
                        .members
                        .iter()
                        .find(|m| m.is_initializable())
                        .map(|m| self.count_scalar_fields(m.typ))
                        .unwrap_or(1)
                } else {
                    1
                }
            }
            _ => 1,
        }
    }

    /// Whether `id` is an unsigned integer type.
    ///
    /// Not the same question as "was the keyword `unsigned` written" -- see
    /// [`Self::spelled_unsigned`]. Two integer types carry no `UNSIGNED`
    /// modifier and are unsigned anyway:
    ///
    /// - `_Bool`: C17 6.2.5p6 lists it among the standard unsigned integer
    ///   types, and 6.3.1.2 confines its values to 0 and 1. It carries no
    ///   modifier because there is no `signed _Bool` to tell it apart from.
    /// - Plain `char`, where 6.2.5p15's implementation-defined choice is
    ///   unsigned. The modifier cannot be stamped on at intern time: `char`,
    ///   `signed char` and `unsigned char` are three distinct types, and the
    ///   modifier is what `TypeKey::Basic` deduplicates on, so that would
    ///   collapse two of them into one.
    pub fn is_unsigned(&self, id: TypeId) -> bool {
        let typ = self.get(id);
        match typ.kind {
            TypeKind::Bool => true,
            TypeKind::Char
                if !typ
                    .modifiers
                    .intersects(TypeModifiers::SIGNED | TypeModifiers::UNSIGNED) =>
            {
                self.plain_char == CharSignedness::Unsigned
            }
            _ => typ.modifiers.contains(TypeModifiers::UNSIGNED),
        }
    }

    /// Plain `char`'s signedness on the target this table describes.
    pub fn plain_char(&self) -> CharSignedness {
        self.plain_char
    }

    /// Whether the declaration of `id` spelled the keyword `unsigned`.
    ///
    /// A question about source text rather than about values, and the only one
    /// a caller that *reprints* a type should ask: plain `char` is an unsigned
    /// type on aarch64 Linux and is still written `char`, and `_Bool` is
    /// unsigned and is written neither way. Using [`Self::is_unsigned`] here would make
    /// a type printer say `unsigned char` for a declaration that says `char`.
    pub fn spelled_unsigned(&self, id: TypeId) -> bool {
        // An enum's `UNSIGNED` records the signedness of its integer type;
        // nothing spelled it, and `unsigned enum E` is not a type.
        let typ = self.get(id);
        typ.kind != TypeKind::Enum && typ.modifiers.contains(TypeModifiers::UNSIGNED)
    }

    /// The common type of two operands under the usual arithmetic conversions
    /// (C17 6.3.1.8) -- the type C converts both to before operating.
    ///
    /// It lives on the table rather than on one of its callers because four of
    /// them need it and only this one has no other state: the parser typing an
    /// expression, the linearizer lowering one, and the constant folder, which
    /// reaches a `&TypeTable` and nothing else.
    ///
    /// The ladder is by conversion **rank**, not by width: on LP64 `long` and
    /// `long long` are both 64 bits, so comparing `size_bits` would answer
    /// with whichever operand stood on the left, letting `l + ll` disagree
    /// with `ll + l`.
    pub fn common_type(&self, left: TypeId, right: TypeId) -> TypeId {
        // Pointers and the like reach here from comparisons, where there is
        // nothing to convert: the operands already have a common type. The
        // rules below are about arithmetic operands only.
        if !self.is_arithmetic(left) || !self.is_arithmetic(right) {
            return if self.size_bits(left) >= self.size_bits(right) {
                left
            } else {
                right
            };
        }

        // Complex is contagious: if either operand is complex the result is,
        // at the common type of the two real parts (C17 6.3.1.8p1).
        let complex = self.is_complex(left) || self.is_complex(right);

        // Ask about the real parts from here on. A complex type carries its
        // base's kind, so the floating tests below cannot tell `_Complex int`
        // from `int` -- and calling it floating, which is what asking
        // `is_complex` did, matched none of the floating kinds and fell
        // through to the `_Float16` default. `_Complex int * _Complex int`
        // came out `_Complex _Float16`: both operands were converted to half
        // precision and multiplied by `__muldc3`.
        let (left, right) = (self.complex_base(left), self.complex_base(right));

        let left_float = self.is_float(left);
        let right_float = self.is_float(right);
        if left_float || right_float {
            let (l, r) = (self.kind(left), self.kind(right));
            let either = |k| l == k || r == k;
            // Widest first. binary128 outranks x87 extended: equal in range,
            // wider in the significand. Both are _Float16 if nothing else,
            // which C23 keeps as itself.
            let kind = [
                TypeKind::Float128,
                TypeKind::LongDouble,
                TypeKind::Double,
                TypeKind::Float,
            ]
            .into_iter()
            .find(|&k| either(k))
            .unwrap_or(TypeKind::Float16);
            // Then, between two names for that format, the preferred one.
            let class = [left, right]
                .into_iter()
                .filter(|&t| self.kind(t) == kind)
                .map(|t| self.get(t).float_class)
                .max_by_key(|c| c.preference())
                .unwrap_or_default();
            let real = match kind {
                TypeKind::Float128 => self.float128_id,
                TypeKind::Float16 => self.float16_id,
                _ => self.floating(kind, class),
            };
            return if complex {
                self.complex_of.get(&real).copied().unwrap_or(real)
            } else {
                real
            };
        }

        // Integers -- including the halves of a GNU complex integer, whose
        // common type is complex-of-whatever the halves agree on. Every
        // `return` below goes through `pick_complex` for that reason.
        //
        // The promotions run first (C17 6.3.1.8p1) -- without them two
        // sub-`int` operands match none of the rules below and fall to the
        // unsigned case, so `unsigned char` arithmetic came out unsigned.
        let left = self.integer_promote(left);
        let right = self.integer_promote(right);
        let (left_unsigned, right_unsigned) = (self.is_unsigned(left), self.is_unsigned(right));

        // Same signedness: the higher rank, and nothing else to decide.
        if left_unsigned == right_unsigned {
            let real = if self.integer_rank(left) >= self.integer_rank(right) {
                left
            } else {
                right
            };
            return self.pick_complex(complex, real, self.make_complex(real));
        }

        // Mixed. C17 6.3.1.8 takes three more steps, and they need rank *and*
        // width, which are different questions: `long` and `long long` rank
        // apart at the same width, and that is what decides `unsigned long`
        // against `long long`.
        let (signed, unsigned) = if left_unsigned {
            (right, left)
        } else {
            (left, right)
        };
        if self.integer_rank(unsigned) >= self.integer_rank(signed) {
            // The unsigned type ranks at least as high, so everything converts
            // to it: `unsigned long` against `long` is `unsigned long`.
            return self.pick_complex(complex, unsigned, self.make_complex(unsigned));
        }
        if self.size_bits(signed) > self.size_bits(unsigned) {
            // The signed type holds every value of the unsigned one, so it
            // survives with its sign: `-1L / 2u` is `long`, not `unsigned
            // long`, and really is negative.
            return self.pick_complex(complex, signed, self.make_complex(signed));
        }
        // Lower rank but no room to spare -- `unsigned long` against `long
        // long`, both 64 bits. Neither can represent the other, so the answer
        // is the unsigned counterpart of the signed type.
        let real = self.unsigned_version(signed);
        self.pick_complex(complex, real, self.make_complex(real))
    }

    /// The integer conversion rank (C17 6.3.1.1p1), as an ordinal.
    ///
    /// Rank is not width. `long` and `long long` are both 64 bits on every
    /// target here and yet rank apart, which is exactly the case that made
    /// comparing `size_bits` return whichever operand came first.
    fn integer_rank(&self, id: TypeId) -> u8 {
        match self.kind(id) {
            TypeKind::Bool => 0,
            TypeKind::Char => 1,
            TypeKind::Short => 2,
            TypeKind::Int => 3,
            // An enumerated type has the rank of its compatible type
            // (6.3.1.1p1); one not yet completed has no other to go on.
            TypeKind::Enum => self
                .enum_compatible_type(id)
                .map_or(3, |int| self.integer_rank(int)),
            TypeKind::Long => 4,
            TypeKind::LongLong => 5,
            TypeKind::Int128 => 6,
            _ => 3,
        }
    }

    /// The unsigned type corresponding to a signed integer type.
    fn unsigned_version(&self, id: TypeId) -> TypeId {
        match self.kind(id) {
            TypeKind::Char => self.uchar_id,
            TypeKind::Short => self.ushort_id,
            TypeKind::Int => self.uint_id,
            TypeKind::Long => self.ulong_id,
            TypeKind::LongLong => self.ulonglong_id,
            TypeKind::Int128 => self.uint128_id,
            _ => id,
        }
    }

    /// One of a real/complex pair, by whether the result is complex.
    fn pick_complex(&self, complex: bool, real: TypeId, cplx: TypeId) -> TypeId {
        if complex {
            cplx
        } else {
            real
        }
    }

    /// Apply the integer promotions (C17 6.3.1.1p2).
    ///
    /// `_Bool`, `char` and `short` -- signed or unsigned -- all become `int`,
    /// which can represent every value of each of them. Every other type is
    /// returned unchanged.
    ///
    /// This lives on the type table rather than on one consumer because the
    /// promotions are a prerequisite of the usual arithmetic conversions
    /// (6.3.1.8p1), and both the parser and the linearizer compute those.
    pub fn integer_promote(&self, id: TypeId) -> TypeId {
        match self.kind(id) {
            TypeKind::Bool | TypeKind::Char | TypeKind::Short => self.int_id,
            // An enumerated type computes as the integer type it is
            // compatible with, as gcc's does: `e + 1` for an enum whose
            // members are all non-negative is `unsigned int`, not the enum.
            TypeKind::Enum => self
                .enum_compatible_type(id)
                .map_or(id, |int| self.integer_promote(int)),
            _ => id,
        }
    }

    /// The default argument promotions (C17 6.5.2.2p6): the type an argument
    /// with no parameter type to convert it to is passed as -- a variadic
    /// one, or any argument of a call without a prototype -- and so the type
    /// a definition with an identifier list receives each parameter as.
    ///
    /// The integer promotions, and `float` (with `_Float16`, as gcc passes
    /// it) to `double`. A complex type is left alone: `kind` answers its
    /// base's kind, and `float _Complex` is not a `float`.
    pub fn default_argument_promote(&self, id: TypeId) -> TypeId {
        if self.is_complex(id) {
            return id;
        }
        // `_Float32` is not `float` and goes as itself (C23 6.5.2.2p6).
        match self.kind(id) {
            TypeKind::Float if self.get(id).float_class == FloatClass::Standard => self.double_id,
            TypeKind::Float16 => self.double_id,
            _ => self.integer_promote(id),
        }
    }

    /// Get the size of a type in bits
    pub fn size_bits(&self, id: TypeId) -> u32 {
        let typ = self.get(id);
        let is_complex = typ.modifiers.contains(TypeModifiers::COMPLEX);
        let multiplier = if is_complex { 2 } else { 1 };
        match typ.kind {
            // GCC extension: sizeof(void) = 1 for pointer arithmetic on void*.
            // Standard C leaves sizeof(void) undefined, but GCC and most code
            // assumes void* arithmetic works like char* (1 byte per unit).
            TypeKind::Void => 8,
            TypeKind::Bool => 8,
            // The integer kinds carry the multiplier because `_Complex int` is
            // a GNU extension c17 accepts: two `int`s, exactly as the floating
            // ones are two `double`s. Leaving them at their real width made
            // `sizeof(_Complex int)` 4, and every write of an imaginary part
            // then landed one object past the end of the storage.
            TypeKind::Char => 8 * multiplier,
            TypeKind::Short => 16 * multiplier,
            TypeKind::Int => 32 * multiplier,
            TypeKind::Long => 64 * multiplier,
            TypeKind::LongLong => 64 * multiplier,
            TypeKind::Int128 => 128 * multiplier,
            TypeKind::Float => 32 * multiplier,
            TypeKind::Double => 64 * multiplier,
            TypeKind::LongDouble => self.longdouble_size_bits() * multiplier,
            TypeKind::Float16 => 16 * multiplier,
            TypeKind::Float128 => 128 * multiplier,
            TypeKind::Pointer => self.pointer_width,
            // An aggregate larger than `u32::MAX` bits -- 512 MB -- has no
            // *value* width, and this answers value widths: it is what the IR
            // records on an instruction and what the ABI classifies. An object
            // that large is only ever copied, and by a byte count, so ask
            // `size_bytes` for its size. Saturating here rather than wrapping
            // keeps a too-wide type from looking small.
            TypeKind::Array => {
                let elem_size = typ.base.map(|b| self.size_bits(b)).unwrap_or(0) as u64;
                let count = typ.array_size.unwrap_or(0) as u64;
                elem_size.saturating_mul(count).min(u32::MAX as u64) as u32
            }
            TypeKind::Struct | TypeKind::Union => {
                let bytes = typ.composite.as_ref().map(|c| c.size).unwrap_or(0) as u64;
                bytes.saturating_mul(8).min(u32::MAX as u64) as u32
            }
            TypeKind::Function => 0,
            // An enumeration is as wide as the integer type it was found to
            // be compatible with (C17 6.7.2.2p4), which is `int` unless a
            // member did not fit. A forward reference has no members yet.
            TypeKind::Enum => (typ.composite.as_ref().map(|c| c.size).unwrap_or(4) * 8) as u32,
            TypeKind::VaList => self.va_list_size_bits(),
        }
    }

    /// The largest object c17 can describe, in bytes: `PTRDIFF_MAX`.
    ///
    /// An object is addressed by pointer arithmetic, and C17 6.5.6p9 makes the
    /// difference of two pointers into one object a `ptrdiff_t`, so an object
    /// larger than that cannot be indexed from end to end. It is gcc's bound
    /// too, and gcc accepts an object of exactly this size. `ptrdiff_t` is
    /// `long` on every target (`__PTRDIFF_TYPE__` in `arch/mod.rs`, and the
    /// type `p - q` is given), so the bound is read from that type's width
    /// rather than written down.
    ///
    /// Nothing inside the compiler is tighter. Struct layout runs in bits,
    /// because a bit-field's position is only expressible there, and it
    /// accumulates them in a `u128`, which no member list whose members are
    /// each describable can overflow; an array is sized in bytes directly.
    ///
    /// It used to be `u64::MAX / 8`, a quarter of this, because layout
    /// accumulated its bits in a `usize`; and before that `u32::MAX / 8` --
    /// 512 MB, the largest object whose size in *bits* fitted the `u32` that
    /// [`Self::size_bits`] answers in. Object sizes are counted in bytes, by
    /// [`Self::size_bytes`].
    pub fn max_object_bytes(&self) -> usize {
        let width = self.size_bits(self.long_id);
        usize::try_from((1u128 << (width - 1)) - 1).unwrap_or(usize::MAX)
    }

    /// The largest object the backend can give a *stack* slot.
    ///
    /// Two bounds again, and this time the operative one is not C's:
    ///
    /// - **The object's own**, [`Self::max_object_bytes`]: what a size can be
    ///   described as at all, and what `sizeof` answers.
    /// - **The frame's**, `i32::MAX` less [`Self::FRAME_HEADROOM_BYTES`] and
    ///   rounded down to an eightbyte, and therefore the operative one here:
    ///   both backends address a local and a stacked argument by a signed
    ///   32-bit displacement from the frame register, so an object past this
    ///   has no slot to be given. The headroom is what the prologue adds after
    ///   the locals are laid out; without it an accepted frame wrapped in that
    ///   arithmetic and the prologue allocated nothing.
    ///
    /// This is a c17 backend limit and not a C one -- C17 says nothing about
    /// where an object with automatic storage duration lives, and gcc compiles
    /// the same declaration with `movabsq`-based 64-bit frame addressing.
    /// Widening both backends' offsets to `i64` is the change that would lift
    /// the bound; until then a diagnostic is the honest answer.
    ///
    /// It does **not** apply to an object with static storage duration, which
    /// is addressed symbolically and works at any size
    /// [`Self::max_object_bytes`] allows: `char g[3000000000];` emits
    /// `.zero 3000000000` on both targets.
    ///
    /// Until this existed every size conversion in `arch/` and `abi/` was a
    /// bare `as i32` and wrapped. `char a[3000000000];` in a function came out
    /// as -1294967296, the `size.max(8)` that follows gave it an eight-byte
    /// slot, and the whole frame was `subq $32, %rsp` with the array laid
    /// across it -- with no diagnostic at all.
    pub const MAX_STACK_OBJECT_BYTES: usize = (i32::MAX as usize - Self::FRAME_HEADROOM_BYTES) & !7;

    /// What a prologue adds to a frame beyond its locals, bounded generously:
    /// the saved frame pointer and link register, every callee-saved general
    /// and floating-point register (at most 160 bytes on aarch64, 48 on
    /// x86-64), the variadic register save area (192 and 176), and the
    /// sixteen-byte roundings between them. The frame's over-alignment is not
    /// in here: it depends on the frame, and `arch::regalloc::grow_frame`
    /// reserves it separately.
    ///
    /// Reserving it once, where a frame is admitted, is what lets every sum
    /// after that stay in `i32` without a check of its own.
    pub const FRAME_HEADROOM_BYTES: usize = 4096;

    /// Get the size of a type in bytes
    /// The size of a type in bytes -- the answer `sizeof` gives.
    ///
    /// Counted in bytes throughout, not derived from [`Self::size_bits`]:
    /// a bit count caps at 512 MB in a `u32`, and going through one made
    /// `sizeof` of a larger object answer that cap rather than its size.
    pub fn size_bytes(&self, id: TypeId) -> usize {
        let typ = self.get(id);
        match typ.kind {
            TypeKind::Struct | TypeKind::Union => {
                typ.composite.as_ref().map(|c| c.size).unwrap_or(0)
            }
            TypeKind::Enum => typ.composite.as_ref().map(|c| c.size).unwrap_or(4),
            // GCC extension: a function type is 1 byte, as `void` is, so
            // `sizeof(f)` is 1 and a pointer to a function steps by one byte
            // -- `fp + 1`, `fp++`, and `fp - fq` dividing by this size. It is
            // an object size only: a function has no value width, and
            // `size_bits` still answers 0 for it.
            TypeKind::Function => 1,
            TypeKind::Array => {
                let elem = typ.base.map(|b| self.size_bytes(b)).unwrap_or(0);
                let count = typ.array_size.unwrap_or(0);
                elem.saturating_mul(count)
            }
            _ => (self.size_bits(id) / 8) as usize,
        }
    }

    /// Get alignment for a type in bytes.
    /// If the type has an explicit alignment (from typedef __attribute__((aligned(N)))),
    /// that takes precedence over the natural alignment.
    pub fn alignment(&self, id: TypeId) -> usize {
        let typ = self.get(id);
        // Explicit alignment from typedef __attribute__((aligned(N))) overrides natural
        if let Some(explicit) = typ.explicit_align {
            return explicit as usize;
        }
        // C17 6.2.5p27 lets an atomic type have a different alignment from its
        // unqualified version, and it must: the hardware's atomic access at
        // width N requires N-byte alignment. At the struct's natural 4,
        // `_Atomic struct S8 { int a, b; }` gives aarch64 SIGBUS on the 8-byte
        // access, and x86-64 one that is not atomic across a cache line.
        //
        // gcc's rule, measured on both targets: a power-of-two size up to 16
        // aligns to its own size, anything else keeps its natural alignment.
        // The odd sizes are the ones with no lock-free access to align for.
        if typ.modifiers.contains(TypeModifiers::ATOMIC) {
            let size = self.size_bytes(id);
            if size.is_power_of_two() && size <= 16 {
                return size.max(self.natural_alignment(id));
            }
        }
        self.natural_alignment(id)
    }

    /// The alignment the type would have without `_Atomic`.
    /// The alignment a type inherently requires, ignoring any `explicit_align`
    /// already recorded on it.
    ///
    /// This is what an explicit alignment may not go below. Asking
    /// `alignment()` instead compares against whatever a *previous* attribute
    /// set, so a type carrying a derived alignment -- a `vector_size` array,
    /// which aligns to its own width -- could not then be given the smaller
    /// alignment the source asked for, though nothing inherent required the
    /// larger one.
    pub fn natural_alignment(&self, id: TypeId) -> usize {
        let typ = self.get(id);
        match typ.kind {
            TypeKind::Void => 1,
            TypeKind::Bool | TypeKind::Char => 1,
            TypeKind::Short => 2,
            TypeKind::Int | TypeKind::Float => 4,
            TypeKind::Long | TypeKind::LongLong | TypeKind::Double | TypeKind::Pointer => 8,
            TypeKind::Int128 => 16,
            TypeKind::LongDouble => self.longdouble_alignment(),
            TypeKind::Float16 => 2,
            TypeKind::Float128 => 16,
            TypeKind::Struct | TypeKind::Union => {
                typ.composite.as_ref().map(|c| c.align).unwrap_or(1)
            }
            TypeKind::Enum => typ.composite.as_ref().map(|c| c.align).unwrap_or(4),
            TypeKind::Array => typ.base.map(|b| self.alignment(b)).unwrap_or(1),
            TypeKind::Function => 1,
            TypeKind::VaList => self.va_list_alignment(),
        }
    }

    // Target-dependent type size helpers

    /// Whether `__float128` is available on this target.
    ///
    /// The type is soft-float everywhere, since neither supported architecture
    /// has binary128 arithmetic in hardware, so it exists only where the
    /// `__*tf*` runtime helpers do. Linux has them in libgcc, versioned
    /// `GCC_4.3.0`. Apple's arm64 runtime has none of them, and clang does not
    /// offer the type there either, so a program using it would compile and
    /// then fail to link on every operation it performed.
    /// The target these types were built for.
    ///
    /// The arch and the OS are what anything here depends on; the rest of a
    /// `Target` is derived from those two.
    pub fn target(&self) -> Target {
        Target::new(self.target_arch, self.target_os)
    }

    /// The unqualified real floating type of `kind` and `class`: `float`,
    /// `_Float32x`, ... A class `kind` has no member of falls back to the
    /// standard type of that kind.
    pub fn floating(&self, kind: TypeKind, class: FloatClass) -> TypeId {
        let key = TypeKey::Basic(kind, 0, class);
        let standard = TypeKey::Basic(kind, 0, FloatClass::Standard);
        self.lookup
            .get(&key)
            .or_else(|| self.lookup.get(&standard))
            .copied()
            .expect("the real floating types are pre-interned")
    }

    /// Whether `_Float64x` exists here; see `arch::has_float64x`.
    pub fn has_float64x(&self) -> bool {
        crate::arch::has_float64x(&Target {
            arch: self.target_arch,
            os: self.target_os,
            ..Target::host()
        })
    }

    pub fn has_float128(&self) -> bool {
        crate::arch::has_float128(&Target {
            arch: self.target_arch,
            os: self.target_os,
            ..Target::host()
        })
    }

    /// Get long double size in bits based on target architecture
    /// - macOS aarch64: 64 bits (same as double)
    /// - x86-64: 128 bits (80-bit x87 padded to 16 bytes)
    /// - aarch64 Linux: 128 bits (IEEE quad precision)
    fn longdouble_size_bits(&self) -> u32 {
        match (self.target_arch, self.target_os) {
            (Arch::Aarch64, Os::MacOS) => 64, // Same as double
            _ => 128, // 80-bit padded (x86-64) or 128-bit quad (aarch64/Linux)
        }
    }

    /// Get long double alignment in bytes based on target architecture
    fn longdouble_alignment(&self) -> usize {
        match (self.target_arch, self.target_os) {
            (Arch::Aarch64, Os::MacOS) => 8, // Same as double
            _ => 16,                         // 16-byte alignment for 80-bit or 128-bit
        }
    }

    /// Get va_list size in bits based on target architecture
    /// - x86-64: 192 bits (24-byte struct)
    /// - aarch64 macOS: 64 bits (simple pointer)
    /// - aarch64 Linux/FreeBSD: 256 bits (32-byte struct)
    fn va_list_size_bits(&self) -> u32 {
        match (self.target_arch, self.target_os) {
            (Arch::X86_64, _) => 192,
            (Arch::Aarch64, Os::MacOS) => 64,
            (Arch::Aarch64, _) => 256, // Linux/FreeBSD
        }
    }

    /// Is `va_list` a plain pointer on this target, rather than an aggregate?
    ///
    /// Everywhere else it is something whose *address* travels: SysV x86_64
    /// spells it `__va_list_tag[1]`, an array, and AAPCS64 uses a 32-byte
    /// record that is passed by reference. Darwin on aarch64 puts every
    /// variadic argument on the stack and spells `va_list` as `char *`, so
    /// there is nothing to decay -- the pointer itself is the value, and
    /// handing a callee its address gives it a pointer to a pointer.
    pub fn va_list_is_pointer(&self) -> bool {
        matches!(
            (self.target_arch, self.target_os),
            (Arch::Aarch64, Os::MacOS)
        )
    }

    /// Get va_list alignment in bytes based on target architecture
    fn va_list_alignment(&self) -> usize {
        8 // All supported platforms use 8-byte alignment for va_list
    }

    /// The type an access to `name` yields, in an object of type `object`.
    ///
    /// C17 6.5.2.3p3/p4: the result of `s.m` and of `p->m` has the
    /// *so-qualified* version of the member's type, so a member of a
    /// `volatile` object is volatile and a member of a `const` object is not
    /// assignable. [`Self::find_member`] answers with the member's *declared*
    /// type, which is what every other caller of it wants -- an offset, a size,
    /// a bit-field's width -- so the rule lives here, beside it, rather than in
    /// each of the two expression forms that need it.
    ///
    /// For `p->m` the object is the pointee: `struct S *volatile p` qualifies
    /// the pointer, not what it points at.
    pub fn member_access_type(&mut self, object: TypeId, name: StringId) -> Option<TypeId> {
        let info = self.find_member(object, name)?;
        Some(self.subobject_type(info.typ, self.qualifiers(object) | info.quals))
    }

    /// The type of a subobject reached inside an object qualified with
    /// `inherited`: the declared type `declared`, so-qualified (C17
    /// 6.5.2.3p3/p4).
    ///
    /// The one place the rule is written. A member access, an initializer
    /// storing into a member of a `volatile` object, and a lookup through an
    /// anonymous `volatile` structure all reach a subobject the same way, and
    /// only [`Type::MEMBER_QUALIFIERS`] of what they pass through travel.
    pub fn subobject_type(&mut self, declared: TypeId, inherited: TypeModifiers) -> TypeId {
        self.qualified_with(declared, inherited & Type::MEMBER_QUALIFIERS)
    }

    /// Is `member` an anonymous structure or union (C17 6.7.2.1p13) -- one
    /// whose members are members of the containing aggregate?
    ///
    /// An unnamed member of structure or union type *with no tag*. An unnamed
    /// bit-field is padding, not an anonymous member.
    pub fn is_anonymous_aggregate(&self, member: &StructMember) -> bool {
        if member.name != StringId::EMPTY || member.bit_width.is_some() {
            return false;
        }
        let t = self.get(member.typ);
        matches!(t.kind, TypeKind::Struct | TypeKind::Union)
            && t.composite.as_ref().is_some_and(|c| c.tag.is_none())
    }

    /// Find a member in a struct/union type, including anonymous struct/union members
    /// C11 6.7.2.1p13: "An unnamed member of structure type with no tag is called an
    /// anonymous structure; an unnamed member of union type with no tag is called an
    /// anonymous union. The members of an anonymous structure or union are considered
    /// to be members of the containing structure or union."
    ///
    /// The `typ` this answers with is the member's *declared* type. An
    /// expression that accesses the member has the so-qualified version of it
    /// instead (C17 6.5.2.3p3/p4) -- see [`Self::member_access_type`], which is
    /// what the `.` and `->` operators go through.
    pub fn find_member(&self, id: TypeId, name: StringId) -> Option<MemberInfo> {
        self.find_member_recursive(id, name, 0)
    }

    /// Recursive helper for find_member that tracks base offset for anonymous members
    fn find_member_recursive(
        &self,
        id: TypeId,
        name: StringId,
        base_offset: usize,
    ) -> Option<MemberInfo> {
        let composite = self.get(id).composite.as_ref()?;
        for member in &composite.members {
            if member.name == name {
                return Some(MemberInfo {
                    offset: base_offset + member.offset,
                    typ: member.typ,
                    bit_offset: member.bit_offset,
                    bit_width: member.bit_width,
                    access_bytes: member.access_bytes,
                    quals: TypeModifiers::empty(),
                });
            }
            if self.is_anonymous_aggregate(member) {
                if let Some(mut found) =
                    self.find_member_recursive(member.typ, name, base_offset + member.offset)
                {
                    found.quals |= self.qualifiers(member.typ) & Type::MEMBER_QUALIFIERS;
                    return Some(found);
                }
            }
        }
        None
    }

    /// Check if two types are compatible (for __builtin_types_compatible_p)
    ///
    /// Top-level qualifiers are ignored, which is what
    /// `__builtin_types_compatible_p` documents and what "compatible type"
    /// means of a whole object. They are *not* ignored below the top level:
    /// C17 6.7.6.1p2 requires two pointers to be identically qualified as well
    /// as to point at compatible types, so `char *` and `const char *` are
    /// different types.
    pub fn types_compatible(&self, id1: TypeId, id2: TypeId) -> bool {
        self.compatible(id1, id2, TopLevelQualifiers::Ignored, EnumMatch::Integer)
    }

    /// Do these two types denote the same type, as a typedef redefinition
    /// requires (C17 6.7p3)?
    ///
    /// Compatibility, minus the two allowances that make different types
    /// compatible: top-level qualifiers count, and an enumerated type is not
    /// its integer type. gcc rejects both `typedef int T; typedef const int T;`
    /// and `typedef enum E T; typedef unsigned T;`.
    pub fn types_same(&self, id1: TypeId, id2: TypeId) -> bool {
        self.compatible(
            id1,
            id2,
            TopLevelQualifiers::Significant,
            EnumMatch::Distinct,
        )
    }

    fn compatible(
        &self,
        id1: TypeId,
        id2: TypeId,
        quals: TopLevelQualifiers,
        enums: EnumMatch,
    ) -> bool {
        // Quick check: same TypeId means same type
        if id1 == id2 {
            return true;
        }
        if quals == TopLevelQualifiers::Significant && self.qualifiers(id1) != self.qualifiers(id2)
        {
            return false;
        }
        // C17 6.7.2.2p4: an enumerated type is compatible with its integer
        // type. The qualifiers have been settled above, so what is left is a
        // comparison of two unqualified integer types.
        if enums == EnumMatch::Integer {
            if let Some((a, b)) = self.enum_as_integer(id1, id2) {
                return self.compatible(a, b, TopLevelQualifiers::Ignored, enums);
            }
        }
        if !self.get(id1).compatible_ignoring_base(self.get(id2)) {
            return false;
        }
        // Then the referenced type and the parameter types, structurally
        // rather than by identity. Recursion terminates: `base` and parameter
        // chains shorten by one derivation at a time, and a struct's members
        // are compared as ids inside its composite rather than followed.
        //
        // The referenced type carries its qualifiers into the comparison --
        // that is the whole of 6.7.6.1p2 -- while a *parameter* does not:
        // 6.7.6.3p15 takes each parameter as having the unqualified version of
        // its declared type, so `void f(const int)` and `void f(int)` are one
        // type.
        let base_ok = match (self.get(id1).base, self.get(id2).base) {
            (Some(a), Some(b)) => self.compatible(a, b, TopLevelQualifiers::Significant, enums),
            (None, None) => true,
            _ => false,
        };
        if !base_ok {
            return false;
        }
        match (&self.get(id1).params, &self.get(id2).params) {
            (Some(a), Some(b)) => a
                .iter()
                .zip(b.iter())
                .all(|(&x, &y)| self.parameters_compatible(x, y, enums)),
            (Some(_), None) => self.prototype_matches_unprototyped(id1, enums),
            (None, Some(_)) => self.prototype_matches_unprototyped(id2, enums),
            (None, None) => true,
        }
    }

    /// Is the function type `proto`, which has a prototype, compatible with
    /// one of the same return type that has none (C17 6.2.7p3)?
    ///
    /// Only when a call through either would pass the same arguments: no
    /// ellipsis, and every parameter of a type the default argument
    /// promotions leave alone. `int (void)` and `int (double)` match `int ()`,
    /// while `int (char)` and `int (float)` -- whose arguments a call without
    /// a prototype passes as `int` and `double` -- do not.
    fn prototype_matches_unprototyped(&self, proto: TypeId, enums: EnumMatch) -> bool {
        let typ = self.get(proto);
        !typ.variadic
            && typ.params.iter().flatten().all(|&p| {
                let promoted = self.default_argument_promote(p);
                self.compatible(p, promoted, TopLevelQualifiers::Ignored, enums)
            })
    }

    /// An enumerated type meeting a type that is not one, with the enum
    /// replaced by the integer type it is compatible with; `None` for any
    /// other pairing.
    ///
    /// Two enums are left alone: different enumerated types are not
    /// compatible even when their integer types agree, and an enum meeting
    /// its own forward declaration is decided by its tag.
    fn enum_as_integer(&self, id1: TypeId, id2: TypeId) -> Option<(TypeId, TypeId)> {
        let is_enum = |id| self.kind(id) == TypeKind::Enum;
        match (is_enum(id1), is_enum(id2)) {
            (true, false) => Some((self.enum_compatible_type(id1)?, id2)),
            (false, true) => Some((id1, self.enum_compatible_type(id2)?)),
            _ => None,
        }
    }

    /// The integer type an enumerated type is compatible with (C17
    /// 6.7.2.2p4), or `None` for any other type and for an enum whose list
    /// has not been seen yet.
    ///
    /// The parser's `enum_underlying_type` makes the choice when the list
    /// closes, and records it as the enum's size and its `UNSIGNED` modifier
    /// -- the two things layout and arithmetic read. This reads them back. An
    /// eight-byte enum is `long`, as it is for gcc on LP64.
    pub fn enum_compatible_type(&self, id: TypeId) -> Option<TypeId> {
        let typ = self.get(id);
        if typ.kind != TypeKind::Enum {
            return None;
        }
        let composite = typ.composite.as_deref().filter(|c| c.is_complete)?;
        let unsigned = typ.modifiers.contains(TypeModifiers::UNSIGNED);
        let int = match (composite.size, unsigned) {
            (1, false) => IntType::SChar,
            (1, true) => IntType::UChar,
            (2, false) => IntType::Short,
            (2, true) => IntType::UShort,
            (4, false) => IntType::Int,
            (4, true) => IntType::UInt,
            (8, false) => IntType::Long,
            (8, true) => IntType::ULong,
            _ => return None,
        };
        Some(self.int_type_id(int))
    }

    /// Are these two parameter types compatible?
    ///
    /// Ordinary compatibility, plus gcc's `transparent_union` rule: a
    /// parameter of a transparent union is passed as its first member, so a
    /// declaration using the union and one using that member describe the same
    /// function. glibc's own socket calls are written that way -- `sendto` is
    /// declared with `__CONST_SOCKADDR_ARG` and defined with
    /// `const struct sockaddr *` -- and refusing the pair reported
    /// "conflicting types" for a header and a source file that agree.
    fn parameters_compatible(&self, a: TypeId, b: TypeId, enums: EnumMatch) -> bool {
        if self.compatible(a, b, TopLevelQualifiers::Ignored, enums) {
            return true;
        }
        for (union_side, other) in [(a, b), (b, a)] {
            if let Some(member) = self.transparent_union_first_member(union_side) {
                if self.compatible(member, other, TopLevelQualifiers::Ignored, enums) {
                    return true;
                }
            }
        }
        false
    }

    /// Check if two types are compatible *and* identically qualified.
    ///
    /// `types_compatible` deliberately ignores top-level qualifiers, which is
    /// what `__builtin_types_compatible_p` documents. C17 6.7.3p10 is stricter:
    /// "for two qualified types to be compatible, both shall have the
    /// identically qualified version of a compatible type". `_Generic` needs
    /// the strict rule in both directions -- `int` and `const int` may coexist
    /// as associations because they are *not* compatible, and the `const int`
    /// association can never be selected because the controlling expression has
    /// been lvalue-converted to an unqualified type.
    pub fn types_compatible_qualified(&self, id1: TypeId, id2: TypeId) -> bool {
        self.compatible(
            id1,
            id2,
            TopLevelQualifiers::Significant,
            EnumMatch::Integer,
        )
    }

    /// The composite type of two compatible types (C17 6.2.7p3), carrying
    /// `a`'s qualifiers at every level where the two may differ only in
    /// derivation.
    ///
    /// An array of known size beats one of unknown size, a function with a
    /// prototype beats one without, and the referenced type, element type,
    /// return type and parameter types are composed in turn. Any other pair
    /// of compatible types has nothing to choose between, and is `a`.
    pub fn composite_type(&mut self, a: TypeId, b: TypeId) -> TypeId {
        if a == b || self.kind(a) != self.kind(b) {
            return a;
        }
        let mut composite = self.get(a).clone();
        let other = self.get(b).clone();
        match composite.kind {
            TypeKind::Pointer | TypeKind::Array | TypeKind::Function => {}
            _ => return a,
        }
        if let (Some(x), Some(y)) = (composite.base, other.base) {
            composite.base = Some(self.composite_type(x, y));
        }
        match composite.kind {
            // "Unknown" is spelled both as no size and as zero; see
            // `redeclaration_compatible`.
            TypeKind::Array if matches!(composite.array_size, None | Some(0)) => {
                composite.array_size = other.array_size.or(composite.array_size);
            }
            TypeKind::Function => match (&composite.params, &other.params) {
                (None, Some(_)) => {
                    composite.params = other.params.clone();
                    composite.variadic = other.variadic;
                }
                (Some(x), Some(y)) if x.len() == y.len() => {
                    let (x, y) = (x.clone(), y.clone());
                    composite.params = Some(
                        x.iter()
                            .zip(&y)
                            .map(|(&p, &q)| self.composite_type(p, q))
                            .collect(),
                    );
                }
                _ => {}
            },
            _ => {}
        }
        self.intern(composite)
    }

    /// The qualifiers of `id`, where those of an array type are its element
    /// type's (C17 6.7.3p10): `const int[3]` is as `const` as `const int`.
    pub fn qualifiers_through_arrays(&self, id: TypeId) -> TypeModifiers {
        match self.kind(id) {
            TypeKind::Array => self
                .base_type(id)
                .map_or_else(TypeModifiers::empty, |e| self.qualifiers_through_arrays(e)),
            _ => self.qualifiers(id),
        }
    }

    /// The unqualified version of `id`, where an array's qualifiers are its
    /// element type's: the inverse of [`Self::qualified_with`], which puts a
    /// qualifier of an array on its elements.
    pub fn unqualified_through_arrays(&mut self, id: TypeId) -> TypeId {
        if self.kind(id) != TypeKind::Array {
            return self.unqualified(id);
        }
        let Some(elem) = self.base_type(id) else {
            return id;
        };
        let bare = self.unqualified_through_arrays(elem);
        if bare == elem {
            return id;
        }
        let mut array = self.get(id).clone();
        array.base = Some(bare);
        self.intern(array)
    }

    /// Compute struct layout with natural alignment
    /// Updates member offsets in place and returns (total_size, alignment)
    ///
    /// The System V ABI allocates every member from a running *bit* offset
    /// measured from the start of the struct. A bitfield takes the next free
    /// bits; its declared type contributes the struct's alignment and the size
    /// of the window the field may not straddle, but never an allocation of
    /// its own. So two bitfields of different declared types share a unit
    /// freely, and a bitfield reuses the padding left by the plain member
    /// before it.
    /// `pack_cap` is the `#pragma pack(n)` in force, if any. A struct-level
    /// `packed` is not a cap but a property of every member, so it arrives in
    /// each member's [`MemberAlign`]; [`Self::member_alignment`] is the one
    /// rule that combines the two.
    pub fn compute_struct_layout(
        &self,
        members: &mut [StructMember],
        pack_cap: Option<u32>,
    ) -> (usize, usize) {
        // In bits, and in a `u128`: every member is at most `usize::MAX`
        // bytes, and no member list that fits in memory can overflow it, so
        // the layout of any member list is exact. A byte count derived from
        // it goes through `bytes_of`, which saturates -- the caller measures
        // the size against `max_object_bytes`, and a saturated size fails
        // that where a wrapped one would have come out small.
        let mut bit_offset = 0u128;
        let bytes_of = |bits: u128| usize::try_from(bits / 8).unwrap_or(usize::MAX);
        let mut max_align = 1usize;
        // Alignment demanded by a zero-width bitfield, on the ABIs where one
        // demands any. Kept separate because packing does not reduce it.
        let mut zero_width_align = 1usize;
        // The furthest bit any access window reaches. Ordinary members never
        // reach past the running offset, but a window is a power-of-two span
        // that can, and the struct has to be large enough to hold it.
        let mut window_end = 0u128;

        for member in members.iter_mut() {
            let Some(bit_width) = member.bit_width else {
                let align = self.member_alignment(member, pack_cap);
                max_align = max_align.max(align);

                bit_offset = bit_offset.next_multiple_of(align as u128 * 8);
                member.offset = bytes_of(bit_offset);
                member.bit_offset = None;
                member.access_bytes = None;

                bit_offset += self.size_bytes(member.typ) as u128 * 8;
                continue;
            };

            let unit_bytes = self.size_bytes(member.typ);
            let unit_bits = unit_bytes as u128 * 8;

            if bit_width == 0 {
                // C17 6.7.2.1p12: a zero-width bitfield forces the *next*
                // member to the next boundary of its declared type's storage
                // unit. Whether it also raises the enclosing struct's
                // alignment is the one point the two ABIs disagree on, and
                // 6.7.2.1p12 leaves it to them.
                //
                // The x86-64 psABI says it does not: `struct { char c;
                // int :0; char d; }` is 5 bytes, alignment 1. AAPCS64 says it
                // contributes its declared type's alignment, making the same
                // struct 8 bytes with alignment 4 -- and unlike an ordinary
                // member's, that contribution survives packing, so it is kept
                // out of `max_align` and applied afterwards. Both are gcc's
                // answers on the respective target.
                if self.target_arch == Arch::Aarch64 {
                    zero_width_align = zero_width_align.max(self.alignment(member.typ));
                }
                bit_offset = bit_offset.next_multiple_of(unit_bits);
                member.offset = bytes_of(bit_offset);
                member.bit_offset = None;
                member.access_bytes = None;
                continue;
            }

            let align = self.member_alignment(member, pack_cap);
            max_align = max_align.max(align);
            // An alignment written on the field places it, as it places any
            // member: `int b:3 __attribute__((aligned(8)))` starts at the
            // next 8-byte boundary, packed or not. Without one, the rules below
            // place it.
            if member.align.written.is_some() {
                bit_offset = bit_offset.next_multiple_of(align as u128 * 8);
            }

            let bit_width = u128::from(bit_width);
            if Self::packs_bitfield(member, pack_cap) {
                // Packed -- by `packed` on the field or its aggregate, or by
                // any pack cap -- the unit rule is switched off entirely, not
                // narrowed to the cap. `#pragma pack(2)` lets a 16-bit
                // field starting at bit 1 straddle both the 2- and the 4-byte
                // boundary, which is gcc's answer and the measurement that
                // rules out the narrowing reading. So the field takes the next
                // free bit, and its access span is exactly the bytes its own
                // bits touch: never wider than the object, so `window_end`
                // takes no contribution here.
                let within = bit_offset % 8;
                member.offset = bytes_of(bit_offset);
                member.bit_offset = Some(within as u32);
                member.access_bytes = Some((within + bit_width).div_ceil(8) as u32);
            } else {
                // Advance only when the field would otherwise straddle a unit
                // boundary, then read and write it through the
                // `sizeof(T)`-aligned window it provably sits inside. That
                // window is wider than the field needs and can span a
                // neighbouring member, which costs nothing -- a store is a
                // read-modify-write, so the neighbour's bits are put back
                // unchanged -- but it does mean the struct has to be big
                // enough to contain the window.
                if bit_offset % unit_bits + bit_width > unit_bits {
                    bit_offset = bit_offset.next_multiple_of(unit_bits);
                }
                let offset_bits = bit_offset / unit_bits * unit_bits;
                member.offset = bytes_of(offset_bits);
                member.bit_offset = Some((bit_offset - offset_bits) as u32);
                member.access_bytes = Some(unit_bytes as u32);
                window_end = window_end.max(offset_bits + unit_bits);
            }

            bit_offset += bit_width;
        }

        let final_align = max_align.max(zero_width_align);
        // `window_end` is a multiple of 8, so rounding the larger of the two
        // up to the alignment is the byte round-up and the padding at once.
        let size_bits = bit_offset
            .max(window_end)
            .next_multiple_of(final_align as u128 * 8);
        (bytes_of(size_bits), final_align)
    }

    /// The alignment a struct or union member is laid out at, and contributes
    /// to its aggregate's: the one rule for `packed` (on the member or the
    /// whole aggregate), `_Alignas`/`aligned` written on the member, and
    /// `#pragma pack(n)`, in gcc's order.
    ///
    /// `packed` drops the type's alignment to 1 -- including an alignment the
    /// type itself carries from a typedef's `aligned`. An alignment written on
    /// the member then raises it, and never lowers it, so `aligned(1)` on an
    /// `int` member leaves it at 4. A pack cap lowers the result last, written
    /// alignment included, and never raises it: `#pragma pack(8)` leaves an
    /// int at 4. Every answer here is gcc's, the same on x86-64 and aarch64.
    pub fn member_alignment(&self, member: &StructMember, pack_cap: Option<u32>) -> usize {
        let base = if member.align.packed {
            1
        } else {
            self.alignment(member.typ)
        };
        let raised = member.align.written.map_or(base, |w| base.max(w as usize));
        pack_cap.map_or(raised, |cap| raised.min(cap as usize))
    }

    /// Whether a bit-field is laid out packed -- at the next free bit, through
    /// an access span of exactly the bytes it touches -- rather than by the
    /// unit rule. Any packing does it, `#pragma pack(n)` at any `n` included.
    fn packs_bitfield(member: &StructMember, pack_cap: Option<u32>) -> bool {
        member.align.packed || pack_cap.is_some()
    }

    /// Get the number of interned types
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.types.len()
    }

    /// Compute union layout (all members at offset 0)
    /// Returns (total_size, alignment)
    /// Members are aligned by [`Self::member_alignment`], as in a struct. A
    /// union's size still follows its widest member; only the alignment, and
    /// so the trailing padding, can change.
    pub fn compute_union_layout(
        &self,
        members: &mut [StructMember],
        pack_cap: Option<u32>,
    ) -> (usize, usize) {
        let mut max_size = 0usize;
        let mut max_align = 1usize;
        // As in a struct: a zero-width bitfield's alignment demand, where the
        // ABI makes one, is not capped by packing.
        let mut zero_width_align = 1usize;

        for member in members.iter_mut() {
            member.offset = 0;
            // Every union member starts at bit zero, but a bitfield still has
            // to be recorded as one: without a width the accessors read and
            // write the whole declared type, so `union { int a:4; unsigned b; }`
            // with `b` set to 15 read `a` back as 15 rather than -1.
            //
            // A member that is not a bitfield is stated to have no bit offset
            // and no storage unit, rather than left as it was found: this
            // computes a layout, so every field of it is an output, and
            // `compute_struct_layout` clears the same two for the same reason.
            if let Some(w) = member.bit_width.filter(|w| *w > 0) {
                member.bit_offset = Some(0);
                // Packed, the span is the bytes the field's own bits touch, as
                // in a struct: `packed union { unsigned a:20; char c; }` is 3
                // bytes under gcc, not 4. Unpacked the two spellings coincide
                // on both targets, and gating on the cap keeps that output
                // bit-identical.
                member.access_bytes = Some(if Self::packs_bitfield(member, pack_cap) {
                    w.div_ceil(8)
                } else {
                    self.size_bytes(member.typ) as u32
                });
            } else {
                member.bit_offset = None;
                member.access_bytes = None;
            }

            // A zero-width bitfield is not an object: it occupies no storage
            // and so contributes neither size nor alignment to the union --
            // gcc makes `union { char c; int :0; }` one byte. The boundary it
            // forces in a struct has no meaning in a union, where every member
            // starts at bit zero.
            if member.bit_width == Some(0) {
                // Except on AAPCS64, where it still demands its type's
                // alignment -- and, as in a struct, packing does not suppress
                // that. The union's size follows from the rounding.
                if self.target_arch == Arch::Aarch64 {
                    zero_width_align = zero_width_align.max(self.alignment(member.typ));
                }
                continue;
            }

            // A packed bit-field contributes only the bytes it occupies, which
            // is what makes `packed union { unsigned a:20; char c; }` three
            // bytes rather than four.
            let member_size = match member.bit_width {
                Some(w) if w > 0 && Self::packs_bitfield(member, pack_cap) => {
                    w.div_ceil(8) as usize
                }
                _ => self.size_bytes(member.typ),
            };
            max_size = max_size.max(member_size);
            max_align = max_align.max(self.member_alignment(member, pack_cap));
        }

        let max_align = max_align.max(zero_width_align);
        // Saturating, as in a struct: a size past `max_object_bytes` is
        // refused by the caller, and rounding must not wrap it small first.
        let size = max_size
            .checked_next_multiple_of(max_align)
            .unwrap_or(usize::MAX);
        (size, max_align)
    }
}

#[cfg(test)]
mod tests {
    /// A bit-field's own bytes: from the byte its first bit is in to the
    /// byte its last bit is in, never empty -- narrower than the access span,
    /// which covers bytes other members own.
    #[test]
    fn a_bitfield_owns_the_bytes_its_bits_reach() {
        use super::{own_bit_bytes, Bitfield};
        assert_eq!(own_bit_bytes(4, 0, 1), 4..5);
        assert_eq!(own_bit_bytes(4, 7, 2), 4..6, "straddles a byte boundary");
        assert_eq!(own_bit_bytes(4, 8, 8), 5..6);
        assert_eq!(
            own_bit_bytes(0, 3, 0),
            0..1,
            "zero width still names a byte"
        );
        let bf = Bitfield::from_parts(8, Some(12), Some(4), Some(4)).unwrap();
        assert_eq!(bf.own_bytes(), 9..10);
        assert_eq!(
            Bitfield::from_parts(8, Some(0), Some(0), Some(4)),
            None,
            "zero width"
        );
        assert_eq!(
            Bitfield::from_parts(8, None, None, None),
            None,
            "not a bit-field"
        );
    }

    /// `is_plain_int128` separates the two types `kind()` cannot.
    #[test]
    fn is_plain_int128_excludes_the_complex_one() {
        let mut types = TypeTable::new(&crate::target::Target::host());
        let plain = types.int128_id;
        let complex = types.intern(Type::with_modifiers(
            TypeKind::Int128,
            TypeModifiers::COMPLEX,
        ));
        // Both answer the same `kind`, which is the whole hazard.
        assert_eq!(types.kind(plain), types.kind(complex));
        assert!(types.is_plain_int128(plain));
        assert!(!types.is_plain_int128(complex));
        // And nothing else is one.
        assert!(!types.is_plain_int128(types.long_id));
        assert!(!types.is_plain_int128(types.double_id));
    }

    /// `volatile` anywhere inside, and nothing through a pointer.
    ///
    /// The qualifier on a member is recorded on `StructMember::typ`, not on
    /// the aggregate, so `modifiers()` of the struct cannot see it -- which is
    /// how `loadfwd` came to forward a load of such a struct across a second
    /// copy of it.
    #[test]
    fn contains_volatile_looks_inside_but_not_through_a_pointer() {
        let mut types = TypeTable::new(&crate::target::Target::host());
        let int = types.int_id;
        let vol_int = types.intern(Type::with_modifiers(TypeKind::Int, TypeModifiers::VOLATILE));
        let member = |typ| StructMember {
            name: StringId::EMPTY,
            typ,
            offset: 0,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            align: MemberAlign::NATURAL,
        };
        let composite = |members| CompositeType {
            tag: None,
            members,
            enum_constants: Vec::new(),
            size: 8,
            align: 4,
            member_align: 4,
            is_complete: true,
            transparent: false,
            anon_id: None,
            tag_type: None,
        };

        // The plain scalars.
        assert!(!types.contains_volatile(int));
        assert!(types.contains_volatile(vol_int));

        // A member carries it to the aggregate, in a struct and in a union.
        let plain_struct = types.intern(Type::struct_type(composite(vec![member(int)])));
        let vol_struct = types.intern(Type::struct_type(composite(vec![
            member(int),
            member(vol_int),
        ])));
        assert!(!types.contains_volatile(plain_struct));
        assert!(types.contains_volatile(vol_struct));
        let vol_union = types.intern(Type::union_type(composite(vec![member(vol_int)])));
        assert!(types.contains_volatile(vol_union));

        // Through another level of nesting, and through an array of them.
        let nested = types.intern(Type::struct_type(composite(vec![member(vol_struct)])));
        assert!(types.contains_volatile(nested));
        let arr = types.intern(Type::array(vol_struct, 4));
        assert!(types.contains_volatile(arr));
        let plain_arr = types.intern(Type::array(plain_struct, 4));
        assert!(!types.contains_volatile(plain_arr));

        // But not through a pointer: `volatile int *p` makes `*p` volatile,
        // not `p`, and a pointer is the only way a type reaches itself.
        let ptr = types.intern(Type::pointer(vol_struct));
        assert!(!types.contains_volatile(ptr));
        let ptr_to_vol_int = types.intern(Type::pointer(vol_int));
        assert!(!types.contains_volatile(ptr_to_vol_int));

        // An incomplete composite has nothing to prove itself with, and
        // answers the way that forbids an optimization.
        let opaque = types.intern(Type::struct_type(CompositeType::incomplete(None)));
        assert!(types.contains_volatile(opaque));
    }

    /// `qualifiers`, `qualified_with` and `unqualified` on the same type.
    ///
    /// The pair has to be exact inverses on the top-level qualifiers and to
    /// leave everything else -- the kind, the size spellings, the storage
    /// class -- alone, because they are what forms and unforms the
    /// so-qualified type of a member access.
    #[test]
    fn qualifying_a_type_adds_and_removes_only_the_qualifiers() {
        let mut types = TypeTable::new(&crate::target::Target::host());
        let int = types.int_id;
        assert!(types.qualifiers(int).is_empty());

        // Adding, one qualifier at a time and then both.
        let vol = types.qualified_with(int, TypeModifiers::VOLATILE);
        assert_eq!(types.qualifiers(vol), TypeModifiers::VOLATILE);
        assert_eq!(types.kind(vol), TypeKind::Int);
        assert_eq!(types.size_bits(vol), types.size_bits(int));
        let cv = types.qualified_with(vol, TypeModifiers::CONST);
        assert_eq!(
            types.qualifiers(cv),
            TypeModifiers::CONST | TypeModifiers::VOLATILE
        );

        // Interned, so the same request answers with the same id -- and a type
        // that already carries the qualifier is returned untouched.
        assert_eq!(types.qualified_with(int, TypeModifiers::VOLATILE), vol);
        assert_eq!(types.qualified_with(vol, TypeModifiers::VOLATILE), vol);
        assert_eq!(types.qualified_with(int, TypeModifiers::empty()), int);

        // Nothing outside `Type::QUALIFIERS` travels: a storage class is a
        // property of a declaration, not of a type.
        assert_eq!(types.qualified_with(int, TypeModifiers::STATIC), int);

        // And back down again.
        assert_eq!(types.unqualified(cv), int);
        assert_eq!(types.unqualified(vol), int);
        assert_eq!(types.unqualified(int), int);

        // A qualifier below the top level is not a top-level qualifier:
        // `volatile int *` is an ordinary pointer.
        let ptr_to_vol = types.intern(Type::pointer(vol));
        assert!(types.qualifiers(ptr_to_vol).is_empty());
        assert_eq!(types.unqualified(ptr_to_vol), ptr_to_vol);

        // Qualifying an array qualifies its element type (C17 6.7.3p10), at
        // every level, and the array itself stays unqualified -- which is what
        // a subscript of it then reads.
        let arr = types.intern(Type::array(int, 4));
        let const_arr = types.qualified_with(arr, TypeModifiers::CONST);
        assert!(types.qualifiers(const_arr).is_empty());
        let elem = types.base_type(const_arr).expect("element type");
        assert_eq!(types.qualifiers(elem), TypeModifiers::CONST);
        let rows = types.intern(Type::array(arr, 2));
        let vol_rows = types.qualified_with(rows, TypeModifiers::VOLATILE);
        let row = types.base_type(vol_rows).expect("row type");
        let cell = types.base_type(row).expect("cell type");
        assert_eq!(types.qualifiers(cell), TypeModifiers::VOLATILE);
        assert!(types.contains_volatile(vol_rows));
    }

    /// C17 6.5.2.3p3/p4: a member access has the *so-qualified* version of the
    /// member's type.
    ///
    /// `find_member` answers with the declared type, which is what an offset or
    /// a width is read from; the access has the object's qualifiers as well,
    /// which is what makes a member of a `volatile` object volatile and a
    /// member of a `const` object unassignable.
    #[test]
    fn a_member_access_is_qualified_by_the_object() {
        let mut types = TypeTable::new(&crate::target::Target::host());
        let mut idents = crate::strings::StringTable::new();
        let a = idents.intern("a");
        let v = idents.intern("v");
        let int = types.int_id;
        let vol_int = types.intern(Type::with_modifiers(TypeKind::Int, TypeModifiers::VOLATILE));
        let member = |name, typ| StructMember {
            name,
            typ,
            offset: 0,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            align: MemberAlign::NATURAL,
        };
        let tag = idents.intern("S");
        let composite = CompositeType {
            tag: Some(tag),
            members: vec![member(a, int), member(v, vol_int)],
            enum_constants: Vec::new(),
            size: 8,
            align: 4,
            member_align: 4,
            is_complete: true,
            transparent: false,
            anon_id: None,
            tag_type: None,
        };
        let plain = types.intern(Type::struct_type(composite));

        // An unqualified object: the declared types, unchanged.
        assert_eq!(types.member_access_type(plain, a), Some(int));
        assert_eq!(types.member_access_type(plain, v), Some(vol_int));
        assert_eq!(types.member_access_type(plain, tag), None);

        // A `volatile` object makes every member volatile, and a `const` one
        // makes every member `const`.
        let vol_obj = types.qualified_with(plain, TypeModifiers::VOLATILE);
        let from_vol = types.member_access_type(vol_obj, a).unwrap();
        assert_eq!(types.qualifiers(from_vol), TypeModifiers::VOLATILE);
        assert!(types.contains_volatile(from_vol));
        let const_obj = types.qualified_with(plain, TypeModifiers::CONST);
        let from_const = types.member_access_type(const_obj, a).unwrap();
        assert_eq!(types.qualifiers(from_const), TypeModifiers::CONST);

        // `_Atomic` does not travel: a member of an `_Atomic` struct cannot be
        // read atomically, and gcc does not claim it can.
        let atomic_obj = types.qualified_with(plain, TypeModifiers::ATOMIC);
        assert_eq!(types.member_access_type(atomic_obj, a), Some(int));

        // A `volatile struct S` written before the definition is a copy of
        // the tag that the definition completes, and keeps its `volatile`:
        // the members come from the copy itself, not from the tag found by
        // name, and the qualifiers with them.
        let incomplete = types.intern(Type::struct_type(CompositeType::incomplete(Some(tag))));
        let vol_incomplete = types.qualified_with(incomplete, TypeModifiers::VOLATILE);
        assert_eq!(types.member_access_type(vol_incomplete, a), None);
        let definition = types.get(plain).composite.as_deref().unwrap().clone();
        types.complete_struct(incomplete, definition);
        assert_eq!(types.member_access_type(vol_incomplete, a), Some(vol_int));
    }

    use super::*;

    /// `make_complex` and `complex_base` must be exact inverses, for every
    /// arithmetic base and whatever qualifiers the type carries.
    ///
    /// Each half of them had its own bug. `complex_base` answered with the
    /// complex type *itself* for an integer base, so both halves of a
    /// `_Complex int` were read from the same address and the imaginary store
    /// overran the object. `make_complex` looked the base up by exact
    /// `TypeId`, so a qualified or typedef'd `long double` missed and took the
    /// `_Complex double` fallback -- `__builtin_complex(7.0L, 8.0L)` built a
    /// 16-byte value for a 32-byte type.
    #[test]
    fn test_complex_base_and_make_complex_are_inverses() {
        // Every target, not just the host: plain `char` is signed on x86-64
        // and Apple arm64 and unsigned on aarch64 Linux, and canonicalizing
        // it by `is_unsigned` rather than by its modifiers sent plain `char`
        // to `unsigned char` on one of them -- so the round trip held here
        // and broke on CI.
        for target in [
            Target::new(Arch::X86_64, Os::Linux),
            Target::new(Arch::Aarch64, Os::Linux),
            Target::new(Arch::Aarch64, Os::MacOS),
        ] {
            check_complex_round_trip(&target);
        }
    }

    /// A complex type is a composite of its two halves even though `kind()`
    /// answers its base's kind -- the reason the predicate exists.
    #[test]
    fn test_is_aggregate_or_complex() {
        let mut types = TypeTable::new(&Target::new(Arch::X86_64, Os::Linux));
        let complex_int = types.make_complex(types.int_id);
        let array = types.intern(Type::array(types.int_id, 4));
        for id in [
            types.complex_float_id,
            types.complex_double_id,
            complex_int,
            array,
        ] {
            assert!(types.is_aggregate_or_complex(id), "{id:?}");
        }
        let ptr = types.pointer_to(types.int_id);
        for id in [
            types.double_id,
            types.int_id,
            types.longdouble_id,
            types.int128_id,
            ptr,
        ] {
            assert!(!types.is_aggregate_or_complex(id), "{id:?}");
        }
    }

    fn check_complex_round_trip(target: &Target) {
        let mut types = TypeTable::new(target);
        let bases = [
            types.char_id,
            types.schar_id,
            types.uchar_id,
            types.short_id,
            types.ushort_id,
            types.int_id,
            types.uint_id,
            types.long_id,
            types.ulong_id,
            types.longlong_id,
            types.ulonglong_id,
            types.int128_id,
            types.uint128_id,
            types.float_id,
            types.double_id,
            types.longdouble_id,
            types.float16_id,
            types.float128_id,
        ];
        for base in bases {
            let cplx = types.make_complex(base);
            assert!(types.is_complex(cplx), "make_complex gave a real type");
            assert_eq!(
                types.complex_base(cplx),
                base,
                "round trip failed for kind {:?}",
                types.kind(base)
            );
            // Two halves, never one.
            assert_eq!(
                types.size_bits(cplx),
                2 * types.size_bits(base),
                "size of complex-of-{:?} is not twice its base",
                types.kind(base)
            );
            // Alignment is the base's, not the pair's.
            assert_eq!(types.alignment(cplx), types.alignment(base));
            // And the two families are told apart.
            assert_eq!(types.is_complex_integer(cplx), types.is_integer(base));
            assert_eq!(types.is_complex_float(cplx), !types.is_integer(base));
            // Already complex: idempotent.
            assert_eq!(types.make_complex(cplx), cplx);
        }

        // Keyed on the kind, so a qualifier cannot make it miss. This is the
        // case that produced a `_Complex double` for a `long double`.
        for base in [types.longdouble_id, types.int_id, types.uint_id] {
            let mut qualified = types.get(base).clone();
            qualified.modifiers |= TypeModifiers::CONST;
            let qualified = types.intern(qualified);
            assert_ne!(qualified, base, "the qualified type should be distinct");
            assert_eq!(
                types.make_complex(qualified),
                types.make_complex(base),
                "a qualified {:?} found a different complex type",
                types.kind(base)
            );
        }
    }

    /// The usual arithmetic conversions run on the *real parts* (C17
    /// 6.3.1.8p1), and re-complexify the answer.
    ///
    /// A complex type carries its base's kind, so calling it floating -- which
    /// is what asking `is_complex` did -- matched none of the floating kinds
    /// and fell through to the `_Float16` default. `_Complex int * _Complex
    /// int` came out `_Complex _Float16`: both operands were rounded to half
    /// precision and multiplied by `__muldc3`.
    #[test]
    fn test_common_type_of_complex_operands() {
        let types = TypeTable::new(&Target::host());
        let c = |t| types.make_complex(t);
        let cases = [
            // Both complex integers: complex of the common integer type.
            (c(types.int_id), c(types.int_id), c(types.int_id)),
            (c(types.int_id), c(types.long_id), c(types.long_id)),
            (c(types.int_id), c(types.uint_id), c(types.uint_id)),
            // A complex integer against a real one: still complex.
            (c(types.int_id), types.int_id, c(types.int_id)),
            (c(types.int_id), types.long_id, c(types.long_id)),
            // Sub-`int` halves promote before ranking, as real ones do.
            (c(types.schar_id), c(types.schar_id), c(types.int_id)),
            (c(types.short_id), types.int_id, c(types.int_id)),
            // Complex is contagious across the families, at the wider base.
            (c(types.int_id), types.double_id, c(types.double_id)),
            (c(types.int_id), c(types.double_id), c(types.double_id)),
            (c(types.float_id), c(types.int_id), c(types.float_id)),
            // And the floating cases are unchanged.
            (c(types.double_id), c(types.double_id), c(types.double_id)),
            (
                c(types.double_id),
                types.longdouble_id,
                c(types.longdouble_id),
            ),
            (c(types.float_id), types.double_id, c(types.double_id)),
            // Two real operands stay real.
            (types.int_id, types.long_id, types.long_id),
            (types.float_id, types.double_id, types.double_id),
        ];
        for (l, r, want) in cases {
            for (a, b) in [(l, r), (r, l)] {
                let got = types.common_type(a, b);
                assert_eq!(
                    (types.kind(got), types.size_bits(got), types.is_complex(got)),
                    (
                        types.kind(want),
                        types.size_bits(want),
                        types.is_complex(want)
                    ),
                    "common_type({:?}{}, {:?}{})",
                    types.kind(a),
                    if types.is_complex(a) { " complex" } else { "" },
                    types.kind(b),
                    if types.is_complex(b) { " complex" } else { "" },
                );
            }
        }
    }

    #[test]
    fn test_basic_types() {
        let types = TypeTable::new(&Target::host());
        assert!(types.is_integer(types.int_id));
        assert!(types.is_arithmetic(types.int_id));
        assert!(types.is_scalar(types.int_id));
        assert!(!types.is_float(types.int_id));
    }

    /// `transparent_union_first_member` is the guard every call site uses, so
    /// it must answer `None` for everything that is not a transparent union --
    /// an ordinary union most of all.
    #[test]
    fn test_transparent_union_first_member() {
        let mut types = TypeTable::new(&Target::host());
        let int_ptr = types.intern(Type::pointer(types.int_id));
        let char_ptr = types.intern(Type::pointer(types.char_id));
        let member = |typ, bit_width| StructMember {
            name: StringId::EMPTY,
            typ,
            offset: 0,
            bit_offset: None,
            bit_width,
            access_bytes: None,
            align: MemberAlign::NATURAL,
        };
        let composite = |members| CompositeType {
            tag: None,
            members,
            enum_constants: Vec::new(),
            size: 8,
            align: 8,
            member_align: 8,
            is_complete: true,
            transparent: false,
            anon_id: None,
            tag_type: None,
        };

        // An ordinary union answers None even though it has members.
        let plain = types.intern(Type::union_type(composite(vec![
            member(int_ptr, None),
            member(char_ptr, None),
        ])));
        assert_eq!(types.transparent_union_first_member(plain), None);

        // Marked, it answers its first member.
        types.set_transparent_union(plain);
        assert_eq!(types.transparent_union_first_member(plain), Some(int_ptr));

        // A struct is never transparent, whatever the flag says.
        let st = types.intern(Type::struct_type(composite(vec![member(int_ptr, None)])));
        types.set_transparent_union(st);
        assert_eq!(types.transparent_union_first_member(st), None);

        // A zero-width bit-field is not a declared member for this purpose.
        // `members` is unfiltered, so one sitting first would otherwise decide
        // the whole ABI of the union.
        let zw = types.intern(Type::union_type(composite(vec![
            member(types.int_id, Some(0)),
            member(char_ptr, None),
        ])));
        types.set_transparent_union(zw);
        assert_eq!(types.transparent_union_first_member(zw), Some(char_ptr));
    }

    /// A cast to union selects a member by type compatibility alone: the
    /// first named, non-bit-field member whose type matches, a qualified
    /// member included, and nothing a conversion would reach.
    #[test]
    fn test_union_member_for_cast() {
        let mut types = TypeTable::new(&Target::host());
        let mut idents = crate::strings::StringTable::new();
        let (bf, ci, l, d, d2) = (
            idents.intern("bf"),
            idents.intern("ci"),
            idents.intern("l"),
            idents.intern("d"),
            idents.intern("d2"),
        );
        let const_int = types.qualified_with(types.int_id, TypeModifiers::CONST);
        let member = |name, typ, bit_width| StructMember {
            name,
            typ,
            offset: 0,
            bit_offset: None,
            bit_width,
            access_bytes: None,
            align: MemberAlign::NATURAL,
        };
        let composite = |members| CompositeType {
            tag: None,
            members,
            enum_constants: Vec::new(),
            size: 8,
            align: 8,
            member_align: 8,
            is_complete: true,
            transparent: false,
            anon_id: None,
            tag_type: None,
        };
        let members = vec![
            member(bf, types.int_id, Some(3)),
            member(StringId::EMPTY, types.char_id, None),
            member(ci, const_int, None),
            member(l, types.long_id, None),
            member(d, types.double_id, None),
            member(d2, types.double_id, None),
        ];
        let u = types.intern(Type::union_type(composite(members.clone())));

        // The bit-field is skipped; the const member matches a plain `int`.
        assert_eq!(types.union_member_for_cast(u, types.int_id), Some(ci));
        assert_eq!(types.union_member_for_cast(u, types.long_id), Some(l));
        // The first of two matching members.
        assert_eq!(types.union_member_for_cast(u, types.double_id), Some(d));
        // No conversion: neither `float` nor `short` matches, and the
        // unnamed `char` member cannot be designated.
        assert_eq!(types.union_member_for_cast(u, types.float_id), None);
        assert_eq!(types.union_member_for_cast(u, types.short_id), None);
        assert_eq!(types.union_member_for_cast(u, types.char_id), None);

        // A struct is not cast to by member.
        let st = types.intern(Type::struct_type(composite(members)));
        assert_eq!(types.union_member_for_cast(st, types.long_id), None);
    }

    #[test]
    fn test_pointer_type() {
        let mut types = TypeTable::new(&Target::host());
        let int_ptr_id = types.intern(Type::pointer(types.int_id));
        assert_eq!(types.kind(int_ptr_id), TypeKind::Pointer);
        assert!(types.is_scalar(int_ptr_id));
        assert!(!types.is_integer(int_ptr_id));

        let base_id = types.base_type(int_ptr_id).unwrap();
        assert_eq!(types.kind(base_id), TypeKind::Int);
    }

    #[test]
    fn test_array_type() {
        let mut types = TypeTable::new(&Target::host());
        let int_arr_id = types.intern(Type::array(types.int_id, 10));
        assert_eq!(types.kind(int_arr_id), TypeKind::Array);
        assert_eq!(types.array_size(int_arr_id), Some(10));

        let base_id = types.base_type(int_arr_id).unwrap();
        assert_eq!(types.kind(base_id), TypeKind::Int);
    }

    #[test]
    fn test_function_type() {
        let mut types = TypeTable::new(&Target::host());
        let func_id = types.intern(Type::function(
            types.int_id,
            vec![types.int_id, types.char_id],
            false,
            false,
        ));
        assert_eq!(types.kind(func_id), TypeKind::Function);
        assert!(!types.is_variadic(func_id));

        let params = types.params(func_id).unwrap();
        assert_eq!(params.len(), 2);
        assert_eq!(types.kind(params[0]), TypeKind::Int);
        assert_eq!(types.kind(params[1]), TypeKind::Char);
    }

    /// An array and a function decay to a pointer; the immutable lookup gives
    /// the interned pointer when there is one and `void *` otherwise, and
    /// either is a full-width pointer.
    #[test]
    fn test_decayed_value_is_an_address() {
        let mut types = TypeTable::new(&Target::host());
        let arr = types.intern(Type::array(types.int_id, 10));
        let func = types.intern(Type::function(types.long_id, vec![], false, false));
        let ptr_bits = types.size_bits(types.void_ptr_id);
        for t in [arr, func] {
            assert!(types.decays(t));
            let v = types.decayed_value(t);
            assert_eq!(types.kind(v), TypeKind::Pointer);
            assert_eq!(types.size_bits(v), ptr_bits);
        }
        let int_ptr = types.decayed(arr);
        assert_eq!(types.decayed_value(arr), int_ptr);
        let fn_ptr = types.decayed(func);
        assert_eq!(types.decayed_value(func), fn_ptr);
        assert!(!types.decays(types.long_id));
        assert_eq!(types.decayed_value(types.long_id), types.long_id);
        assert!(!types.decays(int_ptr));
    }

    /// gcc's `sizeof` of a function type is 1, as `void`'s is, and it is
    /// what arithmetic on a pointer to one steps by; the type still has no
    /// value width. `arithmetic_pointee` answers the function itself for a
    /// function and a pointer to one, where `base_type` answers its return
    /// type.
    #[test]
    fn test_function_type_steps_by_one_byte() {
        let mut types = TypeTable::new(&Target::host());
        let func = types.intern(Type::function(types.long_id, vec![], false, false));
        let fn_ptr = types.decayed(func);
        let arr = types.intern(Type::array(types.int_id, 10));
        assert_eq!(types.size_bytes(func), 1);
        assert_eq!(types.size_bits(func), 0);
        assert_eq!(types.arithmetic_pointee(func), Some(func));
        assert_eq!(types.arithmetic_pointee(fn_ptr), Some(func));
        assert_eq!(types.base_type(func), Some(types.long_id));
        assert_eq!(types.arithmetic_pointee(arr), Some(types.int_id));
        assert_eq!(
            types.arithmetic_pointee(types.void_ptr_id),
            Some(types.void_id)
        );
        assert_eq!(types.arithmetic_pointee(types.long_id), None);
    }

    #[test]
    fn test_unsigned_modifier() {
        let types = TypeTable::new(&Target::host());
        assert!(types.is_unsigned(types.uint_id));
        assert!(!types.is_unsigned(types.int_id));
    }

    #[test]
    fn test_unsigned_of_size() {
        let types = TypeTable::new(&Target::host());
        assert_eq!(types.unsigned_of_size(1), Some(types.uchar_id));
        assert_eq!(types.unsigned_of_size(2), Some(types.ushort_id));
        assert_eq!(types.unsigned_of_size(4), Some(types.uint_id));
        assert_eq!(types.unsigned_of_size(8), Some(types.ulong_id));
        assert_eq!(types.unsigned_of_size(3), None);
        assert_eq!(types.unsigned_of_size(16), None);
    }

    /// Plain `char`'s signedness is the target's (C17 6.2.5p15), and it is a
    /// different question from how the type is spelled. `signed char` and
    /// `unsigned char` say what they are on every target; only bare `char`
    /// moves.
    /// C17 6.5.2.2p6: the integer promotions, and `float` to `double`; a
    /// complex type is not a `float` although `kind` answers `Float` for it.
    #[test]
    fn test_default_argument_promotions() {
        let types = TypeTable::new(&Target::new(Arch::Aarch64, Os::MacOS));
        for (from, to) in [
            (types.bool_id, types.int_id),
            (types.char_id, types.int_id),
            (types.uchar_id, types.int_id),
            (types.short_id, types.int_id),
            (types.ushort_id, types.int_id),
            (types.int_id, types.int_id),
            (types.long_id, types.long_id),
            (types.float_id, types.double_id),
            (types.float16_id, types.double_id),
            (types.double_id, types.double_id),
            (types.longdouble_id, types.longdouble_id),
            (types.complex_float_id, types.complex_float_id),
            (types.void_ptr_id, types.void_ptr_id),
        ] {
            assert_eq!(types.default_argument_promote(from), to, "{from:?}");
        }
    }

    #[test]
    fn test_plain_char_signedness_follows_the_target() {
        // The x86-64 psABI makes plain char signed on Linux and Darwin alike;
        // AAPCS64 makes it unsigned, and Apple arm64 overrides that to signed.
        let tables: Vec<_> = [
            (Arch::X86_64, Os::Linux, false),
            (Arch::X86_64, Os::MacOS, false),
            (Arch::Aarch64, Os::Linux, true),
            (Arch::Aarch64, Os::MacOS, false),
        ]
        .into_iter()
        .map(|(arch, os, unsigned)| {
            let t = TypeTable::new(&Target::new(arch, os));
            assert_eq!(t.is_unsigned(t.char_id), unsigned, "{arch}-{os}");
            assert_eq!(
                t.plain_char() == CharSignedness::Unsigned,
                unsigned,
                "{arch}-{os}"
            );
            t
        })
        .collect();
        let arm = &tables[2];

        // The explicit spellings do not move with the target.
        for t in &tables {
            assert!(!t.is_unsigned(t.schar_id), "signed char is always signed");
            assert!(
                t.is_unsigned(t.uchar_id),
                "unsigned char is always unsigned"
            );
            assert!(t.is_unsigned(t.bool_id), "_Bool is always unsigned");
            assert!(!t.is_unsigned(t.int_id));
        }

        // Spelling is target-independent, and is what a type printer asks.
        // Plain `char` is unsigned on aarch64 Linux and is still written `char`.
        for t in &tables {
            assert!(!t.spelled_unsigned(t.char_id));
            assert!(!t.spelled_unsigned(t.schar_id));
            assert!(t.spelled_unsigned(t.uchar_id));
            assert!(!t.spelled_unsigned(t.bool_id));
        }
        assert_eq!(arm.format_type(arm.char_id, None), "char");
        assert_eq!(arm.format_type(arm.uchar_id, None), "unsigned char");
    }

    /// C17 6.3.1.1p2: everything of lesser rank than `int` becomes `int`,
    /// including the unsigned spellings -- `int` represents every value of
    /// `unsigned char` and `unsigned short`, so neither promotes to
    /// `unsigned int`.
    #[test]
    fn test_integer_promote() {
        let types = TypeTable::new(&Target::host());

        for narrow in [
            types.bool_id,
            types.char_id,
            types.schar_id,
            types.uchar_id,
            types.short_id,
            types.ushort_id,
        ] {
            assert_eq!(
                types.integer_promote(narrow),
                types.int_id,
                "{} should promote to int",
                types.format_type(narrow, None)
            );
        }

        // int and everything wider is returned unchanged.
        for wide in [
            types.int_id,
            types.uint_id,
            types.long_id,
            types.ulong_id,
            types.longlong_id,
            types.double_id,
        ] {
            assert_eq!(types.integer_promote(wide), wide);
        }
    }

    /// `unsized_array_levels` counts the array levels that need a size
    /// expression supplied from outside. It is what says whether a list of
    /// such expressions describes the whole type or only part of it, so
    /// `int[][n]` (two unsized levels, one expression) can be told from
    /// `int[n]` (one and one).
    #[test]
    fn test_unsized_array_levels() {
        let mut types = TypeTable::new(&Target::host());
        let int_id = types.int_id;

        // Not an array at all.
        assert_eq!(types.unsized_array_levels(int_id), 0);

        // Fully sized arrays need nothing.
        let a4 = types.intern(Type::array(int_id, 4));
        assert_eq!(types.unsized_array_levels(a4), 0);
        let a3x4 = types.intern(Type::array(a4, 3));
        assert_eq!(types.unsized_array_levels(a3x4), 0);

        // An absent extent counts once, at whichever level it sits. This is
        // how a variably-modified dimension is represented: the size lives in
        // a side-channel expression, not in the type.
        fn unsized_array(types: &mut TypeTable, base: TypeId) -> TypeId {
            let mut t = Type::array(base, 0);
            t.array_size = None;
            types.intern(t)
        }
        let an = unsized_array(&mut types, int_id);
        assert_eq!(types.unsized_array_levels(an), 1);

        // `int[3][n]`: outer sized, inner not.
        let a3xn = types.intern(Type::array(an, 3));
        assert_eq!(types.unsized_array_levels(a3xn), 1);

        // `int[n][m]`: both absent.
        let anxm = unsized_array(&mut types, an);
        assert_eq!(types.unsized_array_levels(anxm), 2);

        // A pointer to a variably-modified array stops at the pointer: its
        // size is the pointer's, and nothing about its extent is needed.
        let ptr = types.intern(Type::pointer(an));
        assert_eq!(types.unsized_array_levels(ptr), 0);
    }

    /// A zero-width bit-field forces the next member to a boundary on every
    /// target, but whether it also raises the *aggregate's* alignment is
    /// ABI-specific (C17 6.7.2.1p12 leaves it open): the x86-64 psABI says no,
    /// AAPCS64 says it contributes its declared type's alignment. Both answers
    /// are gcc's on the respective target.
    ///
    /// Asserted here for both targets from one host, which the end-to-end test
    /// cannot do -- it only ever compiles for the machine it runs on.
    #[test]
    fn test_zero_width_bitfield_alignment_is_abi_specific() {
        fn layout(target: &Target, pack: Option<u32>, union_: bool) -> (usize, usize) {
            let types = TypeTable::new(target);
            // { char c; int :0; char d; }
            let mut members = vec![
                StructMember {
                    name: StringId::EMPTY,
                    typ: types.char_id,
                    offset: 0,
                    bit_offset: None,
                    bit_width: None,
                    access_bytes: None,
                    align: MemberAlign::NATURAL,
                },
                StructMember {
                    name: StringId::EMPTY,
                    typ: types.int_id,
                    offset: 0,
                    bit_offset: None,
                    bit_width: Some(0),
                    access_bytes: None,
                    align: MemberAlign::NATURAL,
                },
                StructMember {
                    name: StringId::EMPTY,
                    typ: types.char_id,
                    offset: 0,
                    bit_offset: None,
                    bit_width: None,
                    access_bytes: None,
                    align: MemberAlign::NATURAL,
                },
            ];
            if union_ {
                members.truncate(2);
                types.compute_union_layout(&mut members, pack)
            } else {
                types.compute_struct_layout(&mut members, pack)
            }
        }

        let x86 = Target::new(Arch::X86_64, Os::Linux);
        let arm = Target::new(Arch::Aarch64, Os::Linux);

        // Struct: the boundary applies everywhere, the alignment does not.
        assert_eq!(layout(&x86, None, false), (5, 1));
        assert_eq!(layout(&arm, None, false), (8, 4));

        // Packing caps an ordinary member's alignment but not this one.
        assert_eq!(layout(&x86, Some(1), false), (5, 1));
        assert_eq!(layout(&arm, Some(1), false), (8, 4));

        // Union: a zero-width bitfield occupies no storage, so it cannot
        // widen the union.
        assert_eq!(layout(&x86, None, true), (1, 1));
        assert_eq!(layout(&arm, None, true), (4, 4));
        assert_eq!(layout(&x86, Some(1), true), (1, 1));
        assert_eq!(layout(&arm, Some(1), true), (4, 4));
    }

    /// One rule aligns a member: `packed` (on it, or on its whole aggregate)
    /// drops its type's alignment to 1, an alignment written on it raises
    /// that, and a `#pragma pack` cap lowers the result last. Every row is
    /// gcc's, and gcc gives the same on x86-64 and aarch64.
    #[test]
    fn test_member_alignment_rule() {
        fn member(typ: TypeId, written: Option<u32>, packed: bool) -> StructMember {
            StructMember {
                name: StringId::EMPTY,
                typ,
                offset: 0,
                bit_offset: None,
                bit_width: None,
                access_bytes: None,
                align: MemberAlign { written, packed },
            }
        }
        for arch in [Arch::X86_64, Arch::Aarch64] {
            let mut types = TypeTable::new(&Target::new(arch, Os::Linux));
            let int = types.int_id;
            // `typedef int ai8 __attribute__((aligned(8)));`
            let ai8 = types.intern(Type {
                explicit_align: Some(8),
                ..types.get(int).clone()
            });
            for (m, cap, want) in [
                (member(int, None, false), None, 4),
                (member(int, None, true), None, 1),
                // `aligned(1)` alone cannot lower an int.
                (member(int, Some(1), false), None, 4),
                (member(int, Some(2), true), None, 2),
                (member(int, Some(8), true), None, 8),
                // `packed` drops a typedef's alignment too.
                (member(ai8, None, true), None, 1),
                (member(ai8, None, false), Some(2), 2),
                // The cap lowers a written alignment, and raises nothing.
                (member(int, Some(8), false), Some(1), 1),
                (member(int, Some(8), true), Some(2), 2),
                (member(int, None, false), Some(8), 4),
            ] {
                assert_eq!(
                    types.member_alignment(&m, cap),
                    want,
                    "{arch:?} {:?} cap {cap:?}",
                    m.align
                );
            }
        }
    }

    /// `packed` on one member moves only that member, and the aggregate's
    /// alignment is the largest of what its members then demand. Offsets
    /// and sizes are gcc's, on both targets.
    #[test]
    fn test_packed_member_layout() {
        let member = |typ, bit_width, packed| StructMember {
            name: StringId::EMPTY,
            typ,
            offset: 0,
            bit_offset: None,
            bit_width,
            access_bytes: None,
            align: MemberAlign {
                written: None,
                packed,
            },
        };
        for arch in [Arch::X86_64, Arch::Aarch64] {
            let types = TypeTable::new(&Target::new(arch, Os::Linux));
            let (c, i, l) = (types.char_id, types.int_id, types.long_id);

            // struct { char a; long b __attribute__((packed)); int c; }
            let mut m = vec![
                member(c, None, false),
                member(l, None, true),
                member(i, None, false),
            ];
            assert_eq!(types.compute_struct_layout(&mut m, None), (16, 4));
            assert_eq!((m[1].offset, m[2].offset), (1, 12));

            // struct { char a; int b:12 __attribute__((packed)); char c; }:
            // the field takes the next free bit and adds no alignment.
            let mut m = vec![
                member(c, None, false),
                member(i, Some(12), true),
                member(c, None, false),
            ];
            assert_eq!(types.compute_struct_layout(&mut m, None), (4, 1));
            assert_eq!((m[1].offset, m[1].bit_offset), (1, Some(0)));
            assert_eq!(m[1].access_bytes, Some(2));
            assert_eq!(m[2].offset, 3);

            // #pragma pack(2): struct { char a; int b __attribute__((packed)); int c; }
            let mut m = vec![
                member(c, None, false),
                member(i, None, true),
                member(i, None, false),
            ];
            assert_eq!(types.compute_struct_layout(&mut m, Some(2)), (10, 2));
            assert_eq!((m[1].offset, m[2].offset), (1, 6));

            // union { char a; int b:20 __attribute__((packed)); }
            let mut m = vec![member(c, None, false), member(i, Some(20), true)];
            assert_eq!(types.compute_union_layout(&mut m, None), (3, 1));

            // union { char a; int b __attribute__((packed)); int c; }
            let mut m = vec![
                member(c, None, false),
                member(i, None, true),
                member(i, None, false),
            ];
            assert_eq!(types.compute_union_layout(&mut m, None), (4, 4));
        }
    }

    #[test]
    fn test_type_format() {
        let types = TypeTable::new(&Target::host());
        assert_eq!(types.format_type(types.int_id, None), "int");
        assert_eq!(types.format_type(types.uint_id, None), "unsigned int");
    }

    /// A C type reads inside-out: spelled left to right it names a
    /// *different* type -- `int[8] *` reads as "array of pointers", not as
    /// `int (*)[8]`. Every row here was taken from `gcc -std=c17`'s own
    /// diagnostics.
    #[test]
    fn format_type_spells_a_declarator_not_a_suffix_chain() {
        let mut t = TypeTable::new(&Target::host());

        let arr8 = t.intern(Type::array(t.int_id, 8));
        let ptr_to_arr = t.intern(Type::pointer(arr8));
        assert_eq!(t.format_type(ptr_to_arr, None), "int (*)[8]");

        let int_ptr = t.intern(Type::pointer(t.int_id));
        let arr_of_ptr = t.intern(Type::array(int_ptr, 8));
        assert_eq!(t.format_type(arr_of_ptr, None), "int *[8]");

        // The two are different types and must not share a spelling.
        assert_ne!(
            t.format_type(ptr_to_arr, None),
            t.format_type(arr_of_ptr, None)
        );

        // Outermost extent first.
        let arr48 = t.intern(Type::array(arr8, 4));
        assert_eq!(t.format_type(arr48, None), "int[4][8]");
        let ptr_to_arr48 = t.intern(Type::pointer(arr48));
        assert_eq!(t.format_type(ptr_to_arr48, None), "int (*)[4][8]");

        // Pointers chain without spaces between the stars.
        let ptr_ptr = t.intern(Type::pointer(int_ptr));
        assert_eq!(t.format_type(ptr_ptr, None), "int **");

        // A function, and a pointer to one.
        let f_void = t.intern(Type::function(t.int_id, vec![], false, false));
        assert_eq!(t.format_type(f_void, None), "int(void)");
        let ptr_to_f = t.intern(Type::pointer(f_void));
        assert_eq!(t.format_type(ptr_to_f, None), "int (*)(void)");

        // 6.7.6.3p14: no prototype is not the same type as `(void)`, so the
        // two must not print the same either.
        let f_noproto = t.intern(Type::function_no_prototype(t.int_id, false));
        assert_eq!(t.format_type(f_noproto, None), "int()");
        assert_ne!(t.format_type(f_void, None), t.format_type(f_noproto, None));

        // An array of pointers to functions -- both suffixes and a pointer.
        let arr_of_fptr = t.intern(Type::array(ptr_to_f, 4));
        assert_eq!(t.format_type(arr_of_fptr, None), "int (*[4])(void)");

        // A qualifier on the pointer goes after the star, where the
        // declaration writes it.
        let mut cptr = Type::pointer(t.char_id);
        cptr.modifiers |= TypeModifiers::CONST;
        let const_ptr = t.intern(cptr);
        assert_eq!(t.format_type(const_ptr, None), "char * const");
    }

    #[test]
    fn test_nested_pointer() {
        let mut types = TypeTable::new(&Target::host());
        // int **pp
        let int_ptr_id = types.intern(Type::pointer(types.int_id));
        let int_ptr_ptr_id = types.intern(Type::pointer(int_ptr_id));
        assert_eq!(types.kind(int_ptr_ptr_id), TypeKind::Pointer);

        let inner_id = types.base_type(int_ptr_ptr_id).unwrap();
        assert_eq!(types.kind(inner_id), TypeKind::Pointer);

        let innermost_id = types.base_type(inner_id).unwrap();
        assert_eq!(types.kind(innermost_id), TypeKind::Int);
    }

    #[test]
    fn test_pointer_to_array() {
        let mut types = TypeTable::new(&Target::host());
        // int (*p)[10] - pointer to array of 10 ints
        let arr_id = types.intern(Type::array(types.int_id, 10));
        let ptr_to_arr_id = types.intern(Type::pointer(arr_id));

        assert_eq!(types.kind(ptr_to_arr_id), TypeKind::Pointer);
        let base_id = types.base_type(ptr_to_arr_id).unwrap();
        assert_eq!(types.kind(base_id), TypeKind::Array);
        assert_eq!(types.array_size(base_id), Some(10));
    }

    #[test]
    fn test_types_compatible_same_type() {
        let mut types = TypeTable::new(&Target::host());
        let int1 = types.intern(Type::basic(TypeKind::Int));
        let int2 = types.intern(Type::basic(TypeKind::Int));
        assert!(types.types_compatible(int1, int2));

        let char1 = types.intern(Type::basic(TypeKind::Char));
        let char2 = types.intern(Type::basic(TypeKind::Char));
        assert!(types.types_compatible(char1, char2));
    }

    #[test]
    fn test_types_compatible_different_types() {
        let mut types = TypeTable::new(&Target::host());
        let int_type = types.intern(Type::basic(TypeKind::Int));
        let char_type = types.intern(Type::basic(TypeKind::Char));
        assert!(!types.types_compatible(int_type, char_type));

        let long_type = types.intern(Type::basic(TypeKind::Long));
        assert!(!types.types_compatible(int_type, long_type));
    }

    #[test]
    fn test_types_compatible_qualifiers_ignored() {
        let mut types = TypeTable::new(&Target::host());
        let int_type = types.intern(Type::basic(TypeKind::Int));
        let const_int = types.intern(Type::with_modifiers(TypeKind::Int, TypeModifiers::CONST));
        let volatile_int =
            types.intern(Type::with_modifiers(TypeKind::Int, TypeModifiers::VOLATILE));
        let cv_int = types.intern(Type::with_modifiers(
            TypeKind::Int,
            TypeModifiers::CONST | TypeModifiers::VOLATILE,
        ));

        // All should be compatible with plain int
        assert!(types.types_compatible(int_type, const_int));
        assert!(types.types_compatible(int_type, volatile_int));
        assert!(types.types_compatible(int_type, cv_int));
        assert!(types.types_compatible(const_int, volatile_int));
    }

    #[test]
    fn test_types_compatible_signedness_matters() {
        let mut types = TypeTable::new(&Target::host());
        let int_type = types.intern(Type::basic(TypeKind::Int));
        let uint_type = types.intern(Type::with_modifiers(TypeKind::Int, TypeModifiers::UNSIGNED));
        // Signedness is NOT a qualifier, so these are NOT compatible
        assert!(!types.types_compatible(int_type, uint_type));
    }

    /// Pointer compatibility is a question about the *referenced* types, so
    /// it is asked through the table, which recurses. The `Type`-level
    /// comparison deliberately stops before the base.
    #[test]
    fn test_types_compatible_pointers() {
        let mut types = TypeTable::new(&Target::host());
        let int_ptr = types.intern(Type::pointer(types.int_id));
        let int_ptr2 = types.intern(Type::pointer(types.int_id));
        assert!(types.types_compatible(int_ptr, int_ptr2));

        let char_ptr = types.intern(Type::pointer(types.char_id));
        assert!(!types.types_compatible(int_ptr, char_ptr));
    }

    /// A complete enumerated type of `size` bytes, as the parser records one
    /// whose integer type has that size and signedness.
    fn enum_of(
        types: &mut TypeTable,
        idents: &mut crate::strings::StringTable,
        tag: &str,
        size: usize,
        unsigned: bool,
    ) -> TypeId {
        let composite = CompositeType {
            tag: Some(idents.intern(tag)),
            members: Vec::new(),
            enum_constants: Vec::new(),
            size,
            align: size,
            member_align: size,
            is_complete: true,
            transparent: false,
            anon_id: None,
            tag_type: None,
        };
        let mut typ = Type::enum_type(composite);
        if unsigned {
            typ.modifiers |= TypeModifiers::UNSIGNED;
        }
        types.intern(typ)
    }

    /// C17 6.7.2.2p4: an enum is compatible with the one integer type it was
    /// given -- not with the other signedness, not with another type of the
    /// same width, and not with a different enum.
    #[test]
    fn test_types_compatible_enum_and_its_integer_type() {
        let mut types = TypeTable::new(&Target::host());
        let mut idents = crate::strings::StringTable::new();
        let e_uint = enum_of(&mut types, &mut idents, "U", 4, true);
        let e_int = enum_of(&mut types, &mut idents, "S", 4, false);
        let e_ulong = enum_of(&mut types, &mut idents, "UL", 8, true);
        let e_long = enum_of(&mut types, &mut idents, "L", 8, false);

        assert!(types.types_compatible(e_uint, types.uint_id));
        assert!(types.types_compatible(types.uint_id, e_uint));
        assert!(!types.types_compatible(e_uint, types.int_id));
        assert!(types.types_compatible(e_int, types.int_id));
        assert!(!types.types_compatible(e_int, types.uint_id));
        assert!(types.types_compatible(e_ulong, types.ulong_id));
        assert!(!types.types_compatible(e_ulong, types.ulonglong_id));
        assert!(types.types_compatible(e_long, types.long_id));
        assert!(!types.types_compatible(e_long, types.longlong_id));

        // Two enums with the same integer type are still two types.
        let e_uint2 = enum_of(&mut types, &mut idents, "U2", 4, true);
        assert!(!types.types_compatible(e_uint, e_uint2));

        // A forward reference has no integer type yet.
        let fwd = types.intern(Type::incomplete_enum(idents.intern("F")));
        assert_eq!(types.enum_compatible_type(fwd), None);
        assert!(!types.types_compatible(fwd, types.int_id));
        assert_eq!(types.enum_compatible_type(types.int_id), None);
    }

    /// Below a pointer the qualifiers count, the enum rule still applies, and
    /// the signedness still has to agree.
    #[test]
    fn test_types_compatible_pointer_to_enum() {
        let mut types = TypeTable::new(&Target::host());
        let mut idents = crate::strings::StringTable::new();
        let e_uint = enum_of(&mut types, &mut idents, "U", 4, true);
        let e_int = enum_of(&mut types, &mut idents, "S", 4, false);
        let p_e_uint = types.intern(Type::pointer(e_uint));
        let p_e_int = types.intern(Type::pointer(e_int));
        let p_uint = types.intern(Type::pointer(types.uint_id));
        let p_int = types.intern(Type::pointer(types.int_id));
        let const_uint = types.qualified_with(types.uint_id, TypeModifiers::CONST);
        let p_const_uint = types.intern(Type::pointer(const_uint));

        assert!(types.types_compatible(p_e_uint, p_uint));
        assert!(!types.types_compatible(p_e_uint, p_int));
        assert!(types.types_compatible(p_e_int, p_int));
        assert!(!types.types_compatible(p_e_int, p_uint));
        assert!(!types.types_compatible(p_e_uint, p_const_uint));
        assert!(!types.types_compatible(p_e_uint, p_e_int));

        // `_Generic` compares top-level qualifiers too.
        let const_e_uint = types.qualified_with(e_uint, TypeModifiers::CONST);
        assert!(types.types_compatible_qualified(const_e_uint, const_uint));
        assert!(!types.types_compatible_qualified(const_e_uint, types.uint_id));
    }

    /// A typedef redefinition needs the same type: neither an enum's integer
    /// type nor a differently qualified one will do, at any level.
    #[test]
    fn test_types_same_keeps_enums_and_qualifiers_distinct() {
        let mut types = TypeTable::new(&Target::host());
        let mut idents = crate::strings::StringTable::new();
        let e_uint = enum_of(&mut types, &mut idents, "U", 4, true);
        let p_e_uint = types.intern(Type::pointer(e_uint));
        let p_uint = types.intern(Type::pointer(types.uint_id));
        let const_int = types.qualified_with(types.int_id, TypeModifiers::CONST);

        assert!(!types.types_same(e_uint, types.uint_id));
        assert!(!types.types_same(p_e_uint, p_uint));
        assert!(!types.types_same(const_int, types.int_id));
        assert!(types.types_same(e_uint, e_uint));
        let p_uint2 = types.intern(Type::pointer(types.uint_id));
        assert!(types.types_same(p_uint, p_uint2));
    }

    /// An enum computes as its integer type: the promotions and the usual
    /// arithmetic conversions replace it, at the integer type's rank.
    #[test]
    fn test_enum_promotes_to_its_integer_type() {
        let mut types = TypeTable::new(&Target::host());
        let mut idents = crate::strings::StringTable::new();
        let e_uint = enum_of(&mut types, &mut idents, "U", 4, true);
        let e_long = enum_of(&mut types, &mut idents, "L", 8, false);
        let e_ulong = enum_of(&mut types, &mut idents, "UL", 8, true);

        assert_eq!(types.integer_promote(e_uint), types.uint_id);
        assert_eq!(types.integer_promote(e_long), types.long_id);
        assert_eq!(types.common_type(e_uint, types.int_id), types.uint_id);
        assert_eq!(types.common_type(e_long, types.uint_id), types.long_id);
        assert_eq!(types.common_type(e_ulong, e_long), types.ulong_id);
        assert_eq!(
            types.common_type(e_ulong, types.longlong_id),
            types.ulonglong_id
        );
    }

    /// 6.5.16.1p1 keeps the pointee's qualifiers when one side is `void *`
    /// just as when the pointees are compatible: dropping `const` or
    /// `volatile` on the way to or from `void *` is a qualifier discard.
    #[test]
    fn test_void_pointer_assignment_keeps_qualifiers() {
        let mut types = TypeTable::new(&Target::host());
        let const_int = types.qualified_with(types.int_id, TypeModifiers::CONST);
        let volatile_int = types.qualified_with(types.int_id, TypeModifiers::VOLATILE);
        let const_int_ptr = types.intern(Type::pointer(const_int));
        let volatile_int_ptr = types.intern(Type::pointer(volatile_int));
        let int_ptr = types.intern(Type::pointer(types.int_id));
        let (void_ptr, const_void_ptr) = (types.void_ptr_id, types.const_void_ptr_id);
        let discard = Some(AssignFault::QualifierDiscard);
        // (target, value, fault)
        for (target, value, fault) in [
            (void_ptr, const_int_ptr, discard),
            (const_void_ptr, volatile_int_ptr, discard),
            (int_ptr, const_void_ptr, discard),
            (const_void_ptr, const_int_ptr, None),
            (const_int_ptr, const_void_ptr, None),
            (void_ptr, int_ptr, None),
            (int_ptr, void_ptr, None),
        ] {
            assert_eq!(types.assignment_fault(target, value, false), fault);
        }
    }

    #[test]
    fn test_types_compatible_arrays() {
        let mut types = TypeTable::new(&Target::host());
        let arr10 = types.intern(Type::array(types.int_id, 10));
        let arr10_2 = types.intern(Type::array(types.int_id, 10));
        assert!(types.types_compatible(arr10, arr10_2));

        let arr20 = types.intern(Type::array(types.int_id, 20));
        assert!(!types.types_compatible(arr10, arr20));
    }

    #[test]
    fn test_type_deduplication() {
        let mut types = TypeTable::new(&Target::host());
        // Interning the same type should return the same ID
        let int_ptr1 = types.intern(Type::pointer(types.int_id));
        let int_ptr2 = types.intern(Type::pointer(types.int_id));
        assert_eq!(int_ptr1, int_ptr2);

        // Different types should have different IDs
        let char_ptr = types.intern(Type::pointer(types.char_id));
        assert_ne!(int_ptr1, char_ptr);
    }

    #[test]
    fn test_type_table_pre_interned() {
        let types = TypeTable::new(&Target::host());
        // Pre-interned types should be valid
        assert!(types.int_id.is_valid());
        assert!(types.char_id.is_valid());
        assert!(types.void_id.is_valid());
        assert!(types.void_ptr_id.is_valid());

        // And have correct kinds
        assert_eq!(types.kind(types.int_id), TypeKind::Int);
        assert_eq!(types.kind(types.void_ptr_id), TypeKind::Pointer);
    }

    #[test]
    fn test_array_with_modifiers_deduplication() {
        let mut types = TypeTable::new(&Target::host());

        // Create a plain array type: int arr[10]
        let plain_arr = types.intern(Type::array(types.int_id, 10));

        // Create an array type with TYPEDEF modifier: typedef int arr[10]
        let mut typedef_arr_type = Type::array(types.int_id, 10);
        typedef_arr_type.modifiers = TypeModifiers::TYPEDEF;
        let typedef_arr = types.intern(typedef_arr_type);

        // These should be DIFFERENT TypeIds because modifiers are now part of TypeKey
        assert_ne!(
            plain_arr, typedef_arr,
            "Arrays with different modifiers should have different TypeIds"
        );

        // Plain arrays should still deduplicate with each other
        let plain_arr2 = types.intern(Type::array(types.int_id, 10));
        assert_eq!(
            plain_arr, plain_arr2,
            "Same plain arrays should have same TypeId"
        );

        // Typedef arrays should still deduplicate with each other
        let mut typedef_arr_type2 = Type::array(types.int_id, 10);
        typedef_arr_type2.modifiers = TypeModifiers::TYPEDEF;
        let typedef_arr2 = types.intern(typedef_arr_type2);
        assert_eq!(
            typedef_arr, typedef_arr2,
            "Same typedef arrays should have same TypeId"
        );
    }

    /// `TypeKind` already carries the size, so the `SHORT`/`LONG`/`LONGLONG`
    /// modifier bits that a specifier list sets alongside it distinguish
    /// nothing. Comparing them made the canonical interned `long` -- built as
    /// `Type::basic(TypeKind::Long)`, with no modifiers -- incompatible with
    /// the `long` a declaration produces, which carries the bit.
    #[test]
    fn test_size_modifier_bits_do_not_distinguish_types() {
        let mut types = TypeTable::new(&Target::host());

        for (kind, bit) in [
            (TypeKind::Short, TypeModifiers::SHORT),
            (TypeKind::Long, TypeModifiers::LONG),
            (
                TypeKind::LongLong,
                TypeModifiers::LONG | TypeModifiers::LONGLONG,
            ),
            (TypeKind::LongDouble, TypeModifiers::LONG),
        ] {
            let bare = types.intern(Type::basic(kind));
            let mut spelled_type = Type::basic(kind);
            spelled_type.modifiers = bit;
            let spelled = types.intern(spelled_type);
            assert_ne!(bare, spelled, "the two spellings are distinct ids");
            assert!(
                types.types_compatible(bare, spelled),
                "{kind:?} with and without its size bit must be one type"
            );
        }
    }

    /// The same relaxation must not merge types that really are distinct.
    /// Every size lives in its own `TypeKind`, and signedness is carried by a
    /// bit that stays significant.
    #[test]
    fn test_distinct_arithmetic_types_stay_incompatible() {
        let types = TypeTable::new(&Target::host());

        let distinct = [
            types.int_id,
            types.long_id,
            types.uint_id,
            types.ulong_id,
            types.char_id,
            types.double_id,
        ];
        for (i, &a) in distinct.iter().enumerate() {
            for (j, &b) in distinct.iter().enumerate() {
                if i == j {
                    continue;
                }
                assert!(
                    !types.types_compatible(a, b),
                    "{} and {} must stay distinct",
                    types.get(a),
                    types.get(b)
                );
            }
        }
    }

    /// The largest object is `PTRDIFF_MAX`, read off the target's `long`.
    #[test]
    fn test_max_object_bytes_is_ptrdiff_max() {
        let types = TypeTable::new(&Target::host());
        assert_eq!(types.max_object_bytes(), i64::MAX as usize);
    }

    /// Struct layout is exact however far past `u64::MAX` *bits* it runs.
    ///
    /// It accumulated its bits in a `usize`, so a struct past 2^61 bytes had
    /// no layout, and c17 refused one below `PTRDIFF_MAX` that gcc accepts
    /// (gcc.c-torture `991014-1`). The member offsets and sizes here are
    /// gcc's.
    #[test]
    fn test_struct_layout_past_u64_bits() {
        let mut types = TypeTable::new(&Target::host());
        let member = |typ, bit_width| StructMember {
            name: StringId::EMPTY,
            typ,
            offset: 0,
            bit_offset: None,
            bit_width,
            access_bytes: None,
            align: MemberAlign::NATURAL,
        };
        let shorts = types.intern(Type::array(types.short_id, (1 << 62) - 256));
        let chars_max = types.intern(Type::array(types.char_id, i64::MAX as usize));
        let chars_5g = types.intern(Type::array(types.char_id, 5_000_000_000));

        // 991014-1's `struct huge_struct`: 2^63 - 512 bytes, then four ints.
        let int = types.int_id;
        let mut m = vec![
            member(shorts, None),
            member(int, None),
            member(int, None),
            member(int, None),
            member(int, None),
        ];
        let (size, align) = types.compute_struct_layout(&mut m, None);
        assert_eq!((size, align), ((1usize << 63) - 496, 4));
        assert_eq!(m[1].offset, (1 << 63) - 512);
        assert_eq!(m[4].offset, (1 << 63) - 500);

        // Two members of `PTRDIFF_MAX` bytes each: past `u64::MAX` bits,
        // exact in bytes, and so measurably past the bound.
        let mut m = vec![member(chars_max, None), member(chars_max, None)];
        let (size, _) = types.compute_struct_layout(&mut m, None);
        assert_eq!(m[1].offset, i64::MAX as usize);
        assert_eq!(size, u64::MAX as usize - 1);

        // Three are past `usize::MAX` bytes: saturated, never wrapped small.
        let mut m = vec![
            member(chars_max, None),
            member(chars_max, None),
            member(chars_max, None),
            member(int, None),
        ];
        let (size, _) = types.compute_struct_layout(&mut m, None);
        assert_eq!(size, usize::MAX);

        // A bit-field past 2^32 bytes keeps its byte offset and bit position,
        // packed or not.
        for pack in [None, Some(1)] {
            let mut m = vec![
                member(chars_5g, None),
                member(types.uint_id, Some(3)),
                member(types.uint_id, Some(5)),
            ];
            let (size, _) = types.compute_struct_layout(&mut m, pack);
            assert_eq!(m[1].offset, 5_000_000_000);
            assert_eq!(m[2].offset, 5_000_000_000);
            assert_eq!((m[1].bit_offset, m[2].bit_offset), (Some(0), Some(3)));
            assert_eq!(
                size,
                if pack.is_some() {
                    5_000_000_001
                } else {
                    5_000_000_004
                }
            );
        }
    }

    /// A union's rounding saturates rather than wrapping, like a struct's.
    #[test]
    fn test_union_layout_near_the_bound() {
        let mut types = TypeTable::new(&Target::host());
        let member = |typ| StructMember {
            name: StringId::EMPTY,
            typ,
            offset: 0,
            bit_offset: None,
            bit_width: None,
            access_bytes: None,
            align: MemberAlign::NATURAL,
        };
        let chars = types.intern(Type::array(types.char_id, (1 << 62) - 256));
        let mut m = vec![member(types.int_id), member(chars)];
        assert_eq!(
            types.compute_union_layout(&mut m, None),
            ((1 << 62) - 256, 4)
        );

        // One byte under `usize::MAX`, rounded to 4: saturated.
        let chars = types.intern(Type::array(types.char_id, usize::MAX - 1));
        let mut m = vec![member(types.int_id), member(chars)];
        assert_eq!(types.compute_union_layout(&mut m, None).0, usize::MAX);
    }

    /// C17 6.2.7p3: a prototype is compatible with no prototype exactly when
    /// it has no ellipsis and no parameter the default argument promotions
    /// change.
    #[test]
    fn prototype_against_no_prototype() {
        let mut t = TypeTable::new(&Target::host());
        let none = t.intern(Type::function_no_prototype(t.int_id, false));
        let proto = |t: &mut TypeTable, params: Vec<TypeId>, variadic: bool| {
            let int = t.int_id;
            t.intern(Type::function(int, params, variadic, false))
        };
        let (int, double, char, float) = (t.int_id, t.double_id, t.char_id, t.float_id);
        let void_args = proto(&mut t, vec![], false);
        let int_double = proto(&mut t, vec![int, double], false);
        let char_arg = proto(&mut t, vec![char], false);
        let float_arg = proto(&mut t, vec![float], false);
        let variadic = proto(&mut t, vec![int], true);
        for f in [void_args, int_double] {
            assert!(t.types_compatible(none, f));
            assert!(t.types_compatible(f, none));
        }
        for f in [char_arg, float_arg, variadic] {
            assert!(!t.types_compatible(none, f));
            assert!(!t.types_compatible(f, none));
        }
        let long_none = t.intern(Type::function_no_prototype(t.long_id, false));
        assert!(!t.types_compatible(long_none, void_args));
    }

    /// A call's function type is the callee's own, or the one a pointer
    /// callee points to; anything else names none.
    #[test]
    fn callee_function_type_looks_through_one_pointer() {
        let mut t = TypeTable::new(&Target::host());
        let f = t.intern(Type::function(t.int_id, vec![], false, false));
        let pf = t.intern(Type::pointer(f));
        let ppf = t.intern(Type::pointer(pf));
        assert_eq!(t.callee_function_type(f), Some(f));
        assert_eq!(t.callee_function_type(pf), Some(f));
        assert_eq!(t.callee_function_type(ppf), None);
        assert_eq!(t.callee_function_type(t.int_id), None);
        assert_eq!(t.callee_function_type(t.void_ptr_id), None);
    }

    /// The composite type takes the known array extent and the prototype,
    /// at any depth, and keeps the first type's qualifiers.
    #[test]
    fn composite_type_of_compatible_types() {
        let mut t = TypeTable::new(&Target::host());
        let unknown = t.intern(Type {
            array_size: None,
            ..Type::array(t.int_id, 0)
        });
        let three = t.intern(Type::array(t.int_id, 3));
        assert_eq!(t.composite_type(unknown, three), three);
        assert_eq!(t.composite_type(three, unknown), three);

        let p_unknown = t.intern(Type::pointer(unknown));
        let p_three = t.intern(Type::pointer(three));
        assert_eq!(t.composite_type(p_unknown, p_three), p_three);

        let none = t.intern(Type::function_no_prototype(t.int_id, false));
        let void_args = t.intern(Type::function(t.int_id, vec![], false, false));
        assert_eq!(t.composite_type(none, void_args), void_args);
        assert_eq!(t.composite_type(void_args, none), void_args);

        // A parameter of pointer-to-array type composes too.
        let takes_unknown = t.intern(Type::function(t.int_id, vec![p_unknown], false, false));
        let takes_three = t.intern(Type::function(t.int_id, vec![p_three], false, false));
        assert_eq!(t.composite_type(takes_unknown, takes_three), takes_three);

        let const_unknown = t.qualified_with(p_unknown, TypeModifiers::CONST);
        let composite = t.composite_type(const_unknown, p_three);
        assert_eq!(t.qualifiers(composite), TypeModifiers::CONST);
        assert_eq!(t.base_type(composite), Some(three));
    }

    /// An array's qualifiers are its element type's, in both directions.
    #[test]
    fn qualifiers_through_arrays() {
        let mut t = TypeTable::new(&Target::host());
        let const_int = t.qualified_with(t.int_id, TypeModifiers::CONST);
        let arr = t.intern(Type::array(const_int, 3));
        let grid = t.intern(Type::array(arr, 2));
        assert_eq!(t.qualifiers(grid), TypeModifiers::empty());
        assert_eq!(t.qualifiers_through_arrays(grid), TypeModifiers::CONST);

        let bare = t.unqualified_through_arrays(grid);
        assert_eq!(t.qualifiers_through_arrays(bare), TypeModifiers::empty());
        let plain = t.intern(Type::array(t.int_id, 3));
        assert_eq!(t.base_type(bare), Some(plain));
        assert_eq!(t.qualified_with(bare, TypeModifiers::CONST), grid);
    }
}
