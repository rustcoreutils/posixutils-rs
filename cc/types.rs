//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Type system for c17 C17 compiler
// Compositional type model with interning for efficient comparison
//

use crate::float::FpFormat;
use crate::strings::{StringId, StringTable as IdentTable};
use crate::target::{Arch, Os, Target};
use std::collections::HashMap;
use std::fmt;

// ============================================================================
// Type ID - Unique identifier for interned types
// ============================================================================

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

// ============================================================================
// Composite Type Components
// ============================================================================

/// A struct/union member
#[derive(Debug, Clone, PartialEq)]
pub struct StructMember {
    /// Member name (StringId::EMPTY for unnamed bitfields)
    pub name: StringId,
    /// Member type (interned TypeId)
    pub typ: TypeId,
    /// Byte offset within struct (0 for unions, offset of storage unit for bitfields)
    pub offset: usize,
    /// For bitfields: bit offset within storage unit (0 = LSB)
    pub bit_offset: Option<u32>,
    /// For bitfields: bit width
    pub bit_width: Option<u32>,
    /// For bitfields: size of storage unit in bytes
    pub storage_unit_size: Option<u32>,
    /// Explicit alignment from _Alignas specifier (C11 6.7.5)
    /// None means use natural alignment for the type
    pub explicit_align: Option<u32>,
}

/// Information about a struct/union member lookup
#[derive(Debug, Clone, Copy)]
pub struct MemberInfo {
    /// Byte offset within struct
    pub offset: usize,
    /// Member type (interned TypeId)
    pub typ: TypeId,
    /// For bitfields: bit offset within storage unit
    pub bit_offset: Option<u32>,
    /// For bitfields: bit width
    pub bit_width: Option<u32>,
    /// For bitfields: storage unit size in bytes
    pub storage_unit_size: Option<u32>,
}

/// An enum constant
#[derive(Debug, Clone, PartialEq)]
pub struct EnumConstant {
    /// Constant name (interned StringId)
    pub name: StringId,
    /// Constant value
    pub value: i64,
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
    /// False for forward declarations
    pub is_complete: bool,
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
            is_complete: false,
        }
    }

    // NOTE: compute_struct_layout and compute_union_layout have been moved to TypeTable
    // since they require access to member type sizes via TypeId lookup.
}

// ============================================================================
// Type Modifiers
// ============================================================================

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
    }
}

// ============================================================================
// Type Kinds
// ============================================================================

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

// ============================================================================
// Type Representation
// ============================================================================

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

    /// Composite type data (for struct, union, enum)
    pub composite: Option<Box<CompositeType>>,

    /// Explicit alignment from __attribute__((aligned(N))) on typedef.
    /// When set, overrides the natural alignment returned by alignment().
    pub explicit_align: Option<u32>,
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
            composite: None,
            explicit_align: None,
        }
    }
}

impl Type {
    /// Create a new basic type
    pub fn basic(kind: TypeKind) -> Self {
        Self {
            kind,
            ..Default::default()
        }
    }

    /// Create a type with modifiers
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

    /// Create a struct type
    pub fn struct_type(composite: CompositeType) -> Self {
        Self {
            kind: TypeKind::Struct,
            composite: Some(Box::new(composite)),
            ..Default::default()
        }
    }

    /// Create a union type
    pub fn union_type(composite: CompositeType) -> Self {
        Self {
            kind: TypeKind::Union,
            composite: Some(Box::new(composite)),
            ..Default::default()
        }
    }

    /// Create an enum type
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
    pub const DECL_SPECIFIERS: TypeModifiers = TypeModifiers::STATIC
        .union(TypeModifiers::EXTERN)
        .union(TypeModifiers::REGISTER)
        .union(TypeModifiers::AUTO)
        .union(TypeModifiers::TYPEDEF)
        .union(TypeModifiers::THREAD_LOCAL)
        .union(TypeModifiers::INLINE)
        .union(TypeModifiers::NORETURN);

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
        // Top-level qualifiers to ignore
        const QUALIFIERS: TypeModifiers = TypeModifiers::CONST
            .union(TypeModifiers::VOLATILE)
            .union(TypeModifiers::RESTRICT)
            .union(TypeModifiers::ATOMIC);

        // Compare kinds first
        if self.kind != other.kind {
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
        let ignored = QUALIFIERS
            .union(redundant_signed)
            .union(Self::DECL_SPECIFIERS);

        // Compare modifiers (ignoring top-level qualifiers)
        let self_mods = self.modifiers.difference(ignored);
        let other_mods = other.modifiers.difference(ignored);
        if self_mods != other_mods {
            return false;
        }

        // Compare array sizes
        if self.array_size != other.array_size {
            return false;
        }

        // Compare variadic flag
        if self.variadic != other.variadic {
            return false;
        }

        // The base is compared by the caller, which can recurse. Comparing it
        // here by TypeId called two structurally identical types different
        // whenever a declaration specifier had settled on an inner type:
        // `static char *x[]` records STATIC on the element, so assigning it to
        // a plain `char **` field reported "'char**' from 'char**'" -- a
        // complaint about two spellings of one type. Storage class is a
        // property of a declaration, never of a type.

        // Only the *shape* of the parameter list is settled here: whether a
        // prototype was supplied at all, and how many parameters it has. The
        // parameter types themselves are compared by the caller, which can
        // recurse -- comparing them by TypeId called two identical
        // `void *(Parser *)` different whenever a declaration specifier had
        // settled on an inner type, which is 19 lines of CPython's generated
        // parser.
        match (&self.params, &other.params) {
            (Some(a), Some(b)) if a.len() == b.len() => {}
            (None, None) => {}
            _ => return false,
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
                    (Some(x), Some(y)) => x == y,
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

// ============================================================================
// Type Table - Interned type storage and query methods
// ============================================================================

/// Key for type lookup/deduplication (hashable representation)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum TypeKey {
    /// Basic type: kind + modifiers
    Basic(TypeKind, u32),
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
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum AssignFault {
    /// No conversion exists: a pointer against a floating type, an aggregate
    /// against anything but a compatible aggregate, or a `void` value.
    Incompatible,
    /// An integer where a pointer belongs, or the reverse, with no cast.
    IntegerPointerMix,
    /// Two pointers whose referenced types are not compatible.
    PointerMismatch,
    /// The target's referenced type drops a qualifier the source's had.
    QualifierDiscard,
}

impl AssignFault {
    /// Whether this fault is fatal. Only a missing conversion is.
    pub fn is_error(self) -> bool {
        self == AssignFault::Incompatible
    }

    /// The clause naming what went wrong, as gcc words it.
    pub fn describe(self) -> &'static str {
        match self {
            AssignFault::Incompatible => "incompatible types",
            AssignFault::IntegerPointerMix => "makes integer from pointer without a cast",
            AssignFault::PointerMismatch => "incompatible pointer type",
            AssignFault::QualifierDiscard => "discards a qualifier from the pointer target type",
        }
    }
}

const DEFAULT_TYPE_TABLE_CAPACITY: usize = 65536;

/// Type table - stores all types and provides ID-based lookup
/// Pattern follows IdentTable in token/lexer.rs
pub struct TypeTable {
    /// All interned types (indexed by TypeId)
    types: Vec<Type>,
    /// Lookup map for deduplication
    lookup: HashMap<TypeKey, TypeId>,
    /// Pointer size in bits (target-dependent, defaults to 64 for LP64)
    pointer_width: u32,
    /// Target architecture for runtime type size calculations
    target_arch: Arch,
    /// Target OS for runtime type size calculations
    target_os: Os,

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
    pub complex_float_id: TypeId,
    pub complex_double_id: TypeId,
    pub complex_longdouble_id: TypeId,
    pub complex_float16_id: TypeId,
    pub complex_float128_id: TypeId,
    pub void_ptr_id: TypeId,
    pub char_ptr_id: TypeId,
}

impl TypeTable {
    /// Create a new type table with common types pre-interned
    pub fn new(target: &Target) -> Self {
        let mut table = Self {
            types: Vec::with_capacity(DEFAULT_TYPE_TABLE_CAPACITY),
            lookup: HashMap::with_capacity(DEFAULT_TYPE_TABLE_CAPACITY),
            pointer_width: target.pointer_width,
            target_arch: target.arch,
            target_os: target.os,
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
            complex_float_id: TypeId::INVALID,
            complex_double_id: TypeId::INVALID,
            complex_longdouble_id: TypeId::INVALID,
            complex_float16_id: TypeId::INVALID,
            complex_float128_id: TypeId::INVALID,
            void_ptr_id: TypeId::INVALID,
            char_ptr_id: TypeId::INVALID,
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

        // Pre-intern common pointer types
        table.void_ptr_id = table.intern(Type::pointer(table.void_id));
        table.char_ptr_id = table.intern(Type::pointer(table.char_id));

        table
    }

    /// Intern a type, returning its unique ID
    /// Deduplicates equivalent types (same ID for equivalent types)
    pub fn intern(&mut self, typ: Type) -> TypeId {
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
            self.types.push(typ);
            id
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
                    modifiers: typ.modifiers.bits(),
                })
            }
            _ => Some(TypeKey::Basic(typ.kind, typ.modifiers.bits())),
        }
    }

    /// Get a type by ID (returns reference)
    #[inline]
    pub fn get(&self, id: TypeId) -> &Type {
        &self.types[id.0 as usize]
    }

    /// Complete an incomplete struct/union type with its full definition.
    /// This updates the type in place so that all existing pointers to
    /// the incomplete type will now see the complete type.
    pub fn complete_struct(&mut self, id: TypeId, composite: CompositeType) {
        let typ = &mut self.types[id.0 as usize];
        debug_assert!(
            matches!(typ.kind, TypeKind::Struct | TypeKind::Union),
            "complete_struct called on non-struct/union type"
        );
        typ.composite = Some(Box::new(composite));
    }

    // =========================================================================
    // Type query methods (moved from Type to TypeTable)
    // =========================================================================

    /// Get the type kind
    #[inline]
    pub fn kind(&self, id: TypeId) -> TypeKind {
        self.get(id).kind
    }

    /// Get type modifiers
    #[inline]
    pub fn modifiers(&self, id: TypeId) -> TypeModifiers {
        self.get(id).modifiers
    }

    /// Get the base type ID (for pointers, arrays, functions)
    #[inline]
    pub fn base_type(&self, id: TypeId) -> Option<TypeId> {
        self.get(id).base
    }

    /// Look up an existing pointer type to the given base type
    /// Returns void_ptr_id if not found (since all pointers are same size)
    #[inline]
    pub fn pointer_to(&self, base: TypeId) -> TypeId {
        // Look for an existing pointer to this base type
        let key = TypeKey::Pointer(base, 0); // No modifiers
        if let Some(&id) = self.lookup.get(&key) {
            return id;
        }
        // All pointers are same size, so void* works as fallback
        self.void_ptr_id
    }

    // =========================================================================
    // Test-only methods (used by tests but not production code)
    // =========================================================================

    /// Get array size
    #[cfg(test)]
    #[inline]
    pub fn array_size(&self, id: TypeId) -> Option<usize> {
        self.get(id).array_size
    }

    /// Get function parameters
    #[cfg(test)]
    #[inline]
    pub fn params(&self, id: TypeId) -> Option<&Vec<TypeId>> {
        self.get(id).params.as_ref()
    }

    /// Check if function is variadic
    #[cfg(test)]
    #[inline]
    pub fn is_variadic(&self, id: TypeId) -> bool {
        self.get(id).variadic
    }

    /// Get composite type data (for struct/union/enum)
    pub fn composite(&self, id: TypeId) -> Option<&CompositeType> {
        self.get(id).composite.as_deref()
    }

    /// Format a type for display (with recursive base type printing).
    ///
    /// Used by diagnostics that have to name the types they are complaining
    /// about -- an assignment constraint violation is unreadable without them.
    pub fn format_type(&self, id: TypeId, idents: Option<&IdentTable>) -> String {
        let typ = self.get(id);
        let mut result = String::new();

        // Print modifiers
        if typ.modifiers.contains(TypeModifiers::CONST) {
            result.push_str("const ");
        }
        if typ.modifiers.contains(TypeModifiers::VOLATILE) {
            result.push_str("volatile ");
        }
        if typ.modifiers.contains(TypeModifiers::UNSIGNED) {
            result.push_str("unsigned ");
        } else if typ.modifiers.contains(TypeModifiers::SIGNED) && typ.kind == TypeKind::Char {
            result.push_str("signed ");
        }

        match typ.kind {
            TypeKind::Pointer => {
                if let Some(base) = typ.base {
                    result.push_str(&self.format_type(base, idents));
                    result.push('*');
                } else {
                    result.push('*');
                }
            }
            TypeKind::Array => {
                if let Some(base) = typ.base {
                    result.push_str(&self.format_type(base, idents));
                    if let Some(size) = typ.array_size {
                        result.push_str(&format!("[{}]", size));
                    } else {
                        result.push_str("[]");
                    }
                } else {
                    result.push_str("[]");
                }
            }
            TypeKind::Function => {
                if let Some(ret) = typ.base {
                    result.push_str(&self.format_type(ret, idents));
                    result.push('(');
                    if let Some(params) = &typ.params {
                        for (i, &param) in params.iter().enumerate() {
                            if i > 0 {
                                result.push_str(", ");
                            }
                            result.push_str(&self.format_type(param, idents));
                        }
                        if typ.variadic {
                            if !params.is_empty() {
                                result.push_str(", ");
                            }
                            result.push_str("...");
                        }
                    }
                    result.push(')');
                } else {
                    result.push_str("()");
                }
            }
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
            _ => {
                result.push_str(&typ.kind.to_string());
            }
        }

        result
    }

    // =========================================================================
    // Production methods (used by compiler proper)
    // =========================================================================

    /// Check if type is an integer type
    #[inline]
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
    #[inline]
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

    /// Check if type is a complex floating point type
    #[inline]
    pub fn is_complex(&self, id: TypeId) -> bool {
        self.get(id).modifiers.contains(TypeModifiers::COMPLEX)
    }

    /// Get the base float type for a complex type (e.g., double for double _Complex)
    /// Returns the same type if not complex
    #[inline]
    pub fn complex_base(&self, id: TypeId) -> TypeId {
        if !self.is_complex(id) {
            return id;
        }
        match self.get(id).kind {
            TypeKind::Float => self.float_id,
            TypeKind::Double => self.double_id,
            TypeKind::LongDouble => self.longdouble_id,
            TypeKind::Float16 => self.float16_id,
            TypeKind::Float128 => self.float128_id,
            _ => id,
        }
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

    /// Get the complex type for a float base type (e.g., double → double _Complex)
    #[inline]
    pub fn make_complex(&self, id: TypeId) -> TypeId {
        if self.is_complex(id) {
            return id;
        }
        match self.get(id).kind {
            TypeKind::Float => self.complex_float_id,
            TypeKind::Double => self.complex_double_id,
            TypeKind::LongDouble => self.complex_longdouble_id,
            TypeKind::Float16 => self.complex_float16_id,
            TypeKind::Float128 => self.complex_float128_id,
            _ => self.complex_double_id, // default to double _Complex
        }
    }

    /// Check if type is an arithmetic type (integer, float, or complex)
    #[inline]
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
        match self.kind(id) {
            TypeKind::Array => (TypeKind::Pointer, self.base_type(id)),
            TypeKind::Function => (TypeKind::Pointer, Some(id)),
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
        let t_agg = matches!(t_kind, TypeKind::Struct | TypeKind::Union);
        let v_agg = matches!(v_kind, TypeKind::Struct | TypeKind::Union);

        // An aggregate assigns only from a compatible aggregate. Compared
        // through `types_compatible` rather than by TypeId, because struct and
        // union types are deliberately not deduplicated -- they have identity.
        if t_agg || v_agg {
            let ok = t_agg && v_agg && self.types_compatible(target, value);
            return (!ok).then_some(AssignFault::Incompatible);
        }

        if t_ptr && v_ptr {
            let (Some(t_pointee), Some(v_pointee)) = (t_pointee, v_pointee) else {
                return None;
            };
            // `void *` converts to and from any object pointer (6.5.16.1p1).
            if self.kind(t_pointee) == TypeKind::Void || self.kind(v_pointee) == TypeKind::Void {
                return None;
            }
            if !self.types_compatible(t_pointee, v_pointee) {
                return Some(AssignFault::PointerMismatch);
            }
            // Compatible targets, but the assignment must not silently gain
            // write access: the target's qualifiers have to include the
            // source's.
            const QUALS: TypeModifiers = TypeModifiers::CONST
                .union(TypeModifiers::VOLATILE)
                .union(TypeModifiers::RESTRICT)
                .union(TypeModifiers::ATOMIC);
            let t_quals = self.modifiers(t_pointee) & QUALS;
            let v_quals = self.modifiers(v_pointee) & QUALS;
            return (!t_quals.contains(v_quals)).then_some(AssignFault::QualifierDiscard);
        }

        if t_ptr {
            // A null pointer constant is spelled as an integer, and is allowed.
            if value_is_null_constant {
                return None;
            }
            return Some(if self.is_integer(value) {
                AssignFault::IntegerPointerMix
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
                AssignFault::IntegerPointerMix
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
        match self.kind(typ) {
            TypeKind::Array => {
                let elem = self.base_type(typ).unwrap_or(self.char_id);
                self.intern(Type::pointer(elem))
            }
            TypeKind::Function => self.intern(Type::pointer(typ)),
            _ => typ,
        }
    }

    /// Check if type is a scalar type (arithmetic or pointer)
    #[inline]
    pub fn is_scalar(&self, id: TypeId) -> bool {
        self.is_arithmetic(id) || self.get(id).kind == TypeKind::Pointer
    }

    /// Check if type is unsigned
    #[inline]
    pub fn is_unsigned(&self, id: TypeId) -> bool {
        self.get(id).modifiers.contains(TypeModifiers::UNSIGNED)
    }

    /// Check if type is a plain char (no explicit signed/unsigned)
    #[inline]
    pub fn is_plain_char(&self, id: TypeId) -> bool {
        let typ = self.get(id);
        typ.kind == TypeKind::Char
            && !typ.modifiers.contains(TypeModifiers::SIGNED)
            && !typ.modifiers.contains(TypeModifiers::UNSIGNED)
    }

    /// Get the unsigned version of a type
    #[inline]
    pub fn unsigned_version(&self, id: TypeId) -> TypeId {
        match self.get(id).kind {
            TypeKind::Char => self.uchar_id,
            TypeKind::Short => self.ushort_id,
            TypeKind::Int => self.uint_id,
            TypeKind::Long => self.ulong_id,
            TypeKind::LongLong => self.ulonglong_id,
            TypeKind::Int128 => self.uint128_id,
            _ => id, // For non-integer types, just return the original
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
            TypeKind::Char => 8,
            TypeKind::Short => 16,
            TypeKind::Int => 32,
            TypeKind::Long => 64,
            TypeKind::LongLong => 64,
            TypeKind::Int128 => 128,
            TypeKind::Float => 32 * multiplier,
            TypeKind::Double => 64 * multiplier,
            TypeKind::LongDouble => self.longdouble_size_bits() * multiplier,
            TypeKind::Float16 => 16 * multiplier,
            TypeKind::Float128 => 128 * multiplier,
            TypeKind::Pointer => self.pointer_width,
            TypeKind::Array => {
                let elem_size = typ.base.map(|b| self.size_bits(b)).unwrap_or(0) as u64;
                let count = typ.array_size.unwrap_or(0) as u64;
                let total = elem_size.saturating_mul(count);
                if total > u32::MAX as u64 {
                    // Array too large for u32 size_bits; cap at u32::MAX
                    // (size_bytes via size_bits/8 still works for reasonable sizes)
                    u32::MAX
                } else {
                    total as u32
                }
            }
            TypeKind::Struct | TypeKind::Union => {
                (typ.composite.as_ref().map(|c| c.size).unwrap_or(0) * 8) as u32
            }
            TypeKind::Function => 0,
            TypeKind::Enum => 32,
            TypeKind::VaList => self.va_list_size_bits(),
        }
    }

    /// Get the size of a type in bytes
    pub fn size_bytes(&self, id: TypeId) -> usize {
        let typ = self.get(id);
        match typ.kind {
            TypeKind::Struct | TypeKind::Union => {
                typ.composite.as_ref().map(|c| c.size).unwrap_or(0)
            }
            TypeKind::Enum => 4,
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
            TypeKind::Enum => 4,
            TypeKind::Array => typ.base.map(|b| self.alignment(b)).unwrap_or(1),
            TypeKind::Function => 1,
            TypeKind::VaList => self.va_list_alignment(),
        }
    }

    // ========================================================================
    // Target-dependent type size helpers
    // ========================================================================

    /// Whether `__float128` is available on this target.
    ///
    /// The type is soft-float everywhere, since neither supported architecture
    /// has binary128 arithmetic in hardware, so it exists only where the
    /// `__*tf*` runtime helpers do. Linux has them in libgcc, versioned
    /// `GCC_4.3.0`. Apple's arm64 runtime has none of them, and clang does not
    /// offer the type there either, so a program using it would compile and
    /// then fail to link on every operation it performed.
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

    /// Find a member in a struct/union type, including anonymous struct/union members
    /// C11 6.7.2.1p13: "An unnamed member of structure type with no tag is called an
    /// anonymous structure; an unnamed member of union type with no tag is called an
    /// anonymous union. The members of an anonymous structure or union are considered
    /// to be members of the containing structure or union."
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
        let typ = self.get(id);
        if let Some(ref composite) = typ.composite {
            for member in &composite.members {
                if member.name == name {
                    // Found the member directly
                    return Some(MemberInfo {
                        offset: base_offset + member.offset,
                        typ: member.typ,
                        bit_offset: member.bit_offset,
                        bit_width: member.bit_width,
                        storage_unit_size: member.storage_unit_size,
                    });
                }

                // Check if this is an anonymous struct/union (name is empty, no tag)
                // and search recursively in it
                if member.name == StringId::EMPTY {
                    let member_type = self.get(member.typ);
                    let is_anon_aggregate =
                        matches!(member_type.kind, TypeKind::Struct | TypeKind::Union)
                            && member_type
                                .composite
                                .as_ref()
                                .is_some_and(|c| c.tag.is_none());

                    if is_anon_aggregate {
                        if let Some(found) = self.find_member_recursive(
                            member.typ,
                            name,
                            base_offset + member.offset,
                        ) {
                            return Some(found);
                        }
                    }
                }
            }
        }
        None
    }

    /// Check if two types are compatible (for __builtin_types_compatible_p)
    pub fn types_compatible(&self, id1: TypeId, id2: TypeId) -> bool {
        // Quick check: same TypeId means same type
        if id1 == id2 {
            return true;
        }
        if !self.get(id1).compatible_ignoring_base(self.get(id2)) {
            return false;
        }
        // Then the referenced type and the parameter types, structurally
        // rather than by identity. Recursion terminates: `base` and parameter
        // chains shorten by one derivation at a time, and a struct's members
        // are compared as ids inside its composite rather than followed.
        let base_ok = match (self.get(id1).base, self.get(id2).base) {
            (Some(a), Some(b)) => self.types_compatible(a, b),
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
                .all(|(&x, &y)| self.types_compatible(x, y)),
            _ => true,
        }
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
        if id1 == id2 {
            return true;
        }

        const QUALIFIERS: TypeModifiers = TypeModifiers::CONST
            .union(TypeModifiers::VOLATILE)
            .union(TypeModifiers::RESTRICT)
            .union(TypeModifiers::ATOMIC);

        let (t1, t2) = (self.get(id1), self.get(id2));
        if t1.modifiers.intersection(QUALIFIERS) != t2.modifiers.intersection(QUALIFIERS) {
            return false;
        }

        self.types_compatible(id1, id2)
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
    pub fn compute_struct_layout(
        &self,
        members: &mut [StructMember],
        packed: bool,
    ) -> (usize, usize) {
        let mut bit_offset = 0usize;
        let mut max_align = 1usize;
        // The furthest byte any access window reaches. Ordinary members never
        // reach past the running offset, but a window is a power-of-two span
        // that can, and the struct has to be large enough to hold it.
        let mut window_end = 0usize;

        for member in members.iter_mut() {
            let Some(bit_width) = member.bit_width else {
                // Use explicit alignment from _Alignas if specified, otherwise natural alignment.
                // For packed structs, force alignment to 1 (no padding between members).
                let natural_align = if packed {
                    1
                } else {
                    self.alignment(member.typ)
                };
                let align = member
                    .explicit_align
                    .map(|a| a as usize)
                    .unwrap_or(natural_align);
                max_align = max_align.max(align);

                bit_offset = bit_offset.next_multiple_of(align * 8);
                member.offset = bit_offset / 8;
                member.bit_offset = None;
                member.storage_unit_size = None;

                bit_offset += self.size_bytes(member.typ) * 8;
                continue;
            };

            let unit_bytes = self.size_bytes(member.typ);
            let unit_bits = unit_bytes * 8;

            if bit_width == 0 {
                // C17 6.7.2.1p12: a zero-width bitfield forces the *next*
                // member to the next boundary of its declared type's storage
                // unit, and contributes nothing else -- not even alignment.
                // `struct { char c; int :0; char d; }` is 5 bytes with `d` at
                // offset 4, packed or not, which is also gcc's answer.
                bit_offset = bit_offset.next_multiple_of(unit_bits);
                member.offset = bit_offset / 8;
                member.bit_offset = None;
                member.storage_unit_size = None;
                continue;
            }

            max_align = max_align.max(self.alignment(member.typ));

            // Advance only when the field would otherwise straddle a unit
            // boundary. Bitfields ignore `packed`: packing them to the bit
            // would let one straddle, and the access window cannot span an
            // unaligned range (see Known Divergences in cc/doc/TODO.md).
            let bit_width = bit_width as usize;
            if bit_offset % unit_bits + bit_width > unit_bits {
                bit_offset = bit_offset.next_multiple_of(unit_bits);
            }

            // The field is read and written through the `sizeof(T)`-aligned
            // window it now provably sits inside. That window is wider than
            // the field needs and can span a neighbouring member, which costs
            // nothing here -- a store is a read-modify-write, so the
            // neighbour's bits are put back unchanged -- but it does mean the
            // struct has to be big enough to contain the window.
            let offset = bit_offset / unit_bits * unit_bytes;
            member.offset = offset;
            member.bit_offset = Some((bit_offset - offset * 8) as u32);
            member.storage_unit_size = Some(unit_bytes as u32);
            window_end = window_end.max(offset + unit_bytes);

            bit_offset += bit_width;
        }

        let final_align = if packed { 1 } else { max_align };
        let size = bit_offset
            .div_ceil(8)
            .max(window_end)
            .next_multiple_of(final_align);
        (size, final_align)
    }

    /// Get the number of interned types
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.types.len()
    }

    /// Compute union layout (all members at offset 0)
    /// Returns (total_size, alignment)
    pub fn compute_union_layout(&self, members: &mut [StructMember]) -> (usize, usize) {
        let mut max_size = 0usize;
        let mut max_align = 1usize;

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
            if member.bit_width.is_some_and(|w| w > 0) {
                member.bit_offset = Some(0);
                member.storage_unit_size = Some(self.size_bytes(member.typ) as u32);
            } else {
                member.bit_offset = None;
                member.storage_unit_size = None;
            }
            max_size = max_size.max(self.size_bytes(member.typ));
            // Use explicit alignment from _Alignas if specified, otherwise natural alignment
            let natural_align = self.alignment(member.typ);
            let align = member
                .explicit_align
                .map(|a| a as usize)
                .unwrap_or(natural_align);
            max_align = max_align.max(align);
        }

        let size = if max_align > 1 {
            (max_size + max_align - 1) & !(max_align - 1)
        } else {
            max_size
        };
        (size, max_align)
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_types() {
        let types = TypeTable::new(&Target::host());
        assert!(types.is_integer(types.int_id));
        assert!(types.is_arithmetic(types.int_id));
        assert!(types.is_scalar(types.int_id));
        assert!(!types.is_float(types.int_id));
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

    #[test]
    fn test_unsigned_modifier() {
        let types = TypeTable::new(&Target::host());
        assert!(types.is_unsigned(types.uint_id));
        assert!(!types.is_unsigned(types.int_id));
    }

    #[test]
    fn test_type_format() {
        let types = TypeTable::new(&Target::host());
        assert_eq!(types.format_type(types.int_id, None), "int");
        assert_eq!(types.format_type(types.uint_id, None), "unsigned int");
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
}
