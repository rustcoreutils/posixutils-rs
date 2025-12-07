//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Type system for pcc C99 compiler
// Based on sparse's compositional type model
//

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
    /// Member name (empty string for unnamed bitfields)
    pub name: String,
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
    /// Constant name
    pub name: String,
    /// Constant value
    pub value: i64,
}

/// Composite type definition (struct, union, or enum)
#[derive(Debug, Clone, PartialEq)]
pub struct CompositeType {
    /// Tag name (e.g., "point" in "struct point")
    pub tag: Option<String>,
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
    pub fn incomplete(tag: Option<String>) -> Self {
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
    }
}

// ============================================================================
// Type Kinds
// ============================================================================

/// Basic type kinds (mirrors sparse's SYM_* for types)
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
    Float,
    Double,
    LongDouble,

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
            TypeKind::Float => write!(f, "float"),
            TypeKind::Double => write!(f, "double"),
            TypeKind::LongDouble => write!(f, "long double"),
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

/// A C type (compositional structure like sparse)
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

    /// Composite type data (for struct, union, enum)
    pub composite: Option<Box<CompositeType>>,
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
            composite: None,
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
            modifiers: TypeModifiers::empty(),
            base: Some(base),
            array_size: None,
            params: None,
            variadic: false,
            composite: None,
        }
    }

    /// Create an array type (element type is a TypeId)
    pub fn array(base: TypeId, size: usize) -> Self {
        Self {
            kind: TypeKind::Array,
            modifiers: TypeModifiers::empty(),
            base: Some(base),
            array_size: Some(size),
            params: None,
            variadic: false,
            composite: None,
        }
    }

    /// Create a function type (return type and param types are TypeIds)
    pub fn function(return_type: TypeId, params: Vec<TypeId>, variadic: bool) -> Self {
        Self {
            kind: TypeKind::Function,
            modifiers: TypeModifiers::empty(),
            base: Some(return_type),
            array_size: None,
            params: Some(params),
            variadic,
            composite: None,
        }
    }

    /// Create a struct type
    pub fn struct_type(composite: CompositeType) -> Self {
        Self {
            kind: TypeKind::Struct,
            modifiers: TypeModifiers::empty(),
            base: None,
            array_size: None,
            params: None,
            variadic: false,
            composite: Some(Box::new(composite)),
        }
    }

    /// Create a union type
    pub fn union_type(composite: CompositeType) -> Self {
        Self {
            kind: TypeKind::Union,
            modifiers: TypeModifiers::empty(),
            base: None,
            array_size: None,
            params: None,
            variadic: false,
            composite: Some(Box::new(composite)),
        }
    }

    /// Create an enum type
    pub fn enum_type(composite: CompositeType) -> Self {
        Self {
            kind: TypeKind::Enum,
            modifiers: TypeModifiers::empty(),
            base: None,
            array_size: None,
            params: None,
            variadic: false,
            composite: Some(Box::new(composite)),
        }
    }

    /// Create an incomplete (forward-declared) struct type
    pub fn incomplete_struct(tag: String) -> Self {
        Self::struct_type(CompositeType::incomplete(Some(tag)))
    }

    /// Create an incomplete (forward-declared) union type
    pub fn incomplete_union(tag: String) -> Self {
        Self::union_type(CompositeType::incomplete(Some(tag)))
    }

    /// Create an incomplete (forward-declared) enum type
    pub fn incomplete_enum(tag: String) -> Self {
        Self::enum_type(CompositeType::incomplete(Some(tag)))
    }

    /// Find a member in a struct/union type
    /// Returns MemberInfo with full bitfield details if found
    pub fn find_member(&self, name: &str) -> Option<MemberInfo> {
        if let Some(ref composite) = self.composite {
            for member in &composite.members {
                if member.name == name {
                    return Some(MemberInfo {
                        offset: member.offset,
                        typ: member.typ, // TypeId is Copy, no clone needed
                        bit_offset: member.bit_offset,
                        bit_width: member.bit_width,
                        storage_unit_size: member.storage_unit_size,
                    });
                }
            }
        }
        None
    }

    /// Check if two types are compatible (for __builtin_types_compatible_p)
    /// This ignores top-level qualifiers (const, volatile, restrict)
    /// but otherwise requires types to be identical.
    /// Note: Different enum types are NOT compatible, even if they have
    /// the same underlying integer type.
    ///
    /// With TypeId interning, base types are compared by TypeId equality.
    /// For full recursive comparison, use TypeTable::types_compatible().
    pub fn types_compatible(&self, other: &Type) -> bool {
        // Top-level qualifiers to ignore
        const QUALIFIERS: TypeModifiers = TypeModifiers::CONST
            .union(TypeModifiers::VOLATILE)
            .union(TypeModifiers::RESTRICT);

        // Compare kinds first
        if self.kind != other.kind {
            return false;
        }

        // Compare modifiers (ignoring top-level qualifiers)
        let self_mods = self.modifiers.difference(QUALIFIERS);
        let other_mods = other.modifiers.difference(QUALIFIERS);
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

        // Compare base types by TypeId (interned types with same ID are equal)
        if self.base != other.base {
            return false;
        }

        // Compare function parameters by TypeId
        match (&self.params, &other.params) {
            (Some(a), Some(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                // TypeIds are directly comparable
                if a != b {
                    return false;
                }
            }
            (None, None) => {}
            _ => return false,
        }

        // Compare composite types (struct, union, enum)
        // For these, we require the exact same composite definition
        // (different enum types are NOT compatible even with same underlying type)
        match (&self.composite, &other.composite) {
            (Some(a), Some(b)) => a == b,
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
    Array(TypeId, Option<usize>),
    /// Function type
    Function {
        ret: TypeId,
        params: Vec<TypeId>,
        variadic: bool,
    },
}

/// Default capacity for type table allocations (reduces reallocation overhead)
const DEFAULT_TYPE_TABLE_CAPACITY: usize = 2048;

/// Type table - stores all types and provides ID-based lookup
/// Pattern follows IdentTable in token/lexer.rs
pub struct TypeTable {
    /// All interned types (indexed by TypeId)
    types: Vec<Type>,
    /// Lookup map for deduplication
    lookup: HashMap<TypeKey, TypeId>,

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
    pub float_id: TypeId,
    pub double_id: TypeId,
    pub longdouble_id: TypeId,
    pub void_ptr_id: TypeId,
    pub char_ptr_id: TypeId,
}

impl TypeTable {
    /// Create a new type table with common types pre-interned
    pub fn new() -> Self {
        let mut table = Self {
            types: Vec::with_capacity(DEFAULT_TYPE_TABLE_CAPACITY),
            lookup: HashMap::with_capacity(DEFAULT_TYPE_TABLE_CAPACITY),
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
            float_id: TypeId::INVALID,
            double_id: TypeId::INVALID,
            longdouble_id: TypeId::INVALID,
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
        table.float_id = table.intern(Type::basic(TypeKind::Float));
        table.double_id = table.intern(Type::basic(TypeKind::Double));
        table.longdouble_id = table.intern(Type::basic(TypeKind::LongDouble));

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

        match typ.kind {
            TypeKind::Pointer => {
                let base = typ.base?;
                Some(TypeKey::Pointer(base, typ.modifiers.bits()))
            }
            TypeKind::Array => {
                let base = typ.base?;
                Some(TypeKey::Array(base, typ.array_size))
            }
            TypeKind::Function => {
                let ret = typ.base?;
                let params = typ.params.clone().unwrap_or_default();
                Some(TypeKey::Function {
                    ret,
                    params,
                    variadic: typ.variadic,
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
    #[cfg(test)]
    pub fn composite(&self, id: TypeId) -> Option<&CompositeType> {
        self.get(id).composite.as_deref()
    }

    /// Format a type for display (with recursive base type printing)
    #[cfg(test)]
    pub fn format_type(&self, id: TypeId) -> String {
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
                    result.push_str(&self.format_type(base));
                    result.push('*');
                } else {
                    result.push('*');
                }
            }
            TypeKind::Array => {
                if let Some(base) = typ.base {
                    result.push_str(&self.format_type(base));
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
                    result.push_str(&self.format_type(ret));
                    result.push('(');
                    if let Some(params) = &typ.params {
                        for (i, &param) in params.iter().enumerate() {
                            if i > 0 {
                                result.push_str(", ");
                            }
                            result.push_str(&self.format_type(param));
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
        )
    }

    /// Check if type is a floating point type
    #[inline]
    pub fn is_float(&self, id: TypeId) -> bool {
        matches!(
            self.get(id).kind,
            TypeKind::Float | TypeKind::Double | TypeKind::LongDouble
        )
    }

    /// Check if type is an arithmetic type (integer or float)
    #[inline]
    pub fn is_arithmetic(&self, id: TypeId) -> bool {
        self.is_integer(id) || self.is_float(id)
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
            _ => id, // For non-integer types, just return the original
        }
    }

    /// Get the size of a type in bits
    pub fn size_bits(&self, id: TypeId) -> u32 {
        let typ = self.get(id);
        match typ.kind {
            TypeKind::Void => 0,
            TypeKind::Bool => 8,
            TypeKind::Char => 8,
            TypeKind::Short => 16,
            TypeKind::Int => 32,
            TypeKind::Long => 64,
            TypeKind::LongLong => 64,
            TypeKind::Float => 32,
            TypeKind::Double => 64,
            TypeKind::LongDouble => 128,
            TypeKind::Pointer => 64,
            TypeKind::Array => {
                let elem_size = typ.base.map(|b| self.size_bits(b)).unwrap_or(0);
                let count = typ.array_size.unwrap_or(0) as u32;
                elem_size * count
            }
            TypeKind::Struct | TypeKind::Union => {
                (typ.composite.as_ref().map(|c| c.size).unwrap_or(0) * 8) as u32
            }
            TypeKind::Function => 0,
            TypeKind::Enum => 32,
            TypeKind::VaList => {
                #[cfg(target_arch = "x86_64")]
                {
                    192
                }
                #[cfg(all(target_arch = "aarch64", target_os = "macos"))]
                {
                    64
                }
                #[cfg(all(target_arch = "aarch64", not(target_os = "macos")))]
                {
                    256
                }
                #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
                {
                    64
                }
            }
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

    /// Get natural alignment for a type in bytes
    pub fn alignment(&self, id: TypeId) -> usize {
        let typ = self.get(id);
        match typ.kind {
            TypeKind::Void => 1,
            TypeKind::Bool | TypeKind::Char => 1,
            TypeKind::Short => 2,
            TypeKind::Int | TypeKind::Float => 4,
            TypeKind::Long | TypeKind::LongLong | TypeKind::Double | TypeKind::Pointer => 8,
            TypeKind::LongDouble => 16,
            TypeKind::Struct | TypeKind::Union => {
                typ.composite.as_ref().map(|c| c.align).unwrap_or(1)
            }
            TypeKind::Enum => 4,
            TypeKind::Array => typ.base.map(|b| self.alignment(b)).unwrap_or(1),
            TypeKind::Function => 1,
            TypeKind::VaList => 8,
        }
    }

    /// Find a member in a struct/union type
    pub fn find_member(&self, id: TypeId, name: &str) -> Option<MemberInfo> {
        self.get(id).find_member(name)
    }

    /// Check if two types are compatible (for __builtin_types_compatible_p)
    pub fn types_compatible(&self, id1: TypeId, id2: TypeId) -> bool {
        // Quick check: same TypeId means same type
        if id1 == id2 {
            return true;
        }
        // Compare underlying types
        self.get(id1).types_compatible(self.get(id2))
    }

    /// Compute struct layout with natural alignment
    /// Updates member offsets in place and returns (total_size, alignment)
    pub fn compute_struct_layout(&self, members: &mut [StructMember]) -> (usize, usize) {
        let mut offset = 0usize;
        let mut max_align = 1usize;
        let mut current_bit_offset = 0u32;
        let mut current_storage_unit_size = 0u32;

        for member in members.iter_mut() {
            if let Some(bit_width) = member.bit_width {
                let storage_size = self.size_bytes(member.typ) as u32;
                let storage_bits = storage_size * 8;

                if bit_width == 0 {
                    if current_storage_unit_size > 0 {
                        offset += current_storage_unit_size as usize;
                        current_bit_offset = 0;
                        current_storage_unit_size = 0;
                    }
                    member.offset = offset;
                    member.bit_offset = None;
                    member.storage_unit_size = None;
                    continue;
                }

                let need_new_unit = current_storage_unit_size == 0
                    || current_storage_unit_size != storage_size
                    || current_bit_offset + bit_width > storage_bits;

                if need_new_unit {
                    if current_storage_unit_size > 0 {
                        offset += current_storage_unit_size as usize;
                    }
                    let align = storage_size as usize;
                    offset = (offset + align - 1) & !(align - 1);
                    max_align = max_align.max(align);
                    current_bit_offset = 0;
                    current_storage_unit_size = storage_size;
                }

                member.offset = offset;
                member.bit_offset = Some(current_bit_offset);
                member.storage_unit_size = Some(storage_size);
                current_bit_offset += bit_width;
            } else {
                if current_storage_unit_size > 0 {
                    offset += current_storage_unit_size as usize;
                    current_bit_offset = 0;
                    current_storage_unit_size = 0;
                }

                let align = self.alignment(member.typ);
                max_align = max_align.max(align);

                offset = (offset + align - 1) & !(align - 1);
                member.offset = offset;
                member.bit_offset = None;
                member.storage_unit_size = None;

                offset += self.size_bytes(member.typ);
            }
        }

        if current_storage_unit_size > 0 {
            offset += current_storage_unit_size as usize;
        }

        let size = if max_align > 1 {
            (offset + max_align - 1) & !(max_align - 1)
        } else {
            offset
        };
        (size, max_align)
    }

    /// Compute union layout (all members at offset 0)
    /// Returns (total_size, alignment)
    pub fn compute_union_layout(&self, members: &mut [StructMember]) -> (usize, usize) {
        let mut max_size = 0usize;
        let mut max_align = 1usize;

        for member in members.iter_mut() {
            member.offset = 0;
            max_size = max_size.max(self.size_bytes(member.typ));
            max_align = max_align.max(self.alignment(member.typ));
        }

        let size = if max_align > 1 {
            (max_size + max_align - 1) & !(max_align - 1)
        } else {
            max_size
        };
        (size, max_align)
    }
}

impl Default for TypeTable {
    fn default() -> Self {
        Self::new()
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
        let types = TypeTable::new();
        assert!(types.is_integer(types.int_id));
        assert!(types.is_arithmetic(types.int_id));
        assert!(types.is_scalar(types.int_id));
        assert!(!types.is_float(types.int_id));
    }

    #[test]
    fn test_pointer_type() {
        let mut types = TypeTable::new();
        let int_ptr_id = types.intern(Type::pointer(types.int_id));
        assert_eq!(types.kind(int_ptr_id), TypeKind::Pointer);
        assert!(types.is_scalar(int_ptr_id));
        assert!(!types.is_integer(int_ptr_id));

        let base_id = types.base_type(int_ptr_id).unwrap();
        assert_eq!(types.kind(base_id), TypeKind::Int);
    }

    #[test]
    fn test_array_type() {
        let mut types = TypeTable::new();
        let int_arr_id = types.intern(Type::array(types.int_id, 10));
        assert_eq!(types.kind(int_arr_id), TypeKind::Array);
        assert_eq!(types.array_size(int_arr_id), Some(10));

        let base_id = types.base_type(int_arr_id).unwrap();
        assert_eq!(types.kind(base_id), TypeKind::Int);
    }

    #[test]
    fn test_function_type() {
        let mut types = TypeTable::new();
        let func_id = types.intern(Type::function(
            types.int_id,
            vec![types.int_id, types.char_id],
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
        let types = TypeTable::new();
        assert!(types.is_unsigned(types.uint_id));
        assert!(!types.is_unsigned(types.int_id));
    }

    #[test]
    fn test_type_format() {
        let types = TypeTable::new();
        assert_eq!(types.format_type(types.int_id), "int");
        assert_eq!(types.format_type(types.uint_id), "unsigned int");
    }

    #[test]
    fn test_nested_pointer() {
        let mut types = TypeTable::new();
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
        let mut types = TypeTable::new();
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
        let int1 = Type::basic(TypeKind::Int);
        let int2 = Type::basic(TypeKind::Int);
        assert!(int1.types_compatible(&int2));

        let char1 = Type::basic(TypeKind::Char);
        let char2 = Type::basic(TypeKind::Char);
        assert!(char1.types_compatible(&char2));
    }

    #[test]
    fn test_types_compatible_different_types() {
        let int_type = Type::basic(TypeKind::Int);
        let char_type = Type::basic(TypeKind::Char);
        assert!(!int_type.types_compatible(&char_type));

        let long_type = Type::basic(TypeKind::Long);
        assert!(!int_type.types_compatible(&long_type));
    }

    #[test]
    fn test_types_compatible_qualifiers_ignored() {
        let int_type = Type::basic(TypeKind::Int);
        let const_int = Type::with_modifiers(TypeKind::Int, TypeModifiers::CONST);
        let volatile_int = Type::with_modifiers(TypeKind::Int, TypeModifiers::VOLATILE);
        let cv_int = Type::with_modifiers(
            TypeKind::Int,
            TypeModifiers::CONST | TypeModifiers::VOLATILE,
        );

        // All should be compatible with plain int
        assert!(int_type.types_compatible(&const_int));
        assert!(int_type.types_compatible(&volatile_int));
        assert!(int_type.types_compatible(&cv_int));
        assert!(const_int.types_compatible(&volatile_int));
    }

    #[test]
    fn test_types_compatible_signedness_matters() {
        let int_type = Type::basic(TypeKind::Int);
        let uint_type = Type::with_modifiers(TypeKind::Int, TypeModifiers::UNSIGNED);
        // Signedness is NOT a qualifier, so these are NOT compatible
        assert!(!int_type.types_compatible(&uint_type));
    }

    #[test]
    fn test_types_compatible_pointers() {
        let types = TypeTable::new();
        let int_ptr = Type::pointer(types.int_id);
        let int_ptr2 = Type::pointer(types.int_id);
        assert!(int_ptr.types_compatible(&int_ptr2));

        let char_ptr = Type::pointer(types.char_id);
        assert!(!int_ptr.types_compatible(&char_ptr));
    }

    #[test]
    fn test_types_compatible_arrays() {
        let types = TypeTable::new();
        let arr10 = Type::array(types.int_id, 10);
        let arr10_2 = Type::array(types.int_id, 10);
        assert!(arr10.types_compatible(&arr10_2));

        let arr20 = Type::array(types.int_id, 20);
        assert!(!arr10.types_compatible(&arr20));
    }

    #[test]
    fn test_type_deduplication() {
        let mut types = TypeTable::new();
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
        let types = TypeTable::new();
        // Pre-interned types should be valid
        assert!(types.int_id.is_valid());
        assert!(types.char_id.is_valid());
        assert!(types.void_id.is_valid());
        assert!(types.void_ptr_id.is_valid());

        // And have correct kinds
        assert_eq!(types.kind(types.int_id), TypeKind::Int);
        assert_eq!(types.kind(types.void_ptr_id), TypeKind::Pointer);
    }
}
