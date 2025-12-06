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

use std::fmt;

// ============================================================================
// Composite Type Components
// ============================================================================

/// A struct/union member
#[derive(Debug, Clone, PartialEq)]
pub struct StructMember {
    /// Member name (empty string for unnamed bitfields)
    pub name: String,
    /// Member type
    pub typ: Type,
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
#[derive(Debug, Clone)]
pub struct MemberInfo {
    /// Byte offset within struct
    pub offset: usize,
    /// Member type
    pub typ: Type,
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

    /// Compute struct layout with natural alignment, including bitfield packing
    /// Returns (total_size, alignment)
    pub fn compute_struct_layout(members: &mut [StructMember]) -> (usize, usize) {
        let mut offset = 0usize;
        let mut max_align = 1usize;
        let mut current_bit_offset = 0u32;
        let mut current_storage_unit_size = 0u32; // 0 means no active bitfield storage

        for member in members.iter_mut() {
            if let Some(bit_width) = member.bit_width {
                // This is a bitfield
                let storage_size = member.typ.size_bytes() as u32;
                let storage_bits = storage_size * 8;

                if bit_width == 0 {
                    // Zero-width bitfield: force alignment to next storage unit
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

                // Check if we need a new storage unit:
                // - No active storage unit
                // - Storage unit type changed
                // - Bitfield doesn't fit in remaining space
                let need_new_unit = current_storage_unit_size == 0
                    || current_storage_unit_size != storage_size
                    || current_bit_offset + bit_width > storage_bits;

                if need_new_unit {
                    // Close current storage unit if active
                    if current_storage_unit_size > 0 {
                        offset += current_storage_unit_size as usize;
                    }
                    // Align to storage unit size
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
                // Regular member - close any active bitfield storage unit
                if current_storage_unit_size > 0 {
                    offset += current_storage_unit_size as usize;
                    current_bit_offset = 0;
                    current_storage_unit_size = 0;
                }

                let align = member.typ.alignment();
                max_align = max_align.max(align);

                // Align offset to member's alignment
                offset = (offset + align - 1) & !(align - 1);
                member.offset = offset;
                member.bit_offset = None;
                member.storage_unit_size = None;

                // Advance by member size
                offset += member.typ.size_bytes();
            }
        }

        // Close final bitfield storage unit if active
        if current_storage_unit_size > 0 {
            offset += current_storage_unit_size as usize;
        }

        // Pad struct size to alignment
        let size = if max_align > 1 {
            (offset + max_align - 1) & !(max_align - 1)
        } else {
            offset
        };
        (size, max_align)
    }

    /// Compute union layout: all members at offset 0, size = max member size
    /// Returns (total_size, alignment)
    pub fn compute_union_layout(members: &mut [StructMember]) -> (usize, usize) {
        let mut max_size = 0usize;
        let mut max_align = 1usize;

        for member in members.iter_mut() {
            member.offset = 0; // All union members at offset 0
            max_size = max_size.max(member.typ.size_bytes());
            max_align = max_align.max(member.typ.alignment());
        }

        // Pad size to alignment
        let size = if max_align > 1 {
            (max_size + max_align - 1) & !(max_align - 1)
        } else {
            max_size
        };
        (size, max_align)
    }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
/// Types are built compositionally:
/// - `int *p` -> Pointer { base: Int }
/// - `int arr[10]` -> Array { base: Int, size: 10 }
/// - `int (*fp)(int)` -> Pointer { base: Function { return: Int, params: [Int] } }
#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    /// The kind of type
    pub kind: TypeKind,

    /// Type modifiers (const, volatile, signed, unsigned, etc.)
    pub modifiers: TypeModifiers,

    /// Base type for pointers, arrays, and function return types
    pub base: Option<Box<Type>>,

    /// Array size (for arrays)
    pub array_size: Option<usize>,

    /// Function parameter types (for functions)
    pub params: Option<Vec<Type>>,

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

    /// Create a pointer type
    pub fn pointer(base: Type) -> Self {
        Self {
            kind: TypeKind::Pointer,
            modifiers: TypeModifiers::empty(),
            base: Some(Box::new(base)),
            array_size: None,
            params: None,
            variadic: false,
            composite: None,
        }
    }

    /// Create an array type
    pub fn array(base: Type, size: usize) -> Self {
        Self {
            kind: TypeKind::Array,
            modifiers: TypeModifiers::empty(),
            base: Some(Box::new(base)),
            array_size: Some(size),
            params: None,
            variadic: false,
            composite: None,
        }
    }

    /// Create a function type
    pub fn function(return_type: Type, params: Vec<Type>, variadic: bool) -> Self {
        Self {
            kind: TypeKind::Function,
            modifiers: TypeModifiers::empty(),
            base: Some(Box::new(return_type)),
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

    /// Check if this is an integer type
    pub fn is_integer(&self) -> bool {
        matches!(
            self.kind,
            TypeKind::Bool
                | TypeKind::Char
                | TypeKind::Short
                | TypeKind::Int
                | TypeKind::Long
                | TypeKind::LongLong
        )
    }

    /// Check if this is a floating point type
    pub fn is_float(&self) -> bool {
        matches!(
            self.kind,
            TypeKind::Float | TypeKind::Double | TypeKind::LongDouble
        )
    }

    /// Check if this is an arithmetic type (integer or float)
    pub fn is_arithmetic(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    /// Check if this is a scalar type (arithmetic or pointer)
    pub fn is_scalar(&self) -> bool {
        self.is_arithmetic() || self.kind == TypeKind::Pointer
    }

    /// Check if this type is unsigned
    pub fn is_unsigned(&self) -> bool {
        self.modifiers.contains(TypeModifiers::UNSIGNED)
    }

    /// Check if this is a plain char (no explicit signed/unsigned)
    /// Plain char has platform-dependent signedness
    pub fn is_plain_char(&self) -> bool {
        self.kind == TypeKind::Char
            && !self.modifiers.contains(TypeModifiers::SIGNED)
            && !self.modifiers.contains(TypeModifiers::UNSIGNED)
    }

    /// Get the base type (for pointers, arrays, functions)
    pub fn get_base(&self) -> Option<&Type> {
        self.base.as_deref()
    }

    /// Get the size of this type in bits (for a typical 64-bit target)
    /// This is used by the IR to size operations
    pub fn size_bits(&self) -> u32 {
        match self.kind {
            TypeKind::Void => 0,
            TypeKind::Bool => 8,
            TypeKind::Char => 8,
            TypeKind::Short => 16,
            TypeKind::Int => 32,
            TypeKind::Long => 64, // LP64 model (macOS, Linux x86-64)
            TypeKind::LongLong => 64,
            TypeKind::Float => 32,
            TypeKind::Double => 64,
            TypeKind::LongDouble => 128, // x86-64
            TypeKind::Pointer => 64,     // 64-bit pointers
            TypeKind::Array => {
                let base_size = self.base.as_ref().map(|b| b.size_bits()).unwrap_or(0);
                let count = self.array_size.unwrap_or(0) as u32;
                base_size * count
            }
            TypeKind::Function => 0, // Functions don't have a size
            TypeKind::Struct | TypeKind::Union => (self.size_bytes() * 8) as u32,
            TypeKind::Enum => 32, // Enums are int-sized
            // va_list size is platform-specific:
            // - x86-64: 24 bytes (192 bits) - struct with 4 fields
            // - aarch64-macos: 8 bytes (64 bits) - just a char*
            // - aarch64-linux: 32 bytes (256 bits) - struct with 5 fields
            TypeKind::VaList => {
                #[cfg(target_arch = "x86_64")]
                {
                    192
                }
                #[cfg(all(target_arch = "aarch64", target_os = "macos"))]
                {
                    64 // On Apple ARM64, va_list is just char*
                }
                #[cfg(all(target_arch = "aarch64", not(target_os = "macos")))]
                {
                    256 // On Linux ARM64, va_list is a 32-byte struct
                }
                #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
                {
                    64 // Fallback to pointer size
                }
            }
        }
    }

    /// Get the size of this type in bytes
    pub fn size_bytes(&self) -> usize {
        match self.kind {
            TypeKind::Struct | TypeKind::Union => {
                self.composite.as_ref().map(|c| c.size).unwrap_or(0)
            }
            TypeKind::Enum => 4,
            _ => (self.size_bits() / 8) as usize,
        }
    }

    /// Get natural alignment for this type in bytes
    pub fn alignment(&self) -> usize {
        match self.kind {
            TypeKind::Void => 1,
            TypeKind::Bool | TypeKind::Char => 1,
            TypeKind::Short => 2,
            TypeKind::Int | TypeKind::Float => 4,
            TypeKind::Long | TypeKind::LongLong | TypeKind::Double | TypeKind::Pointer => 8,
            TypeKind::LongDouble => 16,
            TypeKind::Struct | TypeKind::Union => {
                self.composite.as_ref().map(|c| c.align).unwrap_or(1)
            }
            TypeKind::Enum => 4, // Enums are int-aligned
            TypeKind::Array => self.base.as_ref().map(|b| b.alignment()).unwrap_or(1),
            TypeKind::Function => 1,
            TypeKind::VaList => 8, // All platforms use 8-byte alignment for va_list
        }
    }

    /// Find a member in a struct/union type
    /// Returns MemberInfo with full bitfield details if found
    pub fn find_member(&self, name: &str) -> Option<MemberInfo> {
        if let Some(ref composite) = self.composite {
            for member in &composite.members {
                if member.name == name {
                    return Some(MemberInfo {
                        offset: member.offset,
                        typ: member.typ.clone(),
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

        // Compare base types (for pointers, arrays, functions)
        match (&self.base, &other.base) {
            (Some(a), Some(b)) => {
                if !a.types_compatible(b) {
                    return false;
                }
            }
            (None, None) => {}
            _ => return false,
        }

        // Compare function parameters
        match (&self.params, &other.params) {
            (Some(a), Some(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                for (pa, pb) in a.iter().zip(b.iter()) {
                    if !pa.types_compatible(pb) {
                        return false;
                    }
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

        match self.kind {
            TypeKind::Pointer => {
                if let Some(base) = &self.base {
                    write!(f, "{}*", base)
                } else {
                    write!(f, "*")
                }
            }
            TypeKind::Array => {
                if let Some(base) = &self.base {
                    if let Some(size) = self.array_size {
                        write!(f, "{}[{}]", base, size)
                    } else {
                        write!(f, "{}[]", base)
                    }
                } else {
                    write!(f, "[]")
                }
            }
            TypeKind::Function => {
                if let Some(ret) = &self.base {
                    write!(f, "{}(", ret)?;
                    if let Some(params) = &self.params {
                        for (i, param) in params.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{}", param)?;
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
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_types() {
        let int_type = Type::basic(TypeKind::Int);
        assert!(int_type.is_integer());
        assert!(int_type.is_arithmetic());
        assert!(int_type.is_scalar());
        assert!(!int_type.is_float());
    }

    #[test]
    fn test_pointer_type() {
        let int_ptr = Type::pointer(Type::basic(TypeKind::Int));
        assert_eq!(int_ptr.kind, TypeKind::Pointer);
        assert!(int_ptr.is_scalar());
        assert!(!int_ptr.is_integer());

        let base = int_ptr.get_base().unwrap();
        assert_eq!(base.kind, TypeKind::Int);
    }

    #[test]
    fn test_array_type() {
        let int_arr = Type::array(Type::basic(TypeKind::Int), 10);
        assert_eq!(int_arr.kind, TypeKind::Array);
        assert_eq!(int_arr.array_size, Some(10));

        let base = int_arr.get_base().unwrap();
        assert_eq!(base.kind, TypeKind::Int);
    }

    #[test]
    fn test_function_type() {
        let func = Type::function(
            Type::basic(TypeKind::Int),
            vec![Type::basic(TypeKind::Int), Type::basic(TypeKind::Char)],
            false,
        );
        assert_eq!(func.kind, TypeKind::Function);
        assert!(!func.variadic);

        let params = func.params.as_ref().unwrap();
        assert_eq!(params.len(), 2);
        assert_eq!(params[0].kind, TypeKind::Int);
        assert_eq!(params[1].kind, TypeKind::Char);
    }

    #[test]
    fn test_unsigned_modifier() {
        let uint = Type::with_modifiers(TypeKind::Int, TypeModifiers::UNSIGNED);
        assert!(uint.is_unsigned());
    }

    #[test]
    fn test_type_display() {
        let int_type = Type::basic(TypeKind::Int);
        assert_eq!(format!("{}", int_type), "int");

        let const_int = Type::with_modifiers(TypeKind::Int, TypeModifiers::CONST);
        assert_eq!(format!("{}", const_int), "const int");

        let uint = Type::with_modifiers(TypeKind::Int, TypeModifiers::UNSIGNED);
        assert_eq!(format!("{}", uint), "unsigned int");

        let int_ptr = Type::pointer(Type::basic(TypeKind::Int));
        assert_eq!(format!("{}", int_ptr), "int*");
    }

    #[test]
    fn test_nested_pointer() {
        // int **pp
        let int_ptr_ptr = Type::pointer(Type::pointer(Type::basic(TypeKind::Int)));
        assert_eq!(int_ptr_ptr.kind, TypeKind::Pointer);

        let inner = int_ptr_ptr.get_base().unwrap();
        assert_eq!(inner.kind, TypeKind::Pointer);

        let innermost = inner.get_base().unwrap();
        assert_eq!(innermost.kind, TypeKind::Int);
    }

    #[test]
    fn test_pointer_to_array() {
        // int (*p)[10] - pointer to array of 10 ints
        let arr_type = Type::array(Type::basic(TypeKind::Int), 10);
        let ptr_to_arr = Type::pointer(arr_type);

        assert_eq!(ptr_to_arr.kind, TypeKind::Pointer);
        let base = ptr_to_arr.get_base().unwrap();
        assert_eq!(base.kind, TypeKind::Array);
        assert_eq!(base.array_size, Some(10));
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
        let int_ptr = Type::pointer(Type::basic(TypeKind::Int));
        let int_ptr2 = Type::pointer(Type::basic(TypeKind::Int));
        assert!(int_ptr.types_compatible(&int_ptr2));

        let char_ptr = Type::pointer(Type::basic(TypeKind::Char));
        assert!(!int_ptr.types_compatible(&char_ptr));
    }

    #[test]
    fn test_types_compatible_arrays() {
        let arr10 = Type::array(Type::basic(TypeKind::Int), 10);
        let arr10_2 = Type::array(Type::basic(TypeKind::Int), 10);
        assert!(arr10.types_compatible(&arr10_2));

        let arr20 = Type::array(Type::basic(TypeKind::Int), 20);
        assert!(!arr10.types_compatible(&arr20));
    }
}
