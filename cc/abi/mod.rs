//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// ABI (Application Binary Interface) handling for pcc
//
// This module provides platform-specific calling convention classification
// for function parameters and return values. It serves as the contract
// between the frontend (linearizer) and backend (code generator).
//
// Supported ABIs:
// - System V AMD64 (x86-64 Linux/BSD/macOS)
// - AAPCS64 (AArch64 Linux/macOS)
//

mod aapcs64;
mod sysv_amd64;

pub use aapcs64::Aapcs64Abi;
pub use sysv_amd64::SysVAmd64Abi;

use crate::target::{Arch, Target};
use crate::types::{TypeId, TypeKind, TypeTable};

// ============================================================================
// Register Classification
// ============================================================================

/// Classification of a single eightbyte (x86-64) or register slot.
///
/// Based on AMD64 ABI Section 3.2.3 classification classes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum RegClass {
    /// No data or padding (zero-sized type)
    #[default]
    NoClass,
    /// Integer or pointer type - uses GP register
    Integer,
    /// Floating-point type - uses SSE/FP register
    Sse,
    /// Must be passed on stack (too large, unaligned, or register exhausted)
    Memory,
}

impl RegClass {
    /// Merge two register classes according to AMD64-ABI 3.2.3p2 rules.
    ///
    /// The merging is used when determining the classification of struct
    /// fields that overlap the same eightbyte.
    pub fn merge(self, other: RegClass) -> RegClass {
        use RegClass::*;
        match (self, other) {
            // (a) If both classes are equal, this is the resulting class
            (a, b) if a == b => a,
            // (b) If one is NO_CLASS, the other is the result
            (NoClass, b) => b,
            (a, NoClass) => a,
            // (c) If one is MEMORY, the result is MEMORY
            (Memory, _) | (_, Memory) => Memory,
            // (d) If one is INTEGER, the result is INTEGER
            (Integer, _) | (_, Integer) => Integer,
            // (e) X87/X87UP/COMPLEX_X87 -> MEMORY (we don't support X87)
            // (f) Otherwise, SSE
            _ => Sse,
        }
    }
}

// ============================================================================
// Argument Classification
// ============================================================================

/// Base type for Homogeneous Floating-Point Aggregate (HFA) on AAPCS64.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HfaBase {
    /// 32-bit float
    Float32,
    /// 64-bit double
    Float64,
}

/// How an argument or return value should be passed.
///
/// This is the primary classification result used by the backend to
/// determine register/stack allocation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArgClass {
    /// Pass directly in register(s).
    /// For x86-64: classification per eightbyte determines GP vs SSE.
    /// For AArch64: simple scalars and small composites.
    Direct {
        /// Classification per eightbyte/slot (usually 1-2 elements)
        classes: Vec<RegClass>,
        /// Size in bits of the value
        size_bits: u32,
    },
    /// Pass by reference (caller allocates, passes pointer).
    /// Used for large structs that don't fit in registers.
    Indirect {
        /// Alignment requirement in bytes
        align: u32,
        /// Size in bits
        size_bits: u32,
    },
    /// Homogeneous Floating-Point Aggregate (AAPCS64 specific).
    /// Struct with 1-4 identical float/double members passed in FP registers.
    Hfa {
        /// Base type (float or double)
        base: HfaBase,
        /// Number of elements (1-4)
        count: u8,
    },
    /// Extend small integer to register width.
    /// Used for char, short, etc.
    Extend {
        /// True for signed extension, false for zero extension
        signed: bool,
        /// Original size in bits
        size_bits: u32,
    },
    /// Zero-sized type, ignored in parameter passing.
    Ignore,
}

// ============================================================================
// Calling Convention
// ============================================================================

/// Calling convention for a function.
///
/// This can be overridden per-function using attributes like
/// `__attribute__((sysv_abi))` or `__attribute__((ms_abi))`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum CallingConv {
    /// Default C calling convention for the target platform.
    #[default]
    C,
    /// Force System V AMD64 ABI (e.g., `__attribute__((sysv_abi))`).
    SysV,
    /// Force Microsoft x64 ABI (e.g., `__attribute__((ms_abi))`).
    Win64,
}

// ============================================================================
// ABI Trait
// ============================================================================

/// Trait for platform-specific ABI classification.
///
/// Implementations provide the rules for classifying C types according
/// to the platform's calling convention.
pub trait Abi {
    /// Classify a parameter type.
    fn classify_param(&self, ty: TypeId, types: &TypeTable) -> ArgClass;

    /// Classify a return type.
    fn classify_return(&self, ty: TypeId, types: &TypeTable) -> ArgClass;
}

// ============================================================================
// ABI Factory
// ============================================================================

/// Get the appropriate ABI implementation for a target.
pub fn get_abi(target: &Target) -> Box<dyn Abi> {
    match target.arch {
        Arch::X86_64 => Box::new(SysVAmd64Abi::new()),
        Arch::Aarch64 => Box::new(Aapcs64Abi::new()),
    }
}

/// Get an ABI implementation for a specific calling convention override.
pub fn get_abi_for_conv(conv: CallingConv, target: &Target) -> Box<dyn Abi> {
    match conv {
        CallingConv::C => get_abi(target),
        CallingConv::SysV => Box::new(SysVAmd64Abi::new()),
        CallingConv::Win64 => {
            // TODO: Implement Win64 ABI when needed
            // For now, fall back to the target default
            get_abi(target)
        }
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Check if a type is a struct or union.
fn is_aggregate(kind: TypeKind) -> bool {
    matches!(kind, TypeKind::Struct | TypeKind::Union)
}

/// Check if a type is a floating-point type.
fn is_float(kind: TypeKind) -> bool {
    matches!(
        kind,
        TypeKind::Float | TypeKind::Double | TypeKind::LongDouble
    )
}

/// Check if a type is an integer type (including char, bool, enums).
/// Note: signedness is tracked via TypeModifiers::UNSIGNED, not separate TypeKind variants.
fn is_integer(kind: TypeKind) -> bool {
    matches!(
        kind,
        TypeKind::Char
            | TypeKind::Short
            | TypeKind::Int
            | TypeKind::Long
            | TypeKind::LongLong
            | TypeKind::Bool
            | TypeKind::Enum
    )
}

/// Check if a type is a pointer type.
fn is_pointer(kind: TypeKind) -> bool {
    kind == TypeKind::Pointer
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_regclass_merge() {
        use RegClass::*;
        // Same class returns that class
        assert_eq!(Integer.merge(Integer), Integer);
        assert_eq!(Sse.merge(Sse), Sse);

        // NoClass with anything returns the other
        assert_eq!(NoClass.merge(Integer), Integer);
        assert_eq!(Sse.merge(NoClass), Sse);

        // Memory dominates
        assert_eq!(Memory.merge(Integer), Memory);
        assert_eq!(Sse.merge(Memory), Memory);

        // Integer dominates over SSE
        assert_eq!(Integer.merge(Sse), Integer);
        assert_eq!(Sse.merge(Integer), Integer);
    }

    #[test]
    fn test_sysv_amd64_basic_types() {
        use crate::types::TypeTable;

        let types = TypeTable::new(64);
        let abi = SysVAmd64Abi::new();

        // Integer types -> INTEGER class
        let int_class = abi.classify_param(types.int_id, &types);
        assert!(
            matches!(int_class, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Integer])
        );

        let long_class = abi.classify_param(types.long_id, &types);
        assert!(
            matches!(long_class, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Integer])
        );

        // Pointer -> INTEGER class
        let ptr_id = types.pointer_to(types.int_id);
        let ptr_class = abi.classify_param(ptr_id, &types);
        assert!(
            matches!(ptr_class, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Integer])
        );

        // Float/double -> SSE class
        let float_class = abi.classify_param(types.float_id, &types);
        assert!(
            matches!(float_class, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Sse])
        );

        let double_class = abi.classify_param(types.double_id, &types);
        assert!(
            matches!(double_class, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Sse])
        );

        // Void -> Ignore
        let void_class = abi.classify_param(types.void_id, &types);
        assert!(matches!(void_class, ArgClass::Ignore));
    }

    #[test]
    fn test_sysv_amd64_small_integers() {
        use crate::types::TypeTable;

        let types = TypeTable::new(64);
        let abi = SysVAmd64Abi::new();

        // char (8 bits) needs extension
        let char_class = abi.classify_param(types.char_id, &types);
        assert!(matches!(char_class, ArgClass::Extend { size_bits: 8, .. }));

        // short (16 bits) needs extension
        let short_class = abi.classify_param(types.short_id, &types);
        assert!(matches!(
            short_class,
            ArgClass::Extend { size_bits: 16, .. }
        ));
    }

    #[test]
    fn test_sysv_amd64_return_values() {
        use crate::types::TypeTable;

        let types = TypeTable::new(64);
        let abi = SysVAmd64Abi::new();

        // Integer return -> INTEGER
        let int_ret = abi.classify_return(types.int_id, &types);
        assert!(
            matches!(int_ret, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Integer])
        );

        // Float return -> SSE
        let float_ret = abi.classify_return(types.float_id, &types);
        assert!(
            matches!(float_ret, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Sse])
        );

        // Void return -> Ignore
        let void_ret = abi.classify_return(types.void_id, &types);
        assert!(matches!(void_ret, ArgClass::Ignore));
    }

    #[test]
    fn test_aapcs64_basic_types() {
        use crate::types::TypeTable;

        let types = TypeTable::new(64);
        let abi = Aapcs64Abi::new();

        // Integer types -> INTEGER class (in X registers)
        let int_class = abi.classify_param(types.int_id, &types);
        assert!(
            matches!(int_class, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Integer])
        );

        // Float/double -> SSE class (in V registers)
        let float_class = abi.classify_param(types.float_id, &types);
        assert!(
            matches!(float_class, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Sse])
        );

        let double_class = abi.classify_param(types.double_id, &types);
        assert!(
            matches!(double_class, ArgClass::Direct { classes, .. } if classes == vec![RegClass::Sse])
        );
    }

    #[test]
    fn test_get_abi_factory() {
        use crate::target::{Arch, Os, Target};

        // x86-64 should get SysV ABI
        let x86_target = Target::new(Arch::X86_64, Os::Linux);
        let _x86_abi = get_abi(&x86_target);

        // AArch64 Linux should get AAPCS64
        let aarch64_target = Target::new(Arch::Aarch64, Os::Linux);
        let _aarch64_abi = get_abi(&aarch64_target);

        // AArch64 macOS also gets AAPCS64
        let darwin_target = Target::new(Arch::Aarch64, Os::MacOS);
        let _darwin_abi = get_abi(&darwin_target);
    }
}
