//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Architecture-specific predefined macros and code generators
//

/// Default capacity for LIR instruction buffers (reduces reallocation overhead)
pub const DEFAULT_LIR_BUFFER_CAPACITY: usize = 5000;

pub mod aarch64;
pub mod codegen;
pub mod dwarf;
pub mod lir;
pub mod regalloc;
pub mod x86_64;

// Re-export inline asm support traits and functions
pub use codegen::{substitute_asm_operands, AsmOperandFormatter};

use crate::target::{Arch, Target};

/// Get architecture-specific predefined macros as (name, value) pairs
pub fn get_arch_macros(target: &Target) -> Vec<(&'static str, Option<&'static str>)> {
    // long is 64-bit on LP64 (Unix), 32-bit on LLP64 (Windows)
    let long_size = if target.long_width == 64 { "8" } else { "4" };

    // Signedness: __CHAR_UNSIGNED__ is defined only if char is unsigned
    let char_unsigned: Option<&'static str> = if target.char_signed { None } else { Some("1") };

    let mut macros = vec![
        // Common architecture macros based on type sizes
        ("__CHAR_BIT__", Some("8")),
        ("__SIZEOF_POINTER__", Some("8")),
        // Type sizes
        ("__SIZEOF_SHORT__", Some("2")),
        ("__SIZEOF_INT__", Some("4")),
        ("__SIZEOF_LONG__", Some(long_size)),
        ("__SIZEOF_LONG_LONG__", Some("8")),
        ("__SIZEOF_FLOAT__", Some("4")),
        ("__SIZEOF_DOUBLE__", Some("8")),
        ("__CHAR_UNSIGNED__", char_unsigned),
    ];

    // LP64 macros only on LP64 targets (Unix), not on LLP64 (Windows)
    if target.long_width == 64 {
        macros.push(("__LP64__", Some("1")));
        macros.push(("_LP64", Some("1")));
    }

    // Architecture-specific
    match target.arch {
        Arch::X86_64 => {
            macros.extend(x86_64::get_macros());
        }
        Arch::Aarch64 => {
            macros.extend(aarch64::get_macros());
        }
    }

    macros
}

/// Get type limit macros (for <limits.h> compatibility)
pub fn get_limit_macros(target: &Target) -> Vec<(&'static str, &'static str)> {
    // long is 64-bit on LP64 (Unix), 32-bit on LLP64 (Windows)
    let (long_max, long_width) = if target.long_width == 64 {
        ("9223372036854775807L", "64")
    } else {
        ("2147483647L", "32")
    };

    vec![
        // Character limits
        ("__SCHAR_MAX__", "127"),
        ("__SHRT_MAX__", "32767"),
        ("__INT_MAX__", "2147483647"),
        ("__LONG_MAX__", long_max),
        ("__LONG_LONG_MAX__", "9223372036854775807LL"),
        // Width macros
        ("__SCHAR_WIDTH__", "8"),
        ("__SHRT_WIDTH__", "16"),
        ("__INT_WIDTH__", "32"),
        ("__LONG_WIDTH__", long_width),
        ("__LLONG_WIDTH__", "64"),
        // Size type (always 64-bit on our targets)
        ("__SIZE_MAX__", "18446744073709551615UL"),
        ("__SIZE_WIDTH__", "64"),
        ("__PTRDIFF_MAX__", "9223372036854775807L"),
        ("__PTRDIFF_WIDTH__", "64"),
        ("__INTPTR_MAX__", "9223372036854775807L"),
        ("__INTPTR_WIDTH__", "64"),
        ("__UINTPTR_MAX__", "18446744073709551615UL"),
    ]
}

/// Get type definition macros (for <stdint.h> and <stddef.h> compatibility)
/// These define the underlying C type for various abstract types
pub fn get_type_macros(_target: &Target) -> Vec<(&'static str, &'static str)> {
    vec![
        // Fixed-width integer types
        ("__INT8_TYPE__", "signed char"),
        ("__INT16_TYPE__", "short"),
        ("__INT32_TYPE__", "int"),
        ("__INT64_TYPE__", "long long int"),
        ("__UINT8_TYPE__", "unsigned char"),
        ("__UINT16_TYPE__", "unsigned short"),
        ("__UINT32_TYPE__", "unsigned int"),
        ("__UINT64_TYPE__", "long long unsigned int"),
        // Least-width integer types (same as fixed-width for common targets)
        ("__INT_LEAST8_TYPE__", "signed char"),
        ("__INT_LEAST16_TYPE__", "short"),
        ("__INT_LEAST32_TYPE__", "int"),
        ("__INT_LEAST64_TYPE__", "long long int"),
        ("__UINT_LEAST8_TYPE__", "unsigned char"),
        ("__UINT_LEAST16_TYPE__", "unsigned short"),
        ("__UINT_LEAST32_TYPE__", "unsigned int"),
        ("__UINT_LEAST64_TYPE__", "long long unsigned int"),
        // Fast integer types (same as fixed-width for common targets)
        ("__INT_FAST8_TYPE__", "signed char"),
        ("__INT_FAST16_TYPE__", "short"),
        ("__INT_FAST32_TYPE__", "int"),
        ("__INT_FAST64_TYPE__", "long long int"),
        ("__UINT_FAST8_TYPE__", "unsigned char"),
        ("__UINT_FAST16_TYPE__", "unsigned short"),
        ("__UINT_FAST32_TYPE__", "unsigned int"),
        ("__UINT_FAST64_TYPE__", "long long unsigned int"),
        // Pointer-width types (always 64-bit on our targets)
        ("__SIZE_TYPE__", "long unsigned int"),
        ("__PTRDIFF_TYPE__", "long int"),
        ("__INTPTR_TYPE__", "long int"),
        ("__UINTPTR_TYPE__", "long unsigned int"),
        ("__INTMAX_TYPE__", "long int"),
        ("__UINTMAX_TYPE__", "long unsigned int"),
        // Character types
        ("__WCHAR_TYPE__", "int"),
        ("__WINT_TYPE__", "int"),
        ("__CHAR16_TYPE__", "unsigned short"),
        ("__CHAR32_TYPE__", "unsigned int"),
        // sig_atomic_t
        ("__SIG_ATOMIC_TYPE__", "int"),
    ]
}

/// Get fixed-width integer limit macros (for <stdint.h> compatibility)
pub fn get_stdint_limit_macros(_target: &Target) -> Vec<(&'static str, &'static str)> {
    vec![
        // Signed fixed-width limits
        ("__INT8_MAX__", "127"),
        ("__INT16_MAX__", "32767"),
        ("__INT32_MAX__", "2147483647"),
        ("__INT64_MAX__", "9223372036854775807LL"),
        // Unsigned fixed-width limits
        ("__UINT8_MAX__", "255"),
        ("__UINT16_MAX__", "65535"),
        ("__UINT32_MAX__", "4294967295U"),
        ("__UINT64_MAX__", "18446744073709551615ULL"),
        // Least-width limits (same as fixed-width)
        ("__INT_LEAST8_MAX__", "127"),
        ("__INT_LEAST16_MAX__", "32767"),
        ("__INT_LEAST32_MAX__", "2147483647"),
        ("__INT_LEAST64_MAX__", "9223372036854775807LL"),
        ("__UINT_LEAST8_MAX__", "255"),
        ("__UINT_LEAST16_MAX__", "65535"),
        ("__UINT_LEAST32_MAX__", "4294967295U"),
        ("__UINT_LEAST64_MAX__", "18446744073709551615ULL"),
        // Fast limits (same as fixed-width)
        ("__INT_FAST8_MAX__", "127"),
        ("__INT_FAST16_MAX__", "32767"),
        ("__INT_FAST32_MAX__", "2147483647"),
        ("__INT_FAST64_MAX__", "9223372036854775807LL"),
        ("__UINT_FAST8_MAX__", "255"),
        ("__UINT_FAST16_MAX__", "65535"),
        ("__UINT_FAST32_MAX__", "4294967295U"),
        ("__UINT_FAST64_MAX__", "18446744073709551615ULL"),
        // Width macros for fixed-width types
        ("__INT8_WIDTH__", "8"),
        ("__INT16_WIDTH__", "16"),
        ("__INT32_WIDTH__", "32"),
        ("__INT64_WIDTH__", "64"),
        ("__INT_LEAST8_WIDTH__", "8"),
        ("__INT_LEAST16_WIDTH__", "16"),
        ("__INT_LEAST32_WIDTH__", "32"),
        ("__INT_LEAST64_WIDTH__", "64"),
        ("__INT_FAST8_WIDTH__", "8"),
        ("__INT_FAST16_WIDTH__", "16"),
        ("__INT_FAST32_WIDTH__", "32"),
        ("__INT_FAST64_WIDTH__", "64"),
        // intmax_t limits (64-bit on all our targets)
        ("__INTMAX_MAX__", "9223372036854775807L"),
        ("__UINTMAX_MAX__", "18446744073709551615UL"),
        ("__INTMAX_WIDTH__", "64"),
        ("__UINTMAX_WIDTH__", "64"),
        // wchar_t and wint_t limits
        ("__WCHAR_MAX__", "2147483647"),
        ("__WCHAR_WIDTH__", "32"),
        ("__WINT_MAX__", "2147483647"),
        ("__WINT_WIDTH__", "32"),
        // sig_atomic_t limits
        ("__SIG_ATOMIC_MAX__", "2147483647"),
        ("__SIG_ATOMIC_WIDTH__", "32"),
    ]
}

/// Get integer constant suffix macros (for <stdint.h> compatibility)
pub fn get_suffix_macros(_target: &Target) -> Vec<(&'static str, &'static str)> {
    vec![
        // Fixed-width suffixes
        ("__INT8_C_SUFFIX__", ""),
        ("__INT16_C_SUFFIX__", ""),
        ("__INT32_C_SUFFIX__", ""),
        ("__INT64_C_SUFFIX__", "LL"),
        ("__UINT8_C_SUFFIX__", ""),
        ("__UINT16_C_SUFFIX__", ""),
        ("__UINT32_C_SUFFIX__", "U"),
        ("__UINT64_C_SUFFIX__", "ULL"),
        // intmax_t suffixes (64-bit uses L suffix on all our targets)
        ("__INTMAX_C_SUFFIX__", "L"),
        ("__UINTMAX_C_SUFFIX__", "UL"),
    ]
}

/// Get format specifier macros (for <inttypes.h> compatibility)
pub fn get_format_macros(_target: &Target) -> Vec<(&'static str, &'static str)> {
    vec![
        // Signed format specifiers
        ("__INT8_FMTd__", "\"hhd\""),
        ("__INT8_FMTi__", "\"hhi\""),
        ("__INT16_FMTd__", "\"hd\""),
        ("__INT16_FMTi__", "\"hi\""),
        ("__INT32_FMTd__", "\"d\""),
        ("__INT32_FMTi__", "\"i\""),
        ("__INT64_FMTd__", "\"lld\""),
        ("__INT64_FMTi__", "\"lli\""),
        // Unsigned format specifiers
        ("__UINT8_FMTo__", "\"hho\""),
        ("__UINT8_FMTu__", "\"hhu\""),
        ("__UINT8_FMTx__", "\"hhx\""),
        ("__UINT8_FMTX__", "\"hhX\""),
        ("__UINT16_FMTo__", "\"ho\""),
        ("__UINT16_FMTu__", "\"hu\""),
        ("__UINT16_FMTx__", "\"hx\""),
        ("__UINT16_FMTX__", "\"hX\""),
        ("__UINT32_FMTo__", "\"o\""),
        ("__UINT32_FMTu__", "\"u\""),
        ("__UINT32_FMTx__", "\"x\""),
        ("__UINT32_FMTX__", "\"X\""),
        ("__UINT64_FMTo__", "\"llo\""),
        ("__UINT64_FMTu__", "\"llu\""),
        ("__UINT64_FMTx__", "\"llx\""),
        ("__UINT64_FMTX__", "\"llX\""),
        // Least-width format specifiers (same as fixed-width)
        ("__INT_LEAST8_FMTd__", "\"hhd\""),
        ("__INT_LEAST8_FMTi__", "\"hhi\""),
        ("__INT_LEAST16_FMTd__", "\"hd\""),
        ("__INT_LEAST16_FMTi__", "\"hi\""),
        ("__INT_LEAST32_FMTd__", "\"d\""),
        ("__INT_LEAST32_FMTi__", "\"i\""),
        ("__INT_LEAST64_FMTd__", "\"lld\""),
        ("__INT_LEAST64_FMTi__", "\"lli\""),
        ("__UINT_LEAST8_FMTo__", "\"hho\""),
        ("__UINT_LEAST8_FMTu__", "\"hhu\""),
        ("__UINT_LEAST8_FMTx__", "\"hhx\""),
        ("__UINT_LEAST8_FMTX__", "\"hhX\""),
        ("__UINT_LEAST16_FMTo__", "\"ho\""),
        ("__UINT_LEAST16_FMTu__", "\"hu\""),
        ("__UINT_LEAST16_FMTx__", "\"hx\""),
        ("__UINT_LEAST16_FMTX__", "\"hX\""),
        ("__UINT_LEAST32_FMTo__", "\"o\""),
        ("__UINT_LEAST32_FMTu__", "\"u\""),
        ("__UINT_LEAST32_FMTx__", "\"x\""),
        ("__UINT_LEAST32_FMTX__", "\"X\""),
        ("__UINT_LEAST64_FMTo__", "\"llo\""),
        ("__UINT_LEAST64_FMTu__", "\"llu\""),
        ("__UINT_LEAST64_FMTx__", "\"llx\""),
        ("__UINT_LEAST64_FMTX__", "\"llX\""),
        // Fast format specifiers (same as fixed-width)
        ("__INT_FAST8_FMTd__", "\"hhd\""),
        ("__INT_FAST8_FMTi__", "\"hhi\""),
        ("__INT_FAST16_FMTd__", "\"hd\""),
        ("__INT_FAST16_FMTi__", "\"hi\""),
        ("__INT_FAST32_FMTd__", "\"d\""),
        ("__INT_FAST32_FMTi__", "\"i\""),
        ("__INT_FAST64_FMTd__", "\"lld\""),
        ("__INT_FAST64_FMTi__", "\"lli\""),
        ("__UINT_FAST8_FMTo__", "\"hho\""),
        ("__UINT_FAST8_FMTu__", "\"hhu\""),
        ("__UINT_FAST8_FMTx__", "\"hhx\""),
        ("__UINT_FAST8_FMTX__", "\"hhX\""),
        ("__UINT_FAST16_FMTo__", "\"ho\""),
        ("__UINT_FAST16_FMTu__", "\"hu\""),
        ("__UINT_FAST16_FMTx__", "\"hx\""),
        ("__UINT_FAST16_FMTX__", "\"hX\""),
        ("__UINT_FAST32_FMTo__", "\"o\""),
        ("__UINT_FAST32_FMTu__", "\"u\""),
        ("__UINT_FAST32_FMTx__", "\"x\""),
        ("__UINT_FAST32_FMTX__", "\"X\""),
        ("__UINT_FAST64_FMTo__", "\"llo\""),
        ("__UINT_FAST64_FMTu__", "\"llu\""),
        ("__UINT_FAST64_FMTx__", "\"llx\""),
        ("__UINT_FAST64_FMTX__", "\"llX\""),
        // intmax_t format specifiers (64-bit uses l format on all our targets)
        ("__INTMAX_FMTd__", "\"ld\""),
        ("__INTMAX_FMTi__", "\"li\""),
        ("__UINTMAX_FMTo__", "\"lo\""),
        ("__UINTMAX_FMTu__", "\"lu\""),
        ("__UINTMAX_FMTx__", "\"lx\""),
        ("__UINTMAX_FMTX__", "\"lX\""),
        // intptr_t format specifiers (64-bit uses l format)
        ("__INTPTR_FMTd__", "\"ld\""),
        ("__INTPTR_FMTi__", "\"li\""),
        ("__UINTPTR_FMTo__", "\"lo\""),
        ("__UINTPTR_FMTu__", "\"lu\""),
        ("__UINTPTR_FMTx__", "\"lx\""),
        ("__UINTPTR_FMTX__", "\"lX\""),
        // ptrdiff_t format specifiers
        ("__PTRDIFF_FMTd__", "\"ld\""),
        ("__PTRDIFF_FMTi__", "\"li\""),
        // size_t format specifiers
        ("__SIZE_FMTo__", "\"lo\""),
        ("__SIZE_FMTu__", "\"lu\""),
        ("__SIZE_FMTx__", "\"lx\""),
        ("__SIZE_FMTX__", "\"lX\""),
    ]
}

/// Get additional sizeof macros
pub fn get_additional_sizeof_macros(_target: &Target) -> Vec<(&'static str, &'static str)> {
    vec![
        // size_t and ptrdiff_t sizes (always 8 bytes on 64-bit)
        ("__SIZEOF_SIZE_T__", "8"),
        ("__SIZEOF_PTRDIFF_T__", "8"),
        // wchar_t and wint_t sizes
        ("__SIZEOF_WCHAR_T__", "4"),
        ("__SIZEOF_WINT_T__", "4"),
    ]
}

/// Get miscellaneous macros
pub fn get_misc_macros(_target: &Target) -> Vec<(&'static str, &'static str)> {
    vec![
        // Pointer width in bits (always 64 on our targets)
        ("__POINTER_WIDTH__", "64"),
        // Alignment
        ("__BIGGEST_ALIGNMENT__", "16"),
        ("__BOOL_WIDTH__", "8"),
        // Byte order (all our supported architectures are little-endian)
        ("__ORDER_LITTLE_ENDIAN__", "1234"),
        ("__ORDER_BIG_ENDIAN__", "4321"),
        ("__ORDER_PDP_ENDIAN__", "3412"),
        ("__BYTE_ORDER__", "__ORDER_LITTLE_ENDIAN__"),
        ("__LITTLE_ENDIAN__", "1"),
        // Floating point
        ("__FLT_RADIX__", "2"),
        ("__FINITE_MATH_ONLY__", "0"),
    ]
}
