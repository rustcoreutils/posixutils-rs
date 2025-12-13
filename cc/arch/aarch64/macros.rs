//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// AArch64-specific predefined macros
//

/// Get AArch64-specific predefined macros
pub fn get_macros() -> Vec<(&'static str, Option<&'static str>)> {
    vec![
        // Architecture identification
        ("__aarch64__", Some("1")),
        ("__arm64__", Some("1")), // macOS uses this
        ("__ARM_ARCH", Some("8")),
        ("__ARM_64BIT_STATE", Some("1")),
        ("__ARM_ARCH_ISA_A64", Some("1")),
        // Byte order (AArch64 is little-endian by default)
        ("__BYTE_ORDER__", Some("__ORDER_LITTLE_ENDIAN__")),
        ("__ORDER_LITTLE_ENDIAN__", Some("1234")),
        ("__ORDER_BIG_ENDIAN__", Some("4321")),
        ("__LITTLE_ENDIAN__", Some("1")),
        ("__AARCH64EL__", Some("1")),
        // Register size
        ("__REGISTER_PREFIX__", Some("")),
        ("__USER_LABEL_PREFIX__", Some("")),
        // Long double is 128-bit IEEE quad precision on AArch64
        ("__SIZEOF_LONG_DOUBLE__", Some("16")),
        ("__LDBL_MANT_DIG__", Some("113")),
        ("__LDBL_DIG__", Some("33")),
        // char is unsigned on ARM by default
        ("__CHAR_UNSIGNED__", Some("1")),
        // SIMD
        ("__ARM_NEON", Some("1")),
        ("__ARM_NEON__", Some("1")),
        // FP support
        ("__ARM_FP", Some("14")), // VFPv3 compatible
        ("__ARM_FP16_FORMAT_IEEE", Some("1")),
        ("__ARM_FEATURE_FMA", Some("1")),
        // Atomic primitives
        ("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1", Some("1")),
        ("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2", Some("1")),
        ("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4", Some("1")),
        ("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8", Some("1")),
        // Lock-free atomics
        ("__GCC_ATOMIC_BOOL_LOCK_FREE", Some("2")),
        ("__GCC_ATOMIC_CHAR_LOCK_FREE", Some("2")),
        ("__GCC_ATOMIC_SHORT_LOCK_FREE", Some("2")),
        ("__GCC_ATOMIC_INT_LOCK_FREE", Some("2")),
        ("__GCC_ATOMIC_LONG_LOCK_FREE", Some("2")),
        ("__GCC_ATOMIC_LLONG_LOCK_FREE", Some("2")),
        ("__GCC_ATOMIC_POINTER_LOCK_FREE", Some("2")),
        // ARM-specific features
        ("__ARM_SIZEOF_WCHAR_T", Some("4")),
        ("__ARM_SIZEOF_MINIMAL_ENUM", Some("4")),
        ("__ARM_FEATURE_UNALIGNED", Some("1")),
        ("__ARM_FEATURE_CLZ", Some("1")),
    ]
}
