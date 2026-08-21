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
        // `long double` is described entirely by `get_float_limit_macros`
        // and `get_additional_sizeof_macros`, which know the OS as well as the
        // architecture — it is quad on aarch64 Linux but plain double on
        // Apple, and this list cannot tell them apart.
        // char is unsigned on ARM by default
        ("__CHAR_UNSIGNED__", Some("1")),
        // Advanced SIMD is mandatory in the AArch64 base architecture, so
        // this is a fact about the target and gcc defines it unconditionally
        // here. It says nothing about whether <arm_neon.h> is available --
        // that is a fact about the *compiler*, and c17 does not ship one.
        // Guarded code that treats the first as implying the second will
        // still fail on the missing header; withdrawing a true statement
        // about the target is not the fix for that.
        ("__ARM_NEON", Some("1")),
        // __ARM_NEON__ is the AArch32 spelling, and gcc does *not* define it
        // on aarch64. c17 did, which was simply wrong.
        // FP support
        ("__ARM_FP", Some("14")), // VFPv3 compatible
        ("__ARM_FP16_FORMAT_IEEE", Some("1")),
        ("__ARM_FEATURE_FMA", Some("1")),
        // __GCC_HAVE_SYNC_COMPARE_AND_SWAP_{1,2,4,8} is deliberately absent;
        // see the note on the x86-64 list. c17 implements no `__sync_*`
        // builtin, so advertising them only sent guarded code into a wall.
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
        // 128-bit integer support
        ("__SIZEOF_INT128__", Some("16")),
    ]
}
