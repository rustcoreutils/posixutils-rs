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
        // `__arm64__` is Apple's spelling, in `get_darwin_macros`.
        ("__ARM_ARCH", Some("8")),
        ("__ARM_64BIT_STATE", Some("1")),
        ("__ARM_ARCH_ISA_A64", Some("1")),
        // Byte order is in `arch::get_misc_macros`, for every target.
        ("__AARCH64EL__", Some("1")),
        // Register size
        ("__REGISTER_PREFIX__", Some("")),
        // `long double` is described entirely by `get_float_limit_macros`
        // and `get_additional_sizeof_macros`, which know the OS as well as the
        // architecture — it is quad on aarch64 Linux but plain double on
        // Apple, and this list cannot tell them apart.
        // `__CHAR_UNSIGNED__` is not here: plain `char` is unsigned under
        // AAPCS64 but signed on Apple arm64, so `get_arch_macros` defines it
        // from `Target::plain_char`.
        // Advanced SIMD is mandatory in the AArch64 base architecture, so
        // this is a fact about the target and gcc defines it unconditionally
        // here. It says nothing about whether <arm_neon.h> is available --
        // that is a fact about the *compiler*, and c17 does not ship one.
        // Guarded code that treats the first as implying the second will
        // still fail on the missing header; withdrawing a true statement
        // about the target is not the fix for that.
        ("__ARM_NEON", Some("1")),
        // __ARM_NEON__ is the AArch32 spelling, and gcc does *not* define it
        // on aarch64.
        // FP support
        ("__ARM_FP", Some("14")), // VFPv3 compatible
        ("__ARM_FP16_FORMAT_IEEE", Some("1")),
        ("__ARM_FEATURE_FMA", Some("1")),
        // The `__sync_*` family is implemented, so the macro that guards it is
        // a true statement about this compiler and is defined. It says the
        // compare-and-swap builtins exist at 1, 2, 4 and 8 bytes, which is
        // exactly c17's lock-free ceiling; 16 is absent because there is no
        // 16-byte atomic here, as there is none in gcc's own default.
        ("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1", Some("1")),
        ("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2", Some("1")),
        ("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4", Some("1")),
        ("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8", Some("1")),
        // The __GCC_ATOMIC_*_LOCK_FREE family is derived from the type
        // sizes, in `arch::get_atomic_macros`.
        // ARM-specific features
        ("__ARM_SIZEOF_WCHAR_T", Some("4")),
        ("__ARM_SIZEOF_MINIMAL_ENUM", Some("4")),
        ("__ARM_FEATURE_UNALIGNED", Some("1")),
        ("__ARM_FEATURE_CLZ", Some("1")),
        // 128-bit integer support
        ("__SIZEOF_INT128__", Some("16")),
    ]
}

/// The macros clang adds for arm64 on Darwin alone. gcc on aarch64 Linux
/// defines neither, and code reads `__arm64__` as "Apple".
pub fn get_darwin_macros() -> Vec<(&'static str, Option<&'static str>)> {
    vec![("__arm64__", Some("1")), ("__arm64", Some("1"))]
}
