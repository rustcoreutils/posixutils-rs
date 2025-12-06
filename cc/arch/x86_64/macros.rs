//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// x86_64-specific predefined macros
//

/// Get x86_64-specific predefined macros
pub fn get_macros() -> Vec<(&'static str, Option<&'static str>)> {
    vec![
        // Architecture identification
        ("__x86_64__", Some("1")),
        ("__x86_64", Some("1")),
        ("__amd64__", Some("1")),
        ("__amd64", Some("1")),
        // Byte order
        ("__BYTE_ORDER__", Some("__ORDER_LITTLE_ENDIAN__")),
        ("__ORDER_LITTLE_ENDIAN__", Some("1234")),
        ("__ORDER_BIG_ENDIAN__", Some("4321")),
        ("__ORDER_PDP_ENDIAN__", Some("3412")),
        ("__LITTLE_ENDIAN__", Some("1")),
        // Register size
        ("__REGISTER_PREFIX__", Some("")),
        ("__USER_LABEL_PREFIX__", Some("")),
        // Long double is 80-bit extended precision (padded to 128 bits)
        ("__SIZEOF_LONG_DOUBLE__", Some("16")),
        ("__LDBL_MANT_DIG__", Some("64")),
        ("__LDBL_DIG__", Some("18")),
        // SSE is baseline for x86_64
        ("__SSE__", Some("1")),
        ("__SSE2__", Some("1")),
        ("__SSE_MATH__", Some("1")),
        ("__SSE2_MATH__", Some("1")),
        // MMX is implied by SSE
        ("__MMX__", Some("1")),
        // FPU type
        ("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_1", Some("1")),
        ("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_2", Some("1")),
        ("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_4", Some("1")),
        ("__GCC_HAVE_SYNC_COMPARE_AND_SWAP_8", Some("1")),
        // Atomic primitives
        ("__GCC_ATOMIC_BOOL_LOCK_FREE", Some("2")),
        ("__GCC_ATOMIC_CHAR_LOCK_FREE", Some("2")),
        ("__GCC_ATOMIC_SHORT_LOCK_FREE", Some("2")),
        ("__GCC_ATOMIC_INT_LOCK_FREE", Some("2")),
        ("__GCC_ATOMIC_LONG_LOCK_FREE", Some("2")),
        ("__GCC_ATOMIC_LLONG_LOCK_FREE", Some("2")),
        ("__GCC_ATOMIC_POINTER_LOCK_FREE", Some("2")),
    ]
}
