//
// Copyright (c) 2025-2026 Jeff Garzik
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
        // Byte order is in `arch::get_misc_macros`, for every target.
        // Register size
        ("__REGISTER_PREFIX__", Some("")),
        // Long double is 80-bit extended precision (padded to 128 bits)
        // `long double` is described by `get_float_limit_macros` and
        // `get_additional_sizeof_macros`; see the note in the aarch64 list.
        // `__SSE__`, `__SSE2__` and their `_MATH__` macros, `__MMX__` and
        // `__FXSR__` are the ISA's, which `-mno-sse`, `-mno-mmx` and the
        // rest can turn off: `token::preprocess::define_isa_macros`.
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
        // 128-bit integer support
        ("__SIZEOF_INT128__", Some("16")),
    ]
}
