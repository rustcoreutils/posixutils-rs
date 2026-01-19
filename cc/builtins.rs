//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Builtin function registry - single source of truth for supported builtins
//
// This module provides the authoritative list of compiler builtins.
// Both the preprocessor (__has_builtin) and parser use this list.
//

/// All supported builtin function names.
/// This is the single source of truth - add new builtins here.
pub const SUPPORTED_BUILTINS: &[&str] = &[
    // Variadic function support
    "__builtin_va_list",
    "__builtin_va_start",
    "__builtin_va_end",
    "__builtin_va_arg",
    "__builtin_va_copy",
    // Byte swap
    "__builtin_bswap16",
    "__builtin_bswap32",
    "__builtin_bswap64",
    // Bit manipulation - count trailing zeros
    "__builtin_ctz",
    "__builtin_ctzl",
    "__builtin_ctzll",
    // Bit manipulation - count leading zeros
    "__builtin_clz",
    "__builtin_clzl",
    "__builtin_clzll",
    // Bit manipulation - population count
    "__builtin_popcount",
    "__builtin_popcountl",
    "__builtin_popcountll",
    // Memory operations
    "__builtin_alloca",
    "__builtin_memset",
    "__builtin_memcpy",
    "__builtin_memmove",
    // Compile-time evaluation
    "__builtin_constant_p",
    "__builtin_types_compatible_p",
    "__builtin_unreachable",
    "__builtin_offsetof",
    "offsetof",
    // Floating-point infinity constants
    "__builtin_inf",
    "__builtin_inff",
    "__builtin_infl",
    "__builtin_huge_val",
    "__builtin_huge_valf",
    "__builtin_huge_vall",
    // Floating-point math
    "__builtin_fabs",
    "__builtin_fabsf",
    "__builtin_fabsl",
    // Floating-point sign bit testing
    "__builtin_signbit",
    "__builtin_signbitf",
    "__builtin_signbitl",
    // NaN constants
    "__builtin_nan",
    "__builtin_nanf",
    "__builtin_nanl",
    "__builtin_nans",
    "__builtin_nansf",
    "__builtin_nansl",
    // Branch prediction
    "__builtin_expect",
    // Pointer alignment hints
    "__builtin_assume_aligned",
    // Cache/memory prefetch
    "__builtin_prefetch",
    // Floating-point rounding mode
    "__builtin_flt_rounds",
    // Frame/return address introspection
    "__builtin_frame_address",
    "__builtin_return_address",
    // C11 atomic builtins (Clang-style)
    "__c11_atomic_init",
    "__c11_atomic_load",
    "__c11_atomic_store",
    "__c11_atomic_exchange",
    "__c11_atomic_compare_exchange_strong",
    "__c11_atomic_compare_exchange_weak",
    "__c11_atomic_fetch_add",
    "__c11_atomic_fetch_sub",
    "__c11_atomic_fetch_and",
    "__c11_atomic_fetch_or",
    "__c11_atomic_fetch_xor",
    "__c11_atomic_thread_fence",
    "__c11_atomic_signal_fence",
];

/// Check if a name is a supported builtin function.
/// Used by __has_builtin() in the preprocessor.
#[inline]
pub fn is_builtin(name: &str) -> bool {
    SUPPORTED_BUILTINS.contains(&name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_builtin() {
        assert!(is_builtin("__builtin_va_start"));
        assert!(is_builtin("__builtin_memcpy"));
        assert!(is_builtin("__c11_atomic_load"));
        assert!(!is_builtin("__builtin_nonexistent"));
        assert!(!is_builtin("printf"));
    }
}
