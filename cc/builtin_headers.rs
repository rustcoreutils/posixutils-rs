//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Builtin headers for c17
//
// These headers are embedded directly in the compiler binary and are
// searched before system headers. They provide compiler-specific
// implementations of standard headers like <stdarg.h> and <stddef.h>.
//

use crate::target::Arch;

/// Builtin stdarg.h - variadic function support
pub const STDARG_H: &str = include_str!("include/stdarg.h");

/// Builtin stddef.h - standard type definitions
pub const STDDEF_H: &str = include_str!("include/stddef.h");

/// Builtin stdbool.h - C99 boolean type support
pub const STDBOOL_H: &str = include_str!("include/stdbool.h");

/// Builtin limits.h - implementation limits
pub const LIMITS_H: &str = include_str!("include/limits.h");

/// Builtin stdalign.h - C11 alignment specifiers
pub const STDALIGN_H: &str = include_str!("include/stdalign.h");

/// Builtin stdatomic.h - C11 atomic operations
pub const STDATOMIC_H: &str = include_str!("include/stdatomic.h");

/// Builtin stdnoreturn.h - C11 _Noreturn convenience macro
pub const STDNORETURN_H: &str = include_str!("include/stdnoreturn.h");

/// Builtin complex.h - C99 complex arithmetic, for a C library without one
/// c17 can use; a hosted glibc build defers to glibc's (see its preamble).
pub const COMPLEX_H: &str = include_str!("include/complex.h");

/// Builtin iso646.h - C95/C99 alternative operator spellings
pub const ISO646_H: &str = include_str!("include/iso646.h");

/// Builtin float.h - floating-point characteristics
pub const FLOAT_H: &str = include_str!("include/float.h");

/// Builtin stdint.h - C17 7.20 exact/minimum/fastest-width integer types.
/// Part of the freestanding header set (C17 4p6), so we must supply it.
pub const STDINT_H: &str = include_str!("include/stdint.h");

/// Builtin cpuid.h - CPU feature detection (x86/x64)
pub const CPUID_H: &str = include_str!("include/cpuid.h");

/// Builtin tgmath.h - C11 7.25 type-generic math macros.
///
/// Registered rather than left to the host, unlike xmmintrin.h below, because
/// this body is genuinely better here: every glibc <tgmath.h> needs compiler
/// internals c17 lacks (__builtin_tgmath, or __builtin_classify_type plus
/// __real__) and #errors out before reaching any of them unless
/// __HAVE_FLOAT128 agrees with __HAVE_FLOAT64X. A user's own tgmath.h still
/// wins: builtin headers are searched after the includer's directory and -I.
pub const TGMATH_H: &str = include_str!("include/tgmath.h");

/// The x86-64 intrinsic headers, SSE through SSE4.2 (`<immintrin.h>` and
/// `<x86intrin.h>` gather them), and `<mm_malloc.h>`. Written in C over GNU
/// vectors; see each header's preamble.
const X86_INTRIN_HEADERS: &[(&str, &str)] = &[
    ("mmintrin.h", include_str!("include/mmintrin.h")),
    ("xmmintrin.h", include_str!("include/xmmintrin.h")),
    ("emmintrin.h", include_str!("include/emmintrin.h")),
    ("pmmintrin.h", include_str!("include/pmmintrin.h")),
    ("tmmintrin.h", include_str!("include/tmmintrin.h")),
    ("smmintrin.h", include_str!("include/smmintrin.h")),
    ("nmmintrin.h", include_str!("include/nmmintrin.h")),
    ("popcntintrin.h", include_str!("include/popcntintrin.h")),
    ("immintrin.h", include_str!("include/immintrin.h")),
    ("x86intrin.h", include_str!("include/x86intrin.h")),
    ("mm_malloc.h", include_str!("include/mm_malloc.h")),
];

/// The AArch64 Advanced SIMD intrinsics, a core subset of the ACLE's.
const ARM_NEON_H: &str = include_str!("include/arm_neon.h");

/// Look up a builtin header by name for a target of architecture `arch`.
///
/// Returns the header content if found, None otherwise.
/// The name should be the basename without path (e.g., "stdarg.h").
/// The intrinsic headers belong to their architecture: on another, the name
/// is looked for as any other is, and found nowhere, as with gcc.
pub fn get_builtin_header(name: &str, arch: Arch) -> Option<&'static str> {
    let intrinsics = match arch {
        Arch::X86_64 => X86_INTRIN_HEADERS
            .iter()
            .find(|(n, _)| *n == name)
            .map(|(_, body)| *body),
        Arch::Aarch64 => (name == "arm_neon.h").then_some(ARM_NEON_H),
    };
    if intrinsics.is_some() {
        return intrinsics;
    }
    match name {
        "stdarg.h" => Some(STDARG_H),
        "complex.h" => Some(COMPLEX_H),
        "iso646.h" => Some(ISO646_H),
        "stdbool.h" => Some(STDBOOL_H),
        "stddef.h" => Some(STDDEF_H),
        "limits.h" => Some(LIMITS_H),
        "stdalign.h" => Some(STDALIGN_H),
        "stdatomic.h" => Some(STDATOMIC_H),
        "stdnoreturn.h" => Some(STDNORETURN_H),
        "float.h" => Some(FLOAT_H),
        "stdint.h" => Some(STDINT_H),
        "cpuid.h" => Some(CPUID_H),
        "tgmath.h" => Some(TGMATH_H),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn get_builtin_header_host(name: &str) -> Option<&'static str> {
        get_builtin_header(name, Arch::X86_64)
    }

    /// The intrinsic headers exist for their own architecture only.
    #[test]
    fn test_intrinsic_headers_follow_the_architecture() {
        assert!(get_builtin_header("emmintrin.h", Arch::X86_64).is_some());
        assert!(get_builtin_header("immintrin.h", Arch::X86_64).is_some());
        assert!(get_builtin_header("emmintrin.h", Arch::Aarch64).is_none());
        assert!(get_builtin_header("arm_neon.h", Arch::Aarch64).is_some());
        assert!(get_builtin_header("arm_neon.h", Arch::X86_64).is_none());
    }

    #[test]
    fn test_stdarg_exists() {
        let header = get_builtin_header_host("stdarg.h");
        assert!(header.is_some());
        assert!(header.unwrap().contains("va_list"));
        assert!(header.unwrap().contains("va_start"));
    }

    #[test]
    fn test_stddef_exists() {
        let header = get_builtin_header_host("stddef.h");
        assert!(header.is_some());
        assert!(header.unwrap().contains("size_t"));
        assert!(header.unwrap().contains("NULL"));
    }

    #[test]
    fn test_stdbool_exists() {
        let header = get_builtin_header_host("stdbool.h");
        assert!(header.is_some());
        assert!(header.unwrap().contains("bool"));
        assert!(header.unwrap().contains("true"));
        assert!(header.unwrap().contains("false"));
    }

    #[test]
    fn test_stdatomic_exists() {
        let header = get_builtin_header_host("stdatomic.h");
        assert!(header.is_some());
        assert!(header.unwrap().contains("atomic_int"));
        assert!(header.unwrap().contains("atomic_load"));
        assert!(header.unwrap().contains("memory_order"));
    }

    #[test]
    fn test_float_exists() {
        let header = get_builtin_header_host("float.h");
        assert!(header.is_some());
        assert!(header.unwrap().contains("FLT_MAX"));
        assert!(header.unwrap().contains("DBL_MAX"));
        assert!(header.unwrap().contains("LDBL_MAX"));
    }

    #[test]
    fn test_unknown_header() {
        assert!(get_builtin_header_host("unknown.h").is_none());
    }
}
