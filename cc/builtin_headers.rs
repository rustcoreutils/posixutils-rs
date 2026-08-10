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

/// Builtin complex.h - C99 complex arithmetic
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

/// Look up a builtin header by name
///
/// Returns the header content if found, None otherwise.
/// The name should be the basename without path (e.g., "stdarg.h").
pub fn get_builtin_header(name: &str) -> Option<&'static str> {
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
        // xmmintrin.h and emmintrin.h are deliberately NOT registered. Their
        // bundled bodies are a bare #error, and registering them meant a user
        // with a real SSE intrinsics header on the include path got our hard
        // failure instead of theirs. Unregistered, the normal search finds it.
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stdarg_exists() {
        let header = get_builtin_header("stdarg.h");
        assert!(header.is_some());
        assert!(header.unwrap().contains("va_list"));
        assert!(header.unwrap().contains("va_start"));
    }

    #[test]
    fn test_stddef_exists() {
        let header = get_builtin_header("stddef.h");
        assert!(header.is_some());
        assert!(header.unwrap().contains("size_t"));
        assert!(header.unwrap().contains("NULL"));
    }

    #[test]
    fn test_stdbool_exists() {
        let header = get_builtin_header("stdbool.h");
        assert!(header.is_some());
        assert!(header.unwrap().contains("bool"));
        assert!(header.unwrap().contains("true"));
        assert!(header.unwrap().contains("false"));
    }

    #[test]
    fn test_stdatomic_exists() {
        let header = get_builtin_header("stdatomic.h");
        assert!(header.is_some());
        assert!(header.unwrap().contains("atomic_int"));
        assert!(header.unwrap().contains("atomic_load"));
        assert!(header.unwrap().contains("memory_order"));
    }

    #[test]
    fn test_float_exists() {
        let header = get_builtin_header("float.h");
        assert!(header.is_some());
        assert!(header.unwrap().contains("FLT_MAX"));
        assert!(header.unwrap().contains("DBL_MAX"));
        assert!(header.unwrap().contains("LDBL_MAX"));
    }

    #[test]
    fn test_unknown_header() {
        assert!(get_builtin_header("unknown.h").is_none());
    }
}
