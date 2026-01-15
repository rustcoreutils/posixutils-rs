//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Builtin headers for pcc
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

/// Builtin stdatomic.h - C11 atomic operations
pub const STDATOMIC_H: &str = include_str!("include/stdatomic.h");

/// Look up a builtin header by name
///
/// Returns the header content if found, None otherwise.
/// The name should be the basename without path (e.g., "stdarg.h").
pub fn get_builtin_header(name: &str) -> Option<&'static str> {
    match name {
        "stdarg.h" => Some(STDARG_H),
        "stdbool.h" => Some(STDBOOL_H),
        "stddef.h" => Some(STDDEF_H),
        "limits.h" => Some(LIMITS_H),
        "stdatomic.h" => Some(STDATOMIC_H),
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
    fn test_unknown_header() {
        assert!(get_builtin_header("unknown.h").is_none());
    }
}
