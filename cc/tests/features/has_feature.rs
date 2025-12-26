//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for __has_builtin, __has_attribute, __has_feature, __has_extension
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
//

use crate::common::compile_and_run;

// ============================================================================
// __has_builtin: Supported and unsupported builtins
// ============================================================================

#[test]
fn has_builtin_tests() {
    let code = r#"
int main(void) {
    // Test 1-16: Supported builtins return 1
    #if !__has_builtin(__builtin_constant_p)
    return 1;
    #endif

    #if !__has_builtin(__builtin_types_compatible_p)
    return 2;
    #endif

    #if !__has_builtin(__builtin_bswap16)
    return 3;
    #endif

    #if !__has_builtin(__builtin_bswap32)
    return 4;
    #endif

    #if !__has_builtin(__builtin_bswap64)
    return 5;
    #endif

    #if !__has_builtin(__builtin_alloca)
    return 6;
    #endif

    #if !__has_builtin(__builtin_ctz)
    return 7;
    #endif

    #if !__has_builtin(__builtin_ctzl)
    return 8;
    #endif

    #if !__has_builtin(__builtin_ctzll)
    return 9;
    #endif

    #if !__has_builtin(__builtin_unreachable)
    return 10;
    #endif

    #if !__has_builtin(__builtin_clz)
    return 11;
    #endif

    #if !__has_builtin(__builtin_clzl)
    return 12;
    #endif

    #if !__has_builtin(__builtin_clzll)
    return 13;
    #endif

    #if !__has_builtin(__builtin_popcount)
    return 14;
    #endif

    #if !__has_builtin(__builtin_popcountl)
    return 15;
    #endif

    #if !__has_builtin(__builtin_popcountll)
    return 16;
    #endif

    #if !__has_builtin(__builtin_offsetof)
    return 17;
    #endif

    #if !__has_builtin(offsetof)
    return 18;
    #endif

    // Test 19-20: Unsupported/unknown builtins return 0
    #if __has_builtin(__builtin_nonexistent_thing)
    return 19;
    #endif

    #if __has_builtin(__builtin_xyz_unknown)
    return 20;
    #endif

    // Test 21-23: Complex expressions
    #if __has_builtin(__builtin_constant_p) && __has_builtin(__builtin_bswap32)
    int ok1 = 1;
    #else
    return 21;
    #endif

    #if __has_builtin(__builtin_nonexistent) || __has_builtin(__builtin_alloca)
    int ok2 = 1;
    #else
    return 22;
    #endif

    #if !__has_builtin(__builtin_nonexistent)
    int ok3 = 1;
    #else
    return 23;
    #endif

    return ok1 + ok2 + ok3 - 3;
}
"#;
    assert_eq!(compile_and_run("has_builtin", code), 0);
}

// ============================================================================
// __has_attribute, __has_feature, __has_extension
// ============================================================================

#[test]
fn has_attribute_and_feature() {
    let code = r#"
int main(void) {
    // Test 1-2: __has_attribute for supported
    #if !__has_attribute(noreturn)
    return 1;
    #endif

    #if !__has_attribute(__noreturn__)
    return 2;
    #endif

    // Test 3-4: __has_attribute for unsupported
    #if __has_attribute(unused)
    return 3;
    #endif

    #if __has_attribute(packed)
    return 4;
    #endif

    // Test 5: __has_feature(statement_expressions) returns 1
    #if !__has_feature(statement_expressions)
    return 5;
    #endif

    // Test 6: __has_extension(statement_expressions) returns 1
    #if !__has_extension(statement_expressions)
    return 6;
    #endif

    // Test 7-9: __has_feature returns 0 for C11 features (not yet implemented)
    #if __has_feature(c_alignas)
    return 7;
    #endif

    #if __has_feature(c_static_assert)
    return 8;
    #endif

    #if __has_feature(c_generic_selections)
    return 9;
    #endif

    // Test 10-11: __has_extension returns 0 for other extensions (not yet implemented)
    #if __has_extension(c_alignas)
    return 10;
    #endif

    #if __has_extension(attribute_deprecated_with_message)
    return 11;
    #endif

    return 0;
}
"#;
    assert_eq!(compile_and_run("has_attr_feature", code), 0);
}
