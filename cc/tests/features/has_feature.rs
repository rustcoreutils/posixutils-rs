//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for __has_builtin, __has_attribute, __has_feature, __has_extension
//

use crate::common::compile_and_run;

/// Test __has_builtin for supported builtins
#[test]
fn has_builtin_supported() {
    let code = r#"
int main(void) {
    // Test supported builtins return 1
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

    return 0;
}
"#;
    assert_eq!(compile_and_run("has_builtin_supported", code), 0);
}

/// Test __has_builtin for unsupported builtins
#[test]
fn has_builtin_unsupported() {
    let code = r#"
int main(void) {
    // Test unsupported/unknown builtins return 0
    #if __has_builtin(__builtin_nonexistent_thing)
    return 1;
    #endif

    #if __has_builtin(__builtin_xyz_unknown)
    return 2;
    #endif

    return 0;
}
"#;
    assert_eq!(compile_and_run("has_builtin_unsupported", code), 0);
}

/// Test __has_attribute always returns 0 (not yet implemented)
#[test]
fn has_attribute_returns_zero() {
    let code = r#"
int main(void) {
    // __has_attribute currently returns 0 for all attributes
    #if __has_attribute(unused)
    return 1;
    #endif

    #if __has_attribute(noreturn)
    return 2;
    #endif

    #if __has_attribute(packed)
    return 3;
    #endif

    return 0;
}
"#;
    assert_eq!(compile_and_run("has_attribute_zero", code), 0);
}

/// Test __has_feature always returns 0 (not yet implemented)
#[test]
fn has_feature_returns_zero() {
    let code = r#"
int main(void) {
    // __has_feature currently returns 0 for all features
    #if __has_feature(c_alignas)
    return 1;
    #endif

    #if __has_feature(c_static_assert)
    return 2;
    #endif

    #if __has_feature(c_generic_selections)
    return 3;
    #endif

    return 0;
}
"#;
    assert_eq!(compile_and_run("has_feature_zero", code), 0);
}

/// Test __has_extension always returns 0 (not yet implemented)
#[test]
fn has_extension_returns_zero() {
    let code = r#"
int main(void) {
    // __has_extension currently returns 0 for all extensions
    #if __has_extension(c_alignas)
    return 1;
    #endif

    #if __has_extension(attribute_deprecated_with_message)
    return 2;
    #endif

    return 0;
}
"#;
    assert_eq!(compile_and_run("has_extension_zero", code), 0);
}

/// Test __has_builtin in complex preprocessor expressions
#[test]
fn has_builtin_complex_expressions() {
    let code = r#"
int main(void) {
    // Test in logical AND
    #if __has_builtin(__builtin_constant_p) && __has_builtin(__builtin_bswap32)
    int ok1 = 1;
    #else
    return 1;
    #endif

    // Test in logical OR
    #if __has_builtin(__builtin_nonexistent) || __has_builtin(__builtin_alloca)
    int ok2 = 1;
    #else
    return 2;
    #endif

    // Test negation
    #if !__has_builtin(__builtin_nonexistent)
    int ok3 = 1;
    #else
    return 3;
    #endif

    // Use the variables to avoid warnings
    return ok1 + ok2 + ok3 - 3;
}
"#;
    assert_eq!(compile_and_run("has_builtin_complex", code), 0);
}
