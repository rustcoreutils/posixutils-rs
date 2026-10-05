//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A function type with a prototype is compatible with one of the same
// return type without a prototype exactly when a call through either passes
// the same arguments (C17 6.2.7p3): no ellipsis, and no parameter of a type
// the default argument promotions change. The rule is one predicate, so
// assignment, the conditional operator, `__builtin_types_compatible_p` and a
// redeclaration all give gcc's answer.
//

use crate::common::compile_and_run;

/// `__builtin_types_compatible_p` asks the same question.
#[test]
fn function_types_compatible_builtin_against_no_prototype() {
    let code = r#"
int main(void) {
    if (!__builtin_types_compatible_p(int (*)(), int (*)(void))) return 1;
    if (!__builtin_types_compatible_p(int (*)(), int (*)(int, double))) return 2;
    if (__builtin_types_compatible_p(int (*)(), int (*)(char))) return 3;
    if (__builtin_types_compatible_p(int (*)(), int (*)(float))) return 4;
    if (__builtin_types_compatible_p(int (*)(), int (*)(int, ...))) return 5;
    if (__builtin_types_compatible_p(long (*)(), int (*)(void))) return 6;
    return 0;
}
"#;
    assert_eq!(compile_and_run("fn_compat_builtin", code, &[]), 0);
}
