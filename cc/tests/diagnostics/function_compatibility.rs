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

use crate::common::{
    compile_and_run, compile_expect_error, compile_expect_no_diagnostic, compile_expect_warning,
};

/// Redeclaring a function without a prototype as one whose arguments would
/// be promoted, or that takes an ellipsis, is a conflict; any other
/// prototype is the composite type.
#[test]
fn function_redeclaration_against_no_prototype() {
    let conflicts = [
        ("redecl_char", "int f(); int f(char);\n"),
        ("redecl_float", "int f(); int f(float);\n"),
        ("redecl_variadic", "int f(); int f(int, ...);\n"),
        ("redecl_char_after", "int f(char); int f();\n"),
    ];
    for (name, src) in conflicts {
        compile_expect_error(name, src, "conflicting types for 'f'");
    }
    let compatible = [
        ("redecl_void", "int f(); int f(void);\n"),
        (
            "redecl_int_double",
            "int f(); int f(int, double, char *);\n",
        ),
        ("redecl_after", "int f(int); int f();\n"),
    ];
    for (name, src) in compatible {
        compile_expect_no_diagnostic(name, src, "conflicting");
    }
}

/// A definition with an identifier list after a prototype is held to its
/// return type alone, as gcc holds it outside `-pedantic`.
#[test]
fn function_identifier_list_definition_after_prototype() {
    compile_expect_no_diagnostic(
        "kr_after_char_proto",
        "int f(char); int f(c) char c; { return c; }\n",
        "conflicting",
    );
    compile_expect_no_diagnostic(
        "kr_after_no_proto",
        "int f(); int f(c) char c; { return c; }\n",
        "conflicting",
    );
    compile_expect_error(
        "kr_after_other_return",
        "long f(int); int f(c) int c; { return c; }\n",
        "conflicting types for 'f'",
    );
}

/// Assigning between the two pointer types follows the same rule.
#[test]
fn function_pointer_assignment_against_no_prototype() {
    let decls = "int (*fp)(void); int (*fnp)(); int (*fq)(int, double); int (*fc)(char);\n";
    compile_expect_no_diagnostic(
        "fnptr_compatible",
        &format!("{decls}void f(void) {{ fp = fnp; fnp = fp; fq = fnp; fnp = fq; }}\n"),
        "warning",
    );
    compile_expect_warning(
        "fnptr_promoted",
        &format!("{decls}void f(void) {{ fc = fnp; }}\n"),
        "incompatible pointer type",
    );
}

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
