//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `format(archetype, n, m)` and `format_arg(n)`: c17 checks no format
// strings, but the attributes' arguments are checked against the function
// as gcc 13 checks them -- the archetype, the parameter numbers, the types
// of the format parameter and of what `format_arg` returns -- in its words.
// gcc reports at the declaration's first column; c17 at the attribute.
//

use crate::test_compile::compile;

/// The diagnostics of `src`, each without its file and position.
fn messages(name: &str, src: &str) -> Vec<String> {
    compile(name, src, &[])
        .stderr
        .lines()
        .filter(|l| !l.ends_with("compilation failed"))
        .map(|l| {
            let at = l
                .find(": warning: ")
                .or_else(|| l.find(": error: "))
                .or_else(|| l.find(": note: "));
            l[at.map_or(0, |i| i + 2)..].to_string()
        })
        .collect()
}

#[test]
fn format_arg_arguments_as_gcc_checks_them() {
    let w = "[-Wattributes]";
    for (decl, said) in [
        (
            "const char *fa(const char *) __attribute__((format_arg(2)));",
            vec![format!("warning: 'format_arg' attribute argument value '2' exceeds the number of function parameters 1 {w}")],
        ),
        (
            "int fa(const char *) __attribute__((format_arg(1)));",
            vec!["error: function does not return string type".to_string()],
        ),
        (
            "void *fa(const char *) __attribute__((format_arg(1)));",
            vec!["error: function does not return string type".to_string()],
        ),
        (
            "const char *fa(int) __attribute__((format_arg(1)));",
            vec!["error: 'format_arg' attribute argument value '1' refers to parameter type 'int'".to_string()],
        ),
        (
            "const unsigned char *fa(const unsigned char *) __attribute__((format_arg(1)));",
            vec!["error: 'format_arg' attribute argument value '1' refers to parameter type 'const unsigned char *'".to_string()],
        ),
        (
            "const char *fa(const char *) __attribute__((format_arg(0)));",
            vec![format!("warning: 'format_arg' attribute argument value '0' does not refer to a function parameter {w}")],
        ),
        (
            "const char *fa(const char *) __attribute__((format_arg(-1)));",
            vec![format!("warning: 'format_arg' attribute argument value '-1' exceeds the number of function parameters 1 {w}")],
        ),
        (
            "int v; const char *fa(const char *) __attribute__((format_arg(v)));",
            vec![format!("warning: 'format_arg' attribute argument value 'v' is not an integer constant {w}")],
        ),
        (
            "const char *fa(const char *) __attribute__((format_arg(\"1\")));",
            vec![format!("warning: 'format_arg' attribute argument has type 'char[2]' {w}")],
        ),
        (
            "const char *fa(const char *) __attribute__((format_arg(1.0)));",
            vec![format!("warning: 'format_arg' attribute argument has type 'double' {w}")],
        ),
        (
            "const char *fa(const char *) __attribute__((format_arg(1, 2)));",
            vec![
                "error: wrong number of arguments specified for 'format_arg' attribute".to_string(),
                "note: expected 1, found 2".to_string(),
            ],
        ),
        (
            "const char *fa(const char *) __attribute__((format_arg));",
            vec![
                "error: wrong number of arguments specified for 'format_arg' attribute".to_string(),
                "note: expected 1, found 0".to_string(),
            ],
        ),
        // What gcc takes: a `char *`, `const`-qualified or not, a variadic
        // function, and no prototype at all.
        ("const char *fa(const char *) __attribute__((format_arg(1)));", vec![]),
        ("char *fa(char *) __attribute__((format_arg(1)));", vec![]),
        ("const char *fa(char *const) __attribute__((format_arg(1)));", vec![]),
        ("const char *fa() __attribute__((format_arg(1)));", vec![]),
    ] {
        assert_eq!(messages("format_arg_args", &format!("{decl}\n")), said, "{decl}");
    }
}

#[test]
fn format_arguments_as_gcc_checks_them() {
    let w = "[-Wattributes]";
    for (decl, said) in [
        (
            "void fp(const char *, ...) __attribute__((format(printf, 2, 3)));",
            vec![format!("warning: 'format' attribute argument 2 value '2' exceeds the number of function parameters 1 {w}")],
        ),
        (
            "void fp(const char *, ...) __attribute__((format(printf, 1, 3)));",
            vec!["error: argument to be formatted is not '...'".to_string()],
        ),
        (
            "void fp(int, ...) __attribute__((format(printf, 1, 2)));",
            vec!["error: 'format' attribute argument 2 value '1' refers to parameter type 'int'".to_string()],
        ),
        (
            "void fp(signed char *, ...) __attribute__((format(printf, 1, 2)));",
            vec!["error: 'format' attribute argument 2 value '1' refers to parameter type 'signed char *'".to_string()],
        ),
        (
            "void fp(const char *, ...) __attribute__((format(bogus, 1, 2)));",
            vec!["warning: 'bogus' is an unrecognized format function type [-Wformat=]".to_string()],
        ),
        (
            "void fp(const char *, ...) __attribute__((format(NSString, 1, 2)));",
            vec!["warning: 'NSString' is only allowed in Objective-C dialects [-Wformat=]".to_string()],
        ),
        (
            "void fp(const char *, ...) __attribute__((format(\"printf\", 1, 2)));",
            vec!["error: unrecognized format specifier".to_string()],
        ),
        (
            "void fp(const char *, ...) __attribute__((format(printf, 1, 1)));",
            vec!["error: 'format' attribute argument 3 value '1' does not refer to a variable argument list".to_string()],
        ),
        (
            "void fp(const char *, int) __attribute__((format(printf, 1, 2)));",
            vec!["error: 'format' attribute argument 3 value '2' does not refer to a variable argument list".to_string()],
        ),
        (
            "void fp(const char *, ...) __attribute__((format(printf, 0, 2)));",
            vec![format!("warning: 'format' attribute argument 2 value '0' does not refer to a function parameter {w}")],
        ),
        (
            "void fp(const char *, ...) __attribute__((format(printf, 1, -1)));",
            vec![format!("warning: 'format' attribute argument 3 value '-1' exceeds the number of function parameters 1 {w}")],
        ),
        (
            "int v; void fp(const char *, ...) __attribute__((format(printf, 1, v)));",
            vec![format!("warning: 'format' attribute argument 3 value 'v' is not an integer constant {w}")],
        ),
        (
            "void fp(const char *, ...) __attribute__((format(printf, \"1\", 2)));",
            vec![format!("warning: 'format' attribute argument 2 has type 'char[2]' {w}")],
        ),
        (
            "void fp(const char *, ...) __attribute__((format(strftime, 1, 2)));",
            vec!["error: strftime formats cannot format arguments".to_string()],
        ),
        (
            "void fp(const char *, ...) __attribute__((format(printf, 1)));",
            vec![
                "error: wrong number of arguments specified for 'format' attribute".to_string(),
                "note: expected 3, found 2".to_string(),
            ],
        ),
        (
            "void fp(const char *, ...) __attribute__((format(printf, 1, 2), format_arg(1)));",
            vec!["error: function does not return string type".to_string()],
        ),
        // On a pointer to a function, and a typedef of one, as well.
        (
            "void (*pf)(const char *, ...) __attribute__((format(printf, 1, 3)));",
            vec!["error: argument to be formatted is not '...'".to_string()],
        ),
        (
            "typedef void T(const char *, ...) __attribute__((format(printf, 1, 3)));",
            vec!["error: argument to be formatted is not '...'".to_string()],
        ),
        // What gcc takes.
        ("void fp(const char *, ...) __attribute__((format(printf, 1, 2)));", vec![]),
        ("void fp(const char *, int) __attribute__((format(printf, 1, 0)));", vec![]),
        ("void fp(int, const char *, ...) __attribute__((format(printf, 2, 3)));", vec![]),
        ("void fp(char *, ...) __attribute__((format(__printf__, 1, 2)));", vec![]),
        ("void fp(const char *, ...) __attribute__((format(gnu_scanf, 1, 2)));", vec![]),
        ("void fp(const char *) __attribute__((format(strftime, 1, 0)));", vec![]),
        ("void fp() __attribute__((format(printf, 1, 2)));", vec![]),
    ] {
        assert_eq!(messages("format_args", &format!("{decl}\n")), said, "{decl}");
    }
}
