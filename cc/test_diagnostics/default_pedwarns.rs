//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's two kinds of pedwarn. The default kind is a warning that
// `-pedantic-errors` makes an error; the `-Wpedantic` kind is silent until
// `-pedantic` asks for it. c17 gives each construct the verdict gcc gives it,
// in gcc's words, and an error only where gcc 14 has one.
//

use crate::test_compile::{compile, compile_accepted};

/// Constraint violations gcc 13 and 14 only warn about by default: accepted
/// with gcc's warning, refused under `-pedantic-errors`.
const DEFAULT_PEDWARNS: &[(&str, &str, &str)] = &[
    (
        "struct_member_missing_semicolon",
        "struct S { int a; int b };\nint main(void){ struct S s = {1, 2}; return s.b - 2; }\n",
        "no semicolon at end of struct or union",
    ),
    (
        "octal_escape_out_of_range",
        "int c = '\\400';\n",
        "octal escape sequence out of range",
    ),
    (
        "integer_constant_too_large",
        "unsigned long long x = 123456789012345678901234567890;\n",
        "integer constant is too large for its type",
    ),
    (
        "integer_constant_too_large_in_if",
        "#if 0 && 123456789012345678901234567890\n#endif\nint x;\n",
        "integer constant is too large for its type",
    ),
    (
        "line_number_wraps",
        "#line 4294967297\nint x;\n",
        "line number out of range",
    ),
    (
        "useless_type_name",
        "int;\n",
        "useless type name in empty declaration",
    ),
    (
        "missing_terminating_quote_in_skipped_block",
        "#if 0\nchar c = 'a\n#endif\nint x;\n",
        "missing terminating ' character",
    ),
    (
        "shift_overflow_in_if",
        "#if 1 << 70\n#endif\nint x;\n",
        "integer overflow in preprocessor expression",
    ),
    (
        "va_opt_in_non_variadic_macro",
        "#define F(a) __VA_OPT__(a)\nint x;\n",
        "__VA_OPT__ is only meaningful in a variadic macro",
    ),
    (
        "va_args_in_non_variadic_macro",
        "#define F(a) __VA_ARGS__\nint x;\n",
        "__VA_ARGS__ can only appear in the expansion of a C99 variadic macro",
    ),
    (
        "extra_tokens_after_endif",
        "#ifdef X\n#endif X\nint x;\n",
        "extra tokens at end of #endif directive",
    ),
    (
        "extra_tokens_after_undef",
        "#undef X Y\nint x;\n",
        "extra tokens at end of #undef directive",
    ),
    (
        "extra_tokens_after_include",
        "#include \"stdbool.h\" junk\nint x;\n",
        "extra tokens at end of #include directive",
    ),
    (
        "extra_tokens_after_line",
        "#line 10 \"a.c\" junk\nint x;\n",
        "extra tokens at end of #line directive",
    ),
    (
        "no_whitespace_after_macro_name",
        "#define X+1\nint x;\n",
        "ISO C99 requires whitespace after the macro name",
    ),
    (
        "macro_redefined",
        "#define X 1\n#define X 2\nint x;\n",
        "'X' redefined: the replacement lists differ",
    ),
    (
        "member_declares_nothing",
        "struct S { int a; struct T { int b; }; };\n",
        "declaration does not declare anything",
    ),
    (
        "excess_scalar_initializer",
        "int x = {1, 2};\n",
        "excess elements in scalar initializer",
    ),
    (
        "excess_array_initializer",
        "int a[2] = {1, 2, 3};\n",
        "excess elements in array initializer",
    ),
    (
        "excess_struct_initializer",
        "struct S { int a; } s = {1, 2};\n",
        "excess elements in struct initializer",
    ),
    (
        "excess_union_initializer",
        "union U { int a; } u = {1, 2};\n",
        "excess elements in union initializer",
    ),
    (
        "initializer_string_too_long",
        "char a[2] = \"abc\";\n",
        "initializer-string for array of 'char' is too long",
    ),
    (
        "parameter_declared_inline",
        "void f(inline int a);\n",
        "parameter 'a' declared 'inline'",
    ),
    (
        "unnamed_parameter_declared_noreturn",
        "void f(_Noreturn int);\n",
        "unnamed parameter declared '_Noreturn'",
    ),
    (
        "conditional_pointer_integer_mismatch",
        "int f(int c, int *p) { return *(c ? p : 1); }\n",
        "pointer/integer type mismatch in conditional expression",
    ),
    (
        "conditional_pointer_mismatch",
        "void *f(int c, int *p, double *q) { return c ? p : q; }\n",
        "pointer type mismatch in conditional expression",
    ),
    (
        "comparison_pointer_integer",
        "int f(int *p, int i) { return p == i; }\n",
        "comparison between pointer and integer",
    ),
    (
        "comparison_distinct_pointers",
        "int f(int *p, double *q) { return p == q; }\n",
        "comparison of distinct pointer types lacks a cast",
    ),
    (
        "func_outside_function",
        "const char *s = __func__;\n",
        "'__func__' is not defined outside of function scope",
    ),
    (
        "constant_so_large_it_is_unsigned",
        "long long x = 18446744073709551615;\n",
        "integer constant is so large that it is unsigned",
    ),
    (
        "old_style_parameter_defaults_to_int",
        "int f(a) { return a; }\n",
        "type of 'a' defaults to 'int'",
    ),
    (
        "inline_main",
        "inline int main(void) { return 0; }\n",
        "cannot inline function 'main'",
    ),
    (
        "variable_declared_inline",
        "inline int x;\n",
        "variable 'x' declared 'inline'",
    ),
    (
        "variable_declared_noreturn",
        "_Noreturn int x;\n",
        "variable 'x' declared '_Noreturn'",
    ),
    (
        "attribute_declaration_at_file_scope",
        "__attribute__((deprecated));\n",
        "empty declaration",
    ),
    (
        "attribute_declaration_as_statement",
        "void f(void) { __attribute__((deprecated)); }\n",
        "empty declaration",
    ),
    (
        "fallthrough_at_file_scope",
        "__attribute__((fallthrough));\n",
        "'fallthrough' attribute at top level",
    ),
    (
        "fallthrough_not_before_label",
        "void f(int x) { switch (x) { case 1: __attribute__((fallthrough)); x++; } }\n",
        "attribute 'fallthrough' not preceding a case label or default label",
    ),
];

/// The conversions 6.5.16.1p1 does not allow but gcc 13 converts anyway, and,
/// under `-fpermissive`, the implicit `int` and implicit function declaration
/// C99 removed. gcc 14 refuses most of them by default; c17 keeps gcc 13's
/// warning, and so takes gcc 13's error under `-pedantic-errors`.
const GCC13_DEFAULT_PEDWARNS: &[(&str, &str, &str, &[&str])] = &[
    (
        "argument_incompatible_pointer",
        "void g(int *p);\nvoid f(double *q) { g(q); }\n",
        "passing argument 1 of 'g' as 'int *' from 'double *' incompatible pointer type",
        &[],
    ),
    (
        "argument_to_unnamed_callee",
        "struct S { void (*fp)(int *); };\nvoid f(struct S s, double *q) { s.fp(q); }\n",
        "passing argument 1 as 'int *' from 'double *' incompatible pointer type",
        &[],
    ),
    (
        "assignment_pointer_from_integer",
        "void f(int *p, int q) { p = q; }\n",
        "assignment to 'int *' from 'int' makes pointer from integer without a cast",
        &[],
    ),
    (
        "initialization_discards_const",
        "const int c;\nint *p = &c;\n",
        "initialization of 'int *' from 'const int *' discards a qualifier from the pointer target type",
        &[],
    ),
    (
        "return_integer_from_pointer",
        "int f(int *p) { return p; }\n",
        "returning 'int *' from a function with return type 'int' makes integer from pointer without a cast",
        &[],
    ),
    (
        "implicit_int_permissive",
        "x;\n",
        "type specifier missing; implicit 'int' was removed in C99",
        &["-fpermissive"],
    ),
    (
        "implicit_function_declaration_permissive",
        "int f(void) { return g(); }\n",
        "implicit declaration of function 'g'",
        &["-fpermissive"],
    ),
];

/// What is wrong with `src`'s default pedwarn `msg`, if anything: it must be
/// a warning, still given under `-Wno-pedantic`, and an error under
/// `-pedantic-errors`.
fn default_pedwarn_fault(name: &str, src: &str, msg: &str, flags: &[&str]) -> Option<String> {
    let with = |extra: &[&str]| compile(name, src, &[flags, extra].concat());
    let warned = with(&[]);
    if !warned.success || !warned.stderr.contains(&format!("warning: {msg}")) {
        return Some(format!(
            "{name}: expected the warning {msg:?}:\n{}",
            warned.stderr
        ));
    }
    // `-Wno-pedantic` is about the other kind, and leaves these alone.
    let still = with(&["-Wno-pedantic"]);
    if !still.success || !still.stderr.contains(msg) {
        return Some(format!("{name}: -Wno-pedantic hid it:\n{}", still.stderr));
    }
    let strict = with(&["-pedantic-errors"]);
    if strict.success || !strict.stderr.contains(&format!("error: {msg}")) {
        return Some(format!(
            "{name}: -pedantic-errors should refuse it:\n{}",
            strict.stderr
        ));
    }
    None
}

#[test]
fn gccs_default_pedwarns_warn_and_are_errors_under_pedantic_errors() {
    let faults: Vec<String> = DEFAULT_PEDWARNS
        .iter()
        .map(|&(name, src, msg)| (name, src, msg, &[][..]))
        .chain(GCC13_DEFAULT_PEDWARNS.iter().copied())
        .filter_map(|(name, src, msg, flags)| default_pedwarn_fault(name, src, msg, flags))
        .collect();
    assert!(faults.is_empty(), "{}", faults.join("\n"));
}

/// `-pedantic-errors` makes errors of what is *given*. As in gcc, `-w` gives
/// nothing, and a system header -- which uses the extensions it objects to --
/// is not held to it.
#[test]
fn pedantic_errors_do_not_reach_hidden_warnings() {
    let src = "struct S { int a; int b };\n";
    let quiet = compile("pedwarn_w", src, &["-w", "-pedantic-errors"]);
    assert!(
        quiet.success && quiet.stderr.is_empty(),
        "-w: {}",
        quiet.stderr
    );
    let system = "# 1 \"sys.h\" 1 3\n\
                  struct S { int a; int b };\n\
                  void f(void); static void g(int c, int x) { c ? f() : x; }\n\
                  # 4 \"main.c\" 2\n\
                  int z;\n";
    let sys = compile("pedwarn_system_header", system, &["-pedantic-errors"]);
    assert!(sys.success && sys.stderr.is_empty(), "{}", sys.stderr);
}

/// What gcc reports only under `-pedantic`: silent by default, a warning
/// under `-pedantic`, an error under `-pedantic-errors`.
#[test]
fn gccs_pedantic_only_diagnostics_are_silent_by_default() {
    for (name, src, msg) in [
        (
            "label_at_end_of_block",
            "void f(void){ goto l; l: }\n",
            "label at end of compound statement",
        ),
        (
            "enumerator_outside_int",
            "enum E { A = 0x80000000 };\n",
            "ISO C restricts enumerator values to range of 'int'",
        ),
        (
            "line_number_zero",
            "#line 0\nint x;\n",
            "line number out of range",
        ),
        (
            "line_number_past_c17s_bound",
            "#line 3000000000\nint x;\n",
            "line number out of range",
        ),
    ] {
        let quiet = compile_accepted(name, src, &[]);
        assert!(!quiet.contains(msg), "{name}: warned by default:\n{quiet}");
        let loud = compile_accepted(name, src, &["-pedantic"]);
        assert!(loud.contains(&format!("warning: {msg}")), "{name}:\n{loud}");
        let strict = compile(name, src, &["-pedantic-errors"]);
        assert!(
            !strict.success && strict.stderr.contains(msg),
            "{name}:\n{}",
            strict.stderr
        );
    }
}

/// A null character between tokens is whitespace, with gcc's plain warning
/// once per run of them -- not a stray token that derails the declaration.
#[test]
fn null_characters_are_ignored_with_a_warning() {
    let src = "int x;\0\0 int y;\0\nint z\0;\n";
    let warned = compile_accepted("null_chars", src, &[]);
    assert_eq!(
        warned.matches("warning: null character(s) ignored").count(),
        3,
        "{warned}"
    );
    // A plain warning, which `-pedantic-errors` leaves alone.
    compile_accepted("null_chars_strict", src, &["-pedantic-errors"]);
}
