//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// The second and third operands of `?:` must be one of the pairs C17
// 6.5.15p3 lists: two arithmetic values, the same structure or union, two
// `void`s, pointers to compatible types, a pointer and a null pointer
// constant, or an object pointer and `void *`. gcc warns about two
// pointers to incompatible types and about a pointer beside an integer, and
// rejects every other pair; the wording and severities here are gcc's.
//

use crate::test_compile::{
    compile, compile_expect_error, compile_expect_no_diagnostic, compile_expect_warning,
};

const DECLS: &str = "struct S { int a; } s, u; struct T { int a; } t; union U { int a; } w;\n\
                     int c; int *p; char *cp; const int *cip; volatile int *vip; void *vp;\n\
                     int (*fp)(void); int (*fnp)(); const int (*cap)[]; int (*a3)[3];\n\
                     unsigned *up; long l; double d; void g(void);\n";

fn source(expr: &str) -> String {
    format!("{DECLS}void f(void) {{ (void)({expr}); }}\n")
}

/// Every pair 6.5.15p3 admits compiles without a word, and a null pointer
/// constant -- however it is spelled -- is no mismatch beside any pointer.
#[test]
fn conditional_permitted_operand_pairs_are_silent() {
    let cases = [
        ("cond_arith", "c ? l : d"),
        ("cond_struct", "c ? s : u"),
        ("cond_union", "c ? w : w"),
        ("cond_void", "c ? g() : g()"),
        ("cond_npc_void", "c ? p : (void *)0"),
        ("cond_npc_void_first", "c ? (void *)0 : p"),
        ("cond_npc_int", "c ? p : 0"),
        ("cond_npc_long", "c ? p : 0L"),
        ("cond_npc_folded", "c ? p : (1 - 1)"),
        ("cond_npc_fnptr", "c ? fp : (void *)0"),
        ("elvis_npc", "p ?: (void *)0"),
        ("cond_void_ptr", "c ? p : vp"),
        ("cond_quals", "c ? cip : vip"),
        ("cond_same", "c ? p : p"),
        ("cond_composite_array", "c ? cap : a3"),
        ("cond_composite_function", "c ? fp : fnp"),
    ];
    for (name, expr) in cases {
        compile_expect_no_diagnostic(name, &source(expr), "conditional");
    }
}

/// Pointers to incompatible types: a cast to a pointer type other than
/// `void *` is a pointer of that type, not a null pointer constant, so
/// `(char *)0` is as much a mismatch as `cp`.
#[test]
fn conditional_incompatible_pointers_warn() {
    let cases = [
        ("cond_char_null", "c ? p : (char *)0"),
        ("cond_char_ptr", "c ? p : cp"),
        ("cond_char_ptr_first", "c ? cp : p"),
        ("cond_qualified", "c ? cip : cp"),
        ("cond_signedness", "c ? p : up"),
        ("cond_array_extent", "c ? a3 : (int (*)[4])0"),
        ("elvis_char_null", "p ?: (char *)0"),
    ];
    for (name, expr) in cases {
        compile_expect_warning(
            name,
            &source(expr),
            "pointer type mismatch in conditional expression",
        );
    }
}

/// A pointer beside an integer that is not a null pointer constant, in
/// either order. An integer that is zero only after passing through a
/// pointer is no integer constant expression (6.6p6), so no null constant.
#[test]
fn conditional_pointer_and_integer_warn() {
    let cases = [
        ("cond_ptr_one", "c ? p : 1"),
        ("cond_long_ptr", "c ? l : p"),
        ("cond_ptr_var", "c ? p : c"),
        ("cond_ptr_through_ptr", "c ? p : (int)(long)(char *)0"),
        ("elvis_ptr_one", "p ?: 1"),
    ];
    for (name, expr) in cases {
        compile_expect_warning(
            name,
            &source(expr),
            "pointer/integer type mismatch in conditional expression",
        );
    }
}

/// Every other pair is a constraint violation, and an error in gcc too.
#[test]
fn conditional_mismatched_operands_are_errors() {
    let cases = [
        ("cond_two_structs", "c ? s : t"),
        ("cond_struct_union", "c ? s : w"),
        ("cond_struct_int", "c ? s : 1"),
        ("cond_int_struct", "c ? 1 : s"),
        ("cond_double_ptr", "c ? 1.0 : p"),
        ("cond_ptr_double", "c ? p : d"),
        ("cond_struct_ptr", "c ? s : p"),
    ];
    for (name, expr) in cases {
        compile_expect_error(
            name,
            &source(expr),
            "type mismatch in conditional expression",
        );
    }
}

/// One error, not one per enclosing operator: the mismatched conditional is
/// left without a type, so the `+` around it has nothing more to say. In
/// `s ?: 1` the structure is the condition, and is reported as one.
#[test]
fn conditional_mismatch_does_not_cascade() {
    let cases = [
        (
            "cond_no_cascade",
            "return (c ? s : t) + 1;",
            "type mismatch",
        ),
        (
            "elvis_no_cascade",
            "return (s ?: 1) + 1;",
            "scalar is required",
        ),
    ];
    for (name, body, expected) in cases {
        let run = compile(name, &format!("{DECLS}int f(void) {{ {body} }}\n"), &[]);
        assert!(!run.success, "{name} should be rejected");
        assert_eq!(
            run.stderr.matches("error").count(),
            1,
            "{name}: expected one error, got:\n{}",
            run.stderr
        );
        assert!(run.stderr.contains(expected), "{name}:\n{}", run.stderr);
    }
}

/// A function pointer beside `void *` is outside 6.5.15p3, whose `void *`
/// carve-out is for object pointers. gcc accepts it in silence and objects
/// only under `-pedantic`, as for the same conversion by assignment, and
/// `-Wno-pedantic` after it silences it again. `(void *)0` is a null pointer
/// constant and draws nothing even then.
#[test]
fn conditional_function_pointer_and_void_pointer() {
    let src = source("c ? fp : vp");
    let want = "ISO C forbids conditional expr between 'void *' and function pointer";
    compile_expect_no_diagnostic("cond_fnptr_void", &src, "ISO C forbids");
    for flags in [&["-pedantic"][..], &["-Wpedantic"]] {
        let run = compile("cond_fnptr_void_pedantic", &src, flags);
        assert!(run.success, "should compile: {}", run.stderr);
        assert!(run.stderr.contains(want), "{flags:?}:\n{}", run.stderr);
    }
    let run = compile(
        "cond_fnptr_void_silenced",
        &src,
        &["-pedantic", "-Wno-pedantic"],
    );
    assert!(run.success, "should compile: {}", run.stderr);
    assert!(
        !run.stderr.contains("ISO C forbids"),
        "-Wno-pedantic should silence it, got:\n{}",
        run.stderr
    );
    let null = compile(
        "cond_fnptr_null",
        &source("c ? fp : (void *)0"),
        &["-pedantic"],
    );
    assert!(
        null.success && !null.stderr.contains("ISO C forbids"),
        "{}",
        null.stderr
    );
}
