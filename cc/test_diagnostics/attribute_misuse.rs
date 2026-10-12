//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// An attribute written where it cannot apply: a function's on an object, a
// typedef, a member or a parameter, and a symbol's on a name with no
// external linkage. gcc 13 warns and ignores most, and refuses `weak` on a
// name that is not public.
//

use crate::test_compile::compile;

/// The diagnostics of `src`, each without its file and position.
fn messages(name: &str, src: &str) -> Vec<String> {
    compile(name, src, &[])
        .stderr
        .lines()
        .filter(|l| !l.ends_with("compilation failed"))
        .map(|l| {
            let at = l.find(": warning: ").or_else(|| l.find(": error: "));
            l[at.map_or(0, |i| i + 2)..].to_string()
        })
        .collect()
}

/// `src` written for each of a file-scope object, a local, a typedef, a
/// member and a parameter, with `attr`.
fn on_non_functions(attr: &str) -> String {
    format!(
        "int gv __attribute__(({attr}));\n\
         void fl(void) {{ int lv __attribute__(({attr})); (void)lv; }}\n\
         typedef int td __attribute__(({attr}));\n\
         struct st {{ int m __attribute__(({attr})); }};\n\
         void fp(int p __attribute__(({attr})));\n"
    )
}

/// A function's own attributes are ignored anywhere else, in the words gcc
/// has for each kind.
#[test]
fn function_attributes_on_non_functions() {
    for (attr, said) in [
        ("cold", "warning: 'cold' attribute ignored [-Wattributes]"),
        (
            "noinline",
            "warning: 'noinline' attribute ignored [-Wattributes]",
        ),
        ("pure", "warning: 'pure' attribute ignored [-Wattributes]"),
        ("const", "warning: 'const' attribute ignored [-Wattributes]"),
        (
            "constructor",
            "warning: 'constructor' attribute ignored [-Wattributes]",
        ),
        (
            "malloc",
            "warning: 'malloc' attribute ignored; valid only for functions [-Wattributes]",
        ),
        (
            "nonnull",
            "warning: 'nonnull' attribute only applies to function types [-Wattributes]",
        ),
        (
            "format(printf, 1, 2)",
            "warning: 'format' attribute only applies to function types [-Wattributes]",
        ),
        (
            "warn_unused_result",
            "warning: 'warn_unused_result' attribute only applies to function types \
             [-Wattributes]",
        ),
        (
            "no_instrument_function",
            "error: 'no_instrument_function' attribute applies only to functions",
        ),
    ] {
        let got = messages("fn_attr_misuse", &on_non_functions(attr));
        assert_eq!(got, vec![said; 5], "{attr}");
    }
}

/// On a pointer to a function, an attribute of the function type and
/// `const` apply to the function; a declaration's own still does not.
#[test]
fn function_attributes_on_function_pointers() {
    for attr in [
        "nonnull",
        "format(printf, 1, 2)",
        "warn_unused_result",
        "const",
    ] {
        let src = format!("int (*fp)(const char *, ...) __attribute__(({attr}));\n");
        assert_eq!(
            messages("fn_ptr_attr", &src),
            Vec::<String>::new(),
            "{attr}"
        );
    }
    assert_eq!(
        messages("fn_ptr_cold", "int (*fp)(void) __attribute__((cold));\n"),
        ["warning: 'cold' attribute ignored [-Wattributes]"]
    );
    // A typedef of a function type takes a type's attribute, not `const`.
    assert_eq!(
        messages(
            "fn_typedef_attr",
            "typedef int F(const char *, ...) __attribute__((format(printf, 1, 2)));\n\
             typedef int G(void) __attribute__((const));\n"
        ),
        ["warning: 'const' attribute ignored [-Wattributes]"]
    );
}

/// On a function all of them are taken in silence.
#[test]
fn function_attributes_on_functions_are_silent() {
    let src = "__attribute__((cold, noinline, pure, malloc, nonnull, warn_unused_result))\n\
               void *f(void *p);\n\
               __attribute__((format(printf, 1, 2), const)) int g(const char *, ...);\n";
    assert_eq!(messages("fn_attr_ok", src), Vec::<String>::new());
}

/// `weak` on a name that is not public is an error; `visibility` on one
/// is ignored, as are `used`, `alias` and `section` where there is no
/// symbol.
#[test]
fn symbol_attributes_need_a_public_symbol() {
    let src = "static int sv __attribute__((weak)); int *psv = &sv;\n\
               static void sf(void) __attribute__((weak)); static void sf(void) {}\n\
               void (*psf)(void) = sf;\n\
               void fl(void) { int lv __attribute__((weak)); (void)lv; }\n";
    assert_eq!(
        messages("weak_private", src),
        [
            "error: weak declaration of 'sv' must be public",
            "error: weak declaration of 'sf' must be public",
            "error: weak declaration of 'lv' must be public",
        ]
    );
    let src = "static int sv __attribute__((visibility(\"hidden\"))); int *psv = &sv;\n\
               static void sf(void) __attribute__((visibility(\"hidden\")));\n\
               static void sf(void) {}\nvoid (*psf)(void) = sf;\n\
               void fl(void) { int lv __attribute__((used)); (void)lv; \
               static int ls __attribute__((alias(\"y\"))); (void)ls; }\n";
    let ignored = |a: &str| format!("warning: '{a}' attribute ignored [-Wattributes]");
    assert_eq!(
        messages("visibility_private", src),
        [
            ignored("visibility"),
            ignored("visibility"),
            ignored("used"),
            ignored("alias"),
        ]
    );
    assert_eq!(
        messages(
            "section_local",
            "void fl(void) { int lv __attribute__((section(\"s\"))); (void)lv; }\n"
        ),
        ["error: section attribute cannot be specified for local variables"]
    );
}

/// On a typedef, a member or a parameter a symbol's attribute has nothing
/// to act on.
#[test]
fn symbol_attributes_on_non_symbols() {
    assert_eq!(
        messages("weak_non_symbol", &on_non_functions("weak"))[1..],
        vec!["warning: 'weak' attribute ignored [-Wattributes]"; 3]
    );
    let src = "typedef int td __attribute__((section(\"s\")));\n\
               struct st { int m __attribute__((section(\"s\"))); };\n";
    assert_eq!(
        messages("section_non_symbol", src),
        [
            "error: section attribute not allowed for 'td'",
            "error: section attribute not allowed for 'm'",
        ]
    );
}

/// `transparent_union` on anything but a union, in gcc's words.
#[test]
fn transparent_union_on_a_non_union() {
    assert_eq!(
        messages(
            "transparent_misuse",
            "struct T { int a; } __attribute__((transparent_union));\n"
        ),
        ["warning: 'transparent_union' attribute ignored [-Wattributes]"]
    );
}

/// `assume_aligned(N[, M])` is gcc's: taken in silence on a function -- the
/// Linux kernel's `__assume_aligned` drew an "attribute directive ignored"
/// warning on every use -- and diagnosed in gcc's words where its arguments
/// are wrong or there is no function type to carry it.
#[test]
fn assume_aligned_is_gccs() {
    let ok = "void *a(void) __attribute__((assume_aligned(64)));\n\
              void *b(void) __attribute__((__assume_aligned__(32, 8)));\n\
              void *(*fp)(void) __attribute__((assume_aligned(16)));\n\
              _Static_assert(__has_attribute(assume_aligned), \"has\");\n";
    assert_eq!(messages("assume_aligned_ok", ok), Vec::<String>::new());
    let bad = "int v __attribute__((assume_aligned(16)));\n\
               void *c(void) __attribute__((assume_aligned(3)));\n\
               void *d(void) __attribute__((assume_aligned(16, 16)));\n\
               void *e(void) __attribute__((assume_aligned(16, 1, 2)));\n\
               void *f(void) __attribute__((assume_aligned));\n";
    assert_eq!(
        messages("assume_aligned_bad", bad),
        [
            "warning: 'assume_aligned' attribute only applies to function types [-Wattributes]",
            "warning: 'assume_aligned' attribute argument 3 is not a power of 2 [-Wattributes]",
            "warning: 'assume_aligned' attribute argument 16 is not in the range [0, 15] \
             [-Wattributes]",
            "error: wrong number of arguments specified for 'assume_aligned' attribute",
            "error: wrong number of arguments specified for 'assume_aligned' attribute",
        ]
    );
}

/// `malloc(dealloc, n)` and `copy(decl)` read their arguments as
/// expressions, so an undeclared name is refused, as gcc refuses it; and
/// `malloc`'s are checked as gcc checks them: the deallocator is a function
/// whose first parameter is a pointer, `n` the number of a pointer
/// parameter of it. Each case alone, with gcc 13's verdict.
#[test]
fn malloc_and_copy_arguments_as_gcc_reads_them() {
    let pre = "void fr(void *);\nvoid fr2(void *, int);\nvoid fi(int);\nint v;\n\
               void (*pf)(void *);\n";
    for (decl, said) in [
        (
            "void *a(int) __attribute__((malloc(nofree, 1)));",
            "error: 'nofree' undeclared here (not in a function)",
        ),
        (
            "void *a(int) __attribute__((malloc(nofree)));",
            "error: 'nofree' undeclared here (not in a function)",
        ),
        (
            "int b __attribute__((copy(nothing)));",
            "error: 'nothing' undeclared here (not in a function)",
        ),
        (
            "void c(void) __attribute__((copy(nothing)));",
            "error: 'nothing' undeclared here (not in a function)",
        ),
        (
            "void g(void) { int y __attribute__((copy(zzz))) = 0; (void)y; }",
            "error: 'zzz' undeclared (first use in this function)",
        ),
        (
            "void *a(int) __attribute__((malloc(v, 1)));",
            "error: 'malloc' attribute argument 1 does not name a function",
        ),
        (
            "void *a(int) __attribute__((malloc(1)));",
            "error: 'malloc' attribute argument 1 does not name a function",
        ),
        (
            "void *a(int) __attribute__((malloc(pf)));",
            "error: 'malloc' attribute argument 1 does not name a function",
        ),
        (
            "void *a(int) __attribute__((malloc(fi)));",
            "error: 'malloc' attribute argument 1 must take a pointer type as its first \
             argument; have 'int'",
        ),
        (
            "void *a(int) __attribute__((malloc(fr, 1, 2)));",
            "error: wrong number of arguments specified for 'malloc' attribute",
        ),
        (
            "void *a(int) __attribute__((malloc(fr, 2)));",
            "warning: 'malloc' attribute argument value '2' exceeds the number of function \
             parameters 1 [-Wattributes]",
        ),
        (
            "void *a(int) __attribute__((malloc(fr, 0)));",
            "warning: 'malloc' attribute argument value '0' does not refer to a function \
             parameter [-Wattributes]",
        ),
        (
            "void *a(int) __attribute__((malloc(fr, v)));",
            "warning: 'malloc' attribute argument value 'v' is not an integer constant \
             [-Wattributes]",
        ),
        (
            "void *a(int) __attribute__((malloc(fr2, 2)));",
            "warning: 'malloc' attribute argument value '2' refers to parameter type 'int' \
             [-Wattributes]",
        ),
        (
            "void *a(int) __attribute__((malloc(fr, \"1\")));",
            "warning: 'malloc' attribute argument has type 'char[2]' [-Wattributes]",
        ),
    ] {
        let got = messages("malloc_copy_args", &format!("{pre}{decl}\n"));
        assert_eq!(
            got.first().map(String::as_str),
            Some(said),
            "{decl}: {got:#?}"
        );
    }
    // What gcc takes.
    for decl in [
        "void *a(int) __attribute__((malloc(fr, 1)));",
        "void *a(int) __attribute__((malloc(&fr)));",
        "void *a(int) __attribute__((malloc(fr2, 1)));",
        "void *a(int) __attribute__((malloc));",
        "int b __attribute__((copy(v)));",
        "void c(void) __attribute__((copy(fr)));",
    ] {
        let got = messages("malloc_copy_ok", &format!("{pre}{decl}\n"));
        assert!(got.is_empty(), "{decl}: {got:#?}");
    }
}
