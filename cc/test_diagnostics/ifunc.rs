//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__attribute__((ifunc("resolver")))`: what gcc refuses, in its words. The
// attribute is ELF-only, so every case names a Linux target and runs the
// same on any host.
//

use crate::test_compile::{compile, compile_accepted, compile_rejected_with};

const LINUX: &str = "--target=x86_64-unknown-linux-gnu";

/// A resolver both cases below can use: it returns `void *`, which gcc
/// accepts for any indirect function.
const RES: &str = "static int impl(int x) { return x; }\n\
                   static void *res(void) { return (void *)impl; }\n";

#[track_caller]
fn expect_error(name: &str, src: &str, expected: &str) {
    let stderr = compile_rejected_with(name, src, &[LINUX]);
    assert!(
        stderr.contains(&format!("error: {expected}")),
        "'{name}': no error {expected:?}.\nstderr:\n{stderr}"
    );
}

#[track_caller]
fn expect_warning(name: &str, src: &str, expected: &str) {
    let stderr = compile_accepted(name, src, &[LINUX]);
    assert!(
        stderr.contains(&format!("warning: {expected}")),
        "'{name}': no warning {expected:?}.\nstderr:\n{stderr}"
    );
}

/// The resolver has to be defined in this unit, as an `alias` target does,
/// and has to be a function.
#[test]
fn diagnostics_ifunc_resolver_must_be_a_local_function() {
    expect_error(
        "ifunc_undefined",
        "int f(int) __attribute__((ifunc(\"nores\")));\n",
        "'f' aliased to undefined symbol 'nores'",
    );
    expect_error(
        "ifunc_declared_only",
        "extern void *res(void);\nint f(int) __attribute__((ifunc(\"res\")));\n",
        "'f' aliased to undefined symbol 'res'",
    );
    expect_error(
        "ifunc_object_resolver",
        "int res;\nint f(int) __attribute__((ifunc(\"res\")));\n",
        "'f' alias between function and variable is not supported",
    );
    // Defined after the indirect function is declared is still defined.
    compile_accepted(
        "ifunc_resolver_after",
        "static void *res(void);\nint f(int) __attribute__((ifunc(\"res\")));\n\
         static int impl(int x) { return x; }\n\
         static void *res(void) { return (void *)impl; }\n",
        &[LINUX],
    );
}

/// The `ifunc` declaration is `f`'s definition, so a body is a second one,
/// and the dynamic linker binds an indirect function once, so it cannot be
/// weak.
#[test]
fn diagnostics_ifunc_is_the_definition() {
    expect_error(
        "ifunc_then_body",
        &format!(
            "{RES}int f(int) __attribute__((ifunc(\"res\")));\nint f(int x) {{ return x; }}\n"
        ),
        "redefinition of 'f'",
    );
    expect_error(
        "ifunc_weak",
        &format!("{RES}int f(int) __attribute__((weak, ifunc(\"res\")));\n"),
        "weak 'f' cannot be defined 'ifunc'",
    );
    // An ordinary redeclaration is not a definition.
    compile_accepted(
        "ifunc_redeclared",
        &format!("{RES}int f(int) __attribute__((ifunc(\"res\")));\nint f(int);\n"),
        &[LINUX],
    );
}

/// gcc ignores `ifunc` on a variable, with a warning, and the variable is
/// an ordinary one.
#[test]
fn diagnostics_ifunc_on_a_variable_is_ignored() {
    let src = format!("{RES}int v __attribute__((ifunc(\"res\")));\nint *p = &v;\n");
    expect_warning("ifunc_variable", &src, "'ifunc' attribute ignored");
    let c = compile("ifunc_variable_asm", &src, &[LINUX]);
    let asm = c.asm.expect("accepted");
    assert!(
        !asm.contains("gnu_indirect_function") && !asm.contains(".set"),
        "the variable became an alias:\n{asm}"
    );
}

/// A resolver returns a pointer to the indirect function's type: anything
/// but a pointer is an error; a pointer to another type a warning; `void *`
/// and the exact type are accepted silently.
#[test]
fn diagnostics_ifunc_resolver_return_type() {
    expect_error(
        "ifunc_resolver_int",
        "static int res(void) { return 0; }\nint f(int) __attribute__((ifunc(\"res\")));\n",
        "'ifunc' resolver for 'f' must return 'int (*)(int)'",
    );
    // gcc reads the resolver's type off the name the attribute gives, so an
    // indirect function is not a resolver even though it resolves to one.
    expect_error(
        "ifunc_resolver_is_ifunc",
        &format!(
            "{RES}int f(int) __attribute__((ifunc(\"res\")));\n\
             int h(int) __attribute__((ifunc(\"f\")));\n"
        ),
        "'ifunc' resolver for 'h' must return 'int (*)(int)'",
    );
    expect_warning(
        "ifunc_resolver_other_fn",
        "typedef long (*fp)(int);\nstatic fp res(void) { return 0; }\n\
         int f(int) __attribute__((ifunc(\"res\")));\n",
        "'ifunc' resolver for 'f' should return 'int (*)(int)'",
    );
    expect_warning(
        "ifunc_resolver_char_ptr",
        "static char *res(void) { return 0; }\n\
         static int f(int) __attribute__((ifunc(\"res\")));\n",
        "'ifunc' resolver for 'f' should return 'int (*)(int)'",
    );
    for (name, src) in [
        ("ifunc_resolver_void_ptr", RES.to_string()),
        (
            "ifunc_resolver_exact",
            "typedef int (*fp)(int);\nstatic int impl(int x) { return x; }\n\
             static fp res(void) { return impl; }\n"
                .to_string(),
        ),
    ] {
        let src = format!("{src}int f(int) __attribute__((ifunc(\"res\")));\n");
        let stderr = compile_accepted(name, &src, &[LINUX]);
        assert!(!stderr.contains("resolver"), "'{name}':\n{stderr}");
    }
}

/// Mach-O has no indirect functions.
#[test]
fn diagnostics_ifunc_unsupported_on_darwin() {
    let src = format!("{RES}int f(int) __attribute__((ifunc(\"res\")));\n");
    for target in ["aarch64-apple-darwin", "x86_64-apple-darwin"] {
        let stderr = compile_rejected_with("ifunc_darwin", &src, &[&format!("--target={target}")]);
        assert!(
            stderr.contains("error: ifunc is not supported on this target"),
            "{target}:\n{stderr}"
        );
    }
}
