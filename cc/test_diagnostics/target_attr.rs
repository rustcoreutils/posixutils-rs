//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__attribute__((target("...")))` and `target_clones(...)`: what gcc 13
// refuses, in its words, and c17's warning for an ISA beyond its SSE4.2
// ceiling. Every case names its target and runs the same on any host.
//

use crate::test_compile::{compile, compile_accepted, compile_rejected_with};

const LINUX: &str = "--target=x86_64-unknown-linux-gnu";

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

/// An ISA gcc knows and c17 does not model is named, and the function is
/// still compiled; the modelled part of the same string applies.
#[test]
fn diagnostics_target_beyond_the_ceiling_is_named() {
    for isa in ["avx", "avx2", "avx512f", "bmi2", "fma", "f16c"] {
        expect_warning(
            &format!("target_{isa}"),
            &format!("__attribute__((target(\"sse4.1,{isa}\"))) int f(int x) {{ return x; }}\n"),
            &format!(
                "ISA '{isa}' in 'target' attribute is beyond c17's SSE4.2 ceiling and is ignored"
            ),
        );
    }
    // Turning off what c17 never generates says nothing.
    let stderr = compile_accepted(
        "target_no_avx2",
        "__attribute__((target(\"no-avx2\"))) int f(int x) { return x; }\n",
        &[LINUX],
    );
    assert!(stderr.is_empty(), "{stderr}");
    // `-Wno-attributes` silences it, as every attribute warning.
    let c = compile(
        "target_avx2_quiet",
        "__attribute__((target(\"avx2\"))) int f(int x) { return x; }\n",
        &[LINUX, "-Wno-attributes"],
    );
    assert!(c.success && c.stderr.is_empty(), "{}", c.stderr);
}

/// The strings gcc refuses, and the one it warns about.
#[test]
fn diagnostics_target_strings_gcc_refuses() {
    expect_error(
        "target_unknown",
        "__attribute__((target(\"foo\"))) int f(int x) { return x; }\n",
        "attribute 'target' argument 'foo' is unknown",
    );
    expect_error(
        "target_unknown_in_list",
        "__attribute__((target(\"sse4.1,bogus\"))) int f(int x) { return x; }\n",
        "attribute 'target' argument 'bogus' is unknown",
    );
    expect_error(
        "target_bad_arch",
        "__attribute__((target(\"arch=foo\"))) int f(int x) { return x; }\n",
        "bad value 'foo' for 'target(\"arch=\")' attribute",
    );
    expect_error(
        "target_bad_fpmath",
        "__attribute__((target(\"fpmath=foo\"))) int f(int x) { return x; }\n",
        "attribute value 'fpmath=foo' is unknown in 'target' attribute",
    );
    expect_error(
        "target_not_string",
        "__attribute__((target(42))) int f(int x) { return x; }\n",
        "attribute 'target' argument is not a string",
    );
    expect_warning(
        "target_empty",
        "__attribute__((target(\"\"))) int f(int x) { return x; }\n",
        "empty string in attribute 'target'",
    );
}

/// `target_clones` lists gcc refuses, and the ones it warns about.
#[test]
fn diagnostics_target_clones_lists() {
    expect_error(
        "clones_no_default",
        "__attribute__((target_clones(\"sse4.1\", \"sse4.2\"))) int f(int x) { return x; }\n",
        "'default' target was not set",
    );
    expect_error(
        "clones_two_defaults",
        "__attribute__((target_clones(\"sse4.2\", \"default\", \"default\")))\n\
         int f(int x) { return x; }\n",
        "multiple 'default' targets were set",
    );
    expect_error(
        "clones_unknown",
        "__attribute__((target_clones(\"sse4.2\", \"default\", \"bogus\")))\n\
         int f(int x) { return x; }\n",
        "attribute 'target_clone' argument 'bogus' is unknown",
    );
    expect_error(
        "clones_negated",
        "__attribute__((target_clones(\"no-sse4.2\", \"default\"))) int f(int x) { return x; }\n",
        "ISA 'no-sse4.2' is not supported in 'target' attribute, use 'arch=' syntax",
    );
    expect_warning(
        "clones_single",
        "__attribute__((target_clones(\"sse4.2\"))) int f(int x) { return x; }\n",
        "single 'target_clones' attribute is ignored",
    );
    expect_warning(
        "clones_above_ceiling",
        "__attribute__((target_clones(\"avx2\", \"sse4.2\", \"default\")))\n\
         int f(int x) { return x; }\n",
        "'target_clones' version 'avx2' is beyond c17's SSE4.2 ceiling and is not built",
    );
    expect_warning(
        "clones_and_target",
        "__attribute__((target(\"sse4.1\"), target_clones(\"sse4.2\", \"default\")))\n\
         int f(int x) { return x; }\n",
        "'target_clones' attribute ignored due to conflict with 'target' attribute",
    );
}

/// Mach-O has no indirect functions: the function is its `default`
/// version, under its own name, and nothing is said.
#[test]
fn diagnostics_target_clones_on_mach_o_is_the_default_body() {
    let c = compile(
        "clones_macho",
        "__attribute__((target_clones(\"sse4.2\", \"default\"))) int f(int x) { return x; }\n",
        &["--target=x86_64-apple-darwin"],
    );
    assert!(c.success && c.stderr.is_empty(), "{}", c.stderr);
    let asm = c.asm.unwrap();
    assert!(asm.contains("_f:") && !asm.contains("resolver"), "{asm}");
}

/// A `target_clones` body is lowered once per version, but a diagnostic about
/// its source is the source's, and is reported once.
#[test]
fn target_clones_body_diagnostics_are_reported_once() {
    let src = "__attribute__((target_clones(\"sse4.2\", \"ssse3\", \"default\")))\n\
               int h(int a) { if (a) goto nowhere; return a; }\n";
    let c = crate::test_compile::compile(
        "target_clones_diag_once",
        src,
        &["--target=x86_64-unknown-linux-gnu"],
    );
    assert!(!c.success);
    assert_eq!(
        c.stderr
            .matches("label 'nowhere' used but not defined")
            .count(),
        1,
        "{}",
        c.stderr
    );
}
