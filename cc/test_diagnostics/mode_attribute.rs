//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `__attribute__((mode(M)))` names a machine mode, and gcc refuses one that
// does not fit the declared type: a floating mode on an integer type, an
// integer mode on a floating one, a mode no pointer has, a non-integer mode
// on an enumeration, and a name that is no mode at all. Every message is
// gcc 13's, for the target named.
//

use crate::test_compile::compile;

const X86_64: &str = "--target=x86_64-unknown-linux-gnu";
const AARCH64: &str = "--target=aarch64-unknown-linux-gnu";

/// Require `decl` to be rejected on `target` with `msg`.
#[track_caller]
fn expect_rejected(target: &str, decl: &str, msg: &str) {
    let src = format!("{decl}\n");
    let c = compile("mode_bad", &src, &[target]);
    assert!(!c.success, "{target}: '{decl}' compiled:\n{}", c.stderr);
    assert!(
        c.stderr.contains(&format!("error: {msg}")),
        "{target}: '{decl}' did not say {msg:?}:\n{}",
        c.stderr
    );
}

/// Require `decl` to compile on `target` with no diagnostic about `mode`.
#[track_caller]
fn expect_accepted(target: &str, decl: &str) {
    let src = format!("{decl}\n");
    let c = compile("mode_ok", &src, &[target]);
    assert!(c.success, "{target}: '{decl}' was rejected:\n{}", c.stderr);
    assert!(
        !c.stderr.contains("mode"),
        "{target}: '{decl}':\n{}",
        c.stderr
    );
}

#[test]
fn diagnostics_mode_of_another_type_class_is_rejected() {
    for target in [X86_64, AARCH64] {
        for (decl, mode) in [
            ("int x __attribute__((mode(SF)));", "SF"),
            ("int x __attribute__((mode(DF)));", "DF"),
            ("int x __attribute__((mode(TF)));", "TF"),
            ("int x __attribute__((mode(HF)));", "HF"),
            ("int x __attribute__((mode(BF)));", "BF"),
            ("int x __attribute__((mode(SD)));", "SD"),
            ("int x __attribute__((mode(SC)));", "SC"),
            ("int x __attribute__((mode(CSI)));", "CSI"),
            ("unsigned x __attribute__((mode(__DF__)));", "DF"),
            ("float x __attribute__((mode(SI)));", "SI"),
            ("double x __attribute__((mode(DI)));", "DI"),
            ("_Bool x __attribute__((mode(SI)));", "SI"),
            ("_Complex float x __attribute__((mode(DF)));", "DF"),
            ("_Complex double x __attribute__((mode(SI)));", "SI"),
            ("struct S { int a; } x __attribute__((mode(SI)));", "SI"),
            ("typedef int T __attribute__((mode(DF)));", "DF"),
            ("struct M { int m __attribute__((mode(SF))); };", "SF"),
        ] {
            expect_rejected(
                target,
                decl,
                &format!("mode '{mode}' applied to inappropriate type"),
            );
        }
    }
    // The x87 format exists only on x86.
    expect_rejected(
        X86_64,
        "int x __attribute__((mode(XF)));",
        "mode 'XF' applied to inappropriate type",
    );
}

#[test]
fn diagnostics_mode_that_names_no_mode_is_rejected() {
    for target in [X86_64, AARCH64] {
        for mode in ["FOO", "KF", "PDI", "si"] {
            expect_rejected(
                target,
                &format!("int x __attribute__((mode({mode})));"),
                &format!("unknown machine mode '{mode}'"),
            );
        }
        for mode in ["BLK", "BI", "CC", "QQ", "OI"] {
            expect_rejected(
                target,
                &format!("int x __attribute__((mode({mode})));"),
                &format!("unable to emulate '{mode}'"),
            );
        }
        expect_rejected(
            target,
            "int *p __attribute__((mode(SI)));",
            "invalid pointer mode 'SI'",
        );
        expect_rejected(
            target,
            "enum E { A } e __attribute__((mode(SF)));",
            "cannot use mode 'SF' for enumerated types",
        );
        expect_rejected(
            target,
            "int x __attribute__((mode));",
            "wrong number of arguments specified for 'mode' attribute",
        );
    }
    for mode in ["XF", "__XF__", "XC"] {
        expect_rejected(
            AARCH64,
            &format!("long double x __attribute__((mode({mode})));"),
            &format!("unknown machine mode '{mode}'"),
        );
    }
}

/// A mode written as something other than a name is ignored, with gcc's
/// warning.
#[test]
fn diagnostics_mode_with_a_non_identifier_is_ignored() {
    let c = compile(
        "mode_int_arg",
        "int x __attribute__((mode(1)));\n",
        &[X86_64],
    );
    assert!(c.success, "{}", c.stderr);
    assert!(
        c.stderr.contains("warning: 'mode' attribute ignored"),
        "{}",
        c.stderr
    );
}

/// The accept side: every mode of the declared type's own class, a pointer
/// at the pointer width, an enumeration at an integer mode.
#[test]
fn diagnostics_mode_of_the_same_type_class_is_accepted() {
    for target in [X86_64, AARCH64] {
        for decl in [
            "int x __attribute__((mode(QI)));",
            "unsigned x __attribute__((mode(__HI__)));",
            "int x __attribute__((mode(SI)));",
            "long x __attribute__((mode(DI)));",
            "int x __attribute__((mode(TI)));",
            "int x __attribute__((mode(byte)));",
            "int x __attribute__((mode(word)));",
            "int x __attribute__((mode(pointer)));",
            "int x __attribute__((mode(__unwind_word__)));",
            "char x __attribute__((mode(SI)));",
            "float x __attribute__((mode(DF)));",
            "double x __attribute__((mode(SF)));",
            "long double x __attribute__((mode(DF)));",
            "float x __attribute__((mode(TF)));",
            "_Complex float x __attribute__((mode(DC)));",
            "_Complex double x __attribute__((mode(SC)));",
            "int *p __attribute__((mode(DI)));",
            "int *p __attribute__((mode(pointer)));",
            "enum E { A } e __attribute__((mode(QI)));",
            "typedef unsigned U8 __attribute__((mode(QI)));",
        ] {
            expect_accepted(target, decl);
        }
    }
    expect_accepted(X86_64, "float x __attribute__((mode(XF)));");
    expect_accepted(X86_64, "_Complex float x __attribute__((mode(XC)));");
}
