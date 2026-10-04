//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 complex-number cases that need only a compile, moved from
// `tests/c99/complex.rs`.
//

use crate::test_compile::compile;

/// c17 provides no imaginary types: they are Annex G's, binding only an
/// implementation that defines `__STDC_IEC_559_COMPLEX__`. Without them
/// `_Imaginary` is no permitted type specifier (C17 6.7.2p2), so each use is
/// one error that says so -- not the implicit-int and keyword-as-name pair it
/// once drew -- while `_Imaginary` in a declarator's name position is the
/// keyword misused.
#[test]
fn c99_imaginary_is_one_accurate_error() {
    let not_supported = "imaginary types are not supported";
    let misused = "'_Imaginary' is a keyword and cannot be used as a name";
    for (name, src, expected) in [
        ("imag_decl", "_Imaginary double x;\n", not_supported),
        ("imag_trailing", "double _Imaginary x;\n", not_supported),
        ("imag_alone", "_Imaginary x;\n", not_supported),
        ("imag_fn", "float _Imaginary f(void);\n", not_supported),
        (
            "imag_member",
            "struct S { double _Imaginary m; };\n",
            not_supported,
        ),
        (
            "imag_local",
            "int f(void) { _Imaginary double y = 0; return 0; }\n",
            not_supported,
        ),
        (
            "imag_cast",
            "double f(double d) { return (double _Imaginary)d; }\n",
            not_supported,
        ),
        (
            "imag_sizeof",
            "int n = sizeof(float _Imaginary);\n",
            not_supported,
        ),
        ("imag_name", "int _Imaginary;\n", misused),
        ("imag_param", "void g(int _Imaginary);\n", misused),
    ] {
        let run = compile(name, src, &[]);
        assert!(!run.success, "{name}: compiled without error");
        let errors: Vec<&str> = run
            .stderr
            .lines()
            .filter(|l| l.contains("error:"))
            .collect();
        assert!(
            errors.len() == 1 && errors[0].contains(expected),
            "{name}: expected exactly one '{expected}', got:\n{}",
            run.stderr
        );
    }
}
