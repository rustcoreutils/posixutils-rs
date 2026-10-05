//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly cases of tests/codegen/fp_compare_traps.rs, in process: which
// compare instruction each floating comparison becomes.
//

use crate::test_asm::asm_probe::{
    asm_for_with, assert_body_contains, assert_body_lacks, calls_any, AARCH64_LINUX, X86_64_LINUX,
};

/// One function per comparison form, for type `T`.
fn forms(t: &str) -> String {
    format!(
        "int lt({t} a, {t} b) {{ return a < b; }}\n\
         int ge({t} a, {t} b) {{ if (a >= b) return 3; return 4; }}\n\
         int eq({t} a, {t} b) {{ return a == b; }}\n\
         int ne({t} a, {t} b) {{ return a != b; }}\n\
         int ql({t} a, {t} b) {{ return __builtin_isless(a, b); }}\n\
         int qg({t} a, {t} b) {{ return __builtin_isgreaterequal(a, b); }}\n"
    )
}

/// C17 F.9.3 on x86-64: `<` and `>=` are `comis*` (or `fcomip` for the x87
/// `long double`), which raise invalid for a quiet NaN; `==`, `!=` and the
/// <math.h> macros are `ucomis*` / `fucomip`, which do not. The flags the
/// two set are the same, so nothing else in the sequence changes.
#[test]
fn codegen_x86_64_relational_compares_signal() {
    for opt in ["-O0", "-O2"] {
        for (t, signaling, quiet) in [
            ("float", " comiss ", "ucomiss "),
            ("double", " comisd ", "ucomisd "),
            ("long double", " fcomip ", "fucomip "),
        ] {
            let asm = asm_for_with("fpcmp_x86", X86_64_LINUX, &forms(t), &[opt]);
            for f in ["lt", "ge"] {
                assert_body_contains(&asm, f, signaling, &format!("{t} {f} {opt}"));
                assert_body_lacks(&asm, f, quiet, &format!("{t} {f} {opt}"));
            }
            for f in ["eq", "ne", "ql", "qg"] {
                assert_body_contains(&asm, f, quiet, &format!("{t} {f} {opt}"));
                assert_body_lacks(&asm, f, signaling, &format!("{t} {f} {opt}"));
            }
        }
    }
}

/// The same on aarch64: `fcmpe` for the relational operators, `fcmp` for
/// the rest. `long double` is binary128 there, so it is libgcc's: the
/// ordering helpers signal, the equality ones do not, and a
/// quiet relational is the signaling helper behind an unordered test.
#[test]
fn codegen_aarch64_relational_compares_signal() {
    for opt in ["-O0", "-O2"] {
        for t in ["float", "double"] {
            let asm = asm_for_with("fpcmp_a64", AARCH64_LINUX, &forms(t), &[opt]);
            for f in ["lt", "ge"] {
                assert_body_contains(&asm, f, "fcmpe ", &format!("{t} {f} {opt}"));
                assert_body_lacks(&asm, f, "fcmp ", &format!("{t} {f} {opt}"));
            }
            for f in ["eq", "ne", "ql", "qg"] {
                assert_body_contains(&asm, f, "fcmp ", &format!("{t} {f} {opt}"));
                assert_body_lacks(&asm, f, "fcmpe ", &format!("{t} {f} {opt}"));
            }
        }
        let asm = asm_for_with("fpcmp_a64_ld", AARCH64_LINUX, &forms("long double"), &[opt]);
        let body = |f| crate::test_asm::asm_probe::body_of(&asm, f).to_string();
        assert!(
            calls_any(&body("lt"), &["__lttf2"]),
            "{opt}\n{}",
            body("lt")
        );
        assert!(
            calls_any(&body("ge"), &["__getf2"]),
            "{opt}\n{}",
            body("ge")
        );
        assert!(
            calls_any(&body("eq"), &["__eqtf2"]),
            "{opt}\n{}",
            body("eq")
        );
        assert!(
            calls_any(&body("ne"), &["__netf2"]),
            "{opt}\n{}",
            body("ne")
        );
        for (f, helper) in [("ql", "__lttf2"), ("qg", "__getf2")] {
            let b = body(f);
            assert!(calls_any(&b, &[helper]), "{opt} {f}\n{b}");
            // The guard: two self-comparisons, each a quiet `__netf2`, and a
            // branch around the signaling call.
            assert!(calls_any(&b, &["__netf2"]), "{opt} {f}\n{b}");
        }
    }
}
