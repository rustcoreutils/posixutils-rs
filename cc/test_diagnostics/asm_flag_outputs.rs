//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Flag output operands, `"=@cc<cond>"`, and what gcc refuses of them: a
// condition the target does not name, alternatives, a flag constraint on an
// input, a type that is not an integer or a pointer, and a template naming
// the operand.
//

use super::asm_constraints::{expect_accepted, expect_rejected, A64, OPTS, X86};

/// `f(long x)` around one asm statement, on line 3, with a local `int r`.
fn statement(body: &str) -> String {
    format!(
        "int f(long x) {{\n\
         int r; float fl; struct {{ char c[3]; }} s; _Complex int ci; void *p;\n\
         __asm__ volatile({body});\n\
         return r;\n\
         }}\n"
    )
}

#[test]
fn asm_flag_outputs_gcc_refuses() {
    let both: &[(&str, &str, &str)] = &[
        (
            "asm_flag_unknown",
            r#""" : "=@ccfoo"(r)"#,
            "unknown 'asm' flag output '=@ccfoo'",
        ),
        (
            "asm_flag_empty",
            r#""" : "=@cc"(r)"#,
            "unknown 'asm' flag output '=@cc'",
        ),
        (
            "asm_flag_alternatives",
            r#""" : "=@ccne,r"(r)"#,
            "alternatives not allowed in 'asm' flag output",
        ),
        (
            "asm_flag_input",
            r#""" : : "@ccne"(x)"#,
            "invalid punctuation '@' in constraint",
        ),
        (
            "asm_flag_plus",
            r#""" : "+@ccne"(r)"#,
            "invalid punctuation '@' in constraint",
        ),
        (
            "asm_flag_tied",
            r#""" : "=@ccne"(r) : "0"(x)"#,
            "invalid punctuation '@' in constraint",
        ),
        (
            "asm_flag_float",
            r#""" : "=@ccne"(fl)"#,
            "invalid type for 'asm' flag output",
        ),
        (
            "asm_flag_struct",
            r#""" : "=@ccne"(s)"#,
            "invalid type for 'asm' flag output",
        ),
        (
            "asm_flag_complex",
            r#""" : "=@ccne"(ci)"#,
            "invalid type for 'asm' flag output",
        ),
    ];
    for &(name, body, wanted) in both {
        for opt in OPTS {
            expect_rejected(name, &statement(body), X86, opt, wanted, 3);
            expect_rejected(name, &statement(body), A64, opt, wanted, 3);
        }
    }
    // Each target's own names: aarch64 has no `z`, x86-64 no `eq`, and gcc
    // takes neither `pe` nor `po` on x86-64.
    let own: &[(&str, &str, [&str; 2], &str)] = &[
        ("asm_flag_x86_eq", r#""" : "=@cceq"(r)"#, X86, "'=@cceq'"),
        ("asm_flag_x86_pe", r#""" : "=@ccpe"(r)"#, X86, "'=@ccpe'"),
        ("asm_flag_x86_po", r#""" : "=@ccpo"(r)"#, X86, "'=@ccpo'"),
        ("asm_flag_a64_z", r#""" : "=@ccz"(r)"#, A64, "'=@ccz'"),
        ("asm_flag_a64_al", r#""" : "=@ccal"(r)"#, A64, "'=@ccal'"),
    ];
    for &(name, body, target, what) in own {
        let wanted = format!("unknown 'asm' flag output {what}");
        expect_rejected(name, &statement(body), target, "-O0", &wanted, 3);
    }
    // An early-clobber flag: gcc refuses it on aarch64 alone.
    expect_rejected(
        "asm_flag_early_a64",
        &statement(r#""" : "=&@cceq"(r)"#),
        A64,
        "-O0",
        "impossible constraint in 'asm'",
        3,
    );
    expect_accepted(
        "asm_flag_early_x86",
        &statement(r#""" : "=&@ccz"(r)"#),
        X86,
        "-O0",
    );
    // The operand is a flag, which a template cannot name.
    expect_rejected(
        "asm_flag_named",
        &statement(r##""# %0" : "=@ccz"(r)"##),
        X86,
        "-O0",
        "invalid use of asm flag output",
        3,
    );
}

/// c17 reads the flags on the fall-through only, so an `asm goto` with a
/// flag output is refused rather than leave the output unset on the jump.
/// gcc takes it.
#[test]
fn asm_flag_output_in_asm_goto_is_refused() {
    let src = "int f(long a) {\n\
               int z;\n\
               __asm__ goto(\"\" : \"=@ccz\"(z) : \"r\"(a) : : out);\n\
               return z;\n\
               out:\n\
               return 2;\n\
               }\n";
    for opt in OPTS {
        expect_rejected(
            "asm_flag_goto",
            src,
            X86,
            opt,
            "flag outputs are not supported in an 'asm goto' statement",
            3,
        );
    }
}

/// What gcc takes: any integer type, a pointer, and the operand numbered
/// among the others while the template names only those.
#[test]
fn asm_flag_outputs_gcc_accepts() {
    let x86 = r##""# %1" : "=@ccz"(r), "=r"(x), "=@ccnae"(p)"##;
    let a64 = r#""// %1" : "=@cceq"(r), "=r"(x), "=@cclo"(p)"#;
    for opt in OPTS {
        let asm = expect_accepted("asm_flag_ok", &statement(x86), X86, opt);
        assert!(
            asm.contains("setz") && asm.contains("setnae"),
            "{opt}:\n{asm}"
        );
        let asm = expect_accepted("asm_flag_ok_a64", &statement(a64), A64, opt);
        assert!(
            asm.contains(", eq") && asm.contains(", lo") && asm.contains("cset"),
            "{opt}:\n{asm}"
        );
    }
}
