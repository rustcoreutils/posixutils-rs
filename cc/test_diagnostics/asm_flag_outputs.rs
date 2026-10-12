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

/// An `asm goto` with a flag output, which gcc takes: the template's jump
/// goes to a label of c17's own that reads the flags, as the fall-through
/// does, on its way to the label's block. `codegen::asm_flag_outputs` runs
/// it.
#[test]
fn asm_flag_output_in_asm_goto_is_read_on_every_path() {
    let src = "int f(long a) {\n\
               int z;\n\
               __asm__ goto(\"cmp $0, %1\\n\\tje %l2\" : \"=@ccz\"(z) : \"r\"(a) : : out);\n\
               return z;\n\
               out:\n\
               return z + 2;\n\
               }\n";
    let a64 = src.replace("cmp $0, %1\\n\\tje", "cmp %1, #0\\n\\tb.eq");
    let a64 = a64.replace("=@ccz", "=@cceq");
    for opt in OPTS {
        let asm = expect_accepted("asm_flag_goto", src, X86, opt);
        assert_eq!(asm.matches("setz").count(), 2, "{opt}:\n{asm}");
        assert!(asm.contains("je .L.asm_goto_flags."), "{opt}:\n{asm}");
        let asm = expect_accepted("asm_flag_goto_a64", &a64, A64, opt);
        assert_eq!(asm.matches(", eq\n").count(), 2, "{opt}:\n{asm}");
        assert!(asm.contains("b.eq .L.asm_goto_flags."), "{opt}:\n{asm}");
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
