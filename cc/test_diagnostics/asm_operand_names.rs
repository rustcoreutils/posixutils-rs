//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Operand names in constraints, and the matching constraints they and the
// operand numbers spell. `"[sum]"` is the operand named `sum`, as gcc reads
// it; c17 refused it as an unknown constraint letter. A name given twice and
// a matching constraint gcc finds inconsistent were accepted, and an output
// two inputs were tied to was an internal error.
//

use super::asm_constraints::{expect_accepted, expect_rejected, A64, OPTS, X86};

/// `f(long a, long b)` around one asm statement, on line 3, that writes the
/// local `sum`.
fn statement(body: &str) -> String {
    format!(
        "long f(long a, long b) {{\n\
         long sum = 0;\n\
         __asm__ volatile({body});\n\
         return sum;\n\
         }}\n"
    )
}

/// gcc's report on a `[name]` that names nothing, a name given twice, and a
/// name that is not an output where a matching constraint wants one.
#[test]
fn asm_constraint_names_gcc_refuses() {
    let cases: &[(&str, &str, &str)] = &[
        (
            "asm_name_undef",
            r#""" : [sum] "=r"(sum) : "[nope]"(a)"#,
            "undefined named operand 'nope'",
        ),
        (
            "asm_name_empty",
            r#""" : [sum] "=r"(sum) : "[]"(a)"#,
            "undefined named operand ''",
        ),
        (
            "asm_name_open",
            r#""" : [sum] "=r"(sum) : "[sum"(a)"#,
            "missing close brace for named operand",
        ),
        (
            "asm_name_dup",
            r#""" : [sum] "=r"(sum) : [sum] "r"(a)"#,
            "duplicate 'asm' operand name 'sum'",
        ),
        (
            "asm_name_dup_out",
            r#""" : [sum] "=r"(sum), [sum] "=r"(a)"#,
            "duplicate 'asm' operand name 'sum'",
        ),
        (
            "asm_name_input",
            r#""" : [sum] "=r"(sum) : [x] "r"(a), "[x]"(b)"#,
            "matching constraint references invalid operand number",
        ),
    ];
    for &(name, body, wanted) in cases {
        for target in [X86, A64] {
            for opt in OPTS {
                expect_rejected(name, &statement(body), target, opt, wanted, 3);
            }
        }
    }
    // A label's name is an operand name too.
    let label = "long f(long a) {\n\
                 long sum = 0;\n\
                 __asm__ goto(\"\" : : [lab] \"r\"(a) : : lab);\n\
                 lab:\n\
                 return sum;\n\
                 }\n";
    expect_rejected(
        "asm_name_dup_label",
        label,
        X86,
        "-O0",
        "duplicate 'asm' operand name 'lab'",
        3,
    );
}

/// An output a matching constraint names may be tied to one input, and only
/// a register output: gcc refuses a second tie, a tie to a `"+"` output --
/// which has its own -- and a tie to a memory output, whether the input
/// names the output by number or by name.
#[test]
fn asm_inconsistent_matching_constraints() {
    const INCONSISTENT: &str = "inconsistent operand constraints in an 'asm'";
    let cases: &[(&str, &str)] = &[
        ("asm_tie_plus", r#""" : "+r"(sum) : "0"(a)"#),
        ("asm_tie_plus_named", r#""" : [sum] "+r"(sum) : "[sum]"(a)"#),
        ("asm_tie_twice", r#""" : "=r"(sum) : "0"(a), "0"(b)"#),
        (
            "asm_tie_twice_named",
            r#""" : [s] "=r"(sum) : "[s]"(a), "0"(b)"#,
        ),
        ("asm_tie_memory", r#""" : "=m"(sum) : "0"(a)"#),
        // Two values, though both read `a`.
        (
            "asm_tie_twice_values",
            r#""" : "=r"(sum) : "0"(a), "0"(a + 1)"#,
        ),
    ];
    for &(name, body) in cases {
        for target in [X86, A64] {
            for opt in OPTS {
                expect_rejected(name, &statement(body), target, opt, INCONSISTENT, 3);
            }
        }
    }
}

/// What gcc accepts: a name anywhere in a constraint, ties to distinct
/// outputs, a named input another input does not tie to, and two ties to one
/// output that read the same local variable -- one value, which gcc finds
/// consistent at every level.
#[test]
fn asm_constraint_names_gcc_accepts() {
    let bodies = [
        r#""" : [sum] "=r"(sum) : "[sum]"(a)"#,
        r#""" : "=r"(sum) : "0"(a), "0"(a)"#,
        r#""" : [s] "=r"(sum) : "[s]"(a), "0"(a)"#,
        r#""" : [sum] "=r"(sum) : "r[sum]"(a)"#,
        r#""" : [s] "=r"(sum), [t] "=r"(b) : "[t]"(a), "[s]"(b)"#,
        r#""" : [sum] "=r"(sum) : [x] "r"(a), "0"(b)"#,
    ];
    for (i, body) in bodies.iter().enumerate() {
        for target in [X86, A64] {
            for opt in OPTS {
                expect_accepted(&format!("asm_name_ok{i}"), &statement(body), target, opt);
            }
        }
    }
}
