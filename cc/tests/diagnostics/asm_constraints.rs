//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Inline-asm constraints c17 used to accept silently.
//
// An immediate-only operand (`"i"`, `"n"`, `"s"`, aarch64 `"S"`) must be a
// constant of the kind its letter takes when the statement is emitted --
// after optimization, as gcc decides, so a literal inlined into a `static
// inline` helper's `"i"(param)` satisfies it at -O2 but not at -O0. c17
// substituted whatever register held the operand instead. A constraint letter
// or sequence c17 does not model -- a flag output `"=@ccz"`, x86-64 `"Yz"`,
// aarch64 `"Ump"` -- was read as nothing, leaving the operand to whatever the
// rest of the string said. Each case here is checked against gcc's verdict;
// every test compiles for a named target, so the letters mean the same on
// any host.
//

use crate::common::{compile_rejected_with, create_c_file, run_c17};

const X86: [&str; 2] = ["--target", "x86_64-unknown-linux-gnu"];
const A64: [&str; 2] = ["--target", "aarch64-unknown-linux-gnu"];
const OPTS: [&str; 2] = ["-O0", "-O2"];

/// Require `src` to be rejected for `target` at `opt` with a diagnostic
/// containing `wanted`, reported on line `line`.
fn expect_rejected(name: &str, src: &str, target: [&str; 2], opt: &str, wanted: &str, line: u32) {
    let stderr = compile_rejected_with(&format!("{name}{opt}"), src, &[target[0], target[1], opt]);
    assert!(
        stderr.contains(wanted),
        "{name} {} {opt}: expected {wanted:?}, got:\n{stderr}",
        target[1]
    );
    assert!(
        stderr.contains(&format!(":{line}:")),
        "{name} {} {opt}: expected the report on line {line}, got:\n{stderr}",
        target[1]
    );
}

/// Compile `src` for `target` at `opt`, require it to be accepted, and return
/// the assembly.
fn expect_accepted(name: &str, src: &str, target: [&str; 2], opt: &str) -> String {
    let c = create_c_file(&format!("{name}{opt}"), src);
    let path = c.path().to_string_lossy().to_string();
    let out = plib::tmp::Builder::new()
        .prefix(&format!("c17_asmc_{name}_"))
        .suffix(".s")
        .tempfile()
        .expect("temp file");
    let out_path = out.path().to_string_lossy().to_string();
    let run = run_c17(&[target[0], target[1], opt, "-S", "-o", &out_path, &path]);
    assert!(
        run.success,
        "{name} {} {opt} should compile:\n{}",
        target[1], run.stderr
    );
    std::fs::read_to_string(&out_path).expect("read assembly")
}

const IMPOSSIBLE: &str = "impossible constraint in 'asm'";

/// A variable under an immediate-only constraint is rejected at every level:
/// nothing makes a parameter constant.
#[test]
fn asm_immediate_operand_that_is_a_variable() {
    let src = "int g;\n\
               void f(int y) {\n\
               __asm__ volatile(\"# %0\" :: \"i\"(y));\n\
               }\n";
    let n = src.replace("\"i\"", "\"n\"");
    for opt in OPTS {
        expect_rejected("asm_i_var", src, X86, opt, IMPOSSIBLE, 3);
        expect_rejected("asm_n_var", &n, X86, opt, IMPOSSIBLE, 3);
        expect_rejected("asm_i_var_a64", src, A64, opt, IMPOSSIBLE, 3);
        expect_rejected("asm_n_var_a64", &n, A64, opt, IMPOSSIBLE, 3);
    }
    // The message says what the operand must be.
    let stderr = compile_rejected_with("asm_n_var_msg", &n, &X86);
    assert!(
        stderr.contains("operand 0 must be an integer constant for \"n\""),
        "{stderr}"
    );
}

/// The Linux-kernel idiom: a `static inline` helper hands its parameter to
/// `"i"`. Inlined with a literal at -O2 the operand is a constant, and gcc
/// accepts it; at -O0 the helper is called, not inlined, and gcc rejects it.
/// Called with a variable, it is rejected at -O2 as well.
#[test]
fn asm_immediate_operand_through_an_inlined_parameter() {
    let helper = "static inline void put(int v) {\n\
                  __asm__ volatile(\"# %0\" :: \"i\"(v));\n\
                  }\n";
    let literal = format!("{helper}void f(void) {{ put(3); put(400); }}\n");
    for target in [X86, A64] {
        expect_rejected("asm_inl_lit", &literal, target, "-O0", IMPOSSIBLE, 2);
        let asm = expect_accepted("asm_inl_lit", &literal, target, "-O2");
        let (three, four_hundred) = if target == X86 {
            ("# $3", "# $400")
        } else {
            ("# #3", "# #400")
        };
        assert!(
            asm.contains(three) && asm.contains(four_hundred),
            "{}: the literals were not substituted:\n{asm}",
            target[1]
        );
    }
    let variable = format!("{helper}void f(int y) {{ put(y); }}\n");
    for opt in OPTS {
        expect_rejected("asm_inl_var", &variable, X86, opt, IMPOSSIBLE, 2);
    }
}

/// A `static inline` helper nothing calls is never emitted, by gcc at any
/// level, so its `"i"(param)` is never judged. c17 emitted one at -O0 and
/// would have rejected it.
#[test]
fn asm_immediate_operand_in_an_unused_static_inline_helper() {
    let src = "static inline void put(int v) {\n\
               __asm__ volatile(\"# %0\" :: \"i\"(v));\n\
               }\n\
               int main(void) { return 0; }\n";
    for opt in OPTS {
        let asm = expect_accepted("asm_unused_helper", src, X86, opt);
        assert!(
            !asm.contains("put:"),
            "an unused static inline was emitted:\n{asm}"
        );
    }
}

/// `n` takes an integer and nothing else; x86-64 `s` takes a symbolic address
/// and never an integer. gcc rejects both mismatches.
#[test]
fn asm_immediate_operand_of_the_wrong_kind() {
    let cases: &[(&str, &str, &str)] = &[
        ("asm_n_sym", "\"n\"(&g)", "an integer constant"),
        ("asm_n_float", "\"n\"(1.0)", "an integer constant"),
        ("asm_s_int", "\"s\"(5)", "a symbolic address constant"),
    ];
    for &(name, operand, what) in cases {
        let src = format!(
            "int g;\n\
             void f(void) {{\n\
             __asm__ volatile(\"# %0\" :: {operand});\n\
             }}\n"
        );
        for opt in OPTS {
            expect_rejected(name, &src, X86, opt, IMPOSSIBLE, 3);
            expect_rejected(name, &src, X86, opt, what, 3);
        }
    }
}

/// aarch64 code is position-independent by default, and there gcc writes a
/// symbol only for `S`: `"i"(&g)` is rejected, and `S` takes no integer.
#[test]
fn asm_aarch64_symbolic_constants_need_s() {
    for (name, operand) in [
        ("asm_a64_i_sym", "\"i\"(&g)"),
        ("asm_a64_s_int", "\"S\"(5)"),
    ] {
        let src = format!(
            "int g;\n\
             void f(void) {{\n\
             __asm__ volatile(\"// %0\" :: {operand});\n\
             }}\n"
        );
        for opt in OPTS {
            expect_rejected(name, &src, A64, opt, IMPOSSIBLE, 3);
        }
    }
}

/// What must still be accepted at -O0, where nothing is propagated: an
/// enumeration constant, `sizeof`, any integer constant expression, a
/// global's address with or without an offset, a function, a string literal
/// -- written as gcc writes them.
#[test]
fn asm_immediate_operands_gcc_accepts() {
    let src = "int g; long x[2]; void fn(void) {}\n\
               enum { E = 5 };\n\
               void f(void) {\n\
               __asm__ volatile(\"# A %0 %1 %2 %3 %4\" :: \"i\"(E), \"n\"(sizeof(long)),\n\
               \"n\"(~5), \"i\"(-1), \"i\"(3 * 4 + 1));\n\
               __asm__ volatile(\"# B %0 %1 %2 %3\" :: \"i\"(&g), \"i\"(&x[1]), \"s\"(fn),\n\
               \"i\"(&g - 3));\n\
               }\n";
    for opt in OPTS {
        let asm = expect_accepted("asm_imm_ok", src, X86, opt);
        for want in ["# A $5 $8 $-6 $-1 $13", "# B $g $x+8 $fn $g-12"] {
            assert!(asm.contains(want), "{opt}: expected {want:?} in:\n{asm}");
        }
    }
    let a64 = "int g; long x[2];\n\
               void f(void) {\n\
               __asm__ volatile(\"// A %0 %1 %2\" :: \"S\"(&g), \"S\"(&x[1]), \"n\"(sizeof(long)));\n\
               }\n";
    for opt in OPTS {
        let asm = expect_accepted("asm_imm_ok_a64", a64, A64, opt);
        assert!(asm.contains("// A g x+8 #8"), "{opt}:\n{asm}");
    }
}

/// gcc's `"i#*X"`: `#` hides the rest of the alternative, so the operand is
/// immediate-only -- and the asm sits behind `__builtin_constant_p`, which
/// must have removed it wherever the operand is not a constant.
#[test]
fn asm_hash_hides_the_rest_of_the_alternative() {
    let src = "extern void doit(int);\n\
               void quick_doit(int x) {\n\
               if (__builtin_constant_p(x) && x != 0)\n\
               __asm__ volatile(\"# %0\" :: \"i#*X\"(x));\n\
               else\n\
               doit(x);\n\
               }\n";
    for opt in ["-O1", "-O2"] {
        expect_accepted("asm_hash", src, X86, opt);
    }
}

/// A constraint c17 does not model is an error naming it. gcc implements
/// these; no corpus c17 builds uses them, so they are rejected rather than
/// read as some other class.
#[test]
fn asm_unsupported_constraints() {
    let x86: &[(&str, &str, &str)] = &[
        (
            "asm_flag_out",
            ":\"=@ccz\"(r)",
            "unsupported flag output constraint \"=@ccz\"",
        ),
        (
            "asm_yz",
            "::\"Yz\"(1.0)",
            "unsupported constraint 'Yz' in asm operand \"Yz\"",
        ),
        ("asm_p", "::\"p\"(&r)", "unsupported constraint 'p'"),
        ("asm_mmx", "::\"y\"(r)", "unsupported constraint 'y'"),
        ("asm_space", "::\"r \"(r)", "unsupported constraint ' '"),
    ];
    for &(name, operands, wanted) in x86 {
        let src = format!(
            "int r;\n\
             void f(void) {{\n\
             __asm__ volatile(\"# %0\" {operands});\n\
             }}\n"
        );
        for opt in OPTS {
            expect_rejected(name, &src, X86, opt, wanted, 3);
        }
    }
    let a64: &[(&str, &str, &str)] = &[
        (
            "asm_a64_flag",
            ":\"=@cceq\"(r)",
            "unsupported flag output constraint",
        ),
        (
            "asm_a64_ump",
            "::\"Ump\"(r)",
            "unsupported constraint 'Ump'",
        ),
        ("asm_a64_k", "::\"k\"(r)", "unsupported constraint 'k'"),
    ];
    for &(name, operands, wanted) in a64 {
        let src = format!(
            "int r;\n\
             void f(void) {{\n\
             __asm__ volatile(\"// %0\" {operands});\n\
             }}\n"
        );
        for opt in OPTS {
            expect_rejected(name, &src, A64, opt, wanted, 3);
        }
    }
}

/// A constraint at odds with its side of the colon, in gcc's words.
#[test]
fn asm_constraint_on_the_wrong_side() {
    let cases: &[(&str, &str, &str)] = &[
        (
            "asm_out_no_eq",
            ":\"r\"(r)",
            "output operand constraint lacks '='",
        ),
        ("asm_out_imm", ":\"=i\"(r)", IMPOSSIBLE),
        (
            "asm_out_tied",
            ":\"=0\"(r)",
            "matching constraint not valid in output operand",
        ),
        (
            "asm_in_eq",
            "::\"=r\"(r)",
            "input operand constraint contains '='",
        ),
        (
            "asm_in_plus",
            "::\"+r\"(r)",
            "input operand constraint contains '+'",
        ),
        (
            "asm_in_amp",
            "::\"&r\"(r)",
            "input operand constraint contains '&'",
        ),
        (
            "asm_in_tied_range",
            ":\"=r\"(r):\"1\"(r)",
            "matching constraint references invalid operand number",
        ),
    ];
    for &(name, operands, wanted) in cases {
        let src = format!(
            "int r;\n\
             void f(void) {{\n\
             __asm__ volatile(\"\" {operands});\n\
             }}\n"
        );
        expect_rejected(name, &src, X86, "-O0", wanted, 3);
    }
}
