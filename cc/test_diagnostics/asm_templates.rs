//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Inline-asm templates and immediates c17 used to pass through silently.
//
// An operand modifier c17 did not know (`%c0`, `%z0`), an operand that does
// not exist (`%3`, `%[nope]`) and a stray `%` all reached the assembler as
// text, which it rejected with a message about the assembly, or accepted as
// something else. A constant outside the range its constraint letter takes
// (`"I"(32)` on x86-64) was written into an instruction that could not
// encode it. Each is now an error at the statement, checked against gcc.
//

use super::asm_constraints::{expect_accepted, expect_rejected, A64, IMPOSSIBLE, OPTS, X86};

/// `int g; long v; double dd;` and `f(long x)` around one asm statement, on
/// line 3.
fn statement(body: &str) -> String {
    format!(
        "int g; long v; double dd;\n\
         void f(long x) {{\n\
         __asm__ volatile({body});\n\
         }}\n"
    )
}

/// Modifiers c17 does not implement -- none in the target corpora -- are
/// rejected by name, never passed through.
#[test]
fn asm_unsupported_operand_modifiers() {
    let cases: &[(&str, [&str; 2], &str, &str)] = &[
        ("asm_mod_y", X86, r##""# %y0" :: "r"(x)"##, "'%y'"),
        ("asm_mod_h_mem", X86, r##""# %H0" :: "m"(v)"##, "'%H'"),
        ("asm_mod_r_sse", X86, r##""# %R0" :: "x"(dd)"##, "'%R'"),
        ("asm_mod_a64_h", A64, r##""// %H0" :: "r"(x)"##, "'%H'"),
        ("asm_mod_a64_q", A64, r##""// %Q0" :: "r"(x)"##, "'%Q'"),
    ];
    for &(name, target, body, modifier) in cases {
        let wanted = format!("unsupported operand modifier {modifier}");
        expect_rejected(name, &statement(body), target, "-O0", &wanted, 3);
    }
}

/// A modifier that exists but does not apply to the operand, as gcc rejects
/// it: `%c` of a register, `%a` of memory, `%n` of a register, `%V` of an
/// SSE register, aarch64 `%d` of a general register.
#[test]
fn asm_modifier_on_the_wrong_kind_of_operand() {
    let cases: &[(&str, [&str; 2], &str, &str)] = &[
        (
            "asm_mod_c_reg",
            X86,
            r##""# %c0" :: "r"(x)"##,
            "'%c' does not apply to a general register",
        ),
        (
            "asm_mod_x_reg",
            X86,
            r##""# %x0" :: "r"(x)"##,
            "'%x' does not apply to a general register",
        ),
        (
            "asm_mod_a_mem",
            X86,
            r##""# %a0" :: "m"(v)"##,
            "'%a' does not apply to a memory reference",
        ),
        (
            "asm_mod_n_reg",
            X86,
            r##""# %n0" :: "r"(x)"##,
            "'%n' does not apply to a general register",
        ),
        (
            "asm_mod_v_sse",
            X86,
            r##""# %V0" :: "x"(dd)"##,
            "'%V' does not apply to a floating-point or vector register",
        ),
        (
            "asm_mod_c_float",
            X86,
            r##""# %c0" :: "i"(1.5)"##,
            "'%c' does not apply to a floating constant",
        ),
        (
            "asm_mod_a64_d_gp",
            A64,
            r##""// %d0" :: "r"(x)"##,
            "'%d' does not apply to a general register",
        ),
        (
            "asm_mod_a64_c_reg",
            A64,
            r##""// %c0" :: "r"(x)"##,
            "'%c' does not apply to a general register",
        ),
        (
            "asm_mod_a64_a_mem",
            A64,
            r##""// %a0" :: "m"(v)"##,
            "'%a' does not apply to a memory reference",
        ),
    ];
    for &(name, target, body, wanted) in cases {
        for opt in OPTS {
            expect_rejected(name, &statement(body), target, opt, wanted, 3);
        }
    }
}

/// What follows a `%` must name an operand: gcc rejects each of these.
#[test]
fn asm_template_references_that_name_nothing() {
    let cases: &[(&str, &str, &str)] = &[
        (
            "asm_tpl_reg",
            r##""movl %eax, %0" :: "r"(x)"##,
            "operand number missing after '%e'",
        ),
        (
            "asm_tpl_range",
            r##""# %3" :: "r"(x)"##,
            "asm operand number 3 out of range",
        ),
        (
            "asm_tpl_name",
            r##""# %[nope]" :: "r"(x)"##,
            "undefined named asm operand 'nope'",
        ),
        (
            "asm_tpl_bang",
            r##""# %!" :: "r"(x)"##,
            "invalid '%!' in asm template",
        ),
        (
            "asm_tpl_label",
            r##""# %l0" :: "r"(x)"##,
            "asm operand 0 named by '%l' is not a label",
        ),
        (
            "asm_tpl_end",
            r##""# %" :: "r"(x)"##,
            "'%' at the end of an asm template",
        ),
        // x86 dialect alternatives that do not close, or nest, as gcc says.
        (
            "asm_tpl_dialect_open",
            r##""mov{q %0, %%rax" :: "r"(x)"##,
            "unterminated assembly dialect alternative",
        ),
        (
            "asm_tpl_dialect_nested",
            r##""mov{q {a} %0, %%rax|x}" :: "r"(x)"##,
            "nested assembly dialect alternatives",
        ),
        // No operands at all is still extended asm.
        (
            "asm_tpl_no_ops",
            r##""movl %eax, %ebx" ::"##,
            "operand number missing after '%e'",
        ),
    ];
    for &(name, body, wanted) in cases {
        expect_rejected(name, &statement(body), X86, "-O0", wanted, 3);
    }
}

/// Basic asm -- no colon -- is emitted as written, as gcc emits it: nothing
/// is substituted, and `%%` stays `%%`.
#[test]
fn asm_basic_template_is_verbatim() {
    let src = statement(r##""# basic %eax %%ebx %0 {x|y} %=""##);
    for target in [X86, A64] {
        let asm = expect_accepted("asm_basic", &src, target, "-O0");
        assert!(
            asm.contains("# basic %eax %%ebx %0 {x|y} %="),
            "{}:\n{asm}",
            target[1]
        );
    }
}

/// An immediate outside every range its letters take is "impossible", as in
/// gcc 13; the constant is sign-extended from the operand's width first, so
/// `(unsigned char)200` is -56 to `N`.
#[test]
fn asm_immediates_out_of_range() {
    let cases: &[(&str, [&str; 2], &str, &str)] = &[
        (
            "asm_rng_x_i",
            X86,
            r##""# %0" :: "I"(32)"##,
            "operand 0 (32) is out of range for \"I\"",
        ),
        (
            "asm_rng_x_j",
            X86,
            r##""# %0" :: "J"(64)"##,
            "(64) is out of range",
        ),
        (
            "asm_rng_x_k",
            X86,
            r##""# %0" :: "K"(128)"##,
            "(128) is out of range",
        ),
        (
            "asm_rng_x_l",
            X86,
            r##""# %0" :: "L"(0xfe)"##,
            "(254) is out of range",
        ),
        (
            "asm_rng_x_m",
            X86,
            r##""# %0" :: "M"(4)"##,
            "(4) is out of range",
        ),
        (
            "asm_rng_x_n",
            X86,
            r##""# %0" :: "N"((unsigned char)200)"##,
            "(-56) is out of range",
        ),
        (
            "asm_rng_x_o",
            X86,
            r##""# %0" :: "O"(128)"##,
            "(128) is out of range",
        ),
        (
            "asm_rng_x_e",
            X86,
            r##""# %0" :: "e"(0x80000000L)"##,
            "is out of range",
        ),
        (
            "asm_rng_x_z",
            X86,
            r##""# %0" :: "Z"(-1)"##,
            "(-1) is out of range",
        ),
        (
            "asm_rng_x_mn",
            X86,
            r##""# %0" :: "mN"(300)"##,
            "fits no alternative of \"mN\"",
        ),
        (
            "asm_rng_a_i",
            A64,
            r##""// %0" :: "I"(4097)"##,
            "(4097) is out of range",
        ),
        (
            "asm_rng_a_j",
            A64,
            r##""// %0" :: "J"(1)"##,
            "(1) is out of range",
        ),
        (
            "asm_rng_a_k",
            A64,
            r##""// %0" :: "K"(0x1234)"##,
            "(4660) is out of range",
        ),
        (
            "asm_rng_a_l",
            A64,
            r##""// %0" :: "L"(0L)"##,
            "(0) is out of range",
        ),
        (
            "asm_rng_a_m",
            A64,
            r##""// %0" :: "M"(0x12345678)"##,
            "is out of range",
        ),
        (
            "asm_rng_a_n",
            A64,
            r##""// %0" :: "N"(0xff00ff00L)"##,
            "is out of range",
        ),
        (
            "asm_rng_a_z",
            A64,
            r##""// %0" :: "Z"(1)"##,
            "(1) is out of range",
        ),
        (
            "asm_rng_a_y",
            A64,
            r##""// %0" :: "Y"(1.0)"##,
            "out of range for \"Y\"",
        ),
        (
            "asm_rng_a_mi",
            A64,
            r##""// %0" :: "mI"(5000)"##,
            "fits no alternative of \"mI\"",
        ),
    ];
    for &(name, target, body, wanted) in cases {
        for opt in OPTS {
            let src = statement(body);
            expect_rejected(name, &src, target, opt, IMPOSSIBLE, 3);
            expect_rejected(name, &src, target, opt, wanted, 3);
        }
    }
}

/// The bounds themselves are accepted, and written as gcc writes them.
#[test]
fn asm_immediates_at_the_bounds() {
    let x86 = statement(
        r##""# %0 %1 %2 %3 %4 %5" :: "I"(31), "K"(-128), "L"(0xffffffffUL),
           "N"((unsigned char)127), "e"(0xffffffffu), "Z"(0xffffffffUL)"##,
    );
    for opt in OPTS {
        let asm = expect_accepted("asm_bounds_x86", &x86, X86, opt);
        let want = "# $31 $-128 $4294967295 $127 $-1 $4294967295";
        assert!(asm.contains(want), "{opt}: expected {want:?} in:\n{asm}");
    }
    let a64 = statement(
        r##""// %0 %1 %2 %3 %4 %5 %6" :: "I"(0xfff000), "J"(-4095), "K"(0x55555555),
           "L"(0xff00ff00ff00ff00UL), "M"(0xffff1234u), "N"(-2L), "Z"(0)"##,
    );
    for opt in OPTS {
        let asm = expect_accepted("asm_bounds_a64", &a64, A64, opt);
        let want = "// 16773120 -4095 1431655765 -71777214294589696 -60876 -2 0";
        assert!(asm.contains(want), "{opt}: expected {want:?} in:\n{asm}");
    }
}

/// aarch64 writes a floating immediate as gcc does: `0` for positive zero,
/// decimal for a value `fmov` encodes, and nothing else.
#[test]
fn asm_aarch64_floating_immediates() {
    let ok = statement(r##""// %0 %1 %2 %3" :: "i"(0.0), "i"(1.5), "i"(-0.125f), "Y"(0.0)"##);
    for opt in OPTS {
        let asm = expect_accepted("asm_a64_fimm", &ok, A64, opt);
        let want = "// 0 1.5e+0 -1.25e-1 0";
        assert!(asm.contains(want), "{opt}: expected {want:?} in:\n{asm}");
    }
    let bad = statement(r##""// %0" :: "i"(1.1)"##);
    for opt in OPTS {
        expect_rejected(
            "asm_a64_fimm_bad",
            &bad,
            A64,
            opt,
            "floating constant that cannot be written into the template",
            3,
        );
    }
}

/// An `asm goto` label is an operand numbered after the rest, hidden `"+"`
/// inputs included, and a bare `%N` writes it as a constant address, as gcc
/// does (gcc.c-torture/compile/pr98096.c).
#[test]
fn asm_goto_label_named_as_a_plain_operand() {
    let src = "int i;\n\
               int f(void) {\n\
               asm goto (\"# %0 %2\" : \"+r\" (i) ::: jmp);\n\
               i += 2;\n\
               jmp: return i;\n\
               }\n";
    for opt in OPTS {
        let x86 = expect_accepted("asm_goto_plain_x86", src, X86, opt);
        assert!(x86.contains(" $.Lf_"), "{opt}:\n{x86}");
        let a64 = expect_accepted("asm_goto_plain_a64", src, A64, opt);
        assert!(a64.contains(" .Lf_"), "{opt}:\n{a64}");
    }
}
