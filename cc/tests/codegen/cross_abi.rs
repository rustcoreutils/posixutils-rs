//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Cross-target ABI assertions, made against generated assembly.
//
// Every other codegen test compiles and *runs* a program, so it can only ever
// check the host's architecture. That left the AArch64 calling convention
// covered by nothing but macOS CI — and two defects lived there through a full
// audit: a complex parameter took a general-purpose register and left the FP
// argument index untouched, so the next floating-point parameter was read from
// the register holding the complex value's real part; and a complex parameter
// was loaded from an incoming *pointer* that no longer existed once the value
// arrived in registers, overwriting what the prologue had just stored.
//
// `--target` lets any host emit for either architecture, so these run
// everywhere.
//

use super::asm_probe::{asm_for, body_of};

/// AAPCS64 passes a `_Complex` as a two-element HFA, so it occupies **two**
/// V registers and the next floating-point parameter starts after both.
/// Classifying it as anything else takes a general-purpose register instead
/// and leaves the FP index where it was, so `after` gets read from V0 — the
/// register holding the complex value's real part.
#[test]
fn codegen_aarch64_complex_param_consumes_two_fp_registers() {
    let src = r#"
        int check_f(float _Complex z, float after) {
            float *f = (float *)&z;
            return (f[0] == 1.0f && f[1] == 2.0f && after == 9.0f) ? 0 : 1;
        }
        int check_d(double _Complex z, double after) {
            double *d = (double *)&z;
            return (d[0] == 1.0 && d[1] == 2.0 && after == 9.0) ? 0 : 1;
        }
    "#;

    for triple in ["aarch64-apple-darwin", "aarch64-unknown-linux-gnu"] {
        let asm = asm_for("two_fp_regs", triple, src);

        let f = body_of(&asm, "check_f");
        assert!(
            f.contains("s2"),
            "on {}, the float after a `float _Complex` must come from S2 \
             (the complex occupies S0 and S1); it is never mentioned:\n{}",
            triple,
            f
        );
        let d = body_of(&asm, "check_d");
        assert!(
            d.contains("d2"),
            "on {}, the double after a `double _Complex` must come from D2:\n{}",
            triple,
            d
        );
    }
}

/// A complex parameter that arrives in registers has exactly one source: the
/// prologue's stores. Treating it as memory-class as well made the body reload
/// it from an incoming pointer that does not exist, so it read whatever was in
/// X0 and overwrote the registers the prologue had just saved.
///
/// Apple's `long double` is a `double`, so all three complex types arrive in
/// V registers there; only a genuinely indirect parameter may be dereferenced.
#[test]
fn codegen_aarch64_register_complex_param_is_not_reloaded_from_a_pointer() {
    let src = r#"
        float re_f(float _Complex z) { float *p = (float *)&z; return p[0]; }
        double re_d(double _Complex z) { double *p = (double *)&z; return p[0]; }
        long double re_l(long double _Complex z) {
            long double *p = (long double *)&z; return p[0];
        }
    "#;
    let asm = asm_for("no_ptr_reload", "aarch64-apple-darwin", src);

    for func in ["re_f", "re_d", "re_l"] {
        let body = body_of(&asm, func);
        assert!(
            body.contains("str s0") || body.contains("str d0"),
            "{}: the prologue must store the incoming V register:\n{}",
            func,
            body
        );
        assert!(
            !body.contains("[x0]") && !body.contains("[x0,"),
            "{}: a register-passed complex must not be reloaded through X0 — \
             that register holds no argument here:\n{}",
            func,
            body
        );
    }
}

/// The x86_64 side of the same classification, so the two conventions are
/// pinned against each other. `long double _Complex` is COMPLEX_X87 there:
/// memory for arguments, st(0)/st(1) for returns — never XMM.
#[test]
fn codegen_x86_64_long_double_complex_uses_x87() {
    let src = r#"
        long double _Complex mk(void) { return __builtin_complex(6.0L, 7.0L); }
    "#;
    let asm = asm_for("x87_return", "x86_64-unknown-linux-gnu", src);
    let body = body_of(&asm, "mk");
    assert!(
        body.contains("fldt") || body.contains("fstpt"),
        "a long double _Complex return must go through the x87 stack:\n{}",
        body
    );
    assert!(
        !body.contains("%xmm"),
        "a long double _Complex has no XMM form — this is what produced \
         `movt %xmm0`:\n{}",
        body
    );
}

/// A `_Complex` that runs out of V registers is laid on the stack, and both
/// sides have to agree about it (#H13).
///
/// The caller used to push the argument pseudo twice and move it with
/// `emit_fp_move`, which for a complex pseudo -- holding the value's *address*
/// -- wrote the pointer's bit pattern into both halves. The callee's prologue
/// simply skipped the copy, leaving the parameter uninitialized.
///
/// Runs from any host via `--target`.
#[test]
fn codegen_aarch64_stacked_complex_argument_is_dereferenced() {
    let src = r#"
        double _Complex sink(double _Complex a, double _Complex b,
                             double _Complex c, double _Complex d,
                             double _Complex e);
        double _Complex call5(double _Complex a, double _Complex b,
                              double _Complex c, double _Complex d,
                              double _Complex e) {
            return sink(a, b, c, d, e);
        }
    "#;
    let asm = super::asm_probe::asm_for("stacked_complex", "aarch64-unknown-linux-gnu", src);
    let body = super::asm_probe::body_of(&asm, "call5");

    // The fifth complex goes to the outgoing stack area, written as two
    // separate elements loaded out of the value's address.
    assert!(
        body.contains("[sp]") || body.contains("[sp, #0]"),
        "the stacked complex must be written to the outgoing area:\n{body}"
    );
    assert!(
        body.contains("ldr d") || body.contains("ldr s"),
        "both elements must be loaded from the value's address:\n{body}"
    );
    // Writing the address itself into the slot is the bug this pins.
    assert!(
        !body.contains("fmov d16, x"),
        "the argument's address must not be stored as if it were the value:\n{body}"
    );
}

/// AAPCS64 §6.4.2: once any argument is laid out on the stack, NSRN is 8, so
/// every later floating-point argument goes to the stack too -- the registers
/// the over-large argument did not fit into are *not* reused. System V does the
/// opposite, which is why the x86_64 fix could not simply be copied.
#[test]
fn codegen_aarch64_nsrn_saturates_after_a_stacked_argument() {
    let src = r#"
        void sink(double a, double b, double c, double d,
                  double e, double f, double g,
                  double _Complex z, double after);
        void call(double _Complex z) {
            sink(1, 2, 3, 4, 5, 6, 7, z, 9.0);
        }
    "#;
    let asm = super::asm_probe::asm_for("nsrn_saturate", "aarch64-unknown-linux-gnu", src);
    let body = super::asm_probe::body_of(&asm, "call");

    // Seven doubles consume d0-d6. The complex needs two registers and only d7
    // remains, so it is stacked -- and `after` must then be stacked as well
    // rather than taking the free d7.
    assert!(
        !body.contains("d7,"),
        "no floating-point argument may use d7 after an argument is stacked:\n{body}"
    );
}
