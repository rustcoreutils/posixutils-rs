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
