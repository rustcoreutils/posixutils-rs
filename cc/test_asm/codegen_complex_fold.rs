//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Assembly cases of tests/codegen/complex_fold.rs, in process: floating
// complex `*` and `/` of constants, folded by the optimizer.
//
// Each is a call to libgcc's `__mul?c3` or `__div?c3`, which the optimizer
// computes when its operands are constant, and which stays a call wherever
// an operand or the result is an infinity or a NaN.
//

use crate::test_asm::asm_probe::{asm_for_with, AARCH64_DARWIN, AARCH64_LINUX, X86_64_LINUX};

/// A constant product and quotient in every floating format, at `TY`.
const CONSTANT: &str = "
    TY _Complex mul(void) {
        TY _Complex a = __builtin_complex((TY)1.0, (TY)2.0);
        TY _Complex b = __builtin_complex((TY)3.0, (TY)-1.0);
        return a * b;
    }
    TY _Complex quo(void) {
        TY _Complex a = __builtin_complex((TY)1.0, (TY)1.0);
        TY _Complex b = __builtin_complex((TY)3.0, (TY)7.0);
        return a / b;
    }
    TY _Complex both(void) {
        TY _Complex a = __builtin_complex((TY)0.1, (TY)0.2), b = __builtin_complex((TY)3.0, (TY)0.5);
        a *= b;
        return a / b;
    }
";

/// The formats each target computes complex `*` and `/` in, and the routine
/// each calls at run time.
const FORMATS: [(&str, &str, &str); 10] = [
    (X86_64_LINUX, "float", "sc3"),
    (X86_64_LINUX, "double", "dc3"),
    (X86_64_LINUX, "long double", "xc3"),
    (X86_64_LINUX, "_Float16", "sc3"),
    (X86_64_LINUX, "_Float128", "tc3"),
    (AARCH64_LINUX, "float", "sc3"),
    (AARCH64_LINUX, "double", "dc3"),
    (AARCH64_LINUX, "long double", "tc3"),
    (AARCH64_LINUX, "_Float16", "sc3"),
    (AARCH64_LINUX, "_Float128", "tc3"),
];

/// With the optimizer, no routine is called for constant operands; the
/// `_Float16` ones widen to `float` for `__mulsc3`, and on x86-64 their
/// conversions are calls too, which must not stand in the way.
#[test]
fn constant_complex_mul_and_div_leave_no_call() {
    for (triple, ty, routine) in FORMATS {
        let src = CONSTANT.replace("TY", ty);
        for opt in ["-O1", "-O2"] {
            let asm = asm_for_with("cfold", triple, &src, &[opt]);
            for callee in ["__mul", "__div", "__extendhf", "__trunc"] {
                assert!(
                    !asm.contains(callee),
                    "{triple} {ty} {opt}: {callee} should have folded away:\n{asm}"
                );
            }
        }
        let asm = asm_for_with("cfold_o0", triple, &src, &["-O0"]);
        for op in ["__mul", "__div"] {
            assert!(
                asm.contains(&format!("{op}{routine}")),
                "{triple} {ty} -O0: {op}{routine} is the program's own call:\n{asm}"
            );
        }
    }
}

/// An infinite or NaN operand, a zero divisor and a product that overflows
/// are left to the routine, which raises what they raise.
#[test]
fn what_the_routine_must_compute_stays_a_call() {
    let src = "
        double _Complex by_inf(void) {
            double _Complex a = __builtin_complex(__builtin_inf(), 1.0);
            return a * __builtin_complex(1.0, 1.0);
        }
        double _Complex by_nan(void) {
            double _Complex a = __builtin_complex(__builtin_nan(\"\"), 1.0);
            return a / __builtin_complex(1.0, 1.0);
        }
        double _Complex by_zero(void) {
            double _Complex a = __builtin_complex(1.0, 1.0);
            return a / __builtin_complex(0.0, 0.0);
        }
        double _Complex overflows(void) {
            double _Complex a = __builtin_complex(1e300, 1e300);
            return a * a;
        }
    ";
    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        let asm = asm_for_with("cfold_special", triple, src, &["-O2"]);
        for callee in ["__muldc3", "__divdc3"] {
            assert!(
                asm.matches(callee).count() >= 2,
                "{triple}: {callee} must stay for each special operand:\n{asm}"
            );
        }
    }
    // Darwin's routines are compiler-rt's, which c17 models beside
    // libgcc's, so a constant folds there as it does anywhere else.
    let src = CONSTANT.replace("TY", "double");
    let asm = asm_for_with("cfold_darwin", AARCH64_DARWIN, &src, &["-O2"]);
    for callee in ["___muldc3", "___divdc3"] {
        assert!(
            !asm.contains(callee),
            "Darwin folds a constant rather than calling {callee}:\n{asm}"
        );
    }
}

/// x86-64 has no half-precision instructions, and its `_Float16`
/// arithmetic, comparisons and conversions are libgcc calls -- made after the
/// optimizer, which folds the constant ones first. Without the optimizer
/// they are still the calls.
#[test]
fn float16_constants_fold_before_the_libcalls_on_x86_64() {
    let src = "
        extern void link_error(void);
        _Float16 h(void) { return (_Float16)1.5 * (_Float16)3.0 - (_Float16)0.25; }
        void f(void) {
            if ((_Float16)1.0 + (_Float16)2.0 != (_Float16)3.0) link_error();
            if ((float)((_Float16)1.0 / (_Float16)3.0) != 0x1.554p-2f) link_error();
            if ((int)(_Float16)7.75 != 7) link_error();
            if (!((_Float16)-1.0 < (_Float16)0.0)) link_error();
        }
    ";
    for opt in ["-O1", "-O2"] {
        let asm = asm_for_with("f16_fold", X86_64_LINUX, src, &[opt]);
        for callee in ["link_error", "__extendhf", "__trunc"] {
            assert!(
                !asm.contains(callee),
                "{opt}: {callee} should have folded away:\n{asm}"
            );
        }
    }
    let asm = asm_for_with("f16_o0", X86_64_LINUX, src, &["-O0"]);
    for callee in ["__extendhfsf2", "__truncsfhf2"] {
        assert!(asm.contains(callee), "-O0 calls {callee}:\n{asm}");
    }
}
