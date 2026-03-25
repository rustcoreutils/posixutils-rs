//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 Complex Number Tests

use crate::common::compile_and_run;

#[test]
fn c99_complex_mega() {
    let code = r#"
#include <complex.h>

int main(void) {
    // ========== BASIC COMPLEX (returns 1-9) ==========
    {
        // complex macro expands to _Complex
        double complex z1 = __builtin_complex(3.0, 4.0);
        if (creal(z1) < 2.9 || creal(z1) > 3.1) return 1;
        if (cimag(z1) < 3.9 || cimag(z1) > 4.1) return 2;

        // I macro
        double complex z2 = I;
        if (cimag(z2) < 0.9 || cimag(z2) > 1.1) return 3;

        // 3 + 4i using I macro
        double complex z3 = 3.0 + 4.0 * I;
        if (creal(z3) < 2.9 || creal(z3) > 3.1) return 4;
        if (cimag(z3) < 3.9 || cimag(z3) > 4.1) return 5;

        // Real scalar init (imag = 0)
        double complex z4 = 5.0;
        if (creal(z4) < 4.9 || creal(z4) > 5.1) return 6;
        if (cimag(z4) < -0.1 || cimag(z4) > 0.1) return 7;
    }

    // ========== COMPLEX ARITHMETIC (returns 10-19) ==========
    {
        double complex a = __builtin_complex(1.0, 2.0);
        double complex b = __builtin_complex(3.0, 4.0);

        // Addition
        double complex sum = a + b;
        if (creal(sum) < 3.9 || creal(sum) > 4.1) return 10;
        if (cimag(sum) < 5.9 || cimag(sum) > 6.1) return 11;

        // Subtraction
        double complex diff = b - a;
        if (creal(diff) < 1.9 || creal(diff) > 2.1) return 12;
        if (cimag(diff) < 1.9 || cimag(diff) > 2.1) return 13;

        // Multiplication: (1+2i)(3+4i) = 3+4i+6i+8i² = 3+10i-8 = -5+10i
        double complex prod = a * b;
        if (creal(prod) < -5.1 || creal(prod) > -4.9) return 14;
        if (cimag(prod) < 9.9 || cimag(prod) > 10.1) return 15;

        // Division: (3+4i)/(1+2i) = (3+4i)(1-2i)/((1+2i)(1-2i)) = (11-2i)/5
        double complex quot = b / a;
        double qr = creal(quot);
        double qi = cimag(quot);
        if (qr < 2.1 || qr > 2.3) return 16;   // 11/5 = 2.2
        if (qi < -0.5 || qi > 0.1) return 17;    // -2/5 = -0.4
    }

    // ========== MIXED REAL + COMPLEX (returns 20-29) ==========
    {
        double complex z = __builtin_complex(2.0, 3.0);

        // Real + Complex
        double complex r1 = 10.0 + z;
        if (creal(r1) < 11.9 || creal(r1) > 12.1) return 20;
        if (cimag(r1) < 2.9 || cimag(r1) > 3.1) return 21;

        // Complex - Real
        double complex r2 = z - 1.0;
        if (creal(r2) < 0.9 || creal(r2) > 1.1) return 22;

        // Real * Complex
        double complex r3 = 2.0 * z;
        if (creal(r3) < 3.9 || creal(r3) > 4.1) return 23;
        if (cimag(r3) < 5.9 || cimag(r3) > 6.1) return 24;
    }

    // ========== CREAL / CIMAG (returns 30-39) ==========
    {
        double complex z = 7.0 + 8.0 * I;
        double r = creal(z);
        double i = cimag(z);
        if (r < 6.9 || r > 7.1) return 30;
        if (i < 7.9 || i > 8.1) return 31;
    }

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("c99_complex_mega", code, &["-lm".to_string()],),
        0,
    );
}
