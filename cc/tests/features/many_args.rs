//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for functions with many mixed int/FP arguments exceeding register counts
// Verifies correct stack offset calculations in regalloc.rs
//

use crate::common::compile_and_run;

#[test]
fn test_many_mixed_args() {
    let code = r#"
// Function with 18 interleaved args: 9 ints + 9 doubles
// x86_64 SysV ABI: 6 int regs (RDI-R9) + 8 FP regs (XMM0-7)
// Stack args: m,o,q (ints at idx 6,7,8) + r (double at idx 8)
double many_args(int a, double b, int c, double d, int e, double f,
                 int g, double h, int i, double j, int k, double l,
                 int m, double n, int o, double p, int q, double r) {
    double sum = (double)a + b + (double)c + d + (double)e + f +
                 (double)g + h + (double)i + j + (double)k + l +
                 (double)m + n + (double)o + p + (double)q + r;
    return sum;
}

int main(void) {
    // ints: 1+2+3+4+5+6+7+8+9 = 45
    // doubles: 0.5+1.5+2.5+3.5+4.5+5.5+6.5+7.5+8.5 = 40.5
    // total: 85.5
    double result = many_args(1, 0.5, 2, 1.5, 3, 2.5,
                              4, 3.5, 5, 4.5, 6, 5.5,
                              7, 6.5, 8, 7.5, 9, 8.5);

    if (result < 85.4 || result > 85.6) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("many_mixed_args", code), 0);
}

#[test]
fn test_many_int_args() {
    let code = r#"
// Function with 12 int args: 6 in registers, 6 on stack
// x86_64: RDI, RSI, RDX, RCX, R8, R9 (6 regs)
// aarch64: X0-X7 (8 regs)
// Tests integer-only stack overflow
long many_ints(int a, int b, int c, int d, int e, int f,
               int g, int h, int i, int j, int k, int l) {
    return (long)a + b + c + d + e + f + g + h + i + j + k + l;
}

int main(void) {
    // 1+2+3+4+5+6+7+8+9+10+11+12 = 78
    long result = many_ints(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12);
    if (result != 78) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("many_int_args", code), 0);
}

#[test]
fn test_many_fp_args() {
    let code = r#"
// Function with 10 double args: 8 in registers, 2 on stack
// x86_64: XMM0-XMM7 (8 regs)
// aarch64: V0-V7 (8 regs)
// Tests FP-only stack overflow - triggers negative offset bug if broken
double many_doubles(double a, double b, double c, double d,
                    double e, double f, double g, double h,
                    double i, double j) {
    return a + b + c + d + e + f + g + h + i + j;
}

int main(void) {
    // 1.0+2.0+3.0+4.0+5.0+6.0+7.0+8.0+9.0+10.0 = 55.0
    double result = many_doubles(1.0, 2.0, 3.0, 4.0, 5.0,
                                  6.0, 7.0, 8.0, 9.0, 10.0);
    if (result < 54.9 || result > 55.1) return 1;
    return 0;
}
"#;
    assert_eq!(compile_and_run("many_fp_args", code), 0);
}
