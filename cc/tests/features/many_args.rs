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
