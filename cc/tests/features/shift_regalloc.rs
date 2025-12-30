//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for x86-64 register allocation with variable shifts
//
// On x86-64, variable shifts (shl/shr with non-constant count) require the
// shift count to be in CL (lower 8 bits of RCX). This creates a constraint
// that can clobber function arguments passed in RCX (the 4th integer arg).
//
// These tests verify that the register allocator correctly spills arguments
// when they would be clobbered by shift operations.
//

#[cfg(target_arch = "x86_64")]
mod x86_64_shift_tests {
    use crate::common::compile_and_run;

    /// Basic test: 4th param (RCX) used with variable left shift
    #[test]
    fn shift_4th_param_left() {
        let code = r#"
// d is in RCX, shift count needs RCX - must spill d first
int shift_left(int a, int b, int c, int d, int shift) {
    return d << shift;
}

int main(void) {
    // 8 << 2 = 32
    if (shift_left(1, 2, 3, 8, 2) != 32) return 1;
    // 1 << 4 = 16
    if (shift_left(0, 0, 0, 1, 4) != 16) return 2;
    // 255 << 0 = 255
    if (shift_left(0, 0, 0, 255, 0) != 255) return 3;
    return 0;
}
"#;
        assert_eq!(compile_and_run("shift_4th_left", code), 0);
    }

    /// 4th param with variable right shift
    #[test]
    fn shift_4th_param_right() {
        let code = r#"
int shift_right(int a, int b, int c, int d, int shift) {
    return d >> shift;
}

int main(void) {
    // 32 >> 2 = 8
    if (shift_right(1, 2, 3, 32, 2) != 8) return 1;
    // 16 >> 4 = 1
    if (shift_right(0, 0, 0, 16, 4) != 1) return 2;
    // -16 >> 2 = -4 (arithmetic shift for signed)
    if (shift_right(0, 0, 0, -16, 2) != -4) return 3;
    return 0;
}
"#;
        assert_eq!(compile_and_run("shift_4th_right", code), 0);
    }

    /// 4th param used BEFORE and AFTER shift - verifies spill/reload
    #[test]
    fn shift_4th_param_used_before_after() {
        let code = r#"
int use_before_after(int a, int b, int c, int d, int shift) {
    int before = d + 1;           // use d before shift
    int shifted = d << shift;     // shift clobbers RCX
    int after = d * 2;            // use d after shift
    return before + shifted + after;
}

int main(void) {
    // d=5, shift=2: before=6, shifted=20, after=10 => 36
    if (use_before_after(0, 0, 0, 5, 2) != 36) return 1;
    // d=1, shift=3: before=2, shifted=8, after=2 => 12
    if (use_before_after(0, 0, 0, 1, 3) != 12) return 2;
    return 0;
}
"#;
        assert_eq!(compile_and_run("shift_4th_before_after", code), 0);
    }

    /// Multiple shifts in same function
    #[test]
    fn shift_multiple_operations() {
        let code = r#"
int multi_shift(int a, int b, int c, int d, int s1, int s2) {
    int left = d << s1;
    int right = d >> s2;
    return left + right;
}

int main(void) {
    // d=8, s1=2, s2=1: left=32, right=4 => 36
    if (multi_shift(0, 0, 0, 8, 2, 1) != 36) return 1;
    // d=16, s1=1, s2=2: left=32, right=4 => 36
    if (multi_shift(0, 0, 0, 16, 1, 2) != 36) return 2;
    return 0;
}
"#;
        assert_eq!(compile_and_run("shift_multiple", code), 0);
    }

    /// All register args (1-6) with shifts - stress test
    #[test]
    fn shift_all_register_args() {
        let code = r#"
// All 6 register args used with shift operations
// x86_64: RDI, RSI, RDX, RCX, R8, R9
long shift_all(int a, int b, int c, int d, int e, int f, int shift) {
    return ((long)a << shift) + ((long)b << shift) + ((long)c << shift) +
           ((long)d << shift) + ((long)e << shift) + ((long)f << shift);
}

int main(void) {
    // All args = 1, shift = 2: each becomes 4, sum = 24
    if (shift_all(1, 1, 1, 1, 1, 1, 2) != 24) return 1;
    // Args 1-6, shift = 1: 2+4+6+8+10+12 = 42
    if (shift_all(1, 2, 3, 4, 5, 6, 1) != 42) return 2;
    return 0;
}
"#;
        assert_eq!(compile_and_run("shift_all_regs", code), 0);
    }

    /// Unsigned right shift (logical vs arithmetic)
    #[test]
    fn shift_unsigned_right() {
        let code = r#"
unsigned int shift_unsigned(int a, int b, int c, unsigned int d, int shift) {
    return d >> shift;
}

int main(void) {
    // 0x80000000 >> 1 = 0x40000000 (logical shift, not sign-extended)
    unsigned int big = 0x80000000u;
    if (shift_unsigned(0, 0, 0, big, 1) != 0x40000000u) return 1;
    // 0xFFFFFFFF >> 4 = 0x0FFFFFFF
    if (shift_unsigned(0, 0, 0, 0xFFFFFFFFu, 4) != 0x0FFFFFFFu) return 2;
    return 0;
}
"#;
        assert_eq!(compile_and_run("shift_unsigned", code), 0);
    }

    /// Shift with expression as count (not just variable)
    #[test]
    fn shift_expression_count() {
        let code = r#"
int shift_expr(int a, int b, int c, int d, int x, int y) {
    return d << (x + y);  // shift count is expression, still needs RCX
}

int main(void) {
    // d=1, x=1, y=2: 1 << 3 = 8
    if (shift_expr(0, 0, 0, 1, 1, 2) != 8) return 1;
    // d=4, x=0, y=1: 4 << 1 = 8
    if (shift_expr(0, 0, 0, 4, 0, 1) != 8) return 2;
    return 0;
}
"#;
        assert_eq!(compile_and_run("shift_expr_count", code), 0);
    }

    /// Shift in loop - multiple iterations using 4th param
    #[test]
    fn shift_in_loop() {
        let code = r#"
int shift_loop(int a, int b, int c, int d, int n) {
    int result = d;
    for (int i = 0; i < n; i++) {
        result = result << 1;  // each iteration doubles
    }
    return result;
}

int main(void) {
    // d=1, n=4: 1 << 4 = 16
    if (shift_loop(0, 0, 0, 1, 4) != 16) return 1;
    // d=3, n=3: 3 << 3 = 24
    if (shift_loop(0, 0, 0, 3, 3) != 24) return 2;
    return 0;
}
"#;
        assert_eq!(compile_and_run("shift_loop", code), 0);
    }
}
