//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for __builtin_constant_p builtin
//

use crate::common::compile_and_run;

#[test]
fn constant_p_comprehensive() {
    let code = r#"
int main(void) {
    // Test 1: Integer literal - should be constant
    if (__builtin_constant_p(42) != 1) return 1;

    // Test 2: Character literal - should be constant
    if (__builtin_constant_p('x') != 1) return 2;

    // Test 3: Constant expression (arithmetic) - should be constant
    if (__builtin_constant_p(10 + 20) != 1) return 3;

    // Test 4: Constant expression (comparison) - should be constant
    if (__builtin_constant_p(5 < 10) != 1) return 4;

    // Test 5: Constant expression (ternary) - should be constant
    if (__builtin_constant_p(1 ? 100 : 200) != 1) return 5;

    // Test 6: Variable - should NOT be constant
    int x = 10;
    if (__builtin_constant_p(x) != 0) return 6;

    // Test 7: Pointer to variable - should NOT be constant
    if (__builtin_constant_p(&x) != 0) return 7;

    // Test 8: Complex constant expression - should be constant
    if (__builtin_constant_p((5 * 3) + (8 / 2) - 1) != 1) return 8;

    // Test 9: Negative constant - should be constant
    if (__builtin_constant_p(-42) != 1) return 9;

    // Test 10: Bitwise operations on constants - should be constant
    if (__builtin_constant_p(0xFF & 0x0F) != 1) return 10;

    // Test 11: Shift operations on constants - should be constant
    if (__builtin_constant_p(1 << 4) != 1) return 11;

    // Test 12: Use in conditional - common pattern
    int result;
    if (__builtin_constant_p(42)) {
        result = 1;
    } else {
        result = 0;
    }
    if (result != 1) return 12;

    // Test 13: Zero literal - should be constant
    if (__builtin_constant_p(0) != 1) return 13;

    return 0;
}
"#;
    assert_eq!(compile_and_run("constant_p_comprehensive", code), 0);
}
