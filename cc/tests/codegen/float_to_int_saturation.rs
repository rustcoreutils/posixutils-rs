//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// A floating constant out of an integer type's range folds as gcc folds it:
// saturated, with a NaN becoming 0. C17 6.3.1.4p1 leaves the conversion
// undefined; the run-time instruction disagrees between targets (x86-64
// gives the minimum, aarch64 saturates), and gcc.c-torture's
// execute/20031003-1 expects gcc's fold.
//

use crate::common::compile_and_run_everywhere;

/// Every target width, every source format and every context gcc folds in,
/// with gcc 13's answers on both targets.
#[test]
fn codegen_out_of_range_float_constant_saturates() {
    compile_and_run_everywhere(
        "float_to_int_saturation",
        include_str!("float_to_int_saturation.c"),
    );
}

/// gcc.c-torture execute/20031003-1: `(int)2147483648.0f`, and a `float`
/// that rounds up to the same value, are `INT_MAX` -- at -O0 too, where only
/// the front end folds.
#[test]
fn codegen_torture_20031003_1_folds_to_int_max() {
    const SRC: &str = r#"
int f1(void) { return (int)2147483648.0f; }
int f2(void) { return (int)(float)(2147483647); }
int main(void)
{
    if (f1() != 2147483647) return 1;
    if (f2() != 2147483647) return 2;
    return 0;
}
"#;
    compile_and_run_everywhere("torture_20031003_1", SRC);
}
