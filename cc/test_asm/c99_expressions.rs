//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 expressions: the compile-only halves of `tests/c99/expressions.rs`
// cases, whose run-time halves run there as sections of a mega program.
//

use crate::test_compile::compile_expect_error;

/// `sizeof` a variably-length array is not an integer constant expression.
///
/// `SizeofType` grew a runtime guard; `SizeofExpr` did not, so `sizeof a` for
/// `int a[n]` folded to 0 -- `size_bits` reports 0 for an array with no extent
/// -- and `case sizeof a:` was accepted with the wrong value, matching
/// `switch (0)`. A `TypeId` for `int[n]` is indistinguishable from `int[]`, so
/// the question has to be asked of the array levels.
///
/// The accepted half -- `sizeof` a *fixed* array is still a constant
/// expression -- runs as the `sizeof_fixed_array_case` section of
/// `c99_expressions_constant_folding_mega` in `tests/c99/expressions.rs`.
#[test]
fn c99_sizeof_a_vla_is_not_a_constant_expression() {
    // `case sizeof a:` must be rejected, not silently folded to 0.
    let rejected = r#"
int g(int n)
{
    int a[n];
    switch (n) {
    case sizeof a: return 1;
    default: return 0;
    }
}
int main(void) { return g(0); }
"#;
    compile_expect_error("sizeof_vla_case_label", rejected, "constant");
}
