//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Compile-only cases of tests/builtins/gnu_batch.rs, in process.
//

use crate::test_compile::compile_expect_error;

/// Where the call is written: the presumed file and line, after `#line`,
/// and the enclosing function's name, or "" at file scope. (The run half is
/// in tests/builtins/gnu_batch.rs; this is its diagnostic half.)
#[test]
fn builtins_position_of_the_call() {
    compile_expect_error(
        "builtins_probability_range",
        "int f(int x) { return __builtin_expect_with_probability(x, 1, 2.0); }\n",
        "probability must be a constant floating-point expression between 0 and 1",
    );
}

/// x86-64 CPU detection reads libgcc's `__cpu_model` as gcc's code does.
/// (The run half is in tests/builtins/gnu_batch.rs.)
#[cfg(target_arch = "x86_64")]
#[test]
fn builtins_x86_cpu_detection() {
    compile_expect_error(
        "builtins_cpu_bad_name",
        "int f(void) { return __builtin_cpu_supports(\"nosuch\"); }\n",
        "parameter to builtin not valid: nosuch",
    );
}
