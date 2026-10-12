//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Floating-point values in registers, at run time: the allocator colors
// them across blocks, and on aarch64 keeps a `double` live across a call in
// V8-V15, whose low halves a callee preserves. What the assembly shows is
// in `test_asm/codegen_regalloc.rs`.
//

use crate::common::compile_and_run_everywhere;

/// Doubles, floats and a `_Float16` live across calls and blocks in a loop;
/// more values live across a call than V8-V15 hold, the arguments among
/// them; a binary128 `long double` and 128-bit vectors live across calls,
/// with callees that overwrite the high halves of V8-V15; values live across
/// `setjmp` when `longjmp` comes back; and the comparison chain 8bdb25c1
/// fixed in the old allocator. Each result is checked against the same
/// computation with every value in memory, or a known answer.
#[test]
fn fp_values_survive_calls_blocks_and_longjmp() {
    compile_and_run_everywhere("regalloc_fp", include_str!("regalloc_fp.c"));
}
