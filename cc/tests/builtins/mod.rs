//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Compiler builtins tests for c17
//
// This module contains mega-tests for compiler builtins:
// - bit_ops: clz, ctz, popcount, bswap
// - memory: alloca, offsetof
// - intrinsics: types_compatible, constant_p, unreachable, expect
// - math: nan, nans, flt_rounds
// - has_feature: __has_builtin, __has_feature
//

mod argument_checks;
mod bit_ops;
mod call_semantics;
mod copy_fold;
mod frame_address;
mod gnu_atomics;
mod gnu_batch;
mod has_feature;
mod intrinsics;
mod issignaling;
mod libm;
mod math;
mod mem_expand;
mod mem_moves;
mod memory;
mod stdio_fold;
mod string_fold;
mod va_arg_pack;
