//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Integration test harness for pcc
//
// Test organization by C standard and feature category:
// - c89/: C89 standard tests (datatypes, operators, control flow, functions, globals, storage)
// - c99/: C99 extensions (types, features, initializers)
// - c11/: C11 features (core, atomics)
// - builtins/: Compiler builtins (bit_ops, memory, intrinsics, math, has_feature)
// - codegen/: Code generation (regalloc, inline_asm, pic, misc)
// - preprocessor/: Preprocessor (macros)
// - misc/: Miscellaneous (setjmp, stmt_expr)
// - tools/: Tool tests (cflow, ctags, cxref)
//

mod builtins;
mod c11;
mod c89;
mod c99;
mod codegen;
mod common;
mod cpython;
mod misc;
mod preprocessor;
mod tools;
