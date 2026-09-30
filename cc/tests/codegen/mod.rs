//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Code Generation Tests
//
// Tests for register allocation, inline assembly, PIC, optimization, and debug info.
//

mod aarch64_offsets;
mod aarch64_runtime;
pub mod asm_probe;
mod atomics_asm;
mod binary128;
mod block_moves;
mod complex_fold;
mod constant_branch;
mod cross_abi;
mod debug_info;
mod inline_asm;
mod memopt;
mod misc;
mod pic;
mod plain_char;
mod promotion;
mod regalloc;
mod scaling;
mod sections;
mod stacked_args;
mod tls_models;
mod trapping_folds;
