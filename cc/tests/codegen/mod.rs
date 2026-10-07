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
mod aggregate_abi;
mod asm_attributes;
mod asm_constraint_letters;
mod asm_operand_modifiers;
pub mod asm_probe;
mod binary128;
mod bitfield_layout;
mod block_moves;
mod call_args;
mod complex_fold;
mod constant_branch;
mod cross_abi;
mod cross_abi_types;
mod cross_abi_varargs;
mod determinism;
mod file_scope_asm;
mod float_to_int_saturation;
mod floating;
mod fp_compare_traps;
mod fp_untaken_arms;
mod gnu_inline_linkage;
mod ifunc;
mod inline_asm;
mod inlining;
mod int128;
mod label_diff;
mod long_double_select;
mod macho_labels;
mod memopt;
mod memopt_blockops;
mod ms_abi;
mod optimizer;
mod pic;
mod plain_char;
mod promotion;
mod regalloc;
mod scaling;
mod sections;
mod simd_headers;
mod stack_protector;
mod stacked_args;
mod storage_order;
mod symbols;
mod tls_models;
mod trapping_folds;
mod types_exprs;
mod unreachable_arms;
mod unwind;
mod varargs;
mod vector_abi;
mod vector_native;
mod vectors;
