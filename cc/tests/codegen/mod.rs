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
mod asm_callee_saved;
mod asm_constraint_letters;
mod asm_fp_clobbers;
mod asm_memory_outputs;
mod asm_named_operands;
mod asm_operand_modifiers;
mod asm_paravirt;
mod asm_pinned_operands;
pub mod asm_probe;
mod asm_register_vars;
mod asm_template_syntax;
mod binary128;
mod bitfield_layout;
mod block_moves;
// Its programs run under qemu-user, with the C library a Linux one.
#[cfg(unix)]
mod branch_protection;
mod call_args;
mod call_attributes;
mod code_options;
// CET and its ELF note are x86-64 Linux's.
#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
mod cf_protection;
mod compiletime_folds;
mod complex_fold;
mod constant_branch;
mod cross_abi;
mod cross_abi_types;
mod cross_abi_varargs;
mod debug_info;
mod determinism;
// Thread cancellation and libgcc's forced unwind, as a glibc system has them.
#[cfg(target_os = "linux")]
mod exceptions;
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
mod kernel;
mod label_diff;
mod long_double_select;
mod macho_labels;
mod memopt;
mod memopt_blockops;
mod ms_abi;
#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
mod named_address_spaces;
mod named_sections;
mod object_layout;
mod optimizer;
mod pic;
mod plain_char;
// `-pg` is refused on Darwin, as clang refuses it.
#[cfg(target_os = "linux")]
mod profiling;
mod promotion;
mod regalloc;
mod rounding_math;
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
