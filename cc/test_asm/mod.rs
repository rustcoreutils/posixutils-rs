//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Compile-only halves of integration-suite cases: generated assembly, and
// the diagnostics of accepted or rejected programs, compiled in process
// through `test_compile` -- the pipeline `c17 -S` runs -- so that no case
// costs a process. The run-time halves stay with their suite in `tests/`.
//
// A case that needs the driver itself -- linking, running, -E output,
// several translation units, files on disk -- stays in `tests/`.
//

mod asm_probe;
mod builtins_bit_ops;
mod builtins_call_semantics;
mod builtins_copy_fold;
mod builtins_gnu_atomics;
mod builtins_gnu_batch;
mod builtins_intrinsics;
mod builtins_libm;
mod builtins_math;
mod builtins_mem_expand;
mod builtins_mem_moves;
mod builtins_stdio_fold;
mod builtins_string_fold;
mod builtins_va_arg_pack;
mod c11_member_lists;
mod c99_complex;
mod c99_expressions;
mod c99_features;
mod c99_identifiers;
mod c99_initializers;
mod c99_stdlib_headers;
mod c99_types;
mod codegen_aggregate_abi;
mod codegen_asm_attributes;
mod codegen_asm_probe;
mod codegen_atomics_asm;
mod codegen_binary128;
mod codegen_block_moves;
mod codegen_complex_fold;
mod codegen_constant_branch;
mod codegen_cross_abi;
mod codegen_cross_abi_types;
mod codegen_cross_abi_varargs;
mod codegen_debug_info;
mod codegen_floating;
mod codegen_fp_compare;
mod codegen_inline_asm;
mod codegen_inlining;
mod codegen_macho_labels;
mod codegen_memopt;
mod codegen_memopt_blockops;
mod codegen_ms_abi;
mod codegen_optimizer;
mod codegen_promotion;
mod codegen_regalloc;
mod codegen_sections;
mod codegen_stacked_args;
mod codegen_symbols;
mod codegen_tls_models;
mod codegen_trapping_folds;
mod codegen_types_exprs;
mod codegen_unwind;
mod codegen_varargs;
mod codegen_vector_abi;
mod codegen_vector_native;
mod misc_statement_attributes;
mod preprocessor_std_dialect;
