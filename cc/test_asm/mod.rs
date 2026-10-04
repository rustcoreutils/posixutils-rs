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
mod c99_complex;
mod c99_expressions;
mod c99_features;
mod c99_identifiers;
mod c99_initializers;
mod c99_stdlib_headers;
mod c99_types;
mod codegen_aggregate_abi;
mod codegen_binary128;
mod codegen_cross_abi;
mod codegen_cross_abi_types;
mod codegen_cross_abi_varargs;
mod codegen_ms_abi;
mod codegen_stacked_args;
mod codegen_varargs;
mod codegen_vector_abi;
