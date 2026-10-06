//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Negative-path tests: programs that must be REJECTED, compiled in process
// through `test_compile` -- the pipeline `c17 -S` runs -- so that no case
// costs a process.
//
// Every other suite proves that accepted programs run correctly. None proved
// that invalid programs are diagnosed — `compile_and_run` collapses a compile
// failure into the sentinel `-1` and discards stderr — which is how a dozen
// missing C constraint checks went unnoticed.
//
// Each constraint gets both directions: a program that must be rejected, and
// one that must still be accepted, so a check cannot pass by rejecting
// everything.
//
// A case that needs the driver itself -- linking, running, several
// translation units, files on disk -- stays in `tests/diagnostics`.
//

mod arrays_and_initializers;
mod asm_constraints;
mod asm_templates;
mod atomics_and_asm;
mod builtins;
mod c11;
mod c89;
mod c99;
mod cast_to_union;
mod codegen;
mod complex_specifiers;
mod conditional_operands;
mod constraint_sweep;
mod constraints_core;
mod declarations;
mod declarators_and_shifts;
mod default_pedwarns;
mod empty_declarations;
mod expressions;
mod function_compatibility;
mod function_pointers;
mod ifunc;
mod incomplete_types;
mod inline_static_reference;
mod jumps_and_characters;
mod keywords_and_suffixes;
mod misc;
mod objects_and_constants;
mod permissive;
mod ranges_designators_labels;
mod return_conversion;
mod specifiers;
mod target_attr;
mod va_arg_pack;
