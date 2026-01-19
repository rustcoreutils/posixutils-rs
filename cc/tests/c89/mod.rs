//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C89 (ANSI C) tests for pcc
//
// This module contains mega-tests for C89-era features:
// - datatypes: ALL integral, floating-point, and aggregate types
// - operators: bitfield, mixed_cmp, ops_struct, short_circuit
// - control_flow: if/while/for/switch/goto
// - functions: function_pointers, calls, recursion
// - globals: ALL globals tests
// - storage: storage.rs + static_local.rs
//

mod control_flow;
mod datatypes;
mod functions;
mod globals;
mod operators;
mod storage;
