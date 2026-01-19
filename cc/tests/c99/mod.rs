//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 tests for pcc
//
// This module contains mega-tests for C99-era features:
// - types: longlong, bool, complex, float16
// - features: VLA, inline, varargs, array_param_qualifiers
// - initializers: designated init, compound literals
//

mod features;
mod initializers;
mod types;
