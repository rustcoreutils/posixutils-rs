//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C99 tests for c17
//
// This module contains mega-tests for C99-era features:
// - types: longlong, bool, complex, float16
// - features: VLA, inline, varargs, array_param_qualifiers
// - initializers: designated init, compound literals
//

mod brace_elision;
mod c99_features_gaps;
mod complex;
mod complex_abi;
mod complex_conversion;
mod expressions;
mod features;
mod identifiers;
mod initializers;
mod initializers_overrides;
mod initializers_static;
mod initializers_strings;
mod stdlib_headers;
mod tgmath;
mod translation_limits;
mod type_macros;
mod types;
mod types_keywords;
