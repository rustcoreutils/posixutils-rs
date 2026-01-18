//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Library interface for posixutils-cc
//
// This module exports the C parser, tokenizer, and related infrastructure
// for use by other crates (cflow, ctags, cxref).
//

pub mod abi;
pub mod arch;
pub mod builtin_headers;
pub mod builtins;
pub mod diag;
pub mod ir;
pub mod opt;
pub mod os;
pub mod parse;
pub mod rtlib;
pub mod strings;
pub mod symbol;
pub mod target;
pub mod token;
pub mod types;
