//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Parse module - AST and parser
//

pub mod ast;
mod expression;
pub mod parser;

#[cfg(test)]
mod test_parser;

// Re-export parser used by main.rs
pub use parser::Parser;
