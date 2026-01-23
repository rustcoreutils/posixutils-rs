//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Token module - lexer and preprocessor
//

pub mod lexer;
pub mod preprocess;

// Re-export items used by main.rs
pub use lexer::{show_token, token_type_name, StreamTable, Tokenizer};
pub use preprocess::{
    preprocess_asm_file, preprocess_with_defines, AsmPreprocessConfig, PreprocessConfig,
};
