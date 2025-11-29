//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fmt;

/// Error types for the yacc parser generator
#[derive(Debug, Clone)]
pub enum YaccError {
    /// Command-line usage error
    Usage(String),
    /// I/O error
    Io(String),
    /// Lexical error (invalid token)
    Lexical {
        line: usize,
        column: usize,
        msg: String,
    },
    /// Syntax error in grammar file
    Syntax { line: usize, msg: String },
    /// Grammar error (undefined symbol, etc.)
    Grammar(String),
}

impl fmt::Display for YaccError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            YaccError::Usage(msg) => write!(f, "{}", msg),
            YaccError::Io(msg) => write!(f, "{}", msg),
            YaccError::Lexical { line, column, msg } => {
                write!(f, "line {}, column {}: {}", line, column, msg)
            }
            YaccError::Syntax { line, msg } => {
                write!(f, "line {}: {}", line, msg)
            }
            YaccError::Grammar(msg) => write!(f, "{}", msg),
        }
    }
}

impl std::error::Error for YaccError {}
