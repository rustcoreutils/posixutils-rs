// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

pub mod lex;
pub mod parse;
pub mod preprocessor;

pub use parse::{Identifier, Makefile, Rule, VariableDefinition};

/// Let's start with defining all kinds of tokens and
/// composite nodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
pub enum SyntaxKind {
    // Simple single-char AST nodes
    SINGLE_QUOTE = 0,
    DOUBLE_QUOTE,
    WHITESPACE,
    BACKSLASH,
    QUESTION,
    AT_SIGN,
    NEWLINE,
    PERCENT,
    EQUALS,
    DOLLAR,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    COLON,
    CARET,
    COMMA,
    LESS,
    PLUS,
    STAR,
    TAB,

    // Keywords
    INCLUDE,
    EXPORT,
    // This may be used as an extension to syntax
    //   OVERRIDE,
    //   UNEXPORT,
    //   IFDEF,
    //   IFNDEF,
    //   IFEQ,
    //   IFNEQ,
    //   ELSE,
    //   ENDIF,
    //   DEFINE,
    //   UNDEFINE,
    //   ENDEF,
    IDENTIFIER,
    // Unused as we have more granular tokens for different operators
    // OPERATOR,
    COMMENT,
    INDENT,
    ERROR,
    TEXT,

    // composite nodes
    ROOT, // The entire file
    RULE, // A single rule
    PREREQUISITES,
    RECIPE,
    VARIABLE,
    EXPR,
    MACRO,
}

/// Convert our `SyntaxKind` into the rowan `SyntaxKind`.
impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}
