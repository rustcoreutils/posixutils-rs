//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::parse::command::{CompleteCommand, Name};

#[derive(Debug, Clone, PartialEq)]
pub enum SpecialParameter {
    At,
    Asterisk,
    Hash,
    QuestionMark,
    Minus,
    Dollar,
    Bang,
    Zero,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Parameter {
    Number(u32),
    Variable(Name),
    Special(SpecialParameter),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParameterExpansion {
    // $parameter or ${parameter}
    Simple(Parameter),
    // ${parameter[:]-[word]}
    UnsetUseDefault {
        parameter: Parameter,
        word: Word,
        default_on_null: bool,
    },
    // ${parameter[:]=[word]}
    UnsetAssignDefault {
        variable: Name,
        word: Word,
        assign_on_null: bool,
    },
    // ${parameter[:]?[word]}
    UnsetError {
        parameter: Parameter,
        word: Word,
        error_on_null: bool,
    },
    // ${parameter[:]+[word]}
    SetUseAlternative {
        parameter: Parameter,
        word: Word,
        substitute_null_with_word: bool,
    },
    // ${#parameter}
    StrLen(Parameter),
    // ${parameter(%[%]|#[#])[word]}
    RemovePattern {
        parameter: Parameter,
        pattern: Word,
        /// otherwise remove smallest
        remove_largest: bool,
        /// otherwise remove suffix
        remove_prefix: bool,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum WordPart {
    UnquotedLiteral(String),
    QuotedLiteral(String),
    ParameterExpansion {
        expansion: ParameterExpansion,
        inside_double_quotes: bool,
    },
    ArithmeticExpansion {
        expr: Word,
        inside_double_quotes: bool,
    },
    CommandSubstitution {
        commands: String,
        inside_double_quotes: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Word {
    pub parts: Vec<WordPart>,
}

#[cfg(test)]
pub mod test_utils {
    use super::*;

    pub fn quoted_literal(contents: &str) -> Word {
        Word {
            parts: vec![WordPart::QuotedLiteral(contents.to_string())],
        }
    }

    pub fn unquoted_literal(contents: &str) -> Word {
        Word {
            parts: vec![WordPart::UnquotedLiteral(contents.to_string())],
        }
    }

    pub fn special_parameter(param: SpecialParameter) -> Word {
        Word {
            parts: vec![WordPart::ParameterExpansion {
                expansion: ParameterExpansion::Simple(Parameter::Special(param)),
                inside_double_quotes: false,
            }],
        }
    }
}
