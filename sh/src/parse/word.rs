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
        parameter: Parameter,
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
pub enum ArithmeticExpr {
    // arithmetic operations
    Add(Box<ArithmeticExpr>, Box<ArithmeticExpr>),
    Sub(Box<ArithmeticExpr>, Box<ArithmeticExpr>),
    Div(Box<ArithmeticExpr>, Box<ArithmeticExpr>),
    Mul(Box<ArithmeticExpr>, Box<ArithmeticExpr>),
    Mod(Box<ArithmeticExpr>, Box<ArithmeticExpr>),
    Negate(Box<ArithmeticExpr>),
    PreInc(Name),
    PostInc(Name),
    PreDec(Name),
    PostDec(Name),

    // compairison operations
    Eq(Box<ArithmeticExpr>, Box<ArithmeticExpr>),
    Neq(Box<ArithmeticExpr>, Box<ArithmeticExpr>),
    Ge(Box<ArithmeticExpr>, Box<ArithmeticExpr>),
    Le(Box<ArithmeticExpr>, Box<ArithmeticExpr>),
    Geq(Box<ArithmeticExpr>, Box<ArithmeticExpr>),
    Leq(Box<ArithmeticExpr>, Box<ArithmeticExpr>),

    // logical operations
    Not(Box<ArithmeticExpr>, Box<ArithmeticExpr>),
    And(Box<ArithmeticExpr>, Box<ArithmeticExpr>),
    Or(Box<ArithmeticExpr>, Box<ArithmeticExpr>),

    // bitwise operations
    BNot(Box<ArithmeticExpr>),
    BAnd(Box<ArithmeticExpr>),
    BOr(Box<ArithmeticExpr>),
    Xor(Box<ArithmeticExpr>),
    Shl(Box<ArithmeticExpr>),
    Shr(Box<ArithmeticExpr>),

    ConditionalOperator {
        condition: Box<ArithmeticExpr>,
        true_expr: Box<ArithmeticExpr>,
        false_expr: Box<ArithmeticExpr>,
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
    ArithmeticExpansion(ArithmeticExpr),
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
