//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::rc::Rc;

pub type CompleteCommandList = Vec<CompleteCommand>;
pub type Name = Rc<str>;

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
        word: Option<Word>,
        default_on_null: bool,
    },
    // ${parameter[:]=[word]}
    UnsetAssignDefault {
        parameter: Parameter,
        word: Option<Word>,
        assign_on_null: bool,
    },
    // ${parameter[:]?[word]}
    UnsetError {
        parameter: Parameter,
        word: Option<Word>,
        error_on_null: bool,
    },
    // ${parameter[:]+[word]}
    SetUseAlternative {
        parameter: Parameter,
        word: Option<Word>,
        substitute_null_with_word: bool,
    },
    // ${#parameter}
    StrLen(Parameter),
    // ${parameter(%[%]|#[#])[word]}
    RemovePattern {
        parameter: Parameter,
        pattern: Option<Word>,
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
    // used in the interpreter to control field splitting
    GeneratedUnquotedLiteral(String),
    QuotedLiteral(String),
    ParameterExpansion { expansion: ParameterExpansion, inside_double_quotes: bool },
    ArithmeticExpansion(ArithmeticExpr),
    CommandSubstitution { command: CompleteCommand, inside_double_quotes: bool },
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Word {
    pub parts: Vec<WordPart>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum IORedirectionKind {
    // >
    RedirectOutput,
    // >|
    RedirectOutputClobber,
    // >>
    RedirectOuputAppend,
    // >&
    DuplicateOutput,
    // <
    RedirectInput,
    // <&
    DuplicateInput,
    // <>
    OpenRW,
}

#[derive(Debug, PartialEq, Clone)]
pub enum RedirectionKind {
    IORedirection { kind: IORedirectionKind, file: Word },
    HereDocument { contents: String },
}

#[derive(Debug, PartialEq, Clone)]
pub struct Redirection {
    pub file_descriptor: Option<u32>,
    pub kind: RedirectionKind,
}

#[derive(PartialEq, Clone)]
pub struct Assignment {
    pub name: Name,
    pub value: Word,
}

impl std::fmt::Debug for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}={:?}", self.name, self.value)
    }
}

#[derive(PartialEq, Default, Clone)]
pub struct SimpleCommand {
    pub assignments: Vec<Assignment>,
    pub redirections: Vec<Redirection>,
    pub words: Vec<Word>,
}

impl std::fmt::Debug for SimpleCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "SimpleCommand:")?;
        writeln!(f, "  assignments: {:?}", self.assignments)?;
        writeln!(f, "  words: {:?}", self.words)?;
        writeln!(f, "  redirections: {:?}", self.redirections)?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CaseItem {
    pub pattern: Vec<Word>,
    pub body: CompleteCommand,
}

#[derive(Debug, PartialEq, Clone)]
pub struct If {
    pub condition: CompleteCommand,
    pub body: CompleteCommand,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompoundCommand {
    BraceGroup(CompleteCommand),
    Subshell(CompleteCommand),
    ForClause {
        iter_var: Name,
        words: Vec<Word>,
        body: CompleteCommand,
    },
    CaseClause {
        arg: Word,
        cases: Vec<CaseItem>,
    },
    IfClause {
        if_chain: Vec<If>,
    },
    WhileClause {
        condition: CompleteCommand,
        body: CompleteCommand,
    },
    UntilClause {
        condition: CompleteCommand,
        body: CompleteCommand,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition {
    pub name: Name,
    pub body: CompoundCommand,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Command {
    FunctionDefinition(FunctionDefinition),
    SimpleCommand(SimpleCommand),
    CompoundCommand {
        command: CompoundCommand,
        redirections: Vec<Redirection>,
    },
}

#[derive(PartialEq, Clone)]
pub struct Pipeline {
    pub commands: Vec<Command>,
    pub negate_status: bool,
}

impl std::fmt::Debug for Pipeline {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Pipeline:")?;
        for command in &self.commands {
            writeln!(f, "{}", indent(command))?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LogicalOp {
    And,
    Or,
    None,
}

#[derive(PartialEq, Clone)]
pub struct Conjunction {
    pub elements: Vec<(Pipeline, LogicalOp)>,
    pub is_async: bool,
}

impl std::fmt::Debug for Conjunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Conjunction{}:",
            if self.is_async { " (async)" } else { "" }
        )?;
        for (pipeline, logical_op) in &self.elements {
            writeln!(f, "{}", indent(pipeline))?;
            if *logical_op != LogicalOp::None {
                writeln!(f, "{:?}", logical_op)?;
            }
        }
        Ok(())
    }
}

#[derive(PartialEq, Clone)]
pub struct CompleteCommand {
    pub commands: Vec<Conjunction>,
}

impl std::fmt::Debug for CompleteCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "CompleteCommand:")?;
        for conjunction in &self.commands {
            writeln!(f, "{}", indent(conjunction))?;
        }
        Ok(())
    }
}

#[derive(PartialEq)]
pub struct Program {
    pub commands: CompleteCommandList,
}

impl std::fmt::Debug for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Program:")?;
        for command in &self.commands {
            writeln!(f, "{:?}", command)?;
        }
        Ok(())
    }
}

fn indent<D: std::fmt::Debug>(val: &D) -> String {
    format!("{:?}", val)
        .lines()
        .map(|line| format!("    {}", line))
        .collect::<Vec<String>>()
        .join("\n")
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
}
