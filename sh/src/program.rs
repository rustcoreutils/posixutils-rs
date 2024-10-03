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

#[derive(Debug, PartialEq)]
pub enum Parameter {
    Number(u32),
    Name(Name),
}

#[derive(Debug, PartialEq)]
pub enum ParameterExpansion {
    // $parameter or ${parameter}
    Simple(Parameter),
    // ${parameter:-[word]}
    NullUnsetUseDefault(Parameter, Option<Word>),
    // ${parameter-[word]}
    UnsetUseDefault(Parameter, Option<Word>),
    // ${parameter:=[word]}
    NullUnsetAssignDefault(Parameter, Option<Word>),
    // ${parameter=[word]}
    UnsetAssignDefault(Parameter, Option<Word>),
    // ${parameter:?[word]}
    NullUnsetError(Parameter, Option<Word>),
    // ${parameter?[word]}
    UnsetError(Parameter, Option<Word>),
    // ${parameter:+[word]}
    SetUseAlternative(Parameter, Option<Word>),
    // ${parameter+[word]}
    SetNullUseAlternative(Parameter, Option<Word>),
    // ${#parameter}
    StrLen(Parameter),
    // ${parameter%[word]}
    RemoveSmallestSuffix(Parameter, Option<Word>),
    // ${parameter%%[word]}
    RemoveLargestSuffix(Parameter, Option<Word>),
    // ${parameter#[word]}
    RemoveSmallestPrefix(Parameter, Option<Word>),
    // ${parameter##[word]}
    RemoveLargestPrefix(Parameter, Option<Word>),
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum WordPart {
    Literal(Rc<str>),
    ParameterExpansion(ParameterExpansion),
    ArithmeticExpansion(ArithmeticExpr),
    CommandSubstitution(CompleteCommand),
}

#[derive(Debug, PartialEq, Default)]
pub struct Word {
    pub parts: Vec<WordPart>,
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum RedirectionKind {
    IORedirection { kind: IORedirectionKind, file: Word },
    HereDocument { contents: String },
}

#[derive(Debug, PartialEq)]
pub struct Redirection {
    pub file_descriptor: Option<u32>,
    pub kind: RedirectionKind,
}

#[derive(Debug, PartialEq)]
pub struct Assignment {
    pub name: Name,
    pub value: Word,
}

#[derive(Debug, PartialEq, Default)]
pub struct SimpleCommand {
    pub command: Option<Word>,
    pub assignments: Vec<Assignment>,
    pub redirections: Vec<Redirection>,
    pub arguments: Vec<Word>,
}

#[derive(Debug, PartialEq)]
pub struct CaseItem {
    pub pattern: Vec<Word>,
    pub body: CompleteCommandList,
}

#[derive(Debug, PartialEq)]
pub struct If {
    pub condition: CompleteCommandList,
    pub body: CompleteCommandList,
}

#[derive(Debug, PartialEq)]
pub enum CompoundCommand {
    BraceGroup(CompleteCommandList),
    Subshell(CompleteCommandList),
    ForClause {
        name: String,
        words: Vec<String>,
        body: CompleteCommandList,
    },
    CaseClause {
        arg: Word,
        cases: Vec<CaseItem>,
    },
    IfClause {
        if_chain: Vec<If>,
    },
    WhileClause {
        condition: CompleteCommandList,
        body: CompleteCommandList,
    },
    UntilClause {
        condition: CompleteCommandList,
        body: CompleteCommandList,
    },
}

#[derive(Debug, PartialEq)]
pub enum Command {
    FunctionDefinition,
    SimpleCommand(SimpleCommand),
    CompoundCommand {
        command: CompoundCommand,
        redirections: Vec<Redirection>,
    },
}

#[derive(Debug, PartialEq)]
pub struct Pipeline {
    pub commands: Vec<Command>,
}

#[derive(Debug, PartialEq)]
pub enum LogicalOp {
    And,
    Or,
    None,
}

#[derive(Debug, PartialEq)]
pub struct Conjunction {
    pub elements: Vec<(Pipeline, LogicalOp)>,
    pub is_async: bool,
}

#[derive(Debug, PartialEq)]
pub struct CompleteCommand {
    pub commands: Vec<Conjunction>,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub commands: CompleteCommandList,
}
