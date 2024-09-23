//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

pub type CompleteCommandList = Vec<CompleteCommand>;
pub type Word = String;
pub type Name = String;

pub enum IORedirectionKind {
    RedirectOutput,
    RedirectOutputClobber,
    RedirectOuputAppend,
    DuplicateOutput,
    RedirectInput,
    DuplicateInput,
    OpenRW,
}

pub enum RedirectionKind {
    IORedirection { kind: IORedirectionKind, file: Word },
    HereDocument { contents: String },
}

pub struct Redirection {
    pub file_descriptor: Option<u32>,
    pub kind: RedirectionKind,
}

pub struct Assignment {
    pub name: Name,
    pub value: Word,
}

pub struct SimpleCommand {
    pub assignments: Vec<Assignment>,
    pub redirections: Vec<Redirection>,
    pub command: Word,
}

pub struct CaseItem {
    pub pattern: Vec<Word>,
    pub body: CompleteCommandList,
}

pub struct If {
    pub condition: CompleteCommandList,
    pub body: CompleteCommandList,
}

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

pub enum Command {
    FunctionDefinition,
    SimpleCommand,
    CompoundCommand {
        command: CompoundCommand,
        redirections: Vec<Redirection>,
    },
}

pub struct Pipeline {
    pub commands: Vec<Command>,
}

pub enum LogicalOp {
    And,
    Or,
    None,
}

pub struct Conjunction {
    pub elements: Vec<(Pipeline, LogicalOp)>,
}

pub enum CompleteCommand {
    AsyncList(Vec<Conjunction>),
    Conjunction(Conjunction),
}

pub struct Program {
    pub commands: CompleteCommandList,
}
