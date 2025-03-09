//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::parse::word::Word;
use std::rc::Rc;

pub type CompleteCommandList = Vec<CompleteCommand>;
pub type Name = Rc<str>;

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
    HereDocument(Word),
    QuotedHereDocument(String),
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

impl SimpleCommand {
    pub fn none_if_empty(self) -> Option<Self> {
        if self == Self::default() {
            None
        } else {
            Some(self)
        }
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct CaseItem {
    pub pattern: Vec<Word>,
    pub body: CompleteCommand,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct If {
    pub condition: CompleteCommand,
    pub body: CompleteCommand,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
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
        else_body: Option<CompleteCommand>,
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

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub struct FunctionDefinition {
    pub name: Name,
    pub body: Rc<CompoundCommand>,
}

#[derive(Debug, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum CommandType {
    FunctionDefinition(FunctionDefinition),
    SimpleCommand(SimpleCommand),
    CompoundCommand {
        command: CompoundCommand,
        redirections: Vec<Redirection>,
    },
}

#[derive(Debug, Clone)]
pub struct Command {
    pub type_: CommandType,
    pub lineno: u32,
}

impl Command {
    pub fn new(type_: CommandType, lineno: u32) -> Self {
        Self { type_, lineno }
    }
}

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq))]
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

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq))]
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

#[derive(Clone)]
#[cfg_attr(test, derive(PartialEq))]
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

fn indent<D: std::fmt::Debug>(val: &D) -> String {
    format!("{:?}", val)
        .lines()
        .map(|line| format!("    {}", line))
        .collect::<Vec<String>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    impl From<CommandType> for Command {
        fn from(value: CommandType) -> Self {
            Command::new(value, 0)
        }
    }

    impl PartialEq for Command {
        fn eq(&self, other: &Self) -> bool {
            self.type_ == other.type_
        }
    }
}
