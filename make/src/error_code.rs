//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use core::fmt;
use std::io;

use crate::parser::parse::ParseError;
use crate::special_target::Error;
use gettextrs::gettext;

/// Represents the error codes that can be returned by the make utility
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ErrorCode {
    // Transparent
    ExecutionError { exit_code: Option<i32> },
    IoError(io::ErrorKind),
    ParserError { constraint: ParseError },

    // Specific
    NoMakefile,
    NotUpToDateError { target: String },
    NoTarget { target: Option<String> },
    NoRule { rule: String },
    RecursivePrerequisite { origin: String },
    SpecialTargetConstraintNotFulfilled { target: String, constraint: Error },
}

impl From<ErrorCode> for i32 {
    fn from(err: ErrorCode) -> i32 {
        (&err).into()
    }
}

// todo: tests error codes
impl From<&ErrorCode> for i32 {
    fn from(err: &ErrorCode) -> i32 {
        use ErrorCode::*;

        match err {
            NotUpToDateError { .. } => 1,
            ExecutionError { .. } => 2,
            IoError(_) => 3,
            ParserError { .. } => 4,
            NoMakefile => 5,
            NoTarget { .. } => 6,
            NoRule { .. } => 7,
            RecursivePrerequisite { .. } => 8,
            SpecialTargetConstraintNotFulfilled { .. } => 9,
        }
    }
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        use ErrorCode::*;

        match self {
            NotUpToDateError { target } => {
                write!(f, "{}: {}", target, gettext("target is not up to date"))
            }
            ExecutionError { exit_code } => match exit_code {
                Some(exit_code) => {
                    write!(f, "{}: {}", gettext("execution error"), exit_code)
                }
                None => {
                    write!(
                        f,
                        "{}: {}",
                        gettext("execution error"),
                        gettext("terminated by signal"),
                    )
                }
            },
            IoError(err) => write!(f, "{}: {}", gettext("io error"), err),
            NoMakefile => write!(f, "{}", gettext("no makefile")),
            ParserError { constraint } => write!(f, "{}: {}", gettext("parse error"), constraint),
            NoTarget { target } => match target {
                Some(target) => write!(f, "{} '{}'", gettext("no target"), target),
                None => write!(f, "{}", gettext("no targets to execute")),
            },
            NoRule { rule } => write!(f, "{} '{}'", gettext("no rule"), rule),
            RecursivePrerequisite { origin } => {
                write!(
                    f,
                    "{} '{}'",
                    gettext("recursive prerequisite found trying to build"),
                    origin,
                )
            }
            SpecialTargetConstraintNotFulfilled { target, constraint } => {
                write!(
                    f,
                    "'{}' {}: {}",
                    target,
                    gettext("special target constraint is not fulfilled"),
                    constraint,
                )
            }
        }
    }
}

impl std::error::Error for ErrorCode {}

impl From<io::Error> for ErrorCode {
    fn from(err: io::Error) -> Self {
        Self::IoError(err.kind())
    }
}
