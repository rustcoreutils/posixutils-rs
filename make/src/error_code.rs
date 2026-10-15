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
    /// A recipe failed. The target is carried so the diagnostic can name
    /// what did not build; `make: execution error: 1` alone left the user to
    /// guess which of a `-k` build's targets it belonged to (audit #89).
    ExecutionError {
        target: String,
        exit_code: Option<i32>,
    },
    IoError(io::ErrorKind),
    ParserError {
        constraint: ParseError,
    },

    // Specific
    NoMakefile,
    NotUpToDateError {
        target: String,
    },
    /// The makefile names no target to build by default.
    NoTargets,
    /// `target` has no rule, does not exist, and there is no `.DEFAULT`.
    /// `needed_by` is the target it is a prerequisite of, if any.
    NoRule {
        target: String,
        needed_by: Option<String>,
    },
    RecursivePrerequisite {
        origin: String,
    },
    SpecialTargetConstraintNotFulfilled {
        target: String,
        constraint: Error,
    },
    /// A target with both `:` and `::` rules, which GNU make refuses too.
    MixedColons {
        target: String,
    },
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
            MixedColons { .. } => 4,
            NoMakefile => 5,
            NoTargets => 6,
            // GNU make's status, which nothing in POSIX contradicts.
            NoRule { .. } => 2,
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
            ExecutionError { target, exit_code } => match exit_code {
                Some(exit_code) => write!(
                    f,
                    "[{}] {}: {}",
                    target,
                    gettext("execution error"),
                    exit_code
                ),
                None => write!(
                    f,
                    "[{}] {}: {}",
                    target,
                    gettext("execution error"),
                    gettext("terminated by signal"),
                ),
            },
            IoError(err) => write!(f, "{}: {}", gettext("io error"), err),
            NoMakefile => write!(f, "{}", gettext("no makefile")),
            ParserError { constraint } => write!(f, "{}: {}", gettext("parse error"), constraint),
            NoTargets => write!(f, "{}", gettext("no targets to execute")),
            // GNU make's wording, word for word: debhelper decides whether a
            // makefile has a `test` or `distclean` target by matching it in
            // the output of `make -n TARGET` (Debian's `exists_make_target`).
            NoRule {
                target,
                needed_by: None,
            } => write!(
                f,
                "*** {} '{}'.  {}",
                gettext("No rule to make target"),
                target,
                gettext("Stop.")
            ),
            NoRule {
                target,
                needed_by: Some(parent),
            } => write!(
                f,
                "*** {} '{}', {} '{}'.  {}",
                gettext("No rule to make target"),
                target,
                gettext("needed by"),
                parent,
                gettext("Stop.")
            ),
            MixedColons { target } => write!(
                f,
                "{} '{}' {}",
                gettext("target"),
                target,
                gettext("has both : and :: rules")
            ),
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
