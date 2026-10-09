//
// Copyright (c) 2024-2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::diff_exit_status::DiffExitStatus;

/// How white space takes part in comparing two lines.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WhiteSpace {
    /// Every byte counts.
    Significant,
    /// `-b`: a run of white space equals any other run, and trailing white
    /// space is ignored.
    IgnoreChanges,
    /// `-w` (GNU): white space is ignored wherever it is.
    IgnoreAll,
}

impl WhiteSpace {
    /// `None` when lines are compared byte for byte; otherwise whether the
    /// normal form drops every run of white space (`-w`) rather than folding
    /// it to one space (`-b`).
    pub fn drop_all(self) -> Option<bool> {
        match self {
            WhiteSpace::Significant => None,
            WhiteSpace::IgnoreChanges => Some(false),
            WhiteSpace::IgnoreAll => Some(true),
        }
    }
}

pub struct FormatOptions {
    pub white_space: WhiteSpace,
    pub output_format: OutputFormat,
    /// `-q` (GNU): report only whether files differ.
    pub brief: bool,
    /// `-N` (GNU): a directory entry missing on one side is compared as an
    /// empty file, or as an empty directory.
    pub new_file: bool,
    /// `-s` (GNU): report a pair of files found identical.
    pub report_identical: bool,
    label1: Option<String>,
    label2: Option<String>,
}

impl FormatOptions {
    /// The result for two files found identical: under -s, say so.
    pub fn identical(&self, name1: &str, name2: &str) -> DiffExitStatus {
        if self.report_identical {
            println!("Files {} and {} are identical", name1, name2);
        }
        DiffExitStatus::NotDifferent
    }

    /// Infallible: the labels are validated where they are parsed, so a bad
    /// combination is a usage error with a diagnostic rather than something
    /// every caller has to unwrap.
    pub fn new(
        white_space: WhiteSpace,
        output_format: OutputFormat,
        label1: Option<String>,
        label2: Option<String>,
    ) -> Self {
        Self {
            white_space,
            output_format,
            brief: false,
            new_file: false,
            report_identical: false,
            label1,
            label2,
        }
    }

    pub fn label1(&self) -> &Option<String> {
        &self.label1
    }

    pub fn label2(&self) -> &Option<String> {
        &self.label2
    }
}

pub enum OutputFormat {
    Default,
    Context(usize),
    EditScript,
    ForwardEditScript,
    Unified(usize),
}
