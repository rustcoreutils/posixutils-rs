//
// Copyright (c) 2024-2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

pub struct FormatOptions {
    pub ignore_trailing_white_spaces: bool,
    pub output_format: OutputFormat,
    label1: Option<String>,
    label2: Option<String>,
}

impl FormatOptions {
    /// Infallible: the labels are validated where they are parsed, so a bad
    /// combination is a usage error with a diagnostic rather than something
    /// every caller has to unwrap.
    pub fn new(
        ignore_trailing_white_spaces: bool,
        output_format: OutputFormat,
        label1: Option<String>,
        label2: Option<String>,
    ) -> Self {
        Self {
            ignore_trailing_white_spaces,
            output_format,
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
