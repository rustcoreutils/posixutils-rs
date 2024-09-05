//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

/// Represents the configuration of the make utility
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    /// Whether to ignore the errors in the rule
    pub ignore: bool,
    /// Whether to execute commands or print to stdout
    pub dry_run: bool,
    /// Whether to print recipe lines
    pub silent: bool,
    /// Whether to touch targets on execution
    pub touch: bool,
}

#[allow(clippy::derivable_impls)]
impl Default for Config {
    fn default() -> Self {
        Self {
            ignore: false,
            dry_run: false,
            silent: false,
            touch: false,
        }
    }
}
