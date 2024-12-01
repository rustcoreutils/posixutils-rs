//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// The configuration for a rule.
pub struct Config {
    /// Whether to ignore the errors in the rule
    pub ignore: bool,
    /// Whether to print recipe lines
    pub silent: bool,
    /// Whether rule includes phony targets
    pub phony: bool,
    /// Whether rule includes precious targets
    pub precious: bool,
}

#[allow(clippy::derivable_impls)]
impl Default for Config {
    fn default() -> Self {
        Self {
            ignore: false,
            silent: false,
            phony: false,
            precious: false,
        }
    }
}
