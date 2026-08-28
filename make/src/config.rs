//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
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
    /// Whether to replace macros within makefiles with envs
    pub env_macros: bool,
    /// Whether to quit without build
    pub quit: bool,
    /// Whether to keep going build targets and write info about errors stderr
    pub keep_going: bool,
    /// Whether to terminate on error
    pub terminate: bool,
    /// Whether to clear default_rules
    pub clear: bool,
    /// Whether to print macro definitions and target descriptions.
    pub print: bool,
    /// Whether to not delete interrupted files on async events.
    pub precious: bool,
    /// Maximum number of targets to update concurrently (`-j`); 1 = sequential.
    pub jobs: usize,
    /// Whether `.NOTPARALLEL` was specified (forces sequential builds).
    pub not_parallel: bool,

    /// The `.SUFFIXES` list, kept in declaration (insertion) order, which
    /// defines the inference-rule search order (POSIX). This is the
    /// authoritative store.
    pub suffixes: Vec<String>,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            ignore: false,
            dry_run: false,
            silent: false,
            touch: false,
            env_macros: false,
            keep_going: false,
            quit: false,
            clear: false,
            print: false,
            precious: false,
            jobs: 1,
            not_parallel: false,
            terminate: true,
            suffixes: [
                ".o", ".c", ".y", ".l", ".a", ".sh", ".c~", ".y~", ".l~", ".sh~",
            ]
            .into_iter()
            .map(String::from)
            .collect(),
        }
    }
}

impl Config {
    /// Adds a new suffix to the `.SUFFIXES` list, preserving insertion order and
    /// avoiding duplicates.
    pub fn add_suffix(&mut self, new_suffix: &str) {
        if !self.suffixes.iter().any(|s| s == new_suffix) {
            self.suffixes.push(new_suffix.to_string());
        }
    }

    /// Clears the `.SUFFIXES` list (an empty `.SUFFIXES:` special target).
    pub fn clear_suffixes(&mut self) {
        self.suffixes.clear();
    }
}
