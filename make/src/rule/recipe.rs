//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

pub mod config;

use core::fmt;
use std::collections::HashSet;

use config::Config;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Prefix {
    Ignore,
    Silent,
    ForceRun,
}

impl Prefix {
    fn get_prefix(c: char) -> Option<Self> {
        match c {
            '-' => Some(Prefix::Ignore),
            '@' => Some(Prefix::Silent),
            '+' => Some(Prefix::ForceRun),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A recipe for a rule.
pub struct Recipe {
    inner: String,

    pub config: Config,
}

impl Recipe {
    /// Creates a new recipe with the given inner recipe.
    ///
    /// Blanks before and among the prefix characters are dropped with them:
    /// the line has already been expanded, and automake's silent rules put a
    /// macro first that expands to nothing, a blank, or `@`
    /// (`$(AM_V_CC)$(COMPILE)`, `$(AM_V_at) rm -f $@`). GNU make reads the
    /// prefixes the same way.
    pub fn new(inner: impl Into<String>) -> Self {
        let inner = inner.into();
        let mut prefixes = HashSet::new();
        let mut start = 0;
        for (i, c) in inner.char_indices() {
            if let Some(prefix) = Prefix::get_prefix(c) {
                prefixes.insert(prefix);
            } else if !matches!(c, ' ' | '\t') {
                start = i;
                break;
            }
            start = i + c.len_utf8();
        }

        Recipe {
            inner: inner[start..].to_string(),
            config: Config::from(prefixes),
        }
    }

    /// Retrieves the inner recipe.
    pub fn inner(&self) -> &str {
        &self.inner
    }
}

impl AsRef<str> for Recipe {
    fn as_ref(&self) -> &str {
        &self.inner
    }
}

impl fmt::Display for Recipe {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}
