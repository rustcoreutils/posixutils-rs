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
    pub fn new(inner: impl Into<String>) -> Self {
        let mut inner = inner.into();
        let prefixes = inner
            .chars()
            .map_while(Prefix::get_prefix)
            .collect::<Vec<_>>();

        // remove the prefix from the inner;
        inner.replace_range(..prefixes.len(), "");

        Recipe {
            inner,
            config: Config::from(prefixes.into_iter().collect::<HashSet<_>>()),
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
