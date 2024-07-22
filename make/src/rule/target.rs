//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use core::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A target for a rule.
pub struct Target {
    name: String,
}

impl Target {
    /// Creates a new target with the given name.
    pub fn new(name: impl Into<String>) -> Self {
        Target { name: name.into() }
    }
}

impl AsRef<str> for Target {
    fn as_ref(&self) -> &str {
        &self.name
    }
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
