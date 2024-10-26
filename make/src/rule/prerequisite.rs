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
/// A prerequisite for a rule.
pub struct Prerequisite {
    name: String,
}

impl Prerequisite {
    /// Creates a new prerequisite with the given name.
    pub fn new(name: impl Into<String>) -> Self {
        Prerequisite { name: name.into() }
    }
}

impl AsRef<str> for Prerequisite {
    fn as_ref(&self) -> &str {
        &self.name
    }
}

impl fmt::Display for Prerequisite {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
