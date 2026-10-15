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
        Prerequisite {
            name: without_dot_slash(name.into()),
        }
    }
}

/// `name` without leading `./` components: `./config.hin` and `config.hin`
/// are one file, so they are one target, as in GNU make. automake names
/// targets `$(srcdir)/file`, which is `./file` when building in the source
/// tree, and depends on them as `file`.
pub fn without_dot_slash(name: String) -> String {
    let rest = dot_slash_stripped(&name);
    match rest.len() == name.len() {
        true => name,
        false => rest.to_string(),
    }
}

/// [`without_dot_slash`], borrowing: the part of `name` after its leading
/// `./` components.
pub fn dot_slash_stripped(name: &str) -> &str {
    let mut rest = name;
    while let Some(stripped) = rest.strip_prefix("./") {
        rest = stripped.trim_start_matches('/');
    }
    match rest.is_empty() && !name.is_empty() {
        true => ".",
        false => rest,
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
