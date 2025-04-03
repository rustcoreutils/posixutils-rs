//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use core::fmt;

use crate::special_target::SpecialTarget;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A target for a rule.
pub enum Target {
    Simple {
        name: &'static str,
    },
    Inference {
        name: &'static str,
        from: &'static str,
        to: &'static str,
    },
    Special(SpecialTarget),
}

impl Target {
    /// Creates a new target with the given name.
    pub fn new(name: impl Into<String>) -> Self {
        let name = name.into();

        if let Some(t) = Self::try_parse_special(&name) {
            return t;
        }

        if let Some(t) = Self::try_parse_inference(&name) {
            return t;
        }

        Target::Simple { name: name.leak() }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Target::Simple { name } => name,
            Target::Inference { name, .. } => name,
            Target::Special(target) => match target {
                SpecialTarget::Default => ".DEFAULT",
                SpecialTarget::Ignore => ".IGNORE",
                SpecialTarget::Posix => ".POSIX",
                SpecialTarget::Precious => ".PRECIOUS",
                SpecialTarget::SccsGet => ".SCCS_GET",
                SpecialTarget::Silent => ".SILENT",
                SpecialTarget::Suffixes => ".SUFFIXES",
                SpecialTarget::Phony => ".PHONY",
            },
        }
    }

    fn try_parse_special(name: &str) -> Option<Self> {
        for variant in SpecialTarget::VARIANTS {
            if variant.as_ref() == name {
                return Some(Target::Special(variant));
            }
        }
        None
    }

    fn try_parse_inference(s: &str) -> Option<Self> {
        let mut from = String::new();
        let mut to = String::new();

        let mut source = s.chars().peekable();
        let Some('.') = source.next() else { None? };

        while let Some(c) = source.peek() {
            match c {
                c @ ('0'..='9' | 'a'..='z' | 'A'..='Z' | '_') => from.push(*c),
                '.' => break,
                _ => None?,
            }
            source.next();
        }

        let Some('.') = source.next() else { None? };
        while let Some(c) = source.peek() {
            match c {
                c @ ('0'..='9' | 'a'..='z' | 'A'..='Z' | '_') => to.push(*c),
                '.' | ' ' | '\t' | ':' => break,
                _ => None?,
            }
            source.next();
        }

        Some(Self::Inference {
            name: format!(".{from}.{to}").leak(),
            from: from.leak(),
            to: to.leak(),
        })
    }
}

impl AsRef<str> for Target {
    fn as_ref(&self) -> &'static str {
        self.name()
    }
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.clone().name())
    }
}
