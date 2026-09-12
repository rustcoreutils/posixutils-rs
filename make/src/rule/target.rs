//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::special_target::SpecialTarget;
use core::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A target for a rule.
pub enum Target {
    Simple {
        name: String,
    },
    Inference {
        name: String,
        from: String,
        to: String,
    },
    Special(SpecialTarget),
}

/// Whether a suffix ends at `next`, so that a `~` just consumed was the
/// suffix's own last character rather than one inside a longer name.
///
/// `.c~` and `.c~.o` are suffix rules; `.gitignore~old` is a file called
/// `.gitignore~old`. Taking the `~` as terminal in the second case dropped
/// `old` and registered the rule under a name nothing could ask for.
fn suffix_ends_here(next: Option<&char>) -> bool {
    matches!(next, None | Some('.') | Some(' ') | Some('\t') | Some(':'))
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

        Target::Simple { name }
    }

    pub fn name(&self) -> &str {
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
                SpecialTarget::Wait => ".WAIT",
                SpecialTarget::NotParallel => ".NOTPARALLEL",
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
                // XSI (POSIX 105941): a trailing `~` turns a suffix into a
                // reference to an SCCS file, as in `.c~.o`. It is only ever the
                // last character of a suffix -- accepting it anywhere else
                // would make `.c~.o` ambiguous, and would swallow the rest of
                // an ordinary target name that merely contains a tilde.
                '~' if !from.is_empty() => {
                    from.push('~');
                    source.next();
                    if !suffix_ends_here(source.peek()) {
                        None?
                    }
                    break;
                }
                '.' => break,
                _ => None?,
            }
            source.next();
        }

        // A single-suffix inference rule (`.c:`) has no second suffix: the
        // target produced has an empty suffix. POSIX requires these to be
        // recognized alongside the two-suffix (`.c.o`) form.
        if from.is_empty() {
            None?
        }
        if !matches!(source.peek(), Some('.')) {
            return Some(Self::Inference {
                name: format!(".{from}"),
                from,
                to: String::new(),
            });
        }

        let Some('.') = source.next() else { None? };
        while let Some(c) = source.peek() {
            match c {
                c @ ('0'..='9' | 'a'..='z' | 'A'..='Z' | '_') => to.push(*c),
                '~' if !to.is_empty() => {
                    to.push('~');
                    source.next();
                    if !suffix_ends_here(source.peek()) {
                        None?
                    }
                    break;
                }
                '.' | ' ' | '\t' | ':' => break,
                _ => None?,
            }
            source.next();
        }

        Some(Self::Inference {
            name: format!(".{from}.{to}"),
            from,
            to,
        })
    }
}

impl AsRef<str> for Target {
    fn as_ref(&self) -> &str {
        self.name()
    }
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}
