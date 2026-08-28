//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! What `.IGNORE`, `.SILENT`, `.PRECIOUS` and `.PHONY` say about a target.
//!
//! POSIX names these per *target*: "prerequisites of this special target are
//! targets themselves". They used to be flags on the `Rule`, but a rule may
//! name several targets, so `.IGNORE: a` on `a b: dep` silenced `b` as well
//! (audit #78). Keyed by target name, that cannot happen.
//!
//! Whole-makefile forms (`.IGNORE` with no prerequisites) are not stored here;
//! they set the corresponding global option, which is what POSIX says they
//! mean -- "make shall behave as if the -i option had been specified".

use std::collections::HashMap;

/// The attributes of one target.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Flags {
    pub ignore: bool,
    pub silent: bool,
    pub precious: bool,
    pub phony: bool,
}

/// Per-target attributes for the whole makefile.
#[derive(Debug, Clone, Default)]
pub struct Attributes(HashMap<String, Flags>);

impl Attributes {
    /// The attributes of `target`; all false for a target never named.
    pub fn of(&self, target: &str) -> Flags {
        self.0.get(target).copied().unwrap_or_default()
    }

    /// Set an attribute on `target`, leaving its others alone.
    ///
    /// POSIX: "subsequent occurrences shall add to the list", so this
    /// accumulates and nothing here ever clears a flag.
    pub fn mark(&mut self, target: &str, set: impl Fn(&mut Flags)) {
        set(self.0.entry(target.to_string()).or_default());
    }

    pub fn is_phony(&self, target: &str) -> bool {
        self.of(target).phony
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_unnamed_target_has_no_attributes() {
        assert_eq!(Attributes::default().of("a"), Flags::default());
    }

    // Audit #78: the defect was one rule carrying the flag for every target it
    // named. Marking one name must not touch the other.
    #[test]
    fn marking_one_target_leaves_its_siblings_alone() {
        let mut attributes = Attributes::default();
        attributes.mark("a", |f| f.ignore = true);
        assert!(attributes.of("a").ignore);
        assert!(!attributes.of("b").ignore);
    }

    #[test]
    fn attributes_accumulate_on_the_same_target() {
        let mut attributes = Attributes::default();
        attributes.mark("a", |f| f.ignore = true);
        attributes.mark("a", |f| f.silent = true);
        let flags = attributes.of("a");
        assert!(flags.ignore && flags.silent);
        assert!(!flags.phony && !flags.precious);
    }
}
