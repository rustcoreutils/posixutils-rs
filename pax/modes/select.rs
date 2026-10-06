//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Selection of archive members by the pattern operands, for list and read
//! mode.
//!
//! POSIX: "the archive members shall be selected based on the user-specified
//! pattern operands as modified by the -c, -n, and -u options. Then, any -s
//! and -i options shall modify, in that order, the names of the selected
//! files." Selection is therefore two steps: [`Selector::select`] decides
//! whether the patterns pick a member, the caller applies `-u`, and
//! [`Selector::take`] records the member as selected. Only a member that is
//! taken uses up its pattern under `-n`, so one that `-u` turns away leaves the
//! pattern free for a later member of the same name.

use crate::archive::ArchiveEntry;
use crate::pattern::{matches_excluded, Name, Pattern};

/// What selects a member.
#[derive(Clone, Copy)]
pub(crate) struct Selection {
    /// The pattern operand that matched it, if one did.
    pattern: Option<usize>,
}

/// Per pattern operand state.
#[derive(Default, Clone)]
struct PatternState {
    /// Some member matched it, so it is not reported as unmatched.
    matched: bool,
    /// `-n`: it has selected its one member.
    taken: bool,
    /// `-n`: the directory it selected, whose hierarchy it still selects.
    dir: Option<Vec<u8>>,
}

/// The pattern operands of list and read mode, and what they have selected.
pub(crate) struct Selector<'a> {
    patterns: &'a [Pattern],
    /// `-c`: select the members the patterns do not match.
    exclude: bool,
    /// `-n`: each pattern selects only the first member it matches.
    first_match: bool,
    /// Unless `-d`, a pattern selecting a directory selects its hierarchy.
    expand_subtree: bool,
    /// tar `--exclude`, independent of the pattern operands.
    exclude_patterns: &'a [Pattern],
    state: Vec<PatternState>,
}

impl<'a> Selector<'a> {
    pub(crate) fn new(
        patterns: &'a [Pattern],
        exclude: bool,
        first_match: bool,
        dir_only: bool,
        exclude_patterns: &'a [Pattern],
    ) -> Self {
        Selector {
            patterns,
            exclude,
            first_match,
            expand_subtree: !dir_only,
            exclude_patterns,
            state: vec![PatternState::default(); patterns.len()],
        }
    }

    /// Whether the patterns select `entry`, before `-u`. Nothing is recorded
    /// against `-n` until the caller [`take`](Self::take)s it.
    pub(crate) fn select(&mut self, entry: &ArchiveEntry) -> Option<Selection> {
        let path = crate::rawpath::as_bytes(&entry.path);

        // tar's exclusion list wins over the pattern operands.
        if matches_excluded(self.exclude_patterns, path) {
            return None;
        }
        if self.patterns.is_empty() {
            return (!self.exclude).then_some(Selection { pattern: None });
        }

        let name = Name::new(path);
        // A name stored as "./x" is also tried as "x".
        let stripped = path.strip_prefix(b"./").map(Name::new);

        if self.exclude {
            // -c: a member any pattern matches is left out. The pattern still
            // matched something, so it is not reported as unmatched.
            let mut any = false;
            for (pattern, state) in self.patterns.iter().zip(&mut self.state) {
                if selects(pattern, &name, stripped.as_ref(), self.expand_subtree) {
                    state.matched = true;
                    any = true;
                }
            }
            return (!any).then_some(Selection { pattern: None });
        }

        for (idx, pattern) in self.patterns.iter().enumerate() {
            let state = &self.state[idx];
            if self.first_match && state.taken {
                // -n: a pattern that selected a directory still selects the
                // file hierarchy rooted at it; otherwise it is used up.
                if state.dir.as_deref().is_some_and(|dir| is_below(path, dir)) {
                    return Some(Selection { pattern: None });
                }
                continue;
            }
            if selects(pattern, &name, stripped.as_ref(), self.expand_subtree) {
                self.state[idx].matched = true;
                return Some(Selection { pattern: Some(idx) });
            }
        }
        None
    }

    /// Record that `entry`, which [`select`](Self::select) picked, has passed
    /// `-u` and is selected.
    pub(crate) fn take(&mut self, selection: Selection, entry: &ArchiveEntry) {
        let Some(idx) = selection.pattern else {
            return;
        };
        let state = &mut self.state[idx];
        state.taken = true;
        if self.first_match && self.expand_subtree && entry.is_dir() {
            let path = crate::rawpath::as_bytes(&entry.path);
            state.dir = Some(trim_dir(path).to_vec());
        }
    }

    /// `-n`: every pattern has selected its member and no directory
    /// hierarchy remains to be selected, so no later member can be.
    pub(crate) fn is_done(&self) -> bool {
        self.first_match
            && !self.exclude
            && !self.patterns.is_empty()
            && self.state.iter().all(|s| s.taken && s.dir.is_none())
    }

    /// Diagnose each pattern operand no archive member matched (POSIX
    /// DESCRIPTION: "If any specified pattern or file operands are not
    /// matched by at least one file or archive member, pax shall write a
    /// diagnostic message to standard error for each one that did not
    /// match"). This holds under `-c` as well: a pattern that excludes
    /// nothing matched nothing.
    pub(crate) fn report_unmatched(&self) {
        for (pattern, state) in self.patterns.iter().zip(&self.state) {
            if !state.matched {
                crate::error::report_error(&pattern.source, gettextrs::gettext("not found"));
            }
        }
    }
}

/// Whether `pattern` selects the member `name`, or its "./"-less spelling.
fn selects(pattern: &Pattern, name: &Name, stripped: Option<&Name>, expand: bool) -> bool {
    pattern.selects(name, expand) || stripped.is_some_and(|s| pattern.selects(s, expand))
}

/// A directory member's name without its trailing slashes or leading "./".
fn trim_dir(path: &[u8]) -> &[u8] {
    let path = path.strip_prefix(b"./").unwrap_or(path);
    let end = path.iter().rposition(|&b| b != b'/').map_or(0, |i| i + 1);
    &path[..end]
}

/// Whether `path` names something below the directory `dir`.
fn is_below(path: &[u8], dir: &[u8]) -> bool {
    let path = path.strip_prefix(b"./").unwrap_or(path);
    path.len() > dir.len() + 1 && path.starts_with(dir) && path[dir.len()] == b'/'
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::archive::EntryType;
    use std::path::PathBuf;

    fn entry(name: &str, entry_type: EntryType) -> ArchiveEntry {
        ArchiveEntry::new(PathBuf::from(name), entry_type)
    }

    #[test]
    fn test_first_match_keeps_the_directory_hierarchy() {
        let patterns = [Pattern::new("d"), Pattern::new("f")];
        let mut sel = Selector::new(&patterns, false, true, false, &[]);
        let mut run = |name: &str, t: EntryType| {
            let e = entry(name, t);
            sel.select(&e).map(|s| sel.take(s, &e)).is_some()
        };
        assert!(run("d/", EntryType::Directory));
        assert!(run("d/x", EntryType::Regular));
        assert!(run("./d/y", EntryType::Regular));
        assert!(!run("dx", EntryType::Regular));
        assert!(run("f", EntryType::Regular));
        assert!(!run("f", EntryType::Regular));
        assert!(!sel.is_done(), "a directory hierarchy is still open");
    }

    #[test]
    fn test_first_match_is_used_only_when_taken() {
        let patterns = [Pattern::new("f")];
        let mut sel = Selector::new(&patterns, false, true, false, &[]);
        let f = entry("f", EntryType::Regular);
        // Selected but turned away (-u): the pattern stays free.
        assert!(sel.select(&f).is_some());
        let s = sel.select(&f).unwrap();
        sel.take(s, &f);
        assert!(sel.is_done());
        assert!(sel.select(&f).is_none());
    }

    #[test]
    fn test_trim_and_below() {
        assert_eq!(trim_dir(b"./d//"), b"d");
        assert!(is_below(b"d/x", b"d"));
        assert!(is_below(b"./d/x", b"d"));
        assert!(!is_below(b"d/", b"d"));
        assert!(!is_below(b"dx/y", b"d"));
    }
}
