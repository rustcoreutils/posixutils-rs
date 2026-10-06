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
use crate::pattern::{matches_excluded, Name, Pattern, Selected};

/// What selects a member.
#[derive(Default)]
pub(crate) struct Selection {
    /// The pattern operands it is the first match for under `-n` -- every one
    /// that matches it otherwise -- each with the hierarchy it goes on to
    /// select under `-n`.
    patterns: Vec<(usize, Option<Hierarchy>)>,
    /// `-n`: the patterns whose hierarchy this member is the directory at the
    /// root of, met after members below it.
    roots: Vec<usize>,
}

/// `-n`: the directory a pattern selected, whose hierarchy it still selects.
#[derive(Clone)]
struct Hierarchy {
    /// Its name, as [`trim_dir`] leaves it.
    dir: Vec<u8>,
    /// The member naming the directory itself has been selected; until it
    /// is, as in an archive listing a directory after its contents, it is
    /// selected when it comes.
    root_seen: bool,
}

/// Per pattern operand state.
#[derive(Default, Clone)]
struct PatternState {
    /// Some member matched it, so it is not reported as unmatched.
    matched: bool,
    /// `-n`: it has selected its one member.
    taken: bool,
    /// `-n`: the directory it selected, whose hierarchy it still selects.
    hierarchy: Option<Hierarchy>,
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
    ///
    /// Every pattern that matches the member is marked matched, not only the
    /// first: overlapping operands (`d d/x`) each match something.
    pub(crate) fn select(&mut self, entry: &ArchiveEntry) -> Option<Selection> {
        let path = crate::rawpath::as_bytes(&entry.path);

        // tar's exclusion list wins over the pattern operands.
        if matches_excluded(self.exclude_patterns, path) {
            return None;
        }
        // No pattern selects every member; under -c, no pattern excepts any.
        if self.patterns.is_empty() {
            return Some(Selection::default());
        }

        let member = Member::new(path, entry.is_dir());
        if self.exclude {
            // -c: a member any pattern matches is left out. The pattern still
            // matched something, so it is not reported as unmatched.
            let mut any = false;
            for (pattern, state) in self.patterns.iter().zip(&mut self.state) {
                if member.selected_by(pattern, self.expand_subtree).is_some() {
                    state.matched = true;
                    any = true;
                }
            }
            return (!any).then(Selection::default);
        }

        let mut selection = Selection::default();
        let mut any = false;
        for (idx, pattern) in self.patterns.iter().enumerate() {
            let state = &mut self.state[idx];
            if self.first_match && state.taken {
                // -n: a pattern that selected a directory still selects the
                // file hierarchy rooted at it; otherwise it is used up.
                match &state.hierarchy {
                    Some(h) if is_below(path, &h.dir) => any = true,
                    Some(h) if !h.root_seen && member.is_dir && trim_dir(path) == h.dir => {
                        selection.roots.push(idx);
                        any = true;
                    }
                    _ => {}
                }
                continue;
            }
            if let Some((how, name)) = member.selected_by(pattern, self.expand_subtree) {
                state.matched = true;
                any = true;
                let hierarchy = self
                    .first_match
                    .then(|| hierarchy_of(how, name, member.is_dir))
                    .flatten();
                selection.patterns.push((idx, hierarchy));
            }
        }
        any.then_some(selection)
    }

    /// Record that `entry`, which [`select`](Self::select) picked, has passed
    /// `-u` and is selected.
    pub(crate) fn take(&mut self, selection: Selection) {
        for (idx, hierarchy) in selection.patterns {
            let state = &mut self.state[idx];
            state.taken = true;
            if self.expand_subtree {
                state.hierarchy = hierarchy;
            }
        }
        for idx in selection.roots {
            if let Some(h) = &mut self.state[idx].hierarchy {
                h.root_seen = true;
            }
        }
    }

    /// `-n`: every pattern has selected its member and no directory
    /// hierarchy remains to be selected, so no later member can be.
    pub(crate) fn is_done(&self) -> bool {
        self.first_match
            && !self.exclude
            && !self.patterns.is_empty()
            && self.state.iter().all(|s| s.taken && s.hierarchy.is_none())
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

/// A member's name, ready for the patterns.
struct Member<'p> {
    path: &'p [u8],
    name: Name<'p>,
    /// A name stored as "./x" is also tried as "x".
    stripped: Option<Name<'p>>,
    is_dir: bool,
}

impl<'p> Member<'p> {
    fn new(path: &'p [u8], is_dir: bool) -> Self {
        Member {
            path,
            name: Name::member(path, is_dir),
            stripped: path.strip_prefix(b"./").map(|p| Name::member(p, is_dir)),
            is_dir,
        }
    }

    /// How `pattern` selects the member, if it does, and the spelling of
    /// its name that it matched.
    fn selected_by(&self, pattern: &Pattern, expand: bool) -> Option<(Selected, &'p [u8])> {
        if let Some(how) = pattern.selects(&self.name, expand) {
            return Some((how, self.path));
        }
        let how = pattern.selects(self.stripped.as_ref()?, expand)?;
        Some((how, &self.path[2..]))
    }
}

/// `-n`: the hierarchy a pattern selecting a member as `how` goes on to
/// select -- the directory it matched, the member's own or one above it.
fn hierarchy_of(how: Selected, name: &[u8], is_dir: bool) -> Option<Hierarchy> {
    let (dir, root_seen) = match how {
        Selected::Itself if is_dir => (name, true),
        Selected::Itself => return None,
        Selected::Below(len) => (&name[..len], false),
    };
    Some(Hierarchy {
        dir: trim_dir(dir).to_vec(),
        root_seen,
    })
}

/// A directory member's name without its trailing slashes or leading "./"
/// ("." for "./" itself; "" for "/").
fn trim_dir(path: &[u8]) -> &[u8] {
    let end = path.iter().rposition(|&b| b != b'/').map_or(0, |i| i + 1);
    let path = &path[..end];
    path.strip_prefix(b"./").unwrap_or(path)
}

/// Whether `path` names something below the directory `dir`.
fn is_below(path: &[u8], dir: &[u8]) -> bool {
    if dir == b"." {
        return path.starts_with(b"./") && trim_dir(path) != b".";
    }
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
            sel.select(&e).map(|s| sel.take(s)).is_some()
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
        sel.take(s);
        assert!(sel.is_done());
        assert!(sel.select(&f).is_none());
    }

    #[test]
    fn test_trim_and_below() {
        assert_eq!(trim_dir(b"./d//"), b"d");
        assert_eq!(trim_dir(b"./"), b".");
        assert!(is_below(b"./x", b"."));
        assert!(!is_below(b"./", b"."));
        assert!(is_below(b"/abs", b""));
        assert!(is_below(b"d/x", b"d"));
        assert!(is_below(b"./d/x", b"d"));
        assert!(!is_below(b"d/", b"d"));
        assert!(!is_below(b"dx/y", b"d"));
    }
}
