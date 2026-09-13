//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Key maps (`:map`, `:map!`) and abbreviations (`:ab`), and the matching rule
//! that drives them.
//!
//! Storage and lookup only; the expansion itself belongs to the input pipeline.
//!
//! The one rule worth reading the spec for is **shortest match wins**. POSIX
//! 96622-96632 records that historical vi behaved differently depending on the
//! order two overlapping maps were defined in — with `ab` and `abc` both
//! mapped, entering `ab` either fired immediately or hung waiting for a `c`,
//! depending which was defined first — and concludes: "For consistency and
//! simplicity of specification, POSIX.1-2024 requires that the shortest match
//! be used at all times." So a completed left-hand side fires even when a
//! longer one also starts with it, and [`MapMatch::Partial`] can only ever mean
//! "a strict prefix of something, and equal to nothing".

use crate::input::Key;
use crate::ui::caret_notation;

/// The literal text of a key sequence.
///
/// Every key in a map or abbreviation came from an ex command line through
/// `Key::from_map_char`, so each has a literal character; the replacement is
/// unreachable for those and exists only to keep this total.
fn keys_text(keys: &[Key]) -> String {
    keys.iter()
        .map(|k| k.literal_char().unwrap_or('\u{FFFD}'))
        .collect()
}

/// How many leading keys `lhs` and `rhs` share.
///
/// This is the count of right-hand-side keys that must **not** be remapped:
/// "if the characters in lhs occur as prefix characters in rhs, those
/// characters shall not be remapped" (95120-95121). POSIX 96613-96616 gives
/// the worked example — with `:map ab abcd`, "the characters 'ab' were used as
/// is and were not remapped, but the characters 'cd' were mapped if
/// appropriate".
fn common_prefix_len(lhs: &[Key], rhs: &[Key]) -> usize {
    lhs.iter()
        .zip(rhs.iter())
        .take_while(|(a, b)| a == b)
        .count()
}

/// One `:map` entry.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MapEntry {
    /// The key sequence that triggers the map.
    pub lhs: Vec<Key>,
    /// What is entered in its place.
    pub rhs: Vec<Key>,
    /// Leading `rhs` keys exempt from further mapping (95120-95121).
    pub no_remap_prefix: usize,
}

/// Which mode's map table a command refers to: `:map` or `:map!`.
///
/// POSIX 95089-95092: appending `!` makes the mapping effective "during open or
/// visual text input mode rather than open or visual command mode. This allows
/// lhs to have two different map definitions at the same time".
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MapMode {
    /// `:map` — open and visual command mode.
    Command,
    /// `:map!` — open and visual text input mode.
    Insert,
}

impl MapMode {
    /// `map!` / `unmap!` address the text input mode map list (95090-95092);
    /// without the bang they address the command mode list.
    pub fn for_bang(bang: bool) -> Self {
        if bang {
            MapMode::Insert
        } else {
            MapMode::Command
        }
    }
}

/// The result of probing a table with the keys held so far.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MapMatch {
    /// No entry begins with these keys.
    NoMatch,
    /// A strict prefix of at least one entry, and equal to none. More input is
    /// needed before this can be resolved.
    Partial,
    /// Equal to the entry at this index. Returned even when a longer entry also
    /// starts with these keys — shortest match wins (96632).
    Full(usize),
}

/// The `:map` entries for one mode.
#[derive(Debug, Default, Clone)]
pub struct MapTable {
    /// Kept sorted by the literal text of `lhs`, so listings are deterministic.
    entries: Vec<MapEntry>,
}

impl MapTable {
    /// Define a mapping, replacing any existing one with the same `lhs`.
    pub fn set(&mut self, lhs: Vec<Key>, rhs: Vec<Key>) {
        let no_remap_prefix = common_prefix_len(&lhs, &rhs);
        let entry = MapEntry {
            lhs,
            rhs,
            no_remap_prefix,
        };
        match self.index_of(&entry.lhs) {
            Some(i) => self.entries[i] = entry,
            None => {
                let key = keys_text(&entry.lhs);
                let at = self.entries.partition_point(|e| keys_text(&e.lhs) < key);
                self.entries.insert(at, entry);
            }
        }
    }

    /// Remove a mapping. `false` when there was none, which POSIX makes an
    /// error for `:unmap` (95457-95462).
    pub fn remove(&mut self, lhs: &[Key]) -> bool {
        match self.index_of(lhs) {
            Some(i) => {
                self.entries.remove(i);
                true
            }
            None => false,
        }
    }

    fn index_of(&self, lhs: &[Key]) -> Option<usize> {
        self.entries.iter().position(|e| e.lhs == lhs)
    }

    /// Classify the keys held so far.
    ///
    /// An exact match wins over a longer entry that also starts with `prefix`
    /// (96632), so this checks for equality first and only then for extension.
    pub fn probe(&self, prefix: &[Key]) -> MapMatch {
        if prefix.is_empty() {
            return MapMatch::NoMatch;
        }
        if let Some(i) = self.index_of(prefix) {
            return MapMatch::Full(i);
        }
        if self.entries.iter().any(|e| e.lhs.starts_with(prefix)) {
            return MapMatch::Partial;
        }
        MapMatch::NoMatch
    }

    /// The entry a [`MapMatch::Full`] refers to.
    pub fn entry(&self, index: usize) -> &MapEntry {
        &self.entries[index]
    }

    /// One line per entry for the listing form, `lhs` then `rhs`.
    ///
    /// POSIX says only "write the current list" (95081-95082); the layout is
    /// ours.
    pub fn list(&self) -> Vec<String> {
        self.entries
            .iter()
            .map(|e| {
                format!(
                    "{:<12} {}",
                    caret_notation(&keys_text(&e.lhs)),
                    caret_notation(&keys_text(&e.rhs))
                )
            })
            .collect()
    }
}

/// One `:ab` entry.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AbbrevEntry {
    /// The word that triggers the abbreviation.
    pub lhs: String,
    /// What is entered in its place.
    pub rhs: Vec<Key>,
    /// Leading `rhs` keys exempt from further abbreviation.
    ///
    /// POSIX does not specify this. It is the map prefix rule (95120-95121)
    /// applied by analogy, and the minimal thing that stops `:ab foo foo`
    /// looping while preserving the re-expansion 96473-96476 requires.
    pub no_reabbrev: usize,
}

/// The `:ab` entries.
#[derive(Debug, Default, Clone)]
pub struct AbbrevTable {
    /// Kept sorted by `lhs`, so listings are deterministic.
    entries: Vec<AbbrevEntry>,
}

impl AbbrevTable {
    /// Define an abbreviation, replacing any existing one with the same `lhs`.
    pub fn set(&mut self, lhs: String, rhs: Vec<Key>) {
        let lhs_keys: Vec<Key> = lhs.chars().map(Key::from_map_char).collect();
        let no_reabbrev = common_prefix_len(&lhs_keys, &rhs);
        let entry = AbbrevEntry {
            lhs,
            rhs,
            no_reabbrev,
        };
        match self.entries.iter().position(|e| e.lhs == entry.lhs) {
            Some(i) => self.entries[i] = entry,
            None => {
                let at = self.entries.partition_point(|e| e.lhs < entry.lhs);
                self.entries.insert(at, entry);
            }
        }
    }

    /// Remove an abbreviation. `false` when there was none, which POSIX makes
    /// an error for `:una` (95436-95437).
    pub fn remove(&mut self, lhs: &str) -> bool {
        match self.entries.iter().position(|e| e.lhs == lhs) {
            Some(i) => {
                self.entries.remove(i);
                true
            }
            None => false,
        }
    }

    /// The entry for exactly this word, if any.
    pub fn lookup(&self, word: &str) -> Option<&AbbrevEntry> {
        self.entries.iter().find(|e| e.lhs == word)
    }

    /// One line per entry for the listing form (94864).
    pub fn list(&self) -> Vec<String> {
        self.entries
            .iter()
            .map(|e| {
                format!(
                    "{:<12} {}",
                    caret_notation(&e.lhs),
                    caret_notation(&keys_text(&e.rhs))
                )
            })
            .collect()
    }
}

/// Every mapping table the editor keeps.
#[derive(Debug, Default, Clone)]
pub struct Maps {
    /// `:map` — command mode.
    pub command: MapTable,
    /// `:map!` — text input mode.
    pub insert: MapTable,
    /// `:ab`.
    pub abbrev: AbbrevTable,
}

impl Maps {
    /// The table for one `:map` mode.
    pub fn table(&self, kind: MapMode) -> &MapTable {
        match kind {
            MapMode::Command => &self.command,
            MapMode::Insert => &self.insert,
        }
    }

    /// The table for one `:map` mode, mutably.
    pub fn table_mut(&mut self, kind: MapMode) -> &mut MapTable {
        match kind {
            MapMode::Command => &mut self.command,
            MapMode::Insert => &mut self.insert,
        }
    }
}

/// Turn command-line text into the keys it names.
pub fn keys_from_text(text: &str) -> Vec<Key> {
    text.chars().map(Key::from_map_char).collect()
}

/// The set of characters to look up when an abbreviation check is triggered.
///
/// `log` is this insert session's text input so far, one entry per character
/// with a flag for characters entered literally after a `^V`. The triggering
/// character is *not* in it yet.
///
/// POSIX 94874-94884 defines the set in three cases, by what precedes the word
/// character that the trigger followed:
///
/// 1. nothing — the set is that word character alone;
/// 2. a word character — the set is the run of word characters ending there;
/// 3. anything else — the set is the run of characters that are neither
///    <blank> nor word characters, plus the trailing word character.
///
/// `None` when no check is due: the last input character has to be an
/// unescaped word character, since an escaped one "shall not" take part
/// (94870-94871).
///
/// The rules can only ever produce a string ending in a word character with at
/// most one word/non-word transition in it, which is the shape 96482-96484
/// describes — "the lhs must end with a word character, there can be no
/// transitions from word to non-word ... other than between the last and
/// next-to-last characters, and there can be no <blank> characters". That is
/// why `:ab (p X` fires and `:ab (pp X` never can.
pub fn abbrev_candidate(log: &[(char, bool)]) -> Option<String> {
    let is_word = crate::command::is_word_char;

    let (&(last, escaped), rest) = log.split_last()?;
    if escaped || !is_word(last) {
        return None;
    }

    // Walk back from the character before the trigger word character. A
    // newline ends the set: it is neither a word character nor something that
    // can sensibly join one, and an abbreviation cannot span a line.
    let take_word = matches!(rest.last(), Some(&(c, esc)) if !esc && is_word(c));
    let mut set: Vec<char> = Vec::new();
    for &(c, esc) in rest.iter().rev() {
        if esc || c == '\n' {
            break;
        }
        let keep = if take_word {
            is_word(c)
        } else {
            !is_word(c) && c != ' ' && c != '\t'
        };
        if !keep {
            break;
        }
        set.push(c);
    }
    set.reverse();
    set.push(last);
    Some(set.into_iter().collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn k(s: &str) -> Vec<Key> {
        keys_from_text(s)
    }

    /// POSIX 96622-96632, the case the rationale works through by name: with
    /// `ab` and `abc` both mapped, `ab` must fire immediately. Historical vi
    /// got this right or wrong depending on definition order, so both orders
    /// are asserted -- testing one would pass against the historical bug.
    #[test]
    fn test_shortest_match_wins_in_either_definition_order() {
        for reversed in [false, true] {
            let mut t = MapTable::default();
            if reversed {
                t.set(k("abc"), k("long"));
                t.set(k("ab"), k("short"));
            } else {
                t.set(k("ab"), k("short"));
                t.set(k("abc"), k("long"));
            }
            match t.probe(&k("ab")) {
                MapMatch::Full(i) => assert_eq!(
                    t.entry(i).rhs,
                    k("short"),
                    "reversed={reversed}: `ab` must take the shorter map"
                ),
                other => panic!("reversed={reversed}: expected Full, got {other:?}"),
            }
        }
    }

    /// A strict prefix of something and equal to nothing is the only case that
    /// has to wait for more input.
    #[test]
    fn test_probe_partial_and_no_match() {
        let mut t = MapTable::default();
        t.set(k("abc"), k("X"));
        assert_eq!(t.probe(&k("a")), MapMatch::Partial);
        assert_eq!(t.probe(&k("ab")), MapMatch::Partial);
        assert!(matches!(t.probe(&k("abc")), MapMatch::Full(_)));
        assert_eq!(t.probe(&k("b")), MapMatch::NoMatch);
        assert_eq!(t.probe(&k("abd")), MapMatch::NoMatch);
        assert_eq!(t.probe(&[]), MapMatch::NoMatch);
    }

    /// POSIX 96613-96616's worked example: `:map ab abcd` leaves `ab` alone and
    /// remaps `cd`.
    #[test]
    fn test_no_remap_prefix_is_the_common_prefix() {
        let mut t = MapTable::default();
        t.set(k("ab"), k("abcd"));
        let MapMatch::Full(i) = t.probe(&k("ab")) else {
            panic!("expected a match")
        };
        assert_eq!(t.entry(i).no_remap_prefix, 2);

        // No shared prefix, so every key of the rhs may be remapped.
        t.set(k("q"), k("dd"));
        let MapMatch::Full(i) = t.probe(&k("q")) else {
            panic!("expected a match")
        };
        assert_eq!(t.entry(i).no_remap_prefix, 0);

        // Wholly self-referential: nothing in the rhs is eligible.
        t.set(k("x"), k("xx"));
        let MapMatch::Full(i) = t.probe(&k("x")) else {
            panic!("expected a match")
        };
        assert_eq!(t.entry(i).no_remap_prefix, 1);
    }

    #[test]
    fn test_set_replaces_and_remove_reports_absence() {
        let mut t = MapTable::default();
        t.set(k("q"), k("one"));
        t.set(k("q"), k("two"));
        assert_eq!(t.list().len(), 1, "redefining must not add a second entry");
        let MapMatch::Full(i) = t.probe(&k("q")) else {
            panic!("expected a match")
        };
        assert_eq!(t.entry(i).rhs, k("two"));

        assert!(t.remove(&k("q")));
        assert!(!t.remove(&k("q")), "removing twice must report the absence");
        assert_eq!(t.probe(&k("q")), MapMatch::NoMatch);
    }

    /// The two tables are independent, which is what 95089-95092 means by "lhs
    /// to have two different map definitions at the same time".
    #[test]
    fn test_command_and_insert_tables_are_independent() {
        let mut m = Maps::default();
        m.table_mut(MapMode::Command).set(k("q"), k("dd"));
        m.table_mut(MapMode::Insert).set(k("q"), k("xyz"));

        let MapMatch::Full(i) = m.table(MapMode::Command).probe(&k("q")) else {
            panic!("expected a command-mode match")
        };
        assert_eq!(m.table(MapMode::Command).entry(i).rhs, k("dd"));

        let MapMatch::Full(i) = m.table(MapMode::Insert).probe(&k("q")) else {
            panic!("expected a text-input-mode match")
        };
        assert_eq!(m.table(MapMode::Insert).entry(i).rhs, k("xyz"));

        assert!(m.table_mut(MapMode::Command).remove(&k("q")));
        assert!(
            matches!(m.table(MapMode::Insert).probe(&k("q")), MapMatch::Full(_)),
            "removing from one table must not touch the other"
        );
    }

    /// Listings are sorted and use caret notation, so a control character in
    /// either side is visible rather than being written raw to the terminal.
    #[test]
    fn test_listing_is_sorted_and_uses_caret_notation() {
        let mut t = MapTable::default();
        t.set(k("z"), k("last"));
        t.set(k("a"), k("first"));
        t.set(k("Q"), k(":wq\r"));
        let out = t.list();
        assert_eq!(out.len(), 3);
        assert!(out[0].starts_with("Q "), "sorted by lhs: {out:?}");
        assert!(out[0].contains(":wq^M"), "CR shown as ^M: {out:?}");
        assert!(out[1].starts_with("a "), "{out:?}");
        assert!(out[2].starts_with("z "), "{out:?}");
    }

    /// POSIX 94875-94884 defines the checked set in three cases. 96489-96495
    /// then gives worked examples of which abbreviations can and cannot fire,
    /// which is unusually direct test data: `:ab (p`, `:ab p` and `:ab ((p`
    /// work, `:ab (` and `:ab (pp` never do.
    #[test]
    fn test_abbrev_candidate_rules() {
        let log = |s: &str| -> Vec<(char, bool)> { s.chars().map(|c| (c, false)).collect() };

        // Rule 1 (94875-94876): nothing before the word character.
        assert_eq!(abbrev_candidate(&log("a")).as_deref(), Some("a"));

        // Rule 2 (94877-94880): a word character before it, so the set is the
        // run of word characters.
        assert_eq!(abbrev_candidate(&log("foo")).as_deref(), Some("foo"));
        assert_eq!(abbrev_candidate(&log("x foo")).as_deref(), Some("foo"));
        assert_eq!(abbrev_candidate(&log("(foo")).as_deref(), Some("foo"));

        // Rule 3 (94881-94884): a non-word, non-<blank> character before it, so
        // the set is that run plus the trailing word character. This is what
        // makes `(p` and `((p` reachable.
        assert_eq!(abbrev_candidate(&log("(p")).as_deref(), Some("(p"));
        assert_eq!(abbrev_candidate(&log("((p")).as_deref(), Some("((p"));
        assert_eq!(abbrev_candidate(&log("x ((p")).as_deref(), Some("((p"));

        // The shapes 96482-96484 says can never be produced, and so can never
        // fire: a set that does not end in a word character, and one with a
        // word/non-word transition anywhere but at the end.
        assert_eq!(abbrev_candidate(&log("(")), None, "must end in a word char");
        assert_ne!(abbrev_candidate(&log("(pp")).as_deref(), Some("(pp"));
        assert_eq!(abbrev_candidate(&log("(pp")).as_deref(), Some("pp"));

        // Nothing to check.
        assert_eq!(abbrev_candidate(&[]), None);
        assert_eq!(abbrev_candidate(&log(" ")), None);
    }

    /// 94870-94871: a character escaped by a `^V` takes no part -- neither as
    /// the trigger's predecessor nor inside the set.
    #[test]
    fn test_abbrev_candidate_stops_at_an_escaped_character() {
        // The final `o` was entered literally, so no check is due at all.
        let escaped_last = vec![('f', false), ('o', false), ('o', true)];
        assert_eq!(abbrev_candidate(&escaped_last), None);

        // An escaped character inside the run ends the set before it.
        let escaped_mid = vec![('f', true), ('o', false), ('o', false)];
        assert_eq!(abbrev_candidate(&escaped_mid).as_deref(), Some("oo"));
    }

    /// An abbreviation cannot span a line.
    #[test]
    fn test_abbrev_candidate_stops_at_a_newline() {
        let log = vec![('a', false), ('\n', false), ('b', false), ('c', false)];
        assert_eq!(abbrev_candidate(&log).as_deref(), Some("bc"));
    }

    #[test]
    fn test_abbrev_table() {
        let mut t = AbbrevTable::default();
        t.set("teh".to_string(), k("the"));
        t.set("foo".to_string(), k("bar"));
        assert_eq!(t.lookup("teh").map(|e| e.rhs.clone()), Some(k("the")));
        assert!(t.lookup("other").is_none());

        let out = t.list();
        assert!(out[0].starts_with("foo "), "sorted by lhs: {out:?}");
        assert!(out[1].starts_with("teh "), "{out:?}");

        assert!(t.remove("foo"));
        assert!(!t.remove("foo"));

        // The self-referential case the prefix rule exists to stop.
        t.set("x".to_string(), k("xy"));
        assert_eq!(t.lookup("x").unwrap().no_reabbrev, 1);
        t.set("a".to_string(), k("bc"));
        assert_eq!(t.lookup("a").unwrap().no_reabbrev, 0);
    }
}
