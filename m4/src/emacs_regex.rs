//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The regular expressions of `regexp` and `patsubst`.
//!
//! GNU m4 compiles them with GNU regex in its default syntax, which is GNU
//! Emacs's: `\(` `\)` group, `\|` alternates, `*` `+` `?` repeat, and the
//! plain `(` `)` `|` `{` `}` are literal.  `^` and `$` anchor only where they
//! can (a branch's start or end) and are literal elsewhere, as are `*` `+`
//! `?` with nothing to repeat.  Anchors also match at a <newline> inside the
//! text.  A bracket expression takes its contents literally, backslash
//! included, and has no character classes.  `\w` `\W` `\b` `\B` `\<` `\>`
//! `\`` `\'` are the word and buffer operators; any other escaped character
//! stands for itself.
//!
//! [`translate`] rewrites such a pattern into the syntax of the `regex`
//! crate, matching bytes, and [`Regex`] matches it as GNU regex does: of
//! the matches that start at the leftmost position it takes the longest
//! (`\(a\|ab\)` matches `ab` in `abc`), where the `regex` crate takes the
//! first alternative that matches.  Back-references (`\1` in a pattern) are
//! not supported; autoconf uses none.

use std::cell::RefCell;
use std::fmt::Write;
use std::ops::Range;

use regex_automata::nfa::thompson::{self, pikevm, pikevm::PikeVM};
use regex_automata::util::{captures::Captures, syntax};
use regex_automata::{Anchored, Input, MatchKind};

/// A compiled `regexp`/`patsubst` pattern.
///
/// A match is found in two steps.  The `regex` crate finds where the
/// leftmost match starts, which does not depend on which of the matches
/// there is taken.  Then a PikeVM anchored at that start, visiting every
/// match state rather than stopping at the first, runs on until no thread
/// is left and reports the last, longest, end.  Its groups are those of the
/// highest-priority path that reaches that end, as backtracking would give
/// with the overall match fixed: `regexp(abb, \(a\|ab\)\(b*\), \1-\2)` is
/// `a-bb`, as in GNU m4 1.4.19.
pub struct Regex {
    leftmost: regex::bytes::Regex,
    longest: PikeVM,
    cache: RefCell<(pikevm::Cache, Captures)>,
}

impl Regex {
    /// Compile the GNU Emacs-syntax `pattern`, or say why it does not.
    pub fn new(pattern: &[u8]) -> Result<Regex, String> {
        let translated = translate(pattern)?;
        let leftmost = regex::bytes::Regex::new(&translated).map_err(|e| e.to_string())?;
        let longest = PikeVM::builder()
            .syntax(syntax::Config::new().utf8(false))
            .thompson(thompson::Config::new().utf8(false))
            .configure(PikeVM::config().match_kind(MatchKind::All))
            .build(&translated)
            .map_err(|e| e.to_string())?;
        let cache = RefCell::new((longest.create_cache(), longest.create_captures()));
        Ok(Regex {
            leftmost,
            longest,
            cache,
        })
    }

    /// The first match in `haystack` at or after `start`, with the whole of
    /// `haystack` as context for anchors and word boundaries.
    pub fn captures_at(&self, haystack: &[u8], start: usize) -> Option<Groups> {
        let begin = self.leftmost.find_at(haystack, start)?.start();
        let mut cache = self.cache.borrow_mut();
        let (cache, captures) = &mut *cache;
        let input = Input::new(haystack).range(begin..).anchored(Anchored::Yes);
        self.longest.search(cache, &input, captures);
        let spans = (0..captures.group_len())
            .map(|group| captures.get_group(group).map(|span| span.range()))
            .collect::<Vec<_>>();
        // A match starts here, so the anchored search finds one.
        spans.first()?.as_ref()?;
        Some(Groups(spans))
    }
}

/// Where each group of a match is in the haystack, group 0 being the whole
/// match; `None` for a group that took no part in it.
pub struct Groups(Vec<Option<Range<usize>>>);

impl Groups {
    /// The number of groups, the whole match included.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Where group `group` matched, if it did.
    pub fn get(&self, group: usize) -> Option<Range<usize>> {
        self.0.get(group).cloned().flatten()
    }

    /// Where the whole match is.
    pub fn whole(&self) -> Range<usize> {
        self.get(0).expect("group 0 is the match")
    }
}

/// Translate the GNU Emacs-syntax `pattern` into a `regex` crate pattern
/// over bytes, or say why it cannot be.
pub fn translate(pattern: &[u8]) -> Result<String, String> {
    let mut out = String::from("(?m-u)");
    // Where the last repeatable atom begins in `out`, if the previous item
    // was one: a repetition operator applies to it, and is a literal
    // character when there is none.
    let mut atom_start: Option<usize> = None;
    // Whether the last item was a repetition operator. GNU regex lets one
    // repeat another (`a**`), which `regex` refuses unless grouped.
    let mut repeated = false;
    let mut branch_start = true;
    // Open `\(` groups, which GNU regex requires to balance.
    let mut depth = 0usize;
    let mut i = 0;
    while i < pattern.len() {
        let c = pattern[i];
        i += 1;
        let at_branch_start = std::mem::replace(&mut branch_start, false);
        match c {
            b'^' if at_branch_start => {
                out.push('^');
                atom_start = None;
                repeated = false;
                // `^*` repeats nothing: GNU makes that `*` literal.
                continue;
            }
            b'$' if at_branch_end(pattern, i) => {
                out.push('$');
                atom_start = None;
                repeated = false;
                continue;
            }
            b'*' | b'+' | b'?' if atom_start.is_some() => {
                let start = atom_start.expect("checked");
                if repeated {
                    out.insert_str(start, "(?:");
                    out.push(')');
                }
                out.push(c as char);
                repeated = true;
                continue;
            }
            b'[' => {
                let start = out.len();
                i = bracket(pattern, i, &mut out)?;
                atom_start = Some(start);
            }
            b'.' => {
                atom_start = Some(out.len());
                out.push('.');
            }
            b'\\' => {
                let Some(&e) = pattern.get(i) else {
                    return Err(String::from("Trailing backslash"));
                };
                i += 1;
                let start = out.len();
                match e {
                    b'(' => {
                        depth += 1;
                        out.push('(');
                        branch_start = true;
                        atom_start = None;
                        repeated = false;
                        continue;
                    }
                    b')' => {
                        if depth == 0 {
                            return Err(String::from("Unmatched ) or \\)"));
                        }
                        depth -= 1;
                        out.push(')');
                        // The group just closed is what a following `*`
                        // repeats.
                        atom_start = Some(group_start(&out));
                    }
                    b'|' => {
                        out.push('|');
                        branch_start = true;
                        atom_start = None;
                        repeated = false;
                        continue;
                    }
                    b'1'..=b'9' => {
                        return Err(String::from("back-references are not supported"));
                    }
                    b'w' => {
                        out.push_str("[0-9A-Za-z_]");
                        atom_start = Some(start);
                    }
                    b'W' => {
                        out.push_str("[^0-9A-Za-z_]");
                        atom_start = Some(start);
                    }
                    b'b' | b'B' | b'<' | b'>' | b'`' | b'\'' => {
                        out.push_str(match e {
                            b'b' => r"\b",
                            b'B' => r"\B",
                            b'<' => r"\b{start}",
                            b'>' => r"\b{end}",
                            b'`' => r"\A",
                            _ => r"\z",
                        });
                        atom_start = None;
                    }
                    other => {
                        literal(other, &mut out);
                        atom_start = Some(start);
                    }
                }
            }
            other => {
                atom_start = Some(out.len());
                literal(other, &mut out);
            }
        }
        repeated = false;
    }
    if depth > 0 {
        return Err(String::from("Unmatched ( or \\("));
    }
    Ok(out)
}

/// Whether a `$` just consumed, with `i` the index after it, ends a branch:
/// the end of the pattern, or just before `\)` or `\|`.
fn at_branch_end(pattern: &[u8], i: usize) -> bool {
    matches!(pattern.get(i..), Some([]) | Some([b'\\', b')' | b'|', ..]))
}

/// Where the group whose `)` ends `out` begins.
fn group_start(out: &str) -> usize {
    let bytes = out.as_bytes();
    let mut depth = 0usize;
    let mut j = bytes.len();
    while j > 0 {
        j -= 1;
        // Every literal parenthesis was written as `\x28` or `\x29`, and a
        // bracket's as a hex escape too, so a bare one is a group's.
        match bytes[j] {
            b')' => depth += 1,
            b'(' => {
                depth -= 1;
                if depth == 0 {
                    return j;
                }
            }
            _ => {}
        }
    }
    0
}

/// Write the byte `c` as a literal.
fn literal(c: u8, out: &mut String) {
    if c.is_ascii_alphanumeric() {
        out.push(c as char);
    } else {
        write!(out, r"\x{c:02X}").expect("writing to a String");
    }
}

/// Translate the bracket expression whose `[` is just before `pattern[i]`,
/// returning the index after its `]`.
fn bracket(pattern: &[u8], mut i: usize, out: &mut String) -> Result<usize, String> {
    let unmatched = || String::from("Unmatched [ or [^");
    let negated = pattern.get(i) == Some(&b'^');
    if negated {
        i += 1;
    }
    let mut ranges: Vec<(u8, u8)> = Vec::new();
    let mut first = true;
    loop {
        let &c = pattern.get(i).ok_or_else(unmatched)?;
        if c == b']' && !first {
            i += 1;
            break;
        }
        first = false;
        i += 1;
        // `a-z`, unless the `-` is the last thing before `]`.
        match (pattern.get(i), pattern.get(i + 1)) {
            (Some(b'-'), Some(&hi)) if hi != b']' => {
                if hi < c {
                    return Err(String::from("Invalid range end"));
                }
                ranges.push((c, hi));
                i += 2;
            }
            _ => ranges.push((c, c)),
        }
    }
    out.push('[');
    if negated {
        out.push('^');
    }
    for (lo, hi) in ranges {
        write!(out, r"\x{lo:02X}").expect("writing to a String");
        if hi != lo {
            write!(out, r"-\x{hi:02X}").expect("writing to a String");
        }
    }
    out.push(']');
    Ok(i)
}

#[cfg(test)]
mod tests {
    use super::{translate, Regex};

    fn find(pattern: &str, text: &str) -> Option<(usize, usize)> {
        let re = Regex::new(pattern.as_bytes()).unwrap();
        re.captures_at(text.as_bytes(), 0)
            .map(|m| (m.whole().start, m.whole().end))
    }

    /// The text of each group of the first match, `-` for one that took no
    /// part.
    fn groups(pattern: &str, text: &str) -> Vec<String> {
        let re = Regex::new(pattern.as_bytes()).unwrap();
        let m = re.captures_at(text.as_bytes(), 0).unwrap();
        (0..m.len())
            .map(|g| m.get(g).map_or(String::from("-"), |r| text[r].to_string()))
            .collect()
    }

    // GNU regex takes the longest of the matches at the leftmost start.
    #[test]
    fn leftmost_longest() {
        assert_eq!(find(r"a\|ab", "abcd"), Some((0, 2)));
        assert_eq!(find(r"abc\|ab\|abcabc", "xabcabc"), Some((1, 7)));
        assert_eq!(find(r"\(ab\)*\(abc\)*", "abc"), Some((0, 3)));
        // Leftmost first: a later, longer match does not win.
        assert_eq!(find(r"a\|bcd", "abcd"), Some((0, 1)));
        // Anchors and word boundaries see the text beyond the match.
        assert_eq!(find(r"a\|ab\b", "ab"), Some((0, 2)));
        assert_eq!(find(r"a\|ab\b", "abc"), Some((0, 1)));
        assert_eq!(find(r"a\|ab\B", "abc"), Some((0, 2)));
    }

    // With the overall match fixed, groups are those of the first path to
    // it, as GNU m4 1.4.19 gives them.
    #[test]
    fn groups_of_the_longest_match() {
        assert_eq!(groups(r"\(a\|ab\)\(c\|bcd\)", "abcd"), ["abcd", "a", "bcd"]);
        assert_eq!(groups(r"\(a\|ab\)\(b*\)", "abb"), ["abb", "a", "bb"]);
        assert_eq!(groups(r"\(a*\)\(a*\)", "aa"), ["aa", "aa", ""]);
        assert_eq!(groups(r"\(ab\|a\)*", "abab"), ["abab", "ab"]);
        assert_eq!(groups(r"\(x\)\|y", "y"), ["y", "-"]);
    }

    #[test]
    fn groups_and_alternation_are_backslashed() {
        assert_eq!(find(r"\(ab\)*c", "ababc"), Some((0, 5)));
        assert_eq!(find(r"x\|y", "ay"), Some((1, 2)));
        assert_eq!(find("(a|b)", "(a|b)"), Some((0, 5)));
        assert_eq!(find("a{2}", "a{2}"), Some((0, 4)));
    }

    #[test]
    fn anchors_are_literal_where_they_cannot_anchor() {
        assert_eq!(find("a^b", "a^b"), Some((0, 3)));
        assert_eq!(find("a$b", "a$b"), Some((0, 3)));
        assert_eq!(find(r"^a\|^b", "b"), Some((0, 1)));
        assert_eq!(find(r"\(a$\)", "ba"), Some((1, 2)));
        // Anchors match at an embedded <newline>.
        assert_eq!(find("^b", "a\nb"), Some((2, 3)));
    }

    #[test]
    fn repetition_with_nothing_to_repeat_is_literal() {
        assert_eq!(find("*a", "x*a"), Some((1, 3)));
        assert_eq!(find(r"\(*a\)", "*a"), Some((0, 2)));
        assert_eq!(find("^*", "*"), Some((0, 1)));
        assert_eq!(find("a**", "aaa"), Some((0, 3)));
    }

    #[test]
    fn brackets_take_backslash_literally() {
        assert_eq!(find(r"[\\]", "a\\b"), Some((1, 2)));
        assert_eq!(find(r"[^\\]", "\\b"), Some((1, 2)));
        assert_eq!(find("[]a]", "]"), Some((0, 1)));
        assert_eq!(find("[$-]", "-"), Some((0, 1)));
        assert_eq!(find("[a-c]+", "xabcd"), Some((1, 4)));
        // A negated list matches a <newline>, `.` does not.
        assert_eq!(find("[^a]", "\n"), Some((0, 1)));
        assert_eq!(find(".", "\n"), None);
    }

    #[test]
    fn word_operators() {
        assert_eq!(find(r"\bdn[l]\b", "x dnl y"), Some((2, 5)));
        assert_eq!(find(r"\bdn[l]\b", "xdnl"), None);
        assert_eq!(find(r"\<a", "ba a"), Some((3, 4)));
        assert_eq!(find(r"\w+", "  ab_9 "), Some((2, 6)));
    }

    #[test]
    fn errors() {
        assert!(translate(br"a\").is_err());
        assert!(translate(b"[a").is_err());
        assert!(translate(br"\(a\)\1").is_err());
    }
}
