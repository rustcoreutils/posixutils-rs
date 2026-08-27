//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Matching of shell patterns (POSIX XCU 2.14) directly against bytes.
//!
//! A shell pattern is a *linear* sequence of items whose only nondeterminism is
//! `*`, so it is matched by simulating a Thompson-style state set over the
//! subject: `O(subject × pattern)` time and `O(pattern)` space, with no input
//! that makes it blow up. `a*a*a*b` against a long non-matching subject costs
//! the same as any other pattern of that length, because the state set
//! deduplicates the backtracking frontier by construction.
//!
//! Simulating the whole set also answers the question the affix-removal
//! operators actually ask. One left-to-right pass yields *every* position at
//! which a match can end, so `${x#pat}` and `${x##pat}` are the first and last
//! of those, rather than the repeated truncate-and-retry the previous
//! regex-based implementation needed.

use crate::pattern::parse::{BracketExpression, BracketItem, PatternItem, RangeEndpoint};
use std::ops::Range;

/// One element of the subject: a character, or a single byte that is not part
/// of a valid one. POSIX (XCU 2.6.5) is explicit that the shell processes
/// arbitrary bytes, so a subject that is not valid text still has to match `*`,
/// `?` and a literal of the same byte.
#[derive(Debug, Clone)]
pub struct Step {
    pub range: Range<usize>,
    pub ch: Option<char>,
}

/// Splits `subject` into steps, decoding what it can. `?` matches one *step*,
/// so a multi-byte character counts as one and removal never cuts one in half.
pub fn steps(subject: &[u8]) -> Vec<Step> {
    let mut result = Vec::with_capacity(subject.len());
    let mut i = 0;
    while i < subject.len() {
        match std::str::from_utf8(&subject[i..]) {
            Ok(valid) => {
                for c in valid.chars() {
                    let len = c.len_utf8();
                    result.push(Step {
                        range: i..i + len,
                        ch: Some(c),
                    });
                    i += len;
                }
            }
            Err(err) => {
                let good = err.valid_up_to();
                if good > 0 {
                    // Safe: `valid_up_to` is a character boundary.
                    let valid = std::str::from_utf8(&subject[i..i + good]).unwrap();
                    for c in valid.chars() {
                        let len = c.len_utf8();
                        result.push(Step {
                            range: i..i + len,
                            ch: Some(c),
                        });
                        i += len;
                    }
                }
                // The offending byte stands for itself.
                result.push(Step {
                    range: i..i + 1,
                    ch: None,
                });
                i += 1;
            }
        }
    }
    result
}

// The wide-character classification functions, which the `libc` crate does not
// declare. They are used rather than Rust's `char::is_*` because the shell
// calls `setlocale` at startup and sorts with `strcoll`: classification has to
// follow the locale like the rest of the crate, not Unicode regardless of it.
/// `wint_t`, which the `libc` crate does not define. It is `unsigned int` on
/// Linux and macOS alike.
#[allow(non_camel_case_types)]
type WInt = libc::c_uint;

extern "C" {
    fn iswalpha(wc: WInt) -> libc::c_int;
    fn iswalnum(wc: WInt) -> libc::c_int;
    fn iswblank(wc: WInt) -> libc::c_int;
    fn iswcntrl(wc: WInt) -> libc::c_int;
    fn iswdigit(wc: WInt) -> libc::c_int;
    fn iswgraph(wc: WInt) -> libc::c_int;
    fn iswlower(wc: WInt) -> libc::c_int;
    fn iswprint(wc: WInt) -> libc::c_int;
    fn iswpunct(wc: WInt) -> libc::c_int;
    fn iswspace(wc: WInt) -> libc::c_int;
    fn iswupper(wc: WInt) -> libc::c_int;
    fn iswxdigit(wc: WInt) -> libc::c_int;
}

fn is_in_class(class: &str, c: char) -> Option<bool> {
    let wc = c as WInt;
    let result = unsafe {
        match class {
            "alpha" => iswalpha(wc),
            "alnum" => iswalnum(wc),
            "blank" => iswblank(wc),
            "cntrl" => iswcntrl(wc),
            "digit" => iswdigit(wc),
            "graph" => iswgraph(wc),
            "lower" => iswlower(wc),
            "print" => iswprint(wc),
            "punct" => iswpunct(wc),
            "space" => iswspace(wc),
            "upper" => iswupper(wc),
            "xdigit" => iswxdigit(wc),
            _ => return None,
        }
    };
    Some(result != 0)
}

/// The 12 classes POSIX defines. An unknown name is an error at construction
/// time, matching what the regex engine used to report.
pub fn is_known_class(class: &str) -> bool {
    matches!(
        class,
        "alpha"
            | "alnum"
            | "blank"
            | "cntrl"
            | "digit"
            | "graph"
            | "lower"
            | "print"
            | "punct"
            | "space"
            | "upper"
            | "xdigit"
    )
}

/// A collating symbol or equivalence class holding exactly one character stands
/// for that character. No locale on the supported platforms defines a
/// multi-character collating element.
fn single_char(name: &str) -> Option<char> {
    let mut chars = name.chars();
    match (chars.next(), chars.next()) {
        (Some(c), None) => Some(c),
        _ => None,
    }
}

fn endpoint_char(endpoint: &RangeEndpoint) -> Option<char> {
    match endpoint {
        RangeEndpoint::Char(c) => Some(*c),
        RangeEndpoint::CollatingSymbol(s) => single_char(s),
    }
}

fn bracket_matches(expr: &BracketExpression, step: &Step, subject: &[u8]) -> bool {
    let matched = expr.items.iter().any(|item| match item {
        BracketItem::Char(c) => step.ch == Some(*c),
        BracketItem::CollatingSymbol(s) | BracketItem::EquivalenceClass(s) => {
            single_char(s).is_some_and(|c| step.ch == Some(c))
        }
        BracketItem::CharacterClass(class) => {
            // A byte that does not form a character belongs to no class.
            step.ch.and_then(|c| is_in_class(class, c)).unwrap_or(false)
        }
        BracketItem::RangeExpression(start, end) => {
            match (endpoint_char(start), endpoint_char(end), step.ch) {
                (Some(lo), Some(hi), Some(c)) => lo <= c && c <= hi,
                // A range over bytes that are not characters compares by byte,
                // which is what the C locale does anyway.
                (None, _, _) | (_, None, _) | (_, _, None) => {
                    let byte = subject[step.range.start];
                    matches!(
                        (endpoint_byte(start), endpoint_byte(end)),
                        (Some(lo), Some(hi)) if lo <= byte && byte <= hi
                    )
                }
            }
        }
    });
    matched == expr.matching
}

fn endpoint_byte(endpoint: &RangeEndpoint) -> Option<u8> {
    endpoint_char(endpoint).and_then(|c| {
        let mut buf = [0u8; 4];
        let encoded = c.encode_utf8(&mut buf);
        (encoded.len() == 1).then(|| buf[0])
    })
}

fn item_matches(item: &PatternItem, step: &Step, subject: &[u8]) -> bool {
    match item {
        PatternItem::Char(c) => step.ch == Some(*c),
        // Matches that exact byte, which by construction is one that does not
        // form a character.
        PatternItem::Byte(b) => {
            step.ch.is_none() && step.range.len() == 1 && subject[step.range.start] == *b
        }
        PatternItem::QuestionMark => true,
        // Handled by the state set, never asked directly.
        PatternItem::Asterisk => false,
        PatternItem::BracketExpression(expr) => bracket_matches(expr, step, subject),
    }
}

/// Propagates the active set through `*`, which may match nothing.
fn close(active: &mut [bool], items: &[&PatternItem]) {
    for j in 0..items.len() {
        if active[j] && matches!(items[j], PatternItem::Asterisk) {
            active[j + 1] = true;
        }
    }
}

/// `result[k]` is true when `items` can match exactly the first `k` steps.
///
/// Items are borrowed so that the suffix pass can hand over a reversed view
/// without cloning; `PatternItem` owns its bracket expressions.
fn reachable_ends(items: &[&PatternItem], steps: &[Step], subject: &[u8]) -> Vec<bool> {
    let m = items.len();
    let mut active = vec![false; m + 1];
    active[0] = true;
    close(&mut active, items);

    let mut ends = vec![false; steps.len() + 1];
    ends[0] = active[m];

    for (i, step) in steps.iter().enumerate() {
        let mut next = vec![false; m + 1];
        for j in 0..m {
            if !active[j] {
                continue;
            }
            if matches!(items[j], PatternItem::Asterisk) {
                // `*` consumes this step and stays where it is.
                next[j] = true;
            } else if item_matches(items[j], step, subject) {
                next[j + 1] = true;
            }
        }
        active = next;
        close(&mut active, items);
        ends[i + 1] = active[m];
    }
    ends
}

/// Rejects a pattern the engine cannot honour. Only character-class names can
/// be wrong; the previous implementation got this from `regcomp`'s REG_ECTYPE,
/// and callers rely on `Pattern::new` failing rather than silently matching
/// nothing.
pub fn validate(items: &[PatternItem]) -> Result<(), String> {
    for item in items {
        if let PatternItem::BracketExpression(expr) = item {
            for member in &expr.items {
                if let BracketItem::CharacterClass(class) = member {
                    if !is_known_class(class) {
                        return Err(format!("invalid character class '{class}'"));
                    }
                }
            }
        }
    }
    Ok(())
}

fn forward(items: &[PatternItem]) -> Vec<&PatternItem> {
    items.iter().collect()
}

/// Does the pattern match the whole subject? This is the question `case` asks,
/// and the one affix removal asks about each candidate.
pub fn matches_whole(items: &[PatternItem], subject: &[u8]) -> bool {
    let steps = steps(subject);
    reachable_ends(&forward(items), &steps, subject)[steps.len()]
}

/// Byte offset just past the shortest prefix of `subject` that the pattern
/// matches in its entirety, or `None` when it matches no prefix.
pub fn shortest_prefix_end(items: &[PatternItem], subject: &[u8]) -> Option<usize> {
    prefix_end(items, subject, false)
}

/// As [`shortest_prefix_end`], for the longest such prefix.
pub fn longest_prefix_end(items: &[PatternItem], subject: &[u8]) -> Option<usize> {
    prefix_end(items, subject, true)
}

fn prefix_end(items: &[PatternItem], subject: &[u8], longest: bool) -> Option<usize> {
    let steps = steps(subject);
    let ends = reachable_ends(&forward(items), &steps, subject);
    let k = pick(&ends, steps.len(), longest)?;
    Some(if k == steps.len() {
        subject.len()
    } else {
        steps[k].range.start
    })
}

/// Byte offset at which the shortest matching suffix begins.
pub fn shortest_suffix_start(items: &[PatternItem], subject: &[u8]) -> Option<usize> {
    suffix_start(items, subject, false)
}

/// Byte offset at which the longest matching suffix begins.
pub fn longest_suffix_start(items: &[PatternItem], subject: &[u8]) -> Option<usize> {
    suffix_start(items, subject, true)
}

/// Suffixes are prefixes read the other way round. Every pattern item is
/// position-symmetric, so reversing the items and the *steps* — never the
/// bytes, which would shred multi-byte characters — reuses the same simulation.
fn suffix_start(items: &[PatternItem], subject: &[u8], longest: bool) -> Option<usize> {
    let steps = steps(subject);
    let reversed_steps: Vec<Step> = steps.iter().rev().cloned().collect();
    let reversed_items: Vec<&PatternItem> = items.iter().rev().collect();
    let ends = reachable_ends(&reversed_items, &reversed_steps, subject);
    // `k` steps consumed, counting from the end of the subject.
    let k = pick(&ends, steps.len(), longest)?;
    Some(if k == 0 {
        subject.len()
    } else {
        steps[steps.len() - k].range.start
    })
}

fn pick(ends: &[bool], max: usize, longest: bool) -> Option<usize> {
    if longest {
        (0..=max).rev().find(|&k| ends[k])
    } else {
        (0..=max).find(|&k| ends[k])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pattern::parse::parse_pattern;
    use crate::wordexp::expanded_word::ExpandedWord;

    fn items(pattern: &str) -> Vec<PatternItem> {
        parse_pattern(&ExpandedWord::unquoted_literal(pattern), false)
            .expect("failed to parse pattern")
    }

    fn matches(pattern: &str, subject: &str) -> bool {
        matches_whole(&items(pattern), subject.as_bytes())
    }

    // The tests below replace the `convert_*` tests of the deleted regex
    // translation layer, which asserted the *spelling* of a generated BRE. They
    // assert matching behavior instead, which is the contract callers depend on.

    #[test]
    fn empty_pattern_matches_only_the_empty_subject() {
        assert!(matches("", ""));
        assert!(!matches("", "a"));
    }

    #[test]
    fn literal_characters_match_themselves_and_nothing_else() {
        assert!(matches("a", "a"));
        assert!(!matches("a", "b"));
        assert!(matches("abc", "abc"));
        assert!(!matches("abc", "ab"));
        assert!(!matches("abc", "abcd"));
    }

    #[test]
    fn a_pattern_must_match_the_whole_subject() {
        assert!(!matches("a", "ab"));
        assert!(!matches("b", "abc"));
        assert!(!matches("oo", "foobar"));
    }

    #[test]
    fn question_mark_matches_exactly_one_character() {
        assert!(matches("?", "a"));
        assert!(!matches("?", ""));
        assert!(!matches("?", "ab"));
        assert!(matches("a?c", "abc"));
        // One *character*, not one byte.
        assert!(matches("?", "é"));
        assert!(matches("h?llo", "héllo"));
    }

    #[test]
    fn asterisk_matches_any_run_including_none() {
        assert!(matches("*", ""));
        assert!(matches("*", "anything at all"));
        assert!(matches("a*", "a"));
        assert!(matches("a*c", "abbbc"));
        assert!(matches("*.c", "main.c"));
        assert!(!matches("*.c", "main.h"));
    }

    #[test]
    fn many_asterisks_do_not_blow_up() {
        // A greedy matcher with backtracking goes exponential here; the state
        // set does not. If this ever regresses, the test hangs.
        let subject = "a".repeat(10_000);
        assert!(!matches("a*a*a*a*a*b", &subject));
        assert!(matches("a*a*a*a*a*a", &subject));
    }

    #[test]
    fn bracket_expression_matches_any_member() {
        assert!(matches("[abc]", "b"));
        assert!(!matches("[abc]", "d"));
        assert!(matches("[a]", "a"));
        assert!(matches("x[abc]z", "xbz"));
    }

    #[test]
    fn negated_bracket_expression_matches_a_non_member() {
        assert!(matches("[!abc]", "d"));
        assert!(!matches("[!abc]", "a"));
        assert!(matches("[!a]", "b"));
    }

    #[test]
    fn bracket_ranges_match_between_their_endpoints() {
        assert!(matches("[a-z]", "m"));
        assert!(!matches("[a-z]", "M"));
        assert!(matches("[0-9]", "5"));
        assert!(!matches("[0-9]", "a"));
        assert!(matches("[!0-9]", "a"));
    }

    #[test]
    fn character_classes_match_their_class() {
        assert!(matches("[[:digit:]]", "7"));
        assert!(!matches("[[:digit:]]", "a"));
        assert!(matches("[[:alpha:]]", "q"));
        assert!(!matches("[[:alpha:]]", "1"));
        assert!(matches("[[:space:]]", " "));
        assert!(matches("[[:upper:]]", "Q"));
        assert!(!matches("[[:upper:]]", "q"));
        assert!(matches("[[:xdigit:]]", "f"));
        assert!(!matches("[[:xdigit:]]", "g"));
        assert!(matches("[![:digit:]]", "a"));
    }

    #[test]
    fn a_bracket_may_mix_members_and_classes() {
        assert!(matches("[x[:digit:]y]", "3"));
        assert!(matches("[x[:digit:]y]", "x"));
        assert!(matches("[x[:digit:]y]", "y"));
        assert!(!matches("[x[:digit:]y]", "z"));
        assert!(matches("[!x[:digit:]y]", "z"));
        assert!(!matches("[!x[:digit:]y]", "3"));
    }

    #[test]
    fn an_unknown_character_class_is_rejected() {
        assert!(validate(&items("[[:nosuchclass:]]")).is_err());
        assert!(validate(&items("[[:digit:]]")).is_ok());
    }

    #[test]
    fn a_single_character_collating_symbol_is_that_character() {
        assert!(matches("[[.a.]]", "a"));
        assert!(!matches("[[.a.]]", "b"));
        assert!(matches("[[=a=]]", "a"));
    }

    #[test]
    fn regex_metacharacters_are_literal_in_a_shell_pattern() {
        // The old translation layer had to escape these; here they never had
        // any meaning to escape away.
        for c in [".", "^", "$", "+", "(", ")", "|", "{", "}"] {
            assert!(matches(c, c), "{c} should match itself");
            assert!(!matches(c, "x"), "{c} should not match x");
        }
        // Inside a bracket they are ordinary members too.
        assert!(matches("[.*^]", "^"));
        assert!(matches("[.*^]", "."));
        assert!(matches("[.*^]", "*"));
        assert!(!matches("[.*^]", "a"));
    }

    #[test]
    fn bytes_that_are_not_characters_still_match() {
        // A file name need not be valid text. `*` and `?` must still match it,
        // and it must not silently become the empty string.
        let subject = b"pre\xffpost";
        assert!(matches_whole(&items("*"), subject));
        assert!(matches_whole(&items("pre?post"), subject));
        assert!(matches_whole(&items("pre*post"), subject));
        assert!(!matches_whole(&items("pre?post"), b"prepost"));
        // It belongs to no character class...
        assert!(!matches_whole(&items("pre[[:alpha:]]post"), subject));
        // ...but a negated bracket still accepts it.
        assert!(matches_whole(&items("pre[!a]post"), subject));
    }

    #[test]
    fn affix_removal_reports_every_reachable_boundary() {
        let p = items("*b");
        assert_eq!(shortest_prefix_end(&p, b"abaaaaabtest"), Some(2));
        assert_eq!(longest_prefix_end(&p, b"abaaaaabtest"), Some(8));
        let p = items("b*");
        assert_eq!(longest_suffix_start(&p, b"testbaaaaaba"), Some(4));
        assert_eq!(shortest_suffix_start(&p, b"testbaaaaaba"), Some(10));
        // No match at all.
        assert_eq!(shortest_prefix_end(&items("z"), b"abc"), None);
        assert_eq!(longest_suffix_start(&items("z"), b"abc"), None);
        // A boundary never falls inside a character.
        assert_eq!(
            shortest_prefix_end(&items("?"), "héllo".as_bytes()),
            Some(1)
        );
        assert_eq!(
            shortest_prefix_end(&items("h?"), "héllo".as_bytes()),
            Some(3)
        );
    }
}
