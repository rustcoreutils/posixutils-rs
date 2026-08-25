//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Pattern validation and anchoring analysis.
//!
//! This module handles validation of pattern restrictions per POSIX specification
//! and parsing of anchoring (^, $) and trailing context (/) operators.

use gettextrs::gettext;

/// Parse anchoring and trailing context from a pattern.
///
/// Returns (bol_anchor, main_pattern, trailing_context, eol_anchor)
pub fn parse_anchoring_and_trailing_context(pattern: &str) -> (bool, String, Option<String>, bool) {
    let mut main_pattern = pattern.to_string();
    let mut bol_anchor = false;
    let mut eol_anchor = false;
    let mut trailing_context = None;

    // Check for ^ at the beginning (BOL anchor)
    if main_pattern.starts_with('^') {
        bol_anchor = true;
        main_pattern = main_pattern[1..].to_string();
    }

    // Check for $ at the end (EOL anchor) - must be unescaped and outside brackets/quotes
    // $ is equivalent to /\n (trailing context with newline)
    if pattern_ends_with_unescaped_dollar(&main_pattern) {
        eol_anchor = true;
        main_pattern.pop(); // Remove the $
    }

    // Look for unescaped / (trailing context operator)
    // The / must not be inside brackets, quotes, or escaped
    if let Some(slash_pos) = find_trailing_context_slash(&main_pattern) {
        let tc = main_pattern[slash_pos + 1..].to_string();
        main_pattern = main_pattern[..slash_pos].to_string();
        trailing_context = Some(tc);
    }

    // If EOL anchor ($) was found, convert to trailing context /\n
    if eol_anchor {
        if trailing_context.is_some() {
            // Can't have both $ and trailing context - $ is already a form of trailing context
            // POSIX says this is undefined, but we'll treat $ as /\n
        }
        trailing_context = Some("\\n".to_string());
    }

    (bol_anchor, main_pattern, trailing_context, eol_anchor)
}

/// One unescaped character of a pattern, with the context it sits in.
struct PatternChar {
    /// Byte offset into the pattern, so callers may slice the pattern with it.
    byte_idx: usize,
    ch: char,
    in_brackets: bool,
    in_quotes: bool,
}

/// Walk a pattern's unescaped characters, tracking bracket-expression and
/// quoted-string context.
///
/// The pattern-level operators (`/` and `$`) are only operators outside
/// `[...]` and `"..."`, and every scanner looking for one needs exactly this
/// state. Keeping it in one place stops the copies from drifting apart.
///
/// Escaped characters are skipped entirely, along with the backslash that
/// escapes them: for these callers "escaped" and "absent" mean the same thing.
/// The reported context is the state *entering* the character, so an opening
/// `[` reports `in_brackets == false` and its closing `]` reports `true`.
fn unescaped_chars(pattern: &str) -> impl Iterator<Item = PatternChar> + '_ {
    let mut in_brackets = false;
    let mut in_quotes = false;
    let mut escape_next = false;

    pattern.char_indices().filter_map(move |(byte_idx, ch)| {
        if escape_next {
            escape_next = false;
            return None;
        }
        if ch == '\\' {
            escape_next = true;
            return None;
        }

        let here = PatternChar {
            byte_idx,
            ch,
            in_brackets,
            in_quotes,
        };

        match ch {
            '"' if !in_brackets => in_quotes = !in_quotes,
            '[' if !in_quotes => in_brackets = true,
            ']' if !in_quotes && in_brackets => in_brackets = false,
            _ => {}
        }

        Some(here)
    })
}

/// Check if pattern ends with an unescaped $ (not inside brackets or quotes)
pub fn pattern_ends_with_unescaped_dollar(pattern: &str) -> bool {
    if !pattern.ends_with('$') {
        return false;
    }

    // '$' is one byte, so the final character starts here.
    let dollar_idx = pattern.len() - 1;

    // An escaped '$' is never yielded by the scanner, which is precisely the
    // "not an anchor" answer.
    unescaped_chars(pattern).any(|pc| pc.byte_idx == dollar_idx && !pc.in_brackets && !pc.in_quotes)
}

/// Find the byte offset of the trailing context operator / (not inside
/// brackets, quotes, or escaped)
pub fn find_trailing_context_slash(pattern: &str) -> Option<usize> {
    unescaped_chars(pattern)
        .find(|pc| pc.ch == '/' && !pc.in_brackets && !pc.in_quotes)
        .map(|pc| pc.byte_idx)
}

/// Count the number of unescaped trailing context operators / in a pattern
pub fn count_trailing_context_slashes(pattern: &str) -> usize {
    unescaped_chars(pattern)
        .filter(|pc| pc.ch == '/' && !pc.in_brackets && !pc.in_quotes)
        .count()
}

/// Validate pattern restrictions per POSIX specification.
///
/// Returns Ok(()) if valid, Err(message) if invalid.
///
/// Validates:
/// - ^ only valid at beginning of pattern
/// - $ only valid at end of pattern (and not with trailing context)
/// - Only one trailing context operator / allowed
pub fn validate_pattern_restrictions(pattern: &str) -> Result<(), String> {
    let mut dollar_positions: Vec<usize> = Vec::new();
    let mut caret_positions: Vec<usize> = Vec::new();

    // First pass: find trailing context operator
    let has_trailing_context = find_trailing_context_slash(pattern).is_some();

    // Second pass: find all unescaped ^ and $ positions. '^' inside [] is a
    // negation character and '$' inside [] is an ordinary member, so both are
    // only operators outside brackets and quotes.
    for pc in unescaped_chars(pattern) {
        match pc.ch {
            '^' if !pc.in_brackets && !pc.in_quotes => caret_positions.push(pc.byte_idx),
            '$' if !pc.in_brackets && !pc.in_quotes => dollar_positions.push(pc.byte_idx),
            _ => {}
        }
    }

    // Validate ^ positions - only valid at beginning
    for pos in &caret_positions {
        if *pos != 0 {
            return Err(format!(
                "{} {}",
                gettext("'^' operator only valid at beginning of pattern, found at position"),
                pos
            ));
        }
    }

    // Validate $ positions - only valid at end (and not with trailing context)
    for pos in &dollar_positions {
        // $ must be at the very end of the pattern ('$' is one byte, so the
        // final character starts at len - 1).
        if *pos != pattern.len() - 1 {
            return Err(format!(
                "{} {}",
                gettext("'$' operator only valid at end of pattern, found at position"),
                pos
            ));
        }
        // $ cannot be used with trailing context
        if has_trailing_context {
            return Err(gettext(
                "'$' cannot be used with trailing context '/'; $ is equivalent to /\\n",
            ));
        }
    }

    // Validate only one trailing context operator
    let tc_count = count_trailing_context_slashes(pattern);
    if tc_count > 1 {
        return Err(format!(
            "{} {}",
            gettext("Only one trailing context operator '/' allowed per pattern, found"),
            tc_count
        ));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_bol_anchor() {
        let (bol, main, tc, eol) = parse_anchoring_and_trailing_context("^foo");
        assert!(bol);
        assert_eq!(main, "foo");
        assert!(tc.is_none());
        assert!(!eol);
    }

    #[test]
    fn test_parse_eol_anchor() {
        let (bol, main, tc, eol) = parse_anchoring_and_trailing_context("foo$");
        assert!(!bol);
        assert_eq!(main, "foo");
        assert_eq!(tc, Some("\\n".to_string()));
        assert!(eol);
    }

    #[test]
    fn test_parse_trailing_context() {
        let (bol, main, tc, eol) = parse_anchoring_and_trailing_context("foo/bar");
        assert!(!bol);
        assert_eq!(main, "foo");
        assert_eq!(tc, Some("bar".to_string()));
        assert!(!eol);
    }

    #[test]
    fn test_validate_caret_at_start() {
        assert!(validate_pattern_restrictions("^foo").is_ok());
    }

    #[test]
    fn test_validate_caret_not_at_start() {
        assert!(validate_pattern_restrictions("foo^bar").is_err());
    }

    #[test]
    fn test_validate_dollar_at_end() {
        assert!(validate_pattern_restrictions("foo$").is_ok());
    }

    #[test]
    fn test_validate_dollar_not_at_end() {
        assert!(validate_pattern_restrictions("foo$bar").is_err());
    }

    #[test]
    fn test_validate_multiple_slashes() {
        assert!(validate_pattern_restrictions("foo/bar/baz").is_err());
    }

    #[test]
    fn test_find_trailing_context() {
        assert_eq!(find_trailing_context_slash("foo/bar"), Some(3));
        assert_eq!(find_trailing_context_slash("foo\\/bar"), None); // Escaped
        assert_eq!(find_trailing_context_slash("[/]foo"), None); // In brackets
    }
}
