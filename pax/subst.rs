//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Substitution expression handling for the -s option
//!
//! Implements POSIX pax -s substitution expressions of the form:
//! `-s /old/new/[gp]`
//!
//! Where:
//! - The first character is the delimiter (can be any non-null character)
//! - `old` is a POSIX Basic Regular Expression (BRE)
//! - `new` is the replacement string (supports `&` and `\1`-`\9`)
//! - `g` flag: global replacement (all occurrences)
//! - `p` flag: print successful substitutions to stderr
//!
//! This implementation uses plib::regex for POSIX BRE support.

use crate::error::{PaxError, PaxResult};
use plib::regex::{Match, Regex, RegexFlags, MAX_CAPTURES};
use std::path::{Path, PathBuf};

/// A compiled substitution expression from -s option
#[derive(Debug)]
pub struct Substitution {
    /// Compiled POSIX regex
    regex: Regex,
    /// Replacement template string (with & and \n references)
    replacement: String,
    /// Replace all occurrences (g flag)
    global: bool,
    /// Print successful substitutions to stderr (p flag)
    print: bool,
}

/// Result of applying substitutions to a path
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SubstResult {
    /// No pattern matched, path unchanged
    Unchanged,
    /// Path was transformed to the new value
    Changed(Vec<u8>),
    /// Path became empty (file should be skipped)
    Empty,
}

impl Clone for Substitution {
    fn clone(&self) -> Self {
        Substitution {
            regex: self.regex.clone(),
            replacement: self.replacement.clone(),
            global: self.global,
            print: self.print,
        }
    }
}

impl Substitution {
    /// Parse a substitution expression like "/old/new/gp"
    ///
    /// The first character is the delimiter. The expression is parsed as:
    /// `<delim><old><delim><new><delim>[flags]`
    pub fn parse(expr: &str) -> PaxResult<Self> {
        if expr.is_empty() {
            return Err(PaxError::PatternError(
                "empty substitution expression".to_string(),
            ));
        }

        let mut chars = expr.chars();
        let delimiter = chars.next().unwrap();

        if delimiter == '\0' {
            return Err(PaxError::PatternError(
                "null character not allowed as delimiter".to_string(),
            ));
        }

        let rest: String = chars.collect();

        // Parse the old pattern (up to next unescaped delimiter)
        let (old_pattern, after_old) = parse_delimited(&rest, delimiter)?;

        // Parse the new pattern (up to next unescaped delimiter)
        let (new_pattern, after_new) = parse_delimited(&after_old, delimiter)?;

        // Parse flags (remainder)
        let flags = after_new;
        let mut global = false;
        let mut print = false;

        for c in flags.chars() {
            match c {
                'g' => global = true,
                'p' => print = true,
                // POSIX `s`/`S` select whether the substitution applies to
                // the contents of a symbolic link. `s` -- do not apply -- is
                // what this implementation does, so it is accepted. `S` asks
                // for the opposite and is not implemented; accepting it would
                // silently do nothing, and a user relocating a tree with `-s`
                // would get symbolic links still pointing at the old one.
                's' => {}
                'S' => {
                    return Err(PaxError::PatternError(
                        "substitution flag 'S' (apply to symbolic link contents) \
                         is not supported"
                            .to_string(),
                    ))
                }
                _ => {
                    return Err(PaxError::PatternError(format!(
                        "unknown substitution flag: {}",
                        c
                    )))
                }
            }
        }

        // Compile the POSIX BRE regex
        let regex = Regex::new(&old_pattern, RegexFlags::bre())
            .map_err(|e| PaxError::PatternError(e.to_string()))?;

        Ok(Substitution {
            regex,
            replacement: new_pattern,
            global,
            print,
        })
    }

    /// Apply this substitution to a path.
    ///
    /// A pathname is bytes, and so is everything here: the regex reports
    /// byte offsets, which need not fall on a character boundary when the
    /// name is not valid text in the current locale (a non-ASCII name under
    /// `LC_ALL=C`), and a name that is not UTF-8 keeps its bytes.
    pub fn apply(&self, path: &[u8]) -> SubstResult {
        let mut result = path.to_vec();
        let mut pos = 0;
        let mut any_match = false;

        while let Some(matches) = self.regex.captures_at_bytes(&result, pos) {
            any_match = true;

            // Build the replacement string
            let replacement = build_replacement(&self.replacement, &result, &matches);

            // Get the absolute positions in result
            let match_start = matches[0].start;
            let match_end = matches[0].end;

            result.splice(match_start..match_end, replacement.iter().copied());

            // If not global, stop after first replacement
            if !self.global {
                break;
            }

            match next_scan_pos(&result, match_start, match_end, replacement.len()) {
                Some(next) if next < result.len() => pos = next,
                _ => break,
            }
        }

        if !any_match {
            return SubstResult::Unchanged;
        }

        if result.is_empty() {
            SubstResult::Empty
        } else {
            SubstResult::Changed(result)
        }
    }
}

/// Where a global substitution resumes scanning, as a byte offset into the
/// rewritten string. `None` once the scan has reached the end.
///
/// Whether to step over a character depends on what the *match* consumed, not
/// on how long the replacement is:
///
/// - a non-empty match was consumed, so scanning continues just past the
///   replacement. Keying this off `replacement.len()` instead meant a deletion
///   (`-s ',a,,g'`) also skipped the character after each match, leaving about
///   half the occurrences in place.
/// - an empty match consumed nothing, so one character of the subject must be
///   stepped over as well. Otherwise the same position matches forever and the
///   string grows without bound -- `-s ',x*,-,g'` never terminated.
///
/// Stepping by a whole character, rather than one byte, keeps the offset on a
/// character boundary for the next match.
fn next_scan_pos(
    result: &[u8],
    match_start: usize,
    match_end: usize,
    replacement_len: usize,
) -> Option<usize> {
    let resume = match_start + replacement_len;
    if match_end > match_start {
        return Some(resume);
    }
    let rest = result.get(resume..).filter(|rest| !rest.is_empty())?;
    Some(resume + plib::locale::mb_char_slices(&rest[..rest.len().min(16)])[0].len())
}

/// Build the replacement string from template and match groups
fn build_replacement(template: &str, input: &[u8], matches: &[Match]) -> Vec<u8> {
    // Pre-allocate with a reasonable estimate (template length + some extra for expansions)
    let mut result = Vec::with_capacity(template.len() + 32);
    // Byte by byte: every special character is ASCII, and no byte of a
    // multibyte UTF-8 character is.
    let mut bytes = template.bytes().peekable();

    while let Some(c) = bytes.next() {
        if c == b'&' {
            // & is replaced by entire match
            if !matches.is_empty() && matches[0].end > matches[0].start {
                result.extend_from_slice(&input[matches[0].start..matches[0].end]);
            }
        } else if c == b'\\' {
            match bytes.peek() {
                Some(&next @ b'1'..=b'9') => {
                    // \1 through \9 - backreference
                    let idx = (next - b'0') as usize;
                    if idx < matches.len()
                        && idx < MAX_CAPTURES
                        && matches[idx].end > matches[idx].start
                    {
                        result.extend_from_slice(&input[matches[idx].start..matches[idx].end]);
                    }
                    bytes.next();
                }
                // \\ -> literal backslash, \& -> literal &
                Some(&next @ (b'\\' | b'&')) => {
                    result.push(next);
                    bytes.next();
                }
                // Keep other backslash sequences as-is
                _ => result.push(c),
            }
        } else {
            result.push(c);
        }
    }

    result
}

/// Parse a delimited string, handling escaped delimiters
///
/// Returns (parsed_string, remaining_after_delimiter)
fn parse_delimited(s: &str, delimiter: char) -> PaxResult<(String, String)> {
    let mut result = String::new();
    let mut chars = s.chars().peekable();
    let mut found_delimiter = false;

    while let Some(c) = chars.next() {
        if c == '\\' {
            // Check if next char is the delimiter (escaped)
            if let Some(&next) = chars.peek() {
                if next == delimiter {
                    // Escaped delimiter - include literal delimiter
                    result.push(delimiter);
                    chars.next();
                    continue;
                }
            }
            // Not an escaped delimiter - keep the backslash
            result.push(c);
        } else if c == delimiter {
            found_delimiter = true;
            break;
        } else {
            result.push(c);
        }
    }

    if !found_delimiter {
        return Err(PaxError::PatternError(format!(
            "missing delimiter '{}' in substitution",
            delimiter
        )));
    }

    let remaining: String = chars.collect();
    Ok((result, remaining))
}

/// The name a member takes under the `-s` expressions, or `None` when it
/// becomes the empty string and so is to be ignored. A `p` expression reports
/// the rewrite on standard error.
pub fn substitute_name(substitutions: &[Substitution], path: &Path) -> Option<PathBuf> {
    substituted(substitutions, path, true)
}

/// The `-s` rewrite of a hard link's target, which is the name of another
/// member and so has to follow wherever `-s` moved that member. Not reported
/// under `p`: the rename is the target member's, and was reported for it.
pub fn substitute_link_target(substitutions: &[Substitution], path: &Path) -> Option<PathBuf> {
    substituted(substitutions, path, false)
}

fn substituted(substitutions: &[Substitution], path: &Path, report: bool) -> Option<PathBuf> {
    match apply_substitutions(substitutions, path, report) {
        SubstResult::Unchanged => Some(path.to_path_buf()),
        SubstResult::Changed(new) => Some(crate::rawpath::from_bytes(&new)),
        SubstResult::Empty => None,
    }
}

/// Apply the `-s` expressions to a member name, in order, stopping at the
/// first that changes it.
///
/// The regex sees the name's own bytes, so a name that is not UTF-8 is
/// matched, rewritten and reported exactly as it is.
fn apply_substitutions(substitutions: &[Substitution], path: &Path, report: bool) -> SubstResult {
    let name = crate::rawpath::as_bytes(path);
    for subst in substitutions {
        match subst.apply(name) {
            SubstResult::Unchanged => continue,
            result => {
                if report && subst.print {
                    let mut line = Vec::new();
                    line.extend_from_slice(crate::rawpath::as_bytes(path));
                    line.extend_from_slice(b" >> ");
                    match &result {
                        SubstResult::Changed(new) => line.extend_from_slice(new),
                        SubstResult::Empty | SubstResult::Unchanged => {}
                    }
                    crate::escape::write_stderr_line(&line);
                }
                return result;
            }
        }
    }
    SubstResult::Unchanged
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_basic() {
        let s = Substitution::parse("/foo/bar/").unwrap();
        assert!(!s.global);
        assert!(!s.print);
    }

    #[test]
    fn test_parse_global_flag() {
        let s = Substitution::parse("/foo/bar/g").unwrap();
        assert!(s.global);
        assert!(!s.print);
    }

    #[test]
    fn test_parse_print_flag() {
        let s = Substitution::parse("/foo/bar/p").unwrap();
        assert!(!s.global);
        assert!(s.print);
    }

    #[test]
    fn test_parse_both_flags() {
        let s = Substitution::parse("/foo/bar/gp").unwrap();
        assert!(s.global);
        assert!(s.print);

        let s = Substitution::parse("/foo/bar/pg").unwrap();
        assert!(s.global);
        assert!(s.print);
    }

    #[test]
    fn test_parse_symlink_flags() {
        // `s` asks for what this implementation does -- substitute pathnames
        // and leave symbolic link contents alone -- so it is accepted.
        assert!(Substitution::parse("/foo/bar/s").is_ok());
        let s = Substitution::parse("/foo/bar/gps").unwrap();
        assert!(s.global);
        assert!(s.print);

        // `S` asks for the opposite and is not implemented. Accepting it
        // would silently do nothing, which is worse than refusing: a user
        // relocating a tree would get links still pointing at the old one.
        let err = Substitution::parse("/foo/bar/S").unwrap_err();
        assert!(
            err.to_string().contains("'S'"),
            "the diagnostic must name the flag: {err}"
        );

        // A genuinely unknown flag is still rejected.
        assert!(Substitution::parse("/foo/bar/z").is_err());
    }

    #[test]
    fn test_parse_alternate_delimiter() {
        let s = Substitution::parse("#foo#bar#").unwrap();
        assert!(!s.global);

        let s = Substitution::parse("|foo|bar|g").unwrap();
        assert!(s.global);
    }

    #[test]
    fn test_parse_escaped_delimiter() {
        // In BRE, to match literal "/", the pattern needs "\/"
        // But our parser handles delimiter escaping in the -s expression itself
        let s = Substitution::parse("/foo\\/bar/baz/").unwrap();
        // The pattern should be "foo/bar" (with literal /)
        assert_eq!(s.regex.as_str(), "foo/bar");
    }

    #[test]
    fn test_parse_empty_error() {
        assert!(Substitution::parse("").is_err());
    }

    #[test]
    fn test_parse_missing_delimiter() {
        assert!(Substitution::parse("/foo").is_err());
        assert!(Substitution::parse("/foo/bar").is_err());
    }

    #[test]
    fn test_parse_unknown_flag() {
        assert!(Substitution::parse("/foo/bar/x").is_err());
    }

    #[test]
    fn test_apply_basic() {
        let s = Substitution::parse("/foo/bar/").unwrap();
        assert_eq!(
            s.apply("hello_foo_world".as_bytes()),
            SubstResult::Changed("hello_bar_world".as_bytes().to_vec())
        );
    }

    #[test]
    fn test_apply_no_match() {
        let s = Substitution::parse("/foo/bar/").unwrap();
        assert_eq!(s.apply("hello_world".as_bytes()), SubstResult::Unchanged);
    }

    #[test]
    fn test_apply_global() {
        let s = Substitution::parse("/foo/bar/g").unwrap();
        assert_eq!(
            s.apply("foo_foo_foo".as_bytes()),
            SubstResult::Changed("bar_bar_bar".as_bytes().to_vec())
        );
    }

    /// A `g` substitution with an empty replacement is a deletion. The loop
    /// advanced past a whole character whenever the *replacement* was empty,
    /// rather than only when the *match* was empty, so every deletion skipped
    /// the character following it and roughly half the matches survived.
    #[test]
    fn test_apply_global_deletion() {
        let s = Substitution::parse("/a//g").unwrap();
        assert_eq!(
            s.apply("aab".as_bytes()),
            SubstResult::Changed("b".as_bytes().to_vec())
        );
        assert_eq!(
            s.apply("banana".as_bytes()),
            SubstResult::Changed("bnn".as_bytes().to_vec())
        );

        // Deleting a multi-character match, adjacent occurrences.
        let s = Substitution::parse("/ab//g").unwrap();
        assert_eq!(
            s.apply("xababy".as_bytes()),
            SubstResult::Changed("xy".as_bytes().to_vec())
        );
    }

    /// Shortening (but non-empty) replacements hit the same advance logic.
    #[test]
    fn test_apply_global_shortening() {
        let s = Substitution::parse("/aa/a/g").unwrap();
        assert_eq!(
            s.apply("aaaa".as_bytes()),
            SubstResult::Changed("aa".as_bytes().to_vec())
        );
    }

    /// The one-character bump that guards against an empty match must land on a
    /// character boundary; a non-ASCII subject would otherwise slice mid-char.
    #[test]
    fn test_apply_global_empty_match_non_ascii() {
        // `x*` matches empty at every position.
        let s = Substitution::parse("/x*/-/g").unwrap();
        match s.apply("éöü".as_bytes()) {
            SubstResult::Changed(_) => {}
            other => panic!("expected a substitution, got {:?}", other),
        }

        let s = Substitution::parse("/é//g").unwrap();
        assert_eq!(
            s.apply("éaéb".as_bytes()),
            SubstResult::Changed("ab".as_bytes().to_vec())
        );
    }

    #[test]
    fn test_apply_non_global() {
        let s = Substitution::parse("/foo/bar/").unwrap();
        assert_eq!(
            s.apply("foo_foo_foo".as_bytes()),
            SubstResult::Changed("bar_foo_foo".as_bytes().to_vec())
        );
    }

    #[test]
    fn test_apply_empty_result() {
        let s = Substitution::parse("/.*//").unwrap();
        assert_eq!(s.apply("hello".as_bytes()), SubstResult::Empty);
    }

    #[test]
    fn test_apply_ampersand_replacement() {
        let s = Substitution::parse("/foo/[&]/").unwrap();
        assert_eq!(
            s.apply("hello_foo_world".as_bytes()),
            SubstResult::Changed("hello_[foo]_world".as_bytes().to_vec())
        );
    }

    #[test]
    fn test_apply_backreference() {
        // In POSIX BRE, grouping is \( and \), not ( )
        // Pattern: \(.*\)_\(.*\)$ matches "hello_world" with groups
        let s = Substitution::parse("/\\(.*\\)_\\(.*\\)$/\\2_\\1/").unwrap();
        assert_eq!(
            s.apply("hello_world".as_bytes()),
            SubstResult::Changed("world_hello".as_bytes().to_vec())
        );
    }

    #[test]
    fn test_apply_prefix() {
        // Add prefix using ^ anchor
        let s = Substitution::parse("/^/prefix\\//").unwrap();
        assert_eq!(
            s.apply("foo/bar".as_bytes()),
            SubstResult::Changed("prefix/foo/bar".as_bytes().to_vec())
        );
    }

    #[test]
    fn test_apply_suffix_removal() {
        // Remove .txt extension using $ anchor
        // In BRE, \. matches literal dot
        let s = Substitution::parse("/\\.txt$//").unwrap();
        assert_eq!(
            s.apply("file.txt".as_bytes()),
            SubstResult::Changed("file".as_bytes().to_vec())
        );
    }

    #[test]
    fn test_apply_substitutions_first_match_wins() {
        let subs = vec![
            Substitution::parse("/foo/first/").unwrap(),
            Substitution::parse("/foo/second/").unwrap(),
        ];
        assert_eq!(
            apply_substitutions(&subs, Path::new("foo"), false),
            SubstResult::Changed("first".as_bytes().to_vec())
        );
    }

    #[test]
    fn test_apply_substitutions_fallthrough() {
        let subs = vec![
            Substitution::parse("/xxx/first/").unwrap(),
            Substitution::parse("/foo/second/").unwrap(),
        ];
        assert_eq!(
            apply_substitutions(&subs, Path::new("foo"), false),
            SubstResult::Changed("second".as_bytes().to_vec())
        );
    }

    #[test]
    fn test_apply_substitutions_none_match() {
        let subs = vec![
            Substitution::parse("/xxx/first/").unwrap(),
            Substitution::parse("/yyy/second/").unwrap(),
        ];
        assert_eq!(
            apply_substitutions(&subs, Path::new("foo"), false),
            SubstResult::Unchanged
        );
    }

    #[test]
    fn test_escaped_ampersand() {
        let s = Substitution::parse("/foo/\\&/").unwrap();
        assert_eq!(
            s.apply("foo".as_bytes()),
            SubstResult::Changed("&".as_bytes().to_vec())
        );
    }
}
