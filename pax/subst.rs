//
// Copyright (c) 2024 Jeff Garzik
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
    Changed(String),
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

    /// Apply this substitution to a path
    pub fn apply(&self, path: &str) -> SubstResult {
        let mut result = path.to_string();
        let mut pos = 0;
        let mut any_match = false;

        while let Some(matches) = self.regex.captures_at(&result, pos) {
            any_match = true;

            // Build the replacement string
            let replacement = build_replacement(&self.replacement, &result, &matches);

            // Get the absolute positions in result
            let match_start = matches[0].start;
            let match_end = matches[0].end;

            // Replace the matched portion
            let new_result = format!(
                "{}{}{}",
                &result[..match_start],
                replacement,
                &result[match_end..]
            );

            // Update position for next iteration
            // Move past the replacement (or at least one char to avoid infinite loop)
            let new_pos = match_start + replacement.len();
            pos = if new_pos > match_start {
                new_pos
            } else {
                match_start + 1
            };

            result = new_result;

            // If not global, stop after first replacement
            if !self.global {
                break;
            }

            // Don't go past the end
            if pos >= result.len() {
                break;
            }
        }

        if !any_match {
            return SubstResult::Unchanged;
        }

        if self.print {
            eprintln!("{} >> {}", path, result);
        }

        if result.is_empty() {
            SubstResult::Empty
        } else {
            SubstResult::Changed(result)
        }
    }
}

/// Build the replacement string from template and match groups
fn build_replacement(template: &str, input: &str, matches: &[Match]) -> String {
    let mut result = String::new();
    let mut chars = template.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '&' {
            // & is replaced by entire match
            if !matches.is_empty() && matches[0].end > matches[0].start {
                result.push_str(&input[matches[0].start..matches[0].end]);
            }
        } else if c == '\\' {
            if let Some(&next) = chars.peek() {
                if next.is_ascii_digit() && next != '0' {
                    // \1 through \9 - backreference
                    let idx = (next as usize) - ('0' as usize);
                    if idx < matches.len()
                        && idx < MAX_CAPTURES
                        && matches[idx].end > matches[idx].start
                    {
                        result.push_str(&input[matches[idx].start..matches[idx].end]);
                    }
                    chars.next();
                } else if next == '\\' {
                    // \\ -> literal backslash
                    result.push('\\');
                    chars.next();
                } else if next == '&' {
                    // \& -> literal &
                    result.push('&');
                    chars.next();
                } else {
                    // Keep other backslash sequences as-is
                    result.push(c);
                }
            } else {
                result.push(c);
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

/// Apply a list of substitutions to a path
///
/// Substitutions are applied in order. The first one that matches
/// (produces a change) wins, and no further substitutions are tried.
pub fn apply_substitutions(substitutions: &[Substitution], path: &str) -> SubstResult {
    for subst in substitutions {
        match subst.apply(path) {
            SubstResult::Unchanged => continue,
            result => return result,
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
            s.apply("hello_foo_world"),
            SubstResult::Changed("hello_bar_world".to_string())
        );
    }

    #[test]
    fn test_apply_no_match() {
        let s = Substitution::parse("/foo/bar/").unwrap();
        assert_eq!(s.apply("hello_world"), SubstResult::Unchanged);
    }

    #[test]
    fn test_apply_global() {
        let s = Substitution::parse("/foo/bar/g").unwrap();
        assert_eq!(
            s.apply("foo_foo_foo"),
            SubstResult::Changed("bar_bar_bar".to_string())
        );
    }

    #[test]
    fn test_apply_non_global() {
        let s = Substitution::parse("/foo/bar/").unwrap();
        assert_eq!(
            s.apply("foo_foo_foo"),
            SubstResult::Changed("bar_foo_foo".to_string())
        );
    }

    #[test]
    fn test_apply_empty_result() {
        let s = Substitution::parse("/.*//").unwrap();
        assert_eq!(s.apply("hello"), SubstResult::Empty);
    }

    #[test]
    fn test_apply_ampersand_replacement() {
        let s = Substitution::parse("/foo/[&]/").unwrap();
        assert_eq!(
            s.apply("hello_foo_world"),
            SubstResult::Changed("hello_[foo]_world".to_string())
        );
    }

    #[test]
    fn test_apply_backreference() {
        // In POSIX BRE, grouping is \( and \), not ( )
        // Pattern: \(.*\)_\(.*\)$ matches "hello_world" with groups
        let s = Substitution::parse("/\\(.*\\)_\\(.*\\)$/\\2_\\1/").unwrap();
        assert_eq!(
            s.apply("hello_world"),
            SubstResult::Changed("world_hello".to_string())
        );
    }

    #[test]
    fn test_apply_prefix() {
        // Add prefix using ^ anchor
        let s = Substitution::parse("/^/prefix\\//").unwrap();
        assert_eq!(
            s.apply("foo/bar"),
            SubstResult::Changed("prefix/foo/bar".to_string())
        );
    }

    #[test]
    fn test_apply_suffix_removal() {
        // Remove .txt extension using $ anchor
        // In BRE, \. matches literal dot
        let s = Substitution::parse("/\\.txt$//").unwrap();
        assert_eq!(
            s.apply("file.txt"),
            SubstResult::Changed("file".to_string())
        );
    }

    #[test]
    fn test_apply_substitutions_first_match_wins() {
        let subs = vec![
            Substitution::parse("/foo/first/").unwrap(),
            Substitution::parse("/foo/second/").unwrap(),
        ];
        assert_eq!(
            apply_substitutions(&subs, "foo"),
            SubstResult::Changed("first".to_string())
        );
    }

    #[test]
    fn test_apply_substitutions_fallthrough() {
        let subs = vec![
            Substitution::parse("/xxx/first/").unwrap(),
            Substitution::parse("/foo/second/").unwrap(),
        ];
        assert_eq!(
            apply_substitutions(&subs, "foo"),
            SubstResult::Changed("second".to_string())
        );
    }

    #[test]
    fn test_apply_substitutions_none_match() {
        let subs = vec![
            Substitution::parse("/xxx/first/").unwrap(),
            Substitution::parse("/yyy/second/").unwrap(),
        ];
        assert_eq!(apply_substitutions(&subs, "foo"), SubstResult::Unchanged);
    }

    #[test]
    fn test_escaped_ampersand() {
        let s = Substitution::parse("/foo/\\&/").unwrap();
        assert_eq!(s.apply("foo"), SubstResult::Changed("&".to_string()));
    }
}
