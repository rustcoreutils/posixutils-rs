//! Pattern validation and anchoring analysis.
//!
//! This module handles validation of pattern restrictions per POSIX specification
//! and parsing of anchoring (^, $) and trailing context (/) operators.

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

/// Check if pattern ends with an unescaped $ (not inside brackets or quotes)
pub fn pattern_ends_with_unescaped_dollar(pattern: &str) -> bool {
    if !pattern.ends_with('$') {
        return false;
    }

    let chars: Vec<char> = pattern.chars().collect();
    let len = chars.len();

    // Count consecutive backslashes before the final $
    let mut backslash_count = 0;
    for i in (0..len.saturating_sub(1)).rev() {
        if chars[i] == '\\' {
            backslash_count += 1;
        } else {
            break;
        }
    }

    // $ is escaped if preceded by odd number of backslashes
    if backslash_count % 2 == 1 {
        return false;
    }

    // Check if $ is inside brackets (simplified check)
    let mut in_brackets = false;
    let mut in_quotes = false;
    for (idx, ch) in chars.iter().enumerate() {
        if idx == len - 1 {
            break; // Don't count the final $
        }
        match ch {
            '"' if !in_brackets => in_quotes = !in_quotes,
            '[' if !in_quotes => in_brackets = true,
            ']' if !in_quotes => in_brackets = false,
            _ => {}
        }
    }

    !in_brackets && !in_quotes
}

/// Find the position of trailing context operator / (not inside brackets, quotes, or escaped)
pub fn find_trailing_context_slash(pattern: &str) -> Option<usize> {
    let chars: Vec<char> = pattern.chars().collect();
    let mut in_brackets = false;
    let mut in_quotes = false;
    let mut escape_next = false;

    for (idx, ch) in chars.iter().enumerate() {
        if escape_next {
            escape_next = false;
            continue;
        }
        match ch {
            '\\' => escape_next = true,
            '"' if !in_brackets => in_quotes = !in_quotes,
            '[' if !in_quotes => in_brackets = true,
            ']' if !in_quotes && in_brackets => in_brackets = false,
            '/' if !in_brackets && !in_quotes => return Some(idx),
            _ => {}
        }
    }

    None
}

/// Count the number of unescaped trailing context operators / in a pattern
pub fn count_trailing_context_slashes(pattern: &str) -> usize {
    let chars: Vec<char> = pattern.chars().collect();
    let mut in_brackets = false;
    let mut in_quotes = false;
    let mut escape_next = false;
    let mut count = 0;

    for ch in chars.iter() {
        if escape_next {
            escape_next = false;
            continue;
        }
        match ch {
            '\\' => escape_next = true,
            '"' if !in_brackets => in_quotes = !in_quotes,
            '[' if !in_quotes => in_brackets = true,
            ']' if !in_quotes && in_brackets => in_brackets = false,
            '/' if !in_brackets && !in_quotes => count += 1,
            _ => {}
        }
    }

    count
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
    let chars: Vec<char> = pattern.chars().collect();
    let mut in_brackets = false;
    let mut in_quotes = false;
    let mut escape_next = false;
    let mut dollar_positions: Vec<usize> = Vec::new();
    let mut caret_positions: Vec<usize> = Vec::new();

    // First pass: find trailing context operator
    let has_trailing_context = find_trailing_context_slash(pattern).is_some();

    // Second pass: find all unescaped ^ and $ positions
    for (idx, ch) in chars.iter().enumerate() {
        if escape_next {
            escape_next = false;
            continue;
        }
        match ch {
            '\\' => escape_next = true,
            '"' if !in_brackets => in_quotes = !in_quotes,
            '[' if !in_quotes => in_brackets = true,
            ']' if !in_quotes && in_brackets => in_brackets = false,
            '^' if !in_brackets && !in_quotes => {
                // ^ inside [] is a negation character, which is valid anywhere
                caret_positions.push(idx);
            }
            '$' if !in_brackets && !in_quotes => {
                dollar_positions.push(idx);
            }
            _ => {}
        }
    }

    // Validate ^ positions - only valid at beginning
    for pos in &caret_positions {
        if *pos != 0 {
            return Err(format!(
                "'^' operator only valid at beginning of pattern, found at position {}",
                pos
            ));
        }
    }

    // Validate $ positions - only valid at end (and not with trailing context)
    for pos in &dollar_positions {
        // $ must be at the very end of the pattern
        if *pos != chars.len() - 1 {
            return Err(format!(
                "'$' operator only valid at end of pattern, found at position {}",
                pos
            ));
        }
        // $ cannot be used with trailing context
        if has_trailing_context {
            return Err(
                "'$' cannot be used with trailing context '/'; $ is equivalent to /\\n".to_string(),
            );
        }
    }

    // Validate only one trailing context operator
    let tc_count = count_trailing_context_slashes(pattern);
    if tc_count > 1 {
        return Err(format!(
            "Only one trailing context operator '/' allowed per pattern, found {}",
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
