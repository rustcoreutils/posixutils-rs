//! Pattern escape sequence and POSIX bracket expression handling.
//!
//! This module handles translation of escape sequences and expansion of
//! POSIX bracket expression constructs like equivalence classes [=c=]
//! and collating elements [.c.].

/// Translate POSIX escape sequences to regex_syntax compatible format.
///
/// Handles:
/// 1. Octal escapes (\NNN): POSIX uses \NNN, regex_syntax needs \xNN
/// 2. Backspace (\b): regex_syntax interprets \b as word boundary,
///    so we convert to \x08
/// 3. Single-digit hex escapes (\xN): regex_syntax requires two digits,
///    so we pad to \x0N
pub fn translate_escape_sequences(input: &str) -> String {
    let mut result = String::new();
    let chars: Vec<char> = input.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        if chars[i] == '\\' && i + 1 < chars.len() {
            let next = chars[i + 1];

            // Handle \b -> \x08 (backspace, not word boundary)
            // In POSIX lex, \b means backspace (0x08), but regex_syntax
            // interprets \b as word boundary assertion
            if next == 'b' {
                result.push_str("\\x08");
                i += 2;
                continue;
            }

            // Handle \xN -> \x0N (pad single hex digit)
            // POSIX says \x followed by "the longest sequence of hexadecimal-digit
            // characters", which can be just one digit. regex_syntax requires two.
            if next == 'x' && i + 2 < chars.len() {
                let hex_start = i + 2;
                let mut hex_digits = String::new();
                let mut j = hex_start;

                // Collect hex digits
                while j < chars.len() && chars[j].is_ascii_hexdigit() {
                    hex_digits.push(chars[j]);
                    j += 1;
                }

                if hex_digits.len() == 1 {
                    // Single hex digit - pad with leading zero
                    result.push_str(&format!("\\x0{}", hex_digits));
                    i = j;
                    continue;
                } else if hex_digits.len() >= 2 {
                    // Two or more hex digits - take first two, leave rest as literal
                    result.push_str(&format!("\\x{}", &hex_digits[..2]));
                    // Any digits beyond the first two become literal characters
                    result.push_str(&hex_digits[2..]);
                    i = j;
                    continue;
                }
                // No hex digits after \x - leave as-is (will error in regex_syntax)
            }

            // Handle octal escapes (\NNN) -> \xNN
            // Check if this is an octal escape (digit 0-7)
            if next.is_ascii_digit() && next < '8' {
                // Consume up to 3 octal digits
                let mut octal_str = String::new();
                let mut j = i + 1;
                while j < chars.len() && octal_str.len() < 3 {
                    let c = chars[j];
                    if c.is_ascii_digit() && c < '8' {
                        octal_str.push(c);
                        j += 1;
                    } else {
                        break;
                    }
                }

                // Convert octal to byte value
                if let Ok(byte_val) = u8::from_str_radix(&octal_str, 8) {
                    // Output as hex escape \xNN
                    result.push_str(&format!("\\x{:02x}", byte_val));
                    i = j;
                    continue;
                }
            }
        }
        result.push(chars[i]);
        i += 1;
    }

    result
}

/// Expand POSIX bracket expression constructs to standard regex form.
///
/// In the POSIX locale (which we assume):
/// - Equivalence class [=c=] matches the character c
/// - Collating element [.c.] matches the character(s) literally (with escaping if needed)
///
/// Examples:
/// - `[abc[=d=]ef]` becomes `[abcdef]`
/// - `[[=a=]]` becomes `[a]`
/// - `[abc[.^.]def]` becomes `[abc\^def]` (^ needs escaping at start of bracket)
pub fn expand_posix_bracket_constructs(pattern: &str) -> String {
    let chars: Vec<char> = pattern.chars().collect();
    let mut result = String::new();
    let mut i = 0;
    let mut in_bracket = false;
    let mut bracket_content_start = false; // True if we just entered a bracket

    while i < chars.len() {
        let ch = chars[i];

        if ch == '\\' && i + 1 < chars.len() {
            // Escape sequence - copy both characters
            result.push(ch);
            result.push(chars[i + 1]);
            i += 2;
            bracket_content_start = false;
            continue;
        }

        if ch == '[' && !in_bracket {
            // Entering a bracket expression
            in_bracket = true;
            bracket_content_start = true;
            result.push(ch);
            i += 1;
            continue;
        }

        if in_bracket && ch == '[' && i + 1 < chars.len() {
            let next = chars[i + 1];
            if next == '=' || next == '.' {
                // Found [= or [. inside brackets - this is a POSIX construct
                let close_seq = if next == '=' { "=]" } else { ".]" };
                if let Some((content, end_idx)) =
                    find_posix_construct_content(&chars, i + 2, close_seq)
                {
                    // In POSIX locale: just output the content
                    // For collating elements with special chars, we need to escape them
                    if next == '.' {
                        // Collating element - escape special bracket chars
                        for c in content.chars() {
                            match c {
                                '^' if bracket_content_start => result.push_str("\\^"),
                                ']' => result.push_str("\\]"),
                                '\\' => result.push_str("\\\\"),
                                '-' => result.push_str("\\-"),
                                _ => result.push(c),
                            }
                            bracket_content_start = false;
                        }
                    } else {
                        // Equivalence class - just output the character(s)
                        result.push_str(&content);
                        if !content.is_empty() {
                            bracket_content_start = false;
                        }
                    }
                    i = end_idx + 1;
                    continue;
                }
                // No valid closing found - pass through as-is (regex_syntax will error)
            }
            // Not a POSIX construct, just a literal [ inside brackets
            bracket_content_start = false;
        }

        if ch == ']' && in_bracket {
            in_bracket = false;
        }

        result.push(ch);
        if in_bracket && ch != '[' {
            bracket_content_start = false;
        }
        i += 1;
    }

    result
}

/// Find the content of a POSIX bracket construct and return (content, end_index)
/// where end_index is the index of the closing bracket ']'
fn find_posix_construct_content(
    chars: &[char],
    start: usize,
    close_seq: &str,
) -> Option<(String, usize)> {
    let close_chars: Vec<char> = close_seq.chars().collect();
    let mut content = String::new();
    let mut i = start;

    while i + close_chars.len() <= chars.len() {
        // Check if we found the closing sequence
        let mut found = true;
        for (j, close_char) in close_chars.iter().enumerate() {
            if chars[i + j] != *close_char {
                found = false;
                break;
            }
        }

        if found {
            // Return content and index of the closing ]
            return Some((content, i + close_chars.len() - 1));
        }

        content.push(chars[i]);
        i += 1;
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_translate_octal_basic() {
        assert_eq!(translate_escape_sequences(r"\0"), r"\x00");
        assert_eq!(translate_escape_sequences(r"\7"), r"\x07");
        assert_eq!(translate_escape_sequences(r"\77"), r"\x3f");
        assert_eq!(translate_escape_sequences(r"\377"), r"\xff");
    }

    #[test]
    fn test_translate_octal_two_digit() {
        assert_eq!(translate_escape_sequences(r"\01"), r"\x01");
        assert_eq!(translate_escape_sequences(r"\12"), r"\x0a");
    }

    #[test]
    fn test_translate_backspace() {
        assert_eq!(translate_escape_sequences(r"\b"), r"\x08");
    }

    #[test]
    fn test_translate_hex_padding() {
        assert_eq!(translate_escape_sequences(r"\x9"), r"\x09");
        assert_eq!(translate_escape_sequences(r"\xa"), r"\x0a");
    }

    #[test]
    fn test_expand_equivalence_class() {
        assert_eq!(expand_posix_bracket_constructs("[[=a=]]"), "[a]");
        assert_eq!(expand_posix_bracket_constructs("[abc[=d=]ef]"), "[abcdef]");
    }

    #[test]
    fn test_expand_collating_element() {
        assert_eq!(expand_posix_bracket_constructs("[[.a.]]"), "[a]");
        // ^ at start of bracket content needs escaping
        assert_eq!(expand_posix_bracket_constructs("[[.^.]]"), "[\\^]");
    }
}
