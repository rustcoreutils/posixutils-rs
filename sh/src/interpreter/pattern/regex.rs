use crate::interpreter::pattern::parse::{
    BracketExpression, BracketItem, ParsedPattern, PatternItem,
};
use regex::Regex;
use std::fmt::Write;

fn is_special_character(c: char) -> bool {
    match c {
        '.' | '^' | '$' | '|' | '?' | '*' | '+' | '(' | ')' | '[' | ']' | '{' | '}' | '\\' => true,
        _ => false,
    }
}

fn is_supported_character_class(class: &str) -> bool {
    match class {
        "alnum" | "alpha" | "ascii" | "blank" | "cntrl" | "digit" | "graph" | "lower" | "print"
        | "punct" | "space" | "upper" | "xdigit" => true,
        _ => false,
    }
}

fn push_char_literal(c: char, string: &mut String) {
    match c {
        c if is_special_character(c) => {
            write!(string, "\\x{{{:X}}}", c as u32).unwrap();
        }
        _ => string.push(c),
    }
}

fn push_bracket_expression(expr: &BracketExpression, string: &mut String) {
    string.push('[');
    if !expr.matching {
        string.push('^');
    }
    for item in &expr.items {
        match item {
            BracketItem::Char(c) => push_char_literal(*c, string),
            BracketItem::CharacterClass(class) => {
                string.push_str("[:");
                if !is_supported_character_class(class) {
                    todo!("unsupported character class: {}", class);
                }
                string.push_str(class);
                string.push_str(":]");
            }
            _ => todo!(),
        }
    }
    string.push(']');
}

pub fn parsed_pattern_to_regex(parsed_pattern: &ParsedPattern) -> Result<Regex, regex::Error> {
    let mut regex_str = String::new();
    for item in &parsed_pattern.items {
        match item {
            PatternItem::Char(c) => push_char_literal(*c, &mut regex_str),
            PatternItem::QuestionMark => regex_str.push('.'),
            PatternItem::Asterisk => regex_str.push_str(".*"),
            PatternItem::BracketExpression(expr) => push_bracket_expression(expr, &mut regex_str),
        }
    }
    Regex::new(&regex_str)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pattern_to_regex_string(parsed_pattern: ParsedPattern) -> String {
        parsed_pattern_to_regex(&parsed_pattern)
            .expect("failed to convert pattern")
            .to_string()
    }

    #[test]
    fn convert_empty_pattern() {
        let regex = pattern_to_regex_string(ParsedPattern { items: vec![] });
        assert_eq!(regex, "");
    }

    #[test]
    fn convert_single_char_pattern() {
        let regex = pattern_to_regex_string(ParsedPattern {
            items: vec![PatternItem::Char('a')],
        });
        assert_eq!(regex, "a");
    }

    #[test]
    fn convert_multiple_char_pattern() {
        let regex = pattern_to_regex_string(ParsedPattern {
            items: vec![
                PatternItem::Char('a'),
                PatternItem::Char('b'),
                PatternItem::Char('c'),
            ],
        });
        assert_eq!(regex, "abc");
    }

    #[test]
    fn convert_question_mark_pattern() {
        let regex = pattern_to_regex_string(ParsedPattern {
            items: vec![
                PatternItem::Char('a'),
                PatternItem::QuestionMark,
                PatternItem::Char('b'),
            ],
        });
        assert_eq!(regex, "a.b");
    }

    #[test]
    fn convert_asterisk_pattern() {
        let regex = pattern_to_regex_string(ParsedPattern {
            items: vec![
                PatternItem::Char('a'),
                PatternItem::Asterisk,
                PatternItem::Char('b'),
            ],
        });
        assert_eq!(regex, "a.*b");
    }

    #[test]
    fn convert_bracket_with_single_character() {
        let regex = pattern_to_regex_string(ParsedPattern {
            items: vec![PatternItem::BracketExpression(BracketExpression {
                matching: true,
                items: vec![BracketItem::Char('a')],
            })],
        });
        assert_eq!(regex, "[a]");
    }

    #[test]
    fn convert_bracket_with_multiple_characters() {
        let regex = pattern_to_regex_string(ParsedPattern {
            items: vec![PatternItem::BracketExpression(BracketExpression {
                matching: true,
                items: vec![
                    BracketItem::Char('a'),
                    BracketItem::Char('b'),
                    BracketItem::Char('c'),
                ],
            })],
        });
        assert_eq!(regex, "[abc]");
    }

    #[test]
    fn convert_bracket_with_character_class() {
        let regex = pattern_to_regex_string(ParsedPattern {
            items: vec![PatternItem::BracketExpression(BracketExpression {
                matching: true,
                items: vec![BracketItem::CharacterClass("digit".to_string())],
            })],
        });
        assert_eq!(regex, "[[:digit:]]");
    }

    #[test]
    fn convert_non_matching_bracket_expression_with_single_character() {
        let regex = pattern_to_regex_string(ParsedPattern {
            items: vec![PatternItem::BracketExpression(BracketExpression {
                matching: false,
                items: vec![BracketItem::Char('a')],
            })],
        });
        assert_eq!(regex, "[^a]");
    }

    #[test]
    fn convert_non_matching_bracket_expression_with_multiple_characters() {
        let regex = pattern_to_regex_string(ParsedPattern {
            items: vec![PatternItem::BracketExpression(BracketExpression {
                matching: false,
                items: vec![
                    BracketItem::Char('a'),
                    BracketItem::Char('b'),
                    BracketItem::Char('c'),
                ],
            })],
        });
        assert_eq!(regex, "[^abc]");
    }

    #[test]
    fn convert_non_matching_bracket_expression_with_character_class() {
        let regex = pattern_to_regex_string(ParsedPattern {
            items: vec![PatternItem::BracketExpression(BracketExpression {
                matching: false,
                items: vec![BracketItem::CharacterClass("digit".to_string())],
            })],
        });
        assert_eq!(regex, "[^[:digit:]]");
    }

    #[test]
    fn convert_bracket_expression_with_characters_and_character_class() {
        let regex = pattern_to_regex_string(ParsedPattern {
            items: vec![PatternItem::BracketExpression(BracketExpression {
                matching: true,
                items: vec![
                    BracketItem::Char('a'),
                    BracketItem::CharacterClass("digit".to_string()),
                    BracketItem::Char('b'),
                ],
            })],
        });
        assert_eq!(regex, "[a[:digit:]b]");
    }

    #[test]
    fn convert_non_matching_bracket_expression_with_characters_character_class() {
        let regex = pattern_to_regex_string(ParsedPattern {
            items: vec![PatternItem::BracketExpression(BracketExpression {
                matching: false,
                items: vec![
                    BracketItem::Char('a'),
                    BracketItem::CharacterClass("digit".to_string()),
                    BracketItem::Char('b'),
                ],
            })],
        });
        assert_eq!(regex, "[^a[:digit:]b]");
    }

    #[test]
    fn special_regex_characters_are_escaped() {
        let regex = pattern_to_regex_string(ParsedPattern {
            items: vec![
                PatternItem::Char('.'),
                PatternItem::Char('^'),
                PatternItem::Char('$'),
                PatternItem::Char('|'),
                PatternItem::Char('?'),
                PatternItem::Char('*'),
                PatternItem::Char('+'),
                PatternItem::Char('('),
                PatternItem::Char(')'),
                PatternItem::Char('['),
                PatternItem::Char(']'),
                PatternItem::Char('{'),
                PatternItem::Char('}'),
                PatternItem::Char('\\'),
            ],
        });
        assert_eq!(regex, "\\x{2E}\\x{5E}\\x{24}\\x{7C}\\x{3F}\\x{2A}\\x{2B}\\x{28}\\x{29}\\x{5B}\\x{5D}\\x{7B}\\x{7D}\\x{5C}");
    }
}
