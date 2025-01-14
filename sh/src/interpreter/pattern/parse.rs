use crate::interpreter::{ExpandedWord, ExpandedWordPart};

#[derive(Debug, PartialEq, Eq)]
pub enum RangeEndpoint {
    Char(char),
    CollatingElement(String),
    CollatingSymbol(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum BracketItem {
    Char(char),
    CollatingSymbol(String),
    EquivalenceClass(String),
    CharacterClass(String),
    RangeExpression(RangeEndpoint, RangeEndpoint),
}

impl TryInto<RangeEndpoint> for BracketItem {
    type Error = BracketItem;
    fn try_into(self) -> Result<RangeEndpoint, BracketItem> {
        match self {
            BracketItem::Char(c) => Ok(RangeEndpoint::Char(c)),
            BracketItem::CollatingSymbol(s) => Ok(RangeEndpoint::CollatingSymbol(s)),
            _ => Err(self),
        }
    }
}

fn is_valid_range_endpoint(item: &BracketItem) -> bool {
    match item {
        BracketItem::Char(_)
        | BracketItem::CollatingSymbol(_) => true,
        _ => false,
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BracketExpression {
    pub items: Vec<BracketItem>,
    pub matching: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub enum PatternItem {
    Char(char),
    QuestionMark,
    Asterisk,
    BracketExpression(BracketExpression),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParsedPattern {
    pub items: Vec<PatternItem>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Token {
    Char(char),
    QuotedChar(char),
    Eof,
}

impl Token {
    fn from_char(c: char, is_quoted: bool) -> Self {
        if is_quoted {
            Self::QuotedChar(c)
        } else {
            Self::Char(c)
        }
    }
}

/// Creates range expressions from a list of bracket items. This function assumes that the `items`
/// is not empty and has at least one element
fn create_range_expressions(items: Vec<BracketItem>) -> Vec<BracketItem> {
    let mut result = Vec::with_capacity(items.len());
    let mut iter = items.into_iter();
    // there is at least one element by contract
    result.push(iter.next().unwrap());
    while let Some(item) = iter.next() {
        if let BracketItem::Char('-') = item {
            match (result.pop().unwrap(), iter.next()) {
                (start, Some(end))
                if is_valid_range_endpoint(&start) && is_valid_range_endpoint(&end) =>
                    {
                        result.push(BracketItem::RangeExpression(
                            start.try_into().unwrap(),
                            end.try_into().unwrap(),
                        ));
                    }
                (item, Some(next)) => {
                    result.push(item);
                    result.push(BracketItem::Char('-'));
                    result.push(next);
                }
                (item, None) => {
                    result.push(item);
                    result.push(BracketItem::Char('-'));
                }
            }
        } else {
            result.push(item);
        }
    }
    result
}

struct Parser<'w> {
    word_parts: std::slice::Iter<'w, ExpandedWordPart>,
    chars: std::str::Chars<'w>,
    inside_quoted_string: bool,
    lookahead: Token,
}

impl Parser<'_> {
    fn advance(&mut self) {
        if let Some(c) = self.chars.next() {
            self.lookahead = Token::from_char(c, self.inside_quoted_string);
        } else if let Some(part) = self.word_parts.next() {
            match part {
                ExpandedWordPart::QuotedLiteral(lit) => {
                    self.inside_quoted_string = true;
                    self.chars = lit.chars();
                    self.advance();
                }
                ExpandedWordPart::UnquotedLiteral(lit)
                | ExpandedWordPart::GeneratedUnquotedLiteral(lit) => {
                    self.inside_quoted_string = false;
                    self.chars = lit.chars();
                    self.advance();
                }
                _ => panic!("non literal word part in pattern parsing"),
            }
        } else {
            self.lookahead = Token::Eof;
        }
    }

    fn store_and_advance(&mut self, items: &mut Vec<PatternItem>) {
        items.push(match self.lookahead {
            Token::Char(c) => PatternItem::Char(c),
            Token::QuotedChar(c) => PatternItem::Char(c),
            Token::Eof => return,
        });
        self.advance();
    }

    fn try_parse_character_class(&mut self) -> Result<String, Vec<PatternItem>> {
        let mut items = Vec::new();
        // skip ':'
        self.store_and_advance(&mut items);
        // character class cannot start with a digit
        if matches!(self.lookahead, Token::Char(c) | Token::QuotedChar(c) if c.is_ascii_digit()) {
            return Err(items);
        }
        let mut class = String::new();
        loop {
            match self.lookahead {
                Token::Char(c) | Token::QuotedChar(c) if c.is_ascii_alphanumeric() => {
                    self.store_and_advance(&mut items);
                    class.push(c);
                }
                Token::Char(':') => {
                    self.store_and_advance(&mut items);
                    return if self.lookahead == Token::Char(']') {
                        self.advance();
                        Ok(class)
                    } else {
                        Err(items)
                    };
                }
                _ => {
                    return Err(items);
                }
            }
        }
    }

    fn try_parse_collating_symbol(&mut self) -> Result<String, Vec<PatternItem>> {
        let mut items = Vec::new();
        // skip '.'
        self.store_and_advance(&mut items);
        let mut symbol = String::new();
        loop {
            match self.lookahead {
                Token::Char('.') => {
                    self.store_and_advance(&mut items);
                    return if self.lookahead == Token::Char(']') {
                        self.advance();
                        Ok(symbol)
                    } else {
                        Err(items)
                    };
                }
                Token::Char(c) | Token::QuotedChar(c) => {
                    self.store_and_advance(&mut items);
                    symbol.push(c);
                }
                _ => {
                    return Err(items);
                }
            }
        }
    }

    fn try_parse_equivalence_class(&mut self) -> Result<String, Vec<PatternItem>> {
        let mut items = Vec::new();
        // skip '='
        self.store_and_advance(&mut items);
        let mut class = String::new();
        loop {
            match self.lookahead {
                Token::Char('=') => {
                    self.store_and_advance(&mut items);
                    return if self.lookahead == Token::Char(']') {
                        self.advance();
                        Ok(class)
                    } else {
                        Err(items)
                    };
                }
                Token::Char(c) | Token::QuotedChar(c) => {
                    self.store_and_advance(&mut items);
                    class.push(c);
                }
                _ => {
                    return Err(items);
                }
            }
        }
    }

    /// Tries to parse a bracket expression, if it fails it returns the literal that was parsed
    fn try_parse_bracket_expression(&mut self) -> Result<BracketExpression, Vec<PatternItem>> {
        // we store all tokens in `pattern_items`, if parsing the bracket expression fails we
        // return the literal
        let mut pattern_items = Vec::new();
        let mut expression_items = Vec::new();
        // skip '['
        self.store_and_advance(&mut pattern_items);
        let mut matching = true;
        match self.lookahead {
            Token::Char(']') => {
                self.store_and_advance(&mut pattern_items);
            }
            Token::Char('!') => {
                matching = false;
                self.store_and_advance(&mut pattern_items);
                if self.lookahead == Token::Char(']') {
                    expression_items.push(BracketItem::Char(']'));
                    self.store_and_advance(&mut pattern_items);
                }
            }
            Token::Char(':') => match self.try_parse_character_class() {
                Ok(class) => {
                    // simple character class, the standard specifies that it is implementation
                    // defined whether these patterns are supported, but bash does support them
                    // so we do too.
                    return Ok(BracketExpression {
                        items: vec![BracketItem::CharacterClass(class)],
                        matching,
                    });
                }
                Err(items) => {
                    pattern_items.extend(items);
                    expression_items.push(BracketItem::Char('['));
                }
            },
            Token::Char('.') => match self.try_parse_collating_symbol() {
                Ok(symbol) => {
                    return Ok(BracketExpression {
                        items: vec![BracketItem::CollatingSymbol(symbol)],
                        matching,
                    });
                }
                Err(items) => {
                    pattern_items.extend(items);
                    expression_items.push(BracketItem::Char('['));
                }
            }
            Token::Char('=') => match self.try_parse_equivalence_class() {
                Ok(class) => {
                    return Ok(BracketExpression {
                        items: vec![BracketItem::EquivalenceClass(class)],
                        matching,
                    });
                }
                Err(items) => {
                    pattern_items.extend(items);
                    expression_items.push(BracketItem::Char('['));
                }
            }
            _ => {}
        }

        loop {
            match self.lookahead {
                Token::Char(']') => {
                    self.advance();
                    return Ok(BracketExpression {
                        items: create_range_expressions(expression_items),
                        matching,
                    });
                }
                Token::Char('[') => {
                    self.store_and_advance(&mut pattern_items);
                    match self.lookahead {
                        Token::Char(':') => match self.try_parse_character_class() {
                            Ok(class) => {
                                // remove '[' since it is part of a character class
                                pattern_items.pop();
                                expression_items.push(BracketItem::CharacterClass(class));
                            }
                            Err(items) => {
                                pattern_items.extend(items);
                                expression_items.push(BracketItem::Char('['));
                            }
                        },
                        Token::Char('.') => match self.try_parse_collating_symbol() {
                            Ok(symbol) => {
                                // remove '[' since it is part of the collating symbol
                                pattern_items.pop();
                                expression_items.push(BracketItem::CollatingSymbol(symbol));
                            }
                            Err(items) => {
                                pattern_items.extend(items);
                                expression_items.push(BracketItem::Char('['));
                            }
                        }
                        Token::Char('=') => match self.try_parse_character_class() {
                            Ok(class) => {
                                // remove '[' since it is part of a character class
                                pattern_items.pop();
                                expression_items.push(BracketItem::EquivalenceClass(class));
                            }
                            Err(items) => {
                                pattern_items.extend(items);
                                expression_items.push(BracketItem::Char('['));
                            }
                        }
                        _ => {
                            expression_items.push(BracketItem::Char('['));
                        }
                    }
                }
                Token::QuotedChar(c) | Token::Char(c) => {
                    expression_items.push(BracketItem::Char(c));
                    self.store_and_advance(&mut pattern_items);
                }
                Token::Eof => return Err(pattern_items),
            }
        }
    }

    fn parse_pattern(&mut self) -> Result<ParsedPattern, String> {
        let mut items = Vec::new();
        loop {
            match self.lookahead {
                Token::Char(c) => match c {
                    '?' => items.push(PatternItem::QuestionMark),
                    '*' => items.push(PatternItem::Asterisk),
                    '[' => match self.try_parse_bracket_expression() {
                        Ok(bracket_expression) => {
                            items.push(PatternItem::BracketExpression(bracket_expression))
                        }
                        Err(pattern_items) => {
                            items.extend(pattern_items);
                        }
                    },
                    other => items.push(PatternItem::Char(other)),
                },
                Token::QuotedChar(c) => items.push(PatternItem::Char(c)),
                Token::Eof => break,
            }
            self.advance();
        }
        Ok(ParsedPattern { items })
    }
}

pub fn parse_pattern(
    pattern: ExpandedWord,
    used_in_filename_expansion: bool,
) -> Result<ParsedPattern, String> {
    let mut parser = Parser {
        lookahead: Token::Eof,
        inside_quoted_string: false,
        chars: "".chars(),
        word_parts: pattern.parts.iter(),
    };
    parser.advance();
    parser.parse_pattern()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_correct_pattern(pattern: ExpandedWord) -> ParsedPattern {
        parse_pattern(pattern, false).unwrap()
    }

    #[test]
    fn parse_empty_pattern() {
        assert_eq!(
            parse_correct_pattern(ExpandedWord { parts: vec![] }),
            ParsedPattern { items: vec![] }
        );
    }

    #[test]
    fn parse_single_char_pattern() {
        assert_eq!(
            parse_correct_pattern(ExpandedWord {
                parts: vec![ExpandedWordPart::UnquotedLiteral("a".to_string())]
            }),
            ParsedPattern {
                items: vec![PatternItem::Char('a')]
            }
        );
    }

    #[test]
    fn parse_multiple_char_pattern() {
        assert_eq!(
            parse_correct_pattern(ExpandedWord {
                parts: vec![ExpandedWordPart::UnquotedLiteral("abc".to_string()), ]
            }),
            ParsedPattern {
                items: vec![
                    PatternItem::Char('a'),
                    PatternItem::Char('b'),
                    PatternItem::Char('c')
                ]
            }
        );
    }

    #[test]
    fn parse_pattern_from_mixed_expanded_word() {
        assert_eq!(
            parse_correct_pattern(ExpandedWord {
                parts: vec![
                    ExpandedWordPart::UnquotedLiteral("a".to_string()),
                    ExpandedWordPart::QuotedLiteral("b".to_string()),
                    ExpandedWordPart::UnquotedLiteral("c".to_string()),
                    ExpandedWordPart::UnquotedLiteral("d".to_string()),
                    ExpandedWordPart::QuotedLiteral("e".to_string()),
                    ExpandedWordPart::QuotedLiteral("f".to_string())
                ]
            }),
            ParsedPattern {
                items: vec![
                    PatternItem::Char('a'),
                    PatternItem::Char('b'),
                    PatternItem::Char('c'),
                    PatternItem::Char('d'),
                    PatternItem::Char('e'),
                    PatternItem::Char('f')
                ]
            }
        );
    }

    #[test]
    fn parse_question_mark_pattern() {
        assert_eq!(
            parse_correct_pattern(ExpandedWord {
                parts: vec![ExpandedWordPart::UnquotedLiteral("a?c".to_string()), ]
            }),
            ParsedPattern {
                items: vec![
                    PatternItem::Char('a'),
                    PatternItem::QuestionMark,
                    PatternItem::Char('c')
                ]
            }
        );
    }

    #[test]
    fn quoted_question_mark_is_parsed_as_char() {
        assert_eq!(
            parse_correct_pattern(ExpandedWord {
                parts: vec![ExpandedWordPart::UnquotedLiteral("a".to_string()), ExpandedWordPart::QuotedLiteral("?".to_string()), ExpandedWordPart::UnquotedLiteral("c".to_string()), ]
            }),
            ParsedPattern {
                items: vec![
                    PatternItem::Char('a'),
                    PatternItem::Char('?'),
                    PatternItem::Char('c')
                ]
            }
        );
    }

    #[test]
    fn parse_asterisk_pattern() {
        assert_eq!(
            parse_correct_pattern(ExpandedWord {
                parts: vec![ExpandedWordPart::UnquotedLiteral("a*c".to_string()), ]
            }),
            ParsedPattern {
                items: vec![
                    PatternItem::Char('a'),
                    PatternItem::Asterisk,
                    PatternItem::Char('c')
                ]
            }
        );
    }

    #[test]
    fn quoted_asterisk_is_parsed_as_char() {
        assert_eq!(
            parse_correct_pattern(ExpandedWord {
                parts: vec![ExpandedWordPart::UnquotedLiteral("a".to_string()), ExpandedWordPart::QuotedLiteral("*".to_string()), ExpandedWordPart::UnquotedLiteral("c".to_string()), ]
            }),
            ParsedPattern {
                items: vec![
                    PatternItem::Char('a'),
                    PatternItem::Char('*'),
                    PatternItem::Char('c')
                ]
            }
        )
    }

    #[test]
    fn parse_bracket_with_single_character() {
        assert_eq!(
            parse_correct_pattern(ExpandedWord {
                parts: vec![ExpandedWordPart::UnquotedLiteral("[a]".to_string()), ]
            }),
            ParsedPattern {
                items: vec![PatternItem::BracketExpression(BracketExpression {
                    matching: true,
                    items: vec![BracketItem::Char('a')]
                })]
            }
        );
    }

    #[test]
    fn parse_bracket_expression_with_multiple_characters() {
        assert_eq!(
            parse_correct_pattern(ExpandedWord {
                parts: vec![ExpandedWordPart::UnquotedLiteral("[abc]".to_string()), ]
            }),
            ParsedPattern {
                items: vec![PatternItem::BracketExpression(BracketExpression {
                    matching: true,
                    items: vec![
                        BracketItem::Char('a'),
                        BracketItem::Char('b'),
                        BracketItem::Char('c')
                    ]
                })]
            }
        );
    }

    #[test]
    fn parse_non_matching_bracket_expression() {
        assert_eq!(
            parse_correct_pattern(ExpandedWord {
                parts: vec![ExpandedWordPart::UnquotedLiteral("[!abc]".to_string()), ]
            }),
            ParsedPattern {
                items: vec![PatternItem::BracketExpression(BracketExpression {
                    matching: false,
                    items: vec![
                        BracketItem::Char('a'),
                        BracketItem::Char('b'),
                        BracketItem::Char('c')
                    ]
                })]
            }
        );
    }

    #[test]
    fn parse_bracket_expression_with_character_class() {
        assert_eq!(
            parse_correct_pattern(ExpandedWord {
                parts: vec![ExpandedWordPart::UnquotedLiteral("[[:class:]]".to_string()), ]
            }),
            ParsedPattern {
                items: vec![PatternItem::BracketExpression(BracketExpression {
                    matching: true,
                    items: vec![BracketItem::CharacterClass("class".to_string())]
                })]
            }
        );
    }

    #[test]
    fn parse_bracket_expression_with_collating_symbol() {
        assert_eq!(parse_correct_pattern(ExpandedWord { parts: vec![ExpandedWordPart::UnquotedLiteral("[.ch.]".to_string())] }), ParsedPattern {
            items: vec![PatternItem::BracketExpression(BracketExpression { items: vec![BracketItem::CollatingSymbol("ch".to_string())], matching: true })]
        });
    }

    #[test]
    fn parse_bracket_expression_with_equivalence_class() {
        assert_eq!(parse_correct_pattern(ExpandedWord { parts: vec![ExpandedWordPart::UnquotedLiteral("[=a=]".to_string())] }), ParsedPattern {
            items: vec![PatternItem::BracketExpression(BracketExpression { items: vec![BracketItem::EquivalenceClass("a".to_string())], matching: true })]
        });
    }

    #[test]
    fn parse_bracket_with_simple_range() {
        assert_eq!(
            parse_correct_pattern(ExpandedWord {
                parts: vec![ExpandedWordPart::UnquotedLiteral("[a-z]".to_string()), ]
            }),
            ParsedPattern {
                items: vec![PatternItem::BracketExpression(BracketExpression {
                    matching: true,
                    items: vec![BracketItem::RangeExpression(
                        RangeEndpoint::Char('a'),
                        RangeEndpoint::Char('z')
                    )]
                })]
            }
        );
    }

    #[test]
    fn minus_is_parsed_as_char_if_it_occurs_first_in_bracket_expression() {
        assert_eq!(
            parse_correct_pattern(ExpandedWord {
                parts: vec![ExpandedWordPart::UnquotedLiteral("[-a]".to_string()), ]
            }),
            ParsedPattern {
                items: vec![PatternItem::BracketExpression(BracketExpression {
                    matching: true,
                    items: vec![BracketItem::Char('-'), BracketItem::Char('a')]
                })]
            }
        );

        assert_eq!(
            parse_correct_pattern(ExpandedWord {
                parts: vec![ExpandedWordPart::UnquotedLiteral("[!-a]".to_string()), ]
            }),
            ParsedPattern {
                items: vec![PatternItem::BracketExpression(BracketExpression {
                    matching: false,
                    items: vec![BracketItem::Char('-'), BracketItem::Char('a')]
                })]
            }
        )
    }

    #[test]
    fn minus_is_parsed_as_char_if_it_occurs_last_in_bracket_expression() {
        assert_eq!(
            parse_correct_pattern(ExpandedWord {
                parts: vec![ExpandedWordPart::UnquotedLiteral("[a-]".to_string()), ]
            }),
            ParsedPattern {
                items: vec![PatternItem::BracketExpression(BracketExpression {
                    matching: true,
                    items: vec![BracketItem::Char('a'), BracketItem::Char('-')]
                })]
            }
        )
    }

    #[test]
    fn quoted_minus_is_parsed_as_char() {
        assert_eq!(
            parse_correct_pattern(ExpandedWord {
                parts: vec![ExpandedWordPart::UnquotedLiteral("[".to_string()), ExpandedWordPart::QuotedLiteral("-".to_string()), ExpandedWordPart::UnquotedLiteral("]".to_string())]
            }),
            ParsedPattern {
                items: vec![PatternItem::BracketExpression(BracketExpression {
                    matching: true,
                    items: vec![BracketItem::Char('-')]
                })]
            }
        )
    }

    #[test]
    fn short_version_of_collating_symbol_is_parsed_correctly()
    {
        assert_eq!(parse_correct_pattern(ExpandedWord { parts: vec![ExpandedWordPart::UnquotedLiteral("[.seq.]".to_string())] }), ParsedPattern {
            items: vec![PatternItem::BracketExpression(BracketExpression { items: vec![BracketItem::CollatingSymbol("seq".to_string())], matching: true })]
        });
    }

    #[test]
    fn short_version_of_equivalence_class_is_parsed_correctly() {
        assert_eq!(parse_correct_pattern(ExpandedWord { parts: vec![ExpandedWordPart::UnquotedLiteral("[=eq=]".to_string())] }), ParsedPattern {
            items: vec![PatternItem::BracketExpression(BracketExpression { items: vec![BracketItem::EquivalenceClass("eq".to_string())], matching: true })]
        });
    }

    #[test]
    fn short_version_of_character_class_is_parsed_correctly() {
        assert_eq!(parse_correct_pattern(ExpandedWord { parts: vec![ExpandedWordPart::UnquotedLiteral("[:class:]".to_string())] }), ParsedPattern {
            items: vec![PatternItem::BracketExpression(BracketExpression { items: vec![BracketItem::CharacterClass("class".to_string())], matching: true })]
        });
    }
}
