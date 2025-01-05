use crate::interpreter::{ExpandedWord, ExpandedWordPart};

pub enum RangeEndpoint {
    Char(char),
    CollatingElement(String),
    CollatingSymbol(String),
    EquivalenceClass(String),
}

pub enum BracketItem {
    Char(char),
    CollatingElements(String),
    CollatingSymbol(String),
    EquivalenceClass(String),
    CharacterClass(String),
    RangeExpression(RangeEndpoint, RangeEndpoint),
}

pub struct BracketExpression {
    pub items: Vec<BracketItem>,
    pub matching: bool,
}

pub enum PatternItem {
    Char(char),
    QuestionMark,
    Asterisk,
    BracketExpression(BracketExpression),
}

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
                matching = true;
                self.store_and_advance(&mut pattern_items);
                if self.lookahead == Token::Char(']') {
                    expression_items.push(BracketItem::Char(']'));
                    self.store_and_advance(&mut pattern_items);
                }
            }
            Token::Char(':') => match self.try_parse_character_class() {
                Ok(class) => {
                    // remove '[' since it is part of a character class
                    pattern_items.pop();
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
            Token::Char('.') => {
                todo!()
            }
            Token::Char('=') => {
                todo!()
            }
            _ => {}
        }

        loop {
            match self.lookahead {
                Token::Char(']') => {
                    self.advance();
                    return Ok(BracketExpression {
                        items: expression_items,
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
                        Token::Char('.') => {
                            todo!()
                        }
                        Token::Char('=') => {
                            todo!()
                        }
                        Token::Char('-') => {
                            todo!()
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
        }
        Ok(ParsedPattern { items })
    }
}

pub fn parse_pattern(
    pattern: ExpandedWord,
    used_in_filename_expansion: bool,
) -> Result<ParsedPattern, String> {
    let mut items = Vec::new();
    let mut parser = Parser {
        lookahead: Token::Eof,
        inside_quoted_string: false,
        chars: "".chars(),
        word_parts: pattern.parts.iter(),
    };
    parser.advance();

    Ok(ParsedPattern { items })
}
