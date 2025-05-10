use super::SyntaxKind;
use std::collections::HashMap;
use std::iter::Peekable;
use std::str::Chars;
use std::sync::LazyLock;

use crate::parser::SyntaxKind::{EXPORT, INCLUDE};
static KEYWORDS: LazyLock<HashMap<&'static str, SyntaxKind>> =
    LazyLock::new(|| HashMap::from_iter([("include", INCLUDE), ("export", EXPORT)]));

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum LineType {
    Recipe,
    Other,
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    line_type: Option<LineType>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
            line_type: None,
        }
    }

    fn is_newline(c: char) -> bool {
        c == '\n' || c == '\r'
    }

    fn is_valid_identifier_char(c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_' || c == '.' || c == '-'
    }

    fn read_while<F>(&mut self, predicate: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut result = String::new();
        while let Some(&c) = self.input.peek() {
            if predicate(c) {
                result.push(c);
                self.input.next();
            } else {
                break;
            }
        }
        result
    }

    /// Retrieves the next token from the input stream, identifying its type and value
    ///
    /// # Returns
    ///
    /// An `Option<(SyntaxKind, String)>`:
    /// - `Some((SyntaxKind, String))` for the next token if available.
    /// - `None` if the input is exhausted.
    ///
    fn next_token(&mut self) -> Option<(SyntaxKind, String)> {
        while matches!(self.input.peek(), Some(' ')) {
            self.input.next();
        }
        if let Some(&c) = self.input.peek() {
            match (c, self.line_type) {
                ('\t', None) => {
                    self.input.next();
                    self.line_type = Some(LineType::Recipe);
                    return Some((SyntaxKind::INDENT, "\t".to_string()));
                }
                (_, None) => {
                    self.line_type = Some(LineType::Other);
                }
                (_, _) => {}
            }

            match c {
                c if Self::is_newline(c) => {
                    self.line_type = None;
                    return Some((SyntaxKind::NEWLINE, self.input.next()?.to_string()));
                }
                '#' => {
                    return Some((
                        SyntaxKind::COMMENT,
                        self.read_while(|c| !Self::is_newline(c)),
                    ));
                }
                _ => {}
            }

            match self.line_type.unwrap() {
                LineType::Recipe => {
                    Some((SyntaxKind::TEXT, self.read_while(|c| !Self::is_newline(c))))
                }
                LineType::Other => match c {
                    c if Self::is_valid_identifier_char(c) => {
                        let ident = self.read_while(Self::is_valid_identifier_char);

                        if let Some(token) = KEYWORDS.get(AsRef::<str>::as_ref(&ident)) {
                            Some((*token, ident))
                        } else {
                            Some((SyntaxKind::IDENTIFIER, ident))
                        }
                    }
                    '+' => {
                        self.input.next();
                        Some((SyntaxKind::PLUS, "+".to_string()))
                    }
                    '?' => {
                        self.input.next();
                        Some((SyntaxKind::QUESTION, "?".to_string()))
                    }
                    ':' => {
                        self.input.next();
                        Some((SyntaxKind::COLON, ":".to_string()))
                    }
                    '=' => {
                        self.input.next();
                        Some((SyntaxKind::EQUALS, "=".to_string()))
                    }
                    '(' => {
                        self.input.next();
                        Some((SyntaxKind::LPAREN, "(".to_string()))
                    }
                    ')' => {
                        self.input.next();
                        Some((SyntaxKind::RPAREN, ")".to_string()))
                    }
                    '{' => {
                        self.input.next();
                        Some((SyntaxKind::LBRACE, "{".to_string()))
                    }
                    '}' => {
                        self.input.next();
                        Some((SyntaxKind::RBRACE, "}".to_string()))
                    }
                    '$' => {
                        self.input.next();
                        Some((SyntaxKind::DOLLAR, "$".to_string()))
                    }
                    ',' => {
                        self.input.next();
                        Some((SyntaxKind::COMMA, ",".to_string()))
                    }
                    '\\' => {
                        self.input.next();
                        Some((SyntaxKind::BACKSLASH, "\\".to_string()))
                    }
                    '"' => {
                        self.input.next();
                        Some((SyntaxKind::DOUBLE_QUOTE, "\"".to_string()))
                    }
                    '\'' => {
                        self.input.next();
                        Some((SyntaxKind::SINGLE_QUOTE, "'".to_string()))
                    }
                    '^' => {
                        self.input.next();
                        Some((SyntaxKind::CARET, "^".to_string()))
                    }
                    '%' => {
                        self.input.next();
                        Some((SyntaxKind::PERCENT, "%".to_string()))
                    }
                    '@' => {
                        self.input.next();
                        Some((SyntaxKind::AT_SIGN, "@".to_string()))
                    }
                    '*' => {
                        self.input.next();
                        Some((SyntaxKind::STAR, "*".to_string()))
                    }
                    '<' => {
                        self.input.next();
                        Some((SyntaxKind::LESS, "<".to_string()))
                    }
                    c => {
                        self.input.next();
                        Some((SyntaxKind::ERROR, c.to_string()))
                    }
                },
            }
        } else {
            None
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = (SyntaxKind, String);

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

pub fn lex(input: &str) -> Vec<(SyntaxKind, String)> {
    let mut lexer = Lexer::new(input);
    lexer.by_ref().collect::<Vec<_>>()
}
