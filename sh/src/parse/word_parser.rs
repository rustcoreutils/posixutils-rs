use crate::parse::lexer::word_lexer::{WordLexer, WordToken};
use crate::parse::word::{Parameter, ParameterExpansion, SpecialParameter, Word, WordPart};
use crate::parse::{ParseResult, ParserError};
use std::rc::Rc;

struct WordParser<'src> {
    lexer: WordLexer<'src>,
    lookahead: WordToken<'src>,
    line_no: u32,
}

impl<'src> WordParser<'src> {
    fn advance(&mut self) -> WordToken<'src> {
        let prev_lookahead = self.lookahead;
        self.lookahead = self.lexer.next_token();
        prev_lookahead
    }

    fn matches_token(&mut self, token: WordToken) -> bool {
        if self.lookahead == token {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_token(&mut self, token: WordToken) -> ParseResult<()> {
        if self.matches_token(token) {
            Ok(())
        } else {
            // word should be valid
            panic!("invalid word")
        }
    }

    fn parse_parameter(&mut self, only_consider_first_digit: bool) -> ParseResult<Parameter> {
        match self.advance() {
            WordToken::Char('@') => Ok(Parameter::Special(SpecialParameter::At)),
            WordToken::Char('*') => Ok(Parameter::Special(SpecialParameter::Asterisk)),
            WordToken::Char('#') => Ok(Parameter::Special(SpecialParameter::Hash)),
            WordToken::Char('?') => Ok(Parameter::Special(SpecialParameter::QuestionMark)),
            WordToken::Char('-') => Ok(Parameter::Special(SpecialParameter::Minus)),
            WordToken::Dollar => Ok(Parameter::Special(SpecialParameter::Dollar)),
            WordToken::Char('!') => Ok(Parameter::Special(SpecialParameter::Bang)),
            WordToken::Char('0') => Ok(Parameter::Special(SpecialParameter::Zero)),
            WordToken::Char(d) if d.is_ascii_digit() => {
                if only_consider_first_digit {
                    Ok(Parameter::Number(d.to_digit(10).unwrap()))
                } else {
                    let mut number = String::new();
                    number.push(d);
                    while let WordToken::Char(d) = self.lookahead {
                        if d.is_ascii_digit() {
                            number.push(d);
                        } else {
                            break;
                        }
                        self.advance();
                    }
                    Ok(Parameter::Number(number.parse().expect("invalid number")))
                }
            }
            WordToken::Char(c) if c == '_' || c.is_alphabetic() => {
                let mut name = String::new();
                name.push(c);
                while let WordToken::Char(c) = self.lookahead {
                    if c.is_alphanumeric() || c == '_' {
                        name.push(c);
                    } else {
                        break;
                    }
                    self.advance();
                }
                Ok(Parameter::Variable(Rc::from(name)))
            }
            other => Err(ParserError::new(
                self.line_no,
                format!("{} is not the start of a valid parameter", other,),
                false,
            )),
        }
    }

    fn parse_parameter_expansion(&mut self) -> ParseResult<ParameterExpansion> {
        // skip '$'
        self.advance();

        if self.lookahead == WordToken::Char('{') {
            self.advance();

            if self.lookahead == WordToken::Char('#') {
                self.advance();
                let expansion = self
                    .parse_parameter(false)
                    .map(ParameterExpansion::StrLen)?;
                self.match_token(WordToken::Char('}'))?;
                return Ok(expansion);
            }
            let parameter = self.parse_parameter(false)?;

            let operator_loc = self.line_no;
            match self.advance() {
                WordToken::Char('}') => Ok(ParameterExpansion::Simple(parameter)),
                WordToken::Char('%') => {
                    let remove_largest = self.matches_token(WordToken::Char('%'));
                    let word = self.parse_word_until(WordToken::Char('}'))?;
                    self.match_token(WordToken::Char('}'))?;
                    Ok(ParameterExpansion::RemovePattern {
                        parameter,
                        pattern: word,
                        remove_largest,
                        remove_prefix: false,
                    })
                }
                WordToken::Char('#') => {
                    let remove_largest = self.matches_token(WordToken::Char('#'));
                    let word = self.parse_word_until(WordToken::Char('}'))?;
                    self.match_token(WordToken::Char('}'))?;
                    Ok(ParameterExpansion::RemovePattern {
                        parameter,
                        pattern: word,
                        remove_largest,
                        remove_prefix: true,
                    })
                }
                mut operation => {
                    let alternative_version = if operation == WordToken::Char(':') {
                        operation = self.advance();
                        true
                    } else {
                        false
                    };
                    let word = self.parse_word_until(WordToken::Char('}'))?;
                    self.match_token(WordToken::Char('}'))?;
                    match operation {
                        WordToken::Char('-') => Ok(ParameterExpansion::UnsetUseDefault {
                            parameter,
                            word,
                            default_on_null: alternative_version,
                        }),
                        WordToken::Char('=') => match parameter {
                            Parameter::Variable(variable) => {
                                Ok(ParameterExpansion::UnsetAssignDefault {
                                    variable,
                                    word,
                                    assign_on_null: alternative_version,
                                })
                            }
                            _ => Err(ParserError::new(
                                operator_loc,
                                "cannot assign to positional argument or special parameter",
                                false,
                            )),
                        },
                        WordToken::Char('?') => Ok(ParameterExpansion::UnsetError {
                            parameter,
                            word,
                            error_on_null: alternative_version,
                        }),
                        WordToken::Char('+') => Ok(ParameterExpansion::SetUseAlternative {
                            parameter,
                            word,
                            substitute_null_with_word: !alternative_version,
                        }),
                        _ => Err(ParserError::new(
                            operator_loc,
                            "invalid format in parameter expansion",
                            false,
                        )),
                    }
                }
            }
        } else {
            self.parse_parameter(true).map(ParameterExpansion::Simple)
        }
    }

    fn parse_word_until(&mut self, end: WordToken) -> ParseResult<Word> {
        fn push_literal(literal: &mut String, parts: &mut Vec<WordPart>, quoted: bool) {
            let mut temp = String::new();
            std::mem::swap(&mut temp, literal);
            if quoted {
                // we want to push regardless of the fact that the string is empty,
                // since we consumed the quote character
                parts.push(WordPart::QuotedLiteral(temp));
            } else if !temp.is_empty() {
                parts.push(WordPart::UnquotedLiteral(temp));
            }
        }
        fn push_literal_and_insert(
            literal: &mut String,
            parts: &mut Vec<WordPart>,
            part: WordPart,
            quoted: bool,
        ) {
            push_literal(literal, parts, quoted);
            parts.push(part);
        }

        let mut current_literal = String::new();
        let mut word_parts = Vec::new();
        let mut inside_double_quotes = false;

        loop {
            if !inside_double_quotes && self.lookahead == end {
                break;
            }
            match self.lookahead {
                WordToken::DoubleQuote => {
                    if inside_double_quotes {
                        push_literal(&mut current_literal, &mut word_parts, true);
                    } else {
                        push_literal(&mut current_literal, &mut word_parts, false);
                    }
                    inside_double_quotes = !inside_double_quotes;
                    self.advance();
                }
                WordToken::SingleQuote => {
                    if inside_double_quotes {
                        current_literal.push('\'');
                    } else {
                        push_literal(&mut current_literal, &mut word_parts, false);
                        loop {
                            match self.lexer.next_char() {
                                Some(c) => {
                                    if c == '\'' {
                                        break;
                                    } else {
                                        current_literal.push(c)
                                    }
                                }
                                None => unreachable!("invalid word"),
                            }
                        }
                        push_literal(&mut current_literal, &mut word_parts, true);
                    }
                    self.advance();
                }
                WordToken::Dollar => {
                    push_literal_and_insert(
                        &mut current_literal,
                        &mut word_parts,
                        WordPart::ParameterExpansion {
                            expansion: self.parse_parameter_expansion()?,
                            inside_double_quotes,
                        },
                        inside_double_quotes,
                    );
                }
                WordToken::Backslash => {
                    if inside_double_quotes {
                        self.advance();
                        match self.lookahead {
                            WordToken::Dollar => {
                                current_literal.push('$');
                                self.advance();
                            }
                            WordToken::DoubleQuote => {
                                current_literal.push('"');
                                self.advance();
                            }
                            WordToken::Backslash => {
                                current_literal.push('\\');
                                self.advance();
                            }
                            _ => {
                                current_literal.push('\\');
                            }
                        }
                    } else {
                        push_literal(&mut current_literal, &mut word_parts, false);
                        current_literal.push(self.lexer.next_char().unwrap());
                        push_literal(&mut current_literal, &mut word_parts, true);
                        self.advance();
                    }
                }
                WordToken::QuotedBacktick => {
                    if inside_double_quotes {
                        current_literal.push('`');
                    } else {
                        push_literal_and_insert(
                            &mut current_literal,
                            &mut word_parts,
                            WordPart::QuotedLiteral("`".to_string()),
                            false,
                        );
                    }
                    self.advance();
                }
                WordToken::CommandSubstitution(commands) => {
                    push_literal_and_insert(
                        &mut current_literal,
                        &mut word_parts,
                        WordPart::CommandSubstitution {
                            commands: commands.to_string(),
                            inside_double_quotes,
                        },
                        inside_double_quotes,
                    );
                    self.advance();
                }
                WordToken::ArithmeticExpansion(expr) => {
                    push_literal_and_insert(
                        &mut current_literal,
                        &mut word_parts,
                        WordPart::ArithmeticExpansion {
                            expr: parse_word(expr, self.line_no)?,
                            inside_double_quotes,
                        },
                        inside_double_quotes,
                    );
                    self.advance();
                }
                WordToken::Char(c) => {
                    current_literal.push(c);
                    self.advance();
                }
                WordToken::EOF => break,
            }
        }

        assert!(!inside_double_quotes);

        push_literal(&mut current_literal, &mut word_parts, false);

        Ok(Word { parts: word_parts })
    }

    fn new(text: &'src str, line_no: u32) -> Self {
        let mut lexer = WordLexer::new(text);
        let lookahead = lexer.next_token();
        Self {
            lexer,
            lookahead,
            line_no,
        }
    }
}

/// # Panics
/// Panics if word is not valid (unclosed quotes, unclosed command substitution, etc.)
pub fn parse_word(text: &str, line_no: u32) -> ParseResult<Word> {
    let mut parser = WordParser::new(text, line_no);
    parser.parse_word_until(WordToken::EOF)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::word::test_utils::{quoted_literal, unquoted_literal};

    fn parse_word(word: &str) -> Word {
        super::parse_word(word, 0).expect("parsing error")
    }

    fn parse_unquoted_parameter_expansion(word: &str) -> ParameterExpansion {
        let word = parse_word(word);
        match word.parts.as_slice() {
            [WordPart::ParameterExpansion {
                expansion,
                inside_double_quotes,
            }] => {
                assert!(!inside_double_quotes);
                expansion.clone()
            }
            _ => panic!("expected parameter expansion, got {:?}", word),
        }
    }

    fn parse_unquoted_command_substitution(word: &str) -> String {
        let word = parse_word(word);
        match word.parts.as_slice() {
            [WordPart::CommandSubstitution {
                commands,
                inside_double_quotes,
            }] => {
                assert!(!inside_double_quotes);
                commands.clone()
            }
            _ => panic!("expected command substitution got {:?}", word),
        }
    }

    #[test]
    fn parse_simple_word() {
        assert_eq!(parse_word("hello"), unquoted_literal("hello"));
    }

    #[test]
    fn parse_word_with_single_quotes() {
        assert_eq!(
            parse_word("'single quoted string ${test} `command` $((1 + 1)) $(command2) \nnewline'"),
            quoted_literal(
                "single quoted string ${test} `command` $((1 + 1)) $(command2) \nnewline"
            )
        );
    }

    #[test]
    fn single_quotes_inside_dobule_quotes_are_ignored() {
        assert_eq!(parse_word("\"'\""), quoted_literal("'"));
    }

    #[test]
    fn parse_simple_word_with_double_quotes() {
        assert_eq!(
            parse_word("\"double quoted string \nnewline\""),
            quoted_literal("double quoted string \nnewline")
        );
    }

    #[test]
    fn parse_simple_parameter_expansion() {
        assert_eq!(
            parse_unquoted_parameter_expansion("$test"),
            ParameterExpansion::Simple(Parameter::Variable(Rc::from("test")))
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("$1"),
            ParameterExpansion::Simple(Parameter::Number(1))
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test}"),
            ParameterExpansion::Simple(Parameter::Variable(Rc::from("test")))
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${12345}"),
            ParameterExpansion::Simple(Parameter::Number(12345))
        );
    }

    #[test]
    fn parse_special_parameters() {
        assert_eq!(
            parse_unquoted_parameter_expansion("$@"),
            ParameterExpansion::Simple(Parameter::Special(SpecialParameter::At))
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("$*"),
            ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk))
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("$#"),
            ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Hash))
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("$?"),
            ParameterExpansion::Simple(Parameter::Special(SpecialParameter::QuestionMark))
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("$-"),
            ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Minus))
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("$$"),
            ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Dollar))
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("$!"),
            ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Bang))
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("$0"),
            ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Zero))
        );
    }

    #[test]
    fn parse_parameter_expansion_expression() {
        assert_eq!(
            parse_unquoted_parameter_expansion("${test:-default}"),
            ParameterExpansion::UnsetUseDefault {
                parameter: Parameter::Variable(Rc::from("test")),
                word: unquoted_literal("default"),
                default_on_null: true,
            }
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test-default}"),
            ParameterExpansion::UnsetUseDefault {
                parameter: Parameter::Variable(Rc::from("test")),
                word: unquoted_literal("default"),
                default_on_null: false,
            }
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test:=default}"),
            ParameterExpansion::UnsetAssignDefault {
                variable: Rc::from("test"),
                word: unquoted_literal("default"),
                assign_on_null: true,
            }
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test=default}"),
            ParameterExpansion::UnsetAssignDefault {
                variable: Rc::from("test"),
                word: unquoted_literal("default"),
                assign_on_null: false,
            }
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test:?default}"),
            ParameterExpansion::UnsetError {
                parameter: Parameter::Variable(Rc::from("test")),
                word: unquoted_literal("default"),
                error_on_null: true,
            }
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test?default}"),
            ParameterExpansion::UnsetError {
                parameter: Parameter::Variable(Rc::from("test")),
                word: unquoted_literal("default"),
                error_on_null: false,
            }
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test:+default}"),
            ParameterExpansion::SetUseAlternative {
                parameter: Parameter::Variable(Rc::from("test")),
                word: unquoted_literal("default"),
                substitute_null_with_word: false,
            }
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test+default}"),
            ParameterExpansion::SetUseAlternative {
                parameter: Parameter::Variable(Rc::from("test")),
                word: unquoted_literal("default"),
                substitute_null_with_word: true,
            }
        );
    }

    #[test]
    fn test_parse_parameter_expansion_expression_with_no_default() {
        assert_eq!(
            parse_unquoted_parameter_expansion("${test:-}"),
            ParameterExpansion::UnsetUseDefault {
                parameter: Parameter::Variable(Rc::from("test")),
                word: Word::default(),
                default_on_null: true
            }
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test-}"),
            ParameterExpansion::UnsetUseDefault {
                parameter: Parameter::Variable(Rc::from("test")),
                word: Word::default(),
                default_on_null: false
            }
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test:=}"),
            ParameterExpansion::UnsetAssignDefault {
                variable: Rc::from("test"),
                word: Word::default(),
                assign_on_null: true
            }
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test=}"),
            ParameterExpansion::UnsetAssignDefault {
                variable: Rc::from("test"),
                word: Word::default(),
                assign_on_null: false,
            }
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test:?}"),
            ParameterExpansion::UnsetError {
                parameter: Parameter::Variable(Rc::from("test")),
                word: Word::default(),
                error_on_null: true
            }
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test?}"),
            ParameterExpansion::UnsetError {
                parameter: Parameter::Variable(Rc::from("test")),
                word: Word::default(),
                error_on_null: false
            }
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test:+}"),
            ParameterExpansion::SetUseAlternative {
                parameter: Parameter::Variable(Rc::from("test")),
                word: Word::default(),
                substitute_null_with_word: false
            }
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test+}"),
            ParameterExpansion::SetUseAlternative {
                parameter: Parameter::Variable(Rc::from("test")),
                word: Word::default(),
                substitute_null_with_word: true
            }
        );
    }

    #[test]
    fn parse_string_operations_in_parameter_expansion() {
        assert_eq!(
            parse_unquoted_parameter_expansion("${#test}"),
            ParameterExpansion::StrLen(Parameter::Variable(Rc::from("test")))
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test%pattern}"),
            ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable(Rc::from("test")),
                pattern: unquoted_literal("pattern"),
                remove_prefix: false,
                remove_largest: false,
            }
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test%%pattern}"),
            ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable(Rc::from("test")),
                pattern: unquoted_literal("pattern"),
                remove_prefix: false,
                remove_largest: true,
            }
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test#pattern}"),
            ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable(Rc::from("test")),
                pattern: unquoted_literal("pattern"),
                remove_prefix: true,
                remove_largest: false,
            }
        );
        assert_eq!(
            parse_unquoted_parameter_expansion("${test##pattern}"),
            ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable(Rc::from("test")),
                pattern: unquoted_literal("pattern"),
                remove_prefix: true,
                remove_largest: true,
            }
        );
    }

    #[test]
    fn parse_simple_command_substitution() {
        assert_eq!(
            parse_unquoted_command_substitution("$(echo hello)"),
            "echo hello"
        );
        assert_eq!(parse_word("`echo hello`"), parse_word("$(echo hello)"));
    }

    #[test]
    fn parse_command_substitution_inside_double_quotes() {
        assert_eq!(
            parse_word("\"hello $(echo world)\""),
            Word {
                parts: vec![
                    WordPart::QuotedLiteral("hello ".to_string()),
                    WordPart::CommandSubstitution {
                        commands: "echo world".to_string(),
                        inside_double_quotes: true
                    },
                    WordPart::QuotedLiteral("".to_string())
                ]
            }
        );
        assert_eq!(
            parse_word("\"hello `echo world`\""),
            parse_word("\"hello $(echo world)\"")
        );
    }

    #[test]
    fn parse_recursive_command_substitution() {
        assert_eq!(
            parse_unquoted_command_substitution("$(echo $(echo hello))"),
            "echo $(echo hello)"
        );
    }

    #[test]
    fn test_parse_parameter_expansion_inside_double_quotes() {
        assert_eq!(
            parse_word("\"hello $test\""),
            Word {
                parts: vec![
                    WordPart::QuotedLiteral("hello ".to_string()),
                    WordPart::ParameterExpansion {
                        expansion: ParameterExpansion::Simple(Parameter::Variable(Rc::from(
                            "test"
                        ))),
                        inside_double_quotes: true
                    },
                    WordPart::QuotedLiteral("".to_string())
                ]
            }
        );
        assert_eq!(
            parse_word("\"hello ${test}\""),
            Word {
                parts: vec![
                    WordPart::QuotedLiteral("hello ".to_string()),
                    WordPart::ParameterExpansion {
                        expansion: ParameterExpansion::Simple(Parameter::Variable(Rc::from(
                            "test"
                        ))),
                        inside_double_quotes: true
                    },
                    WordPart::QuotedLiteral("".to_string())
                ]
            }
        );
    }

    #[test]
    fn test_parse_empty_double_string_word() {
        assert_eq!(parse_word("\"\""), quoted_literal(""));
    }

    #[test]
    fn test_parse_empty_single_string_word() {
        assert_eq!(parse_word("''"), quoted_literal(""));
    }

    #[test]
    fn remove_quotes_from_word() {
        assert_eq!(parse_word("\"hello\""), quoted_literal("hello"));
        assert_eq!(parse_word("'hello'"), quoted_literal("hello"));
        assert_eq!(parse_word("\"\""), quoted_literal(""));
        assert_eq!(parse_word("''"), quoted_literal(""));
        assert_eq!(parse_word("'\"hello\"'"), quoted_literal("\"hello\""));
        assert_eq!(parse_word("\"'hello'\""), quoted_literal("'hello'"));
        assert_eq!(parse_word("\\'"), quoted_literal("'"));
        assert_eq!(parse_word("\\\""), quoted_literal("\""));
        assert_eq!(parse_word("\\\\"), quoted_literal("\\"));
        assert_eq!(
            parse_word("\\$1"),
            Word {
                parts: vec![
                    WordPart::QuotedLiteral("$".into()),
                    WordPart::UnquotedLiteral("1".into())
                ]
            }
        );
        assert_eq!(parse_word("\\$"), quoted_literal("$"));
        assert_eq!(parse_word("'\\'"), quoted_literal("\\"));
    }

    #[test]
    fn parse_backslash_inside_double_quotes() {
        assert_eq!(parse_word("\"\\$\""), quoted_literal("$"));
        assert_eq!(parse_word("\"\\`\""), quoted_literal("`"));
        assert_eq!(parse_word("\"\\\"\""), quoted_literal("\""));
        assert_eq!(parse_word("\"\\\\\""), quoted_literal("\\"));
        assert_eq!(parse_word("\"\\a\""), quoted_literal("\\a"));
    }
}
