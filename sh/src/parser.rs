//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{path::is_separator, rc::Rc};

use crate::program::SpecialParameter;
use crate::{
    lexer::{is_blank, is_operator, Lexer, ShellToken, SourceLocation, TokenId, WordToken},
    program::{
        ArithmeticExpr, Assignment, CaseItem, Command, CompleteCommand, CompleteCommandList,
        CompoundCommand, Conjunction, FunctionDefinition, IORedirectionKind, If, LogicalOp, Name,
        Parameter, ParameterExpansion, Pipeline, Program, Redirection, RedirectionKind,
        SimpleCommand, Word, WordPart,
    },
};

fn is_valid_name(name: &str) -> bool {
    name.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_')
        && name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn try_word_to_assignment(word: Word) -> Result<Assignment, Word> {
    if let Some(WordPart::UnquotedLiteral(name)) = word.parts.first() {
        if let Some(eq_pos) = name.find('=') {
            let (name, rest) = name.split_at(eq_pos);
            if !is_valid_name(name) {
                return Err(word);
            }
            let name = Rc::<str>::from(name);
            let word_start = rest[1..].to_string();
            let mut word_parts = Vec::with_capacity(word.parts.len());
            if !word_start.is_empty() {
                word_parts.push(WordPart::UnquotedLiteral(word_start));
            }
            word_parts.extend(word.parts.into_iter().skip(1));
            Ok(Assignment {
                name,
                value: Word { parts: word_parts },
            })
        } else {
            Err(word)
        }
    } else {
        Err(word)
    }
}

fn try_into_name(word: Word) -> Result<Name, Word> {
    if word.parts.len() != 1 {
        return Err(word);
    }

    if let WordPart::UnquotedLiteral(name) = word.parts.first().unwrap() {
        if is_valid_name(name) {
            Ok(Rc::from(name.as_str()))
        } else {
            Err(word)
        }
    } else {
        Err(word)
    }
}

struct Parser<'src> {
    lexer: Lexer<'src>,
    shell_lookahead: ShellToken,
    shell_lookahead_token_id: TokenId,
    word_lookahead: WordToken,
    word_lookahead_token_id: TokenId,
    lookahead_source_location: SourceLocation,
}

impl<'src> Parser<'src> {
    fn advance_shell(&mut self) {
        let (lookahead, source_location, id) = self.lexer.next_shell_token();
        self.shell_lookahead = lookahead;
        self.lookahead_source_location = source_location;
        self.shell_lookahead_token_id = id;
    }

    fn advance_word(&mut self) {
        let (lookahead, source_location, id) = self.lexer.next_word_token();
        self.word_lookahead = lookahead;
        self.lookahead_source_location = source_location;
        self.word_lookahead_token_id = id;
    }

    fn shell_lookahead(&mut self) -> ShellToken {
        if self.shell_lookahead_token_id < self.word_lookahead_token_id {
            self.lexer.rollback_last_token();
            self.advance_shell();
            self.shell_lookahead
        } else {
            self.shell_lookahead
        }
    }

    fn word_lookahead(&mut self) -> WordToken {
        if self.word_lookahead_token_id < self.shell_lookahead_token_id {
            self.lexer.rollback_last_token();
            self.advance_word();
            self.word_lookahead
        } else {
            self.word_lookahead
        }
    }

    // TODO: look into renaming this, its kind of confusing as it seems like this should be
    // in `parse_word_until`
    fn go_to_word_start(&mut self, ignore_reserved_words: bool) -> bool {
        if self.shell_lookahead() == ShellToken::WordStart {
            self.advance_word();
            true
        } else if ignore_reserved_words && self.shell_lookahead().is_reserved_word() {
            self.lexer.rollback_last_token();
            // the reserved word could be after blanks, which we need to skip
            self.lexer.skip_blanks();
            self.advance_word();
            true
        } else {
            // there is no word here
            false
        }
    }

    fn matches_shell_alterntives(&mut self, tokens: &[ShellToken]) -> Option<ShellToken> {
        for token in tokens {
            if self.shell_lookahead() == *token {
                self.advance_shell();
                return Some(*token);
            }
        }
        None
    }

    fn match_shell_token(&mut self, token: ShellToken) {
        if self.shell_lookahead() != token {
            todo!();
        }
        self.advance_shell();
    }

    fn match_shell_token_opt(&mut self, token: ShellToken) {
        if self.shell_lookahead() == token {
            self.advance_shell();
        }
    }

    fn match_word_token(&mut self, token: WordToken) {
        if self.word_lookahead() != token {
            todo!()
        }
        self.advance_word();
    }

    fn skip_linebreak(&mut self) {
        // "\n"*
        while self.shell_lookahead() == ShellToken::Newline {
            self.advance_shell();
        }
    }

    fn parse_arithmetic_expansion(&mut self) -> ArithmeticExpr {
        todo!()
    }

    fn parse_parameter(&mut self, only_consider_first_digit: bool) -> Parameter {
        fn advance_and_return(parser: &mut Parser, parameter: Parameter) -> Parameter {
            parser.advance_word();
            parameter
        }

        match self.word_lookahead() {
            WordToken::Char('@') => {
                advance_and_return(self, Parameter::Special(SpecialParameter::At))
            }
            WordToken::Char('*') => {
                advance_and_return(self, Parameter::Special(SpecialParameter::Asterisk))
            }
            WordToken::Char('#') => {
                advance_and_return(self, Parameter::Special(SpecialParameter::Hash))
            }
            WordToken::Char('?') => {
                advance_and_return(self, Parameter::Special(SpecialParameter::QuestionMark))
            }
            WordToken::Char('-') => {
                advance_and_return(self, Parameter::Special(SpecialParameter::Minus))
            }
            WordToken::Dollar => {
                advance_and_return(self, Parameter::Special(SpecialParameter::Dollar))
            }
            WordToken::Char('!') => {
                advance_and_return(self, Parameter::Special(SpecialParameter::Bang))
            }
            WordToken::Char('0') => {
                advance_and_return(self, Parameter::Special(SpecialParameter::Zero))
            }
            WordToken::Char(d) if d.is_ascii_digit() => {
                if only_consider_first_digit {
                    Parameter::Number(d.to_digit(10).unwrap())
                } else {
                    // FIXME: refactor this, its almost identical to the loop below.
                    let mut number = String::new();
                    number.push(d);
                    self.advance_word();
                    while let WordToken::Char(d) = self.word_lookahead() {
                        if d.is_ascii_digit() {
                            number.push(d);
                        } else {
                            break;
                        }
                        self.advance_word();
                    }
                    Parameter::Number(number.parse().expect("invalid number"))
                }
            }
            WordToken::Char(c) if c == '_' || c.is_alphabetic() => {
                let mut name = String::new();
                name.push(c);
                self.advance_word();
                while let WordToken::Char(c) = self.word_lookahead() {
                    if c.is_alphanumeric() || c == '_' {
                        name.push(c);
                    } else {
                        break;
                    }
                    self.advance_word();
                }
                Parameter::Variable(Rc::from(name))
            }
            _ => todo!("error: expected parameter"),
        }
    }

    fn parse_parameter_expansion(&mut self) -> ParameterExpansion {
        // skip '$'
        self.advance_word();

        if self.word_lookahead() == WordToken::Char('{') {
            self.advance_word();
            if self.word_lookahead() == WordToken::Char('#') {
                self.advance_word();
                return ParameterExpansion::StrLen(self.parse_parameter(false));
            }
            let parameter = self.parse_parameter(false);
            if self.word_lookahead() == WordToken::Char('}') {
                self.advance_word();
                return ParameterExpansion::Simple(parameter);
            }

            if self.word_lookahead() == WordToken::Char('%') {
                self.advance_word();
                if self.word_lookahead() == WordToken::Char('%') {
                    self.advance_word();
                    let word = self.parse_word_until(WordToken::Char('}'));
                    return ParameterExpansion::RemovePattern {
                        parameter,
                        pattern: word,
                        remove_largest: true,
                        remove_prefix: false,
                    };
                } else {
                    let word = self.parse_word_until(WordToken::Char('}'));
                    return ParameterExpansion::RemovePattern {
                        parameter,
                        pattern: word,
                        remove_largest: false,
                        remove_prefix: false,
                    };
                }
            }

            if self.word_lookahead() == WordToken::Char('#') {
                self.advance_word();
                if self.word_lookahead() == WordToken::Char('#') {
                    self.advance_word();
                    let word = self.parse_word_until(WordToken::Char('}'));
                    return ParameterExpansion::RemovePattern {
                        parameter,
                        pattern: word,
                        remove_largest: true,
                        remove_prefix: true,
                    };
                } else {
                    let word = self.parse_word_until(WordToken::Char('}'));
                    return ParameterExpansion::RemovePattern {
                        parameter,
                        pattern: word,
                        remove_largest: false,
                        remove_prefix: true,
                    };
                }
            }
            // TODO: restructure this for better errors
            if self.word_lookahead() == WordToken::Char(':') {
                self.advance_word();
                let operation = self.word_lookahead();
                self.advance_word();
                let word = self.parse_word_until(WordToken::Char('}'));
                match operation {
                    WordToken::Char('-') => ParameterExpansion::UnsetUseDefault {
                        parameter,
                        word,
                        default_on_null: true,
                    },
                    WordToken::Char('=') => ParameterExpansion::UnsetAssignDefault {
                        parameter,
                        word,
                        assign_on_null: true,
                    },
                    WordToken::Char('?') => ParameterExpansion::UnsetError {
                        parameter,
                        word,
                        error_on_null: true,
                    },
                    WordToken::Char('+') => ParameterExpansion::SetUseAlternative {
                        parameter,
                        word,
                        substitute_null_with_word: false,
                    },
                    _ => todo!("error"),
                }
            } else {
                let operation = self.word_lookahead();
                self.advance_word();
                let word = self.parse_word_until(WordToken::Char('}'));
                if word.is_none() && operation == WordToken::Char('}') {
                    return ParameterExpansion::Simple(parameter);
                }
                match operation {
                    WordToken::Char('-') => ParameterExpansion::UnsetUseDefault {
                        parameter,
                        word,
                        default_on_null: false,
                    },
                    WordToken::Char('=') => ParameterExpansion::UnsetAssignDefault {
                        parameter,
                        word,
                        assign_on_null: false,
                    },
                    WordToken::Char('?') => ParameterExpansion::UnsetError {
                        parameter,
                        word,
                        error_on_null: false,
                    },
                    WordToken::Char('+') => ParameterExpansion::SetUseAlternative {
                        parameter,
                        word,
                        substitute_null_with_word: true,
                    },
                    _ => todo!("error"),
                }
            }
        } else {
            ParameterExpansion::Simple(self.parse_parameter(true))
        }
    }

    fn parse_word_until(&mut self, end: WordToken) -> Option<Word> {
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
            if !inside_double_quotes && self.word_lookahead() == end {
                break;
            }
            match self.word_lookahead() {
                WordToken::DoubleQuote => {
                    if inside_double_quotes {
                        push_literal(&mut current_literal, &mut word_parts, true);
                    } else {
                        push_literal(&mut current_literal, &mut word_parts, false);
                    }
                    inside_double_quotes = !inside_double_quotes;
                    self.advance_word();
                }
                WordToken::SingleQuote => {
                    if !inside_double_quotes {
                        push_literal(&mut current_literal, &mut word_parts, false);
                        loop {
                            if let Some((c, _)) = self.lexer.next_char() {
                                if c == '\'' {
                                    break;
                                }
                                current_literal.push(c);
                            } else {
                                todo!("error: expected ', got end of file")
                            }
                        }
                        push_literal(&mut current_literal, &mut word_parts, true);
                    } else {
                        current_literal.push('\'');
                    }
                    self.advance_word();
                }
                WordToken::Dollar => {
                    push_literal_and_insert(
                        &mut current_literal,
                        &mut word_parts,
                        WordPart::ParameterExpansion(self.parse_parameter_expansion()),
                        inside_double_quotes,
                    );
                }
                WordToken::Backtick => {
                    self.advance_word();
                    push_literal_and_insert(
                        &mut current_literal,
                        &mut word_parts,
                        WordPart::CommandSubstitution(
                            self.parse_complete_command(WordToken::Backtick),
                        ),
                        inside_double_quotes,
                    );
                    self.match_word_token(WordToken::Backtick);
                }
                WordToken::CommandSubstitutionStart => {
                    self.advance_word();
                    push_literal_and_insert(
                        &mut current_literal,
                        &mut word_parts,
                        WordPart::CommandSubstitution(
                            self.parse_complete_command(WordToken::Char(')')),
                        ),
                        inside_double_quotes,
                    );
                    self.match_word_token(WordToken::Char(')'));
                }
                WordToken::EscapedBacktick => {
                    todo!("implement nested command substitution");
                }
                WordToken::ArithmeticExpansionStart => {
                    // the closing )) should be consumed by `parse_arithmetic_expansion`
                    push_literal_and_insert(
                        &mut current_literal,
                        &mut word_parts,
                        WordPart::ArithmeticExpansion(self.parse_arithmetic_expansion()),
                        inside_double_quotes,
                    );
                    // TODO: should improve this for better errors. Now it would produce
                    // expected ')' instead of expected '))'
                    self.match_word_token(WordToken::Char(')'));
                    self.match_word_token(WordToken::Char(')'));
                }
                WordToken::Char(c) => {
                    if !inside_double_quotes && (is_operator(c) || is_blank(c)) {
                        break;
                    }
                    current_literal.push(c);
                    self.advance_word();
                }
                WordToken::EOF => break,
            }
        }

        if inside_double_quotes {
            todo!("error: unclosed double quotes");
        }

        push_literal(&mut current_literal, &mut word_parts, false);

        if word_parts.is_empty() {
            None
        } else {
            Some(Word { parts: word_parts })
        }
    }

    fn parse_redirection_kind(&mut self) -> Option<RedirectionKind> {
        if self.shell_lookahead() == ShellToken::DLess
            || self.shell_lookahead() == ShellToken::DLessDash
        {
            let remove_leading_tabs = self.shell_lookahead() == ShellToken::DLessDash;
            let mut contents = String::new();
            let end = self.lexer.next_line().0;
            loop {
                let line = self.lexer.next_line().0;
                if line == end {
                    break;
                }
                if remove_leading_tabs {
                    contents.push_str(line.trim_start_matches('\t'));
                } else {
                    contents.push_str(line);
                }
            }
            self.advance_shell();
            return Some(RedirectionKind::HereDocument { contents });
        }
        let kind = match self.shell_lookahead() {
            ShellToken::Greater => IORedirectionKind::RedirectOutput,
            ShellToken::Clobber => IORedirectionKind::RedirectOutputClobber,
            ShellToken::DGreat => IORedirectionKind::RedirectOuputAppend,
            ShellToken::GreatAnd => IORedirectionKind::DuplicateOutput,
            ShellToken::Less => IORedirectionKind::RedirectInput,
            ShellToken::LessAnd => IORedirectionKind::DuplicateInput,
            ShellToken::LessGreat => IORedirectionKind::OpenRW,
            _ => return None,
        };
        // advance the operator
        self.advance_shell();
        if self.go_to_word_start(true) {
            let file = self.parse_word_until(WordToken::EOF).unwrap();
            Some(RedirectionKind::IORedirection { kind, file })
        } else {
            todo!("error: expected word, got ..")
        }
    }

    fn parse_redirection_opt(&mut self) -> Option<Redirection> {
        if let ShellToken::IoNumber(n) = self.shell_lookahead() {
            if !(0..9).contains(&n) {
                // TODO: bash supports (0..1023), should look into this
                todo!("error: bad file descriptor");
            }
            // skip number
            self.advance_shell();
            if let Some(kind) = self.parse_redirection_kind() {
                Some(Redirection {
                    kind,
                    file_descriptor: Some(n),
                })
            } else {
                todo!("error: expected redirection, found ..")
            }
        } else {
            self.parse_redirection_kind().map(|kind| Redirection {
                file_descriptor: None,
                kind,
            })
        }
    }

    fn parse_simple_command(
        &mut self,
        start: Option<Word>,
        word_stop: WordToken,
    ) -> Option<SimpleCommand> {
        // simple_command = (io_redirect | assignment_word)* word? (io_redirect | word)*

        let mut command = SimpleCommand::default();

        fn wrap_command(command: SimpleCommand) -> Option<SimpleCommand> {
            if command == SimpleCommand::default() {
                None
            } else {
                Some(command)
            }
        }

        /// returns true if there are no more words to parse
        fn add_word_or_assignment(
            parser: &mut Parser,
            word_stop: WordToken,
            word: Option<Word>,
            command: &mut SimpleCommand,
        ) -> bool {
            if let Some(word) = word {
                match try_word_to_assignment(word) {
                    Ok(assignment) => command.assignments.push(assignment),
                    Err(cmd) => {
                        command.words.push(cmd);
                    }
                }
            }
            if word_stop != WordToken::EOF && parser.word_lookahead() == word_stop {
                true
            } else {
                false
            }
        }

        if add_word_or_assignment(self, word_stop, start, &mut command) {
            return wrap_command(command);
        }

        while command.words.is_empty() {
            if self.go_to_word_start(true) {
                let word = self.parse_word_until(word_stop);
                if add_word_or_assignment(self, word_stop, word, &mut command) {
                    return wrap_command(command);
                }
            } else {
                if let Some(redirection) = self.parse_redirection_opt() {
                    command.redirections.push(redirection);
                } else {
                    return wrap_command(command);
                }
            }
        }

        loop {
            if self.go_to_word_start(true) {
                if let Some(word) = self.parse_word_until(word_stop) {
                    command.words.push(word);
                }
                if word_stop != WordToken::EOF && self.word_lookahead() == word_stop {
                    return wrap_command(command);
                }
            } else {
                if let Some(redirection) = self.parse_redirection_opt() {
                    command.redirections.push(redirection);
                } else {
                    return wrap_command(command);
                }
            }
        }
    }

    fn parse_compound_list(&mut self, word_stop: WordToken) -> CompleteCommand {
        self.skip_linebreak();

        const END_TOKENS: &[ShellToken] = &[
            ShellToken::RParen,
            ShellToken::RBrace,
            ShellToken::Do,
            ShellToken::Done,
            ShellToken::Elif,
            ShellToken::Else,
            ShellToken::Esac,
            ShellToken::Fi,
            ShellToken::Then,
        ];

        let mut last_conjunction = self.parse_and_or(word_stop);
        let mut commands = Vec::new();
        while let Some(mut conjunction) = last_conjunction {
            match self.shell_lookahead() {
                ShellToken::And => {
                    conjunction.is_async = true;
                }
                ShellToken::SemiColon | ShellToken::Newline => {}
                _ => {
                    commands.push(conjunction);
                    break;
                }
            }
            self.advance_shell();
            self.skip_linebreak();
            if END_TOKENS.contains(&self.shell_lookahead()) {
                commands.push(conjunction);
                break;
            }
            commands.push(conjunction);
            last_conjunction = self.parse_and_or(word_stop);
        }
        if commands.is_empty() {
            todo!("error: expected command, found ...")
        }
        CompleteCommand { commands }
    }

    fn parse_brace_group(&mut self) -> CompoundCommand {
        // consume '{'
        self.advance_shell();
        let inner = self.parse_compound_list(WordToken::Char('}'));
        self.match_shell_token(ShellToken::RBrace);
        CompoundCommand::BraceGroup(inner)
    }

    fn parse_subshell(&mut self) -> CompoundCommand {
        // consume '('
        self.advance_shell();
        let inner = self.parse_compound_list(WordToken::Char(')'));
        self.match_shell_token(ShellToken::RParen);
        CompoundCommand::Subshell(inner)
    }

    fn parse_do_group(&mut self) -> CompleteCommand {
        self.match_shell_token(ShellToken::Do);
        let inner = self.parse_compound_list(WordToken::EOF);
        self.match_shell_token(ShellToken::Done);
        inner
    }

    fn parse_for_clause(&mut self) -> CompoundCommand {
        // consume 'for'
        self.advance_shell();
        self.go_to_word_start(true);
        let iter_var = if let Some(name) = self
            .parse_word_until(WordToken::EOF)
            .map(|w| try_into_name(w).ok())
            .flatten()
        {
            name
        } else {
            todo!("error: expected name")
        };
        self.skip_linebreak();
        let mut words = Vec::new();
        if self.shell_lookahead() == ShellToken::In {
            self.advance_shell();
            while self.go_to_word_start(true) {
                words.push(self.parse_word_until(WordToken::EOF).unwrap());
            }
        }
        match self.shell_lookahead() {
            ShellToken::SemiColon | ShellToken::Newline => {
                self.advance_shell();
                self.skip_linebreak();
            }
            _ => {}
        }
        let body = self.parse_do_group();
        CompoundCommand::ForClause {
            iter_var,
            words,
            body,
        }
    }

    fn parse_case_item(&mut self) -> CaseItem {
        self.match_shell_token_opt(ShellToken::LParen);
        let mut pattern = Vec::new();
        while self.shell_lookahead() != ShellToken::RParen {
            self.go_to_word_start(true);
            pattern.push(self.parse_word_until(WordToken::EOF).unwrap());
            if self.shell_lookahead() != ShellToken::Pipe {
                break;
            }
            self.advance_shell();
        }
        self.match_shell_token(ShellToken::RParen);

        let body = self.parse_compound_list(WordToken::EOF);

        if self.shell_lookahead() == ShellToken::DSemi {
            self.advance_shell();
            self.skip_linebreak();
        } else if self.shell_lookahead() != ShellToken::Esac {
            todo!("error: expected ';;', found ...")
        }

        CaseItem { body, pattern }
    }

    fn parse_case_clause(&mut self) -> CompoundCommand {
        // consume 'case'
        self.advance_shell();
        self.go_to_word_start(true);
        let arg = self.parse_word_until(WordToken::EOF).unwrap();
        self.skip_linebreak();
        self.match_shell_token(ShellToken::In);
        self.skip_linebreak();
        let mut cases = Vec::new();
        loop {
            match self.shell_lookahead() {
                ShellToken::Esac => break,
                _ => {
                    cases.push(self.parse_case_item());
                }
            }
        }
        self.match_shell_token(ShellToken::Esac);
        CompoundCommand::CaseClause { arg, cases }
    }

    fn parse_if_clause(&mut self) -> CompoundCommand {
        // consume 'if'
        self.advance_shell();
        let mut if_chain = Vec::new();
        let condition = self.parse_compound_list(WordToken::EOF);
        self.match_shell_token(ShellToken::Then);
        let then_part = self.parse_compound_list(WordToken::EOF);
        if_chain.push(If {
            condition,
            body: then_part,
        });
        while self.shell_lookahead() == ShellToken::Elif {
            // consume 'elif'
            self.advance_shell();
            let condition = self.parse_compound_list(WordToken::EOF);
            self.match_shell_token(ShellToken::Then);
            let then_part = self.parse_compound_list(WordToken::EOF);
            if_chain.push(If {
                condition,
                body: then_part,
            });
        }
        if self.shell_lookahead() == ShellToken::Else {
            self.advance_shell();
            let else_part = self.parse_compound_list(WordToken::EOF);
            if_chain.push(If {
                condition: CompleteCommand {
                    commands: Vec::new(),
                },
                body: else_part,
            });
        }
        self.match_shell_token(ShellToken::Fi);
        CompoundCommand::IfClause { if_chain }
    }

    fn parse_while_clause(&mut self) -> CompoundCommand {
        // consume 'while'
        self.advance_shell();
        let condition = self.parse_compound_list(WordToken::EOF);
        let body = self.parse_do_group();
        CompoundCommand::WhileClause { condition, body }
    }

    fn parse_until_clause(&mut self) -> CompoundCommand {
        // consume 'until'
        self.advance_shell();
        let condition = self.parse_compound_list(WordToken::EOF);
        let body = self.parse_do_group();
        CompoundCommand::UntilClause { condition, body }
    }

    fn parse_compound_command(&mut self) -> Option<CompoundCommand> {
        match self.shell_lookahead() {
            ShellToken::LBrace => Some(self.parse_brace_group()),
            ShellToken::LParen => Some(self.parse_subshell()),
            ShellToken::For => Some(self.parse_for_clause()),
            ShellToken::Case => Some(self.parse_case_clause()),
            ShellToken::If => Some(self.parse_if_clause()),
            ShellToken::While => Some(self.parse_while_clause()),
            ShellToken::Until => Some(self.parse_until_clause()),
            _ => None,
        }
    }

    fn parse_function_definition(&mut self, name: Name) -> FunctionDefinition {
        // consume '('
        self.advance_shell();
        self.match_shell_token(ShellToken::RParen);
        let body = self.parse_compound_command();
        if let Some(body) = body {
            FunctionDefinition { name, body }
        } else {
            todo!("error: expected compound command")
        }
    }

    fn parse_command(&mut self, word_stop: WordToken) -> Option<Command> {
        // command =
        // 			| compound_command redirect_list?
        // 			| simple_command
        // 			| function_definition
        if let Some(compound_command) = self.parse_compound_command() {
            let mut redirections = Vec::new();
            while let Some(redirection) = self.parse_redirection_opt() {
                redirections.push(redirection);
            }
            Some(Command::CompoundCommand {
                command: compound_command,
                redirections,
            })
        } else {
            self.go_to_word_start(false);
            if let Some(start) = self.parse_word_until(word_stop) {
                match try_into_name(start) {
                    Ok(name) if self.shell_lookahead() == ShellToken::LParen => Some(
                        Command::FunctionDefinition(self.parse_function_definition(name)),
                    ),
                    Ok(name) => {
                        let start = Word {
                            parts: vec![WordPart::UnquotedLiteral(name.to_string())],
                        };
                        self.parse_simple_command(Some(start), word_stop)
                            .map(Command::SimpleCommand)
                    }
                    Err(word) => self
                        .parse_simple_command(Some(word), word_stop)
                        .map(Command::SimpleCommand),
                }
            } else {
                self.parse_simple_command(None, word_stop)
                    .map(Command::SimpleCommand)
            }
        }
    }

    fn parse_pipeline(&mut self, word_stop: WordToken) -> Option<Pipeline> {
        // pipeline = "!" command ("|" linebreak command)*
        // TODO: implement the "!"* part

        let negate_status = self
            .matches_shell_alterntives(&[ShellToken::Bang])
            .is_some();
        let mut commands = Vec::new();
        commands.push(self.parse_command(word_stop)?);
        while self.shell_lookahead() == ShellToken::Pipe {
            self.advance_shell();
            self.skip_linebreak();
            if let Some(command) = self.parse_command(word_stop) {
                commands.push(command);
            } else {
                todo!("error: expected command, found ...")
            }
        }
        Some(Pipeline {
            commands,
            negate_status,
        })
    }

    fn parse_and_or(&mut self, word_stop: WordToken) -> Option<Conjunction> {
        // and_or = pipeline (("&&" | "||") linebreak pipeline)*
        let mut last = self.parse_pipeline(word_stop)?;
        let mut elements = Vec::new();
        while let Some(op) = self.matches_shell_alterntives(&[ShellToken::AndIf, ShellToken::OrIf])
        {
            let op = match op {
                ShellToken::AndIf => LogicalOp::And,
                ShellToken::OrIf => LogicalOp::Or,
                _ => unreachable!(),
            };
            self.skip_linebreak();
            let next = if let Some(next) = self.parse_pipeline(word_stop) {
                next
            } else {
                todo!("error: expected command, found ...")
            };
            let previous = last;
            last = next;
            elements.push((previous, op));
        }
        elements.push((last, LogicalOp::None));
        Some(Conjunction {
            elements,
            is_async: false,
        })
    }

    fn parse_complete_command(&mut self, word_stop: WordToken) -> CompleteCommand {
        // complete_command = and_or (separator_op and_or)* separator_op?
        let mut commands = Vec::new();
        while self.shell_lookahead() != ShellToken::Newline
            || self.shell_lookahead() != ShellToken::EOF
        {
            let mut and_or = if let Some(and_or) = self.parse_and_or(word_stop) {
                and_or
            } else {
                todo!("error: expected command, found ...")
            };
            // FIXME: very ugly, should move is_async somewhere else
            if self.shell_lookahead() == ShellToken::And {
                and_or.is_async = true;
            }
            commands.push(and_or);
            if self.shell_lookahead() == ShellToken::And
                || self.shell_lookahead() == ShellToken::SemiColon
            {
                self.advance_shell();
            } else {
                break;
            }
        }
        CompleteCommand { commands }
    }

    fn parse_program(mut self) -> Program {
        // program = linebreak (complete_command (complete_command  newline_list)*)? linebreak
        // set initial lookahead
        self.advance_shell();
        self.skip_linebreak();
        let mut commands = Vec::new();
        while self.shell_lookahead() != ShellToken::EOF {
            if commands.len() > 0 {
                self.match_shell_token(ShellToken::Newline);
                self.skip_linebreak();
            }
            if self.shell_lookahead() == ShellToken::EOF {
                break;
            }
            commands.push(self.parse_complete_command(WordToken::EOF));
        }

        Program { commands }
    }

    fn new(source: &'src str) -> Self {
        Self {
            lexer: Lexer::new(source),
            shell_lookahead: ShellToken::EOF,
            shell_lookahead_token_id: TokenId::default(),
            word_lookahead: WordToken::EOF,
            word_lookahead_token_id: TokenId::default(),
            lookahead_source_location: SourceLocation::default(),
        }
    }
}

pub fn parse(text: &str) -> Program {
    Parser::new(text).parse_program()
}

#[cfg(test)]
mod tests {
    use crate::program::test_utils::{quoted_literal, unquoted_literal};

    use super::*;

    fn pipeline_from_word(word: Word) -> Pipeline {
        Pipeline {
            commands: vec![Command::SimpleCommand(SimpleCommand {
                words: vec![word],
                ..Default::default()
            })],
            negate_status: false,
        }
    }

    fn conjunction_from_word(word: Word, is_async: bool) -> Conjunction {
        Conjunction {
            elements: vec![(pipeline_from_word(word), LogicalOp::None)],
            is_async,
        }
    }

    fn complete_command_from_word(word: Word, is_async: bool) -> CompleteCommand {
        CompleteCommand {
            commands: vec![conjunction_from_word(word, is_async)],
        }
    }

    fn unwrap_complete_command(program: Program) -> CompleteCommand {
        assert_eq!(program.commands.len(), 1);
        program.commands.into_iter().next().unwrap()
    }

    fn unwrap_conjunction(program: Program) -> Conjunction {
        let complete_command = unwrap_complete_command(program);
        assert_eq!(complete_command.commands.len(), 1);
        complete_command.commands.into_iter().next().unwrap()
    }

    fn unwrap_pipeline(program: Program) -> Pipeline {
        let conjunction = unwrap_conjunction(program);
        assert_eq!(conjunction.elements.len(), 1);
        let (pipeline, _) = conjunction.elements.into_iter().next().unwrap();
        pipeline
    }

    fn unwrap_command(program: Program) -> Command {
        let pipeline = unwrap_pipeline(program);
        assert_eq!(pipeline.commands.len(), 1);
        pipeline.commands.into_iter().next().unwrap()
    }

    fn unwrap_simple_command(program: Program) -> SimpleCommand {
        let command = unwrap_command(program);
        if let Command::SimpleCommand(command) = command {
            command
        } else {
            panic!("expected simple command")
        }
    }

    fn parse_word(word: &str) -> Word {
        let command = unwrap_simple_command(parse(word));
        command.words.into_iter().next().unwrap()
    }

    fn parse_parameter_expansion(word: &str) -> ParameterExpansion {
        let word = parse_word(word);
        if let WordPart::ParameterExpansion(expansion) = word.parts.into_iter().next().unwrap() {
            expansion
        } else {
            panic!("expected parameter expansion")
        }
    }

    fn parse_simple_command(text: &str) -> SimpleCommand {
        unwrap_simple_command(parse(text))
    }

    fn parse_single_redirection(text: &str) -> Redirection {
        let cmd = parse_simple_command(text);
        assert!(cmd.words.is_empty());
        assert!(cmd.assignments.is_empty());
        assert_eq!(cmd.redirections.len(), 1);
        cmd.redirections.into_iter().next().unwrap()
    }

    fn parse_compound_command(text: &str) -> (CompoundCommand, Vec<Redirection>) {
        let command = unwrap_command(parse(text));
        if let Command::CompoundCommand {
            command,
            redirections,
        } = command
        {
            (command, redirections)
        } else {
            panic!("expected compound command, got {:?}", command)
        }
    }

    fn parse_command(text: &str) -> Command {
        unwrap_command(parse(text))
    }

    fn parse_pipeline(text: &str) -> Pipeline {
        unwrap_pipeline(parse(text))
    }

    fn parse_conjunction(text: &str) -> Conjunction {
        unwrap_conjunction(parse(text))
    }

    fn parse_complete_command(text: &str) -> CompleteCommand {
        unwrap_complete_command(parse(text))
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
    fn sigle_quotes_inside_dobule_quotes_are_ignored() {
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
            parse_parameter_expansion("$test"),
            ParameterExpansion::Simple(Parameter::Variable(Rc::from("test")))
        );
        assert_eq!(
            parse_parameter_expansion("$1"),
            ParameterExpansion::Simple(Parameter::Number(1))
        );
        assert_eq!(
            parse_parameter_expansion("${test}"),
            ParameterExpansion::Simple(Parameter::Variable(Rc::from("test")))
        );
        assert_eq!(
            parse_parameter_expansion("${12345}"),
            ParameterExpansion::Simple(Parameter::Number(12345))
        );
    }

    #[test]
    fn parse_special_parameters() {
        assert_eq!(
            parse_parameter_expansion("$@"),
            ParameterExpansion::Simple(Parameter::Special(SpecialParameter::At))
        );
        assert_eq!(
            parse_parameter_expansion("$*"),
            ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Asterisk))
        );
        assert_eq!(
            parse_parameter_expansion("$#"),
            ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Hash))
        );
        assert_eq!(
            parse_parameter_expansion("$?"),
            ParameterExpansion::Simple(Parameter::Special(SpecialParameter::QuestionMark))
        );
        assert_eq!(
            parse_parameter_expansion("$-"),
            ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Minus))
        );
        assert_eq!(
            parse_parameter_expansion("$$"),
            ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Dollar))
        );
        assert_eq!(
            parse_parameter_expansion("$!"),
            ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Bang))
        );
        assert_eq!(
            parse_parameter_expansion("$0"),
            ParameterExpansion::Simple(Parameter::Special(SpecialParameter::Zero))
        );
    }

    #[test]
    fn parse_parameter_expansion_expression() {
        assert_eq!(
            parse_parameter_expansion("${test:-default}"),
            ParameterExpansion::UnsetUseDefault {
                parameter: Parameter::Variable(Rc::from("test")),
                word: Some(unquoted_literal("default")),
                default_on_null: true,
            }
        );
        assert_eq!(
            parse_parameter_expansion("${test-default}"),
            ParameterExpansion::UnsetUseDefault {
                parameter: Parameter::Variable(Rc::from("test")),
                word: Some(unquoted_literal("default")),
                default_on_null: false,
            }
        );
        assert_eq!(
            parse_parameter_expansion("${test:=default}"),
            ParameterExpansion::UnsetAssignDefault {
                parameter: Parameter::Variable(Rc::from("test")),
                word: Some(unquoted_literal("default")),
                assign_on_null: true,
            }
        );
        assert_eq!(
            parse_parameter_expansion("${test=default}"),
            ParameterExpansion::UnsetAssignDefault {
                parameter: Parameter::Variable(Rc::from("test")),
                word: Some(unquoted_literal("default")),
                assign_on_null: false,
            }
        );
        assert_eq!(
            parse_parameter_expansion("${test:?default}"),
            ParameterExpansion::UnsetError {
                parameter: Parameter::Variable(Rc::from("test")),
                word: Some(unquoted_literal("default")),
                error_on_null: true,
            }
        );
        assert_eq!(
            parse_parameter_expansion("${test?default}"),
            ParameterExpansion::UnsetError {
                parameter: Parameter::Variable(Rc::from("test")),
                word: Some(unquoted_literal("default")),
                error_on_null: false,
            }
        );
        assert_eq!(
            parse_parameter_expansion("${test:+default}"),
            ParameterExpansion::SetUseAlternative {
                parameter: Parameter::Variable(Rc::from("test")),
                word: Some(unquoted_literal("default")),
                substitute_null_with_word: false,
            }
        );
        assert_eq!(
            parse_parameter_expansion("${test+default}"),
            ParameterExpansion::SetUseAlternative {
                parameter: Parameter::Variable(Rc::from("test")),
                word: Some(unquoted_literal("default")),
                substitute_null_with_word: true,
            }
        );
    }

    #[test]
    fn test_parse_parameter_expansion_expression_with_no_default() {
        assert_eq!(
            parse_parameter_expansion("${test:-}"),
            ParameterExpansion::UnsetUseDefault {
                parameter: Parameter::Variable(Rc::from("test")),
                word: None,
                default_on_null: true
            }
        );
        assert_eq!(
            parse_parameter_expansion("${test-}"),
            ParameterExpansion::UnsetUseDefault {
                parameter: Parameter::Variable(Rc::from("test")),
                word: None,
                default_on_null: false
            }
        );
        assert_eq!(
            parse_parameter_expansion("${test:=}"),
            ParameterExpansion::UnsetAssignDefault {
                parameter: Parameter::Variable(Rc::from("test")),
                word: None,
                assign_on_null: true
            }
        );
        assert_eq!(
            parse_parameter_expansion("${test=}"),
            ParameterExpansion::UnsetAssignDefault {
                parameter: Parameter::Variable(Rc::from("test")),
                word: None,
                assign_on_null: false,
            }
        );
        assert_eq!(
            parse_parameter_expansion("${test:?}"),
            ParameterExpansion::UnsetError {
                parameter: Parameter::Variable(Rc::from("test")),
                word: None,
                error_on_null: true
            }
        );
        assert_eq!(
            parse_parameter_expansion("${test?}"),
            ParameterExpansion::UnsetError {
                parameter: Parameter::Variable(Rc::from("test")),
                word: None,
                error_on_null: false
            }
        );
        assert_eq!(
            parse_parameter_expansion("${test:+}"),
            ParameterExpansion::SetUseAlternative {
                parameter: Parameter::Variable(Rc::from("test")),
                word: None,
                substitute_null_with_word: false
            }
        );
        assert_eq!(
            parse_parameter_expansion("${test+}"),
            ParameterExpansion::SetUseAlternative {
                parameter: Parameter::Variable(Rc::from("test")),
                word: None,
                substitute_null_with_word: true
            }
        );
    }

    #[test]
    fn parse_string_operations_in_parameter_expansion() {
        assert_eq!(
            parse_parameter_expansion("${#test}"),
            ParameterExpansion::StrLen(Parameter::Variable(Rc::from("test")))
        );
        assert_eq!(
            parse_parameter_expansion("${test%pattern}"),
            ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable(Rc::from("test")),
                pattern: Some(unquoted_literal("pattern")),
                remove_prefix: false,
                remove_largest: false,
            }
        );
        assert_eq!(
            parse_parameter_expansion("${test%%pattern}"),
            ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable(Rc::from("test")),
                pattern: Some(unquoted_literal("pattern")),
                remove_prefix: false,
                remove_largest: true,
            }
        );
        assert_eq!(
            parse_parameter_expansion("${test#pattern}"),
            ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable(Rc::from("test")),
                pattern: Some(unquoted_literal("pattern")),
                remove_prefix: true,
                remove_largest: false,
            }
        );
        assert_eq!(
            parse_parameter_expansion("${test##pattern}"),
            ParameterExpansion::RemovePattern {
                parameter: Parameter::Variable(Rc::from("test")),
                pattern: Some(unquoted_literal("pattern")),
                remove_prefix: true,
                remove_largest: true,
            }
        );
    }

    #[test]
    fn parse_simple_command_no_assignments_no_redirections_no_arguments() {
        let command = parse_simple_command("pwd");
        assert_eq!(command.words, vec![unquoted_literal("pwd")]);
        assert!(command.assignments.is_empty());
        assert!(command.redirections.is_empty());
    }

    #[test]
    fn parse_simple_command_single_assignment() {
        let command = parse_simple_command("a=1");
        assert_eq!(command.assignments.len(), 1);
        assert_eq!(command.assignments[0].name, Rc::from("a"));
        assert_eq!(command.assignments[0].value, unquoted_literal("1"));
        assert!(command.redirections.is_empty());
        assert!(command.words.is_empty());
    }

    #[test]
    fn parse_simple_command_multiple_assignment() {
        let command =
            parse_simple_command("PATH=/bin:/usr/bin:/usr/local/bin a=1 b=\"this is a test\"");
        assert_eq!(command.assignments.len(), 3);
        assert_eq!(command.assignments[0].name, Rc::from("PATH"));
        assert_eq!(
            command.assignments[0].value,
            unquoted_literal("/bin:/usr/bin:/usr/local/bin")
        );
        assert_eq!(command.assignments[1].name, Rc::from("a"));
        assert_eq!(command.assignments[1].value, unquoted_literal("1"));
        assert_eq!(command.assignments[2].name, Rc::from("b"));
        assert_eq!(
            command.assignments[2].value,
            quoted_literal("this is a test")
        );
        assert!(command.redirections.is_empty());
        assert!(command.words.is_empty());
    }

    #[test]
    fn parse_redirections_without_file_descriptors() {
        assert_eq!(
            parse_single_redirection("> test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectOutput,
                    file: unquoted_literal("test_file")
                }
            }
        );
        assert_eq!(
            parse_single_redirection(">| test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectOutputClobber,
                    file: unquoted_literal("test_file")
                }
            }
        );
        assert_eq!(
            parse_single_redirection(">> test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectOuputAppend,
                    file: unquoted_literal("test_file")
                }
            }
        );
        assert_eq!(
            parse_single_redirection(">& test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::DuplicateOutput,
                    file: unquoted_literal("test_file")
                }
            }
        );
        assert_eq!(
            parse_single_redirection("< test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectInput,
                    file: unquoted_literal("test_file")
                }
            }
        );
        assert_eq!(
            parse_single_redirection("<& test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::DuplicateInput,
                    file: unquoted_literal("test_file")
                }
            }
        );
        assert_eq!(
            parse_single_redirection("<> test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::OpenRW,
                    file: unquoted_literal("test_file")
                }
            }
        );
    }

    #[test]
    fn parse_redirection_with_optional_file_descriptor() {
        assert_eq!(
            parse_single_redirection("2> test_file"),
            Redirection {
                file_descriptor: Some(2),
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectOutput,
                    file: unquoted_literal("test_file")
                }
            }
        );
    }

    #[test]
    fn parse_simple_command_single_redirection() {
        let command = parse_simple_command("> file.txt");
        assert!(command.words.is_empty());
        assert!(command.assignments.is_empty());
        assert_eq!(command.redirections.len(), 1);
        assert_eq!(
            command.redirections[0].kind,
            RedirectionKind::IORedirection {
                kind: IORedirectionKind::RedirectOutput,
                file: unquoted_literal("file.txt")
            }
        );
    }

    #[test]
    fn parse_command_with_redirections() {
        let command = parse_simple_command("< input command > output");
        assert_eq!(command.words, vec![unquoted_literal("command")]);
        assert_eq!(
            command.redirections,
            vec![
                Redirection {
                    file_descriptor: None,
                    kind: RedirectionKind::IORedirection {
                        kind: IORedirectionKind::RedirectInput,
                        file: unquoted_literal("input")
                    }
                },
                Redirection {
                    file_descriptor: None,
                    kind: RedirectionKind::IORedirection {
                        kind: IORedirectionKind::RedirectOutput,
                        file: unquoted_literal("output")
                    }
                }
            ]
        );
        assert!(command.assignments.is_empty());
    }

    #[test]
    fn parse_simple_command_with_arguments() {
        let command = parse_simple_command("echo this is a test");
        assert!(command.assignments.is_empty());
        assert!(command.redirections.is_empty());
        assert_eq!(
            command.words,
            vec![
                unquoted_literal("echo"),
                unquoted_literal("this"),
                unquoted_literal("is"),
                unquoted_literal("a"),
                unquoted_literal("test")
            ]
        )
    }

    #[test]
    fn parse_simple_command_with_arguments_and_redirections() {
        let command = parse_simple_command("cat test_file.txt >> ../other_file.txt");
        assert_eq!(
            command.words,
            vec![unquoted_literal("cat"), unquoted_literal("test_file.txt")]
        );
        assert_eq!(
            command.redirections,
            vec![Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectOuputAppend,
                    file: unquoted_literal("../other_file.txt")
                }
            }]
        );
        assert!(command.assignments.is_empty());
    }

    #[test]
    fn parse_simple_command_with_arguments_redirections_and_assignments() {
        let command = parse_simple_command("CARGO_LOG=warn cargo build > build_result.txt");
        assert_eq!(
            command.assignments,
            vec![Assignment {
                name: Rc::from("CARGO_LOG"),
                value: unquoted_literal("warn")
            }]
        );
        assert_eq!(
            command.words,
            vec![unquoted_literal("cargo"), unquoted_literal("build")]
        );
        assert_eq!(
            command.redirections,
            vec![Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectOutput,
                    file: unquoted_literal("build_result.txt")
                }
            }]
        )
    }

    #[test]
    fn parse_here_document_redirection() {
        assert_eq!(
            parse_single_redirection("<<end\nthis\nis\n\ta\ntest\nend\n"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::HereDocument {
                    contents: "this\nis\n\ta\ntest\n".to_string()
                }
            }
        )
    }

    #[test]
    fn parse_here_document_redirection_remove_leading_tabs() {
        assert_eq!(
            parse_single_redirection("<<-end\nthis\nis\n\ta\n\t\t\t\ttest\nend\n"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::HereDocument {
                    contents: "this\nis\na\ntest\n".to_string()
                }
            }
        )
    }

    #[test]
    fn parse_simple_pipeline() {
        let pipeline = parse_pipeline("echo hello | wc -l");
        assert_eq!(pipeline.commands.len(), 2);
        assert_eq!(
            pipeline.commands[0],
            Command::SimpleCommand(SimpleCommand {
                words: vec![unquoted_literal("echo"), unquoted_literal("hello")],
                ..Default::default()
            })
        );
        assert_eq!(
            pipeline.commands[1],
            Command::SimpleCommand(SimpleCommand {
                words: vec![unquoted_literal("wc"), unquoted_literal("-l")],
                ..Default::default()
            })
        );
    }

    #[test]
    fn parse_simple_conjunction() {
        let conjunction = parse_conjunction("a && b");
        assert_eq!(conjunction.elements.len(), 2);
        assert!(!conjunction.is_async);
        assert_eq!(
            conjunction.elements[0],
            (
                Pipeline {
                    commands: vec![Command::SimpleCommand(SimpleCommand {
                        words: vec![unquoted_literal("a")],
                        ..Default::default()
                    })],
                    negate_status: false
                },
                LogicalOp::And
            )
        );
        assert_eq!(
            conjunction.elements[1],
            (
                Pipeline {
                    commands: vec![Command::SimpleCommand(SimpleCommand {
                        words: vec![unquoted_literal("b")],
                        ..Default::default()
                    })],
                    negate_status: false
                },
                LogicalOp::None
            )
        );
    }

    #[test]
    fn parse_commands_separated_by_semicolon() {
        let command = parse_complete_command("a; b");
        assert_eq!(command.commands.len(), 2);
        assert_eq!(
            command.commands[0],
            Conjunction {
                elements: vec![(
                    Pipeline {
                        commands: vec![Command::SimpleCommand(SimpleCommand {
                            words: vec![unquoted_literal("a")],
                            ..Default::default()
                        })],
                        negate_status: false
                    },
                    LogicalOp::None
                )],
                is_async: false,
            }
        );
        assert_eq!(
            command.commands[1],
            Conjunction {
                elements: vec![(
                    Pipeline {
                        commands: vec![Command::SimpleCommand(SimpleCommand {
                            words: vec![unquoted_literal("b")],
                            ..Default::default()
                        })],
                        negate_status: false
                    },
                    LogicalOp::None
                )],
                is_async: false,
            }
        )
    }

    #[test]
    fn parse_simple_command_substitution() {
        assert_eq!(
            parse_word("$(echo hello)"),
            Word {
                parts: vec![WordPart::CommandSubstitution(CompleteCommand {
                    commands: vec![Conjunction {
                        elements: vec![(
                            Pipeline {
                                commands: vec![Command::SimpleCommand(SimpleCommand {
                                    words: vec![
                                        unquoted_literal("echo"),
                                        unquoted_literal("hello")
                                    ],
                                    ..Default::default()
                                })],
                                negate_status: false
                            },
                            LogicalOp::None
                        )],
                        is_async: false
                    }]
                })]
            }
        );
        assert_eq!(parse_word("`echo hello`"), parse_word("$(echo hello)"));
    }

    #[test]
    fn parse_command_substitution_inside_string() {
        assert_eq!(
            parse_word("\"hello $(echo world)\""),
            Word {
                parts: vec![
                    WordPart::QuotedLiteral("hello ".to_string()),
                    WordPart::CommandSubstitution(CompleteCommand {
                        commands: vec![Conjunction {
                            elements: vec![(
                                Pipeline {
                                    commands: vec![Command::SimpleCommand(SimpleCommand {
                                        words: vec![
                                            unquoted_literal("echo"),
                                            unquoted_literal("world")
                                        ],
                                        ..Default::default()
                                    })],
                                    negate_status: false
                                },
                                LogicalOp::None
                            )],
                            is_async: false
                        }]
                    }),
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
        let inner = Word {
            parts: vec![WordPart::CommandSubstitution(CompleteCommand {
                commands: vec![Conjunction {
                    elements: vec![(
                        Pipeline {
                            commands: vec![Command::SimpleCommand(SimpleCommand {
                                words: vec![unquoted_literal("echo"), unquoted_literal("hello")],
                                ..Default::default()
                            })],
                            negate_status: false,
                        },
                        LogicalOp::None,
                    )],
                    is_async: false,
                }],
            })],
        };
        assert_eq!(
            parse_word("$(echo $(echo hello))"),
            Word {
                parts: vec![WordPart::CommandSubstitution(CompleteCommand {
                    commands: vec![Conjunction {
                        elements: vec![(
                            Pipeline {
                                commands: vec![Command::SimpleCommand(SimpleCommand {
                                    words: vec![unquoted_literal("echo"), inner],
                                    ..Default::default()
                                })],
                                negate_status: false
                            },
                            LogicalOp::None
                        )],
                        is_async: false
                    }]
                })]
            }
        );
    }

    #[test]
    fn test_parse_parameter_expansion_inside_a_string() {
        assert_eq!(
            parse_word("\"hello $test\""),
            Word {
                parts: vec![
                    WordPart::QuotedLiteral("hello ".to_string()),
                    WordPart::ParameterExpansion(ParameterExpansion::Simple(Parameter::Variable(
                        Rc::from("test")
                    ))),
                    WordPart::QuotedLiteral("".to_string())
                ]
            }
        );
        assert_eq!(
            parse_word("\"hello ${test}\""),
            Word {
                parts: vec![
                    WordPart::QuotedLiteral("hello ".to_string()),
                    WordPart::ParameterExpansion(ParameterExpansion::Simple(Parameter::Variable(
                        Rc::from("test")
                    ))),
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
    fn parse_brace_group() {
        assert_eq!(
            parse_compound_command("{word}").0,
            CompoundCommand::BraceGroup(complete_command_from_word(
                unquoted_literal("word"),
                false
            ))
        );
        assert_eq!(
            parse_compound_command("{\ncmd1; cmd2;\ncmd3\n\n\ncmd4 &\n}").0,
            CompoundCommand::BraceGroup(CompleteCommand {
                commands: vec![
                    conjunction_from_word(unquoted_literal("cmd1"), false),
                    conjunction_from_word(unquoted_literal("cmd2"), false),
                    conjunction_from_word(unquoted_literal("cmd3"), false),
                    conjunction_from_word(unquoted_literal("cmd4"), true)
                ]
            })
        )
    }

    #[test]
    fn parse_subshell() {
        // assert_eq!(
        //     parse_compound_command("(word)").0,
        //     CompoundCommand::Subshell(complete_command_from_word(unquoted_literal("word"), false))
        // );
        assert_eq!(
            parse_compound_command("(\ncmd1; cmd2 & cmd3;\n\n\ncmd4 &\n)").0,
            CompoundCommand::Subshell(CompleteCommand {
                commands: vec![
                    conjunction_from_word(unquoted_literal("cmd1"), false),
                    conjunction_from_word(unquoted_literal("cmd2"), true),
                    conjunction_from_word(unquoted_literal("cmd3"), false),
                    conjunction_from_word(unquoted_literal("cmd4"), true)
                ]
            })
        )
    }

    #[test]
    fn parse_for_clause() {
        assert_eq!(
            parse_compound_command("for i in 1 2 3; do\ncmd\ndone").0,
            CompoundCommand::ForClause {
                iter_var: Rc::from("i"),
                words: vec![
                    unquoted_literal("1"),
                    unquoted_literal("2"),
                    unquoted_literal("3")
                ],
                body: CompleteCommand {
                    commands: vec![conjunction_from_word(unquoted_literal("cmd"), false)]
                }
            }
        );
    }

    #[test]
    fn parse_empty_case_clause() {
        assert_eq!(
            parse_compound_command("case word in esac").0,
            CompoundCommand::CaseClause {
                arg: unquoted_literal("word"),
                cases: Vec::new()
            }
        );
        assert_eq!(
            parse_compound_command("case word \n\nin\n esac").0,
            CompoundCommand::CaseClause {
                arg: unquoted_literal("word"),
                cases: Vec::new()
            }
        )
    }

    #[test]
    fn parse_case_clause_one_case() {
        assert_eq!(
            parse_compound_command("case word in (pattern) cmd;; esac").0,
            CompoundCommand::CaseClause {
                arg: unquoted_literal("word"),
                cases: vec![CaseItem {
                    pattern: vec![unquoted_literal("pattern")],
                    body: complete_command_from_word(unquoted_literal("cmd"), false)
                }]
            }
        );
        assert_eq!(
            parse_compound_command("case word in (pattern) cmd;; esac"),
            parse_compound_command("case word in pattern) cmd;; esac")
        );
        assert_eq!(
            parse_compound_command("case word in (pattern) cmd;; esac"),
            parse_compound_command("case word\n in \n(pattern)\n\ncmd\nesac")
        );
    }

    #[test]
    fn parse_case_clause_multiple_cases() {
        assert_eq!(
            parse_compound_command(
                "case word in (pattern1) cmd1;; (pattern2) cmd2;; (pattern3) cmd3;; esac"
            )
            .0,
            CompoundCommand::CaseClause {
                arg: unquoted_literal("word"),
                cases: vec![
                    CaseItem {
                        pattern: vec![unquoted_literal("pattern1")],
                        body: complete_command_from_word(unquoted_literal("cmd1"), false)
                    },
                    CaseItem {
                        pattern: vec![unquoted_literal("pattern2")],
                        body: complete_command_from_word(unquoted_literal("cmd2"), false)
                    },
                    CaseItem {
                        pattern: vec![unquoted_literal("pattern3")],
                        body: complete_command_from_word(unquoted_literal("cmd3"), false)
                    }
                ]
            }
        );
        assert_eq!(
            parse_compound_command(
                "case word in (pattern1) cmd1;; (pattern2) cmd2;; (pattern3) cmd3;; esac"
            ),
            parse_compound_command(
                "case word in\n(pattern1)\n cmd1\n;;\n (pattern2) \ncmd2\n;;\n (pattern3)\n cmd3\n esac"
            )
        );
    }

    #[test]
    fn parse_if_clause_no_else() {
        assert_eq!(
            parse_compound_command("if condition; then cmd; fi").0,
            CompoundCommand::IfClause {
                if_chain: vec![If {
                    condition: CompleteCommand {
                        commands: vec![conjunction_from_word(unquoted_literal("condition"), false)]
                    },
                    body: CompleteCommand {
                        commands: vec![conjunction_from_word(unquoted_literal("cmd"), false)]
                    }
                }]
            }
        );
    }

    #[test]
    fn parse_if_clause_one_else() {
        assert_eq!(
            parse_compound_command("if condition; then cmd; else cmd2; fi").0,
            CompoundCommand::IfClause {
                if_chain: vec![
                    If {
                        condition: CompleteCommand {
                            commands: vec![conjunction_from_word(
                                unquoted_literal("condition"),
                                false
                            )]
                        },
                        body: CompleteCommand {
                            commands: vec![conjunction_from_word(unquoted_literal("cmd"), false)]
                        }
                    },
                    If {
                        condition: CompleteCommand {
                            commands: Vec::new()
                        },
                        body: CompleteCommand {
                            commands: vec![conjunction_from_word(unquoted_literal("cmd2"), false)]
                        }
                    }
                ]
            }
        );
    }

    #[test]
    fn parse_if_clause_with_elif_chain() {
        assert_eq!(
            parse_compound_command("if condition1; then cmd1; elif condition2; then cmd2; elif condition3; then cmd3; else cmd4; fi").0,
            CompoundCommand::IfClause {
                if_chain: vec![
                    If {
                        condition: CompleteCommand {
                            commands: vec![conjunction_from_word(
                                unquoted_literal("condition1"),
                                false
                            )]
                        },
                        body: CompleteCommand {
                            commands: vec![conjunction_from_word(unquoted_literal("cmd1"), false)]
                        }
                    },
                    If {
                        condition: CompleteCommand {
                            commands: vec![conjunction_from_word(
                                unquoted_literal("condition2"),
                                false
                            )]
                        },
                        body: CompleteCommand {
                            commands: vec![conjunction_from_word(unquoted_literal("cmd2"), false)]
                        }
                    },
                    If {
                        condition: CompleteCommand {
                            commands: vec![conjunction_from_word(
                                unquoted_literal("condition3"),
                                false
                            )]
                        },
                        body: CompleteCommand {
                            commands: vec![conjunction_from_word(unquoted_literal("cmd3"), false)]
                        }
                    },
                    If {
                        condition: CompleteCommand {
                            commands: Vec::new()
                        },
                        body: CompleteCommand {
                            commands: vec![conjunction_from_word(unquoted_literal("cmd4"), false)]
                        }
                    }
                ]
            }
        );
    }

    #[test]
    fn parse_while_clause() {
        assert_eq!(
            parse_compound_command("while condition; do cmd; done").0,
            CompoundCommand::WhileClause {
                condition: CompleteCommand {
                    commands: vec![conjunction_from_word(unquoted_literal("condition"), false)]
                },
                body: CompleteCommand {
                    commands: vec![conjunction_from_word(unquoted_literal("cmd"), false)]
                }
            }
        );
    }

    #[test]
    fn parse_until_clause() {
        assert_eq!(
            parse_compound_command("until condition; do cmd; done").0,
            CompoundCommand::UntilClause {
                condition: CompleteCommand {
                    commands: vec![conjunction_from_word(unquoted_literal("condition"), false)]
                },
                body: CompleteCommand {
                    commands: vec![conjunction_from_word(unquoted_literal("cmd"), false)]
                }
            }
        );
    }

    #[test]
    fn parse_compound_command_with_redirections() {
        let (command, redirections) = parse_compound_command("if a; then b; fi > file.txt");
        assert_eq!(
            command,
            CompoundCommand::IfClause {
                if_chain: vec![If {
                    condition: CompleteCommand {
                        commands: vec![conjunction_from_word(unquoted_literal("a"), false)]
                    },
                    body: CompleteCommand {
                        commands: vec![conjunction_from_word(unquoted_literal("b"), false)]
                    }
                }]
            }
        );
        assert_eq!(
            redirections,
            vec![Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectOutput,
                    file: unquoted_literal("file.txt")
                }
            }]
        );
    }

    #[test]
    fn parse_function_definition() {
        assert_eq!(
            parse_command("function_name() { cmd; }"),
            Command::FunctionDefinition(FunctionDefinition {
                name: Rc::from("function_name"),
                body: CompoundCommand::BraceGroup(complete_command_from_word(
                    unquoted_literal("cmd"),
                    false
                ))
            })
        );

        assert_eq!(
            parse_command("function_name() ( cmd1 )"),
            Command::FunctionDefinition(FunctionDefinition {
                name: Rc::from("function_name"),
                body: CompoundCommand::Subshell(CompleteCommand {
                    commands: vec![conjunction_from_word(unquoted_literal("cmd1"), false),]
                })
            })
        );
    }

    #[test]
    fn parse_pipeline_negate_status() {
        assert_eq!(
            parse_pipeline("!cmd"),
            Pipeline {
                commands: vec![Command::SimpleCommand(SimpleCommand {
                    words: vec![unquoted_literal("cmd")],
                    ..Default::default()
                })],
                negate_status: true
            }
        );
    }

    #[test]
    fn parse_reserved_word_as_simple_word_when_used_as_command_argument() {
        assert_eq!(
            parse_simple_command("echo if"),
            SimpleCommand {
                words: vec![unquoted_literal("echo"), unquoted_literal("if")],
                ..Default::default()
            }
        );
    }

    #[test]
    fn for_iterator_variable_can_be_a_reserved_word() {
        assert_eq!(
            parse_compound_command("for for in 1 2 3; do\ncmd\ndone").0,
            CompoundCommand::ForClause {
                iter_var: Rc::from("for"),
                words: vec![
                    unquoted_literal("1"),
                    unquoted_literal("2"),
                    unquoted_literal("3")
                ],
                body: CompleteCommand {
                    commands: vec![conjunction_from_word(unquoted_literal("cmd"), false)]
                }
            }
        );
    }

    #[test]
    fn case_arg_can_be_a_reserved_word() {
        assert_eq!(
            parse_compound_command("case case in esac").0,
            CompoundCommand::CaseClause {
                arg: unquoted_literal("case"),
                cases: Vec::new()
            }
        );
    }

    #[test]
    fn word_after_in_can_be_a_reserved_word() {
        assert_eq!(
            parse_compound_command("for word in in; do cmd; done").0,
            CompoundCommand::ForClause {
                iter_var: Rc::from("word"),
                words: vec![unquoted_literal("in")],
                body: CompleteCommand {
                    commands: vec![conjunction_from_word(unquoted_literal("cmd"), false)]
                }
            }
        );
    }
}
