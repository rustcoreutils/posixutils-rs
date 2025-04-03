//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::borrow::Cow;
use std::rc::Rc;

use crate::nonempty::NonEmpty;
use crate::parse::command::{
    Assignment, CaseItem, Command, CommandType, CompleteCommand, CompoundCommand, Conjunction,
    FunctionDefinition, IORedirectionKind, If, LogicalOp, Name, Pipeline, Redirection,
    RedirectionKind, SimpleCommand,
};
use crate::parse::lexer::command_lexer::{CommandLexer, CommandToken};
use crate::parse::lexer::is_blank;
use crate::parse::word::{
    Parameter, ParameterExpansion, SpecialParameter, Word, WordPair, WordPart,
};
use crate::parse::word_parser::parse_word_pair;
use crate::parse::{AliasTable, ParseResult, ParserError};

pub fn is_valid_name(name: &str) -> bool {
    name.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_')
        && name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn try_into_assignment(word: &str, line_no: u32) -> ParseResult<Result<Assignment, &str>> {
    if !word.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_') {
        return Ok(Err(word));
    }
    if let Some((pos, c)) = word
        .char_indices()
        .find(|(_, c)| !c.is_ascii_alphanumeric() && *c != '_')
    {
        if c == '=' {
            let (name, value) = word.split_at(pos);
            let name = Rc::from(name);
            parse_word_pair(&value[1..], line_no, false).map(|value| Ok(Assignment { name, value }))
        } else {
            Ok(Err(word))
        }
    } else {
        Ok(Err(word))
    }
}

pub struct CommandParser<'src> {
    lexer: CommandLexer<'src>,
    lookahead: CommandToken<'src>,
    lookahead_lineno: u32,
    start_lineno: u32,
    parsed_one_command: bool,
}

impl<'src> CommandParser<'src> {
    fn reached_eof(&self) -> bool {
        self.lookahead == CommandToken::Eof
    }

    pub fn lineno(&self) -> u32 {
        self.lookahead_lineno + self.start_lineno
    }

    /// advances the current shell token and returns the previous shell lookahead
    fn advance(&mut self) -> ParseResult<CommandToken<'src>> {
        let (mut next_token, next_token_line_no) = self.lexer.next_token()?;
        self.lookahead_lineno = next_token_line_no;
        std::mem::swap(&mut self.lookahead, &mut next_token);
        Ok(next_token)
    }

    fn match_alternatives(
        &mut self,
        tokens: &[CommandToken],
    ) -> ParseResult<Option<CommandToken<'src>>> {
        if tokens.contains(&self.lookahead) {
            self.advance().map(Some)
        } else {
            Ok(None)
        }
    }

    fn match_token(&mut self, token: CommandToken) -> ParseResult<()> {
        if self.lookahead != token {
            return Err(ParserError::new(
                self.lookahead_lineno,
                format!("expected {}, got {}", token, self.lookahead),
                self.reached_eof(),
            ));
        }
        self.advance()?;
        Ok(())
    }

    fn match_name(&mut self) -> ParseResult<Name> {
        let line_no = self.lookahead_lineno;
        match self.lookahead.as_word_str() {
            Some(word) if is_valid_name(word) => self
                .advance()
                .map(|word| word.into_word_cow().unwrap().into_owned().into()),
            _ => Err(ParserError::new(
                line_no,
                format!("expected name, got {}", self.lookahead),
                self.reached_eof(),
            )),
        }
    }

    fn parse_word_pair(&mut self) -> ParseResult<WordPair> {
        let line_no = self.lookahead_lineno;
        let token = self.advance()?;
        if let Some(word) = token.as_word_str() {
            parse_word_pair(word, line_no, false)
        } else {
            Err(ParserError::new(
                line_no,
                format!("expected word, found {}", token),
                self.reached_eof(),
            ))
        }
    }

    fn match_shell_token_opt(&mut self, token: CommandToken) -> ParseResult<()> {
        if self.lookahead == token {
            self.advance()?;
        }
        Ok(())
    }

    fn skip_linebreak(&mut self) -> ParseResult<()> {
        // "\n"*
        while self.lookahead == CommandToken::Newline {
            self.advance()?;
        }
        Ok(())
    }

    fn parse_redirection_kind(&mut self) -> ParseResult<Option<RedirectionKind>> {
        let kind = match &self.lookahead {
            CommandToken::Greater => IORedirectionKind::RedirectOutput,
            CommandToken::Clobber => IORedirectionKind::RedirectOutputClobber,
            CommandToken::DGreat => IORedirectionKind::RedirectOuputAppend,
            CommandToken::GreatAnd => IORedirectionKind::DuplicateOutput,
            CommandToken::Less => IORedirectionKind::RedirectInput,
            CommandToken::LessAnd => IORedirectionKind::DuplicateInput,
            CommandToken::LessGreat => IORedirectionKind::OpenRW,
            CommandToken::HereDocument {
                delimiter,
                contents,
            } => {
                let contents = parse_word_pair(contents.as_ref(), self.lookahead_lineno, true)?;
                let delimiter = delimiter.to_string();
                self.advance()?;
                return Ok(Some(RedirectionKind::HereDocument {
                    delimiter,
                    contents,
                }));
            }
            CommandToken::QuotedHereDocument { .. } => {
                if let CommandToken::QuotedHereDocument {
                    start_delimiter,
                    end_delimiter,
                    contents,
                } = self.advance()?
                {
                    return Ok(Some(RedirectionKind::QuotedHereDocument {
                        start_delimiter: start_delimiter.into_owned(),
                        end_delimiter: end_delimiter.into_owned(),
                        contents: contents.into_owned(),
                    }));
                } else {
                    unreachable!()
                }
            }
            _ => return Ok(None),
        };
        let line_no = self.lookahead_lineno;
        // advance the operator
        self.advance()?;
        match self.advance()? {
            CommandToken::Word(word) => {
                let file = parse_word_pair(&word, line_no, false)?;
                Ok(Some(RedirectionKind::IORedirection { kind, file }))
            }
            other => Err(ParserError::new(
                self.lookahead_lineno,
                format!("expected word, got {}", other),
                other == CommandToken::Eof,
            )),
        }
    }

    fn parse_redirection_opt(&mut self) -> ParseResult<Option<Redirection>> {
        if let CommandToken::IoNumber(n) = self.lookahead {
            if !(0..1023).contains(&n) {
                return Err(ParserError::new(
                    self.lookahead_lineno,
                    "invalid file descriptor",
                    false,
                ));
            }
            // skip number
            self.advance()?;
            if let Some(kind) = self.parse_redirection_kind()? {
                Ok(Some(Redirection {
                    kind,
                    file_descriptor: Some(n),
                }))
            } else {
                Err(ParserError::new(
                    self.lookahead_lineno,
                    "expected redirection operator after file descriptor",
                    self.lookahead == CommandToken::Eof,
                ))
            }
        } else {
            Ok(self.parse_redirection_kind()?.map(|kind| Redirection {
                file_descriptor: None,
                kind,
            }))
        }
    }

    fn is_currently_processing_substitution(&self, word: &str) -> bool {
        self.lexer.is_currently_processing_tag(word)
    }

    fn alias_substitution(
        &mut self,
        word: Cow<'src, str>,
        apply_alias_substitution_to_next_word: &mut bool,
        alias_table: &AliasTable,
    ) -> ParseResult<Option<WordPair>> {
        let mut next_substitution = word;
        let mut performed_one_substitution = false;
        loop {
            if self.is_currently_processing_substitution(next_substitution.as_ref()) {
                return parse_word_pair(&next_substitution, self.lookahead_lineno, false).map(Some);
            }
            if let Some(alias) = alias_table.get(next_substitution.as_ref()) {
                *apply_alias_substitution_to_next_word = alias.ends_with(is_blank);
                self.lexer.insert_text_at_current_position(
                    alias.to_string().into(),
                    next_substitution.as_ref(),
                );
                performed_one_substitution = true;
                self.advance()?;
                if let CommandToken::Word(word) = &self.lookahead {
                    next_substitution = word.clone();
                } else {
                    return Ok(None);
                }
            } else {
                if !performed_one_substitution {
                    *apply_alias_substitution_to_next_word = false;
                }
                return parse_word_pair(next_substitution.as_ref(), self.lookahead_lineno, false)
                    .map(Some);
            }
        }
    }

    fn parse_simple_command(
        &mut self,
        end: CommandToken,
        alias_table: &AliasTable,
    ) -> ParseResult<Option<SimpleCommand>> {
        // simple_command = (io_redirect | assignment_word)* word? (io_redirect | word)*

        let mut command = SimpleCommand::default();
        let mut continue_to_apply_alias_substitution = true;

        while command.words.is_empty() {
            if self.lookahead == end {
                return Ok(command.none_if_empty());
            }
            match self.lookahead.as_word_str() {
                Some(word) => {
                    match try_into_assignment(word, self.lookahead_lineno)? {
                        Ok(assignment) => command.assignments.push(assignment),
                        Err(word) => {
                            if continue_to_apply_alias_substitution {
                                let next_word = self.alias_substitution(
                                    word.to_string().into(),
                                    &mut continue_to_apply_alias_substitution,
                                    alias_table,
                                )?;
                                command.words.extend(next_word.into_iter());
                            } else {
                                command.words.push(parse_word_pair(
                                    word,
                                    self.lookahead_lineno,
                                    false,
                                )?);
                            }
                        }
                    };
                    self.advance()?;
                }
                None => match self.parse_redirection_opt()? {
                    Some(redirection) => command.redirections.push(redirection),
                    None => break,
                },
            }
        }

        loop {
            if self.lookahead == end {
                return Ok(command.none_if_empty());
            }
            match self.lookahead.as_word_str() {
                Some(word) => {
                    if continue_to_apply_alias_substitution {
                        let next_word = self.alias_substitution(
                            word.to_string().into(),
                            &mut continue_to_apply_alias_substitution,
                            alias_table,
                        )?;
                        command.words.extend(next_word);
                    } else {
                        command
                            .words
                            .push(parse_word_pair(word, self.lookahead_lineno, false)?);
                    }
                    self.advance()?;
                }
                None => match self.parse_redirection_opt()? {
                    Some(redirection) => command.redirections.push(redirection),
                    None => break,
                },
            }
        }

        Ok(command.none_if_empty())
    }

    fn parse_compound_list(
        &mut self,
        end: CommandToken,
        alias_table: &AliasTable,
    ) -> ParseResult<CompleteCommand> {
        self.skip_linebreak()?;
        let list_start = self.lookahead_lineno;

        const END_TOKENS: &[CommandToken] = &[
            CommandToken::RParen,
            CommandToken::RBrace,
            CommandToken::Do,
            CommandToken::Done,
            CommandToken::Elif,
            CommandToken::Else,
            CommandToken::Esac,
            CommandToken::Fi,
            CommandToken::Then,
        ];

        let mut last_conjunction = self.parse_and_or(end.clone(), alias_table)?;
        let mut commands = Vec::new();
        while let Some(mut conjunction) = last_conjunction {
            match self.lookahead {
                CommandToken::And => {
                    conjunction.is_async = true;
                }
                CommandToken::SemiColon | CommandToken::Newline => {}
                _ => {
                    commands.push(conjunction);
                    break;
                }
            }
            self.advance()?;
            self.skip_linebreak()?;
            if END_TOKENS.contains(&self.lookahead) {
                commands.push(conjunction);
                break;
            }
            commands.push(conjunction);
            last_conjunction = self.parse_and_or(end.clone(), alias_table)?;
        }
        if let Ok(commands) = commands.try_into() {
            Ok(CompleteCommand { commands })
        } else {
            Err(ParserError::new(
                list_start,
                "expected command",
                self.lookahead == CommandToken::Eof,
            ))
        }
    }

    fn parse_brace_group(&mut self, alias_table: &AliasTable) -> ParseResult<CompoundCommand> {
        // consume '{'
        self.advance()?;
        let inner = self.parse_compound_list(CommandToken::RBrace, alias_table)?;
        self.match_token(CommandToken::RBrace)?;
        Ok(CompoundCommand::BraceGroup(inner))
    }

    fn parse_subshell(&mut self, alias_table: &AliasTable) -> ParseResult<CompoundCommand> {
        // consume '('
        self.advance()?;
        let inner = self.parse_compound_list(CommandToken::RParen, alias_table)?;
        self.match_token(CommandToken::RParen)?;
        Ok(CompoundCommand::Subshell(inner))
    }

    fn parse_do_group(&mut self, alias_table: &AliasTable) -> ParseResult<CompleteCommand> {
        self.match_token(CommandToken::Do)?;
        let inner = self.parse_compound_list(CommandToken::Done, alias_table)?;
        self.match_token(CommandToken::Done)?;
        Ok(inner)
    }

    fn parse_for_clause(&mut self, alias_table: &AliasTable) -> ParseResult<CompoundCommand> {
        // consume 'for'
        self.advance()?;
        let iter_var = self.match_name()?;
        self.skip_linebreak()?;
        let mut words = Vec::new();
        if self.lookahead == CommandToken::In {
            self.advance()?;
            while self.lookahead.as_word_str().is_some() {
                let word = self.advance()?.into_word_cow().unwrap();
                words.push(parse_word_pair(&word, self.lookahead_lineno, false)?);
            }
        } else {
            let default_iterable_word = Word {
                parts: vec![WordPart::ParameterExpansion {
                    expansion: ParameterExpansion::Simple(Parameter::Special(SpecialParameter::At)),
                    inside_double_quotes: false,
                }],
            };
            words.push(WordPair::new(default_iterable_word, "$@"))
        }
        match self.lookahead {
            CommandToken::SemiColon | CommandToken::Newline => {
                self.advance()?;
                self.skip_linebreak()?;
            }
            _ => {}
        }
        let body = self.parse_do_group(alias_table)?;
        Ok(CompoundCommand::ForClause {
            iter_var,
            words,
            body,
        })
    }

    fn parse_case_item(&mut self, alias_table: &AliasTable) -> ParseResult<CaseItem> {
        self.match_shell_token_opt(CommandToken::LParen)?;
        let mut pattern = Vec::new();
        while self.lookahead != CommandToken::RParen {
            pattern.push(self.parse_word_pair()?);
            if self.lookahead != CommandToken::Pipe {
                break;
            }
            self.advance()?;
        }
        self.match_token(CommandToken::RParen)?;
        let pattern = pattern.try_into().map_err(|_| {
            ParserError::new(
                self.lookahead_lineno,
                "expected pattern",
                self.lookahead == CommandToken::Eof,
            )
        })?;

        let body = self.parse_compound_list(CommandToken::Eof, alias_table)?;

        if self.lookahead == CommandToken::DSemi {
            self.advance()?;
            self.skip_linebreak()?;
        } else if self.lookahead != CommandToken::Esac {
            return Err(ParserError::new(
                self.lookahead_lineno,
                format!("expected ';;', found {}", self.lookahead),
                self.lookahead == CommandToken::Eof,
            ));
        }

        Ok(CaseItem { body, pattern })
    }

    fn parse_case_clause(&mut self, alias_table: &AliasTable) -> ParseResult<CompoundCommand> {
        // consume 'case'
        self.advance()?;
        let arg = self.parse_word_pair()?;
        self.skip_linebreak()?;
        self.match_token(CommandToken::In)?;
        self.skip_linebreak()?;
        let mut cases = Vec::new();
        loop {
            match self.lookahead {
                CommandToken::Esac => break,
                _ => {
                    cases.push(self.parse_case_item(alias_table)?);
                }
            }
        }
        self.match_token(CommandToken::Esac)?;
        Ok(CompoundCommand::CaseClause { arg, cases })
    }

    fn parse_if_clause(&mut self, alias_table: &AliasTable) -> ParseResult<CompoundCommand> {
        // consume 'if'
        self.advance()?;
        // there is a terminator after the condition, we don't need to terminate on CommandToken::Then
        let condition = self.parse_compound_list(CommandToken::Eof, alias_table)?;
        self.match_token(CommandToken::Then)?;
        let then_part = self.parse_compound_list(CommandToken::Eof, alias_table)?;
        let mut if_chain = NonEmpty::new(If {
            condition,
            body: then_part,
        });
        while self.lookahead == CommandToken::Elif {
            // consume 'elif'
            self.advance()?;
            let condition = self.parse_compound_list(CommandToken::Eof, alias_table)?;
            self.match_token(CommandToken::Then)?;
            let then_part = self.parse_compound_list(CommandToken::Eof, alias_table)?;
            if_chain.push(If {
                condition,
                body: then_part,
            });
        }
        let mut else_body = None;
        if self.lookahead == CommandToken::Else {
            self.advance()?;
            else_body = Some(self.parse_compound_list(CommandToken::Eof, alias_table)?);
        }
        self.match_token(CommandToken::Fi)?;
        Ok(CompoundCommand::IfClause {
            if_chain,
            else_body,
        })
    }

    fn parse_while_clause(&mut self, alias_table: &AliasTable) -> ParseResult<CompoundCommand> {
        // consume 'while'
        self.advance()?;
        let condition = self.parse_compound_list(CommandToken::Eof, alias_table)?;
        let body = self.parse_do_group(alias_table)?;
        Ok(CompoundCommand::WhileClause { condition, body })
    }

    fn parse_until_clause(&mut self, alias_table: &AliasTable) -> ParseResult<CompoundCommand> {
        // consume 'until'
        self.advance()?;
        let condition = self.parse_compound_list(CommandToken::Eof, alias_table)?;
        let body = self.parse_do_group(alias_table)?;
        Ok(CompoundCommand::UntilClause { condition, body })
    }

    fn parse_compound_command(
        &mut self,
        alias_table: &AliasTable,
    ) -> ParseResult<Option<CompoundCommand>> {
        match &self.lookahead {
            CommandToken::LParen => self.parse_subshell(alias_table).map(Some),
            CommandToken::LBrace => self.parse_brace_group(alias_table).map(Some),
            CommandToken::For => self.parse_for_clause(alias_table).map(Some),
            CommandToken::Case => self.parse_case_clause(alias_table).map(Some),
            CommandToken::If => self.parse_if_clause(alias_table).map(Some),
            CommandToken::While => self.parse_while_clause(alias_table).map(Some),
            CommandToken::Until => self.parse_until_clause(alias_table).map(Some),
            _ => Ok(None),
        }
    }

    fn parse_function_definition(
        &mut self,
        name: Name,
        alias_table: &AliasTable,
    ) -> ParseResult<FunctionDefinition> {
        // consume '('
        self.advance()?;
        self.match_token(CommandToken::RParen)?;
        if let Some(body) = self.parse_compound_command(alias_table)? {
            Ok(FunctionDefinition {
                name,
                body: Rc::from(body),
            })
        } else {
            Err(ParserError::new(
                self.lookahead_lineno,
                "expected compound command",
                self.lookahead == CommandToken::Eof,
            ))
        }
    }

    fn parse_command(
        &mut self,
        end: CommandToken,
        alias_table: &AliasTable,
    ) -> ParseResult<Option<Command>> {
        // command =
        // 			| compound_command redirect_list?
        // 			| simple_command
        // 			| function_definition
        let start_lineno = self.lineno();
        if let Some(compound_command) = self.parse_compound_command(alias_table)? {
            let mut redirections = Vec::new();
            while let Some(redirection) = self.parse_redirection_opt()? {
                redirections.push(redirection);
            }
            Ok(Some(Command::new(
                CommandType::CompoundCommand {
                    command: compound_command,
                    redirections,
                },
                start_lineno,
            )))
        } else {
            let command_type = match &self.lookahead {
                CommandToken::Word(word) if is_valid_name(word) => {
                    if self.lexer.is_next_lparen() {
                        let word = self.advance()?.into_word_cow().unwrap();
                        assert_eq!(self.lookahead, CommandToken::LParen);
                        Some(
                            self.parse_function_definition(word.into_owned().into(), alias_table)
                                .map(CommandType::FunctionDefinition)?,
                        )
                    } else {
                        self.parse_simple_command(end, alias_table)?
                            .map(CommandType::SimpleCommand)
                    }
                }
                _ => self
                    .parse_simple_command(end, alias_table)?
                    .map(CommandType::SimpleCommand),
            };
            Ok(command_type.map(|type_| Command::new(type_, self.lineno())))
        }
    }

    fn parse_pipeline(
        &mut self,
        end: CommandToken,
        alias_table: &AliasTable,
    ) -> ParseResult<Option<Pipeline>> {
        // pipeline = "!" command ("|" linebreak command)*
        let negate_status = self.match_alternatives(&[CommandToken::Bang])?.is_some();
        let mut commands = if let Some(command) = self.parse_command(end.clone(), alias_table)? {
            NonEmpty::new(command)
        } else {
            return Ok(None);
        };
        while self.lookahead == CommandToken::Pipe {
            let pipe_location = self.lookahead_lineno;
            self.advance()?;
            self.skip_linebreak()?;
            if let Some(command) = self.parse_command(end.clone(), alias_table)? {
                commands.push(command);
            } else {
                return Err(ParserError::new(
                    pipe_location,
                    "right hand side of pipe operator should be a command",
                    self.lookahead == CommandToken::Eof,
                ));
            }
        }
        Ok(Some(Pipeline {
            commands,
            negate_status,
        }))
    }

    fn parse_and_or(
        &mut self,
        end: CommandToken,
        alias_table: &AliasTable,
    ) -> ParseResult<Option<Conjunction>> {
        // and_or = pipeline (("&&" | "||") linebreak pipeline)*
        let mut last = if let Some(pipeline) = self.parse_pipeline(end.clone(), alias_table)? {
            pipeline
        } else {
            return Ok(None);
        };
        let mut elements = Vec::new();
        while let Some(op) = self.match_alternatives(&[CommandToken::AndIf, CommandToken::OrIf])? {
            let op = match op {
                CommandToken::AndIf => LogicalOp::And,
                CommandToken::OrIf => LogicalOp::Or,
                _ => unreachable!(),
            };
            let operator_location = self.lookahead_lineno;
            self.skip_linebreak()?;
            let next = if let Some(next) = self.parse_pipeline(end.clone(), alias_table)? {
                next
            } else {
                return Err(ParserError::new(
                    operator_location,
                    "right hand side of pipe operator should be a command",
                    self.lookahead == CommandToken::Eof,
                ));
            };
            let previous = last;
            last = next;
            elements.push((previous, op));
        }
        elements.push((last, LogicalOp::None));
        Ok(Some(Conjunction {
            elements: elements.try_into().unwrap(),
            // temporary value, will be set by the caller
            is_async: false,
        }))
    }

    fn parse_complete_command(&mut self, alias_table: &AliasTable) -> ParseResult<CompleteCommand> {
        // complete_command = and_or (separator_op and_or)* separator_op?
        let mut commands = Vec::new();
        while self.lookahead != CommandToken::Newline || self.lookahead != CommandToken::Eof {
            let command_start = self.lookahead_lineno;
            if let Some(mut and_or) = self.parse_and_or(CommandToken::Eof, alias_table)? {
                if self.lookahead == CommandToken::And {
                    and_or.is_async = true;
                }
                commands.push(and_or);
                if self.lookahead == CommandToken::And || self.lookahead == CommandToken::SemiColon
                {
                    self.advance()?;
                } else {
                    break;
                }
            } else if commands.is_empty() {
                return Err(ParserError::new(command_start, "expected command", false));
            } else {
                break;
            };
        }
        Ok(CompleteCommand {
            commands: commands.try_into().unwrap(),
        })
    }

    pub fn parse_next_command(
        &mut self,
        alias_table: &AliasTable,
    ) -> ParseResult<Option<CompleteCommand>> {
        if self.lookahead == CommandToken::Eof {
            return Ok(None);
        }
        if self.parsed_one_command {
            self.match_token(CommandToken::Newline)?;
            self.skip_linebreak()?;
            if self.lookahead == CommandToken::Eof {
                return Ok(None);
            }
        }
        let command = self.parse_complete_command(alias_table)?;
        self.parsed_one_command = true;
        Ok(Some(command))
    }

    pub fn new(source: &'src str, start_lineno: u32) -> ParseResult<Self> {
        let mut lexer = CommandLexer::new(source);
        let (lookahead, lookahead_lineno) = lexer.next_token()?;
        let mut parser = Self {
            lexer,
            lookahead,
            lookahead_lineno,
            parsed_one_command: false,
            start_lineno,
        };
        parser.skip_linebreak()?;
        Ok(parser)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::word::test_utils::{
        quoted_literal, special_parameter, unquoted_literal, unquoted_literal_pair,
    };
    use crate::parse::word::SpecialParameter;

    fn parse_complete_command(
        text: &str,
        alias_table: AliasTable,
    ) -> ParseResult<Option<CompleteCommand>> {
        let mut parser = CommandParser::new(text, 1)?;
        parser.parse_next_command(&alias_table)
    }

    fn parse_correct_complete_command(
        text: &str,
        alias_table: AliasTable,
    ) -> Option<CompleteCommand> {
        parse_complete_command(text, alias_table).expect("parsing failed")
    }

    fn pipeline_from_word_pair(word: WordPair) -> Pipeline {
        Pipeline {
            commands: NonEmpty::new(
                CommandType::SimpleCommand(SimpleCommand {
                    words: vec![word],
                    ..Default::default()
                })
                .into(),
            ),
            negate_status: false,
        }
    }

    fn conjunction_from_word_pair(word: WordPair, is_async: bool) -> Conjunction {
        Conjunction {
            elements: NonEmpty::new((pipeline_from_word_pair(word), LogicalOp::None)),
            is_async,
        }
    }

    fn complete_command_from_word_pair(word: WordPair, is_async: bool) -> CompleteCommand {
        CompleteCommand {
            commands: NonEmpty::new(conjunction_from_word_pair(word, is_async)),
        }
    }

    fn unwrap_conjunction(cmd: CompleteCommand) -> Conjunction {
        assert_eq!(cmd.commands.len(), 1);
        cmd.commands.into_iter().next().unwrap()
    }

    fn unwrap_pipeline(cmd: CompleteCommand) -> Pipeline {
        let conjunction = unwrap_conjunction(cmd);
        assert_eq!(conjunction.elements.len(), 1);
        let (pipeline, _) = conjunction.elements.into_iter().next().unwrap();
        pipeline
    }

    fn unwrap_command(cmd: CompleteCommand) -> CommandType {
        let pipeline = unwrap_pipeline(cmd);
        assert_eq!(pipeline.commands.len(), 1);
        pipeline.commands.into_iter().next().unwrap().type_
    }

    fn unwrap_simple_command(cmd: CompleteCommand) -> SimpleCommand {
        let command = unwrap_command(cmd);
        if let CommandType::SimpleCommand(command) = command {
            command
        } else {
            panic!("expected simple command")
        }
    }

    fn parse_simple_command_with_alias_table(text: &str, alias_table: AliasTable) -> SimpleCommand {
        unwrap_simple_command(
            parse_correct_complete_command(text, alias_table).expect("no commands"),
        )
    }

    fn parse_simple_command(text: &str) -> SimpleCommand {
        parse_simple_command_with_alias_table(text, AliasTable::default())
    }

    fn parse_single_redirection(text: &str) -> Redirection {
        let cmd = parse_simple_command(text);
        assert!(cmd.words.is_empty());
        assert!(cmd.assignments.is_empty());
        assert_eq!(cmd.redirections.len(), 1);
        cmd.redirections.into_iter().next().unwrap()
    }

    fn parse_compound_command(text: &str) -> (CompoundCommand, Vec<Redirection>) {
        let command = unwrap_command(
            parse_correct_complete_command(text, AliasTable::default()).expect("no commands"),
        );
        if let CommandType::CompoundCommand {
            command,
            redirections,
        } = command
        {
            (command, redirections)
        } else {
            panic!("expected compound command, got {:?}", command)
        }
    }

    fn parse_command(text: &str) -> CommandType {
        unwrap_command(
            parse_correct_complete_command(text, AliasTable::default()).expect("no commands"),
        )
    }

    fn parse_pipeline(text: &str) -> Pipeline {
        unwrap_pipeline(
            parse_correct_complete_command(text, AliasTable::default()).expect("no commands"),
        )
    }

    fn parse_conjunction(text: &str) -> Conjunction {
        unwrap_conjunction(
            parse_correct_complete_command(text, AliasTable::default()).expect("no commands"),
        )
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
                    file: unquoted_literal_pair("test_file")
                }
            }
        );
        assert_eq!(
            parse_single_redirection(">| test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectOutputClobber,
                    file: unquoted_literal_pair("test_file")
                }
            }
        );
        assert_eq!(
            parse_single_redirection(">> test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectOuputAppend,
                    file: unquoted_literal_pair("test_file")
                }
            }
        );
        assert_eq!(
            parse_single_redirection(">& test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::DuplicateOutput,
                    file: unquoted_literal_pair("test_file")
                }
            }
        );
        assert_eq!(
            parse_single_redirection("< test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectInput,
                    file: unquoted_literal_pair("test_file")
                }
            }
        );
        assert_eq!(
            parse_single_redirection("<& test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::DuplicateInput,
                    file: unquoted_literal_pair("test_file")
                }
            }
        );
        assert_eq!(
            parse_single_redirection("<> test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::OpenRW,
                    file: unquoted_literal_pair("test_file")
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
                    file: unquoted_literal_pair("test_file")
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
                file: unquoted_literal_pair("file.txt")
            }
        );
    }

    #[test]
    fn parse_command_with_redirections() {
        let command = parse_simple_command("< input command > output");
        assert_eq!(command.words, vec![unquoted_literal_pair("command")]);
        assert_eq!(
            command.redirections,
            vec![
                Redirection {
                    file_descriptor: None,
                    kind: RedirectionKind::IORedirection {
                        kind: IORedirectionKind::RedirectInput,
                        file: unquoted_literal_pair("input")
                    }
                },
                Redirection {
                    file_descriptor: None,
                    kind: RedirectionKind::IORedirection {
                        kind: IORedirectionKind::RedirectOutput,
                        file: unquoted_literal_pair("output")
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
                unquoted_literal_pair("echo"),
                unquoted_literal_pair("this"),
                unquoted_literal_pair("is"),
                unquoted_literal_pair("a"),
                unquoted_literal_pair("test")
            ]
        )
    }

    #[test]
    fn parse_simple_command_with_arguments_and_redirections() {
        let command = parse_simple_command("cat test_file.txt >> ../other_file.txt");
        assert_eq!(
            command.words,
            vec![
                unquoted_literal_pair("cat"),
                unquoted_literal_pair("test_file.txt")
            ]
        );
        assert_eq!(
            command.redirections,
            vec![Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectOuputAppend,
                    file: unquoted_literal_pair("../other_file.txt")
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
                value: unquoted_literal_pair("warn")
            }]
        );
        assert_eq!(
            command.words,
            vec![
                unquoted_literal_pair("cargo"),
                unquoted_literal_pair("build")
            ]
        );
        assert_eq!(
            command.redirections,
            vec![Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectOutput,
                    file: unquoted_literal_pair("build_result.txt")
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
                    delimiter: "end".to_string(),
                    contents: WordPair {
                        word: quoted_literal("this\nis\n\ta\ntest\n"),
                        as_string: "this\nis\n\ta\ntest\n".to_string()
                    }
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
                    delimiter: "end".to_string(),
                    contents: WordPair {
                        word: quoted_literal("this\nis\na\ntest\n"),
                        as_string: "this\nis\na\ntest\n".to_string()
                    }
                }
            }
        )
    }

    #[test]
    fn parse_simple_pipeline() {
        let pipeline = parse_pipeline("echo hello | wc -l");
        assert_eq!(pipeline.commands.len(), 2);
        assert_eq!(
            pipeline.commands[0].type_,
            CommandType::SimpleCommand(SimpleCommand {
                words: vec![
                    unquoted_literal_pair("echo"),
                    unquoted_literal_pair("hello")
                ],
                ..Default::default()
            })
        );
        assert_eq!(
            pipeline.commands[1].type_,
            CommandType::SimpleCommand(SimpleCommand {
                words: vec![unquoted_literal_pair("wc"), unquoted_literal_pair("-l")],
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
                pipeline_from_word_pair(unquoted_literal_pair("a")),
                LogicalOp::And
            )
        );

        assert_eq!(
            conjunction.elements[1],
            (
                pipeline_from_word_pair(unquoted_literal_pair("b")),
                LogicalOp::None
            )
        );
    }

    #[test]
    fn parse_commands_separated_by_semicolon() {
        let command =
            parse_correct_complete_command("a; b", AliasTable::default()).expect("no commands");
        assert_eq!(command.commands.len(), 2);
        assert_eq!(
            command.commands[0],
            conjunction_from_word_pair(unquoted_literal_pair("a"), false)
        );

        assert_eq!(
            command.commands[1],
            conjunction_from_word_pair(unquoted_literal_pair("b"), false)
        )
    }

    #[test]
    fn parse_async_conjunction_with_single_command() {
        let conjunction = parse_conjunction("cmd &");
        assert_eq!(conjunction.elements.len(), 1);
        assert!(conjunction.is_async);
        assert_eq!(
            conjunction.elements[0].0,
            pipeline_from_word_pair(unquoted_literal_pair("cmd"))
        )
    }

    #[test]
    fn parse_async_conjunction_with_multiple_commands() {
        let conjunction = parse_conjunction("x && y || z &");
        assert_eq!(conjunction.elements.len(), 3);
        assert!(conjunction.is_async);
        assert_eq!(
            conjunction.elements[0],
            (
                pipeline_from_word_pair(unquoted_literal_pair("x")),
                LogicalOp::And
            )
        );
        assert_eq!(
            conjunction.elements[1],
            (
                pipeline_from_word_pair(unquoted_literal_pair("y")),
                LogicalOp::Or
            )
        );
        assert_eq!(
            conjunction.elements[2],
            (
                pipeline_from_word_pair(unquoted_literal_pair("z")),
                LogicalOp::None
            )
        );
    }

    #[test]
    fn ampersand_terminates_current_and_or_list() {
        let complete_command =
            parse_correct_complete_command("a & b", AliasTable::default()).expect("no commands");
        assert_eq!(complete_command.commands.len(), 2);
        assert!(complete_command.commands[0].is_async);
    }

    #[test]
    fn parse_brace_group() {
        assert_eq!(
            parse_compound_command("{ word }").0,
            CompoundCommand::BraceGroup(complete_command_from_word_pair(
                unquoted_literal_pair("word"),
                false
            ))
        );
        assert_eq!(
            parse_compound_command("{\ncmd1; cmd2;\ncmd3\n\n\ncmd4 &\n}").0,
            CompoundCommand::BraceGroup(CompleteCommand {
                commands: vec![
                    conjunction_from_word_pair(unquoted_literal_pair("cmd1"), false),
                    conjunction_from_word_pair(unquoted_literal_pair("cmd2"), false),
                    conjunction_from_word_pair(unquoted_literal_pair("cmd3"), false),
                    conjunction_from_word_pair(unquoted_literal_pair("cmd4"), true)
                ]
                .try_into()
                .unwrap()
            })
        )
    }

    #[test]
    fn parse_subshell() {
        assert_eq!(
            parse_compound_command("( word )").0,
            CompoundCommand::Subshell(complete_command_from_word_pair(
                unquoted_literal_pair("word"),
                false
            ))
        );
        assert_eq!(
            parse_compound_command("(\ncmd1; cmd2 & cmd3;\n\n\ncmd4 &\n)").0,
            CompoundCommand::Subshell(CompleteCommand {
                commands: vec![
                    conjunction_from_word_pair(unquoted_literal_pair("cmd1"), false),
                    conjunction_from_word_pair(unquoted_literal_pair("cmd2"), true),
                    conjunction_from_word_pair(unquoted_literal_pair("cmd3"), false),
                    conjunction_from_word_pair(unquoted_literal_pair("cmd4"), true)
                ]
                .try_into()
                .unwrap()
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
                    unquoted_literal_pair("1"),
                    unquoted_literal_pair("2"),
                    unquoted_literal_pair("3")
                ],
                body: complete_command_from_word_pair(unquoted_literal_pair("cmd"), false)
            }
        );
    }

    #[test]
    fn parse_for_clause_with_no_words() {
        assert_eq!(
            parse_compound_command("for i; do\ncmd\ndone").0,
            CompoundCommand::ForClause {
                iter_var: Rc::from("i"),
                words: vec![WordPair::new(special_parameter(SpecialParameter::At), "$@")],
                body: complete_command_from_word_pair(unquoted_literal_pair("cmd"), false)
            }
        );
    }

    #[test]
    fn parse_empty_case_clause() {
        assert_eq!(
            parse_compound_command("case word in esac").0,
            CompoundCommand::CaseClause {
                arg: unquoted_literal_pair("word"),
                cases: Vec::new()
            }
        );
        assert_eq!(
            parse_compound_command("case word \n\nin\n esac").0,
            CompoundCommand::CaseClause {
                arg: unquoted_literal_pair("word"),
                cases: Vec::new()
            }
        )
    }

    #[test]
    fn parse_case_clause_one_case() {
        assert_eq!(
            parse_compound_command("case word in (pattern) cmd;; esac").0,
            CompoundCommand::CaseClause {
                arg: unquoted_literal_pair("word"),
                cases: vec![CaseItem {
                    pattern: NonEmpty::new(unquoted_literal_pair("pattern")),
                    body: complete_command_from_word_pair(unquoted_literal_pair("cmd"), false)
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
                arg: unquoted_literal_pair("word"),
                cases: vec![
                    CaseItem {
                        pattern: NonEmpty::new(unquoted_literal_pair("pattern1")),
                        body: complete_command_from_word_pair(unquoted_literal_pair("cmd1"), false)
                    },
                    CaseItem {
                        pattern: NonEmpty::new(unquoted_literal_pair("pattern2")),
                        body: complete_command_from_word_pair(unquoted_literal_pair("cmd2"), false)
                    },
                    CaseItem {
                        pattern: NonEmpty::new(unquoted_literal_pair("pattern3")),
                        body: complete_command_from_word_pair(unquoted_literal_pair("cmd3"), false)
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
                if_chain: NonEmpty::new(If {
                    condition: complete_command_from_word_pair(
                        unquoted_literal_pair("condition"),
                        false
                    ),
                    body: complete_command_from_word_pair(unquoted_literal_pair("cmd"), false)
                }),
                else_body: None
            }
        );
    }

    #[test]
    fn parse_if_clause_one_else() {
        assert_eq!(
            parse_compound_command("if condition; then cmd; else cmd2; fi").0,
            CompoundCommand::IfClause {
                if_chain: NonEmpty::new(If {
                    condition: complete_command_from_word_pair(
                        unquoted_literal_pair("condition"),
                        false
                    ),
                    body: complete_command_from_word_pair(unquoted_literal_pair("cmd"), false)
                },),
                else_body: Some(complete_command_from_word_pair(
                    unquoted_literal_pair("cmd2"),
                    false
                ))
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
                        condition: complete_command_from_word_pair(
                                unquoted_literal_pair("condition1"),
                                false
                            ),
                        body: complete_command_from_word_pair(unquoted_literal_pair("cmd1"), false)
                    },
                    If {
                        condition: complete_command_from_word_pair(
                                unquoted_literal_pair("condition2"),
                                false
                            ),
                        body: complete_command_from_word_pair(unquoted_literal_pair("cmd2"), false)
                    },
                    If {
                        condition: complete_command_from_word_pair(
                                unquoted_literal_pair("condition3"),
                                false
                            ),
                        body: complete_command_from_word_pair(unquoted_literal_pair("cmd3"), false)
                    },
                ].try_into().unwrap(),
                else_body: Some(complete_command_from_word_pair(unquoted_literal_pair("cmd4"), false))
            }
        );
    }

    #[test]
    fn parse_while_clause() {
        assert_eq!(
            parse_compound_command("while condition; do cmd; done").0,
            CompoundCommand::WhileClause {
                condition: complete_command_from_word_pair(
                    unquoted_literal_pair("condition"),
                    false
                ),
                body: complete_command_from_word_pair(unquoted_literal_pair("cmd"), false)
            }
        );
    }

    #[test]
    fn parse_until_clause() {
        assert_eq!(
            parse_compound_command("until condition; do cmd; done").0,
            CompoundCommand::UntilClause {
                condition: complete_command_from_word_pair(
                    unquoted_literal_pair("condition"),
                    false
                ),
                body: complete_command_from_word_pair(unquoted_literal_pair("cmd"), false)
            }
        );
    }

    #[test]
    fn parse_compound_command_with_redirections() {
        let (command, redirections) = parse_compound_command("if a; then b; fi > file.txt");
        assert_eq!(
            command,
            CompoundCommand::IfClause {
                if_chain: NonEmpty::new(If {
                    condition: complete_command_from_word_pair(unquoted_literal_pair("a"), false),
                    body: complete_command_from_word_pair(unquoted_literal_pair("b"), false)
                }),
                else_body: None
            }
        );
        assert_eq!(
            redirections,
            vec![Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectOutput,
                    file: unquoted_literal_pair("file.txt")
                }
            }]
        );
    }

    #[test]
    fn parse_function_definition() {
        assert_eq!(
            parse_command("function_name() { cmd; }"),
            CommandType::FunctionDefinition(FunctionDefinition {
                name: Rc::from("function_name"),
                body: Rc::from(CompoundCommand::BraceGroup(
                    complete_command_from_word_pair(unquoted_literal_pair("cmd"), false)
                ))
            })
        );

        assert_eq!(
            parse_command("function_name() ( cmd1 )"),
            CommandType::FunctionDefinition(FunctionDefinition {
                name: Rc::from("function_name"),
                body: Rc::from(CompoundCommand::Subshell(CompleteCommand {
                    commands: NonEmpty::new(conjunction_from_word_pair(
                        unquoted_literal_pair("cmd1"),
                        false
                    ))
                }))
            })
        );
    }

    #[test]
    fn parse_pipeline_negate_status() {
        assert_eq!(
            parse_pipeline("! cmd"),
            Pipeline {
                commands: NonEmpty::new(
                    CommandType::SimpleCommand(SimpleCommand {
                        words: vec![unquoted_literal_pair("cmd")],
                        ..Default::default()
                    })
                    .into()
                ),
                negate_status: true
            }
        );
    }

    #[test]
    fn parse_reserved_word_as_simple_word_when_used_as_command_argument() {
        assert_eq!(
            parse_simple_command("echo if"),
            SimpleCommand {
                words: vec![unquoted_literal_pair("echo"), unquoted_literal_pair("if")],
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
                    unquoted_literal_pair("1"),
                    unquoted_literal_pair("2"),
                    unquoted_literal_pair("3")
                ],
                body: complete_command_from_word_pair(unquoted_literal_pair("cmd"), false)
            }
        );
    }

    #[test]
    fn case_arg_can_be_a_reserved_word() {
        assert_eq!(
            parse_compound_command("case case in esac").0,
            CompoundCommand::CaseClause {
                arg: unquoted_literal_pair("case"),
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
                words: vec![unquoted_literal_pair("in")],
                body: complete_command_from_word_pair(unquoted_literal_pair("cmd"), false)
            }
        );
    }

    #[test]
    fn invalid_parameter_is_error() {
        assert!(parse_complete_command("$.", AliasTable::default())
            .is_err_and(|err| !err.could_be_resolved_with_more_input));
        assert!(parse_complete_command("$\n0", AliasTable::default())
            .is_err_and(|err| !err.could_be_resolved_with_more_input));
        assert!(parse_complete_command("$", AliasTable::default())
            .is_err_and(|err| !err.could_be_resolved_with_more_input))
    }

    #[test]
    fn invalid_format_specifier_in_parameter_expansion_is_error() {
        assert!(
            parse_complete_command("${word:.other_word}", AliasTable::default())
                .is_err_and(|err| !err.could_be_resolved_with_more_input)
        );
        assert!(parse_complete_command("${word:\n-}", AliasTable::default())
            .is_err_and(|err| !err.could_be_resolved_with_more_input));
        assert!(parse_complete_command("${word:", AliasTable::default())
            .is_err_and(|err| err.could_be_resolved_with_more_input));

        assert!(
            parse_complete_command("${word;word}", AliasTable::default())
                .is_err_and(|err| !err.could_be_resolved_with_more_input)
        );
        assert!(parse_complete_command("${word", AliasTable::default())
            .is_err_and(|err| err.could_be_resolved_with_more_input));
    }

    #[test]
    fn unclosed_quotes_are_error() {
        assert!(
            parse_complete_command("\"unclosed string", AliasTable::default())
                .is_err_and(|err| err.could_be_resolved_with_more_input)
        );
        assert!(
            parse_complete_command("'unclosed string", AliasTable::default())
                .is_err_and(|err| err.could_be_resolved_with_more_input)
        );
    }

    #[test]
    fn out_of_range_file_descriptor_is_error() {
        assert!(
            parse_complete_command("2000> file.txt", AliasTable::default())
                .is_err_and(|err| !err.could_be_resolved_with_more_input)
        );
    }

    #[test]
    fn pipe_with_no_following_command_is_error() {
        assert!(parse_complete_command("command |", AliasTable::default())
            .is_err_and(|err| err.could_be_resolved_with_more_input));
    }

    #[test]
    fn logical_op_with_no_following_command_is_error() {
        assert!(parse_complete_command("command &&", AliasTable::default())
            .is_err_and(|err| err.could_be_resolved_with_more_input));
        assert!(parse_complete_command("command ||", AliasTable::default())
            .is_err_and(|err| err.could_be_resolved_with_more_input));
    }

    #[test]
    fn alias_substitution_word_to_word() {
        assert_eq!(
            parse_simple_command_with_alias_table(
                "cmd_x",
                AliasTable::from([("cmd_x".to_string(), "cmd_y".to_string())])
            ),
            SimpleCommand {
                words: vec![unquoted_literal_pair("cmd_y")],
                ..Default::default()
            }
        );
        assert_eq!(
            parse_simple_command_with_alias_table(
                "cmd_x arg1 arg2",
                AliasTable::from([("cmd_x".to_string(), "cmd_y".to_string())])
            ),
            SimpleCommand {
                words: vec![
                    unquoted_literal_pair("cmd_y"),
                    unquoted_literal_pair("arg1"),
                    unquoted_literal_pair("arg2")
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn alias_substitution_word_to_words() {
        assert_eq!(
            parse_simple_command_with_alias_table(
                "cmd_x",
                AliasTable::from([("cmd_x".to_string(), "cmd_y arg1 arg2".to_string())])
            ),
            SimpleCommand {
                words: vec![
                    unquoted_literal_pair("cmd_y"),
                    unquoted_literal_pair("arg1"),
                    unquoted_literal_pair("arg2")
                ],
                ..Default::default()
            }
        );
        assert_eq!(
            parse_simple_command_with_alias_table(
                "cmd_x arg3 arg4",
                AliasTable::from([("cmd_x".to_string(), "cmd_y arg1 arg2".to_string())])
            ),
            SimpleCommand {
                words: vec![
                    unquoted_literal_pair("cmd_y"),
                    unquoted_literal_pair("arg1"),
                    unquoted_literal_pair("arg2"),
                    unquoted_literal_pair("arg3"),
                    unquoted_literal_pair("arg4"),
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn recursive_alias_substitution_word_to_word() {
        assert_eq!(
            parse_simple_command_with_alias_table(
                "cmd_x",
                AliasTable::from([
                    ("cmd_x".to_string(), "cmd_y".to_string()),
                    ("cmd_y".to_string(), "cmd_z".to_string())
                ])
            ),
            SimpleCommand {
                words: vec![unquoted_literal_pair("cmd_z")],
                ..Default::default()
            }
        );
        assert_eq!(
            parse_simple_command_with_alias_table(
                "cmd_x",
                AliasTable::from([
                    ("cmd_x".to_string(), "cmd_y".to_string()),
                    ("cmd_y".to_string(), "cmd_z".to_string()),
                    ("cmd_z".to_string(), "cmd_w".to_string())
                ])
            ),
            SimpleCommand {
                words: vec![unquoted_literal_pair("cmd_w")],
                ..Default::default()
            }
        );
        assert_eq!(
            parse_simple_command_with_alias_table(
                "cmd_x arg1 arg2",
                AliasTable::from([
                    ("cmd_x".to_string(), "cmd_y".to_string()),
                    ("cmd_y".to_string(), "cmd_z".to_string()),
                    ("cmd_z".to_string(), "cmd_w".to_string())
                ])
            ),
            SimpleCommand {
                words: vec![
                    unquoted_literal_pair("cmd_w"),
                    unquoted_literal_pair("arg1"),
                    unquoted_literal_pair("arg2")
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn recursive_alias_substitution_word_to_words() {
        assert_eq!(
            parse_simple_command_with_alias_table(
                "cmd_x",
                AliasTable::from([
                    ("cmd_x".to_string(), "cmd_y arg1 arg2".to_string()),
                    ("cmd_y".to_string(), "cmd_z arg3".to_string())
                ])
            ),
            SimpleCommand {
                words: vec![
                    unquoted_literal_pair("cmd_z"),
                    unquoted_literal_pair("arg3"),
                    unquoted_literal_pair("arg1"),
                    unquoted_literal_pair("arg2")
                ],
                ..Default::default()
            }
        );
        assert_eq!(
            parse_simple_command_with_alias_table(
                "cmd_x arg4 arg5",
                AliasTable::from([
                    ("cmd_x".to_string(), "cmd_y arg2 arg3".to_string()),
                    ("cmd_y".to_string(), "cmd_z arg1".to_string())
                ])
            ),
            SimpleCommand {
                words: vec![
                    unquoted_literal_pair("cmd_z"),
                    unquoted_literal_pair("arg1"),
                    unquoted_literal_pair("arg2"),
                    unquoted_literal_pair("arg3"),
                    unquoted_literal_pair("arg4"),
                    unquoted_literal_pair("arg5"),
                ],
                ..Default::default()
            }
        )
    }

    #[test]
    fn recursive_alias_substitution_word_to_word_with_cycle() {
        assert_eq!(
            parse_simple_command_with_alias_table(
                "cmd_x",
                AliasTable::from([
                    ("cmd_x".to_string(), "cmd_y".to_string()),
                    ("cmd_y".to_string(), "cmd_x".to_string())
                ])
            ),
            SimpleCommand {
                words: vec![unquoted_literal_pair("cmd_x")],
                ..Default::default()
            }
        );
        assert_eq!(
            parse_simple_command_with_alias_table(
                "cmd_x arg1 arg2",
                AliasTable::from([
                    ("cmd_x".to_string(), "cmd_y".to_string()),
                    ("cmd_y".to_string(), "cmd_x".to_string())
                ])
            ),
            SimpleCommand {
                words: vec![
                    unquoted_literal_pair("cmd_x"),
                    unquoted_literal_pair("arg1"),
                    unquoted_literal_pair("arg2")
                ],
                ..Default::default()
            }
        );
        assert_eq!(
            parse_simple_command_with_alias_table(
                "cmd1",
                AliasTable::from([
                    ("cmd1".to_string(), "cmd2".to_string()),
                    ("cmd2".to_string(), "cmd3".to_string()),
                    ("cmd3".to_string(), "cmd2".to_string())
                ])
            ),
            SimpleCommand {
                words: vec![unquoted_literal_pair("cmd2")],
                ..Default::default()
            }
        );
    }

    #[test]
    fn recursive_alias_substitution_word_to_words_with_cycle() {
        assert_eq!(
            parse_simple_command_with_alias_table(
                "cmd_x",
                AliasTable::from([
                    ("cmd_x".to_string(), "cmd_y arg2".to_string()),
                    ("cmd_y".to_string(), "cmd_x arg1".to_string())
                ])
            ),
            SimpleCommand {
                words: vec![
                    unquoted_literal_pair("cmd_x"),
                    unquoted_literal_pair("arg1"),
                    unquoted_literal_pair("arg2")
                ],
                ..Default::default()
            }
        );
        assert_eq!(
            parse_simple_command_with_alias_table(
                "cmd_x arg3 arg4",
                AliasTable::from([
                    ("cmd_x".to_string(), "cmd_y arg2".to_string()),
                    ("cmd_y".to_string(), "cmd_x arg1".to_string())
                ])
            ),
            SimpleCommand {
                words: vec![
                    unquoted_literal_pair("cmd_x"),
                    unquoted_literal_pair("arg1"),
                    unquoted_literal_pair("arg2"),
                    unquoted_literal_pair("arg3"),
                    unquoted_literal_pair("arg4"),
                ],
                ..Default::default()
            }
        );
        assert_eq!(
            parse_simple_command_with_alias_table(
                "cmd1 arg4 arg5",
                AliasTable::from([
                    ("cmd1".to_string(), "cmd2 arg3".to_string()),
                    ("cmd2".to_string(), "cmd3 arg2".to_string()),
                    ("cmd3".to_string(), "cmd2 arg1".to_string())
                ])
            ),
            SimpleCommand {
                words: vec![
                    unquoted_literal_pair("cmd2"),
                    unquoted_literal_pair("arg1"),
                    unquoted_literal_pair("arg2"),
                    unquoted_literal_pair("arg3"),
                    unquoted_literal_pair("arg4"),
                    unquoted_literal_pair("arg5"),
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn continued_alias_substitution() {
        assert_eq!(
            parse_simple_command_with_alias_table(
                "cmd_x arg1",
                AliasTable::from([
                    ("cmd_x".to_string(), "cmd_y ".to_string()),
                    ("arg1".to_string(), "arg2".to_string())
                ])
            ),
            SimpleCommand {
                words: vec![
                    unquoted_literal_pair("cmd_y"),
                    unquoted_literal_pair("arg2")
                ],
                ..Default::default()
            }
        );
        assert_eq!(
            parse_simple_command_with_alias_table(
                "cmd_x arg1 arg2",
                AliasTable::from([
                    ("cmd_x".to_string(), "cmd_y ".to_string()),
                    ("arg1".to_string(), "arg_x ".to_string()),
                    ("arg2".to_string(), "arg_y ".to_string())
                ])
            ),
            SimpleCommand {
                words: vec![
                    unquoted_literal_pair("cmd_y"),
                    unquoted_literal_pair("arg_x"),
                    unquoted_literal_pair("arg_y")
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn alias_substitution_word_to_conjunction() {
        let command = parse_correct_complete_command(
            "cmd_x",
            AliasTable::from([("cmd_x".to_string(), "cmd_y && cmd_z".to_string())]),
        )
        .expect("no commands");
        assert_eq!(
            command,
            CompleteCommand {
                commands: NonEmpty::new(Conjunction {
                    elements: vec![
                        (
                            pipeline_from_word_pair(unquoted_literal_pair("cmd_y")),
                            LogicalOp::And
                        ),
                        (
                            pipeline_from_word_pair(unquoted_literal_pair("cmd_z")),
                            LogicalOp::None
                        )
                    ]
                    .try_into()
                    .unwrap(),
                    is_async: false
                })
            }
        );
    }

    #[test]
    fn recursive_alias_substitution_word_to_conjunction() {
        let command = parse_correct_complete_command(
            "a",
            AliasTable::from([
                ("a".to_string(), "b && c".to_string()),
                ("b".to_string(), "cmd_x".to_string()),
                ("c".to_string(), "cmd_y".to_string()),
            ]),
        )
        .expect("no commands");
        assert_eq!(
            command,
            CompleteCommand {
                commands: NonEmpty::new(Conjunction {
                    elements: vec![
                        (
                            pipeline_from_word_pair(unquoted_literal_pair("cmd_x")),
                            LogicalOp::And
                        ),
                        (
                            pipeline_from_word_pair(unquoted_literal_pair("cmd_y")),
                            LogicalOp::None
                        )
                    ]
                    .try_into()
                    .unwrap(),
                    is_async: false
                })
            }
        );
    }
}
