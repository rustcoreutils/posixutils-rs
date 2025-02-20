//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::parse::command::{
    Assignment, CaseItem, Command, CompleteCommand, CompoundCommand, Conjunction,
    FunctionDefinition, IORedirectionKind, If, LogicalOp, Name, Pipeline, Program, Redirection,
    RedirectionKind, SimpleCommand,
};
use crate::parse::lexer::command_lexer::{CommandLexer, CommandToken};
use crate::parse::word::Word;
use crate::parse::word_parser::parse_word;
use crate::parse::{ParseResult, ParserError};
use std::borrow::Cow;
use std::rc::Rc;

fn is_valid_name(name: &str) -> bool {
    name.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_')
        && name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn is_valid_alias_name(name: &str) -> bool {
    name.chars().all(|c| {
        c.is_ascii_alphanumeric() || c == '_' || c == '!' || c == '%' || c == ',' || c == '@'
    })
}

fn try_into_assignment(
    word: &str,
    line_no: u32,
    reached_eof: bool,
) -> ParseResult<Result<Assignment, Word>> {
    if !word.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_') {
        return parse_word(word, line_no, reached_eof).map(Err);
    }
    if let Some((pos, c)) = word
        .char_indices()
        .find(|(_, c)| !c.is_ascii_alphanumeric() && *c != '_')
    {
        if c == '=' {
            let (name, value) = word.split_at(pos);
            let name = Rc::from(name);
            parse_word(&value[1..], line_no, reached_eof)
                .map(|value| Ok(Assignment { name, value }))
        } else {
            parse_word(word, line_no, reached_eof).map(Err)
        }
    } else {
        parse_word(word, line_no, reached_eof).map(Err)
    }
}

struct CommandParser<'src> {
    lexer: CommandLexer<'src>,
    lookahead: CommandToken<'src>,
    lookahead_lineno: u32,
}

impl<'src> CommandParser<'src> {
    fn reached_eof(&self) -> bool {
        self.lookahead == CommandToken::EOF
    }

    /// advances the current shell token and returns the previous shell lookahead
    fn advance(&mut self) -> ParseResult<CommandToken<'src>> {
        let (mut next_token, next_token_line_no) = self.lexer.next_token()?;
        self.lookahead_lineno = next_token_line_no;
        std::mem::swap(&mut self.lookahead, &mut next_token);
        Ok(next_token)
    }

    fn advance_if_word(&mut self) -> ParseResult<Option<Cow<'src, str>>> {
        if self.lookahead.as_word_str().is_some() {
            self.advance()
                .map(|word| Some(word.into_word_cow().unwrap()))
        } else {
            Ok(None)
        }
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
            Some(word) if is_valid_name(&word) => self
                .advance()
                .map(|word| word.into_word_cow().unwrap().into_owned().into()),
            _ => Err(ParserError::new(
                line_no,
                format!("expected name, got {}", self.lookahead),
                self.reached_eof(),
            )),
        }
    }

    fn parse_word(&mut self) -> ParseResult<Word> {
        let line_no = self.lookahead_lineno;
        let token = self.advance()?;
        if let Some(word) = token.as_word_str() {
            parse_word(&word, line_no, self.reached_eof())
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
        if let CommandToken::HereDocument(_) = &self.lookahead {
            let contents = self.advance()?.unwrap_here_document_contents();
            return Ok(Some(RedirectionKind::HereDocument {
                contents: contents.into(),
            }));
        }
        let kind = match self.lookahead {
            CommandToken::Greater => IORedirectionKind::RedirectOutput,
            CommandToken::Clobber => IORedirectionKind::RedirectOutputClobber,
            CommandToken::DGreat => IORedirectionKind::RedirectOuputAppend,
            CommandToken::GreatAnd => IORedirectionKind::DuplicateOutput,
            CommandToken::Less => IORedirectionKind::RedirectInput,
            CommandToken::LessAnd => IORedirectionKind::DuplicateInput,
            CommandToken::LessGreat => IORedirectionKind::OpenRW,
            _ => return Ok(None),
        };
        let line_no = self.lookahead_lineno;
        // advance the operator
        self.advance()?;
        match self.advance()? {
            CommandToken::Word(word) => {
                let file = parse_word(&word, line_no, self.reached_eof())?;
                Ok(Some(RedirectionKind::IORedirection { kind, file }))
            }
            other => Err(ParserError::new(
                self.lookahead_lineno,
                format!("expected word, got {}", other),
                other == CommandToken::EOF,
            )),
        }
    }

    fn parse_redirection_opt(&mut self) -> ParseResult<Option<Redirection>> {
        if let CommandToken::IoNumber(n) = self.lookahead {
            if !(0..9).contains(&n) {
                // TODO: bash supports (0..1023), should look into this
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
                    self.lookahead == CommandToken::EOF,
                ))
            }
        } else {
            Ok(self.parse_redirection_kind()?.map(|kind| Redirection {
                file_descriptor: None,
                kind,
            }))
        }
    }

    fn parse_simple_command(
        &mut self,
        start: Option<&str>,
        end: CommandToken,
    ) -> ParseResult<Option<SimpleCommand>> {
        // simple_command = (io_redirect | assignment_word)* word? (io_redirect | word)*

        let mut command = SimpleCommand::default();

        if let Some(word) = start {
            match try_into_assignment(&word, self.lookahead_lineno, self.reached_eof())? {
                Ok(assignment) => command.assignments.push(assignment),
                Err(word) => command.words.push(word),
            }
        }

        while command.words.is_empty() {
            if self.lookahead == end {
                return Ok(command.none_if_empty());
            }
            match self.advance_if_word()? {
                Some(word) => {
                    match try_into_assignment(&word, self.lookahead_lineno, self.reached_eof())? {
                        Ok(assignment) => command.assignments.push(assignment),
                        Err(word) => command.words.push(word),
                    }
                }
                None => match self.parse_redirection_opt()? {
                    Some(redirection) => command.redirections.push(redirection),
                    None => break,
                },
            }
        }

        loop {
            if self.lookahead == end {
                // at this point command cannot be empty, it has at least one word
                return Ok(command.none_if_empty());
            }
            let line_no = self.lookahead_lineno;
            match self.advance_if_word()? {
                Some(word) => command
                    .words
                    .push(parse_word(&word, line_no, self.reached_eof())?),
                None => match self.parse_redirection_opt()? {
                    Some(redirection) => command.redirections.push(redirection),
                    None => break,
                },
            }
        }

        Ok(command.none_if_empty())
    }

    fn parse_compound_list(&mut self, end: CommandToken) -> ParseResult<CompleteCommand> {
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

        let mut last_conjunction = self.parse_and_or(end.clone())?;
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
            last_conjunction = self.parse_and_or(end.clone())?;
        }
        if commands.is_empty() {
            return Err(ParserError::new(
                list_start,
                "expected command",
                self.lookahead == CommandToken::EOF,
            ));
        }
        Ok(CompleteCommand { commands })
    }

    fn parse_brace_group(&mut self) -> ParseResult<CompoundCommand> {
        // consume '{'
        self.advance()?;
        let inner = self.parse_compound_list(CommandToken::RBrace)?;
        self.match_token(CommandToken::RBrace)?;
        Ok(CompoundCommand::BraceGroup(inner))
    }

    fn parse_subshell(&mut self) -> ParseResult<CompoundCommand> {
        // consume '('
        self.advance()?;
        let inner = self.parse_compound_list(CommandToken::RParen)?;
        self.match_token(CommandToken::RParen)?;
        Ok(CompoundCommand::Subshell(inner))
    }

    fn parse_do_group(&mut self) -> ParseResult<CompleteCommand> {
        self.match_token(CommandToken::Do)?;
        let inner = self.parse_compound_list(CommandToken::Done)?;
        self.match_token(CommandToken::Done)?;
        Ok(inner)
    }

    fn parse_for_clause(&mut self) -> ParseResult<CompoundCommand> {
        // consume 'for'
        self.advance()?;
        let iter_var = self.match_name()?;
        self.skip_linebreak()?;
        let mut words = Vec::new();
        if self.lookahead == CommandToken::In {
            self.advance()?;
            while self.lookahead.as_word_str().is_some() {
                let word = self.advance()?.into_word_cow().unwrap();
                words.push(parse_word(
                    &word,
                    self.lookahead_lineno,
                    self.lookahead == CommandToken::EOF,
                )?);
            }
        }
        match self.lookahead {
            CommandToken::SemiColon | CommandToken::Newline => {
                self.advance()?;
                self.skip_linebreak()?;
            }
            _ => {}
        }
        let body = self.parse_do_group()?;
        Ok(CompoundCommand::ForClause {
            iter_var,
            words,
            body,
        })
    }

    fn parse_case_item(&mut self) -> ParseResult<CaseItem> {
        self.match_shell_token_opt(CommandToken::LParen);
        let mut pattern = Vec::new();
        while self.lookahead != CommandToken::RParen {
            pattern.push(self.parse_word()?);
            if self.lookahead != CommandToken::Pipe {
                break;
            }
            self.advance()?;
        }
        self.match_token(CommandToken::RParen)?;

        let body = self.parse_compound_list(CommandToken::EOF)?;

        if self.lookahead == CommandToken::DSemi {
            self.advance()?;
            self.skip_linebreak()?;
        } else if self.lookahead != CommandToken::Esac {
            return Err(ParserError::new(
                self.lookahead_lineno,
                format!("expected ';;', found {}", self.lookahead),
                self.lookahead == CommandToken::EOF,
            ));
        }

        Ok(CaseItem { body, pattern })
    }

    fn parse_case_clause(&mut self) -> ParseResult<CompoundCommand> {
        // consume 'case'
        self.advance()?;
        let arg = self.parse_word()?;
        self.skip_linebreak()?;
        self.match_token(CommandToken::In)?;
        self.skip_linebreak()?;
        let mut cases = Vec::new();
        loop {
            match self.lookahead {
                CommandToken::Esac => break,
                _ => {
                    cases.push(self.parse_case_item()?);
                }
            }
        }
        self.match_token(CommandToken::Esac)?;
        Ok(CompoundCommand::CaseClause { arg, cases })
    }

    fn parse_if_clause(&mut self) -> ParseResult<CompoundCommand> {
        // consume 'if'
        self.advance()?;
        let mut if_chain = Vec::new();
        // there is a terminator after the condition, we don't need to terminate on CommandToken::Then
        let condition = self.parse_compound_list(CommandToken::EOF)?;
        self.match_token(CommandToken::Then)?;
        let then_part = self.parse_compound_list(CommandToken::EOF)?;
        if_chain.push(If {
            condition,
            body: then_part,
        });
        while self.lookahead == CommandToken::Elif {
            // consume 'elif'
            self.advance()?;
            let condition = self.parse_compound_list(CommandToken::EOF)?;
            self.match_token(CommandToken::Then)?;
            let then_part = self.parse_compound_list(CommandToken::EOF)?;
            if_chain.push(If {
                condition,
                body: then_part,
            });
        }
        if self.lookahead == CommandToken::Else {
            self.advance()?;
            let else_part = self.parse_compound_list(CommandToken::EOF)?;
            if_chain.push(If {
                condition: CompleteCommand {
                    commands: Vec::new(),
                },
                body: else_part,
            });
        }
        self.match_token(CommandToken::Fi)?;
        Ok(CompoundCommand::IfClause { if_chain })
    }

    fn parse_while_clause(&mut self) -> ParseResult<CompoundCommand> {
        // consume 'while'
        self.advance()?;
        let condition = self.parse_compound_list(CommandToken::EOF)?;
        let body = self.parse_do_group()?;
        Ok(CompoundCommand::WhileClause { condition, body })
    }

    fn parse_until_clause(&mut self) -> ParseResult<CompoundCommand> {
        // consume 'until'
        self.advance()?;
        let condition = self.parse_compound_list(CommandToken::EOF)?;
        let body = self.parse_do_group()?;
        Ok(CompoundCommand::UntilClause { condition, body })
    }

    fn parse_compound_command(&mut self) -> ParseResult<Option<CompoundCommand>> {
        match &self.lookahead {
            CommandToken::LParen => self.parse_subshell().map(Some),
            CommandToken::LBrace => self.parse_brace_group().map(Some),
            CommandToken::For => self.parse_for_clause().map(Some),
            CommandToken::Case => self.parse_case_clause().map(Some),
            CommandToken::If => self.parse_if_clause().map(Some),
            CommandToken::While => self.parse_while_clause().map(Some),
            CommandToken::Until => self.parse_until_clause().map(Some),
            _ => Ok(None),
        }
    }

    fn parse_function_definition(&mut self, name: Name) -> ParseResult<FunctionDefinition> {
        // consume '('
        self.advance()?;
        self.match_token(CommandToken::RParen)?;
        if let Some(body) = self.parse_compound_command()? {
            Ok(FunctionDefinition { name, body })
        } else {
            todo!("error: expected compound command")
        }
    }

    fn parse_command(&mut self, end: CommandToken) -> ParseResult<Option<Command>> {
        // command =
        // 			| compound_command redirect_list?
        // 			| simple_command
        // 			| function_definition
        if let Some(compound_command) = self.parse_compound_command()? {
            let mut redirections = Vec::new();
            while let Some(redirection) = self.parse_redirection_opt()? {
                redirections.push(redirection);
            }
            Ok(Some(Command::CompoundCommand {
                command: compound_command,
                redirections,
            }))
        } else {
            match &self.lookahead {
                CommandToken::Word(word) if is_valid_name(word) => {
                    let word = self.advance()?.into_word_cow().unwrap();
                    if self.lookahead == CommandToken::LParen {
                        self.parse_function_definition(word.into_owned().into())
                            .map(Command::FunctionDefinition)
                            .map(Some)
                    } else {
                        Ok(self
                            .parse_simple_command(Some(&word), end)?
                            .map(Command::SimpleCommand))
                    }
                }
                _ => Ok(self
                    .parse_simple_command(None, end)?
                    .map(Command::SimpleCommand)),
            }
        }
    }

    fn parse_pipeline(&mut self, end: CommandToken) -> ParseResult<Option<Pipeline>> {
        // pipeline = "!" command ("|" linebreak command)*
        let negate_status = self.match_alternatives(&[CommandToken::Bang])?.is_some();
        let mut commands = Vec::new();
        if let Some(command) = self.parse_command(end.clone())? {
            commands.push(command);
        } else {
            return Ok(None);
        }
        while self.lookahead == CommandToken::Pipe {
            let pipe_location = self.lookahead_lineno;
            self.advance()?;
            self.skip_linebreak()?;
            if let Some(command) = self.parse_command(end.clone())? {
                commands.push(command);
            } else {
                return Err(ParserError::new(
                    pipe_location,
                    "right hand side of pipe operator should be a command",
                    self.lookahead == CommandToken::EOF,
                ));
            }
        }
        Ok(Some(Pipeline {
            commands,
            negate_status,
        }))
    }

    fn parse_and_or(&mut self, end: CommandToken) -> ParseResult<Option<Conjunction>> {
        // and_or = pipeline (("&&" | "||") linebreak pipeline)*
        let mut last = if let Some(pipeline) = self.parse_pipeline(end.clone())? {
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
            let next = if let Some(next) = self.parse_pipeline(end.clone())? {
                next
            } else {
                return Err(ParserError::new(
                    operator_location,
                    "right hand side of pipe operator should be a command",
                    self.lookahead == CommandToken::EOF,
                ));
            };
            let previous = last;
            last = next;
            elements.push((previous, op));
        }
        elements.push((last, LogicalOp::None));
        Ok(Some(Conjunction {
            elements,
            is_async: false,
        }))
    }

    fn parse_complete_command(&mut self) -> ParseResult<CompleteCommand> {
        // complete_command = and_or (separator_op and_or)* separator_op?
        let mut commands = Vec::new();
        while self.lookahead != CommandToken::Newline || self.lookahead != CommandToken::EOF {
            let command_start = self.lookahead_lineno;
            let mut and_or = if let Some(and_or) = self.parse_and_or(CommandToken::EOF)? {
                and_or
            } else {
                return Err(ParserError::new(command_start, "expected command", false));
            };
            if self.lookahead == CommandToken::And {
                and_or.is_async = true;
            }
            commands.push(and_or);
            if self.lookahead == CommandToken::And || self.lookahead == CommandToken::SemiColon {
                self.advance()?;
            } else {
                break;
            }
        }
        Ok(CompleteCommand { commands })
    }

    fn parse_program(mut self) -> ParseResult<Program> {
        // program = linebreak (complete_command (complete_command  newline_list)*)? linebreak
        self.skip_linebreak()?;
        let mut commands = Vec::new();
        while self.lookahead != CommandToken::EOF {
            if commands.len() > 0 {
                self.match_token(CommandToken::Newline)?;
                self.skip_linebreak()?;
            }
            if self.lookahead == CommandToken::EOF {
                break;
            }
            commands.push(self.parse_complete_command()?);
        }

        Ok(Program { commands })
    }

    fn new(source: &'src str) -> ParseResult<Self> {
        let mut lexer = CommandLexer::new(source);
        let (lookahead, lookahead_lineno) = lexer.next_token()?;
        Ok(Self {
            lexer,
            lookahead,
            lookahead_lineno,
        })
    }
}

pub fn parse(text: &str) -> ParseResult<Program> {
    CommandParser::new(text)?.parse_program()
}

#[cfg(test)]
mod tests {
    use crate::parse::word::test_utils::{quoted_literal, unquoted_literal};

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

    fn parse_simple_command(text: &str) -> SimpleCommand {
        unwrap_simple_command(parse(text).expect("parsing failed"))
    }

    fn parse_single_redirection(text: &str) -> Redirection {
        let cmd = parse_simple_command(text);
        assert!(cmd.words.is_empty());
        assert!(cmd.assignments.is_empty());
        assert_eq!(cmd.redirections.len(), 1);
        cmd.redirections.into_iter().next().unwrap()
    }

    fn parse_compound_command(text: &str) -> (CompoundCommand, Vec<Redirection>) {
        let command = unwrap_command(parse(text).expect("parsing failed"));
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
        unwrap_command(parse(text).expect("parsing failed"))
    }

    fn parse_pipeline(text: &str) -> Pipeline {
        unwrap_pipeline(parse(text).expect("parsing failed"))
    }

    fn parse_conjunction(text: &str) -> Conjunction {
        unwrap_conjunction(parse(text).expect("parsing failed"))
    }

    fn parse_complete_command(text: &str) -> CompleteCommand {
        unwrap_complete_command(parse(text).expect("parsing failed"))
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
    fn parse_brace_group() {
        assert_eq!(
            parse_compound_command("{ word }").0,
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
            parse_pipeline("! cmd"),
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

    #[test]
    fn invalid_parameter_is_error() {
        assert!(parse("$.").is_err_and(|err| !err.could_be_resolved_with_more_input));
        assert!(parse("$\n0").is_err_and(|err| !err.could_be_resolved_with_more_input));
        assert!(parse("$").is_err_and(|err| err.could_be_resolved_with_more_input))
    }

    #[test]
    fn invalid_format_specifier_in_parameter_expansion_is_error() {
        assert!(
            parse("${word:.other_word}").is_err_and(|err| !err.could_be_resolved_with_more_input)
        );
        assert!(parse("${word:\n-}").is_err_and(|err| !err.could_be_resolved_with_more_input));
        assert!(parse("${word:").is_err_and(|err| err.could_be_resolved_with_more_input));

        assert!(parse("${word;word}").is_err_and(|err| !err.could_be_resolved_with_more_input));
        assert!(parse("${word").is_err_and(|err| err.could_be_resolved_with_more_input));
    }

    #[test]
    fn unclosed_quotes_are_error() {
        assert!(parse("\"unclosed string").is_err_and(|err| err.could_be_resolved_with_more_input));
        assert!(parse("'unclosed string").is_err_and(|err| err.could_be_resolved_with_more_input));
    }

    #[test]
    fn out_of_range_file_descriptor_is_error() {
        assert!(parse("2000> file.txt").is_err_and(|err| !err.could_be_resolved_with_more_input));
    }

    #[test]
    fn pipe_with_no_following_command_is_error() {
        assert!(parse("command |").is_err_and(|err| err.could_be_resolved_with_more_input));
    }

    #[test]
    fn logical_op_with_no_following_command_is_error() {
        assert!(parse("command &&").is_err_and(|err| err.could_be_resolved_with_more_input));
        assert!(parse("command ||").is_err_and(|err| err.could_be_resolved_with_more_input));
    }
}
