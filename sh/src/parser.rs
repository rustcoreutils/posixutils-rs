//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::{path::is_separator, rc::Rc};

use crate::{
    lexer::{is_blank, is_operator, Lexer, ShellToken, SourceLocation, WordToken},
    program::{
        ArithmeticExpr, Assignment, Command, CompleteCommand, CompoundCommand, Conjunction,
        IORedirectionKind, LogicalOp, Parameter, ParameterExpansion, Pipeline, Program,
        Redirection, RedirectionKind, SimpleCommand, Word, WordPart,
    },
};

fn try_word_to_assignment(word: Word) -> Result<Assignment, Word> {
    if let Some(WordPart::Literal(name)) = word.parts.first() {
        if let Some(eq_pos) = name.find('=') {
            let (name, rest) = name.split_at(eq_pos);
            let name = Rc::<str>::from(name);
            let word_start = rest[1..].to_string();
            let mut word_parts = vec![WordPart::Literal(Rc::from(word_start))];
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

fn try_into_name(word: Word) -> Result<Rc<str>, Word> {
    if word.parts.len() == 1 {
        if let WordPart::Literal(name) = word.parts.first().unwrap() {
            Ok(name.clone())
        } else {
            Err(word)
        }
    } else {
        Err(word)
    }
}

struct Parser<'src> {
    lexer: Lexer<'src>,
    shell_lookahead: (ShellToken, SourceLocation),
    word_lookahead: (WordToken, SourceLocation),
}

impl<'src> Parser<'src> {
    fn advance_shell(&mut self) {
        self.shell_lookahead = self.lexer.next_shell_token();
        if self.shell_lookahead.0 == ShellToken::WordStart {
            self.advance_word();
        }
    }

    fn advance_word(&mut self) {
        self.word_lookahead = self.lexer.next_word_token();
    }

    fn shell_lookahead(&mut self) -> ShellToken {
        if self.shell_lookahead.1.start < self.word_lookahead.1.start {
            self.lexer.rollback_last_token();
            self.advance_shell();
            self.shell_lookahead.0
        } else {
            self.shell_lookahead.0
        }
    }

    fn word_lookahead(&mut self) -> WordToken {
        if self.word_lookahead.1.start < self.shell_lookahead.1.start {
            self.lexer.rollback_last_token();
            self.advance_word();
            self.word_lookahead.0
        } else {
            self.word_lookahead.0
        }
    }

    fn match_str(&mut self, s: &str) {
        todo!();
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
        match self.word_lookahead() {
            WordToken::Char('@') => todo!(),
            WordToken::Char('*') => todo!(),
            WordToken::Char('#') => todo!(),
            WordToken::Char('?') => todo!(),
            WordToken::Char('-') => todo!(),
            WordToken::Char('$') => todo!(),
            WordToken::Char('!') => todo!(),
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
                Parameter::Name(Rc::from(name))
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
                    return ParameterExpansion::RemoveLargestSuffix(parameter, word);
                } else {
                    let word = self.parse_word_until(WordToken::Char('}'));
                    return ParameterExpansion::RemoveSmallestSuffix(parameter, word);
                }
            }

            if self.word_lookahead() == WordToken::Char('#') {
                self.advance_word();
                if self.word_lookahead() == WordToken::Char('#') {
                    self.advance_word();
                    let word = self.parse_word_until(WordToken::Char('}'));
                    return ParameterExpansion::RemoveLargestPrefix(parameter, word);
                } else {
                    let word = self.parse_word_until(WordToken::Char('}'));

                    return ParameterExpansion::RemoveSmallestPrefix(parameter, word);
                }
            }
            // TODO: restructure this for better errors
            if self.word_lookahead() == WordToken::Char(':') {
                self.advance_word();
                let operation = self.word_lookahead();
                self.advance_word();
                let word = self.parse_word_until(WordToken::Char('}'));
                match operation {
                    WordToken::Char('-') => {
                        ParameterExpansion::NullUnsetUseDefault(parameter, word)
                    }
                    WordToken::Char('=') => {
                        ParameterExpansion::NullUnsetAssignDefault(parameter, word)
                    }
                    WordToken::Char('?') => ParameterExpansion::NullUnsetError(parameter, word),
                    WordToken::Char('+') => ParameterExpansion::SetUseAlternative(parameter, word),
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
                    WordToken::Char('-') => ParameterExpansion::UnsetUseDefault(parameter, word),
                    WordToken::Char('=') => ParameterExpansion::UnsetAssignDefault(parameter, word),
                    WordToken::Char('?') => ParameterExpansion::UnsetError(parameter, word),
                    WordToken::Char('+') => {
                        ParameterExpansion::SetNullUseAlternative(parameter, word)
                    }
                    _ => todo!("error"),
                }
            }
        } else {
            ParameterExpansion::Simple(self.parse_parameter(true))
        }
    }

    fn parse_word_until(&mut self, end: WordToken) -> Option<Word> {
        let mut current_literal = String::new();
        let mut word_parts = Vec::new();
        fn push_literal(literal: &mut String, parts: &mut Vec<WordPart>) {
            let mut temp = String::new();
            std::mem::swap(&mut temp, literal);
            if !temp.is_empty() {
                parts.push(WordPart::Literal(Rc::from(temp)));
            }
        }
        fn push_literal_and_insert(
            literal: &mut String,
            parts: &mut Vec<WordPart>,
            part: WordPart,
        ) {
            push_literal(literal, parts);
            parts.push(part);
        }

        let mut inside_double_quotes = false;

        loop {
            if !inside_double_quotes && self.word_lookahead() == end {
                break;
            }
            match self.word_lookahead() {
                WordToken::DoubleQuote => {
                    inside_double_quotes = !inside_double_quotes;
                    current_literal.push('"');
                    self.advance_word();
                }
                WordToken::SingleQuote => {
                    current_literal.push('\'');
                    if !inside_double_quotes {
                        loop {
                            if let Some((c, _)) = self.lexer.next_char() {
                                current_literal.push(c);
                                if c == '\'' {
                                    break;
                                }
                            } else {
                                todo!("error: expected ', got end of file")
                            }
                        }
                    }
                    self.advance_word();
                }
                WordToken::Dollar => {
                    push_literal_and_insert(
                        &mut current_literal,
                        &mut word_parts,
                        WordPart::ParameterExpansion(self.parse_parameter_expansion()),
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

        push_literal(&mut current_literal, &mut word_parts);

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
        if self.shell_lookahead() == ShellToken::WordStart {
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

    fn parse_simple_command(&mut self, word_stop: WordToken) -> SimpleCommand {
        // simple_command = (io_redirect | assignment_word)* word? (io_redirect | word)*

        let mut command = SimpleCommand::default();

        loop {
            match self.shell_lookahead() {
                ShellToken::WordStart => {
                    if let Some(word) = self.parse_word_until(word_stop) {
                        match try_word_to_assignment(word) {
                            Ok(assignment) => command.assignments.push(assignment),
                            Err(cmd) => {
                                command.command = Some(cmd);
                                break;
                            }
                        }
                    }
                    if word_stop != WordToken::EOF && self.word_lookahead() == word_stop {
                        return command;
                    }
                }
                _ => {
                    if let Some(redirection) = self.parse_redirection_opt() {
                        command.redirections.push(redirection);
                    } else {
                        return command;
                    }
                }
            }
            self.advance_shell();
        }

        loop {
            match self.shell_lookahead() {
                ShellToken::WordStart => {
                    if let Some(word) = self.parse_word_until(word_stop) {
                        command.arguments.push(word);
                    }
                    if word_stop != WordToken::EOF && self.word_lookahead() == word_stop {
                        return command;
                    }
                }
                _ => {
                    if let Some(redirection) = self.parse_redirection_opt() {
                        command.redirections.push(redirection);
                    } else {
                        return command;
                    }
                }
            }
            self.advance_shell();
        }
    }

    fn parse_compound_command(&mut self) -> Option<CompoundCommand> {
        todo!()
    }

    fn parse_command(&mut self, word_stop: WordToken) -> Command {
        // command =
        // 			| compound_command redirect_list?
        // 			| simple_command
        // 			| function_definition
        Command::SimpleCommand(self.parse_simple_command(word_stop))
        // TODO: other commands
    }

    fn parse_pipeline(&mut self, word_stop: WordToken) -> Pipeline {
        // pipeline = "!" command ("|" linebreak command)*
        // TODO: implement the "!"* part
        let mut commands = Vec::new();
        commands.push(self.parse_command(word_stop));
        while self.shell_lookahead() == ShellToken::Pipe {
            self.advance_shell();
            self.skip_linebreak();
            commands.push(self.parse_command(word_stop));
        }
        Pipeline { commands }
    }

    fn parse_and_or(&mut self, word_stop: WordToken) -> Conjunction {
        // and_or = pipeline (("&&" | "||") linebreak pipeline)*
        let mut last = self.parse_pipeline(word_stop);
        let mut elements = Vec::new();
        while let Some(op) = self.matches_shell_alterntives(&[ShellToken::AndIf, ShellToken::OrIf])
        {
            let op = match op {
                ShellToken::AndIf => LogicalOp::And,
                ShellToken::OrIf => LogicalOp::Or,
                _ => unreachable!(),
            };
            self.skip_linebreak();
            let next = self.parse_pipeline(word_stop);
            let previous = last;
            last = next;
            elements.push((previous, op));
        }
        elements.push((last, LogicalOp::None));
        Conjunction {
            elements,
            is_async: false,
        }
    }

    fn parse_complete_command(&mut self, word_stop: WordToken) -> CompleteCommand {
        // complete_command = and_or (separator_op and_or)* separator_op?
        let mut commands = Vec::new();
        while self.shell_lookahead() != ShellToken::Newline {
            let mut and_or = self.parse_and_or(word_stop);
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
            shell_lookahead: (ShellToken::EOF, SourceLocation::default()),
            word_lookahead: (WordToken::EOF, SourceLocation::default()),
        }
    }
}

pub fn parse(text: &str) -> Program {
    Parser::new(text).parse_program()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn literal_word(contents: &str) -> Word {
        Word {
            parts: vec![WordPart::Literal(Rc::from(contents))],
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
        let command = unwrap_simple_command(parse(&format!("fake_cmd {}", word)));
        command.arguments.into_iter().next().unwrap()
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
        assert_eq!(cmd.command, None);
        assert!(cmd.arguments.is_empty());
        assert!(cmd.assignments.is_empty());
        assert_eq!(cmd.redirections.len(), 1);
        cmd.redirections.into_iter().next().unwrap()
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
        assert_eq!(parse_word("hello"), literal_word("hello"));
    }

    #[test]
    fn parse_word_with_single_quotes() {
        assert_eq!(
            parse_word("'single quoted string ${test} `command` $((1 + 1)) $(command2) \nnewline'"),
            literal_word(
                "'single quoted string ${test} `command` $((1 + 1)) $(command2) \nnewline'"
            )
        );
    }

    #[test]
    fn sigle_quotes_inside_dobule_quotes_are_ignored() {
        assert_eq!(parse_word("\"'\""), literal_word("\"'\""));
    }

    #[test]
    fn parse_simple_word_with_double_quotes() {
        assert_eq!(
            parse_word("\"double quoted string \nnewline\""),
            literal_word("\"double quoted string \nnewline\"")
        );
    }

    #[test]
    fn parse_simple_parameter_expansion() {
        assert_eq!(
            parse_parameter_expansion("$test"),
            ParameterExpansion::Simple(Parameter::Name(Rc::from("test")))
        );
        assert_eq!(
            parse_parameter_expansion("$1"),
            ParameterExpansion::Simple(Parameter::Number(1))
        );
        assert_eq!(
            parse_parameter_expansion("${test}"),
            ParameterExpansion::Simple(Parameter::Name(Rc::from("test")))
        );
        assert_eq!(
            parse_parameter_expansion("${12345}"),
            ParameterExpansion::Simple(Parameter::Number(12345))
        );
    }

    #[test]
    fn parse_parameter_expansion_expression() {
        assert_eq!(
            parse_parameter_expansion("${test:-default}"),
            ParameterExpansion::NullUnsetUseDefault(
                Parameter::Name(Rc::from("test")),
                Some(literal_word("default"))
            )
        );
        assert_eq!(
            parse_parameter_expansion("${test-default}"),
            ParameterExpansion::UnsetUseDefault(
                Parameter::Name(Rc::from("test")),
                Some(literal_word("default"))
            )
        );
        assert_eq!(
            parse_parameter_expansion("${test:=default}"),
            ParameterExpansion::NullUnsetAssignDefault(
                Parameter::Name(Rc::from("test")),
                Some(literal_word("default"))
            )
        );
        assert_eq!(
            parse_parameter_expansion("${test=default}"),
            ParameterExpansion::UnsetAssignDefault(
                Parameter::Name(Rc::from("test")),
                Some(literal_word("default"))
            )
        );
        assert_eq!(
            parse_parameter_expansion("${test:?default}"),
            ParameterExpansion::NullUnsetError(
                Parameter::Name(Rc::from("test")),
                Some(literal_word("default"))
            )
        );
        assert_eq!(
            parse_parameter_expansion("${test?default}"),
            ParameterExpansion::UnsetError(
                Parameter::Name(Rc::from("test")),
                Some(literal_word("default"))
            )
        );
        assert_eq!(
            parse_parameter_expansion("${test:+default}"),
            ParameterExpansion::SetUseAlternative(
                Parameter::Name(Rc::from("test")),
                Some(literal_word("default"))
            )
        );
        assert_eq!(
            parse_parameter_expansion("${test+default}"),
            ParameterExpansion::SetNullUseAlternative(
                Parameter::Name(Rc::from("test")),
                Some(literal_word("default"))
            )
        );
    }

    #[test]
    fn test_parse_parameter_expansion_expression_with_no_default() {
        assert_eq!(
            parse_parameter_expansion("${test:-}"),
            ParameterExpansion::NullUnsetUseDefault(Parameter::Name(Rc::from("test")), None)
        );
        assert_eq!(
            parse_parameter_expansion("${test-}"),
            ParameterExpansion::UnsetUseDefault(Parameter::Name(Rc::from("test")), None)
        );
        assert_eq!(
            parse_parameter_expansion("${test:=}"),
            ParameterExpansion::NullUnsetAssignDefault(Parameter::Name(Rc::from("test")), None)
        );
        assert_eq!(
            parse_parameter_expansion("${test=}"),
            ParameterExpansion::UnsetAssignDefault(Parameter::Name(Rc::from("test")), None)
        );
        assert_eq!(
            parse_parameter_expansion("${test:?}"),
            ParameterExpansion::NullUnsetError(Parameter::Name(Rc::from("test")), None)
        );
        assert_eq!(
            parse_parameter_expansion("${test?}"),
            ParameterExpansion::UnsetError(Parameter::Name(Rc::from("test")), None)
        );
        assert_eq!(
            parse_parameter_expansion("${test:+}"),
            ParameterExpansion::SetUseAlternative(Parameter::Name(Rc::from("test")), None)
        );
        assert_eq!(
            parse_parameter_expansion("${test+}"),
            ParameterExpansion::SetNullUseAlternative(Parameter::Name(Rc::from("test")), None)
        );
    }

    #[test]
    fn parse_string_operations_in_parameter_expansion() {
        assert_eq!(
            parse_parameter_expansion("${#test}"),
            ParameterExpansion::StrLen(Parameter::Name(Rc::from("test")))
        );
        assert_eq!(
            parse_parameter_expansion("${test%pattern}"),
            ParameterExpansion::RemoveSmallestSuffix(
                Parameter::Name(Rc::from("test")),
                Some(literal_word("pattern"))
            )
        );
        assert_eq!(
            parse_parameter_expansion("${test%%pattern}"),
            ParameterExpansion::RemoveLargestSuffix(
                Parameter::Name(Rc::from("test")),
                Some(literal_word("pattern"))
            )
        );
        assert_eq!(
            parse_parameter_expansion("${test#pattern}"),
            ParameterExpansion::RemoveSmallestPrefix(
                Parameter::Name(Rc::from("test")),
                Some(literal_word("pattern"))
            )
        );
        assert_eq!(
            parse_parameter_expansion("${test##pattern}"),
            ParameterExpansion::RemoveLargestPrefix(
                Parameter::Name(Rc::from("test")),
                Some(literal_word("pattern"))
            )
        );
    }

    #[test]
    fn parse_simple_command_no_assignments_no_redirections_no_arguments() {
        let command = parse_simple_command("pwd");
        assert_eq!(command.command, Some(literal_word("pwd")));
        assert!(command.arguments.is_empty());
        assert!(command.assignments.is_empty());
        assert!(command.redirections.is_empty());
    }

    #[test]
    fn parse_simple_command_single_assignment() {
        let command = parse_simple_command("a=1");
        assert!(command.command.is_none());
        assert_eq!(command.assignments.len(), 1);
        assert_eq!(command.assignments[0].name, Rc::from("a"));
        assert_eq!(command.assignments[0].value, literal_word("1"));
        assert!(command.redirections.is_empty());
        assert!(command.arguments.is_empty());
    }

    #[test]
    fn parse_simple_command_multiple_assignment() {
        let command =
            parse_simple_command("PATH=/bin:/usr/bin:/usr/local/bin a=1 b=\"this is a test\"");
        assert!(command.command.is_none());
        assert_eq!(command.assignments.len(), 3);
        assert_eq!(command.assignments[0].name, Rc::from("PATH"));
        assert_eq!(
            command.assignments[0].value,
            literal_word("/bin:/usr/bin:/usr/local/bin")
        );
        assert_eq!(command.assignments[1].name, Rc::from("a"));
        assert_eq!(command.assignments[1].value, literal_word("1"));
        assert_eq!(command.assignments[2].name, Rc::from("b"));
        assert_eq!(
            command.assignments[2].value,
            literal_word("\"this is a test\"")
        );
        assert!(command.redirections.is_empty());
        assert!(command.arguments.is_empty());
    }

    #[test]
    fn parse_redirections_without_file_descriptors() {
        assert_eq!(
            parse_single_redirection("> test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectOutput,
                    file: literal_word("test_file")
                }
            }
        );
        assert_eq!(
            parse_single_redirection(">| test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectOutputClobber,
                    file: literal_word("test_file")
                }
            }
        );
        assert_eq!(
            parse_single_redirection(">> test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectOuputAppend,
                    file: literal_word("test_file")
                }
            }
        );
        assert_eq!(
            parse_single_redirection(">& test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::DuplicateOutput,
                    file: literal_word("test_file")
                }
            }
        );
        assert_eq!(
            parse_single_redirection("< test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectInput,
                    file: literal_word("test_file")
                }
            }
        );
        assert_eq!(
            parse_single_redirection("<& test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::DuplicateInput,
                    file: literal_word("test_file")
                }
            }
        );
        assert_eq!(
            parse_single_redirection("<> test_file"),
            Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::OpenRW,
                    file: literal_word("test_file")
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
                    file: literal_word("test_file")
                }
            }
        );
    }

    #[test]
    fn parse_simple_command_single_redirection() {
        let command = parse_simple_command("> file.txt");
        assert!(command.command.is_none());
        assert!(command.arguments.is_empty());
        assert!(command.assignments.is_empty());
        assert_eq!(command.redirections.len(), 1);
        assert_eq!(
            command.redirections[0].kind,
            RedirectionKind::IORedirection {
                kind: IORedirectionKind::RedirectOutput,
                file: literal_word("file.txt")
            }
        );
    }

    #[test]
    fn parse_command_with_redirections() {
        let command = parse_simple_command("< input command > output");
        assert_eq!(command.command, Some(literal_word("command")));
        assert_eq!(
            command.redirections,
            vec![
                Redirection {
                    file_descriptor: None,
                    kind: RedirectionKind::IORedirection {
                        kind: IORedirectionKind::RedirectInput,
                        file: literal_word("input")
                    }
                },
                Redirection {
                    file_descriptor: None,
                    kind: RedirectionKind::IORedirection {
                        kind: IORedirectionKind::RedirectOutput,
                        file: literal_word("output")
                    }
                }
            ]
        );
        assert!(command.arguments.is_empty());
        assert!(command.assignments.is_empty());
    }

    #[test]
    fn parse_simple_command_with_arguments() {
        let command = parse_simple_command("echo this is a test");
        assert!(command.assignments.is_empty());
        assert!(command.redirections.is_empty());
        assert_eq!(command.command, Some(literal_word("echo")));
        assert_eq!(
            command.arguments,
            vec![
                literal_word("this"),
                literal_word("is"),
                literal_word("a"),
                literal_word("test")
            ]
        )
    }

    #[test]
    fn parse_simple_command_with_arguments_and_redirections() {
        let command = parse_simple_command("cat test_file.txt >> ../other_file.txt");
        assert_eq!(command.command, Some(literal_word("cat")));
        assert_eq!(command.arguments, vec![literal_word("test_file.txt")]);
        assert_eq!(
            command.redirections,
            vec![Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectOuputAppend,
                    file: literal_word("../other_file.txt")
                }
            }]
        );
        assert!(command.assignments.is_empty());
    }

    #[test]
    fn parse_simple_command_with_arguments_redirections_and_assignments() {
        let command = parse_simple_command("CARGO_LOG=warn cargo build > build_result.txt");
        assert_eq!(command.command, Some(literal_word("cargo")));
        assert_eq!(
            command.assignments,
            vec![Assignment {
                name: Rc::from("CARGO_LOG"),
                value: literal_word("warn")
            }]
        );
        assert_eq!(command.arguments, vec![literal_word("build")]);
        assert_eq!(
            command.redirections,
            vec![Redirection {
                file_descriptor: None,
                kind: RedirectionKind::IORedirection {
                    kind: IORedirectionKind::RedirectOutput,
                    file: literal_word("build_result.txt")
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
                command: Some(literal_word("echo")),
                arguments: vec![literal_word("hello")],
                ..Default::default()
            })
        );
        assert_eq!(
            pipeline.commands[1],
            Command::SimpleCommand(SimpleCommand {
                command: Some(literal_word("wc")),
                arguments: vec![literal_word("-l")],
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
                        command: Some(literal_word("a")),
                        ..Default::default()
                    })]
                },
                LogicalOp::And
            )
        );
        assert_eq!(
            conjunction.elements[1],
            (
                Pipeline {
                    commands: vec![Command::SimpleCommand(SimpleCommand {
                        command: Some(literal_word("b")),
                        ..Default::default()
                    })]
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
                            command: Some(literal_word("a")),
                            ..Default::default()
                        })]
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
                            command: Some(literal_word("b")),
                            ..Default::default()
                        })]
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
                                    command: Some(literal_word("echo")),
                                    arguments: vec![literal_word("hello")],
                                    ..Default::default()
                                })]
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
                    WordPart::Literal(Rc::from("\"hello ")),
                    WordPart::CommandSubstitution(CompleteCommand {
                        commands: vec![Conjunction {
                            elements: vec![(
                                Pipeline {
                                    commands: vec![Command::SimpleCommand(SimpleCommand {
                                        command: Some(literal_word("echo")),
                                        arguments: vec![literal_word("world")],
                                        ..Default::default()
                                    })]
                                },
                                LogicalOp::None
                            )],
                            is_async: false
                        }]
                    }),
                    WordPart::Literal(Rc::from("\""))
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
                                command: Some(literal_word("echo")),
                                arguments: vec![literal_word("hello")],
                                ..Default::default()
                            })],
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
                                    command: Some(literal_word("echo")),
                                    arguments: vec![inner],
                                    ..Default::default()
                                })]
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
                    WordPart::Literal(Rc::from("\"hello ")),
                    WordPart::ParameterExpansion(ParameterExpansion::Simple(Parameter::Name(
                        Rc::from("test")
                    ))),
                    WordPart::Literal(Rc::from("\""))
                ]
            }
        );
        assert_eq!(
            parse_word("\"hello ${test}\""),
            Word {
                parts: vec![
                    WordPart::Literal(Rc::from("\"hello ")),
                    WordPart::ParameterExpansion(ParameterExpansion::Simple(Parameter::Name(
                        Rc::from("test")
                    ))),
                    WordPart::Literal(Rc::from("\""))
                ]
            }
        );
    }

    #[test]
    fn test_parse_empty_double_string_word() {
        assert_eq!(parse_word("\"\""), literal_word("\"\""));
    }

    #[test]
    fn test_parse_empty_single_string_word() {
        assert_eq!(parse_word("''"), literal_word("''"));
    }
}
