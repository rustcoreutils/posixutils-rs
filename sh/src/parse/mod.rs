use std::collections::HashMap;

pub mod command;
pub mod command_parser;
mod lexer;
pub mod word;
mod word_parser;

#[derive(Debug, Clone)]
pub struct ParserError {
    pub lineno: u32,
    pub message: String,
    pub could_be_resolved_with_more_input: bool,
}

impl ParserError {
    fn new<S: Into<String>>(
        lineno: u32,
        message: S,
        could_be_resolved_with_more_input: bool,
    ) -> Self {
        Self {
            lineno,
            message: message.into(),
            could_be_resolved_with_more_input,
        }
    }
}

pub type ParseResult<T> = Result<T, ParserError>;

pub type AliasTable = HashMap<String, String>;
