use std::{io, num::ParseIntError};

pub type PatchResult<T> = Result<T, PatchError>;

#[derive(Debug)]
pub enum PatchError {
    IOError(io::Error),
    ParseIntError(ParseIntError),
    Error(&'static str),
    RegexError(regex::Error),
}

impl From<io::Error> for PatchError {
    fn from(value: io::Error) -> Self {
        Self::IOError(value)
    }
}

impl From<ParseIntError> for PatchError {
    fn from(value: ParseIntError) -> Self {
        Self::ParseIntError(value)
    }
}

impl From<regex::Error> for PatchError {
    fn from(value: regex::Error) -> Self {
        Self::RegexError(value)
    }
}
