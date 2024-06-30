use error::Error;
use std::{
    ffi::{OsStr, OsString},
    io::Write,
    os::unix::ffi::OsStrExt,
    path::PathBuf,
};

use clap::builder::{TypedValueParser, ValueParserFactory};
use evaluate::State;

pub mod error;
mod eval_macro;
mod evaluate;
mod lexer;
mod precedence;
#[cfg(test)]
mod test_utils;

// TODO: potentially we can use a reference here to avoid allocation
#[derive(Debug, Clone)]
pub struct ArgumentName(OsString);

impl From<OsString> for ArgumentName {
    fn from(value: OsString) -> Self {
        Self(value)
    }
}

impl From<OsString> for ArgumentValue {
    fn from(value: OsString) -> Self {
        Self(value)
    }
}

// TODO: potentially we can use a reference here to avoid allocation
#[derive(Debug, Clone)]
pub struct ArgumentValue(OsString);

#[derive(Debug, Clone)]
pub struct ArgumentDefine {
    pub name: ArgumentName,
    pub value: Option<ArgumentValue>,
}

#[derive(Clone)]
pub struct ArgumentDefineParser;

impl TypedValueParser for ArgumentDefineParser {
    type Value = ArgumentDefine;

    fn parse_ref(
        &self,
        _cmd: &clap::Command,
        _arg: Option<&clap::Arg>,
        value: &std::ffi::OsStr,
    ) -> std::result::Result<Self::Value, clap::Error> {
        let value_bytes = value.as_encoded_bytes();
        // TODO: do we need to support stripping whitespace after or before the `=`
        let mut split = value_bytes.splitn(2, |b| *b == b'=');
        // TODO: use error
        let name = OsStr::from_bytes(split.next().unwrap()).to_owned().into();

        let value = match split.next() {
            // TODO: perhaps we should use
            // https://doc.rust-lang.org/std/ffi/struct.OsStr.html#method.from_encoded_bytes_unchecked
            // instead?
            Some(value) => Some(OsStr::from_bytes(value).to_owned().into()),
            None => None,
        };
        Ok(ArgumentDefine { name, value })
    }
}

impl ValueParserFactory for ArgumentDefine {
    type Parser = ArgumentDefineParser;

    fn value_parser() -> Self::Parser {
        ArgumentDefineParser
    }
}

#[derive(Debug, clap::Parser, Clone)]
#[command(version, about)]
pub struct Args {
    /// Enable line synchronization output for the c99 preprocessor phase (that is, #line
    /// directives).
    #[arg(short = 's', long)]
    pub line_synchronization: bool,
    /// `name[=val]`
    ///
    /// Define `name` to `val` or to `null` if `=val` is omitted.
    #[arg(short = 'D', long)]
    pub define: Vec<ArgumentDefine>,
    // Undefine `name`.
    #[arg(short = 'U', long)]
    pub undefine: Vec<ArgumentName>,
    /// Whether to read input from a file.
    pub files: Vec<PathBuf>,
}

impl Default for Args {
    fn default() -> Self {
        Self {
            line_synchronization: false,
            define: Vec::default(),
            undefine: Vec::default(),
            files: Vec::default(),
        }
    }
}

pub fn run<STDOUT: Write, STDERR: Write>(
    stdout: &mut STDOUT,
    stderr: &mut STDERR,
    args: Args,
) -> crate::error::Result<()> {
    match run_impl(stdout, stderr, args) {
        Ok(_) => Ok(()),
        Err(error @ Error::Exit(_)) => Err(error),
        Err(error) => {
            if let Err(error) = stderr.write_all(error.to_string().as_bytes()) {
                return Err(error.into());
            }
            Err(error)
        }
    }
}

pub fn run_impl<STDOUT: Write, STDERR: Write>(
    stdout: &mut STDOUT,
    stderr: &mut STDERR,
    args: Args,
) -> crate::error::Result<()> {
    let mut state = State::default();
    if args.files.is_empty() {
        log::info!("Processing input from STDIN");
        state = lexer::process_streaming(
            state,
            evaluate::evaluate,
            std::io::stdin(),
            stdout,
            stderr,
            true,
            true,
        )?;
    } else {
        for file_path in args.files {
            log::info!("Processing input from {file_path:?}");
            state = lexer::process_streaming(
                state,
                evaluate::evaluate,
                std::fs::File::open(file_path)?,
                stdout,
                stderr,
                true,
                true,
            )?;
        }
    }

    for buffer in state.divert_buffers {
        let buffer = buffer.0.borrow();
        stdout.write_all(&*buffer)?;
    }
    for wrap in state.m4wrap {
        stdout.write_all(&wrap)?;
    }
    Ok(())
}