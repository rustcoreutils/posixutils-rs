use std::{
    ffi::{OsStr, OsString},
    os::unix::ffi::OsStrExt,
    path::PathBuf,
};

use clap::{
    builder::{TypedValueParser, ValueParserFactory},
    Parser,
};

use crate::{evaluate::State, lexer::ParseConfig};

mod error;
mod evaluate;
mod lexer;

// TODO: potentially we can use a reference here to avoid allocation
#[derive(Debug, Clone)]
struct ArgumentName(OsString);

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
struct ArgumentValue(OsString);

#[derive(Debug, Clone)]
struct ArgumentDefine {
    name: ArgumentName,
    value: Option<ArgumentValue>,
}

#[derive(Clone)]
struct ArgumentDefineParser;

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
struct Args {
    /// Enable line synchronization output for the c99 preprocessor phase (that is, #line
    /// directives).
    #[arg(short = 's', long)]
    line_synchronization: bool,
    /// `name[=val]`
    ///
    /// Define `name` to `val` or to `null` if `=val` is omitted.
    #[arg(short = 'D', long)]
    defines: Vec<ArgumentDefine>,
    // Undefine `name`.
    #[arg(short = 'U', long)]
    undefines: Vec<ArgumentName>,
    /// Whether to read input from a file.
    file: Option<PathBuf>,
}

fn main() {
    env_logger::init();
    let args = Args::parse();

    let stdout = std::io::stdout();
    if let Some(file_path) = args.file {
        lexer::process_streaming(
            State::default(),
            ParseConfig::default(),
            evaluate::evaluate,
            std::fs::File::open(file_path).unwrap(),
            stdout,
        )
        .unwrap();
    } else {
        lexer::process_streaming(
            State::default(),
            ParseConfig::default(),
            evaluate::evaluate,
            std::io::stdin(),
            stdout,
        )
        .unwrap();
    }
}
