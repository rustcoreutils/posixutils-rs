use error::{Error, ErrorKind, Result};
use lexer::{MacroName, MacroParseConfig};
use std::{io::Write, path::PathBuf, rc::Rc};

use clap::builder::{TypedValueParser, ValueParserFactory};
use evaluate::{
    InputRead, MacroDefinition, MacroDefinitionImplementation, State, UserDefinedMacro,
};

pub mod error;
mod eval_macro;
mod evaluate;
mod lexer;
mod precedence;
#[cfg(test)]
mod test_utils;

pub const EOF: u8 = b'\0';

#[derive(Debug, Clone)]
pub struct ArgumentDefine {
    pub name: MacroName,
    pub definition: Vec<u8>,
}

#[derive(Clone)]
pub struct MacroNameParser;

impl TypedValueParser for MacroNameParser {
    type Value = MacroName;

    fn parse_ref(
        &self,
        cmd: &clap::Command,
        _arg: Option<&clap::Arg>,
        value: &std::ffi::OsStr,
    ) -> std::result::Result<Self::Value, clap::Error> {
        let value_bytes = value.as_encoded_bytes();
        MacroName::try_from_slice(value_bytes).map_err(|_error| {
            clap::Error::new(clap::error::ErrorKind::ValueValidation).with_cmd(cmd)
        })
    }
}

impl ValueParserFactory for MacroName {
    type Parser = MacroNameParser;

    fn value_parser() -> Self::Parser {
        MacroNameParser
    }
}

#[derive(Clone)]
pub struct ArgumentDefineParser;

impl TypedValueParser for ArgumentDefineParser {
    type Value = ArgumentDefine;

    fn parse_ref(
        &self,
        cmd: &clap::Command,
        _arg: Option<&clap::Arg>,
        value: &std::ffi::OsStr,
    ) -> std::result::Result<Self::Value, clap::Error> {
        let value_bytes = value.as_encoded_bytes();
        // TODO: do we need to support stripping whitespace after or before the `=`
        let mut split = value_bytes.splitn(2, |b| *b == b'=');
        // TODO: use error
        let name = MacroName::try_from_slice(value_bytes).map_err(|_error| {
            clap::Error::new(clap::error::ErrorKind::ValueValidation).with_cmd(cmd)
        })?;

        let value = match split.next() {
            // TODO: perhaps we should use
            // https://doc.rust-lang.org/std/ffi/struct.OsStr.html#method.from_encoded_bytes_unchecked
            // instead?
            Some(value) => value.to_vec(),
            None => Vec::default(),
        };
        Ok(ArgumentDefine {
            name,
            definition: value,
        })
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
    pub undefine: Vec<MacroName>,
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

pub fn run<STDOUT: Write + 'static, STDERR: Write>(
    stdout: STDOUT,
    mut stderr: STDERR,
    args: Args,
) -> crate::error::Result<()> {
    match run_impl(stdout, &mut stderr, args) {
        Ok(_) => Ok(()),
        Err(error) => match error.kind {
            ErrorKind::Exit(_) => Err(error),
            _ => {
                if let Err(error) = stderr.write_all(format!("{error:#}").as_bytes()) {
                    return Err(error.into());
                }
                Err(error)
            }
        },
    }
}

pub fn run_impl<STDOUT: Write + 'static, STDERR: Write>(
    stdout: STDOUT,
    mut stderr: STDERR,
    args: Args,
) -> crate::error::Result<()> {
    let mut state = State::new(Box::new(stdout), Vec::new());
    if args.files.is_empty() {
        state
            .input
            .input
            .push(evaluate::Input::new(InputRead::Stdin(std::io::stdin())))
    } else {
        for file_path in args.files {
            state
                .input
                .input
                .push(evaluate::Input::new(InputRead::File {
                    file: std::fs::File::open(&file_path)?,
                    path: file_path,
                }));
        }
    };

    // TODO: add test for this.
    for define in args.define {
        // TODO: probably need to move this into evaluate module.
        let definition = Rc::new(MacroDefinition {
            parse_config: MacroParseConfig {
                name: define.name.clone(),
                min_args: 0,
            },
            implementation: MacroDefinitionImplementation::UserDefined(UserDefinedMacro {
                definition: define.definition,
            }),
        });
        state
            .macro_definitions
            .insert(define.name, vec![definition]);
    }

    // TODO: add test for this.
    for name in args.undefine {
        state.macro_definitions.remove(&name);
    }

    evaluate::process_streaming(state, &mut stderr)?;

    Ok(())
}
