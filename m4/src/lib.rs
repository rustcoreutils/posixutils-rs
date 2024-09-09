use error::{Error, ErrorKind, Result};
use input::{Input, InputRead};
use lexer::MacroName;
use macros::MacroDefinition;
use state::State;
use std::{cell::RefCell, ffi::OsStr, io::Write, path::PathBuf, rc::Rc};

pub mod error;
mod input;
mod lexer;
mod macros;
mod main_loop;
mod output;
mod precedence;
mod state;
pub const EOF: u8 = b'\0';

#[derive(Debug, Clone)]
pub struct ArgumentDefine {
    pub name: MacroName,
    pub definition: Vec<u8>,
}

impl ArgumentDefine {
    pub fn parse(value: &OsStr) -> std::result::Result<Self, clap::Error> {
        let value_bytes = value.as_encoded_bytes();
        let mut split = value_bytes.splitn(2, |b| *b == b'=');
        let name =
            MacroName::try_from_slice(split.next().unwrap_or_default()).map_err(|_error| {
                let mut e = clap::Error::new(clap::error::ErrorKind::ValueValidation);
                e.insert(
                    clap::error::ContextKind::InvalidValue,
                    clap::error::ContextValue::String(
                        String::from_utf8_lossy(value_bytes).to_string(),
                    ),
                );
                e
            })?;

        let value = match split.next() {
            // TODO(performance): perhaps we should use
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

/// Define a symbol name to have some value [`DefineDirective::Define`] or NULL
/// [`DefineDirective::Undefine`].
#[derive(Debug, Clone)]
pub enum DefineDirective {
    /// `name[=val]`
    ///
    /// Define `name` to `val` or to `null` if `=val` is omitted.
    Define(ArgumentDefine),
    // Undefine `name`.
    Undefine(MacroName),
}

#[derive(Debug, Clone, Default)]
pub struct Args {
    /// Enable line synchronization output for the c99 preprocessor phase (that is, #line
    /// directives).
    pub line_synchronization: bool,
    /// See [`DefineDirective`].
    pub define_directives: Vec<DefineDirective>,
    /// Whether to read input from a file.
    pub files: Vec<PathBuf>,
}

impl Args {
    pub fn parse() -> Self {
        let matches = clap::command!()
            .arg(
                clap::Arg::new("line_synchronization")
                    .short('s')
                    .help("Output line synchronization directives, suitable for cpp")
                    .action(clap::ArgAction::SetTrue),
            )
            .arg(
                clap::Arg::new("define")
                    .short('D')
                    .value_name("name[=value]")
                    .help("Define the symbol name to have some value (or NULL)")
                    .num_args(1)
                    .action(clap::ArgAction::Append),
            )
            .arg(
                clap::Arg::new("undefine")
                    .short('U')
                    .value_name("name")
                    .num_args(1)
                    .action(clap::ArgAction::Append),
            )
            .arg(clap::Arg::new("file").action(clap::ArgAction::Append))
            .get_matches();

        let line_synchronization = matches.get_flag("line_synchronization");

        let files = matches
            .get_raw("file")
            .unwrap_or_default()
            .map(PathBuf::from)
            .collect();

        // Order of defines and undefines is important, so we need to do this, otherwise we'd just
        // use the clap derive macro instead.
        let mut define_directives = Vec::new();
        let defines = matches.get_raw("define").unwrap_or_default();
        for (value, index) in defines.zip(matches.indices_of("define").unwrap_or_default()) {
            let value = ArgumentDefine::parse(value).expect("Invalid -D argument definition");
            define_directives.push((index, DefineDirective::Define(value)));
        }
        let undefines = matches.get_raw("undefine").unwrap_or_default();
        for (value, index) in undefines.zip(matches.indices_of("undefine").unwrap_or_default()) {
            let value = MacroName::parse_cmd(value).expect("Invalid -U argument undefine");
            define_directives.push((index, DefineDirective::Undefine(value)));
        }
        define_directives.sort_by_key(|d| d.0);
        let define_directives = define_directives.into_iter().map(|d| d.1).collect();

        Self {
            line_synchronization,
            define_directives,
            files,
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
    let stdout = Rc::new(RefCell::new(stdout));
    let mut state = State::try_new(stdout.clone(), Vec::new(), args.line_synchronization)?;
    if args.files.is_empty() {
        state.input.input_push(
            Input::new(InputRead::Stdin(std::io::stdin())),
            &mut *stdout.borrow_mut(),
        )?;
    } else {
        for file_path in args.files {
            state.input.input_push(
                Input::new(InputRead::File {
                    file: std::fs::File::open(&file_path)?,
                    path: file_path,
                }),
                &mut *stdout.borrow_mut(),
            )?;
        }
    };

    for directive in args.define_directives {
        match directive {
            DefineDirective::Define(define) => {
                let definition = Rc::new(MacroDefinition::new_user_defined(
                    define.name.clone(),
                    define.definition,
                ));

                state
                    .macro_definitions
                    .insert(define.name, vec![definition]);
            }
            DefineDirective::Undefine(name) => {
                state.macro_definitions.remove(&name);
            }
        }
    }

    main_loop::main_loop(state, &mut stderr)?;

    Ok(())
}
