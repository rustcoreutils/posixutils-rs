use error::{Error, ErrorKind, Result};
use gettextrs::gettext;
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

/// A single command-line item affecting input processing, retained in
/// command-line order so that `-D`/`-U` options can be interspersed with file
/// operands (POSIX: "options can be interspersed with operands"; the order of
/// `-D` and `-U` is significant).
#[derive(Debug, Clone)]
pub enum InputItem {
    /// `-D name[=val]` — define `name` to `val` (or null if `=val` is omitted).
    Define(ArgumentDefine),
    /// `-U name` — undefine `name`.
    Undefine(MacroName),
    /// A file operand to process (`-` denotes standard input).
    File(PathBuf),
}

#[derive(Debug, Clone, Default)]
pub struct Args {
    /// Enable line synchronization output for the c99 preprocessor phase (that is, #line
    /// directives).
    pub line_synchronization: bool,
    /// `-D`, `-U`, and file operands in command-line order.
    pub items: Vec<InputItem>,
}

impl Args {
    pub fn parse() -> Self {
        let matches = clap::command!()
            .arg(
                clap::Arg::new("line_synchronization")
                    .short('s')
                    .help(gettext(
                        "Output line synchronization directives, suitable for cpp",
                    ))
                    .action(clap::ArgAction::SetTrue),
            )
            .arg(
                clap::Arg::new("define")
                    .short('D')
                    .value_name("name[=value]")
                    .help(gettext(
                        "Define the symbol name to have some value (or NULL)",
                    ))
                    .num_args(1)
                    .action(clap::ArgAction::Append),
            )
            .arg(
                clap::Arg::new("undefine")
                    .short('U')
                    .value_name("name")
                    .help(gettext("Undefine the symbol name"))
                    .num_args(1)
                    .action(clap::ArgAction::Append),
            )
            .arg(clap::Arg::new("file").action(clap::ArgAction::Append))
            .get_matches();

        let line_synchronization = matches.get_flag("line_synchronization");

        // Collect -D, -U, and file operands tagged with their command-line
        // position, then process them in that order. The relative order of -D
        // and -U is significant, and options may be interspersed with operands.
        let mut items: Vec<(usize, InputItem)> = Vec::new();
        let defines = matches.get_raw("define").unwrap_or_default();
        for (value, index) in defines.zip(matches.indices_of("define").unwrap_or_default()) {
            let value = ArgumentDefine::parse(value).expect("Invalid -D argument definition");
            items.push((index, InputItem::Define(value)));
        }
        let undefines = matches.get_raw("undefine").unwrap_or_default();
        for (value, index) in undefines.zip(matches.indices_of("undefine").unwrap_or_default()) {
            let value = MacroName::parse_cmd(value).expect("Invalid -U argument undefine");
            items.push((index, InputItem::Undefine(value)));
        }
        let files = matches.get_raw("file").unwrap_or_default();
        for (value, index) in files.zip(matches.indices_of("file").unwrap_or_default()) {
            items.push((index, InputItem::File(PathBuf::from(value))));
        }
        items.sort_by_key(|(index, _)| *index);
        let items = items.into_iter().map(|(_, item)| item).collect();

        Self {
            line_synchronization,
            items,
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

    let has_file = args
        .items
        .iter()
        .any(|item| matches!(item, InputItem::File(_)));

    if !has_file {
        // No file operands: apply any -D/-U in order, then process standard
        // input as a single stream.
        for item in &args.items {
            apply_define_item(&mut state, item);
        }
        state.input.input_push(
            Input::new(InputRead::Stdin(std::io::stdin())),
            &mut *stdout.borrow_mut(),
        )?;
        state = main_loop::process(state, &mut stderr)?;
    } else {
        // Process -D/-U options and file operands in command-line order so a
        // -D/-U takes effect for the operands that follow it. Each file is
        // processed to completion before the next item; m4wrap text and
        // diversions are flushed once, after all input (see finalize below).
        for item in args.items {
            match item {
                InputItem::Define(_) | InputItem::Undefine(_) => {
                    apply_define_item(&mut state, &item)
                }
                InputItem::File(path) => {
                    // Discard the previous (now exhausted) input before the next.
                    while state.input.input_len() > 0 {
                        state.input.input_pop();
                    }
                    let reader = if path.as_os_str() == "-" {
                        Ok(InputRead::Stdin(std::io::stdin()))
                    } else {
                        std::fs::File::open(&path).map(|file| InputRead::File {
                            file,
                            path: path.clone(),
                        })
                    };
                    match reader {
                        Ok(read) => {
                            state
                                .input
                                .input_push(Input::new(read), &mut *stdout.borrow_mut())?;
                            state = main_loop::process(state, &mut stderr)?;
                        }
                        // An unreadable file operand is a recoverable error (GNU
                        // m4): diagnose, flag failure, and continue.
                        Err(error) => {
                            let msg = gettext("cannot open `{}': {}")
                                .replacen("{}", &path.display().to_string(), 1)
                                .replacen("{}", &error.to_string(), 1);
                            writeln!(stderr, "m4: {msg}")?;
                            state.exit_error = true;
                        }
                    }
                }
            }
        }
    }

    let exit_error = state.exit_error;
    // Flush m4wrap text (rescanned) and any remaining diversions, once, at the
    // true end of input. Skipped only if no input was ever opened.
    if state.input.input_len() > 0 {
        state = main_loop::finalize(state, &mut stderr)?;
    }

    // A recoverable error (a diagnostic emitted via State::emit_error, e.g. a
    // bad eval expression, or an unreadable file operand) leaves processing
    // intact but must still yield a non-zero exit status.
    if exit_error || state.exit_error {
        return Err(Error::new(ErrorKind::Exit(1)));
    }

    Ok(())
}

/// Apply a `-D`/`-U` directive to the macro table. (File items are handled by
/// the caller.)
fn apply_define_item(state: &mut State, item: &InputItem) {
    match item {
        InputItem::Define(define) => {
            let definition = Rc::new(MacroDefinition::new_user_defined(
                define.name.clone(),
                define.definition.clone(),
            ));
            state
                .macro_definitions
                .insert(define.name.clone(), vec![definition]);
        }
        InputItem::Undefine(name) => {
            state.macro_definitions.remove(name);
        }
        InputItem::File(_) => {}
    }
}
