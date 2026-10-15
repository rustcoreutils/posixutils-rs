//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use error::{Error, ErrorKind, Result, WriteFailed};
use gettextrs::gettext;
use input::{Input, InputRead};
use lexer::MacroName;
use macros::MacroDefinition;
use state::State;
use std::{
    cell::RefCell,
    ffi::{OsStr, OsString},
    io::Write,
    path::PathBuf,
    rc::Rc,
};

mod debug;
mod emacs_regex;
pub mod error;
mod format;
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
                crate::lexer::invalid_name_error(
                    &format!("-D <{}>", crate::lexer::DEFINE_VALUE_NAME),
                    value_bytes,
                )
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
    /// `-I`: directories searched, in order, for an input file not found
    /// where it is named.
    pub include_dirs: Vec<PathBuf>,
    /// `-P`: every built-in is named with an `m4_` prefix.
    pub prefix_builtins: bool,
    /// `-E` given once (diagnostics make the exit status a failure) or
    /// twice (the first diagnostic stops m4).
    pub fatal_warnings: u8,
    /// `-L`: the deepest macro calls may nest, 0 for no limit.
    pub nesting_limit: usize,
    /// `-d`: the debug flags, as given.
    pub debug_flags: Option<Vec<u8>>,
    /// `--debugfile`: `Some(None)` for standard error, `Some(Some(name))`
    /// for a file, nowhere if the name is empty.
    pub debugfile: Option<Option<Vec<u8>>>,
    /// `-t`: macros to trace.
    pub trace: Vec<Vec<u8>>,
}

/// What clap records for a `--debugfile` without `=FILE`: standard error.
/// No file operand can contain a NUL, so no name is mistaken for it.
const DEBUGFILE_STDERR: &str = "\0";

impl Args {
    fn command() -> clap::Command {
        clap::command!()
            // GNU getopt takes any unambiguous prefix of a long option, and
            // libtoolize spells `--prefix-builtins` as `--prefix`.
            .infer_long_args(true)
            .arg(
                clap::Arg::new("line_synchronization")
                    .short('s')
                    .long("synclines")
                    .help(gettext(
                        "Output line synchronization directives, suitable for cpp",
                    ))
                    .action(clap::ArgAction::SetTrue),
            )
            .arg(
                clap::Arg::new("define")
                    .short('D')
                    .long("define")
                    .value_name(crate::lexer::DEFINE_VALUE_NAME)
                    .help(gettext(
                        "Define the symbol name to have some value (or NULL)",
                    ))
                    .num_args(1)
                    .allow_hyphen_values(true)
                    .action(clap::ArgAction::Append),
            )
            .arg(
                clap::Arg::new("undefine")
                    .short('U')
                    .long("undefine")
                    .value_name(crate::lexer::UNDEFINE_VALUE_NAME)
                    .help(gettext("Undefine the symbol name"))
                    .num_args(1)
                    .allow_hyphen_values(true)
                    .action(clap::ArgAction::Append),
            )
            .arg(
                clap::Arg::new("include")
                    .short('I')
                    .long("include")
                    .value_name("DIRECTORY")
                    .help(gettext("Append DIRECTORY to the include path"))
                    .num_args(1)
                    .value_parser(clap::value_parser!(PathBuf))
                    .action(clap::ArgAction::Append),
            )
            .arg(
                clap::Arg::new("prefix_builtins")
                    .short('P')
                    .long("prefix-builtins")
                    .help(gettext("Force a `m4_' prefix to all builtins"))
                    .action(clap::ArgAction::SetTrue),
            )
            .arg(
                clap::Arg::new("fatal_warnings")
                    .short('E')
                    .long("fatal-warnings")
                    .help(gettext(
                        "Once: warnings become errors, twice: stop execution at first error",
                    ))
                    .action(clap::ArgAction::Count),
            )
            .arg(
                clap::Arg::new("nesting_limit")
                    .short('L')
                    .long("nesting-limit")
                    .value_name("NUMBER")
                    .help(gettext("Change the nesting limit, 0 for unlimited"))
                    .num_args(1)
                    .value_parser(clap::value_parser!(usize)),
            )
            .arg(
                // GNU getopt gives `-d` an argument only when attached
                // (`-daq`); `parse` spells that `--debug=aq` for clap.
                clap::Arg::new("debug")
                    .short('d')
                    .long("debug")
                    .value_name("FLAGS")
                    .help(gettext(
                        "Set the debug flags (no FLAGS implies `aeq'): a args, c call, \
                         e expansion, f file, i input, l line, p path, q quote, t trace all, \
                         x call id, V all",
                    ))
                    .num_args(0..=1)
                    .require_equals(true)
                    .default_missing_value("")
                    .value_parser(clap::value_parser!(OsString)),
            )
            .arg(
                clap::Arg::new("debugfile")
                    .long("debugfile")
                    .value_name("FILE")
                    .help(gettext(
                        "Redirect debug and trace output to FILE (default stderr, \
                         discard if empty string)",
                    ))
                    .num_args(0..=1)
                    .require_equals(true)
                    .default_missing_value(DEBUGFILE_STDERR)
                    .value_parser(clap::value_parser!(OsString)),
            )
            .arg(
                clap::Arg::new("trace")
                    .short('t')
                    .long("trace")
                    .value_name("NAME")
                    .help(gettext("Trace NAME when it is defined"))
                    .num_args(1)
                    .allow_hyphen_values(true)
                    .value_parser(clap::value_parser!(OsString))
                    .action(clap::ArgAction::Append),
            )
            .arg(
                // autom4te always passes it; the extensions it names are the
                // only mode there is.
                clap::Arg::new("gnu")
                    .short('g')
                    .long("gnu")
                    .help(gettext("Enable the GNU extensions (always enabled)"))
                    .action(clap::ArgAction::SetTrue),
            )
            .arg(clap::Arg::new("file").action(clap::ArgAction::Append))
    }

    pub fn parse() -> Self {
        let argv = plib::optarg::spell_optional_argument(
            std::env::args_os().collect(),
            'd',
            "debug",
            &plib::optarg::OptionArguments::of(Self::command()),
        );
        let matches = plib::optarg::get_matches_from(
            Self::command(),
            plib::optarg::keep_leading_equals_with(argv, Self::command),
        );
        let bytes = |value: &OsString| value.as_encoded_bytes().to_vec();

        let line_synchronization = matches.get_flag("line_synchronization");

        // Collect -D, -U, and file operands tagged with their command-line
        // position, then process them in that order. The relative order of -D
        // and -U is significant, and options may be interspersed with operands.
        let mut items: Vec<(usize, InputItem)> = Vec::new();
        // A name that is not a name token is a usage error, so let clap render
        // and exit the way it does for any other bad option-argument. These
        // used to `.expect()`, which printed the Debug spelling of a
        // clap::Error -- every style field and all -- and aborted with 101.
        let defines = matches.get_raw("define").unwrap_or_default();
        for (value, index) in defines.zip(matches.indices_of("define").unwrap_or_default()) {
            let value = ArgumentDefine::parse(value).unwrap_or_else(|e| e.exit());
            items.push((index, InputItem::Define(value)));
        }
        let undefines = matches.get_raw("undefine").unwrap_or_default();
        for (value, index) in undefines.zip(matches.indices_of("undefine").unwrap_or_default()) {
            let value = MacroName::parse_cmd(value).unwrap_or_else(|e| e.exit());
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
            include_dirs: matches
                .get_many::<PathBuf>("include")
                .unwrap_or_default()
                .cloned()
                .collect(),
            prefix_builtins: matches.get_flag("prefix_builtins"),
            fatal_warnings: matches.get_count("fatal_warnings"),
            nesting_limit: matches
                .get_one::<usize>("nesting_limit")
                .copied()
                .unwrap_or(0),
            debug_flags: matches.get_one::<OsString>("debug").map(bytes),
            debugfile: matches
                .get_one::<OsString>("debugfile")
                .map(|name| (name != DEBUGFILE_STDERR).then(|| bytes(name))),
            trace: matches
                .get_many::<OsString>("trace")
                .unwrap_or_default()
                .map(bytes)
                .collect(),
        }
    }
}

pub fn run<STDOUT: Write + 'static, STDERR: Write>(
    stdout: STDOUT,
    mut stderr: STDERR,
    args: Args,
) -> crate::error::Result<()> {
    match run_impl(MarkWriteErrors(stdout), &mut stderr, args) {
        Ok(_) => Ok(()),
        Err(error) => match error.kind {
            ErrorKind::Exit(_) => Err(error),
            ErrorKind::Write => {
                let reason = std::error::Error::source(&error)
                    .map(plib::diag::error_text)
                    .unwrap_or_default();
                writeln!(stderr, "m4: {}: {reason}", gettext("write error"))?;
                Err(error)
            }
            _ => {
                if let Err(error) = stderr.write_all(format!("{error:#}").as_bytes()) {
                    return Err(error.into());
                }
                Err(error)
            }
        },
    }
}

/// The output stream, with every failure marked as a [`WriteFailed`] so it is
/// reported as a write error.
struct MarkWriteErrors<W>(W);

impl<W: Write> Write for MarkWriteErrors<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf).map_err(WriteFailed::mark)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush().map_err(WriteFailed::mark)
    }
}

pub fn run_impl<STDOUT: Write + 'static, STDERR: Write>(
    stdout: STDOUT,
    mut stderr: STDERR,
    args: Args,
) -> crate::error::Result<()> {
    let stdout = Rc::new(RefCell::new(stdout));
    let mut state = State::try_new(stdout.clone(), Vec::new(), args.line_synchronization)?;
    state.configure(&args, &mut stderr)?;

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
        state
            .input
            .input_push(Input::new(InputRead::Stdin(std::io::stdin())));
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
                    let reader = if path.as_os_str() == "-" {
                        Ok(InputRead::Stdin(std::io::stdin()))
                    } else {
                        state
                            .open_input(&path)
                            .map(|(file, path)| InputRead::File { file, path })
                    };
                    match reader {
                        Ok(read) => {
                            // Discard the previous (now exhausted) input only once
                            // the next one has opened. Popping before the open
                            // would, on a failed open, leave the input stack empty
                            // and cause finalize() to be skipped — dropping any
                            // m4wrap text or diversions queued by earlier files.
                            while state.input.input_len() > 0 {
                                state.input.input_pop();
                            }
                            state.input.input_push(Input::new(read));
                            state = main_loop::process(state, &mut stderr)?;
                        }
                        // An unreadable file operand is a recoverable error (GNU
                        // m4): diagnose, flag failure, and continue.
                        Err(error) => {
                            let msg = gettext("cannot open `{}': {}")
                                .replacen("{}", &path.display().to_string(), 1)
                                .replacen("{}", &plib::diag::io_error_text(&error), 1);
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
    state.debug.flush()?;

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
