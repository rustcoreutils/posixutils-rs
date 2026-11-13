//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::OsString;
use std::io::{BufRead, BufWriter, IsTerminal};

use bc_util::{
    input::Pending, instructions::Program, interpreter::Interpreter, output::OutputWriter,
    parser::parse_program_at,
};
use clap::Parser;

use gettextrs::gettext;
use plib::diag;
use rustyline::{error::ReadlineError, DefaultEditor};

mod bc_util;

/// bc - arbitrary-precision arithmetic language
#[derive(Parser)]
#[command(version, about = gettext("bc - arbitrary-precision arithmetic language"))]
struct Args {
    #[arg(short = 'l')]
    define_math_functions: bool,

    files: Vec<OsString>,
}

/// The source name bc implementations conventionally use for standard input.
const STDIN_NAME: &str = "(standard_in)";

/// Stack for the interpreter thread.
///
/// Expression and function-call evaluation recurse on the machine stack, and
/// bc programs recurse legitimately -- a recursive factorial is the textbook
/// example. The default thread stack bounds that far below `MAX_EVAL_DEPTH`,
/// so the work runs on a thread large enough that the interpreter's own limit
/// is what reports the problem, with a diagnostic, instead of the process
/// aborting on a guard page.
///
/// The size is what a runaway recursion actually touches before that limit
/// fires, with margin. An unoptimized build uses several times the frame space
/// of a release one, so it needs the larger reservation; sizing the release
/// binary for the debug case would reserve address space no release run uses.
#[cfg(debug_assertions)]
const INTERPRETER_STACK_SIZE: usize = 512 * 1024 * 1024;
#[cfg(not(debug_assertions))]
const INTERPRETER_STACK_SIZE: usize = 128 * 1024 * 1024;

/// Start the interpreter thread, falling back to the default stack size.
///
/// A constrained address space -- `RLIMIT_AS`, or strict overcommit -- can
/// refuse a stack this large, and refusing to run at all would be a worse
/// answer than running with less room: only a program that recurses far enough
/// to need it is affected, and such a program then fails the way it did before
/// the large stack existed.
fn spawn_interpreter() -> std::io::Result<std::thread::JoinHandle<()>> {
    std::thread::Builder::new()
        .stack_size(INTERPRETER_STACK_SIZE)
        .spawn(run)
        .or_else(|_| std::thread::Builder::new().spawn(run))
}

fn main() {
    plib::io::ensure_std_fds_open();
    // The signal disposition, locale, message catalog and panic hook are
    // process-wide, and are set before the interpreter thread exists.
    diag::init_locale("bc");
    match spawn_interpreter() {
        // `run` ends the process itself; join only returns if it panicked.
        Ok(interpreter) => {
            if interpreter.join().is_err() {
                std::process::exit(1);
            }
        }
        Err(e) => {
            diag::error(&diag::io_error_text(&e));
            std::process::exit(1);
        }
    }
}

/// Report a failed write to standard output and exit.
///
/// Nothing more can be shown, and the unwritten output is still buffered: going
/// on would only fail, and report, again at the next flush.
fn write_failed(e: &std::io::Error) -> ! {
    diag::error(&format!(
        "{}: {}",
        gettext("write error"),
        diag::io_error_text(e)
    ));
    std::process::exit(1);
}

/// What to do after a piece of input has been dealt with.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Flow {
    /// Go on reading this input.
    Continue,
    /// A syntax error in a non-interactive input: nothing more is read from
    /// it, since what follows the error may belong to the rejected construct.
    StopInput,
    /// `quit` was read: stop altogether.
    Quit,
}

/// How one input is read: a file operand, or standard input.
struct Source<'s> {
    /// The name diagnostics about the input itself use.
    name: &'s str,
    /// The file name runtime errors cite, if any.
    file: Option<&'s str>,
    /// POSIX: bc is interactive when "the standard input and standard
    /// output to bc are attached to a terminal". An error then leaves the
    /// exit status alone; otherwise it makes bc exit with failure.
    interactive: bool,
    /// Typed at a terminal. POSIX asks an interactive bc to "recover
    /// following any error in the input"; a session typed at a terminal
    /// recovers even with its output redirected, rather than ending at the
    /// first typo. Other input stops at a syntax error.
    typed: bool,
    /// Flush output after each item, so a session at a terminal, or a
    /// program driving bc through pipes, sees each result as it is made.
    flush_each: bool,
}

/// The interpreter and its output, fed input piece by piece.
struct Session<'a> {
    interpreter: Interpreter,
    out: OutputWriter<'a>,
    had_error: bool,
}

impl Session<'_> {
    fn flush(&mut self) {
        if let Err(e) = self.out.flush() {
            write_failed(&e);
        }
    }

    /// Add one line of input, with its newline if it has one, and run what
    /// it completes. A line that is not text ends the input.
    fn feed(&mut self, pending: &mut Pending, line: &[u8], source: &Source) -> Flow {
        let Ok(line) = std::str::from_utf8(line) else {
            self.flush();
            diag::error(&format!("{}: {}", source.name, gettext("not a text file")));
            self.had_error = true;
            return Flow::StopInput;
        };
        pending.push_line(line);
        // A line typed at a terminal is parsed at once, so a typo inside an
        // open bracket is reported now rather than when the bracket closes.
        // Other input is parsed once it may be complete: parsing after every
        // line would make a long construct cost quadratic time.
        if !pending.ready() && !source.typed {
            return Flow::Continue;
        }
        self.run_pending(pending, source, false)
    }

    /// Run whatever is still gathered when the input ends. Input that ends in
    /// the middle of a construct is reported, not silently dropped.
    fn finish(&mut self, pending: &mut Pending, source: &Source) -> Flow {
        if pending.is_empty() {
            return Flow::Continue;
        }
        self.run_pending(pending, source, true)
    }

    fn run_pending(&mut self, pending: &mut Pending, source: &Source, at_end: bool) -> Flow {
        match parse_program_at(pending.text(), source.file, pending.first_line()) {
            Ok(program) => {
                pending.clear();
                self.exec(program, source)
            }
            Err(e) if e.is_incomplete && !at_end => Flow::Continue,
            Err(e) => {
                pending.clear();
                self.flush();
                for (line, col, message) in e.diagnostics() {
                    diag::error_at(diag::Position::new(line, col), message);
                }
                // Input cut off in mid-construct is a failure even at a
                // terminal: the session did not run what was typed.
                if at_end || !source.interactive {
                    self.had_error = true;
                }
                if source.typed {
                    Flow::Continue
                } else {
                    Flow::StopInput
                }
            }
        }
    }

    /// Run parsed items. A runtime error abandons the rest of its item only.
    fn exec(&mut self, program: Program, source: &Source) -> Flow {
        if let Err(e) = self.interpreter.exec(program, &mut self.out) {
            if let Some(failure) = e.write_failure() {
                write_failed(failure);
            }
            self.flush();
            diag::error(&e.to_string());
            if !source.interactive {
                self.had_error = true;
            }
        }
        if source.flush_each {
            self.flush();
        }
        if self.interpreter.has_quit() {
            Flow::Quit
        } else {
            Flow::Continue
        }
    }

    /// End bc, with failure if any error made it fail.
    fn exit(&mut self) -> ! {
        self.flush();
        std::process::exit(if self.had_error { 1 } else { 0 });
    }

    /// Run a file operand, as it is read, one item at a time.
    fn run_file(&mut self, text: &[u8], source: &Source) -> Flow {
        let mut pending = Pending::new();
        for line in text.split_inclusive(|&b| b == b'\n') {
            let flow = self.feed(&mut pending, line, source);
            if flow != Flow::Continue {
                return flow;
            }
        }
        self.finish(&mut pending, source)
    }

    /// Run standard input without line editing: input that is not a
    /// terminal, or a terminal whose output goes elsewhere. It is program
    /// text, read as bytes exactly as a file is.
    fn run_stdin_stream(&mut self, source: &Source) {
        let mut pending = Pending::new();
        let mut reader = std::io::stdin().lock();
        let mut line = Vec::new();
        loop {
            line.clear();
            match reader.read_until(b'\n', &mut line) {
                Ok(0) => break,
                Ok(_) => {}
                Err(e) => {
                    self.flush();
                    diag::error(&format!("{}: {}", source.name, diag::io_error_text(&e)));
                    self.had_error = true;
                    return;
                }
            }
            match self.feed(&mut pending, &line, source) {
                Flow::Continue => {}
                Flow::StopInput => return,
                Flow::Quit => self.exit(),
            }
        }
        if self.finish(&mut pending, source) == Flow::Quit {
            self.exit();
        }
    }

    /// Run standard input at a terminal, with line editing and history.
    fn run_stdin_terminal(&mut self, source: &Source) {
        let mut repl = match DefaultEditor::new() {
            Ok(repl) => repl,
            Err(e) => {
                diag::error(&diag::error_text(&e));
                std::process::exit(1);
            }
        };
        let mut pending = Pending::new();
        loop {
            let prompt = if pending.is_empty() { ">> " } else { ".. " };
            match repl.readline(prompt) {
                Ok(line) => {
                    let _ = repl.add_history_entry(line.as_str());
                    let line = format!("{}\n", line);
                    match self.feed(&mut pending, line.as_bytes(), source) {
                        Flow::Continue => {}
                        Flow::StopInput => return,
                        Flow::Quit => self.exit(),
                    }
                }
                // Interrupt (Ctrl-C) abandons a construct being typed; at an
                // empty prompt it ends the session, as end of input does.
                Err(ReadlineError::Interrupted) if !pending.is_empty() => pending.clear(),
                Err(ReadlineError::Eof) | Err(ReadlineError::Interrupted) => break,
                Err(e) => {
                    diag::error(&diag::error_text(&e));
                    self.had_error = true;
                    break;
                }
            }
        }
        if self.finish(&mut pending, source) == Flow::Quit {
            self.exit();
        }
    }
}

/// Whether standard input is a regular file. Such input is all there, so
/// there is no one waiting on each result as it is made.
fn stdin_is_regular_file() -> bool {
    stdin_as_file()
        .and_then(|file| file.metadata())
        .is_ok_and(|metadata| metadata.is_file())
}

/// A duplicate of standard input's descriptor, as a `File` whose metadata can
/// be read. Dropping it closes only the duplicate.
#[cfg(unix)]
fn stdin_as_file() -> std::io::Result<std::fs::File> {
    use std::os::fd::AsFd;
    let fd = std::io::stdin().as_fd().try_clone_to_owned()?;
    Ok(std::fs::File::from(fd))
}

/// A duplicate of standard input's handle, as a `File` whose metadata can be
/// read. A console handle has none, and so is not a regular file. Dropping it
/// closes only the duplicate.
#[cfg(windows)]
fn stdin_as_file() -> std::io::Result<std::fs::File> {
    use std::os::windows::io::AsHandle;
    let handle = std::io::stdin().as_handle().try_clone_to_owned()?;
    Ok(std::fs::File::from(handle))
}

fn run() {
    // The diagnostic prefix and source name are per thread.
    diag::init("bc");

    let args = plib::optarg::parse::<Args>();

    // Block-buffered so a long-running loop streams rather than accumulating
    // its whole output in memory; flushed after each standard input item so
    // interactive output appears without delay, as POSIX requires.
    let stdout = std::io::stdout();
    let mut sink = BufWriter::new(stdout.lock());
    let mut session = Session {
        interpreter: Interpreter::default(),
        out: OutputWriter::new(&mut sink),
        had_error: false,
    };

    if args.define_math_functions {
        diag::set_source("math library");
        let library = include_str!("bc_util/math_functions.bc");
        let load = parse_program_at(library, None, 1)
            .map_err(|e| e.to_string())
            .and_then(|lib| {
                session
                    .interpreter
                    .exec(lib, &mut session.out)
                    .map_err(|e| e.to_string())
            });
        if let Err(e) = load {
            diag::error(&format!(
                "{}: {}",
                gettext("internal error loading the standard math functions"),
                e
            ));
            std::process::exit(1);
        }
    }

    for file in args.files {
        let name = file.to_string_lossy().into_owned();
        diag::set_source(&name);
        let bytes = match std::fs::read(&file) {
            Ok(bytes) => bytes,
            Err(e) => {
                // POSIX CONSEQUENCES OF ERRORS: if a file operand cannot be
                // accessed, write a diagnostic and terminate.
                session.flush();
                diag::error(&format!("{}: {}", name, diag::io_error_text(&e)));
                std::process::exit(1);
            }
        };
        let source = Source {
            name: &name,
            file: file.to_str(),
            interactive: false,
            typed: false,
            flush_each: false,
        };
        let flow = session.run_file(&bytes, &source);
        session.flush();
        if flow == Flow::Quit {
            session.exit();
        }
    }

    diag::set_source(STDIN_NAME);
    let typed = std::io::stdin().is_terminal();
    let source = Source {
        name: STDIN_NAME,
        file: None,
        interactive: typed && std::io::stdout().is_terminal(),
        typed,
        flush_each: typed || !stdin_is_regular_file(),
    };
    // Line editing writes prompts and terminal controls to standard output,
    // so it is only for a session whose output is the terminal too.
    if source.interactive {
        session.run_stdin_terminal(&source);
    } else {
        session.run_stdin_stream(&source);
    }
    session.exit();
}
