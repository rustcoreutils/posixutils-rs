//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! vi-rs: A POSIX-compliant vi editor implementation in Rust.
//!
//! This library provides a complete vi editor implementation following
//! the POSIX specification.

pub mod buffer;
pub mod bytes;
pub mod command;
pub mod config;
pub mod editor;
pub mod error;
pub mod ex;
pub mod file;
pub mod input;
pub mod maps;
pub mod mode;
pub mod options;
pub mod recover;
pub mod register;
pub mod search;
pub mod shell;
pub mod signals;
pub mod tags;
pub mod ui;
pub mod undo;

pub use buffer::{Buffer, BufferMode, Line, Position, Range};
pub use command::{
    change, delete, put_after, put_before, shift_left, shift_right, yank, CommandParser,
    MotionCommand, MotionResult, OperatorResult, ParsedCommand, ParserState,
};
pub use editor::{Editor, ExInsertMode, FindCommand, LastCommand};
pub use error::{Result, ViError};
pub use ex::{parse_ex_command, Address, AddressRange, ExCommand, ExResult};
pub use file::{read_file, write_file, FileInfo, FileManager, WriteStats};
pub use input::{InputReader, Key};
pub use mode::{enter_insert_mode, process_insert_key, InsertKind, InsertState, Mode};
pub use options::Options;
pub use register::{RegisterContent, Registers};
pub use search::{SearchDirection, SearchState, SubstituteConfig, Substitutor};
pub use shell::{ShellExecutor, ShellOutput};
pub use ui::{
    byte_offset_to_display_col, caret_notation, char_width, display_col_to_byte_offset,
    expand_for_display, string_width, truncate_to_width, Screen, StatusLine, Terminal,
    TerminalSize,
};
pub use undo::{Change, ChangeKind, UndoManager};

use std::io::IsTerminal;
use std::process;

/// Invocation mode for the editor.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InvokedAs {
    /// Invoked as vi - start in visual mode.
    Vi,
    /// Invoked as ex - start in ex (line) mode.
    Ex,
}

impl InvokedAs {
    /// Detect invocation mode from argv[0].
    ///
    /// If the program name ends with "ex", returns `Ex`.
    /// Otherwise returns `Vi` (default).
    pub fn detect() -> Self {
        // `args_os`: `args` panics on a program name that is not valid UTF-8.
        let prog = std::env::args_os().next().unwrap_or_default();
        let prog = prog.to_string_lossy();
        if prog.ends_with("ex") {
            InvokedAs::Ex
        } else {
            InvokedAs::Vi
        }
    }

    /// The name this invocation is known by, for diagnostics and the locale.
    pub fn name(self) -> &'static str {
        match self {
            InvokedAs::Ex => "ex",
            InvokedAs::Vi => "vi",
        }
    }
}

/// Run the editor with the given invocation mode and command-line arguments.
///
/// This is the main entry point for both vi and ex binaries.
/// Returns the exit code.
pub fn run_editor(invoked_as: InvokedAs, args: &[String]) -> i32 {
    // Preserve the buffer on hangup/termination, and prune old recovery files.
    signals::install_hangup_handlers();
    recover::cleanup_stale(&recover::default_base(), 14 * 24 * 60 * 60);

    let mut opts = parse_args(invoked_as, args);
    let prog_name = invoked_as.name();

    // -r with no file and no -t (94207-94209): every other option, EXINIT
    // and .exrc are ignored; list what can be recovered and exit.
    if opts.recover && opts.files.is_empty() && opts.tag.is_none() {
        let recs = recover::list(&recover::default_base());
        if recs.is_empty() {
            println!("No files to recover");
        } else {
            println!("Recoverable files:");
            for r in &recs {
                bytes::println(&format!(
                    "  {}",
                    r.orig_path.as_deref().unwrap_or("(unnamed)")
                ));
            }
        }
        return 0;
    }
    if let Some(e) = &opts.error {
        eprintln!("{}: {}", prog_name, e);
        return 1;
    }

    // POSIX: if standard input is not a terminal, behave as if -s was given.
    // That also means assuming a terminal that cannot support visual mode, so
    // fall back to ex (line) mode rather than trying to enable raw mode on a
    // non-terminal stdin (which would fail).
    if !std::io::stdin().is_terminal() {
        opts.silent_mode = true;
        opts.start_in_ex_mode = true;
    }

    // Create editor with appropriate mode
    let mut editor = match Editor::new(opts.start_in_ex_mode, opts.silent_mode) {
        Ok(e) => e,
        Err(e) => {
            eprintln!("{}: failed to initialize: {}", prog_name, e);
            return 1;
        }
    };

    // Set read-only mode if requested
    if opts.readonly {
        editor.set_readonly(true);
    }

    // Apply the initial window size (-w).
    if let Some(n) = opts.window {
        editor.set_window(n);
    }

    // Load startup configuration (EXINIT or $HOME/.exrc) before editing the
    // first file. POSIX: in silent/batch mode (-s, or non-terminal stdin),
    // EXINIT and .exrc are not consulted.
    if !opts.silent_mode {
        if let Err(e) = editor.load_startup_config() {
            eprintln!("{}: startup config: {}", prog_name, e);
        }
        // Continue anyway - don't fail on config errors
    }

    // The first file to edit (94432-94443): the tag's, else the first
    // operand.  With -r each file is recovered the first time it is edited
    // (94444-94451); the -c commands run in the first buffer loaded from a
    // file that exists, before the tag's movement (94452-94459).
    editor.set_initial_commands(prog_name, std::mem::take(&mut opts.commands));
    if opts.recover {
        editor.recover_on_first_edit(&opts.files, opts.tag.is_some());
    }
    if let Some(tag) = &opts.tag {
        // A tag that cannot be found is a diagnostic, as for `:tag`;
        // editing goes on.
        if let Err(e) = editor.tag(tag) {
            eprintln!("{}: {}", prog_name, e);
        }
        editor.set_args_after_tag(&opts.files);
    } else if !opts.files.is_empty() {
        if let Err(e) = editor.open_files(opts.files) {
            eprintln!("{}: {}", prog_name, e);
            return 1;
        }
    }

    // Run the editor
    match editor.run() {
        Ok(code) => code,
        Err(e) => {
            eprintln!("{}: {}", prog_name, e);
            1
        }
    }
}

/// Parsed command-line options.
struct EditorOptions {
    /// Start in ex mode.
    start_in_ex_mode: bool,
    /// Silent/batch mode for ex (-s).
    silent_mode: bool,
    /// Read-only mode (-R).
    readonly: bool,
    /// Initial commands to execute (-c or +command), in order (94196-94197).
    commands: Vec<String>,
    /// Recover a previously-preserved buffer (-r).
    recover: bool,
    /// Tag to jump to at startup (-t).
    tag: Option<String>,
    /// Initial window size (-w).
    window: Option<usize>,
    /// Files to edit.
    files: Vec<String>,
    /// The first command-line error, reported unless `-r` lists (which
    /// ignores every other option, 94207-94209).
    error: Option<String>,
}

/// Parse the command line as XBD 12.2 utility syntax (ex 94191-94192):
/// options may be grouped (`-sR`), an option-argument attached (`-c1p`,
/// `-w5`) or separate, `--` ends the options, and so does the first
/// operand.  `+command` is `-c command` (`+` may delimit an option, and
/// `+` alone goes to the last line), and `-`, whose meaning is
/// unspecified, is the historical `-s`.
fn parse_args(invoked_as: InvokedAs, args: &[String]) -> EditorOptions {
    let mut opts = EditorOptions {
        start_in_ex_mode: invoked_as == InvokedAs::Ex,
        silent_mode: false,
        readonly: false,
        commands: Vec::new(),
        recover: false,
        tag: None,
        window: None,
        files: Vec::new(),
        error: None,
    };
    let fail = |opts: &mut EditorOptions, msg: String| {
        opts.error.get_or_insert(msg);
    };

    let mut i = 1;
    while i < args.len() {
        let arg = args[i].as_str();
        match arg {
            "--" => {
                i += 1;
                break;
            }
            "--version" => {
                println!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
                println!("{}", env!("CARGO_PKG_DESCRIPTION"));
                process::exit(0);
            }
            "--help" => {
                print_usage(&args[0], invoked_as);
                process::exit(0);
            }
            "-" => opts.silent_mode = true,
            "+" => opts.commands.push("$".to_string()),
            _ if arg.starts_with('+') => opts.commands.push(arg[1..].to_string()),
            _ if arg.starts_with('-') => {
                // One or more options; one that takes an argument takes the
                // rest of this word, or the next word.
                for (at, c) in arg[1..].char_indices() {
                    match c {
                        'R' => opts.readonly = true,
                        'r' => opts.recover = true,
                        's' => opts.silent_mode = true,
                        'v' => opts.start_in_ex_mode = false,
                        'h' => {
                            print_usage(&args[0], invoked_as);
                            process::exit(0);
                        }
                        'c' | 't' | 'w' => {
                            let rest = &arg[1 + at + 1..];
                            let value = if !rest.is_empty() {
                                rest.to_string()
                            } else {
                                i += 1;
                                match args.get(i) {
                                    Some(v) => v.clone(),
                                    None => {
                                        fail(
                                            &mut opts,
                                            format!("option requires an argument -- {c}"),
                                        );
                                        break;
                                    }
                                }
                            };
                            match c {
                                'c' => opts.commands.push(value),
                                // 94223-94224: one -t only.
                                't' if opts.tag.is_some() => {
                                    fail(&mut opts, "only one -t option is allowed".to_string())
                                }
                                't' => opts.tag = Some(value),
                                _ => match value.parse::<usize>() {
                                    Ok(size) => opts.window = Some(size),
                                    Err(_) => {
                                        fail(&mut opts, format!("invalid window size: {value}"))
                                    }
                                },
                            }
                            break;
                        }
                        _ => fail(&mut opts, format!("unknown option -- {c}")),
                    }
                }
            }
            _ => break,
        }
        i += 1;
    }
    opts.files = args[i.min(args.len())..].to_vec();
    opts
}

fn print_usage(program: &str, invoked_as: InvokedAs) {
    let name = if invoked_as == InvokedAs::Ex {
        "ex"
    } else {
        "vi"
    };

    println!("Usage: {} [options] [file...]", program);
    println!();
    println!("{}", env!("CARGO_PKG_DESCRIPTION"));
    println!();
    println!("Options:");
    println!("  -R              Read-only mode");
    println!("  -c command      Execute command after opening");
    println!("  -r [file...]    Recover files; with no file, list recoverable ones");
    println!("  -t tagstring    Edit the file containing tagstring");
    println!("  -w size         Set the window option");
    println!("  +command        Same as -c command");

    if invoked_as == InvokedAs::Ex {
        println!("  -s              Silent (batch) mode");
        println!("  -v              Start in visual mode");
    }

    println!("  --version       Print version information");
    println!("  --help, -h      Print this help message");
    println!();

    if invoked_as == InvokedAs::Ex {
        println!("In {} mode, enter commands at the ':' prompt.", name);
        println!("Use 'visual' or 'vi' command to enter visual mode.");
    } else {
        println!("While editing:");
        println!("  :q              Quit");
        println!("  :w              Write file");
        println!("  :wq             Write and quit");
        println!("  :q!             Quit without saving");
        println!("  i               Enter insert mode");
        println!("  ESC             Return to command mode");
        println!("  /pattern        Search forward");
        println!("  ?pattern        Search backward");
        println!("  Q               Enter ex mode");
        println!("  :help           Show help");
    }
}
