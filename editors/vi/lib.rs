//! vi-rs: A POSIX-compliant vi editor implementation in Rust.
//!
//! This library provides a complete vi editor implementation following
//! the POSIX specification.

pub mod buffer;
pub mod command;
pub mod config;
pub mod editor;
pub mod error;
pub mod ex;
pub mod file;
pub mod input;
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
pub use search::{SearchDirection, SearchState, Substitutor};
pub use shell::{ShellExecutor, ShellOutput};
pub use ui::{
    byte_offset_to_display_col, char_width, display_col_to_byte_offset, expand_for_display,
    string_width, truncate_to_width, Screen, StatusLine, Terminal, TerminalSize,
};
pub use undo::{Change, ChangeKind, UndoManager};

use gettextrs::{setlocale, LocaleCategory};
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
        let prog = std::env::args().next().unwrap_or_default();
        if prog.ends_with("ex") {
            InvokedAs::Ex
        } else {
            InvokedAs::Vi
        }
    }
}

/// Run the editor with the given invocation mode and command-line arguments.
///
/// This is the main entry point for both vi and ex binaries.
/// Returns the exit code.
pub fn run_editor(invoked_as: InvokedAs, args: &[String]) -> i32 {
    // Honor the user's locale: LC_CTYPE/LC_COLLATE drive the libc regex engine
    // and LC_MESSAGES localizes diagnostics.
    setlocale(LocaleCategory::LcAll, "");

    // Preserve the buffer on hangup/termination, and prune old recovery files.
    signals::install_hangup_handlers();
    recover::cleanup_stale(14 * 24 * 60 * 60);

    let opts = match parse_args(invoked_as, args) {
        Ok(o) => o,
        Err(e) => {
            let name = if invoked_as == InvokedAs::Ex {
                "ex"
            } else {
                "vi"
            };
            eprintln!("{}: {}", name, e);
            return 1;
        }
    };

    let prog_name = if invoked_as == InvokedAs::Ex {
        "ex"
    } else {
        "vi"
    };

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

    // Load startup configuration (EXINIT or $HOME/.exrc)
    // Per POSIX, this happens before editing the first file
    if let Err(e) = editor.load_startup_config() {
        if !opts.silent_mode {
            eprintln!("{}: startup config: {}", prog_name, e);
        }
        // Continue anyway - don't fail on config errors
    }

    // Recovery mode (-r): list recoverable buffers (no operand) or recover
    // the named file; otherwise open the operands normally.
    if opts.recover {
        if opts.files.is_empty() {
            let recs = recover::list();
            if recs.is_empty() {
                println!("No files to recover");
            } else {
                println!("Recoverable files:");
                for r in &recs {
                    println!("  {}", r.orig_path.as_deref().unwrap_or("(unnamed)"));
                }
            }
            return 0;
        }
        if let Err(e) = editor.recover(opts.files.first().map(|s| s.as_str())) {
            eprintln!("{}: {}", prog_name, e);
            return 1;
        }
    } else if let Some(tag) = &opts.tag {
        // -t: open the file containing the tag and jump to it.
        if let Err(e) = editor.tag(tag) {
            eprintln!("{}: {}", prog_name, e);
            return 1;
        }
    } else if !opts.files.is_empty() {
        if let Err(e) = editor.open_files(opts.files) {
            eprintln!("{}: {}", prog_name, e);
            return 1;
        }
    }

    // Execute initial command if specified
    if let Some(cmd) = opts.command {
        if let Err(e) = editor.execute_initial_command(&cmd) {
            eprintln!("{}: {}", prog_name, e);
            // Don't exit - continue with the editor
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
    /// Initial command to execute (-c or +command).
    command: Option<String>,
    /// Recover a previously-preserved buffer (-r).
    recover: bool,
    /// Tag to jump to at startup (-t).
    tag: Option<String>,
    /// Files to edit.
    files: Vec<String>,
}

/// Parse command-line arguments for the editor.
fn parse_args(
    invoked_as: InvokedAs,
    args: &[String],
) -> std::result::Result<EditorOptions, String> {
    let mut opts = EditorOptions {
        start_in_ex_mode: invoked_as == InvokedAs::Ex,
        silent_mode: false,
        readonly: false,
        command: None,
        recover: false,
        tag: None,
        files: Vec::new(),
    };

    let mut i = 1;
    while i < args.len() {
        let arg = &args[i];
        match arg.as_str() {
            "-R" => {
                opts.readonly = true;
            }
            "-r" => {
                opts.recover = true;
            }
            "-c" => {
                i += 1;
                if i < args.len() {
                    opts.command = Some(args[i].clone());
                } else {
                    return Err("-c requires an argument".to_string());
                }
            }
            "-t" => {
                i += 1;
                if i < args.len() {
                    opts.tag = Some(args[i].clone());
                } else {
                    return Err("-t requires a tagstring".to_string());
                }
            }
            "-w" => {
                // Window size (handled by options)
                i += 1;
                // Skip the size argument
            }
            "-s" => {
                // Silent/batch mode (ex only, but accept for both)
                opts.silent_mode = true;
            }
            "-v" => {
                // Start in visual mode (ex option to behave like vi)
                opts.start_in_ex_mode = false;
            }
            "--version" => {
                println!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
                println!("{}", env!("CARGO_PKG_DESCRIPTION"));
                process::exit(0);
            }
            "--help" | "-h" => {
                print_usage(&args[0], invoked_as);
                process::exit(0);
            }
            s if s.starts_with('+') => {
                opts.command = Some(s[1..].to_string());
            }
            s if s.starts_with('-') => {
                return Err(format!("unknown option: {}", s));
            }
            _ => {
                opts.files.push(arg.clone());
            }
        }
        i += 1;
    }

    Ok(opts)
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
