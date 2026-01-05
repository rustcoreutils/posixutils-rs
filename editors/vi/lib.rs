//! vi-rs: A POSIX-compliant vi editor implementation in Rust.
//!
//! This library provides a complete vi editor implementation following
//! the POSIX specification.

pub mod buffer;
pub mod command;
pub mod editor;
pub mod error;
pub mod ex;
pub mod file;
pub mod input;
pub mod mode;
pub mod options;
pub mod register;
pub mod search;
pub mod shell;
pub mod ui;
pub mod undo;

pub use buffer::{Buffer, BufferMode, Line, Position, Range};
pub use command::{
    CommandParser, MotionCommand, MotionResult, OperatorResult, ParsedCommand, ParserState, change,
    delete, put_after, put_before, shift_left, shift_right, yank,
};
pub use editor::{Editor, ExInsertMode, FindCommand, LastCommand};
pub use error::{Result, ViError};
pub use ex::{Address, AddressRange, ExCommand, ExResult, parse_ex_command};
pub use file::{FileInfo, FileManager, WriteStats, read_file, write_file};
pub use input::{InputReader, Key};
pub use mode::{InsertKind, InsertState, Mode, enter_insert_mode, process_insert_key};
pub use options::Options;
pub use register::{RegisterContent, Registers};
pub use search::{SearchDirection, SearchState, Substitutor};
pub use shell::{ShellExecutor, ShellOutput};
pub use ui::{
    Screen, StatusLine, Terminal, TerminalSize, byte_offset_to_display_col, char_width,
    display_col_to_byte_offset, expand_for_display, string_width, truncate_to_width,
};
pub use undo::{Change, ChangeKind, UndoManager};

use std::process;

/// Invocation mode for the editor.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InvokedAs {
    /// Invoked as vi - start in visual mode.
    Vi,
    /// Invoked as ex - start in ex (line) mode.
    Ex,
}

/// Run the editor with the given invocation mode and command-line arguments.
///
/// This is the main entry point for both vi and ex binaries.
/// Returns the exit code.
pub fn run_editor(invoked_as: InvokedAs, args: &[String]) -> i32 {
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
    let mut editor = match Editor::new_with_mode(opts.start_in_ex_mode, opts.silent_mode) {
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

    // Open files
    if !opts.files.is_empty() {
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
                return Err("recovery mode not supported".to_string());
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
                return Err("tag mode not supported".to_string());
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
