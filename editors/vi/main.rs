//! vi-rs: A POSIX-compliant vi editor implementation in Rust.
//!
//! This is the main entry point for the vi editor.

use std::env;
use std::process;
use vi_rs::Editor;

fn main() {
    let args: Vec<String> = env::args().collect();

    // Parse command line arguments
    let mut files = Vec::new();
    let mut readonly = false;
    let mut command: Option<String> = None;
    let mut i = 1;

    while i < args.len() {
        let arg = &args[i];
        match arg.as_str() {
            "-R" => {
                // Read-only mode
                readonly = true;
            }
            "-r" => {
                // Recovery mode (not implemented)
                eprintln!("vi: recovery mode not supported");
                process::exit(1);
            }
            "-c" => {
                // Execute command
                i += 1;
                if i < args.len() {
                    command = Some(args[i].clone());
                } else {
                    eprintln!("vi: -c requires an argument");
                    process::exit(1);
                }
            }
            "-t" => {
                // Tag search (not implemented)
                eprintln!("vi: tag mode not supported");
                process::exit(1);
            }
            "-w" => {
                // Window size (handled by options)
                i += 1;
                // Skip the size argument
            }
            "--version" => {
                println!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
                println!("{}", env!("CARGO_PKG_DESCRIPTION"));
                process::exit(0);
            }
            "--help" | "-h" => {
                print_usage(&args[0]);
                process::exit(0);
            }
            s if s.starts_with('+') => {
                // +command
                command = Some(s[1..].to_string());
            }
            s if s.starts_with('-') => {
                eprintln!("vi: unknown option: {}", s);
                process::exit(1);
            }
            _ => {
                files.push(arg.clone());
            }
        }
        i += 1;
    }

    // Create editor
    let mut editor = match Editor::new() {
        Ok(e) => e,
        Err(e) => {
            eprintln!("vi: failed to initialize: {}", e);
            process::exit(1);
        }
    };

    // Set read-only mode if requested
    if readonly {
        // TODO: Set readonly flag in editor
    }

    // Open files
    if !files.is_empty() {
        if let Err(e) = editor.open_files(files) {
            eprintln!("vi: {}", e);
            process::exit(1);
        }
    }

    // Execute initial command if specified
    if let Some(_cmd) = command {
        // TODO: Execute command after opening
    }

    // Run the editor
    let exit_code = match editor.run() {
        Ok(code) => code,
        Err(e) => {
            eprintln!("vi: {}", e);
            1
        }
    };

    process::exit(exit_code);
}

fn print_usage(program: &str) {
    println!("Usage: {} [options] [file...]", program);
    println!();
    println!("{}", env!("CARGO_PKG_DESCRIPTION"));
    println!();
    println!("Options:");
    println!("  -R              Read-only mode");
    println!("  -c command      Execute command after opening");
    println!("  +command        Same as -c command");
    println!("  --version       Print version information");
    println!("  --help, -h      Print this help message");
    println!();
    println!("While editing:");
    println!("  :q              Quit");
    println!("  :w              Write file");
    println!("  :wq             Write and quit");
    println!("  :q!             Quit without saving");
    println!("  i               Enter insert mode");
    println!("  ESC             Return to command mode");
    println!("  /pattern        Search forward");
    println!("  ?pattern        Search backward");
    println!("  :help           Show help");
}
