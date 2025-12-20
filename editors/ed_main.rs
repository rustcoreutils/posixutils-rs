//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! ed - edit text
//!
//! POSIX.1-2024 compliant ed line editor.

mod ed;

use clap::Parser;
use std::io::{self, BufReader, BufWriter};

/// ed - edit text
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Use string as the prompt when in command mode
    #[arg(short, long, default_value = "")]
    prompt: String,

    /// Suppress the writing of byte counts by e, E, r, and w commands
    /// and the '!' prompt after !command
    #[arg(short, long)]
    silent: bool,

    /// File to edit
    file: Option<String>,
}

/// Set up signal handlers per POSIX requirements for ed.
fn setup_signals() {
    unsafe {
        // SIGQUIT: Ignore (POSIX requirement)
        libc::signal(libc::SIGQUIT, libc::SIG_IGN);

        // Note: SIGINT and SIGHUP handling would require more complex
        // integration with the editor loop. For now, we let the default
        // behavior apply (SIGINT terminates, SIGHUP terminates).
        // A full implementation would need to:
        // - SIGINT: Set a flag, check in main loop, print "?" and continue
        // - SIGHUP: Save buffer to ed.hup, then exit
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    // Set up signal handlers
    setup_signals();

    let stdin = io::stdin();
    let stdout = io::stdout();
    let reader = BufReader::new(stdin.lock());
    let writer = BufWriter::new(stdout.lock());

    let mut editor = ed::Editor::new(reader, writer);

    // Set options from command line
    if !args.prompt.is_empty() {
        editor.prompt = args.prompt;
        editor.show_prompt = true;
    }
    editor.silent = args.silent;

    // Load file if specified
    if let Some(ref path) = args.file {
        match editor.load_file(path) {
            Ok(bytes) => {
                if !args.silent {
                    println!("{}", bytes);
                }
            }
            Err(e) => {
                if !args.silent {
                    eprintln!("{}: {}", path, e);
                }
            }
        }
    }

    // Run the editor loop
    if let Err(e) = editor.run() {
        eprintln!("ed: {}", e);
        std::process::exit(1);
    }

    Ok(())
}
