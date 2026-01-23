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
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::io::{self, BufReader, BufWriter};
use std::sync::atomic::{AtomicBool, Ordering};

/// Global flag for SIGINT received
pub static SIGINT_RECEIVED: AtomicBool = AtomicBool::new(false);

/// Global flag for SIGHUP received
pub static SIGHUP_RECEIVED: AtomicBool = AtomicBool::new(false);

/// ed - edit text
#[derive(Parser, Debug)]
#[command(version, about = gettext("ed - edit text"))]
struct Args {
    #[arg(short, long, default_value = "", help = gettext("Use string as the prompt when in command mode"))]
    prompt: String,

    #[arg(short, long, help = gettext("Suppress the writing of byte counts by e, E, r, and w commands and the '!' prompt after !command"))]
    silent: bool,

    #[arg(help = gettext("File to edit"))]
    file: Option<String>,
}

/// SIGINT signal handler - sets the SIGINT_RECEIVED flag
extern "C" fn sigint_handler(_signum: libc::c_int) {
    SIGINT_RECEIVED.store(true, Ordering::SeqCst);
}

/// SIGHUP signal handler - sets the SIGHUP_RECEIVED flag
extern "C" fn sighup_handler(_signum: libc::c_int) {
    SIGHUP_RECEIVED.store(true, Ordering::SeqCst);
}

/// Set up signal handlers per POSIX requirements for ed.
fn setup_signals() {
    unsafe {
        // SIGQUIT: Ignore (POSIX requirement)
        libc::signal(libc::SIGQUIT, libc::SIG_IGN);

        // SIGINT: Set flag to be checked in main loop
        libc::signal(
            libc::SIGINT,
            sigint_handler as *const () as libc::sighandler_t,
        );

        // SIGHUP: Set flag to save buffer and exit
        libc::signal(
            libc::SIGHUP,
            sighup_handler as *const () as libc::sighandler_t,
        );
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

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
