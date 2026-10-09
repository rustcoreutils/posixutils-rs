//
// Copyright (c) 2024-2026 Jeff Garzik
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
use gettextrs::gettext;
use std::ffi::OsString;
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
    #[arg(short, long, allow_hyphen_values = true, default_value = "", help = gettext("Use string as the prompt when in command mode"))]
    prompt: String,

    // Repeatable: `ed -s - file` is -s twice once `-` is spelled as -s.
    #[arg(short, long, overrides_with = "silent", help = gettext("Suppress the writing of byte counts by e, E, r, and w commands and the '!' prompt after !command"))]
    silent: bool,

    #[arg(help = gettext("File to edit"))]
    file: Option<String>,
}

/// Spell the historic option `-` as `-s`.
///
/// `ed - file` is the form POSIX withdrew in favour of `-s`, and GNU patch
/// still runs it to apply an ed-style diff. A `-` is the option only where an
/// option may stand: before the file operand and before `--`, and never as
/// the option-argument of `-p`, which may be a prompt of `-`.
fn rewrite_lone_dash(argv: Vec<OsString>) -> Vec<OsString> {
    let mut out = Vec::with_capacity(argv.len());
    let mut words = argv.into_iter();
    out.extend(words.next());
    while let Some(word) = words.next() {
        let text = word.to_str().unwrap_or_default();
        if text == "-" {
            out.push(OsString::from("-s"));
            continue;
        }
        let takes_argument = text == "--prompt"
            || (text.starts_with('-')
                && !text.starts_with("--")
                && text.find('p') == Some(text.len() - 1));
        let operand = text == "--" || !text.starts_with('-');
        out.push(word);
        if operand {
            break;
        }
        if takes_argument {
            out.extend(words.next());
        }
    }
    out.extend(words);
    out
}

/// SIGINT signal handler - sets the SIGINT_RECEIVED flag
extern "C" fn sigint_handler(_signum: libc::c_int) {
    SIGINT_RECEIVED.store(true, Ordering::SeqCst);
}

/// SIGHUP signal handler - sets the SIGHUP_RECEIVED flag
extern "C" fn sighup_handler(_signum: libc::c_int) {
    SIGHUP_RECEIVED.store(true, Ordering::SeqCst);
}

/// Install `handler` for `signum` *without* `SA_RESTART`.
///
/// `libc::signal` has BSD semantics on the platforms this targets, and those
/// include `SA_RESTART`: the kernel restarts an interrupted read rather than
/// returning `EINTR`, so a signal arriving while ed waits for a command was
/// not noticed until the user typed something.  For SIGHUP there is no such
/// keystroke coming, so the buffer ed is meant to save to `ed.hup` went with
/// the process instead.
fn install(signum: libc::c_int, handler: extern "C" fn(libc::c_int)) {
    unsafe {
        let mut action: libc::sigaction = std::mem::zeroed();
        action.sa_sigaction = handler as usize;
        libc::sigemptyset(&mut action.sa_mask);
        action.sa_flags = 0; // deliberately not SA_RESTART
        libc::sigaction(signum, &action, std::ptr::null_mut());
    }
}

/// Set up signal handlers per POSIX requirements for ed.
fn setup_signals() {
    // SIGQUIT: Ignore (POSIX requirement). No handler runs, so the restart
    // semantics that matter above do not apply.
    unsafe {
        libc::signal(libc::SIGQUIT, libc::SIG_IGN);
    }
    install(libc::SIGINT, sigint_handler);
    install(libc::SIGHUP, sighup_handler);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("ed");

    let args = Args::parse_from(rewrite_lone_dash(std::env::args_os().collect()));

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
                eprintln!("{}: {}", path, plib::diag::error_text(&e));
                editor.error_occurred = true;
            }
        }
    }

    // Run the editor loop
    if let Err(e) = editor.run() {
        eprintln!("ed: {}", plib::diag::io_error_text(&e));
        std::process::exit(1);
    }

    // POSIX EXIT STATUS: greater than 0 if any file or command error occurred.
    if editor.error_occurred {
        std::process::exit(1);
    }

    Ok(())
}
