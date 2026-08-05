//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::gettext;
use plib::tty::{self, mesg_allowed, set_mesg};
use std::io::{self, IsTerminal};
use std::process::ExitCode;

/// mesg - permit or deny messages
#[derive(Parser)]
#[command(version, about = gettext("mesg - permit or deny messages"))]
struct Args {
    /// "y" or "n":  Grant (y) or deny (n) permission to other users to send messages to the terminal device.
    operand: Option<String>,
}

enum Stream {
    Stdin,
    Stdout,
    Stderr,
}

/// Per POSIX/System V, operate on the first terminal among standard input,
/// standard output, and standard error (in that order).
fn find_tty() -> Option<Stream> {
    if io::stdin().is_terminal() {
        Some(Stream::Stdin)
    } else if io::stdout().is_terminal() {
        Some(Stream::Stdout)
    } else if io::stderr().is_terminal() {
        Some(Stream::Stderr)
    } else {
        None
    }
}

fn tty_to_fd(tty: Stream) -> i32 {
    match tty {
        Stream::Stdin => libc::STDIN_FILENO,
        Stream::Stdout => libc::STDOUT_FILENO,
        Stream::Stderr => libc::STDERR_FILENO,
    }
}

fn stat_tty() -> io::Result<(i32, libc::stat)> {
    let fd = match find_tty() {
        Some(tty) => tty_to_fd(tty),
        None => return Err(io::Error::other("no terminal")),
    };

    Ok((fd, tty::fstat(fd)?))
}

/// POSIX EXIT STATUS: 0 if receiving messages is allowed, 1 if not. The status
/// reflects the resulting state (verified against the system `mesg`: `mesg n`
/// exits 1, `mesg y` exits 0).
fn exit_for(allowed: bool) -> ExitCode {
    if allowed {
        ExitCode::SUCCESS
    } else {
        ExitCode::from(1)
    }
}

fn main() -> ExitCode {
    plib::diag::init_locale("mesg");

    let args = Args::parse();

    let (fd, st) = match stat_tty() {
        Ok(v) => v,
        Err(_) => {
            plib::diag::error(&gettext("cannot find a terminal"));
            return ExitCode::from(2);
        }
    };

    match args.operand.as_deref() {
        // No operand: report the current state without changing it.
        None => {
            let allowed = mesg_allowed(&st);
            println!("{}", if allowed { "is y" } else { "is n" });
            exit_for(allowed)
        }
        Some(op) => {
            // POSIX locale operands are exactly `y` and `n`.
            let grant = match op {
                "y" => true,
                "n" => false,
                _ => {
                    plib::diag::error(&gettext("invalid operand (expected 'y' or 'n')"));
                    return ExitCode::from(2);
                }
            };
            if let Err(e) = set_mesg(fd, &st, grant) {
                plib::diag::error(&format!(
                    "{}: {}",
                    gettext("failed to change terminal mode"),
                    e
                ));
                return ExitCode::from(2);
            }
            // Exit status reflects the resulting messaging state.
            exit_for(grant)
        }
    }
}
