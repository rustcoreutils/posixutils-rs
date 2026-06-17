//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::env;
use std::fs::{File, OpenOptions};
use std::io::{self, IsTerminal};
use std::os::unix::fs::OpenOptionsExt;
use std::os::unix::io::AsRawFd;
use std::process::{self, Command};

use gettextrs::gettext;
use libc::{dup, dup2, signal, SIGHUP, SIG_ERR, SIG_IGN};
use plib::diag;

enum NohupDir {
    Current,
    Home,
}

/// Open (create, append) `nohup.out`, trying the current directory first and
/// falling back to `$HOME/nohup.out`. A newly-created file is mode 0600 per
/// POSIX (the mode is only applied on creation; an existing file keeps its
/// permissions).
fn get_nohup_out_file() -> io::Result<(File, NohupDir)> {
    match OpenOptions::new()
        .create(true)
        .append(true)
        .mode(0o600)
        .open("nohup.out")
    {
        Ok(file) => Ok((file, NohupDir::Current)),
        Err(_) => {
            // POSIX: the fallback directory is the HOME environment variable.
            let home = env::var("HOME").map_err(|_| {
                io::Error::new(
                    io::ErrorKind::NotFound,
                    gettext("HOME environment variable not set"),
                )
            })?;
            let mut path = std::path::PathBuf::from(home);
            path.push("nohup.out");
            let file = OpenOptions::new()
                .create(true)
                .append(true)
                .mode(0o600)
                .open(path)?;
            Ok((file, NohupDir::Home))
        }
    }
}

/// True if `fd` refers to an open file description.
fn fd_is_open(fd: i32) -> bool {
    unsafe { libc::fcntl(fd, libc::F_GETFD) != -1 }
}

fn fatal(msg: String) -> ! {
    diag::error(&msg);
    process::exit(127);
}

fn main() {
    diag::init_locale("nohup");

    // Ignore SIGHUP before doing anything else; the disposition is inherited
    // across exec, so the utility runs immune to hangups.
    if unsafe { signal(SIGHUP, SIG_IGN) } == SIG_ERR {
        fatal(format!(
            "{}: {}",
            gettext("cannot ignore SIGHUP"),
            io::Error::last_os_error()
        ));
    }

    // Save the original stderr so a spawn failure can still be reported to the
    // user even after stderr has been redirected to nohup.out.
    let original_stderr = unsafe { dup(libc::STDERR_FILENO) };
    if original_stderr == -1 {
        fatal(gettext("failed to duplicate stderr"));
    }

    let mut args = env::args().skip(1);
    let command = match args.next() {
        Some(cmd) => cmd,
        None => fatal(gettext("usage: nohup utility [argument...]")),
    };

    let stdout_is_tty = io::stdout().is_terminal();
    let stderr_is_tty = io::stderr().is_terminal();
    let stdout_open = fd_is_open(libc::STDOUT_FILENO);

    // We need nohup.out for: redirecting a terminal stdout, and/or redirecting
    // a terminal stderr when stdout is itself a terminal or is closed.
    // (Simplified from `stdout_is_tty || (stderr_is_tty && (stdout_is_tty || !stdout_open))`.)
    let need_nohup_out = stdout_is_tty || (stderr_is_tty && !stdout_open);

    let nohup_out = if need_nohup_out {
        match get_nohup_out_file() {
            Ok(f) => Some(f),
            Err(e) => fatal(format!("{}: {}", gettext("cannot open nohup.out"), e)),
        }
    } else {
        None
    };

    // Redirect a terminal stdout to nohup.out, and announce it on stderr (the
    // notice is written while stderr is still the terminal).
    if stdout_is_tty {
        let (file, which) = nohup_out.as_ref().unwrap();
        if unsafe { dup2(file.as_raw_fd(), libc::STDOUT_FILENO) } == -1 {
            fatal(format!(
                "{}: {}",
                gettext("failed to redirect stdout"),
                io::Error::last_os_error()
            ));
        }
        let path = match which {
            NohupDir::Current => "nohup.out",
            NohupDir::Home => "$HOME/nohup.out",
        };
        eprintln!("{} {}", gettext("appending output to"), path);
    }

    // Redirect a terminal stderr. If stdout is open but is not a terminal,
    // stderr follows the *same* open file description as stdout; otherwise
    // (stdout is a terminal or is closed) stderr goes to nohup.out.
    if stderr_is_tty {
        let target_fd = if stdout_open && !stdout_is_tty {
            libc::STDOUT_FILENO
        } else {
            nohup_out.as_ref().unwrap().0.as_raw_fd()
        };
        if unsafe { dup2(target_fd, libc::STDERR_FILENO) } == -1 {
            // stderr redirect failed; report via the saved original stderr.
            unsafe { dup2(original_stderr, libc::STDERR_FILENO) };
            fatal(format!(
                "{}: {}",
                gettext("failed to redirect stderr"),
                io::Error::last_os_error()
            ));
        }
    }

    match Command::new(&command).args(args).spawn() {
        Ok(mut child) => {
            let code = match child.wait() {
                Ok(status) => status.code().unwrap_or(127),
                Err(_) => 127,
            };
            process::exit(code);
        }
        Err(error) => {
            // Restore the original stderr so the diagnostic reaches the user.
            unsafe {
                dup2(original_stderr, libc::STDERR_FILENO);
                libc::close(original_stderr);
            }
            match error.kind() {
                io::ErrorKind::NotFound => {
                    diag::error(&format!("{}: {}", command, gettext("command not found")));
                    process::exit(127);
                }
                _ => {
                    diag::error(&format!("{}: {}", command, gettext("cannot execute")));
                    process::exit(126);
                }
            }
        }
    }
}
