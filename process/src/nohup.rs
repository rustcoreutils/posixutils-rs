//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use gettextrs::{bind_textdomain_codeset, textdomain};
use libc::signal;
use libc::{dup, dup2, SIGHUP, SIG_IGN};
use plib::PROJECT_NAME;
use std::env;
use std::fs::{File, OpenOptions};
use std::io;
use std::os::unix::io::AsRawFd;
use std::process::{self, Command};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    unsafe {
        // Ignore the SIGHUP signal
        signal(SIGHUP, SIG_IGN);
    }

    // Save the original stderr
    let original_stderr = unsafe { dup(libc::STDERR_FILENO) };
    if original_stderr == -1 {
        eprintln!("Failed to duplicate stderr");
        process::exit(127);
    }

    // Getting the command and arguments
    let mut args = env::args().skip(1);
    let command = match args.next() {
        Some(cmd) => cmd,
        None => {
            eprintln!("Usage: nohup <command> [args...]");
            process::exit(127);
        }
    };

    // Redirecting stdout and stderr to the nohup.out file if they are connected to a terminal
    if atty::is(atty::Stream::Stdout) || atty::is(atty::Stream::Stderr) {
        let nohup_out_file =
            get_nohup_out_file().expect("Failed to open nohup.out in current or home directory");

        if atty::is(atty::Stream::Stdout) {
            let fd = nohup_out_file.0.as_raw_fd();

            if unsafe { dup2(fd, libc::STDOUT_FILENO) } == -1 {
                eprintln!("Failed to redirect stdout");
                process::exit(127);
            }

            match nohup_out_file.1 {
                NohupDir::Current => {
                    eprintln!(
                        "Name of the file to which the output is being appended: `nohup.out`"
                    );
                }
                NohupDir::Home => {
                    eprintln!(
                        "Name of the file to which the output is being appended: `$HOME/nohup.out`"
                    );
                }
            }
        }

        if atty::is(atty::Stream::Stderr) {
            let fd = nohup_out_file.0.as_raw_fd();

            if unsafe { dup2(fd, libc::STDERR_FILENO) } == -1 {
                eprintln!("Failed to redirect stderr");
                process::exit(127);
            }
        }
    }

    match Command::new(command).args(args).spawn() {
        Ok(mut process) => {
            process::exit(process.wait()?.code().unwrap_or(127));
        }
        Err(error) => {
            use std::io::ErrorKind;

            // Restore the original stderr

            if unsafe { dup2(original_stderr, libc::STDERR_FILENO) } == -1 {
                eprintln!("Failed to restore stderr");
                process::exit(127);
            }

            // Close the duplicated descriptor as it's no longer needed
            unsafe { libc::close(original_stderr) };

            match error.kind() {
                ErrorKind::NotFound => {
                    eprintln!("Error: command not found");
                    process::exit(127);
                }
                _ => {
                    eprintln!("Error: command found but could not be invoked");
                    process::exit(126);
                }
            }
        }
    }
}

enum NohupDir {
    Current,
    Home,
}

fn get_nohup_out_file() -> Result<(File, NohupDir), io::Error> {
    // Attempting to open or create a nohup.out file in the current directory
    match OpenOptions::new()
        .create(true)
        .append(true)
        .open("nohup.out")
    {
        Ok(file) => Ok((file, NohupDir::Current)),
        Err(_) => {
            // If unsuccessful, attempt to create a nohup.out file in the home directory
            if let Some(home_dir) = dirs::home_dir() {
                let mut home_nohup_path = home_dir;
                home_nohup_path.push("nohup.out");
                let file = OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(home_nohup_path)?;
                Ok((file, NohupDir::Home))
            } else {
                Err(io::Error::new(
                    io::ErrorKind::NotFound,
                    "Home directory not found",
                ))
            }
        }
    }
}
