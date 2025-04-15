//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::Local;
use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};
use plib::{curuser, platform, utmpx};
use std::fs::{self, OpenOptions};
use std::io::{self, BufRead, Write};
use std::os::unix::fs::MetadataExt;
use std::os::unix::fs::PermissionsExt;
use std::process::exit;

const ALERT_CHAR: char = '\u{07}';
const INTR_CHAR: char = '\u{03}';
const EOF_CHAR: char = '\u{04}';
const ERASE_CHAR: char = '\u{08}';
const KILL_CHAR: char = '\u{15}';

/// write - write to another user
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Login name of the person to whom the message shall be written.
    username: String,

    /// The terminal to which the message shall be written.
    terminal: Option<String>,
}

// Select terminal in an implementation-defined manner and return terminal
// Print an informational message about the chosen terminal
fn select_terminal(user_name: &str) -> String {
    let entries = utmpx::load();

    // Filter the entries to find terminals for the specified user
    let user_entries: Vec<_> = entries
        .into_iter()
        .filter(|entry| entry.user == user_name && entry.typ == platform::USER_PROCESS)
        .collect();

    if user_entries.is_empty() {
        eprintln!("{}: {}", gettext("No terminals found for user"), user_name);
        exit(1);
    }

    // Avoid selecting "console" and ensure we get a valid terminal
    for entry in &user_entries {
        if entry.line != "console" {
            let terminal = format!("/dev/{}", &entry.line);
            return terminal;
        }
    }

    eprintln!(
        "{}: {}",
        gettext("No valid terminals found for user"),
        user_name
    );
    exit(1);
}

fn check_write_permission(terminal: &str) -> bool {
    // Get the metadata for the terminal
    match fs::metadata(terminal) {
        Ok(metadata) => {
            let permissions = metadata.permissions().mode();

            // Get the current user ID and group ID
            let uid = unsafe { libc::getuid() };
            let gid = unsafe { libc::getgid() };

            // Check if the user is the owner and has write permission
            if metadata.uid() == uid && (permissions & 0o200 != 0) {
                return true;
            }

            // Check if the user's group has write permission
            if metadata.gid() == gid && (permissions & 0o020 != 0) {
                return true;
            }

            // Check if others have write permission
            if (permissions & 0o002) != 0 {
                return true;
            }

            false
        }
        Err(err) => {
            eprintln!(
                "{} {}: {}",
                gettext("Error checking metadata for terminal"),
                terminal,
                err
            );
            false
        }
    }
}

fn get_current_date() -> String {
    // Retrieve the current date and time in a human-readable format
    let now = Local::now();
    now.format("%Y-%m-%d %H:%M:%S").to_string()
}

fn process_erase_or_kill(line: &str) {
    let stdout = io::stdout();
    let mut handle = stdout.lock();

    for ch in line.chars() {
        match ch {
            ERASE_CHAR => {
                // Handle the erase character by writing backspace to the terminal
                handle
                    .write_all(b"\x08 \x08")
                    .expect("Failed to write erase character");
            }
            KILL_CHAR => {
                // Handle the kill character by writing a newline to the terminal
                handle
                    .write_all(b"\n")
                    .expect("Failed to write kill character");
            }
            _ => {
                handle
                    .write_all(ch.to_string().as_bytes())
                    .expect("Failed to write character");
            }
        }
    }

    handle.flush().expect("Failed to flush output");
}

fn write_to_terminal(terminal: &str, message: &str) {
    // Write the message to the specified terminal
    let mut file = OpenOptions::new()
        .write(true)
        .open(terminal)
        .expect("Failed to open terminal");
    file.write_all(message.as_bytes())
        .expect("Failed to write to terminal");
}

// Alert the sender's terminal twice
fn alert_sender_terminal() {
    let alert_message = format!("{}{}", ALERT_CHAR, ALERT_CHAR);
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    handle
        .write_all(alert_message.as_bytes())
        .expect("Failed to write alert");
    handle.flush().expect("Failed to flush alert");
}

// Check if the line contains interrupt or end-of-file characters
fn is_interrupt_or_eof(line: &str) -> bool {
    line.chars().all(|c| c == INTR_CHAR || c == EOF_CHAR)
}

// Check if the line contains the alert character
fn contains_alert_character(line: &str) -> bool {
    line.chars().all(|c| c == ALERT_CHAR)
}

// Return the alert character
fn get_alert_character() -> String {
    ALERT_CHAR.to_string()
}

// Check if the line contains erase or kill characters
fn contains_erase_or_kill_character(line: &str) -> bool {
    line.chars().all(|c| c == ERASE_CHAR || c == KILL_CHAR)
}

// Check if the line contains printable or space characters
fn contains_printable_or_space(line: &str) -> bool {
    line.chars()
        .all(|c| c.is_ascii_graphic() || c.is_ascii_whitespace())
}

// Process non-printable characters to implementation-defined sequences of printable characters
fn process_non_printable(line: &str) -> String {
    let mut s = String::with_capacity(line.len());

    for ch in line.chars() {
        if ch.is_ascii_control() {
            s.push_str(&format!("^{}", ch.to_ascii_uppercase()));
        } else {
            s.push(ch);
        }
    }

    s
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let user_name = args.username;
    let terminal = match args.terminal {
        Some(terminal) => {
            if terminal.starts_with("/dev/") {
                terminal
            } else {
                format!("/dev/{}", terminal)
            }
        }
        None => select_terminal(&user_name),
    };

    if !check_write_permission(&terminal) {
        eprintln!("{}", gettext("Permission denied"));
        exit(1);
    }

    let sender_login_id = curuser::login_name();
    let sending_terminal = curuser::tty();
    let date = get_current_date();

    let message = format!(
        "Message from {} ({}) [{}]...\n",
        sender_login_id, sending_terminal, date
    );
    write_to_terminal(&terminal, &message);

    alert_sender_terminal();

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        if is_interrupt_or_eof(&line) {
            write_to_terminal(&terminal, "EOT\n");
            exit(0);
        } else if contains_alert_character(&line) {
            write_to_terminal(&terminal, &get_alert_character());
        } else if contains_erase_or_kill_character(&line) {
            process_erase_or_kill(&line);
        } else if contains_printable_or_space(&line) {
            write_to_terminal(&terminal, &format!("{}\n", line));
        } else {
            write_to_terminal(&terminal, &process_non_printable(&line));
        }
    }

    // Add the EOF message before exiting
    write_to_terminal(&terminal, "EOF\n");

    Ok(())
}
