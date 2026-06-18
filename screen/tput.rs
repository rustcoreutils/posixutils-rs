//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::{self, Write};
use std::os::unix::ffi::OsStrExt;
use std::process::ExitCode;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use terminfo::{capability as cap, Database};

// POSIX exit codes for tput
const EXIT_SUCCESS: u8 = 0;
// EXIT_BOOLEAN_FALSE: u8 = 1; // Reserved for Boolean operand not set (not used currently)
const EXIT_USAGE_ERROR: u8 = 2;
const EXIT_NO_TERMINFO: u8 = 3;
const EXIT_INVALID_OPERAND: u8 = 4;
const EXIT_OTHER_ERROR: u8 = 5;

#[derive(Parser)]
#[command(version, about = gettext("tput - change terminal characteristics"))]
struct Args {
    #[arg(short = 'T', long, help = gettext("Indicate the type of terminal"))]
    term: Option<String>,

    #[arg(required = true, help = gettext("Terminal operand(s) to execute"))]
    operands: Vec<String>,
}

// The `if`/`rf` capabilities name a file whose *contents* are the
// initialization/reset sequence; emit the file's bytes, not its name.
// Best-effort: an unreadable file is treated as an unavailable capability.
// Returns true if any bytes were actually written.
fn emit_cap_file(path_bytes: &[u8]) -> bool {
    let path = std::ffi::OsStr::from_bytes(path_bytes);
    match std::fs::read(path) {
        Ok(contents) => {
            let _ = io::stdout().write_all(&contents);
            true
        }
        Err(_) => false,
    }
}

// The `iprog` capability names a program to run to initialize the terminal.
// Best-effort: errors (missing program, non-zero exit) are non-fatal.
fn run_cap_prog(path_bytes: &[u8]) {
    let path = std::ffi::OsStr::from_bytes(path_bytes);
    let _ = std::process::Command::new(path).status();
}

fn tput_init(info: &Database) -> terminfo::Result<()> {
    if let Some(cap) = info.get::<cap::Init1String>() {
        cap.expand().to(io::stdout())?;
    }
    if let Some(cap) = info.get::<cap::Init2String>() {
        cap.expand().to(io::stdout())?;
    }
    if let Some(cap) = info.get::<cap::InitFile>() {
        emit_cap_file(cap.as_ref());
    }
    // (init has no fallback; emit best-effort)
    if let Some(cap) = info.get::<cap::InitProg>() {
        run_cap_prog(cap.as_ref());
    }
    if let Some(cap) = info.get::<cap::Init3String>() {
        cap.expand().to(io::stdout())?;
    }

    Ok(())
}

fn tput_reset(info: &Database) -> terminfo::Result<()> {
    let mut emitted = false;
    if let Some(cap) = info.get::<cap::Reset1String>() {
        cap.expand().to(io::stdout())?;
        emitted = true;
    }
    if let Some(cap) = info.get::<cap::Reset2String>() {
        cap.expand().to(io::stdout())?;
        emitted = true;
    }
    if let Some(cap) = info.get::<cap::ResetFile>() {
        // Only counts as "reset emitted" if the file was actually readable, so
        // an unreadable rf still falls back to the init sequence below.
        emitted |= emit_cap_file(cap.as_ref());
    }
    if let Some(cap) = info.get::<cap::Reset3String>() {
        cap.expand().to(io::stdout())?;
        emitted = true;
    }

    // Historical parity: a terminal with no reset strings falls back to its
    // initialization sequence.
    if !emitted {
        return tput_init(info);
    }

    Ok(())
}

fn tput_clear(info: &Database) -> terminfo::Result<()> {
    if let Some(clear) = info.get::<cap::ClearScreen>() {
        clear.expand().to(io::stdout())?;
    }

    Ok(())
}

/// Check if an operand is a valid tput operand name
fn is_valid_operand(operand: &str) -> bool {
    matches!(operand, "clear" | "init" | "reset")
}

/// Process a single operand
/// Returns true on success, false on failure
fn process_operand(info: &Database, operand: &str) -> Result<(), u8> {
    match operand {
        "clear" => {
            if let Err(_e) = tput_clear(info) {
                return Err(EXIT_OTHER_ERROR);
            }
        }
        "init" => {
            if let Err(_e) = tput_init(info) {
                return Err(EXIT_OTHER_ERROR);
            }
        }
        "reset" => {
            if let Err(_e) = tput_reset(info) {
                return Err(EXIT_OTHER_ERROR);
            }
        }
        _ => {
            eprintln!("{}: {}", gettext("Invalid operand"), operand);
            return Err(EXIT_INVALID_OPERAND);
        }
    }
    Ok(())
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    if textdomain("posixutils-rs").is_err() {
        return ExitCode::from(EXIT_OTHER_ERROR);
    }
    if bind_textdomain_codeset("posixutils-rs", "UTF-8").is_err() {
        return ExitCode::from(EXIT_OTHER_ERROR);
    }

    let args = match Args::try_parse() {
        Ok(a) => a,
        Err(e) => {
            // Clap already prints the error message
            let _ = e.print();
            return ExitCode::from(EXIT_USAGE_ERROR);
        }
    };

    // If every operand is invalid, report exit 4 without needing a terminal
    // (no valid work to do, and exit 4 outranks the no-terminfo exit 3).
    if !args.operands.iter().any(|o| is_valid_operand(o)) {
        for operand in &args.operands {
            eprintln!("{}: {}", gettext("Invalid operand"), operand);
        }
        return ExitCode::from(EXIT_INVALID_OPERAND);
    }

    // Load terminfo database
    let info = match &args.term {
        None => match Database::from_env() {
            Ok(db) => db,
            Err(_) => {
                eprintln!(
                    "{}",
                    gettext("Cannot determine terminal type from TERM environment variable")
                );
                return ExitCode::from(EXIT_NO_TERMINFO);
            }
        },
        Some(termtype) => match Database::from_name(termtype) {
            Ok(db) => db,
            Err(_) => {
                eprintln!("{}: {}", gettext("Unknown terminal type"), termtype);
                return ExitCode::from(EXIT_NO_TERMINFO);
            }
        },
    };

    // Process each operand in order, continuing past failures (POSIX
    // CONSEQUENCES OF ERRORS). A valid operand runs even if a later operand is
    // invalid; the invalid operand is reported and sets exit 4.
    let mut exit_code = EXIT_SUCCESS;

    for operand in &args.operands {
        if let Err(code) = process_operand(&info, operand) {
            exit_code = code;
            // Continue processing remaining operands per POSIX
        }
    }

    ExitCode::from(exit_code)
}
