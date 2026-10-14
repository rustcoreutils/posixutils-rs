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
use gettextrs::gettext;
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
    #[arg(short = 'T', long, allow_hyphen_values = true, help = gettext("Indicate the type of terminal"))]
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

/// The length of the terminfo padding specification at the start of `s`,
/// if there is one: `$<`, a delay in milliseconds that begins with a digit
/// or `.` (digits, then optionally `.` and more digits), any number of `*`
/// (proportional to the lines affected) and `/` (mandatory) flags, and `>`.
fn padding_len(s: &[u8]) -> Option<usize> {
    let rest = s.strip_prefix(b"$<")?;
    if !matches!(rest.first(), Some(b'0'..=b'9' | b'.')) {
        return None;
    }
    let mut i = rest.iter().take_while(|b| b.is_ascii_digit()).count();
    if rest.get(i) == Some(&b'.') {
        i += 1;
        i += rest[i..].iter().take_while(|b| b.is_ascii_digit()).count();
    }
    i += rest[i..]
        .iter()
        .take_while(|&&b| b == b'*' || b == b'/')
        .count();
    (rest.get(i) == Some(&b'>')).then_some(2 + i + 1)
}

/// Remove the padding specifications from an expanded capability.
///
/// Padding is a delay the terminal needs, which tputs(3) provides by sending
/// pad characters at the line's speed, or by waiting, never as the text of
/// the specification. tput's output is commonly captured or redirected, where
/// there is no line speed to pad for, so like ncurses' tput it writes none.
/// Text that only looks like the start of one is ordinary output.
fn strip_padding(expanded: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(expanded.len());
    let mut i = 0;
    while i < expanded.len() {
        match padding_len(&expanded[i..]) {
            Some(len) => i += len,
            None => {
                out.push(expanded[i]);
                i += 1;
            }
        }
    }
    out
}

/// Write an expanded capability to standard output, without its padding.
fn put(expanded: Vec<u8>) -> io::Result<()> {
    io::stdout().write_all(&strip_padding(&expanded))
}

fn tput_init(info: &Database) -> terminfo::Result<()> {
    if let Some(cap) = info.get::<cap::Init1String>() {
        put(cap.expand().to_vec()?)?;
    }
    if let Some(cap) = info.get::<cap::Init2String>() {
        put(cap.expand().to_vec()?)?;
    }
    if let Some(cap) = info.get::<cap::InitFile>() {
        emit_cap_file(cap.as_ref());
    }
    // (init has no fallback; emit best-effort)
    if let Some(cap) = info.get::<cap::InitProg>() {
        run_cap_prog(cap.as_ref());
    }
    if let Some(cap) = info.get::<cap::Init3String>() {
        put(cap.expand().to_vec()?)?;
    }

    Ok(())
}

fn tput_reset(info: &Database) -> terminfo::Result<()> {
    let mut emitted = false;
    if let Some(cap) = info.get::<cap::Reset1String>() {
        put(cap.expand().to_vec()?)?;
        emitted = true;
    }
    if let Some(cap) = info.get::<cap::Reset2String>() {
        put(cap.expand().to_vec()?)?;
        emitted = true;
    }
    if let Some(cap) = info.get::<cap::ResetFile>() {
        // Only counts as "reset emitted" if the file was actually readable, so
        // an unreadable rf still falls back to the init sequence below.
        emitted |= emit_cap_file(cap.as_ref());
    }
    if let Some(cap) = info.get::<cap::Reset3String>() {
        put(cap.expand().to_vec()?)?;
        emitted = true;
    }

    // Historical parity: a terminal with no reset strings falls back to its
    // initialization sequence.
    if !emitted {
        return tput_init(info);
    }

    Ok(())
}

/// Write the clear-screen sequence, then, as ncurses' tput and clear do, the
/// user-defined `E3` capability if the terminal has one: it clears the
/// scrollback, which `clear` alone leaves behind on terminals such as xterm
/// and the Linux console. `E3` takes no parameters and is written as stored.
fn tput_clear(info: &Database) -> terminfo::Result<()> {
    if let Some(clear) = info.get::<cap::ClearScreen>() {
        put(clear.expand().to_vec()?)?;
    }
    if let Some(terminfo::Value::String(e3)) = info.raw("E3") {
        put(e3.clone())?;
    }

    Ok(())
}

/// Check if an operand is a valid tput operand name
fn is_valid_operand(operand: &str) -> bool {
    matches!(operand, "clear" | "init" | "reset")
}

/// How processing an operand failed.
enum Failure {
    /// Exit with this status; any diagnostic has been written.
    Status(u8),
    /// Writing standard output failed, which every later operand would too.
    Write(io::Error),
}

/// Map a capability's result to the operand's: a failed write is a write
/// error, any other failure is silent with the "other error" status.
fn cap_result(result: terminfo::Result<()>) -> Result<(), Failure> {
    match result {
        Ok(()) => Ok(()),
        Err(terminfo::Error::Io(e)) => Err(Failure::Write(e)),
        Err(_) => Err(Failure::Status(EXIT_OTHER_ERROR)),
    }
}

/// Process a single operand
fn process_operand(info: &Database, operand: &str) -> Result<(), Failure> {
    match operand {
        "clear" => cap_result(tput_clear(info)),
        "init" => cap_result(tput_init(info)),
        "reset" => cap_result(tput_reset(info)),
        _ => {
            eprintln!("{}: {}", gettext("Invalid operand"), operand);
            Err(Failure::Status(EXIT_INVALID_OPERAND))
        }
    }
}

/// Report a failed write to standard output.
fn report_write_error(e: &io::Error) {
    plib::diag::error(&format!(
        "{}: {}",
        gettext("write error"),
        plib::diag::io_error_text(e)
    ));
}

fn main() -> ExitCode {
    plib::diag::init_locale("tput");

    let args = match plib::optarg::try_parse::<Args>() {
        Ok(a) => a,
        // Help and version are written to standard output with status 0.
        Err(e) if !e.use_stderr() => plib::optarg::exit(e),
        Err(e) => {
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
        match process_operand(&info, operand) {
            Ok(()) => {}
            // Continue processing remaining operands per POSIX
            Err(Failure::Status(code)) => exit_code = code,
            Err(Failure::Write(e)) => {
                report_write_error(&e);
                return ExitCode::from(EXIT_OTHER_ERROR);
            }
        }
    }

    // The sequences end without a <newline>, so they are still in standard
    // output's line buffer, where the flush at exit would lose a write error.
    if let Err(e) = io::stdout().flush() {
        report_write_error(&e);
        exit_code = exit_code.max(EXIT_OTHER_ERROR);
    }

    ExitCode::from(exit_code)
}
