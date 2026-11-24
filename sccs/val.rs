//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! val - validate SCCS files

use std::ffi::{OsStr, OsString};
use std::io::{self, BufRead, Write};
use std::os::unix::ffi::OsStrExt;
use std::path::PathBuf;
use std::process::ExitCode;

use clap::Parser;
use gettextrs::gettext;
use plib::sccsfile::{paths, SccsFile, Sid};

// Exit status bit masks per POSIX
const ERR_MISSING_FILE: u8 = 0x80;
const ERR_UNKNOWN_OPTION: u8 = 0x40;
const ERR_CORRUPTED_FILE: u8 = 0x20;
const ERR_CANNOT_OPEN: u8 = 0x10;
const ERR_SID_INVALID: u8 = 0x08;
const ERR_SID_NOT_FOUND: u8 = 0x04;
const ERR_Y_MISMATCH: u8 = 0x02;
const ERR_M_MISMATCH: u8 = 0x01;

/// val - validate SCCS files
#[derive(Parser)]
#[command(version, about = gettext("val - validate SCCS files"))]
struct Args {
    #[arg(short = 's', help = gettext("Silence diagnostic messages"))]
    silent: bool,

    #[arg(short = 'm', value_name = "NAME", help = gettext("Compare module name with %M% keyword"))]
    module_name: Option<OsString>,

    #[arg(short = 'r', value_name = "SID", help = gettext("Check that SID exists"))]
    sid: Option<String>,

    #[arg(short = 'y', value_name = "TYPE", help = gettext("Compare type with %Y% keyword"))]
    type_name: Option<OsString>,

    #[arg(help = gettext("SCCS files to validate (use - for stdin)"))]
    files: Vec<PathBuf>,
}

/// Validate a single SID string
fn validate_sid(sid_str: &str) -> Result<Sid, u8> {
    // Parse the SID
    let sid: Sid = sid_str.parse().map_err(|_| ERR_SID_INVALID)?;

    // Check for invalid SIDs per POSIX:
    // - 1.0 is invalid (level 0 not allowed)
    // - 1.1.0 is invalid (branch 0 not allowed in branch position)
    if sid.lev == 0 {
        return Err(ERR_SID_INVALID);
    }

    // R.L.B.0 is invalid (sequence 0 not allowed if branch specified)
    if sid.br != 0 && sid.seq == 0 {
        return Err(ERR_SID_INVALID);
    }

    // Partial SIDs (just release number) are ambiguous
    if sid.is_partial() {
        return Err(ERR_SID_INVALID);
    }

    Ok(sid)
}

/// Validate a single SCCS file.
///
/// Any discrepancy strings (the `<unspecified string>` of the POSIX output
/// format) are pushed onto `diags` rather than written directly, so that the
/// caller can format them for either command-line or standard-input mode.
fn validate_file(args: &Args, file_path: &PathBuf, diags: &mut Vec<String>) -> u8 {
    let mut errors: u8 = 0;

    // Check if it's a valid s-file name
    if !paths::is_sfile(file_path) {
        diags.push(gettext("not an SCCS file"));
        return ERR_CANNOT_OPEN;
    }

    // Read the file once: the structure and the checksum are checked on the
    // same bytes.
    let data = match std::fs::read(file_path) {
        Ok(data) => data,
        Err(e) => {
            diags.push(plib::diag::io_error_text(&e));
            return ERR_CANNOT_OPEN;
        }
    };

    // Parse strictly: a line no record accounts for is corruption.
    let sccs = match SccsFile::from_bytes_strict(&data) {
        Ok(s) => s,
        Err(e) => {
            diags.push(e.to_string());
            return ERR_CORRUPTED_FILE;
        }
    };

    // Verify checksum. A header-only file (no body) still has one.
    if sccs.verify_checksum().is_err() {
        diags.push(gettext("corrupted SCCS file (checksum error)"));
        errors |= ERR_CORRUPTED_FILE;
    }

    // Check -r SID if specified
    if let Some(ref sid_str) = args.sid {
        match validate_sid(sid_str) {
            Ok(sid) => {
                // Check if SID exists in the file
                if sccs.find_delta_by_sid(&sid).is_none() {
                    diags.push(format!(
                        "{} {} {}",
                        gettext("SID"),
                        sid,
                        gettext("does not exist")
                    ));
                    errors |= ERR_SID_NOT_FOUND;
                }
            }
            Err(e) => {
                diags.push(format!(
                    "{} {} {}",
                    gettext("SID"),
                    sid_str,
                    gettext("is invalid or ambiguous")
                ));
                errors |= e;
            }
        }
    }

    // Check -m module name if specified
    if let Some(ref expected_name) = args.module_name {
        let actual_name = posixutils_sccs::idkw::module_name_of(&sccs, file_path);

        if actual_name != expected_name.as_bytes() {
            diags.push(gettext("%M%, -m mismatch"));
            errors |= ERR_M_MISMATCH;
        }
    }

    // Check -y type if specified
    if let Some(ref expected_type) = args.type_name {
        let actual_type = sccs.module_type().unwrap_or(b"");

        if actual_type != expected_type.as_bytes() {
            diags.push(gettext("%Y%, -y mismatch"));
            errors |= ERR_Y_MISMATCH;
        }
    }

    errors
}

fn main() -> ExitCode {
    plib::diag::init_locale("val");

    // Parse the command line manually so that an unknown or duplicate
    // keyletter yields the 0x40 exit bit rather than clap's default exit(2).
    let args = match plib::optarg::try_parse::<Args>() {
        Ok(a) => a,
        Err(e) => {
            // --help / --version are not errors; let clap print and exit 0.
            use clap::error::ErrorKind;
            if matches!(e.kind(), ErrorKind::DisplayHelp | ErrorKind::DisplayVersion) {
                plib::optarg::exit(e);
            }
            return ExitCode::from(ERR_UNKNOWN_OPTION);
        }
    };

    // Missing file argument.
    if args.files.is_empty() {
        return ExitCode::from(ERR_MISSING_FILE);
    }

    let mut exit_code: u8 = 0;

    // What is written to standard output: pathnames and input lines are
    // bytes, and go out as they are.
    let mut stdout = io::stdout().lock();
    let mut write_failed = false;
    let mut put = |parts: &[&[u8]]| {
        if !write_failed {
            if let Err(e) = stdout.write_all(&parts.concat()) {
                plib::diag::write_error(&e);
                write_failed = true;
            }
        }
    };

    // Check if reading from stdin
    if args.files.len() == 1 && args.files[0].as_os_str() == "-" {
        // Read command lines from stdin. A line is bytes: the pathnames on
        // it need not be UTF-8. (Such a line used to be skipped unread.)
        let mut stdin = io::stdin().lock();
        let mut line = Vec::new();
        loop {
            line.clear();
            match stdin.read_until(b'\n', &mut line) {
                Ok(0) => break,
                Ok(_) => {}
                // An error (EISDIR, EIO) would recur on every retry: report
                // it once and stop reading.
                Err(e) => {
                    eprintln!("val: -: {}", plib::diag::io_error_text(&e));
                    exit_code |= ERR_CANNOT_OPEN;
                    break;
                }
            }
            let line = line.strip_suffix(b"\n").unwrap_or(&line);

            // Parse the line as arguments.
            // Note: POSIX says no shell expansions are applied.
            let words = line
                .split(u8::is_ascii_whitespace)
                .filter(|word| !word.is_empty())
                .map(OsStr::from_bytes);
            let line_args: Vec<&OsStr> = std::iter::once(OsStr::new("val")).chain(words).collect();
            if line_args.len() == 1 {
                continue;
            }

            // Re-parse args for this line. An unknown or duplicate keyletter
            // contributes the 0x40 bit to this line's aggregate code.
            match Args::try_parse_from(plib::optarg::keep_leading_equals::<Args>(line_args)) {
                Ok(line_parsed) => {
                    for file in &line_parsed.files {
                        let mut diags: Vec<String> = Vec::new();
                        let file_errors = validate_file(&line_parsed, file, &mut diags);
                        if file_errors != 0 {
                            // POSIX format for stdin mode:
                            //   "%s\n\n %s: %s\n", <input>, <pathname>, <string>
                            // i.e. the input line, a blank line, then a
                            // SPACE-indented "pathname: discrepancy".
                            put(&[line, b"\n\n"]);
                            if !line_parsed.silent {
                                for diag in &diags {
                                    let name = file.as_os_str().as_bytes();
                                    put(&[b" ", name, b": ", diag.as_bytes(), b"\n"]);
                                }
                            }
                        }
                        exit_code |= file_errors;
                    }
                }
                Err(_) => {
                    // Unknown or duplicate keyletter on this input line.
                    put(&[line, b"\n\n"]);
                    exit_code |= ERR_UNKNOWN_OPTION;
                }
            }
        }
    } else {
        // Process files from command line
        for file in &args.files {
            let mut diags: Vec<String> = Vec::new();
            let file_errors = validate_file(&args, file, &mut diags);
            if !args.silent {
                // Command-line format: "%s: %s\n", <pathname>, <string>
                for diag in &diags {
                    let name = file.as_os_str().as_bytes();
                    put(&[name, b": ", diag.as_bytes(), b"\n"]);
                }
            }
            exit_code |= file_errors;
        }
    }
    if write_failed {
        exit_code |= ERR_CANNOT_OPEN;
    }

    ExitCode::from(exit_code)
}
