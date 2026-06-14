//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! val - validate SCCS files

use std::io::{self, BufRead};
use std::path::PathBuf;
use std::process::ExitCode;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
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
    module_name: Option<String>,

    #[arg(short = 'r', value_name = "SID", help = gettext("Check that SID exists"))]
    sid: Option<String>,

    #[arg(short = 'y', value_name = "TYPE", help = gettext("Compare type with %Y% keyword"))]
    type_name: Option<String>,

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

    // Try to parse the SCCS file
    let sccs = match SccsFile::from_path(file_path) {
        Ok(s) => s,
        Err(e) => {
            diags.push(e.to_string());
            // Determine if it's "cannot open" or "corrupted"
            if file_path.exists() {
                return ERR_CORRUPTED_FILE;
            } else {
                return ERR_CANNOT_OPEN;
            }
        }
    };

    // Verify checksum
    if let Ok(data) = std::fs::read(file_path) {
        // Find end of first line
        let newline_pos = data.iter().position(|&b| b == b'\n').unwrap_or(data.len());
        let content_start = newline_pos + 1;

        // A header-only file (no body) still has a checksum to verify.
        if content_start <= data.len() {
            let body = data.get(content_start..).unwrap_or(&[]);
            let computed = plib::sccsfile::compute_checksum(body);
            if computed != sccs.header.checksum {
                diags.push(gettext("corrupted SCCS file (checksum error)"));
                errors |= ERR_CORRUPTED_FILE;
            }
        }
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
        let actual_name = sccs
            .module_name()
            .map(|s| s.to_string())
            .unwrap_or_else(|| {
                paths::module_name(file_path).unwrap_or_else(|| "unknown".to_string())
            });

        if &actual_name != expected_name {
            diags.push(gettext("%M%, -m mismatch"));
            errors |= ERR_M_MISMATCH;
        }
    }

    // Check -y type if specified
    if let Some(ref expected_type) = args.type_name {
        let actual_type = sccs.module_type().unwrap_or("");

        if actual_type != expected_type {
            diags.push(gettext("%Y%, -y mismatch"));
            errors |= ERR_Y_MISMATCH;
        }
    }

    errors
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    // Parse the command line manually so that an unknown or duplicate
    // keyletter yields the 0x40 exit bit rather than clap's default exit(2).
    let args = match Args::try_parse_from(std::env::args_os()) {
        Ok(a) => a,
        Err(e) => {
            // --help / --version are not errors; let clap print and exit 0.
            use clap::error::ErrorKind;
            if matches!(e.kind(), ErrorKind::DisplayHelp | ErrorKind::DisplayVersion) {
                e.exit();
            }
            return ExitCode::from(ERR_UNKNOWN_OPTION);
        }
    };

    // Missing file argument.
    if args.files.is_empty() {
        return ExitCode::from(ERR_MISSING_FILE);
    }

    let mut exit_code: u8 = 0;

    // Check if reading from stdin
    if args.files.len() == 1 && args.files[0].to_string_lossy() == "-" {
        // Read command lines from stdin
        let stdin = io::stdin();
        for line in stdin.lock().lines() {
            let line = match line {
                Ok(l) => l,
                Err(_) => continue,
            };

            if line.trim().is_empty() {
                continue;
            }

            // Parse the line as arguments.
            // Note: POSIX says no shell expansions are applied.
            let mut line_args = vec!["val".to_string()];
            line_args.extend(line.split_whitespace().map(String::from));

            // Re-parse args for this line. An unknown or duplicate keyletter
            // contributes the 0x40 bit to this line's aggregate code.
            match Args::try_parse_from(&line_args) {
                Ok(line_parsed) => {
                    for file in &line_parsed.files {
                        let mut diags: Vec<String> = Vec::new();
                        let file_errors = validate_file(&line_parsed, file, &mut diags);
                        if file_errors != 0 {
                            // POSIX format for stdin mode:
                            //   "%s\n\n %s: %s\n", <input>, <pathname>, <string>
                            // i.e. the input line, a blank line, then a
                            // SPACE-indented "pathname: discrepancy".
                            print!("{}\n\n", line);
                            if !line_parsed.silent {
                                for diag in &diags {
                                    println!(" {}: {}", file.display(), diag);
                                }
                            }
                        }
                        exit_code |= file_errors;
                    }
                }
                Err(_) => {
                    // Unknown or duplicate keyletter on this input line.
                    print!("{}\n\n", line);
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
                    println!("{}: {}", file.display(), diag);
                }
            }
            exit_code |= file_errors;
        }
    }

    ExitCode::from(exit_code)
}
