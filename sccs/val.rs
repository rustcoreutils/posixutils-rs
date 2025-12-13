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
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use plib::sccsfile::{paths, SccsFile, Sid};

// Exit status bit masks per POSIX
const ERR_MISSING_FILE: u8 = 0x80;
const ERR_CORRUPTED_FILE: u8 = 0x20;
const ERR_CANNOT_OPEN: u8 = 0x10;
const ERR_SID_INVALID: u8 = 0x08;
const ERR_SID_NOT_FOUND: u8 = 0x04;
const ERR_Y_MISMATCH: u8 = 0x02;
const ERR_M_MISMATCH: u8 = 0x01;

/// val - validate SCCS files
#[derive(Parser)]
#[command(version, about = "val - validate SCCS files")]
struct Args {
    /// Silence diagnostic messages
    #[arg(short = 's')]
    silent: bool,

    /// Compare module name with %M% keyword
    #[arg(short = 'm', value_name = "NAME")]
    module_name: Option<String>,

    /// Check that SID exists
    #[arg(short = 'r', value_name = "SID")]
    sid: Option<String>,

    /// Compare type with %Y% keyword
    #[arg(short = 'y', value_name = "TYPE")]
    type_name: Option<String>,

    /// SCCS files to validate (use - for stdin)
    #[arg(required = true)]
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

/// Validate a single SCCS file
fn validate_file(args: &Args, file_path: &PathBuf) -> u8 {
    let mut errors: u8 = 0;

    // Check if it's a valid s-file name
    if !paths::is_sfile(file_path) {
        if !args.silent {
            println!("{}: not an SCCS file", file_path.display());
        }
        return ERR_CANNOT_OPEN;
    }

    // Try to parse the SCCS file
    let sccs = match SccsFile::from_path(file_path) {
        Ok(s) => s,
        Err(e) => {
            if !args.silent {
                println!("{}: {}", file_path.display(), e);
            }
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

        if content_start < data.len() {
            let computed = plib::sccsfile::compute_checksum(&data[content_start..]);
            if computed != sccs.header.checksum {
                if !args.silent {
                    println!(
                        "{}: corrupted SCCS file (checksum error)",
                        file_path.display()
                    );
                }
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
                    if !args.silent {
                        println!("{}: SID {} does not exist", file_path.display(), sid);
                    }
                    errors |= ERR_SID_NOT_FOUND;
                }
            }
            Err(e) => {
                if !args.silent {
                    println!(
                        "{}: SID {} is invalid or ambiguous",
                        file_path.display(),
                        sid_str
                    );
                }
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
            if !args.silent {
                println!("{}: %M%, -m mismatch", file_path.display());
            }
            errors |= ERR_M_MISMATCH;
        }
    }

    // Check -y type if specified
    if let Some(ref expected_type) = args.type_name {
        let actual_type = sccs.module_type().unwrap_or("");

        if actual_type != expected_type {
            if !args.silent {
                println!("{}: %Y%, -y mismatch", file_path.display());
            }
            errors |= ERR_Y_MISMATCH;
        }
    }

    errors
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    let args = Args::parse();

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

            // Parse the line as arguments
            // Note: POSIX says no shell expansions are applied
            let mut line_args = vec!["val".to_string()];
            line_args.extend(line.split_whitespace().map(String::from));

            // Re-parse args for this line
            match Args::try_parse_from(&line_args) {
                Ok(line_parsed) => {
                    for file in &line_parsed.files {
                        let file_errors = validate_file(&line_parsed, file);
                        if file_errors != 0 {
                            println!("{}", line);
                            println!();
                        }
                        exit_code |= file_errors;
                    }
                }
                Err(_) => {
                    println!("{}", line);
                    println!();
                    // Unknown option would be 0x40, but clap handles this
                }
            }
        }
    } else {
        // Process files from command line
        for file in &args.files {
            exit_code |= validate_file(&args, file);
        }
    }

    ExitCode::from(exit_code)
}
