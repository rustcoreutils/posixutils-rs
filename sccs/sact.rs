//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! sact - print current SCCS file-editing activity

use std::fs;
use std::io::{self, BufRead};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::sccsfile::{parse_pfile, paths};

/// sact - print current SCCS file-editing activity
#[derive(Parser)]
#[command(version, about = gettext("sact - print current SCCS file-editing activity"))]
struct Args {
    #[arg(required = true, help = gettext("SCCS files to check (use - for stdin)"))]
    files: Vec<PathBuf>,
}

fn process_sfile(sfile: &Path, show_header: bool) -> io::Result<bool> {
    // Check if p-file exists
    let pfile = paths::pfile_from_sfile(sfile);

    if !pfile.exists() {
        return Ok(false);
    }

    // Read and parse p-file
    let contents = fs::read_to_string(&pfile)?;
    let entries =
        parse_pfile(&contents).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

    if entries.is_empty() {
        return Ok(false);
    }

    // Print header if needed (multiple files)
    if show_header {
        println!("\n{}:", sfile.display());
    }

    // Output format: old_sid new_sid user date time
    for entry in entries {
        println!(
            "{} {} {} {} {}",
            entry.old_sid,
            entry.new_sid,
            entry.user,
            entry.datetime.date_string(),
            entry.datetime.time_string()
        );
    }

    Ok(true)
}

fn process_directory(dir: &Path, show_header: bool, had_error: &mut bool) -> io::Result<bool> {
    let mut found_any = false;

    let entries = fs::read_dir(dir)?;
    for entry in entries.flatten() {
        let path = entry.path();

        // Skip non-SCCS files and unreadable files
        if paths::is_sfile(&path) {
            match process_sfile(&path, show_header) {
                Ok(found) => found_any = found_any || found,
                Err(_) => {
                    // A corrupt p-file within a directory is reported via the
                    // aggregate exit status but does not abort the scan.
                    *had_error = true;
                }
            }
        }
    }

    Ok(found_any)
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    let args = Args::parse();

    let reading_stdin = args.files.len() == 1 && args.files[0].to_string_lossy() == "-";

    // Per POSIX, the "\n%s:\n" pathname header is written when there is more
    // than one named file, or a directory or standard input is named. A lone
    // "-" reads stdin (each line a pathname) and is treated as multiple files,
    // so headers are shown for it.
    let show_header = reading_stdin
        || args.files.len() > 1
        || args.files.first().map(|p| p.is_dir()).unwrap_or(false);

    let mut had_error = false;

    // Check if reading from stdin
    if reading_stdin {
        let stdin = io::stdin();
        for line in stdin.lock().lines() {
            let line = match line {
                Ok(l) => l,
                Err(_) => continue,
            };

            let path = PathBuf::from(line.trim());

            // Skip non-SCCS files and unreadable files
            if !paths::is_sfile(&path) || !path.exists() {
                continue;
            }

            // Silently ignore unreadable / non-SCCS files per spec.
            process_sfile(&path, true).ok();
        }
    } else {
        for file in &args.files {
            if file.is_dir() {
                if process_directory(file, show_header, &mut had_error).is_err() {
                    had_error = true;
                }
            } else if paths::is_sfile(file) && process_sfile(file, show_header).is_err() {
                had_error = true;
            }
            // A named operand that is neither a directory nor a recognizable
            // SCCS file is silently ignored (cssc returns 0 for a nonexistent
            // operand), matching the no-impending-delta success case.
        }
    }

    if had_error {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
