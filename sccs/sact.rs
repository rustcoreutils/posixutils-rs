//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! sact - print current SCCS file-editing activity

use std::io;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::sccsfile::paths;
use posixutils_sccs::{diag, operands, pfile};

/// sact - print current SCCS file-editing activity
#[derive(Parser)]
#[command(version, about = gettext("sact - print current SCCS file-editing activity"))]
struct Args {
    #[arg(required = true, help = gettext("SCCS files to check (use - for stdin)"))]
    files: Vec<PathBuf>,
}

fn process_sfile(sfile: &Path, show_header: bool) -> io::Result<bool> {
    // A named operand that does not exist is an error (EXIT STATUS 113805,
    // ">0 An error occurred"), not a quiet "no impending deltas". Without this
    // check `sact s.typo` was byte-for-byte indistinguishable from `sact` on a
    // real file with nothing checked out: no output, no diagnostic, exit 0.
    if !sfile.exists() {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            gettext("no such SCCS file"),
        ));
    }

    let entries = pfile::read(sfile)?;

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

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    let args = Args::parse();
    let files = operands::expand(&args.files);

    // Per POSIX, the "\n%s:\n" pathname header is written when there is more
    // than one named file, or a directory or standard input is named.
    let show_header = operands::wants_banner(&args.files, &files);

    let mut had_error = false;

    for path in &files {
        if !paths::is_sfile(path) {
            diag::error_path("sact", path, &gettext("not an SCCS file"));
            had_error = true;
            continue;
        }
        if let Err(e) = process_sfile(path, show_header) {
            // STDERR is "used only for optional informative messages
            // concerning SCCS files with no impending deltas, and for
            // diagnostic messages" (113796-113797) — so say what went wrong
            // rather than failing silently.
            diag::error_path("sact", path, &e.to_string());
            had_error = true;
        }
    }

    if had_error {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
