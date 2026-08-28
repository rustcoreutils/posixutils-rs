//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! unget - undo a previous get of an SCCS file

use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::sccsfile::{paths, Sid};
use posixutils_sccs::{diag, operands, pfile};

/// unget - undo a previous get of an SCCS file
#[derive(Parser)]
#[command(version, about = gettext("unget - undo a previous get of an SCCS file"))]
struct Args {
    #[arg(short = 'r', value_name = "SID", help = gettext("Specify the SID to unget (when user has multiple pending edits)"))]
    sid: Option<String>,

    #[arg(short = 's', help = gettext("Silent mode (suppress output)"))]
    silent: bool,

    #[arg(short = 'n', help = gettext("Do not remove the g-file (keep edited file)"))]
    keep_gfile: bool,

    #[arg(required = true, help = gettext("SCCS files to process (use - for stdin)"))]
    files: Vec<PathBuf>,
}

fn unget_file(sfile: &Path, args: &Args, show_header: bool) -> io::Result<bool> {
    // Check if it's a valid s-file
    if !paths::is_sfile(sfile) {
        diag::error_path("unget", sfile, &gettext("not an SCCS file"));
        return Ok(false);
    }

    let mut entries = pfile::read(sfile)?;
    if entries.is_empty() {
        diag::error_path(
            "unget",
            sfile,
            &gettext("no outstanding delta for current user"),
        );
        return Ok(false);
    }

    let current_user = posixutils_sccs::username();

    // Find the entry to remove
    let target_sid: Option<Sid> = if let Some(ref sid_str) = args.sid {
        Some(
            sid_str
                .parse()
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?,
        )
    } else {
        None
    };

    // Find matching entry for current user
    let entry_idx = entries.iter().position(|e| {
        if e.user != current_user {
            return false;
        }
        if let Some(ref target) = target_sid {
            // Must match the new_sid
            &e.new_sid == target
        } else {
            true // First entry for this user
        }
    });

    let entry_idx = match entry_idx {
        Some(idx) => idx,
        None => {
            let msg = match target_sid {
                Some(ref sid) => format!(
                    "{} {} {} {}",
                    gettext("SID"),
                    sid,
                    gettext("not found for user"),
                    current_user
                ),
                None => format!(
                    "{} {}",
                    gettext("no outstanding delta for user"),
                    current_user
                ),
            };
            diag::error_path("unget", sfile, &msg);
            return Ok(false);
        }
    };

    let entry = entries.remove(entry_idx);

    // Output the SID being ungot (unless silent). When more than one file (or a
    // directory or standard input) is named, each SID is preceded by a
    // "\n%s:\n" pathname header.
    if !args.silent {
        if show_header {
            println!("\n{}:", sfile.display());
        }
        println!("{}", entry.new_sid);
    }

    // Remove g-file unless -n specified
    if !args.keep_gfile {
        let gfile = paths::gfile_from_sfile(sfile);
        if let Some(gfile) = gfile {
            if gfile.exists() {
                fs::remove_file(&gfile)?;
            }
        }
    }

    // Update or remove the p-file. An empty p-file and an absent one are
    // different states, so the writer unlinks rather than truncating.
    pfile::write(sfile, &entries)?;

    Ok(true)
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    let args = Args::parse();

    let mut success = true;

    // A pathname header precedes each SID when more than one file is named, or
    // a directory or standard input is named.
    let files = operands::expand(&args.files);
    let show_header = operands::wants_banner(&args.files, &files);

    for file in &files {
        match unget_file(file, &args, show_header) {
            Ok(ok) => success = success && ok,
            Err(e) => {
                diag::error_path("unget", file, &e.to_string());
                success = false;
            }
        }
    }

    if success {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
