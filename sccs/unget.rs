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
use plib::sccsfile::{parse_pfile, paths, PfileEntry, Sid};

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

fn get_current_user() -> String {
    plib::sccsfile::real_login_name()
}

fn unget_file(sfile: &Path, args: &Args, show_header: bool) -> io::Result<bool> {
    // Check if it's a valid s-file
    if !paths::is_sfile(sfile) {
        eprintln!("{}: {}", sfile.display(), gettext("not an SCCS file"));
        return Ok(false);
    }

    // Check if p-file exists
    let pfile = paths::pfile_from_sfile(sfile);
    if !pfile.exists() {
        eprintln!(
            "{}: {}",
            sfile.display(),
            gettext("no outstanding delta for current user")
        );
        return Ok(false);
    }

    // Parse p-file
    let contents = fs::read_to_string(&pfile)?;
    let mut entries =
        parse_pfile(&contents).map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

    if entries.is_empty() {
        eprintln!(
            "{}: {}",
            sfile.display(),
            gettext("no outstanding delta for current user")
        );
        return Ok(false);
    }

    let current_user = get_current_user();

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
            if let Some(ref sid) = target_sid {
                eprintln!(
                    "{}: {} {} {} {}",
                    sfile.display(),
                    gettext("SID"),
                    sid,
                    gettext("not found for user"),
                    current_user
                );
            } else {
                eprintln!(
                    "{}: {} {}",
                    sfile.display(),
                    gettext("no outstanding delta for user"),
                    current_user
                );
            }
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

    // Update or remove p-file
    if entries.is_empty() {
        fs::remove_file(&pfile)?;
    } else {
        // Rewrite p-file with remaining entries
        let new_contents = entries
            .iter()
            .map(format_pfile_entry)
            .collect::<Vec<_>>()
            .join("");
        fs::write(&pfile, new_contents)?;
    }

    Ok(true)
}

fn format_pfile_entry(entry: &PfileEntry) -> String {
    let mut line = format!(
        "{} {} {} {} {}",
        entry.old_sid,
        entry.new_sid,
        entry.user,
        entry.datetime.date_string(),
        entry.datetime.time_string()
    );

    // Add optional include/exclude fields if present
    if let Some(ref included) = entry.included {
        line.push_str(&format!(" -i{}", included));
    }
    if let Some(ref excluded) = entry.excluded {
        line.push_str(&format!(" -x{}", excluded));
    }

    line.push('\n');
    line
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    let args = Args::parse();

    let mut success = true;

    // A pathname header precedes each SID when more than one file is named, or a
    // directory or standard input is named.
    let is_stdin = args.files.len() == 1 && args.files[0].as_os_str() == "-";
    let has_dir = args.files.iter().any(|f| f.is_dir());
    let files = paths::expand_operands(&args.files);
    let show_header = is_stdin || has_dir || files.len() > 1;

    for file in &files {
        match unget_file(file, &args, show_header) {
            Ok(ok) => success = success && ok,
            Err(e) => {
                eprintln!("unget: {}: {}", file.display(), e);
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
