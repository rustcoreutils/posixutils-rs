//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! rmdel - remove a delta from an SCCS file

use std::fs;
use std::io::{self, BufRead};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::sccsfile::{parse_pfile, paths, DeltaType, SccsFile, Sid};

/// rmdel - remove a delta from an SCCS file
#[derive(Parser)]
#[command(version, about = gettext("rmdel - remove a delta from an SCCS file"))]
struct Args {
    /// SID of delta to remove (required)
    #[arg(short = 'r', value_name = "SID", required = true)]
    sid: String,

    /// SCCS files to process (use - for stdin)
    #[arg(required = true)]
    files: Vec<PathBuf>,
}

fn get_current_user() -> String {
    std::env::var("LOGNAME")
        .or_else(|_| std::env::var("USER"))
        .unwrap_or_else(|_| "unknown".to_string())
}

fn rmdel_file(sfile: &Path, sid: &Sid) -> io::Result<bool> {
    // Check if it's a valid s-file
    if !paths::is_sfile(sfile) {
        eprintln!("{}: not an SCCS file", sfile.display());
        return Ok(false);
    }

    // Parse the SCCS file
    let mut sccs = match SccsFile::from_path(sfile) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{}: {}", sfile.display(), e);
            return Ok(false);
        }
    };

    // Find the delta to remove
    let delta_idx = sccs.header.deltas.iter().position(|d| &d.sid == sid);
    let delta_idx = match delta_idx {
        Some(idx) => idx,
        None => {
            eprintln!("{}: SID {} not found", sfile.display(), sid);
            return Ok(false);
        }
    };

    // Check ownership - user must own the delta
    let current_user = get_current_user();
    if sccs.header.deltas[delta_idx].user != current_user {
        eprintln!("{}: you do not own delta {}", sfile.display(), sid);
        return Ok(false);
    }

    // Check that delta is a leaf (no successors)
    // A delta is a leaf if no other delta has its serial number as predecessor
    let target_serial = sccs.header.deltas[delta_idx].serial;
    let has_successors = sccs.header.deltas.iter().any(|d| {
        // Check if any delta's predecessor serial matches this delta's serial
        d.pred_serial == target_serial && d.serial != target_serial
    });

    if has_successors {
        eprintln!(
            "{}: delta {} is not a leaf delta (has successors)",
            sfile.display(),
            sid
        );
        return Ok(false);
    }

    // Check that delta is not checked out for editing
    let pfile = paths::pfile_from_sfile(sfile);
    if pfile.exists() {
        let contents = fs::read_to_string(&pfile)?;
        if let Ok(entries) = parse_pfile(&contents) {
            for entry in &entries {
                if &entry.old_sid == sid || &entry.new_sid == sid {
                    eprintln!("{}: delta {} is being edited", sfile.display(), sid);
                    return Ok(false);
                }
            }
        }
    }

    // Check delta type - can't remove already removed delta
    if sccs.header.deltas[delta_idx].delta_type == DeltaType::Removed {
        eprintln!("{}: delta {} is already removed", sfile.display(), sid);
        return Ok(false);
    }

    // Mark delta as removed
    sccs.header.deltas[delta_idx].delta_type = DeltaType::Removed;

    // Write the modified file back
    let contents = sccs.to_bytes();

    // Write atomically (to temp file then rename)
    let tmp_path = sfile.with_extension("tmp");
    fs::write(&tmp_path, contents)?;
    fs::rename(&tmp_path, sfile)?;

    Ok(true)
}

fn process_directory(dir: &Path, sid: &Sid) -> io::Result<bool> {
    let mut success = true;

    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if paths::is_sfile(&path) {
                match rmdel_file(&path, sid) {
                    Ok(ok) => success = success && ok,
                    Err(e) => {
                        eprintln!("{}: {}", path.display(), e);
                        success = false;
                    }
                }
            }
        }
    }

    Ok(success)
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    let args = Args::parse();

    // Parse the SID
    let sid: Sid = match args.sid.parse() {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Invalid SID '{}': {}", args.sid, e);
            return ExitCode::FAILURE;
        }
    };

    let mut success = true;

    // Check if reading from stdin
    if args.files.len() == 1 && args.files[0].to_string_lossy() == "-" {
        let stdin = io::stdin();
        for line in stdin.lock().lines() {
            let line = match line {
                Ok(l) => l,
                Err(_) => continue,
            };

            let path = PathBuf::from(line.trim());
            if !path.exists() {
                continue;
            }

            if path.is_dir() {
                match process_directory(&path, &sid) {
                    Ok(ok) => success = success && ok,
                    Err(e) => {
                        eprintln!("{}: {}", path.display(), e);
                        success = false;
                    }
                }
            } else {
                match rmdel_file(&path, &sid) {
                    Ok(ok) => success = success && ok,
                    Err(e) => {
                        eprintln!("{}: {}", path.display(), e);
                        success = false;
                    }
                }
            }
        }
    } else {
        for file in &args.files {
            if file.is_dir() {
                match process_directory(file, &sid) {
                    Ok(ok) => success = success && ok,
                    Err(e) => {
                        eprintln!("{}: {}", file.display(), e);
                        success = false;
                    }
                }
            } else {
                match rmdel_file(file, &sid) {
                    Ok(ok) => success = success && ok,
                    Err(e) => {
                        eprintln!("{}: {}", file.display(), e);
                        success = false;
                    }
                }
            }
        }
    }

    if success {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}
