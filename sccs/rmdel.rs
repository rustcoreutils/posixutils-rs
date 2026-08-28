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
use std::io;
use std::os::unix::fs::MetadataExt;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::sccsfile::{paths, DeltaType, SccsFile, Sid};
use posixutils_sccs::{diag, operands, pfile, sfio, zlock};

/// rmdel - remove a delta from an SCCS file
#[derive(Parser)]
#[command(version, about = gettext("rmdel - remove a delta from an SCCS file"))]
struct Args {
    #[arg(short = 'r', value_name = "SID", required = true, help = gettext("SID of delta to remove (required)"))]
    sid: String,

    #[arg(required = true, help = gettext("SCCS files to process (use - for stdin)"))]
    files: Vec<PathBuf>,
}

fn rmdel_file(sfile: &Path, sid: &Sid) -> io::Result<bool> {
    // Check if it's a valid s-file
    if !paths::is_sfile(sfile) {
        diag::error_path("rmdel", sfile, &gettext("not an SCCS file"));
        return Ok(false);
    }

    // Acquire the per-command z-file lock around the read-modify-write. If
    // another SCCS command already holds it, report and skip.
    let _zlock = match zlock::acquire(sfile) {
        Ok(z) => z,
        Err(e) if zlock::is_held(&e) => {
            diag::error_path("rmdel", sfile, &gettext("being edited"));
            return Ok(false);
        }
        Err(e) => return Err(e),
    };

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
            eprintln!(
                "{}: {} {} {}",
                sfile.display(),
                gettext("SID"),
                sid,
                gettext("not found")
            );
            return Ok(false);
        }
    };

    // Check ownership.  Per POSIX, removal of a delta is restricted to:
    //   1. the user who made the delta,
    //   2. the owner of the SCCS file, or
    //   3. the owner of the directory containing the SCCS file.
    let current_user = posixutils_sccs::username();
    let current_uid = unsafe { libc::getuid() };

    let is_delta_author = sccs.header.deltas[delta_idx].user == current_user;

    let owns_sfile = fs::metadata(sfile)
        .map(|m| m.uid() == current_uid)
        .unwrap_or(false);

    let dir = sfile
        .parent()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    let owns_dir = fs::metadata(dir)
        .map(|m| m.uid() == current_uid)
        .unwrap_or(false);

    if !(is_delta_author || owns_sfile || owns_dir) {
        eprintln!(
            "{}: {} {}",
            sfile.display(),
            gettext("you are not permitted to remove delta"),
            sid
        );
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
            "{}: {} {} {}",
            sfile.display(),
            gettext("delta"),
            sid,
            gettext("is not a leaf delta (has successors)")
        );
        return Ok(false);
    }

    // Check that delta is not checked out for editing
    for entry in pfile::read(sfile)? {
        if entry.old_sid == *sid || entry.new_sid == *sid {
            diag::error_path(
                "rmdel",
                sfile,
                &format!(
                    "{} {} {}",
                    gettext("delta"),
                    sid,
                    gettext("is being edited")
                ),
            );
            return Ok(false);
        }
    }

    // Check delta type - can't remove already removed delta
    if sccs.header.deltas[delta_idx].delta_type == DeltaType::Removed {
        eprintln!(
            "{}: {} {} {}",
            sfile.display(),
            gettext("delta"),
            sid,
            gettext("is already removed")
        );
        return Ok(false);
    }

    // Preserve the s-file's own mode across the rewrite.
    let perms = sfio::sfile_perms(sfile);

    // Mark delta as removed and reweave the body so its footprint is undone.
    let serial = sccs.header.deltas[delta_idx].serial;
    sccs.remove_delta(serial);

    sfio::write_xfile_atomic(
        sfile,
        &paths::xfile_from_sfile(sfile),
        &sccs.to_bytes(),
        perms,
    )?;

    Ok(true)
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    zlock::install_cleanup();

    let args = Args::parse();

    // Parse the SID
    let sid: Sid = match args.sid.parse() {
        Ok(s) => s,
        Err(e) => {
            diag::error(
                "rmdel",
                &format!("{} '{}': {}", gettext("Invalid SID"), args.sid, e),
            );
            return ExitCode::FAILURE;
        }
    };

    let mut success = true;
    for path in operands::expand(&args.files) {
        match rmdel_file(&path, &sid) {
            Ok(ok) => success = success && ok,
            Err(e) => {
                diag::error_path("rmdel", &path, &e.to_string());
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
