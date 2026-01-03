//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! delta - make a delta (change) to an SCCS file

use std::collections::HashSet;
use std::fs::{self, File};
use std::io::{self, BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::sccsfile::{
    paths, BodyRecord, DeltaEntry, DeltaStats, DeltaType, PfileEntry, SccsDateTime, SccsFile, Sid,
};

/// delta - make a delta (change) to an SCCS file
#[derive(Parser)]
#[command(version, about = gettext("delta - make a delta (change) to an SCCS file"))]
struct Args {
    #[arg(short = 'r', value_name = "SID", help = gettext("SID of delta to create (if multiple edits pending)"))]
    sid: Option<String>,

    #[arg(short = 'y', value_name = "COMMENT", help = gettext("Comment for delta"))]
    comment: Option<Option<String>>,

    #[arg(short = 'n', help = gettext("Retain g-file after delta"))]
    keep_gfile: bool,

    #[arg(short = 'p', help = gettext("Print diff output"))]
    print_diff: bool,

    #[arg(short = 's', help = gettext("Silent mode (suppress informational messages)"))]
    silent: bool,

    #[arg(required = true, help = gettext("SCCS files to process"))]
    files: Vec<PathBuf>,
}

fn get_username() -> String {
    std::env::var("USER")
        .or_else(|_| std::env::var("LOGNAME"))
        .unwrap_or_else(|_| "unknown".to_string())
}

/// Find the p-file entry for this user
fn find_pfile_entry(sfile_path: &Path, requested_sid: Option<&Sid>) -> io::Result<PfileEntry> {
    let pfile_path = paths::pfile_from_sfile(sfile_path);

    if !pfile_path.exists() {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!(
                "No outstanding get -e (no p-file for {})",
                sfile_path.display()
            ),
        ));
    }

    let contents = fs::read_to_string(&pfile_path)?;
    let entries = plib::sccsfile::parse_pfile(&contents)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;

    let user = get_username();

    // Filter to this user's entries
    let user_entries: Vec<_> = entries.into_iter().filter(|e| e.user == user).collect();

    if user_entries.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!("No outstanding get -e by user {}", user),
        ));
    }

    // If SID specified, find matching entry
    if let Some(sid) = requested_sid {
        for entry in user_entries {
            if &entry.new_sid == sid || &entry.old_sid == sid {
                return Ok(entry);
            }
        }
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!("No edit pending for SID {}", sid),
        ));
    }

    // If only one entry, use it
    if user_entries.len() == 1 {
        return Ok(user_entries.into_iter().next().unwrap());
    }

    // Multiple entries - need -r option
    Err(io::Error::new(
        io::ErrorKind::InvalidInput,
        format!(
            "Multiple edits pending by user {}; use -r to specify SID",
            user
        ),
    ))
}

/// Read g-file content
fn read_gfile(sfile_path: &Path) -> io::Result<Vec<String>> {
    let gfile_path = paths::gfile_from_sfile(sfile_path)
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "Invalid s-file name"))?;

    if !gfile_path.exists() {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!("g-file {} not found", gfile_path.display()),
        ));
    }

    let file = File::open(&gfile_path)?;
    let reader = BufReader::new(file);
    reader.lines().collect()
}

/// Reconstruct the base version (what was gotten with get -e)
fn reconstruct_base(sccs: &SccsFile, base_serial: u16) -> io::Result<Vec<String>> {
    let applied_set = sccs
        .compute_applied_set(base_serial)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;

    Ok(sccs.evaluate_body(&applied_set))
}

/// Simple line-based diff algorithm
/// Returns (unchanged_count, inserted_lines, deleted_lines)
/// and generates the new body records
fn compute_diff(
    base_lines: &[String],
    new_lines: &[String],
    _new_serial: u16,
    print_diff: bool,
) -> (DeltaStats, Vec<DiffOp>) {
    // Use a simple diff algorithm (Myers or similar would be better, but this works)
    let mut ops = Vec::new();
    let mut i = 0; // index into base_lines
    let mut j = 0; // index into new_lines

    let mut inserted = 0u32;
    let mut deleted = 0u32;
    let mut unchanged = 0u32;

    while i < base_lines.len() || j < new_lines.len() {
        if i < base_lines.len() && j < new_lines.len() && base_lines[i] == new_lines[j] {
            // Lines match - unchanged
            ops.push(DiffOp::Keep);
            unchanged += 1;
            i += 1;
            j += 1;
        } else {
            // Look ahead to find matching lines
            let mut found_match = false;

            // Try to find new_lines[j] in remaining base_lines
            for look_ahead in 1..=10.min(base_lines.len() - i) {
                if i + look_ahead < base_lines.len() && base_lines[i + look_ahead] == new_lines[j] {
                    // Delete lines from base up to the match
                    for k in 0..look_ahead {
                        ops.push(DiffOp::Delete);
                        deleted += 1;
                        if print_diff {
                            println!("- {}", base_lines[i + k]);
                        }
                    }
                    i += look_ahead;
                    found_match = true;
                    break;
                }
            }

            if !found_match {
                // Try to find base_lines[i] in remaining new_lines
                let mut found_insert = false;
                for look_ahead in 1..=10.min(new_lines.len() - j) {
                    if j + look_ahead < new_lines.len()
                        && new_lines[j + look_ahead] == base_lines[i]
                    {
                        // Insert lines from new up to the match
                        for k in 0..look_ahead {
                            ops.push(DiffOp::Insert(new_lines[j + k].clone()));
                            inserted += 1;
                            if print_diff {
                                println!("+ {}", new_lines[j + k]);
                            }
                        }
                        j += look_ahead;
                        found_insert = true;
                        break;
                    }
                }

                if !found_insert {
                    if i < base_lines.len() && j < new_lines.len() {
                        // Replace: delete old, insert new
                        ops.push(DiffOp::Delete);
                        deleted += 1;
                        if print_diff {
                            println!("- {}", base_lines[i]);
                        }
                        ops.push(DiffOp::Insert(new_lines[j].clone()));
                        inserted += 1;
                        if print_diff {
                            println!("+ {}", new_lines[j]);
                        }
                        i += 1;
                        j += 1;
                    } else if i < base_lines.len() {
                        // Delete remaining base lines
                        ops.push(DiffOp::Delete);
                        deleted += 1;
                        if print_diff {
                            println!("- {}", base_lines[i]);
                        }
                        i += 1;
                    } else if j < new_lines.len() {
                        // Insert remaining new lines
                        ops.push(DiffOp::Insert(new_lines[j].clone()));
                        inserted += 1;
                        if print_diff {
                            println!("+ {}", new_lines[j]);
                        }
                        j += 1;
                    }
                }
            }
        }
    }

    (DeltaStats::new(inserted, deleted, unchanged), ops)
}

#[derive(Debug)]
enum DiffOp {
    Keep,
    Insert(String),
    Delete,
}

/// Apply diff operations to the SCCS body
fn apply_diff_to_body(
    sccs: &SccsFile,
    base_serial: u16,
    new_serial: u16,
    diff_ops: &[DiffOp],
) -> Vec<BodyRecord> {
    let applied_set = sccs.compute_applied_set(base_serial).unwrap();

    // First, collect all lines that need to be deleted or kept
    let mut delete_set = HashSet::new();
    let mut insert_after: Vec<(usize, Vec<String>)> = Vec::new();

    let mut base_idx = 0;
    let mut inserts_at_pos: Vec<String> = Vec::new();

    for op in diff_ops {
        match op {
            DiffOp::Keep => {
                if !inserts_at_pos.is_empty() {
                    insert_after.push((base_idx, inserts_at_pos.clone()));
                    inserts_at_pos.clear();
                }
                base_idx += 1;
            }
            DiffOp::Delete => {
                delete_set.insert(base_idx);
                base_idx += 1;
            }
            DiffOp::Insert(line) => {
                inserts_at_pos.push(line.clone());
            }
        }
    }
    // Handle trailing inserts
    if !inserts_at_pos.is_empty() {
        insert_after.push((base_idx, inserts_at_pos));
    }

    // Now rebuild the body with new control records
    let mut new_body = Vec::new();
    let mut stack: Vec<(bool, u16)> = Vec::new();
    let mut current_base_line = 0;

    for record in &sccs.body {
        match record {
            BodyRecord::Insert(serial) => {
                stack.push((true, *serial));
                new_body.push(record.clone());
            }
            BodyRecord::Delete(serial) => {
                stack.push((false, *serial));
                new_body.push(record.clone());
            }
            BodyRecord::End(_serial) => {
                stack.pop();
                new_body.push(record.clone());
            }
            BodyRecord::Text(_line) => {
                let visible = is_line_visible(&stack, &applied_set);

                if visible {
                    // Check for inserts before this position
                    for (pos, lines) in &insert_after {
                        if *pos == current_base_line && !lines.is_empty() {
                            new_body.push(BodyRecord::Insert(new_serial));
                            for insert_line in lines {
                                new_body.push(BodyRecord::Text(insert_line.clone()));
                            }
                            new_body.push(BodyRecord::End(new_serial));
                        }
                    }

                    // Check if this line should be deleted
                    if delete_set.contains(&current_base_line) {
                        new_body.push(BodyRecord::Delete(new_serial));
                        new_body.push(record.clone());
                        new_body.push(BodyRecord::End(new_serial));
                    } else {
                        new_body.push(record.clone());
                    }

                    current_base_line += 1;
                } else {
                    new_body.push(record.clone());
                }
            }
        }
    }

    // Handle inserts at the end
    for (pos, lines) in &insert_after {
        if *pos == current_base_line && !lines.is_empty() {
            new_body.push(BodyRecord::Insert(new_serial));
            for line in lines {
                new_body.push(BodyRecord::Text(line.clone()));
            }
            new_body.push(BodyRecord::End(new_serial));
        }
    }

    new_body
}

/// Check if a line should be visible given the current stack state
fn is_line_visible(stack: &[(bool, u16)], applied_set: &HashSet<u16>) -> bool {
    for &(is_insert, serial) in stack {
        if is_insert {
            if !applied_set.contains(&serial) {
                return false;
            }
        } else if applied_set.contains(&serial) {
            return false;
        }
    }
    true
}

/// Remove entry from p-file
fn remove_pfile_entry(sfile_path: &Path, entry_to_remove: &PfileEntry) -> io::Result<()> {
    let pfile_path = paths::pfile_from_sfile(sfile_path);

    if !pfile_path.exists() {
        return Ok(());
    }

    let contents = fs::read_to_string(&pfile_path)?;
    let entries = plib::sccsfile::parse_pfile(&contents)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;

    // Filter out the entry we're removing
    let remaining: Vec<_> = entries
        .into_iter()
        .filter(|e| {
            !(e.old_sid == entry_to_remove.old_sid
                && e.new_sid == entry_to_remove.new_sid
                && e.user == entry_to_remove.user)
        })
        .collect();

    if remaining.is_empty() {
        // Remove p-file entirely
        fs::remove_file(&pfile_path)?;
    } else {
        // Rewrite p-file with remaining entries
        let mut file = File::create(&pfile_path)?;
        for entry in remaining {
            writeln!(file, "{}", entry.to_line())?;
        }
    }

    Ok(())
}

fn process_file(args: &Args, sfile_path: &Path) -> io::Result<bool> {
    // Validate s-file
    if !paths::is_sfile(sfile_path) {
        eprintln!(
            "delta: {}: {}",
            sfile_path.display(),
            gettext("not an SCCS file")
        );
        return Ok(false);
    }

    // Find p-file entry
    let requested_sid: Option<Sid> = args
        .sid
        .as_ref()
        .map(|s| s.parse())
        .transpose()
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "Invalid SID"))?;

    let pfile_entry = find_pfile_entry(sfile_path, requested_sid.as_ref())?;

    // Parse SCCS file
    let mut sccs = SccsFile::from_path(sfile_path)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;

    // Find the base delta
    let base_delta = sccs
        .find_delta_by_sid(&pfile_entry.old_sid)
        .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Base SID not found"))?;
    let base_serial = base_delta.serial;

    // Read g-file
    let new_lines = read_gfile(sfile_path)?;

    // Reconstruct base version
    let base_lines = reconstruct_base(&sccs, base_serial)?;

    // Compute diff
    let (stats, diff_ops) = compute_diff(
        &base_lines,
        &new_lines,
        sccs.max_serial() + 1,
        args.print_diff,
    );

    // Get comment
    let comment = match &args.comment {
        Some(Some(c)) => c.clone(),
        Some(None) => String::new(), // Empty comment
        None => String::new(),       // Default empty for now (could prompt)
    };

    // Create new delta entry
    let new_serial = sccs.max_serial() + 1;
    let new_delta = DeltaEntry {
        delta_type: DeltaType::Normal,
        sid: pfile_entry.new_sid,
        datetime: SccsDateTime::now(),
        user: get_username(),
        serial: new_serial,
        pred_serial: base_serial,
        stats,
        included: Vec::new(),
        excluded: Vec::new(),
        ignored: Vec::new(),
        mr_numbers: Vec::new(),
        comments: if comment.is_empty() {
            Vec::new()
        } else {
            vec![comment]
        },
    };

    // Print info
    if !args.silent {
        println!("{}", pfile_entry.new_sid);
        println!("{} inserted", new_delta.stats.inserted);
        println!("{} deleted", new_delta.stats.deleted);
        println!("{} unchanged", new_delta.stats.unchanged);
    }

    // Apply diff to body
    let new_body = apply_diff_to_body(&sccs, base_serial, new_serial, &diff_ops);
    sccs.body = new_body;

    // Add new delta to header (at the beginning - deltas are stored newest first)
    sccs.header.deltas.insert(0, new_delta);

    // Write atomically via x-file
    let x_file = paths::xfile_from_sfile(sfile_path);
    let original_perms = fs::metadata(sfile_path)?.permissions();

    let serialized = sccs.to_bytes();
    fs::write(&x_file, &serialized)?;
    fs::set_permissions(&x_file, original_perms)?;
    fs::rename(&x_file, sfile_path)?;

    // Remove p-file entry
    remove_pfile_entry(sfile_path, &pfile_entry)?;

    // Remove g-file (unless -n)
    if !args.keep_gfile {
        if let Some(gfile_path) = paths::gfile_from_sfile(sfile_path) {
            let _ = fs::remove_file(gfile_path);
        }
    }

    Ok(true)
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    let args = Args::parse();

    let mut exit_code = ExitCode::SUCCESS;

    for file_path in &args.files {
        match process_file(&args, file_path) {
            Ok(true) => {}
            Ok(false) => exit_code = ExitCode::FAILURE,
            Err(e) => {
                eprintln!("delta: {}: {}", file_path.display(), e);
                exit_code = ExitCode::FAILURE;
            }
        }
    }

    exit_code
}
