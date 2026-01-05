//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! get - get a version of an SCCS file

use std::collections::HashSet;
use std::fs::{self, File, OpenOptions};
use std::io::{self, Write};
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};
use plib::sccsfile::{DeltaEntry, PfileEntry, SccsDateTime, SccsFile, SccsFlag, Sid, paths};

/// get - get a version of an SCCS file
#[derive(Parser)]
#[command(version, about = gettext("get - get a version of an SCCS file"))]
struct Args {
    #[arg(short = 'r', value_name = "SID", help = gettext("SID to retrieve"))]
    sid: Option<String>,

    #[arg(short = 'e', help = gettext("Get for editing (create p-file)"))]
    edit: bool,

    #[arg(short = 'b', help = gettext("Create branch (with -e)"))]
    branch: bool,

    #[arg(short = 'k', help = gettext("Suppress keyword expansion"))]
    no_keywords: bool,

    #[arg(short = 'p', help = gettext("Write to stdout instead of g-file"))]
    to_stdout: bool,

    #[arg(short = 's', help = gettext("Silent mode (suppress informational messages)"))]
    silent: bool,

    #[arg(short = 'g', help = gettext("Suppress retrieval, just verify SID exists"))]
    no_get: bool,

    #[arg(short = 'm', help = gettext("Prefix each line with SID of inserting delta"))]
    show_sid: bool,

    #[arg(short = 'n', help = gettext("Prefix each line with module name"))]
    show_module: bool,

    #[arg(required = true, help = gettext("SCCS files to process"))]
    files: Vec<PathBuf>,
}

fn get_username() -> String {
    std::env::var("USER")
        .or_else(|_| std::env::var("LOGNAME"))
        .unwrap_or_else(|_| "unknown".to_string())
}

/// Expand SCCS keywords in a line
fn expand_keywords(
    line: &str,
    sccs_file: &SccsFile,
    delta: &DeltaEntry,
    sfile_path: &Path,
    line_number: usize,
) -> String {
    let mut result = line.to_string();

    // Get module name (from 'm' flag or filename)
    let module_name = sccs_file
        .module_name()
        .map(|s| s.to_string())
        .unwrap_or_else(|| paths::module_name(sfile_path).unwrap_or_else(|| "unknown".to_string()));

    // Get module type from 't' flag
    let module_type = sccs_file.module_type().unwrap_or("");

    // Get Q flag value
    let q_value = sccs_file
        .header
        .flags
        .iter()
        .find_map(|f| {
            if let SccsFlag::QText(s) = f {
                Some(s.as_str())
            } else {
                None
            }
        })
        .unwrap_or("");

    // Current date/time
    let now = SccsDateTime::now();

    // SCCS filename
    let sccs_filename = sfile_path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("s.unknown");

    // Absolute path
    let abs_path = std::fs::canonicalize(sfile_path)
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|_| sfile_path.to_string_lossy().to_string());

    // %Z% = @(#)
    result = result.replace("%Z%", "@(#)");

    // %M% = module name
    result = result.replace("%M%", &module_name);

    // %I% = SID (full)
    result = result.replace("%I%", &delta.sid.to_string());

    // %R% = release
    result = result.replace("%R%", &delta.sid.rel.to_string());

    // %L% = level
    result = result.replace("%L%", &delta.sid.lev.to_string());

    // %B% = branch
    result = result.replace("%B%", &delta.sid.br.to_string());

    // %S% = sequence
    result = result.replace("%S%", &delta.sid.seq.to_string());

    // %D% = current date YY/MM/DD
    result = result.replace("%D%", &now.date_string());

    // %H% = current date MM/DD/YY
    result = result.replace(
        "%H%",
        &format!("{:02}/{:02}/{:02}", now.month, now.day, now.year),
    );

    // %T% = current time HH:MM:SS
    result = result.replace("%T%", &now.time_string());

    // %E% = delta date YY/MM/DD
    result = result.replace("%E%", &delta.datetime.date_string());

    // %G% = delta date MM/DD/YY
    result = result.replace(
        "%G%",
        &format!(
            "{:02}/{:02}/{:02}",
            delta.datetime.month, delta.datetime.day, delta.datetime.year
        ),
    );

    // %U% = delta time HH:MM:SS
    result = result.replace("%U%", &delta.datetime.time_string());

    // %Y% = module type
    result = result.replace("%Y%", module_type);

    // %F% = SCCS filename
    result = result.replace("%F%", sccs_filename);

    // %P% = SCCS absolute pathname
    result = result.replace("%P%", &abs_path);

    // %Q% = q flag value
    result = result.replace("%Q%", q_value);

    // %C% = current line number
    result = result.replace("%C%", &line_number.to_string());

    // %W% = @(#)module<tab>SID (shorthand)
    let w_value = format!("@(#){}\t{}", module_name, delta.sid);
    result = result.replace("%W%", &w_value);

    // %A% = @(#)type module SID@(#) (shorthand)
    let a_value = format!("@(#){} {} {}@(#)", module_type, module_name, delta.sid);
    result = result.replace("%A%", &a_value);

    result
}

/// Find the delta to retrieve based on -r option
fn find_target_delta<'a>(
    sccs: &'a SccsFile,
    requested_sid: Option<&str>,
) -> Result<&'a DeltaEntry, String> {
    match requested_sid {
        None => {
            // No SID specified: get trunk head
            sccs.get_trunk_head()
                .ok_or_else(|| "No deltas in file".to_string())
        }
        Some(sid_str) => {
            let sid: Sid = sid_str
                .parse()
                .map_err(|_| format!("Invalid SID: {}", sid_str))?;

            // Try exact match first
            if let Some(delta) = sccs.find_delta_by_sid(&sid) {
                return Ok(delta);
            }

            // Partial SID handling
            if sid.is_partial() {
                // Only release specified - find highest level in that release
                let matching = sccs
                    .header
                    .deltas
                    .iter()
                    .filter(|d| d.sid.rel == sid.rel && d.sid.is_trunk())
                    .max_by(|a, b| a.sid.cmp(&b.sid));

                matching.ok_or_else(|| format!("No delta found for release {}", sid.rel))
            } else if sid.lev != 0 && sid.br == 0 && sid.seq == 0 {
                // R.L specified - exact trunk match
                sccs.find_delta_by_sid(&sid)
                    .ok_or_else(|| format!("SID {} not found", sid))
            } else {
                // Full SID - must match exactly
                Err(format!("SID {} not found", sid))
            }
        }
    }
}

/// Compute new SID for editing
fn compute_new_sid(sccs: &SccsFile, target: &DeltaEntry, create_branch: bool) -> Sid {
    if create_branch && sccs.branch_enabled() {
        // Create branch: R.L.(max_branch+1).1
        let max_branch = sccs
            .header
            .deltas
            .iter()
            .filter(|d| d.sid.rel == target.sid.rel && d.sid.lev == target.sid.lev)
            .map(|d| d.sid.br)
            .max()
            .unwrap_or(0);

        Sid::new(target.sid.rel, target.sid.lev, max_branch + 1, 1)
    } else if !target.sid.is_trunk() {
        // From branch: increment sequence
        Sid::new(
            target.sid.rel,
            target.sid.lev,
            target.sid.br,
            target.sid.seq + 1,
        )
    } else {
        // From trunk: increment level
        Sid::trunk(target.sid.rel, target.sid.lev + 1)
    }
}

/// Check if there's an existing p-file entry for this SID
fn check_pfile_lock(sfile_path: &Path, new_sid: &Sid) -> Result<(), String> {
    let pfile_path = paths::pfile_from_sfile(sfile_path);
    if pfile_path.exists() {
        let contents =
            fs::read_to_string(&pfile_path).map_err(|e| format!("Cannot read p-file: {}", e))?;

        for entry in
            plib::sccsfile::parse_pfile(&contents).map_err(|e| format!("Invalid p-file: {}", e))?
        {
            if entry.new_sid == *new_sid {
                return Err(format!("SID {} is being edited by {}", new_sid, entry.user));
            }
        }
    }
    Ok(())
}

/// Create or append to p-file
fn create_pfile_entry(sfile_path: &Path, old_sid: &Sid, new_sid: &Sid) -> io::Result<()> {
    let pfile_path = paths::pfile_from_sfile(sfile_path);
    let user = get_username();
    let now = SccsDateTime::now();

    let entry = PfileEntry {
        old_sid: *old_sid,
        new_sid: *new_sid,
        user,
        datetime: now,
        included: None,
        excluded: None,
    };

    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&pfile_path)?;

    writeln!(file, "{}", entry.to_line())?;

    // Set permissions: writable by owner only
    let perms = fs::Permissions::from_mode(0o644);
    fs::set_permissions(&pfile_path, perms)?;

    Ok(())
}

/// Evaluate body with SID tracking for -m option
fn evaluate_body_with_sid(sccs: &SccsFile, applied_set: &HashSet<u16>) -> Vec<(String, Sid)> {
    use plib::sccsfile::BodyRecord;

    let mut output = Vec::new();
    let mut stack: Vec<(bool, u16)> = Vec::new(); // (is_insert, serial)
    let mut current_insert_serial: Option<u16> = None;

    for record in &sccs.body {
        match record {
            BodyRecord::Insert(serial) => {
                stack.push((true, *serial));
                if applied_set.contains(serial) {
                    current_insert_serial = Some(*serial);
                }
            }
            BodyRecord::Delete(serial) => {
                stack.push((false, *serial));
            }
            BodyRecord::End(_) => {
                if let Some((is_insert, serial)) = stack.pop() {
                    if is_insert && Some(serial) == current_insert_serial {
                        // Find next innermost insert
                        current_insert_serial = stack
                            .iter()
                            .rev()
                            .find(|(is_ins, s)| *is_ins && applied_set.contains(s))
                            .map(|(_, s)| *s);
                    }
                }
            }
            BodyRecord::Text(line) => {
                if is_line_visible(&stack, applied_set) {
                    // Find which delta inserted this line
                    let inserting_serial = current_insert_serial.unwrap_or(1);
                    let sid = sccs
                        .find_delta_by_serial(inserting_serial)
                        .map(|d| d.sid)
                        .unwrap_or(Sid::trunk(1, 1));
                    output.push((line.clone(), sid));
                }
            }
        }
    }

    output
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

fn process_file(args: &Args, sfile_path: &Path, multiple_files: bool) -> io::Result<bool> {
    // Print filename if multiple files
    if multiple_files && !args.silent {
        let output_stream: &mut dyn Write = if args.to_stdout {
            &mut io::stderr()
        } else {
            &mut io::stdout()
        };
        writeln!(output_stream, "\n{}:", sfile_path.display())?;
    }

    // Validate s-file
    if !paths::is_sfile(sfile_path) {
        eprintln!(
            "get: {}: {}",
            sfile_path.display(),
            gettext("not an SCCS file")
        );
        return Ok(false);
    }

    // Parse SCCS file
    let sccs = SccsFile::from_path(sfile_path)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;

    // Find target delta
    let target = find_target_delta(&sccs, args.sid.as_deref())
        .map_err(|e| io::Error::new(io::ErrorKind::NotFound, e))?;

    let target_serial = target.serial;
    let target_sid = target.sid;

    // If editing, compute new SID and check for locks
    let new_sid = if args.edit {
        let new_sid = compute_new_sid(&sccs, target, args.branch);

        // Check for existing edit lock (unless joint edit is allowed)
        if !sccs.joint_edit() {
            check_pfile_lock(sfile_path, &new_sid)
                .map_err(|e| io::Error::new(io::ErrorKind::AlreadyExists, e))?;
        }

        Some(new_sid)
    } else {
        None
    };

    // Print SID info (unless silent)
    if !args.silent {
        let output_stream: &mut dyn Write = if args.to_stdout {
            &mut io::stderr()
        } else {
            &mut io::stdout()
        };

        writeln!(output_stream, "{}", target_sid)?;
        if let Some(ref ns) = new_sid {
            writeln!(output_stream, "new delta {}", ns)?;
        }
    }

    // If -g, skip actual retrieval
    if args.no_get {
        return Ok(true);
    }

    // Compute applied set
    let applied_set = sccs
        .compute_applied_set(target_serial)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;

    // Get module name for -n option
    let module_name = sccs
        .module_name()
        .map(|s| s.to_string())
        .unwrap_or_else(|| paths::module_name(sfile_path).unwrap_or_else(|| "unknown".to_string()));

    // Evaluate body
    let suppress_keywords = args.no_keywords || args.edit;

    let lines: Vec<String> = if args.show_sid {
        // Need to track which delta inserted each line
        let lines_with_sid = evaluate_body_with_sid(&sccs, &applied_set);
        let target_delta = sccs.find_delta_by_serial(target_serial).unwrap();

        lines_with_sid
            .into_iter()
            .enumerate()
            .map(|(i, (line, sid))| {
                let expanded = if suppress_keywords {
                    line
                } else {
                    expand_keywords(&line, &sccs, target_delta, sfile_path, i + 1)
                };

                if args.show_module {
                    format!("{}\t{}\t{}", module_name, sid, expanded)
                } else {
                    format!("{}\t{}", sid, expanded)
                }
            })
            .collect()
    } else {
        let content = sccs.evaluate_body(&applied_set);
        let target_delta = sccs.find_delta_by_serial(target_serial).unwrap();

        content
            .into_iter()
            .enumerate()
            .map(|(i, line)| {
                let expanded = if suppress_keywords {
                    line
                } else {
                    expand_keywords(&line, &sccs, target_delta, sfile_path, i + 1)
                };

                if args.show_module {
                    format!("{}\t{}", module_name, expanded)
                } else {
                    expanded
                }
            })
            .collect()
    };

    let line_count = lines.len();

    // Write output
    if args.to_stdout {
        let stdout = io::stdout();
        let mut handle = stdout.lock();
        for line in &lines {
            writeln!(handle, "{}", line)?;
        }
    } else {
        // Write to g-file
        let gfile_path = paths::gfile_from_sfile(sfile_path)
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "Invalid s-file name"))?;

        let mut file = File::create(&gfile_path)?;
        for line in &lines {
            writeln!(file, "{}", line)?;
        }

        // Set permissions: read-only unless editing
        let mode = if args.edit { 0o644 } else { 0o444 };
        let perms = fs::Permissions::from_mode(mode);
        fs::set_permissions(&gfile_path, perms)?;
    }

    // Create p-file if editing
    if args.edit {
        if let Some(ns) = new_sid {
            create_pfile_entry(sfile_path, &target_sid, &ns)?;
        }
    }

    // Print line count (unless silent)
    if !args.silent {
        let output_stream: &mut dyn Write = if args.to_stdout {
            &mut io::stderr()
        } else {
            &mut io::stdout()
        };
        writeln!(output_stream, "{} lines", line_count)?;
    }

    Ok(true)
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    let args = Args::parse();

    let multiple_files = args.files.len() > 1;
    let mut exit_code = ExitCode::SUCCESS;

    for file_path in &args.files {
        match process_file(&args, file_path, multiple_files) {
            Ok(true) => {}
            Ok(false) => exit_code = ExitCode::FAILURE,
            Err(e) => {
                eprintln!("get: {}: {}", file_path.display(), e);
                exit_code = ExitCode::FAILURE;
            }
        }
    }

    exit_code
}
