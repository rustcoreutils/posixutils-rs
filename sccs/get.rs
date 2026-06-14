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
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::sccsfile::{paths, DeltaEntry, PfileEntry, SccsDateTime, SccsFile, SccsFlag, Sid, ZLock};

/// get - get a version of an SCCS file
#[derive(Parser)]
#[command(version, about = gettext("get - get a version of an SCCS file"))]
struct Args {
    #[arg(short = 'r', value_name = "SID", help = gettext("SID to retrieve"))]
    sid: Option<String>,

    #[arg(short = 'c', value_name = "CUTOFF", help = gettext("Cutoff date-time; exclude deltas created after it"))]
    cutoff: Option<String>,

    #[arg(short = 'i', value_name = "LIST", help = gettext("List of deltas (SIDs) to include"))]
    include: Option<String>,

    #[arg(short = 'x', value_name = "LIST", help = gettext("List of deltas (SIDs) to exclude"))]
    exclude: Option<String>,

    #[arg(short = 'l', help = gettext("Write a delta summary into an l-file"))]
    lfile: bool,

    #[arg(short = 'L', help = gettext("Write a delta summary to standard output"))]
    lfile_stdout: bool,

    #[arg(short = 't', help = gettext("Access the most recently created (top) delta in a release"))]
    top: bool,

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
    plib::sccsfile::real_login_name()
}

/// Parse a `-c cutoff` date-time string into (year, month, day, hour, min, sec).
///
/// Digits are grouped YY[MM[DD[HH[MM[SS]]]]] with non-numeric separators
/// ignored. The 2-digit-year POSIX pivot applies: 69-99 => 1900+, 00-68 =>
/// 2000+. Units omitted from the cutoff default to their maximum possible
/// value (so `-c 7502` means up to the end of February 1975), per the spec.
fn parse_cutoff(cutoff: &str) -> Option<(u16, u8, u8, u8, u8, u8)> {
    let digits: String = cutoff.chars().filter(|c| c.is_ascii_digit()).collect();

    let len = digits.len();
    if len < 2 {
        return None;
    }

    // POSIX -c uses a 2-digit year (YY) with the documented century pivot:
    // 69-99 => 1900+, 00-68 => 2000+.
    let yy: u16 = digits[0..2].parse().ok()?;
    let year = if yy < 69 { 2000 + yy } else { 1900 + yy };
    let rest = &digits[2..];

    let month = if rest.len() >= 2 {
        rest[0..2].parse().unwrap_or(12)
    } else {
        12
    };
    let rest = if rest.len() >= 2 { &rest[2..] } else { "" };

    let day = if rest.len() >= 2 {
        rest[0..2].parse().unwrap_or(31)
    } else {
        31
    };
    let rest = if rest.len() >= 2 { &rest[2..] } else { "" };

    let hour = if rest.len() >= 2 {
        rest[0..2].parse().unwrap_or(23)
    } else {
        23
    };
    let rest = if rest.len() >= 2 { &rest[2..] } else { "" };

    let min = if rest.len() >= 2 {
        rest[0..2].parse().unwrap_or(59)
    } else {
        59
    };
    let rest = if rest.len() >= 2 { &rest[2..] } else { "" };

    let sec = if rest.len() >= 2 {
        rest[0..2].parse().unwrap_or(59)
    } else {
        59
    };

    Some((year, month, day, hour, min, sec))
}

/// Full 4-digit year for a delta's 2-digit datetime year (POSIX pivot).
fn delta_full_year(dt: &SccsDateTime) -> u16 {
    if dt.year < 100 {
        if dt.year < 69 {
            2000 + dt.year
        } else {
            1900 + dt.year
        }
    } else {
        dt.year
    }
}

/// True if the delta was created after the cutoff date-time.
fn delta_after_cutoff(delta: &DeltaEntry, cutoff: (u16, u8, u8, u8, u8, u8)) -> bool {
    let dt = &delta.datetime;
    (
        delta_full_year(dt),
        dt.month,
        dt.day,
        dt.hour,
        dt.minute,
        dt.second,
    ) > cutoff
}

/// Resolve a comma/space-separated SID list (e.g. "1.2,1.3") into the set of
/// delta serial numbers. Unknown SIDs are reported and skipped.
fn resolve_sid_list(sccs: &SccsFile, list: &str) -> Vec<u16> {
    let mut serials = Vec::new();
    for tok in list.split([',', ' ']).filter(|s| !s.is_empty()) {
        match tok.parse::<Sid>() {
            Ok(sid) => {
                if let Some(d) = sccs.find_delta_by_sid(&sid) {
                    serials.push(d.serial);
                } else {
                    eprintln!("get: {}: {}", tok, gettext("no such delta"));
                }
            }
            Err(_) => {
                eprintln!("get: {}: {}", tok, gettext("invalid SID"));
            }
        }
    }
    serials
}

/// True if `ancestor` is `serial` itself or appears in `serial`'s predecessor
/// chain (used to cascade -x exclusions to descendant deltas).
fn is_descendant_of(sccs: &SccsFile, serial: u16, ancestor: u16) -> bool {
    let mut cur = serial;
    while cur != 0 {
        if cur == ancestor {
            return true;
        }
        match sccs.find_delta_by_serial(cur) {
            Some(d) => cur = d.pred_serial,
            None => break,
        }
    }
    false
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

/// Find the most recently created (top) delta. With a requested release, the
/// search is restricted to deltas in that release; otherwise it considers all
/// applied (non-removed) deltas in the file.
fn find_top_delta<'a>(
    sccs: &'a SccsFile,
    requested_sid: Option<&str>,
) -> Result<&'a DeltaEntry, String> {
    let release: Option<u16> = match requested_sid {
        None => None,
        Some(s) => {
            let sid: Sid = s.parse().map_err(|_| format!("Invalid SID: {}", s))?;
            Some(sid.rel)
        }
    };

    sccs.header
        .deltas
        .iter()
        .filter(|d| d.delta_type == plib::sccsfile::DeltaType::Normal)
        .filter(|d| release.is_none_or(|r| d.sid.rel == r))
        .max_by(|a, b| {
            let ka = (
                delta_full_year(&a.datetime),
                a.datetime.month,
                a.datetime.day,
                a.datetime.hour,
                a.datetime.minute,
                a.datetime.second,
                a.serial,
            );
            let kb = (
                delta_full_year(&b.datetime),
                b.datetime.month,
                b.datetime.day,
                b.datetime.hour,
                b.datetime.minute,
                b.datetime.second,
                b.serial,
            );
            ka.cmp(&kb)
        })
        .ok_or_else(|| match release {
            Some(r) => format!("No delta found for release {}", r),
            None => "No deltas in file".to_string(),
        })
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

/// True if the line contains an SCCS identification keyword of the form `%X%`
/// where X is a single uppercase letter.
fn line_has_id_keyword(line: &str) -> bool {
    let bytes = line.as_bytes();
    let mut i = 0;
    while i + 2 < bytes.len() {
        if bytes[i] == b'%' && bytes[i + 1].is_ascii_uppercase() && bytes[i + 2] == b'%' {
            return true;
        }
        i += 1;
    }
    false
}

/// Create or append to p-file
fn create_pfile_entry(
    sfile_path: &Path,
    old_sid: &Sid,
    new_sid: &Sid,
    included: Option<String>,
    excluded: Option<String>,
) -> io::Result<()> {
    let pfile_path = paths::pfile_from_sfile(sfile_path);
    let user = get_username();
    let now = SccsDateTime::now();

    let entry = PfileEntry {
        old_sid: *old_sid,
        new_sid: *new_sid,
        user,
        datetime: now,
        included,
        excluded,
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

/// Render the delta-summary table (the l-file body). Deltas are listed newest
/// first. Each entry is a status line followed by comment lines indented one
/// tab and terminated by a blank line, per the POSIX l-file format:
///   "%c%c%c %s\t%s %s\n", code1, code2, code3, SID, date-time, login
fn render_lfile(sccs: &SccsFile, applied_set: &HashSet<u16>) -> String {
    let mut out = String::new();
    // Newest delta first: iterate in descending serial order.
    let mut deltas: Vec<&DeltaEntry> = sccs
        .header
        .deltas
        .iter()
        .filter(|d| d.delta_type == plib::sccsfile::DeltaType::Normal)
        .collect();
    deltas.sort_by_key(|d| std::cmp::Reverse(d.serial));

    for d in deltas {
        let code1 = if applied_set.contains(&d.serial) {
            ' '
        } else {
            '*'
        };
        // code2/code3 special-reason codes are unsupported here; use spaces.
        out.push(code1);
        out.push(' ');
        out.push(' ');
        out.push(' ');
        out.push_str(&format!(
            "{}\t{} {} {}\n",
            d.sid,
            d.datetime.date_string(),
            d.datetime.time_string(),
            d.user
        ));
        for c in &d.comments {
            out.push('\t');
            out.push_str(c);
            out.push('\n');
        }
        out.push('\n');
    }
    out
}

/// Write the delta summary to an l-file (`to_stdout` = false) or to standard
/// output (`to_stdout` = true, the -L option).
fn write_lfile(
    sccs: &SccsFile,
    sfile_path: &Path,
    applied_set: &HashSet<u16>,
    to_stdout: bool,
) -> io::Result<()> {
    let table = render_lfile(sccs, applied_set);
    if to_stdout {
        io::stdout().write_all(table.as_bytes())?;
    } else {
        let lfile_path = paths::lfile_from_sfile(sfile_path);
        fs::write(&lfile_path, table.as_bytes())?;
        // l-file is read-only, owned by the real user.
        fs::set_permissions(&lfile_path, fs::Permissions::from_mode(0o444))?;
    }
    Ok(())
}

fn process_file(args: &Args, sfile_path: &Path, multiple_files: bool) -> io::Result<bool> {
    // Print filename if multiple files
    if multiple_files && !args.silent {
        let output_stream: &mut dyn Write = if args.to_stdout || args.lfile_stdout {
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

    // Find target delta. With -t, access the most recently created (top)
    // delta in the requested release (or overall when no -r is given).
    let target = if args.top {
        find_top_delta(&sccs, args.sid.as_deref())
            .map_err(|e| io::Error::new(io::ErrorKind::NotFound, e))?
    } else {
        find_target_delta(&sccs, args.sid.as_deref())
            .map_err(|e| io::Error::new(io::ErrorKind::NotFound, e))?
    };

    let target_serial = target.serial;
    let target_sid = target.sid;

    // If editing, acquire the per-command z-file lock (held until this
    // function returns, then released so a later delta/unget can run), compute
    // the new SID and check for the p-file edit lock.
    //
    // `_zlock` is bound at function scope so the z-file persists across the
    // early-return (encoded) path below and is removed on every return.
    let _zlock;
    let new_sid = if args.edit {
        _zlock = match ZLock::acquire(sfile_path) {
            Ok(z) => z,
            Err(e) if e.kind() == io::ErrorKind::AlreadyExists => {
                eprintln!("get: {}: {}", sfile_path.display(), gettext("being edited"));
                return Ok(false);
            }
            Err(e) => return Err(e),
        };

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

    // Informative output goes to stderr when the text retrieval occupies
    // standard output (-p) or when -L writes the delta summary there.
    let info_to_stderr = args.to_stdout || args.lfile_stdout;

    // Resolve -i / -x SID lists into delta serials.
    let included_serials: Vec<u16> = args
        .include
        .as_deref()
        .map(|l| resolve_sid_list(&sccs, l))
        .unwrap_or_default();
    let excluded_serials: Vec<u16> = args
        .exclude
        .as_deref()
        .map(|l| resolve_sid_list(&sccs, l))
        .unwrap_or_default();

    // Compute applied set, then apply -c cutoff and -i/-x adjustments.
    let mut applied_set = sccs
        .compute_applied_set(target_serial)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;

    // -c cutoff: drop any applied delta created after the cutoff date-time.
    if let Some(cutoff_str) = args.cutoff.as_deref() {
        let cutoff = parse_cutoff(cutoff_str).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("Invalid cutoff date: '{}'", cutoff_str),
            )
        })?;
        applied_set.retain(|&serial| {
            sccs.find_delta_by_serial(serial)
                .map(|d| !delta_after_cutoff(d, cutoff))
                .unwrap_or(false)
        });
    }

    // -i: force-include the listed deltas (and their predecessor chains).
    for &inc in &included_serials {
        let mut cur = inc;
        while cur != 0 {
            applied_set.insert(cur);
            match sccs.find_delta_by_serial(cur) {
                Some(d) => cur = d.pred_serial,
                None => break,
            }
        }
    }

    // -x: force-exclude the listed deltas. Only the named deltas are dropped
    // from the applied set (descendant deltas remain applied); the Excluded:
    // notation, however, additionally lists descendants of the excluded
    // deltas, matching historical SCCS bookkeeping.
    let mut excluded_effective: Vec<u16> = Vec::new();
    if !excluded_serials.is_empty() {
        for &x in &excluded_serials {
            applied_set.remove(&x);
        }
        // Notation set: each excluded delta plus any (applied or not) delta
        // whose predecessor chain passes through an excluded delta.
        excluded_effective = sccs
            .header
            .deltas
            .iter()
            .filter(|d| d.delta_type == plib::sccsfile::DeltaType::Normal)
            .map(|d| d.serial)
            .filter(|&s| {
                excluded_serials
                    .iter()
                    .any(|&x| is_descendant_of(&sccs, s, x))
            })
            .collect();
        excluded_effective.sort_by_key(|&s| {
            sccs.find_delta_by_serial(s)
                .map(|d| d.sid)
                .unwrap_or_default()
        });
    }

    // Print SID info and -i/-x notation (unless silent)
    if !args.silent {
        let output_stream: &mut dyn Write = if info_to_stderr {
            &mut io::stderr()
        } else {
            &mut io::stdout()
        };

        writeln!(output_stream, "{}", target_sid)?;
        if let Some(ref ns) = new_sid {
            writeln!(output_stream, "new delta {}", ns)?;
        }

        if !included_serials.is_empty() {
            writeln!(output_stream, "Included:")?;
            let mut inc = included_serials.clone();
            inc.sort_by_key(|&s| {
                sccs.find_delta_by_serial(s)
                    .map(|d| d.sid)
                    .unwrap_or_default()
            });
            for s in &inc {
                if let Some(d) = sccs.find_delta_by_serial(*s) {
                    writeln!(output_stream, "{}", d.sid)?;
                }
            }
        }
        if !excluded_effective.is_empty() {
            writeln!(output_stream, "Excluded:")?;
            for s in &excluded_effective {
                if let Some(d) = sccs.find_delta_by_serial(*s) {
                    writeln!(output_stream, "{}", d.sid)?;
                }
            }
        }
    }

    // -l / -L: write the delta summary table.
    if args.lfile || args.lfile_stdout {
        write_lfile(&sccs, sfile_path, &applied_set, args.lfile_stdout)?;
    }

    // If -g, skip actual retrieval
    if args.no_get {
        return Ok(true);
    }

    // Get module name for -n option
    let module_name = sccs
        .module_name()
        .map(|s| s.to_string())
        .unwrap_or_else(|| paths::module_name(sfile_path).unwrap_or_else(|| "unknown".to_string()));

    // Encoded (binary / no-trailing-newline) bodies: uudecode and emit raw
    // bytes, with no ID-keyword expansion (the content is opaque).
    if sccs.is_encoded() {
        let encoded_lines = sccs.evaluate_body(&applied_set);
        let raw = plib::sccsfile::uudecode_sccs(&encoded_lines);

        if args.to_stdout {
            io::stdout().write_all(&raw)?;
        } else {
            let gfile_path = paths::gfile_from_sfile(sfile_path).ok_or_else(|| {
                io::Error::new(io::ErrorKind::InvalidInput, "Invalid s-file name")
            })?;
            fs::write(&gfile_path, &raw)?;
            let mode = if args.edit { 0o644 } else { 0o444 };
            fs::set_permissions(&gfile_path, fs::Permissions::from_mode(mode))?;
        }

        if args.edit {
            if let Some(ns) = new_sid {
                create_pfile_entry(
                    sfile_path,
                    &target_sid,
                    &ns,
                    args.include.clone(),
                    args.exclude.clone(),
                )?;
            }
        }

        if !args.silent {
            let line_count = raw.iter().filter(|&&b| b == b'\n').count();
            let output_stream: &mut dyn Write = if info_to_stderr {
                &mut io::stderr()
            } else {
                &mut io::stdout()
            };
            writeln!(output_stream, "{} lines", line_count)?;
        }

        return Ok(true);
    }

    // Evaluate body
    let suppress_keywords = args.no_keywords || args.edit;

    // Track whether any ID keyword (%X%) appeared in the retrieved text.
    let mut had_keyword = false;

    let lines: Vec<String> = if args.show_sid {
        // Need to track which delta inserted each line
        let lines_with_sid = evaluate_body_with_sid(&sccs, &applied_set);
        let target_delta = sccs.find_delta_by_serial(target_serial).unwrap();

        lines_with_sid
            .into_iter()
            .enumerate()
            .map(|(i, (line, sid))| {
                if line_has_id_keyword(&line) {
                    had_keyword = true;
                }
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
                if line_has_id_keyword(&line) {
                    had_keyword = true;
                }
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

    // "No id keywords" handling. When no keyword was present (and keyword
    // expansion was not suppressed), SCCS warns. If the s-file has the 'i'
    // flag set, this is escalated to a fatal error: no g-file is written.
    if !had_keyword && !suppress_keywords {
        let has_i_flag = sccs
            .header
            .flags
            .iter()
            .any(|f| matches!(f, SccsFlag::IdKeywordError(_)));
        if has_i_flag {
            // Fatal: always reported, even under -s; no g-file is written.
            eprintln!(
                "get: {}: {}",
                sfile_path.display(),
                gettext("No id keywords.")
            );
            return Ok(false);
        } else if !args.silent {
            // Warning: suppressed by -s along with other informative output.
            eprintln!(
                "get: warning: {}: {}",
                sfile_path.display(),
                gettext("No id keywords.")
            );
        }
    }

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
            create_pfile_entry(
                sfile_path,
                &target_sid,
                &ns,
                args.include.clone(),
                args.exclude.clone(),
            )?;
        }
    }

    // Print line count (unless silent)
    if !args.silent {
        let output_stream: &mut dyn Write = if info_to_stderr {
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

    // Expand operands: a lone "-" reads s-file pathnames from stdin, and
    // directory operands expand to their sorted s.* members.
    let files = paths::expand_operands(&args.files);

    // Per spec, when a directory or standard input ("-") is named, each
    // pathname is written before its output even if it resolves to a single
    // file.
    let multiple_files = files.len() > 1
        || (args.files.len() == 1 && args.files[0].as_os_str() == "-")
        || args.files.iter().any(|p| p.is_dir());
    let mut exit_code = ExitCode::SUCCESS;

    for file_path in &files {
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
