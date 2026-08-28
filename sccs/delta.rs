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
use std::io::{self, BufRead, BufReader, IsTerminal, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::linediff::LineOp;
use plib::sccsfile::{
    paths, BodyRecord, DeltaEntry, DeltaStats, DeltaType, PfileEntry, SccsDateTime, SccsFile,
    SccsFlag, Sid,
};
use posixutils_sccs::{diag, mrlist, operands, pfile, protect, sfio, zlock};

/// True if standard input is a terminal.
fn stdin_is_tty() -> bool {
    io::stdin().is_terminal()
}

/// Read a single line from standard input, stripping the trailing newline.
/// Returns an empty string at EOF.
fn read_stdin_line() -> String {
    let mut line = String::new();
    let _ = io::stdin().lock().read_line(&mut line);
    while line.ends_with('\n') || line.ends_with('\r') {
        line.pop();
    }
    line
}

/// delta - make a delta (change) to an SCCS file
#[derive(Parser)]
#[command(version, about = gettext("delta - make a delta (change) to an SCCS file"))]
struct Args {
    #[arg(short = 'r', value_name = "SID", help = gettext("SID of delta to create (if multiple edits pending)"))]
    sid: Option<String>,

    #[arg(short = 'y', value_name = "COMMENT", num_args = 0..=1, default_missing_value = "", help = gettext("Comment for delta"))]
    comment: Option<String>,

    #[arg(short = 'm', value_name = "MRLIST", num_args = 0..=1, default_missing_value = "", help = gettext("Modification request (MR) numbers for delta"))]
    mrlist: Option<String>,

    #[arg(short = 'g', value_name = "LIST", help = gettext("List of deltas to ignore at this change level"))]
    glist: Option<String>,

    #[arg(short = 'n', help = gettext("Retain g-file after delta"))]
    keep_gfile: bool,

    #[arg(short = 'p', help = gettext("Print diff output"))]
    print_diff: bool,

    #[arg(short = 's', help = gettext("Silent mode (suppress informational messages)"))]
    silent: bool,

    #[arg(required = true, help = gettext("SCCS files to process"))]
    files: Vec<PathBuf>,
}

/// Find the p-file entry for this user
fn find_pfile_entry(sfile_path: &Path, requested_sid: Option<&Sid>) -> io::Result<PfileEntry> {
    let entries = pfile::read(sfile_path)?;
    if entries.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!(
                "{} ({} {})",
                gettext("No outstanding get -e"),
                gettext("no p-file for"),
                sfile_path.display()
            ),
        ));
    }

    let user = posixutils_sccs::username();

    // Filter to this user's entries
    let user_entries: Vec<_> = entries.into_iter().filter(|e| e.user == user).collect();

    if user_entries.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!("{} {}", gettext("No outstanding get -e by user"), user),
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
            format!("{} {}", gettext("No edit pending for SID"), sid),
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
            "{} {}{}",
            gettext("Multiple edits pending by user"),
            user,
            gettext("; use -r to specify SID")
        ),
    ))
}

/// Read g-file content
fn read_gfile(sfile_path: &Path) -> io::Result<Vec<String>> {
    let gfile_path = paths::gfile_from_sfile(sfile_path).ok_or_else(|| {
        io::Error::new(io::ErrorKind::InvalidInput, gettext("Invalid s-file name"))
    })?;

    if !gfile_path.exists() {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!(
                "{} {} {}",
                gettext("g-file"),
                gfile_path.display(),
                gettext("not found")
            ),
        ));
    }

    let file = File::open(&gfile_path)?;
    let reader = BufReader::new(file);
    reader.lines().collect()
}

/// Read the g-file as the lines to diff against the stored body.
///
/// An encoded (binary) s-file stores uuencoded text, and `get` writes the
/// decoded bytes to the g-file, so the two are only comparable after encoding
/// the g-file back. `delta` never checked `is_encoded()` at all: it read the
/// raw bytes as UTF-8 lines and diffed them against uuencoded text, so a
/// binary file could be created and retrieved but never updated -- it failed
/// with "stream did not contain valid UTF-8".
fn read_gfile_for(sccs: &SccsFile, sfile_path: &Path) -> io::Result<Vec<String>> {
    if !sccs.is_encoded() {
        return read_gfile(sfile_path);
    }

    let gfile_path = paths::gfile_from_sfile(sfile_path).ok_or_else(|| {
        io::Error::new(io::ErrorKind::InvalidInput, gettext("Invalid s-file name"))
    })?;
    let raw = fs::read(&gfile_path)?;
    Ok(plib::sccsfile::uuencode_sccs(&raw))
}

/// The set of deltas that made up the version `get -e` handed the user.
///
/// The `-i`/`-x` lists recorded in the p-file are part of that: without them
/// the base still holds an excluded delta's lines, and the diff charges their
/// absence from the g-file to the user as deletions they never made. The same
/// set has to drive the body rewrite, or the rewrite counts base lines the
/// diff never saw and lands the edit at the wrong offset.
fn base_applied_set(
    sccs: &SccsFile,
    base_serial: u16,
    included: &[u16],
    excluded: &[u16],
) -> io::Result<HashSet<u16>> {
    sccs.applied_set_with(base_serial, included, excluded)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))
}

/// Resolve one of the p-file's `-i`/`-x` option-arguments into delta serials.
///
/// The list was written by `get -e`, which already validated it, so an
/// unresolvable token here means the s-file changed underneath the edit.
fn resolve_pfile_list(
    sccs: &SccsFile,
    list: Option<&str>,
    sfile_path: &Path,
) -> Result<Vec<u16>, String> {
    let Some(list) = list else {
        return Ok(Vec::new());
    };
    let (serials, unresolved) = sccs.resolve_sid_list(list)?;
    for tok in unresolved {
        diag::error_path(
            "delta",
            sfile_path,
            &format!("{}: {}", tok, gettext("no such delta")),
        );
    }
    Ok(serials)
}

/// Diff the base version against the g-file.
///
/// The edit script is minimal (see `plib::linediff`). The previous greedy scan
/// with a ten-line lookahead recorded more edits than necessary whenever the
/// next matching line lay outside that window, and indexed the new text
/// unguarded once it had run past the end, panicking on any change that
/// deleted two or more trailing lines.
fn compute_diff(base_lines: &[String], new_lines: &[String]) -> (DeltaStats, Vec<LineOp>) {
    let ops = plib::linediff::diff(base_lines, new_lines);

    let mut stats = DeltaStats::default();
    for op in &ops {
        match op {
            LineOp::Keep => stats.unchanged += 1,
            LineOp::Delete => stats.deleted += 1,
            LineOp::Insert(_) => stats.inserted += 1,
        }
    }

    (stats, ops)
}

fn print_normal_diff(base_lines: &[String], new_lines: &[String], ops: &[LineOp]) {
    // Render a single line-range as "a" or "a,b".
    fn range(start: usize, count: usize) -> String {
        if count == 0 {
            // For an addition/deletion at position 0, the range is the line
            // number *after* which the change applies.
            start.to_string()
        } else if count == 1 {
            start.to_string()
        } else {
            format!("{},{}", start, start + count - 1)
        }
    }

    let mut bi = 0usize; // 0-based index into base_lines
    let mut ni = 0usize; // 0-based index into new_lines
    let mut k = 0usize;

    while k < ops.len() {
        match &ops[k] {
            LineOp::Keep => {
                bi += 1;
                ni += 1;
                k += 1;
            }
            LineOp::Delete | LineOp::Insert(_) => {
                // Collect a maximal run of deletes then inserts (a replace),
                // or just deletes, or just inserts.
                let del_start = bi;
                let ins_start = ni;
                let mut dels: Vec<String> = Vec::new();
                let mut inss: Vec<String> = Vec::new();

                while k < ops.len() {
                    match &ops[k] {
                        LineOp::Delete => {
                            dels.push(base_lines[bi].clone());
                            bi += 1;
                            k += 1;
                        }
                        _ => break,
                    }
                }
                while k < ops.len() {
                    match &ops[k] {
                        LineOp::Insert(_) => {
                            inss.push(new_lines[ni].clone());
                            ni += 1;
                            k += 1;
                        }
                        _ => break,
                    }
                }

                let nd = dels.len();
                let np = inss.len();
                if nd > 0 && np > 0 {
                    // Change.
                    println!("{}c{}", range(del_start + 1, nd), range(ins_start + 1, np));
                    for l in &dels {
                        println!("< {}", l);
                    }
                    println!("---");
                    for l in &inss {
                        println!("> {}", l);
                    }
                } else if nd > 0 {
                    // Deletion: lines del_start+1..del_start+nd removed after
                    // new line ins_start.
                    println!("{}d{}", range(del_start + 1, nd), ins_start);
                    for l in &dels {
                        println!("< {}", l);
                    }
                } else if np > 0 {
                    // Addition: lines added after base line del_start.
                    println!("{}a{}", del_start, range(ins_start + 1, np));
                    for l in &inss {
                        println!("> {}", l);
                    }
                }
            }
        }
    }
}

/// Apply diff operations to the SCCS body
fn apply_diff_to_body(
    sccs: &SccsFile,
    applied_set: &HashSet<u16>,
    new_serial: u16,
    new_lines: &[String],
    diff_ops: &[LineOp],
) -> Vec<BodyRecord> {
    // First, collect all lines that need to be deleted or kept
    let mut delete_set = HashSet::new();
    let mut insert_after: Vec<(usize, Vec<String>)> = Vec::new();

    let mut base_idx = 0;
    let mut inserts_at_pos: Vec<String> = Vec::new();

    for op in diff_ops {
        match op {
            LineOp::Keep => {
                if !inserts_at_pos.is_empty() {
                    insert_after.push((base_idx, inserts_at_pos.clone()));
                    inserts_at_pos.clear();
                }
                base_idx += 1;
            }
            LineOp::Delete => {
                delete_set.insert(base_idx);
                base_idx += 1;
            }
            LineOp::Insert(j) => {
                inserts_at_pos.push(new_lines[*j].clone());
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
                let visible = plib::sccsfile::is_line_visible(&stack, applied_set);

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

/// Remove entry from p-file
fn remove_pfile_entry(sfile_path: &Path, entry_to_remove: &PfileEntry) -> io::Result<()> {
    let remaining: Vec<_> = pfile::read(sfile_path)?
        .into_iter()
        .filter(|e| {
            !(e.old_sid == entry_to_remove.old_sid
                && e.new_sid == entry_to_remove.new_sid
                && e.user == entry_to_remove.user)
        })
        .collect();

    pfile::write(sfile_path, &remaining)
}

/// Determine the MR list for the new delta, honoring the `v` flag.
///
/// Returns `Ok(Some(mrs))` with the (possibly empty) MR list to record, or
/// `Ok(None)` if the delta must be aborted (validation failed / required MR
/// missing); in the abort case a diagnostic has already been written.
///
/// `stdin_consumed` is true when the file operand was `-`, in which case stdin
/// has already been read for pathnames and may not be used for prompting.
fn gather_mrs(
    args: &Args,
    sccs: &SccsFile,
    sfile_path: &Path,
    stdin_consumed: bool,
) -> io::Result<Option<Vec<String>>> {
    // Locate the v flag, if any.
    let v_flag = sccs.header.flags.iter().find_map(|f| match f {
        SccsFlag::MrValidation(prog) => Some(prog.clone()),
        _ => None,
    });
    let v_set = v_flag.is_some();

    // Source the raw MR list: -m argument, else (if v is set) a prompt/stdin.
    let raw: Option<String> = match &args.mrlist {
        Some(s) => Some(s.clone()),
        None => {
            if v_set && !stdin_consumed {
                if stdin_is_tty() {
                    print!("{}", gettext("MRs? "));
                    io::stdout().flush().ok();
                }
                Some(read_stdin_line())
            } else {
                None
            }
        }
    };

    let mrs: Vec<String> = mrlist::parse(raw.as_deref());

    if !v_set {
        // MRs are only meaningful when the v flag is set.
        if args.mrlist.is_some() {
            eprintln!(
                "delta: {}: {}",
                sfile_path.display(),
                gettext("MR verification ('v') flag not set, MRs are not allowed.")
            );
            return Ok(None);
        }
        return Ok(Some(Vec::new()));
    }

    // v flag is set: MRs are required.
    if mrs.is_empty() {
        eprintln!(
            "delta: {}: {}",
            sfile_path.display(),
            gettext("MR number(s) must be supplied.")
        );
        return Ok(None);
    }

    // If the v flag names a validation program, run it with the MRs as args.
    if let Some(prog) = v_flag.flatten() {
        if !prog.is_empty() {
            match Command::new(&prog).args(&mrs).status() {
                Ok(status) if status.success() => {}
                Ok(_) => {
                    eprintln!(
                        "delta: {}: {}",
                        sfile_path.display(),
                        gettext("MR validation failed.")
                    );
                    return Ok(None);
                }
                Err(e) => {
                    eprintln!("delta: {}: {}: {}", sfile_path.display(), prog, e);
                    return Ok(None);
                }
            }
        }
    }

    Ok(Some(mrs))
}

/// Determine the comment for the new delta.
///
/// Uses `-y` if supplied; otherwise prompts on a terminal (or reads stdin
/// silently when not a terminal). When the operand was `-`, stdin is already
/// consumed and the comment defaults to empty.
fn gather_comment(args: &Args, stdin_consumed: bool) -> String {
    match &args.comment {
        Some(c) => c.clone(),
        None => {
            if stdin_consumed {
                String::new()
            } else {
                if stdin_is_tty() {
                    print!("{}", gettext("comments? "));
                    io::stdout().flush().ok();
                }
                read_stdin_line()
            }
        }
    }
}

fn process_file(args: &Args, sfile_path: &Path, stdin_consumed: bool) -> io::Result<bool> {
    // Validate s-file
    if !paths::is_sfile(sfile_path) {
        eprintln!(
            "delta: {}: {}",
            sfile_path.display(),
            gettext("not an SCCS file")
        );
        return Ok(false);
    }

    // Acquire the per-command z-file lock around the s-file rewrite (POSIX
    // `shall`). If another SCCS command holds it, report and skip.
    let _zlock = match zlock::acquire(sfile_path) {
        Ok(z) => z,
        Err(e) if e.kind() == io::ErrorKind::AlreadyExists => {
            eprintln!(
                "delta: {}: {}",
                sfile_path.display(),
                gettext("being edited")
            );
            return Ok(false);
        }
        Err(e) => return Err(e),
    };

    // Find p-file entry
    let requested_sid: Option<Sid> = args
        .sid
        .as_ref()
        .map(|s| s.parse())
        .transpose()
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, gettext("Invalid SID")))?;

    let pfile_entry = find_pfile_entry(sfile_path, requested_sid.as_ref())?;

    // Parse SCCS file
    let mut sccs = SccsFile::from_path(sfile_path)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;

    // The user list can be narrowed while an edit is outstanding, so the
    // p-file lock taken by an earlier `get -e` is no evidence of present
    // permission. Re-check before anything is written.
    //
    // The release checked is the one `get -e` checked -- the release the edit
    // was *retrieved* from, not the one it will create. Checking the new SID
    // here instead let `get -e -r2` succeed against a locked release 2 and
    // then made the resulting delta permanently uncommittable, stranding the
    // work behind a p-file lock nothing would clear. CSSC gates the retrieved
    // release too.
    if let Err(refusal) =
        protect::check_edit(&sccs, pfile_entry.old_sid.rel, &posixutils_sccs::username())
    {
        diag::error_path("delta", sfile_path, refusal.message());
        return Ok(false);
    }

    // Find the base delta
    let base_delta = sccs
        .find_delta_by_sid(&pfile_entry.old_sid)
        .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, gettext("Base SID not found")))?;
    let base_serial = base_delta.serial;

    // Read g-file
    let new_lines = read_gfile_for(&sccs, sfile_path)?;

    // The edit was made against the version `get -e` produced, which means the
    // -i/-x lists it recorded in the p-file, not the plain base.
    let forced_in = match resolve_pfile_list(&sccs, pfile_entry.included.as_deref(), sfile_path) {
        Ok(v) => v,
        Err(e) => {
            diag::error_path("delta", sfile_path, &e);
            return Ok(false);
        }
    };
    let forced_out = match resolve_pfile_list(&sccs, pfile_entry.excluded.as_deref(), sfile_path) {
        Ok(v) => v,
        Err(e) => {
            diag::error_path("delta", sfile_path, &e);
            return Ok(false);
        }
    };

    // Reconstruct base version
    let applied_set = base_applied_set(&sccs, base_serial, &forced_in, &forced_out)?;
    let base_lines = sccs.evaluate_body(&applied_set);

    // Compute diff
    let (stats, diff_ops) = compute_diff(&base_lines, &new_lines);

    // Gather MRs (honoring the v flag) and the comment. The MR prompt, if any,
    // must precede the comment prompt.
    let mrs = match gather_mrs(args, &sccs, sfile_path, stdin_consumed)? {
        Some(m) => m,
        None => return Ok(false),
    };
    let comment = gather_comment(args, stdin_consumed);

    // Resolve -g list (SIDs, ranges or serial numbers) into serials to ignore.
    // A malformed range aborts before anything is written: recording the delta
    // with a silently wrong ignore-list would bake the mistake into history.
    let ignored = match &args.glist {
        Some(list) => match sccs.resolve_ignore_list(list) {
            Ok((serials, unresolved)) => {
                for tok in unresolved {
                    diag::error_path(
                        "delta",
                        sfile_path,
                        &format!("{}: {}", tok, gettext("no such delta")),
                    );
                }
                serials
            }
            Err(e) => {
                diag::error_path("delta", sfile_path, &e);
                return Ok(false);
            }
        },
        None => Vec::new(),
    };

    // Create new delta entry
    let new_serial = sccs.max_serial() + 1;
    let new_delta = DeltaEntry {
        delta_type: DeltaType::Normal,
        sid: pfile_entry.new_sid,
        datetime: SccsDateTime::now(),
        user: posixutils_sccs::username(),
        serial: new_serial,
        pred_serial: base_serial,
        stats,
        // The forced inclusions and exclusions belong to this delta's
        // provenance: POSIX mandates prs :Dn:/:Dx: report them, and without
        // them the exclusion reads as an ordinary edit forever after.
        included: forced_in,
        excluded: forced_out,
        ignored,
        mr_numbers: mrs,
        comments: if comment.is_empty() {
            Vec::new()
        } else {
            vec![comment]
        },
    };

    // Print info. The new SID is printed first, then (with -p) the diff in
    // diff-normal format, then the insert/delete/unchanged counts.
    if !args.silent {
        println!("{}", new_delta.sid);
    }
    if args.print_diff {
        print_normal_diff(&base_lines, &new_lines, &diff_ops);
    }
    if !args.silent {
        println!("{} {}", new_delta.stats.inserted, gettext("inserted"));
        println!("{} {}", new_delta.stats.deleted, gettext("deleted"));
        println!("{} {}", new_delta.stats.unchanged, gettext("unchanged"));
    }

    // Apply diff to body
    let new_body = apply_diff_to_body(&sccs, &applied_set, new_serial, &new_lines, &diff_ops);
    sccs.body = new_body;

    // Add new delta to header (at the beginning - deltas are stored newest first)
    sccs.header.deltas.insert(0, new_delta);

    // Write atomically via the x-file, which is registered for SIGINT cleanup
    // around the write+rename so an interrupt removes the temporary.
    sfio::write_xfile_atomic(
        sfile_path,
        &paths::xfile_from_sfile(sfile_path),
        &sccs.to_bytes(),
        sfio::sfile_perms(sfile_path),
    )?;

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

    zlock::install_cleanup();

    let args = Args::parse();

    // When the single operand is '-', the spec requires the comment to be
    // supplied via -y (and the MR list via -m if the v flag is set), since
    // standard input is consumed reading the list of SCCS pathnames.
    let stdin_consumed = args.files.len() == 1 && args.files[0].as_os_str() == "-";
    if stdin_consumed && args.comment.is_none() {
        eprintln!(
            "delta: {}",
            gettext("the -y option is required when the file operand is '-'")
        );
        return ExitCode::FAILURE;
    }

    // Expand operands: lone '-' reads pathnames from stdin; directories expand
    // to their sorted s.* members.
    let files = operands::expand(&args.files);

    let mut exit_code = ExitCode::SUCCESS;

    for file_path in &files {
        match process_file(&args, file_path, stdin_consumed) {
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
