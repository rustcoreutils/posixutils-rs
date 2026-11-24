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
use std::fs;
use std::io::{self, BufRead, IsTerminal, Write};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use gettextrs::gettext;
use plib::linediff::LineOp;
use plib::sccsfile::{
    paths, CommentLine, DeltaEntry, DeltaStats, DeltaType, MrNumber, NotText, PfileEntry,
    SccsDateTime, SccsFile, SccsFlag, Selection, Sid, SidList,
};
use posixutils_sccs::invalid_input;
use posixutils_sccs::txn::{self, Txn};
use posixutils_sccs::{diag, mrlist, operands, protect, zlock};

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
    #[arg(short = 'r', allow_hyphen_values = true, value_name = "SID", help = gettext("SID of delta to create (if multiple edits pending)"))]
    sid: Option<String>,

    #[arg(short = 'y', value_name = "COMMENT", num_args = 0..=1, default_missing_value = "", help = gettext("Comment for delta"))]
    comment: Option<String>,

    #[arg(short = 'm', value_name = "MRLIST", num_args = 0..=1, default_missing_value = "", help = gettext("Modification request (MR) numbers for delta"))]
    mrlist: Option<String>,

    #[arg(short = 'g', allow_hyphen_values = true, value_name = "LIST", help = gettext("List of deltas to ignore at this change level"))]
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
fn find_pfile_entry(txn: &Txn, requested_sid: Option<&Sid>) -> io::Result<PfileEntry> {
    let sfile_path = txn.sfile();
    let entries = txn.pending()?;
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

    let user = posixutils_sccs::username()?;

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
fn read_gfile(sfile_path: &Path) -> io::Result<Vec<u8>> {
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

    fs::read(&gfile_path)
}

/// The g-file's content as the lines to diff against the stored body, or
/// why it cannot be stored.
///
/// An encoded (binary) s-file stores uuencoded text, and `get` writes the
/// decoded bytes to the g-file, so the two are only comparable after encoding
/// the g-file back; any bytes can be stored that way. A text s-file stores
/// the lines themselves, and only what `not_storable_as_text` lets through.
fn gfile_lines(sccs: &SccsFile, content: &[u8]) -> Result<Vec<Vec<u8>>, NotText> {
    if sccs.is_encoded() {
        return Ok(plib::sccsfile::uuencode_sccs(content));
    }
    match plib::sccsfile::not_storable_as_text(content) {
        Some(why) => Err(why),
        None => Ok(plib::sccsfile::split_lines(content)
            .into_iter()
            .map(<[u8]>::to_vec)
            .collect()),
    }
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
    let selection = Selection {
        included,
        excluded,
        cutoff: None,
    };
    sccs.applied_set(base_serial, &selection)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))
}

/// Resolve one of the p-file's `-i`/`-x` option-arguments into delta serials.
///
/// The list was written by `get -e`, which already validated it, so an
/// unresolvable token here means the s-file changed underneath the edit.
fn resolve_pfile_list(
    sccs: &SccsFile,
    list: Option<&SidList>,
    sfile_path: &Path,
) -> Result<Vec<u16>, String> {
    let Some(list) = list else {
        return Ok(Vec::new());
    };
    let (serials, unresolved) = sccs.resolve_sid_list(list.as_str())?;
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
fn compute_diff(base_lines: &[Vec<u8>], new_lines: &[Vec<u8>]) -> (DeltaStats, Vec<LineOp>) {
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

fn print_normal_diff(
    base_lines: &[Vec<u8>],
    new_lines: &[Vec<u8>],
    ops: &[LineOp],
) -> io::Result<()> {
    let mut out = io::stdout().lock();
    // One "< " or "> " line: the text as stored, which need not be UTF-8.
    let line = |out: &mut io::StdoutLock, mark: &[u8], text: &[u8]| -> io::Result<()> {
        out.write_all(mark)?;
        out.write_all(text)?;
        out.write_all(b"\n")
    };
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
                let mut dels: Vec<&[u8]> = Vec::new();
                let mut inss: Vec<&[u8]> = Vec::new();

                while k < ops.len() {
                    match &ops[k] {
                        LineOp::Delete => {
                            dels.push(&base_lines[bi]);
                            bi += 1;
                            k += 1;
                        }
                        _ => break,
                    }
                }
                while k < ops.len() {
                    match &ops[k] {
                        LineOp::Insert(_) => {
                            inss.push(&new_lines[ni]);
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
                    writeln!(
                        out,
                        "{}c{}",
                        range(del_start + 1, nd),
                        range(ins_start + 1, np)
                    )?;
                    for l in &dels {
                        line(&mut out, b"< ", l)?;
                    }
                    writeln!(out, "---")?;
                    for l in &inss {
                        line(&mut out, b"> ", l)?;
                    }
                } else if nd > 0 {
                    // Deletion: lines del_start+1..del_start+nd removed after
                    // new line ins_start.
                    writeln!(out, "{}d{}", range(del_start + 1, nd), ins_start)?;
                    for l in &dels {
                        line(&mut out, b"< ", l)?;
                    }
                } else if np > 0 {
                    // Addition: lines added after base line del_start.
                    writeln!(out, "{}a{}", del_start, range(ins_start + 1, np))?;
                    for l in &inss {
                        line(&mut out, b"> ", l)?;
                    }
                }
            }
        }
    }
    Ok(())
}

/// Remove entry from p-file
fn remove_pfile_entry(txn: &Txn, entry_to_remove: &PfileEntry) -> io::Result<()> {
    let remaining: Vec<_> = txn
        .pending()?
        .into_iter()
        .filter(|e| {
            !(e.old_sid == entry_to_remove.old_sid
                && e.new_sid == entry_to_remove.new_sid
                && e.user == entry_to_remove.user)
        })
        .collect();

    txn.set_pending(&remaining)
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
    let v_flag = sccs.header().flags.iter().find_map(|f| match f {
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
        if !prog.is_empty() && !mrlist::validate("delta", sfile_path, &prog, &mrs) {
            return Ok(None);
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

    // Take the lock before anything of the s-file or p-file is read. If
    // another SCCS command holds it, report and skip.
    let txn = match Txn::begin(sfile_path) {
        Ok(txn) => txn,
        Err(e) if txn::is_locked(&e) => {
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

    let pfile_entry = find_pfile_entry(&txn, requested_sid.as_ref())?;

    let mut sccs = txn.read()?;

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
    let user = posixutils_sccs::username()?;
    if let Err(refusal) = protect::check_edit(&sccs, pfile_entry.old_sid.rel, &user) {
        diag::error_path("delta", sfile_path, &refusal.message());
        return Ok(false);
    }

    // Find the base delta
    let base_delta = sccs
        .find_delta_by_sid(&pfile_entry.old_sid)
        .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, gettext("Base SID not found")))?;
    let base_serial = base_delta.serial;

    // Read g-file. Input must be a text file (POSIX delta 92274): what a
    // text s-file cannot hold is refused, as historical SCCS refuses it,
    // rather than stored as something that reads back differently. The edit
    // stays pending, to be fixed and retried.
    let new_lines = match gfile_lines(&sccs, &read_gfile(sfile_path)?) {
        Ok(lines) => lines,
        Err(why) => {
            let reason = match why {
                NotText::NoFinalNewline => gettext("No newline at end of file"),
                NotText::Nul => gettext("file contains a NUL character"),
                NotText::LeadingSoh(line) => format!(
                    "{} {} {}",
                    gettext("line"),
                    line,
                    gettext("begins with a control character (SOH)")
                ),
            };
            let gfile = paths::gfile_from_sfile(sfile_path).unwrap_or_default();
            diag::error_path(
                "delta",
                sfile_path,
                &format!(
                    "{}: {}; {}",
                    gfile.display(),
                    reason,
                    gettext("not a text file, cannot be stored")
                ),
            );
            return Ok(false);
        }
    };

    // The edit was made against the version `get -e` produced, which means the
    // -i/-x lists it recorded in the p-file, not the plain base.
    let forced_in = match resolve_pfile_list(&sccs, pfile_entry.included.as_ref(), sfile_path) {
        Ok(v) => v,
        Err(e) => {
            diag::error_path("delta", sfile_path, &e);
            return Ok(false);
        }
    };
    let forced_out = match resolve_pfile_list(&sccs, pfile_entry.excluded.as_ref(), sfile_path) {
        Ok(v) => v,
        Err(e) => {
            diag::error_path("delta", sfile_path, &e);
            return Ok(false);
        }
    };

    // Reconstruct base version
    let applied_set = base_applied_set(&sccs, base_serial, &forced_in, &forced_out)?;
    let base_lines = sccs
        .evaluate_body(&applied_set)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;

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
        stats,
        // The forced inclusions and exclusions belong to this delta's
        // provenance: POSIX mandates prs :Dn:/:Dx: report them, and without
        // them the exclusion reads as an ordinary edit forever after.
        included: forced_in,
        excluded: forced_out,
        ignored,
        mr_numbers: mrs
            .iter()
            .map(|mr| MrNumber::new(mr.as_bytes()))
            .collect::<Result<_, _>>()
            .map_err(invalid_input)?,
        comments: if comment.is_empty() {
            Vec::new()
        } else {
            CommentLine::lines(comment.as_bytes())
        },
        ..DeltaEntry::new(
            DeltaType::Normal,
            pfile_entry.new_sid,
            SccsDateTime::now(),
            user,
            new_serial,
            base_serial,
        )
    };

    // Print info. The new SID is printed first, then (with -p) the diff in
    // diff-normal format, then the insert/delete/unchanged counts.
    if !args.silent {
        println!("{}", new_delta.sid);
    }
    if args.print_diff {
        print_normal_diff(&base_lines, &new_lines, &diff_ops)?;
    }
    if !args.silent {
        println!("{} {}", new_delta.stats.inserted, gettext("inserted"));
        println!("{} {}", new_delta.stats.deleted, gettext("deleted"));
        println!("{} {}", new_delta.stats.unchanged, gettext("unchanged"));
    }

    // Apply diff to body
    sccs.add_delta(new_delta, &applied_set, &diff_ops, &new_lines)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;

    // The s-file first, then the p-file, as SCCS orders them: an edit that
    // has become a delta must not be left looking pending for want of the
    // s-file, and the other way round the delta would be lost.
    txn.write(&sccs)?;
    remove_pfile_entry(&txn, &pfile_entry)?;

    // Remove g-file (unless -n)
    if !args.keep_gfile {
        if let Some(gfile_path) = paths::gfile_from_sfile(sfile_path) {
            let _ = fs::remove_file(gfile_path);
        }
    }

    Ok(true)
}

fn main() -> ExitCode {
    plib::diag::init_locale("delta");

    zlock::install_cleanup();

    let args = plib::optarg::parse::<Args>();

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
                eprintln!(
                    "delta: {}: {}",
                    file_path.display(),
                    plib::diag::io_error_text(&e)
                );
                exit_code = ExitCode::FAILURE;
            }
        }
    }

    exit_code
}
