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
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use gettextrs::gettext;
use plib::sccsfile::{
    paths, DeltaEntry, PfileEntry, SccsDateTime, SccsFile, SccsFlag, Selection, Sid, SidList,
};
use posixutils_sccs::txn::{self, Txn};
use posixutils_sccs::{cutoff, diag, idkw, operands, pfile, protect, sfio, zlock};

/// get - get a version of an SCCS file
#[derive(Parser)]
#[command(version, about = gettext("get - get a version of an SCCS file"))]
struct Args {
    #[arg(short = 'r', allow_hyphen_values = true, value_name = "SID", help = gettext("SID to retrieve"))]
    sid: Option<String>,

    #[arg(short = 'c', allow_hyphen_values = true, value_name = "CUTOFF", help = gettext("Cutoff date-time; exclude deltas created after it"))]
    cutoff: Option<String>,

    #[arg(short = 'i', allow_hyphen_values = true, value_name = "LIST", help = gettext("List of deltas (SIDs) to include"))]
    include: Option<String>,

    #[arg(short = 'x', allow_hyphen_values = true, value_name = "LIST", help = gettext("List of deltas (SIDs) to exclude"))]
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

/// Find the delta to retrieve based on -r option.
///
/// Implements the "SID Retrieved" column of the POSIX get(1) SID table
/// (99233-99253). A release-only `-r R` is not merely a lookup: R may name a
/// release that does not exist yet, in which case the retrieved delta comes
/// from a neighbouring release (mR above, hR below).
fn find_target_delta<'a>(
    sccs: &'a SccsFile,
    requested_sid: Option<&str>,
) -> Result<&'a DeltaEntry, String> {
    match requested_sid {
        None => {
            // No SID specified: get trunk head
            sccs.get_trunk_head()
                .ok_or_else(|| gettext("No deltas in file"))
        }
        Some(sid_str) => {
            let sid: Sid = sid_str
                .parse()
                .map_err(|_| format!("{}: {}", gettext("Invalid SID"), sid_str))?;

            // Try exact match first. A removed delta is not found: it has
            // no text to retrieve.
            if let Some(delta) = sccs.find_delta_by_sid(&sid) {
                return Ok(delta);
            }

            if sid.is_partial() {
                // "R": the highest level within release R (R.mL).
                if let Some(d) = sccs
                    .live_deltas()
                    .filter(|d| d.sid.rel == sid.rel && d.sid.is_trunk())
                    .max_by(|a, b| a.sid.cmp(&b.sid))
                {
                    return Ok(d);
                }

                // Release R holds no applied delta. R > mR retrieves the trunk
                // head mR.mL (and goes on to create R.1); R < mR retrieves
                // hR.mL, hR being the highest existing release below R.
                let trunk_head = sccs
                    .get_trunk_head()
                    .ok_or_else(|| gettext("No deltas in file"))?;
                if sid.rel > trunk_head.sid.rel {
                    return Ok(trunk_head);
                }
                sccs.live_deltas()
                    .filter(|d| d.sid.is_trunk() && d.sid.rel < sid.rel)
                    .max_by(|a, b| a.sid.cmp(&b.sid))
                    .ok_or_else(|| format!("{} {}", gettext("No delta found for release"), sid.rel))
            } else if sid.br != 0 && sid.seq == 0 {
                // "R.L.B": the highest sequence on that branch (R.L.B.mS).
                sccs.live_deltas()
                    .filter(|d| d.sid.same_branch(&sid))
                    .max_by(|a, b| a.sid.cmp(&b.sid))
                    .ok_or_else(|| format!("{} {} {}", gettext("SID"), sid, gettext("not found")))
            } else {
                // R.L and R.L.B.S must match an applied delta exactly.
                Err(format!(
                    "{} {} {}",
                    gettext("SID"),
                    sid,
                    gettext("not found")
                ))
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
            let sid: Sid = s
                .parse()
                .map_err(|_| format!("{}: {}", gettext("Invalid SID"), s))?;
            Some(sid.rel)
        }
    };

    sccs.live_deltas()
        .filter(|d| release.is_none_or(|r| d.sid.rel == r))
        .max_by(|a, b| {
            let ka = (
                cutoff::full_year(&a.datetime),
                a.datetime.month,
                a.datetime.day,
                a.datetime.hour,
                a.datetime.minute,
                a.datetime.second,
                a.serial,
            );
            let kb = (
                cutoff::full_year(&b.datetime),
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
            Some(r) => format!("{} {}", gettext("No delta found for release"), r),
            None => gettext("No deltas in file"),
        })
}

/// Does any delta already succeed `target` on the line it sits on?
///
/// For a trunk delta that means any later trunk delta, in this release or a
/// higher one; for a branch delta, a higher sequence on the same branch. A
/// delta removed by rmdel is not one: it is gone, and the next delta takes
/// its SID (`SccsFile::live_deltas`).
fn has_successor(sccs: &SccsFile, target: &DeltaEntry) -> bool {
    sccs.live_deltas().any(|d| {
        if target.sid.is_trunk() {
            d.sid.is_trunk() && d.sid > target.sid
        } else {
            d.sid.same_branch(&target.sid) && d.sid.seq > target.sid.seq
        }
    })
}

/// Compute the SID of the delta to be created, i.e. the "SID of Delta to be
/// Created" column of the POSIX get(1) SID table (99233-99253).
///
/// The table reduces to one rule plus one special case: branch off the
/// retrieved delta whenever -b was given (and the b flag permits it) or the
/// retrieved delta already has a successor; otherwise extend its line. The
/// special case is a release-only -r naming a release above the newest one,
/// which forces the first delta of that new release.
fn compute_new_sid(
    sccs: &SccsFile,
    target: &DeltaEntry,
    create_branch: bool,
    requested_sid: Option<&Sid>,
) -> Sid {
    let branching = create_branch && sccs.branch_enabled();

    // "R > mR": force creation of the first delta in a new release (99237).
    // -b outranks it, per 99239.
    if !branching {
        if let Some(req) = requested_sid {
            if req.is_partial() && req.rel > target.sid.rel {
                return Sid::trunk(req.rel, 1);
            }
        }
    }

    if branching || has_successor(sccs, target) {
        // Branch off the retrieved delta: R.L.(mB+1).1
        let max_branch = sccs
            .live_deltas()
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
fn check_pfile_lock(txn: &Txn, new_sid: &Sid) -> Result<(), String> {
    let entries = txn
        .pending()
        .map_err(|e| format!("{}: {}", gettext("Invalid p-file"), e))?;

    match pfile::find_by_new_sid(&entries, new_sid) {
        Some(entry) => Err(format!(
            "{} {} {} {}",
            gettext("SID"),
            new_sid,
            gettext("is being edited by"),
            entry.user
        )),
        None => Ok(()),
    }
}

/// Record the edit in the p-file, after those already pending.
fn create_pfile_entry(
    txn: &Txn,
    old_sid: &Sid,
    new_sid: &Sid,
    included: Option<SidList>,
    excluded: Option<SidList>,
) -> io::Result<()> {
    let entry = PfileEntry {
        old_sid: *old_sid,
        new_sid: *new_sid,
        user: posixutils_sccs::username()?,
        datetime: SccsDateTime::now(),
        included,
        excluded,
    };

    let mut entries = txn.pending()?;
    entries.push(entry);
    txn.set_pending(&entries)
}

/// Render the delta-summary table (the l-file body). Deltas are listed newest
/// first. Each entry is a status line followed by comment lines indented one
/// tab and terminated by a blank line, per the POSIX l-file format:
///   "%c%c%c %s\t%s %s\n", code1, code2, code3, SID, date-time, login
fn render_lfile(sccs: &SccsFile, applied_set: &HashSet<u16>) -> Vec<u8> {
    let mut out = Vec::new();
    // Newest delta first: iterate in descending serial order.
    let mut deltas: Vec<&DeltaEntry> = sccs.live_deltas().collect();
    deltas.sort_by_key(|d| std::cmp::Reverse(d.serial));

    for d in deltas {
        let code1 = if applied_set.contains(&d.serial) {
            b' '
        } else {
            b'*'
        };
        // code2/code3 special-reason codes are unsupported here; use spaces.
        out.extend_from_slice(&[code1, b' ', b' ', b' ']);
        out.extend_from_slice(
            format!(
                "{}\t{} {} ",
                d.sid,
                d.datetime.date_string(),
                d.datetime.time_string()
            )
            .as_bytes(),
        );
        out.extend_from_slice(&d.user);
        out.push(b'\n');
        for c in &d.comments {
            out.push(b'\t');
            out.extend_from_slice(c);
            out.push(b'\n');
        }
        out.push(b'\n');
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
        Stdout.write_all(&table)?;
    } else {
        // The l-file is read-only, so it must be replaced rather than
        // reopened: writing it a second time in the same directory otherwise
        // fails with EACCES on the file this same code wrote.
        let lfile_path = paths::lfile_from_sfile(sfile_path);
        sfio::write_replacing(&lfile_path, &table, 0o444)?;
    }
    Ok(())
}

/// An s-file that cannot be read, as the error get reports.
fn unreadable(e: plib::sccsfile::SccsError) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, e.to_string())
}

/// The `-i` and `-x` lists of the command line, checked: what the version is
/// made from, and what an edit records in the p-file for delta.
struct Lists {
    include: Option<SidList>,
    exclude: Option<SidList>,
}

impl Lists {
    fn from_args(args: &Args) -> Result<Lists, plib::sccsfile::SccsError> {
        let list = |arg: &Option<String>| {
            arg.as_deref()
                .map(|l| SidList::new(l.as_bytes()))
                .transpose()
        };
        Ok(Lists {
            include: list(&args.include)?,
            exclude: list(&args.exclude)?,
        })
    }
}

fn process_file(
    args: &Args,
    lists: &Lists,
    sfile_path: &Path,
    multiple_files: bool,
) -> io::Result<bool> {
    // Print filename if multiple files
    if multiple_files && !args.silent {
        let output_stream: &mut dyn Write = if args.to_stdout || args.lfile_stdout {
            &mut io::stderr()
        } else {
            &mut Stdout
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

    // An edit changes SCCS state (the p-file), so it takes the lock before
    // the s-file is opened and holds it until this function returns: the
    // new SID, the edit's permission and the p-file entry are all decided
    // from the file as it is under the lock. A plain retrieval changes
    // nothing and takes no lock.
    let txn = if args.edit {
        match Txn::begin(sfile_path) {
            Ok(txn) => Some(txn),
            Err(e) if txn::is_locked(&e) => {
                diag::error_path("get", sfile_path, &gettext("being edited"));
                return Ok(false);
            }
            Err(e) => return Err(e),
        }
    } else {
        None
    };
    let sccs = match &txn {
        Some(txn) => txn.read()?,
        None => SccsFile::from_path(sfile_path).map_err(unreadable)?,
    };

    // The `d` flag names "the default delta number (SID) to be used by a get
    // command" (POSIX 84036), so it stands in for -r when -r is absent. An
    // explicit -r still outranks it.
    let default_sid = sccs.default_sid().map(|s| s.to_string());
    let requested_sid: Option<&str> = args.sid.as_deref().or(default_sid.as_deref());

    // Find target delta. With -t, access the most recently created (top)
    // delta in the requested release (or overall when no -r is given).
    let target = if args.top {
        find_top_delta(&sccs, requested_sid)
            .map_err(|e| io::Error::new(io::ErrorKind::NotFound, e))?
    } else {
        find_target_delta(&sccs, requested_sid)
            .map_err(|e| io::Error::new(io::ErrorKind::NotFound, e))?
    };

    let target_serial = target.serial;
    let target_sid = target.sid;

    // If editing, compute the new SID and check for the p-file edit lock.
    let new_sid = if let Some(txn) = &txn {
        // Ceiling, floor, locked releases and the authorized-user list are
        // enforced here and not for a plain retrieval: POSIX 99077-99078
        // attaches them to -e. A refusal leaves no p-file entry behind, and
        // the lock goes when this function returns.
        let user = posixutils_sccs::username()?;
        if let Err(refusal) = protect::check_edit(&sccs, target_sid.rel, &user) {
            diag::error_path("get", sfile_path, &refusal.message());
            return Ok(false);
        }

        let requested: Option<Sid> = requested_sid.and_then(|s| s.parse().ok());
        let new_sid = compute_new_sid(&sccs, target, args.branch, requested.as_ref());

        // Check for existing edit lock (unless joint edit is allowed)
        if !sccs.joint_edit() {
            check_pfile_lock(txn, &new_sid)
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
    // A malformed range is fatal: silently retrieving a version the caller did
    // not ask for is worse than refusing, and the g-file would otherwise be
    // written from the wrong delta set.
    let mut list_error = None;
    let mut resolve = |list: &Option<SidList>| -> Vec<u16> {
        match list.as_ref().map(|l| sccs.resolve_sid_list(l.as_str())) {
            Some(Ok((serials, unresolved))) => {
                for tok in unresolved {
                    diag::error_path(
                        "get",
                        sfile_path,
                        &format!("{}: {}", tok, gettext("no such delta")),
                    );
                }
                serials
            }
            Some(Err(e)) => {
                list_error.get_or_insert(e);
                Vec::new()
            }
            None => Vec::new(),
        }
    };
    let included_serials: Vec<u16> = resolve(&lists.include);
    let excluded_serials: Vec<u16> = resolve(&lists.exclude);
    if let Some(e) = list_error {
        diag::error_path("get", sfile_path, &e);
        return Ok(false);
    }

    // The deltas applied: the target's line of predecessors, as cut off by
    // -c and adjusted by -i and -x.
    let cutoff = match args.cutoff.as_deref() {
        Some(text) => Some(cutoff::parse(text).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("{}: '{}'", gettext("Invalid cutoff date"), text),
            )
        })?),
        None => None,
    };
    let after_cutoff = |delta: &DeltaEntry| cutoff.is_some_and(|c| c.is_after(delta));
    let selection = Selection {
        included: &included_serials,
        excluded: &excluded_serials,
        cutoff: Some(&after_cutoff),
    };
    let applied_set = sccs
        .applied_set(target_serial, &selection)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;

    // The Excluded: notation lists each excluded delta and every delta made
    // from one, matching historical SCCS bookkeeping; only the deltas named
    // are left out of the version.
    let mut excluded_effective: Vec<u16> = Vec::new();
    if !excluded_serials.is_empty() {
        excluded_effective = sccs
            .live_deltas()
            .map(|d| d.serial)
            .filter(|&s| {
                excluded_serials
                    .iter()
                    .any(|&x| sccs.is_descendant_of(s, x))
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
            &mut Stdout
        };

        writeln!(output_stream, "{}", target_sid)?;
        if let Some(ref ns) = new_sid {
            writeln!(output_stream, "{} {}", gettext("new delta"), ns)?;
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

    // Record the pending edit. This happens before retrieval, not after: -g
    // suppresses the g-file but not the lock, and `get -e -g` announcing a new
    // delta while leaving no p-file behind would hand the caller a lock that
    // does not exist.
    if let Some(txn) = &txn {
        if let Some(ns) = new_sid {
            create_pfile_entry(
                txn,
                &target_sid,
                &ns,
                lists.include.clone(),
                lists.exclude.clone(),
            )?;
        }
    }

    // If -g, skip actual retrieval
    if args.no_get {
        return Ok(true);
    }

    // Get module name for -n option
    let module_name = idkw::module_name_of(&sccs, sfile_path);

    // Encoded (binary / no-trailing-newline) bodies: uudecode and emit raw
    // bytes, with no ID-keyword expansion (the content is opaque).
    if sccs.is_encoded() {
        let encoded_lines = sccs.evaluate_body(&applied_set).map_err(unreadable)?;
        let raw = plib::sccsfile::uudecode_sccs(&encoded_lines);

        if args.to_stdout {
            Stdout.write_all(&raw)?;
        } else {
            let gfile_path = paths::gfile_from_sfile(sfile_path).ok_or_else(|| {
                io::Error::new(io::ErrorKind::InvalidInput, gettext("Invalid s-file name"))
            })?;
            let mode = if args.edit { 0o644 } else { 0o444 };
            sfio::write_replacing(&gfile_path, &raw, mode)?;
        }

        if !args.silent {
            let line_count = raw.iter().filter(|&&b| b == b'\n').count();
            let output_stream: &mut dyn Write = if info_to_stderr {
                &mut io::stderr()
            } else {
                &mut Stdout
            };
            writeln!(output_stream, "{} {}", line_count, gettext("lines"))?;
        }

        return Ok(true);
    }

    // Evaluate body
    let suppress_keywords = args.no_keywords || args.edit;

    // Track whether any ID keyword (%X%) appeared in the retrieved text.
    let mut had_keyword = false;

    let target_delta = sccs.find_delta_by_serial(target_serial).unwrap();
    let keyword_values = idkw::KeywordValues::new(&sccs, target_delta, sfile_path);

    // The body, each line paired with the SID of the delta that inserted it
    // when -m wants that prefix.
    let body: Vec<(Vec<u8>, Option<Sid>)> = if args.show_sid {
        sccs.evaluate_body_annotated(&applied_set)
            .map_err(unreadable)?
            .into_iter()
            .map(|(serial, line)| {
                // The inserting delta, as its SID.
                let sid = sccs
                    .find_delta_by_serial(serial)
                    .map_or(Sid::trunk(1, 1), |d| d.sid);
                (line, Some(sid))
            })
            .collect()
    } else {
        sccs.evaluate_body(&applied_set)
            .map_err(unreadable)?
            .into_iter()
            .map(|line| (line, None))
            .collect()
    };

    let lines: Vec<Vec<u8>> = body
        .into_iter()
        .enumerate()
        .map(|(i, (line, sid))| {
            if idkw::contains(&line) {
                had_keyword = true;
            }
            let expanded = if suppress_keywords {
                line
            } else {
                idkw::expand(&line, &keyword_values, i + 1)
            };

            // -n and -m prefixes, each followed by a tab, module name first.
            let mut out = Vec::new();
            if args.show_module {
                out.extend_from_slice(&module_name);
                out.push(b'\t');
            }
            if let Some(sid) = sid {
                out.extend_from_slice(sid.to_string().as_bytes());
                out.push(b'\t');
            }
            out.extend_from_slice(&expanded);
            out
        })
        .collect();

    // "No id keywords" handling. When no keyword was present (and keyword
    // expansion was not suppressed), SCCS warns. If the s-file has the 'i'
    // flag set, this is escalated to a fatal error: no g-file is written.
    if !had_keyword && !suppress_keywords {
        let has_i_flag = sccs
            .header()
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
        for line in &lines {
            Stdout.write_all(line)?;
            Stdout.write_all(b"\n")?;
        }
    } else {
        // Write to g-file
        let gfile_path = paths::gfile_from_sfile(sfile_path).ok_or_else(|| {
            io::Error::new(io::ErrorKind::InvalidInput, gettext("Invalid s-file name"))
        })?;

        // A non-edit g-file is read-only, so the previous run's g-file must be
        // replaced rather than reopened: `File::create` on it fails EACCES,
        // which made a second `get` of any file impossible.
        let mut content = Vec::new();
        for line in &lines {
            content.extend_from_slice(line);
            content.push(b'\n');
        }
        let mode = if args.edit { 0o644 } else { 0o444 };
        sfio::write_replacing(&gfile_path, &content, mode)?;
    }

    // Print line count (unless silent)
    if !args.silent {
        let output_stream: &mut dyn Write = if info_to_stderr {
            &mut io::stderr()
        } else {
            &mut Stdout
        };
        writeln!(output_stream, "{} {}", line_count, gettext("lines"))?;
    }

    Ok(true)
}

/// Standard output, whose write failures are marked as [`StdoutWriteError`]
/// so that `main` reports them as write errors rather than against the
/// s-file being processed.
struct Stdout;

impl Write for Stdout {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        io::stdout().write(buf).map_err(StdoutWriteError::mark)
    }

    fn flush(&mut self) -> io::Result<()> {
        io::stdout().flush().map_err(StdoutWriteError::mark)
    }
}

/// A failed write to standard output, carried inside an [`io::Error`].
#[derive(Debug)]
struct StdoutWriteError(io::Error);

impl StdoutWriteError {
    fn mark(e: io::Error) -> io::Error {
        io::Error::new(e.kind(), StdoutWriteError(e))
    }
}

impl std::fmt::Display for StdoutWriteError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&plib::diag::io_error_text(&self.0))
    }
}

impl std::error::Error for StdoutWriteError {}

fn main() -> ExitCode {
    plib::diag::init_locale("get");

    // `get -e` takes the z-file lock and registers it for cleanup; without the
    // handler installed the registry is inert and ^C strands z.<name>.
    zlock::install_cleanup();

    let args = plib::optarg::parse::<Args>();
    let lists = match Lists::from_args(&args) {
        Ok(lists) => lists,
        Err(e) => {
            eprintln!("get: {e}");
            return ExitCode::FAILURE;
        }
    };

    // Expand operands: a lone "-" reads s-file pathnames from stdin, and
    // directory operands expand to their sorted s.* members.
    let files = operands::expand(&args.files);

    // Per spec, when a directory or standard input ("-") is named, each
    // pathname is written before its output even if it resolves to a single
    // file.
    let multiple_files = operands::wants_banner(&args.files, &files);
    let mut exit_code = ExitCode::SUCCESS;

    for file_path in &files {
        match process_file(&args, &lists, file_path, multiple_files) {
            Ok(true) => {}
            Ok(false) => exit_code = ExitCode::FAILURE,
            // Every later file would fail the same way.
            Err(e)
                if e.get_ref()
                    .is_some_and(|inner| inner.is::<StdoutWriteError>()) =>
            {
                diag::error("get", &format!("{}: {e}", gettext("write error")));
                return ExitCode::FAILURE;
            }
            Err(e) => {
                diag::error_path("get", file_path, &e.to_string());
                exit_code = ExitCode::FAILURE;
            }
        }
    }

    exit_code
}
