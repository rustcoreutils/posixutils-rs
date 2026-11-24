//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! prs - print SCCS file information

use std::ffi::{OsStr, OsString};
use std::io::{self, Write};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use gettextrs::gettext;
use plib::sccsfile::{paths, DeltaEntry, DeltaType, SccsFile, SccsFlag, Sid};
use posixutils_sccs::optscan::{self, Letters};
use posixutils_sccs::{cutoff, diag, idkw, operands};

/// prs - print SCCS file information
#[derive(Parser)]
#[command(version, about = gettext("prs - print SCCS file information"))]
struct Args {
    #[arg(short = 'a', help = gettext("Include removed deltas"))]
    all_deltas: bool,

    #[arg(short = 'c', allow_hyphen_values = true, value_name = "CUTOFF", help = gettext("Cutoff date/time (YY[MM[DD[HH[MM[SS]]]]])"))]
    cutoff: Option<String>,

    #[arg(short = 'd', allow_hyphen_values = true, value_name = "DATASPEC", help = gettext("Data format specification"))]
    dataspec: Option<OsString>,

    #[arg(short = 'e', help = gettext("Select deltas earlier than or equal to -c or -r"))]
    earlier: bool,

    #[arg(short = 'l', help = gettext("Select deltas later than or equal to -c or -r"))]
    later: bool,

    /// `-r[SID]`: taken from the command line by [`optscan`], its argument
    /// being optional and attached. A bare `-r` is the empty string, the
    /// most recently created delta.
    #[arg(skip)]
    sid: Option<String>,

    #[arg(required = true, help = gettext("SCCS files to process (use - for stdin)"))]
    files: Vec<PathBuf>,
}

// Default output format (per POSIX, there's a header and per-delta format)
const DEFAULT_HEADER_FORMAT: &[u8] = b":PN::\n\n";
const DEFAULT_DELTA_FORMAT: &[u8] = b":Dt:\t:DL:\nMRs:\n:MR:COMMENTS:\n:C:\n";

fn flag_yes_no(val: bool) -> &'static str {
    if val {
        "yes"
    } else {
        "no"
    }
}

/// Multi-line ("M-format") keywords. Their values are emitted with each line
/// terminated by a <newline>, and an M-format keyword appearing as the final
/// token of a dataspec forces a trailing <newline> on the delta's output.
fn is_multiline_keyword(keyword: &str) -> bool {
    matches!(keyword, "MR" | "C" | "UN" | "FL" | "FD" | "GB")
}

/// Whether `keyword` is a recognized prs data keyword.
fn is_known_keyword(keyword: &str) -> bool {
    matches!(
        keyword,
        "BF" | "CB"
            | "Ds"
            | "F"
            | "FB"
            | "FD"
            | "FL"
            | "J"
            | "KF"
            | "KV"
            | "LK"
            | "M"
            | "MF"
            | "MP"
            | "ND"
            | "PN"
            | "Q"
            | "UN"
            | "Y"
            | "A"
            | "B"
            | "C"
            | "D"
            | "Dd"
            | "Dg"
            | "DI"
            | "DL"
            | "Dm"
            | "Dn"
            | "DP"
            | "DS"
            | "Dt"
            | "DT"
            | "Dx"
            | "Dy"
            | "GB"
            | "I"
            | "L"
            | "Ld"
            | "Li"
            | "Lu"
            | "MR"
            | "P"
            | "R"
            | "S"
            | "T"
            | "Th"
            | "Tm"
            | "Ts"
            | "W"
            | "Z"
    )
}

/// Expand data keywords in format string for a specific delta. Returns the
/// expanded text along with a flag indicating whether the last token emitted
/// was a multi-line ("M-format") keyword.
fn expand_keywords(
    format: &[u8],
    sccs: &SccsFile,
    delta: &DeltaEntry,
    sfile_path: &Path,
) -> (Vec<u8>, bool) {
    let mut result: Vec<u8> = Vec::new();
    let mut last_was_multiline = false;
    // The specification is bytes: what is not a keyword or an escape is
    // written as it stands.
    let chars = format;
    let mut i = 0;

    while i < chars.len() {
        let c = chars[i];
        if c == b':' {
            // Try to match a recognized keyword: ':' kw ':'.
            let mut j = i + 1;
            while j < chars.len() && chars[j].is_ascii_alphanumeric() {
                j += 1;
            }
            let keyword = std::str::from_utf8(&chars[i + 1..j]).unwrap_or_default();
            if !keyword.is_empty()
                && j < chars.len()
                && chars[j] == b':'
                && is_known_keyword(keyword)
            {
                // Recognized keyword - expand it.
                let value = expand_single_keyword(keyword, sccs, delta, sfile_path);
                let multiline = is_multiline_keyword(keyword);
                if multiline {
                    // Each value line is newline-terminated; an empty value
                    // (e.g. :MR: for a delta with no MRs) emits nothing, not a
                    // blank line.
                    if !value.is_empty() {
                        for line in value.split(|&b| b == b'\n') {
                            result.extend_from_slice(line);
                            result.push(b'\n');
                        }
                    }
                } else {
                    result.extend_from_slice(&value);
                }
                last_was_multiline = multiline && !value.is_empty();
                i = j + 1;
                continue;
            }
            // Not a recognized keyword - emit the ':' literally and continue
            // scanning from the next character (so "::" prints verbatim).
            result.push(b':');
            last_was_multiline = false;
            i += 1;
        } else if c == b'\\' {
            // Handle escape sequences.
            match chars.get(i + 1) {
                Some(b'n') => {
                    result.push(b'\n');
                    i += 2;
                }
                Some(b't') => {
                    result.push(b'\t');
                    i += 2;
                }
                Some(b'\\') => {
                    result.push(b'\\');
                    i += 2;
                }
                _ => {
                    result.push(c);
                    i += 1;
                }
            }
            last_was_multiline = false;
        } else {
            result.push(c);
            last_was_multiline = false;
            i += 1;
        }
    }

    (result, last_was_multiline)
}

/// The value of one data keyword, as bytes: the keywords that print what the
/// s-file stores -- comments, MRs, users, descriptive text, flag values, the
/// body -- print it as stored, which need not be UTF-8. The rest are text prs
/// composes, from [`keyword_text`].
fn expand_single_keyword(
    keyword: &str,
    sccs: &SccsFile,
    delta: &DeltaEntry,
    sfile_path: &Path,
) -> Vec<u8> {
    let text = |kw: &str| keyword_text(kw, sccs, delta).into_bytes();
    let stored = |v: Option<&[u8]>| v.unwrap_or(b"").to_vec();
    // A list, one entry per line, or "none" when there is no entry.
    let list_or_none = |v: &[Vec<u8>]| {
        if v.is_empty() {
            b"none".to_vec()
        } else {
            v.join(&b"\n"[..])
        }
    };
    let module = || idkw::module_name_of(sccs, sfile_path);
    match keyword {
        "FD" => list_or_none(&sccs.header().descriptive_text),
        "FL" => format_flags(&sccs.header().flags),
        "KV" => stored(sccs.id_keyword_check().flatten()),
        "M" => module(),
        // Names are bytes. :PN: is "the SCCS-file pathname" as given (the
        // operand), not resolved.
        "F" => sfile_path
            .file_name()
            .unwrap_or_default()
            .as_bytes()
            .to_vec(),
        "PN" => sfile_path.as_os_str().as_bytes().to_vec(),
        "MP" => stored(sccs.mr_program().flatten()),
        "Q" => stored(sccs.q_text()),
        "UN" => list_or_none(&sccs.header().users),
        "Y" => stored(sccs.module_type()),
        "A" => [
            text("Z"),
            stored(sccs.module_type()),
            b" ".to_vec(),
            module(),
            b" ".to_vec(),
            text("I"),
            text("Z"),
        ]
        .concat(),
        "C" => delta.comments.join(&b"\n"[..]),
        "Dt" => [
            text("DT"),
            b" ".to_vec(),
            text("I"),
            b" ".to_vec(),
            text("D"),
            b" ".to_vec(),
            text("T"),
            b" ".to_vec(),
            delta.user.to_vec(),
            b" ".to_vec(),
            text("DS"),
            b" ".to_vec(),
            text("DP"),
        ]
        .concat(),
        "GB" => gotten_body(sccs, delta, sfile_path),
        "MR" => delta.mr_numbers.join(&b"\n"[..]),
        "P" => delta.user.to_vec(),
        "W" => [text("Z"), module(), b"\t".to_vec(), text("I")].concat(),
        _ => text(keyword),
    }
}

fn keyword_text(keyword: &str, sccs: &SccsFile, delta: &DeltaEntry) -> String {
    match keyword {
        // Global keywords
        "BF" => flag_yes_no(has_flag(&sccs.header().flags, |f| {
            matches!(f, SccsFlag::BranchEnabled)
        }))
        .to_string(),
        "CB" => sccs
            .header()
            .flags
            .iter()
            .find_map(|f| match f {
                SccsFlag::Ceiling(v) => Some(v.to_string()),
                _ => None,
            })
            .unwrap_or_else(|| "none".to_string()),
        "Ds" => sccs
            .default_sid()
            .map(|s| s.to_string())
            .unwrap_or_else(|| "none".to_string()),
        "FB" => sccs
            .header()
            .flags
            .iter()
            .find_map(|f| match f {
                SccsFlag::Floor(v) => Some(v.to_string()),
                _ => None,
            })
            .unwrap_or_else(|| "none".to_string()),
        "J" => flag_yes_no(has_flag(&sccs.header().flags, |f| {
            matches!(f, SccsFlag::JointEdit)
        }))
        .to_string(),
        "KF" => flag_yes_no(has_flag(&sccs.header().flags, |f| {
            matches!(f, SccsFlag::IdKeywordError(_))
        }))
        .to_string(),
        // An absent `l` flag is "none"; `l a` locks every release and renders
        // as "a", matching CSSC. Encoding "all" as an empty list made the most
        // restrictive setting a file can carry report as the least.
        "LK" => sccs
            .locked_releases()
            .map(|lock| lock.value_string())
            .unwrap_or_else(|| "none".to_string()),
        "MF" => flag_yes_no(has_flag(&sccs.header().flags, |f| {
            matches!(f, SccsFlag::MrValidation(_))
        }))
        .to_string(),
        "ND" => flag_yes_no(has_flag(&sccs.header().flags, |f| {
            matches!(f, SccsFlag::NullDelta)
        }))
        .to_string(),

        // Version-specific keywords
        "B" => {
            if delta.sid.br > 0 {
                delta.sid.br.to_string()
            } else {
                String::new()
            }
        }
        // Gotten body: reconstruct this delta's source text.
        "D" => delta.datetime.date_string(),
        "Dd" => format!("{:02}", delta.datetime.day),
        "Dg" => delta
            .ignored
            .iter()
            .map(|n| n.to_string())
            .collect::<Vec<_>>()
            .join(" "),
        "DI" => {
            let inc = delta
                .included
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            let exc = delta
                .excluded
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            let ign = delta
                .ignored
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
                .join(" ");
            if exc.is_empty() && ign.is_empty() {
                inc
            } else if ign.is_empty() {
                format!("{}/{}", inc, exc)
            } else {
                format!("{}/{}/{}", inc, exc, ign)
            }
        }
        "DL" => format!(
            "{:05}/{:05}/{:05}",
            delta.stats.inserted.min(99999),
            delta.stats.deleted.min(99999),
            delta.stats.unchanged.min(99999)
        ),
        "Dm" => format!("{:02}", delta.datetime.month),
        "Dn" => delta
            .included
            .iter()
            .map(|n| n.to_string())
            .collect::<Vec<_>>()
            .join(" "),
        "DP" => delta.pred_serial.to_string(),
        "DS" => delta.serial.to_string(),
        "DT" => match delta.delta_type {
            DeltaType::Normal => "D",
            DeltaType::Removed => "R",
        }
        .to_string(),
        "Dx" => delta
            .excluded
            .iter()
            .map(|n| n.to_string())
            .collect::<Vec<_>>()
            .join(" "),
        "Dy" => format!("{:02}", delta.datetime.year % 100),
        "I" => delta.sid.to_string(),
        "L" => delta.sid.lev.to_string(),
        "Ld" => format!("{:05}", delta.stats.deleted.min(99999)),
        "Li" => format!("{:05}", delta.stats.inserted.min(99999)),
        "Lu" => format!("{:05}", delta.stats.unchanged.min(99999)),
        "R" => delta.sid.rel.to_string(),
        "S" => {
            if delta.sid.seq > 0 {
                delta.sid.seq.to_string()
            } else {
                String::new()
            }
        }
        "T" => delta.datetime.time_string(),
        "Th" => format!("{:02}", delta.datetime.hour),
        "Tm" => format!("{:02}", delta.datetime.minute),
        "Ts" => format!("{:02}", delta.datetime.second),
        "Z" => "@(#)".to_string(),

        // Unknown keyword - return as-is
        _ => format!(":{}:", keyword),
    }
}

/// The gotten body of `delta`: its text as `get` retrieves it, with the
/// identification keywords expanded. An encoded file's bytes are opaque and
/// have none.
///
/// The body is known to be readable: `prs_file` checks the weave before it
/// expands a dataspec that asks for `:GB:`.
fn gotten_body(sccs: &SccsFile, delta: &DeltaEntry, sfile_path: &Path) -> Vec<u8> {
    let Ok(applied_set) = sccs.compute_applied_set(delta.serial) else {
        return Vec::new();
    };
    let lines = sccs.evaluate_body(&applied_set).unwrap_or_default();

    if sccs.is_encoded() {
        let encoded = lines;
        let mut raw = plib::sccsfile::uudecode_sccs(&encoded);
        // The M-format expander newline-terminates each line, so drop a single
        // trailing newline to avoid emitting a spurious blank line.
        if raw.ends_with(b"\n") {
            raw.pop();
        }
        raw
    } else {
        let values = idkw::KeywordValues::new(sccs, delta, sfile_path);
        lines
            .iter()
            .enumerate()
            .map(|(i, line)| idkw::expand(line, &values, i + 1))
            .collect::<Vec<_>>()
            .join(&b"\n"[..])
    }
}

fn has_flag<F>(flags: &[SccsFlag], predicate: F) -> bool
where
    F: Fn(&SccsFlag) -> bool,
{
    flags.iter().any(predicate)
}

fn format_flags(flags: &[SccsFlag]) -> Vec<u8> {
    flags
        .iter()
        .filter_map(|f| f.prs_fl_line())
        .collect::<Vec<_>>()
        .join(&b"\n"[..])
}

fn prs_file(sfile: &Path, args: &Args) -> io::Result<bool> {
    // Check if it's a valid s-file
    if !paths::is_sfile(sfile) {
        eprintln!("{}: {}", sfile.display(), gettext("not an SCCS file"));
        return Ok(false);
    }

    // Parse the SCCS file
    let sccs = match SccsFile::from_path(sfile) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{}: {}", sfile.display(), e);
            return Ok(false);
        }
    };

    let use_default_format = args.dataspec.is_none();
    let format = args
        .dataspec
        .as_deref()
        .map_or(DEFAULT_DELTA_FORMAT, OsStr::as_bytes);

    // :GB: reads the body, which a damaged weave makes unreadable.
    if format.windows(4).any(|w| w == b":GB:") {
        if let Err(e) = sccs.check_weave() {
            diag::error_path("prs", sfile, &e.to_string());
            return Ok(false);
        }
    }

    // The default header (":PN::\n\n") is only emitted when neither -d nor -r
    // is specified (POSIX SYNOPSIS).
    if use_default_format && args.sid.is_none() {
        // We need a dummy delta to expand the header format
        // Since :PN: doesn't depend on delta, use the first one
        if let Some(first_delta) = sccs.header().deltas.first() {
            let (header, _) = expand_keywords(DEFAULT_HEADER_FORMAT, &sccs, first_delta, sfile);
            io::stdout().write_all(&header)?;
        }
    }

    // Parse target SID if specified. An empty -r option-argument (i.e. bare
    // "-r") selects the most recently created delta (the trunk head).
    let target_sid: Option<Sid> = match args.sid {
        Some(ref sid_str) if sid_str.is_empty() => {
            match sccs.get_trunk_head() {
                Some(head) => Some(head.sid),
                // No trunk head: nothing to report on.
                None => return Ok(true),
            }
        }
        Some(ref sid_str) => Some(
            sid_str
                .parse()
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?,
        ),
        None => None,
    };

    // Parse cutoff if specified. An unparseable one is an error: silently
    // dropping it would report every delta, which looks like a successful
    // query rather than a rejected one.
    let cutoff = match args.cutoff.as_deref() {
        Some(c) => Some(cutoff::parse(c).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("{}: '{}'", gettext("Invalid cutoff date"), c),
            )
        })?),
        None => None,
    };

    // Determine which deltas to process
    let deltas: Vec<&DeltaEntry> = sccs
        .header()
        .deltas
        .iter()
        .filter(|d| {
            // Filter by delta type (include removed only if -a)
            if !args.all_deltas && d.delta_type == DeltaType::Removed {
                return false;
            }

            // Filter by SID
            if let Some(ref target) = target_sid {
                if args.earlier {
                    // Include deltas at or before target SID
                    if d.sid > *target {
                        return false;
                    }
                } else if args.later {
                    // Include deltas at or after target SID
                    if d.sid < *target {
                        return false;
                    }
                } else {
                    // Exact match
                    if &d.sid != target {
                        return false;
                    }
                }
            }

            // Filter by cutoff. "No changes (deltas) to the SCCS file that
            // were created after the specified cutoff date-time shall be
            // included in the output" (112241-112243) is unconditional, so it
            // applies without -e/-l too — the synopsis at 112215 makes -e|-l
            // optional in the -c form. Only -l inverts the comparison, since
            // it asks for deltas at or later than the cutoff.
            if let Some(cutoff) = cutoff {
                if args.later {
                    if !cutoff.is_at_or_after(d) {
                        return false;
                    }
                } else if !cutoff.is_at_or_before(d) {
                    return false;
                }
            }

            true
        })
        .collect();

    // If no SID specified and no -e/-l, default to latest (first in list)
    let deltas_to_print =
        if args.dataspec.is_some() && target_sid.is_none() && !args.earlier && !args.later {
            // -d without -e or -l: only print latest delta
            deltas.into_iter().take(1).collect::<Vec<_>>()
        } else {
            deltas
        };

    // Print output for each selected delta. Each delta's expanded dataspec is
    // terminated with a <newline> unless it already ends with one and the final
    // token was not a multi-line keyword.
    for delta in deltas_to_print {
        let (mut output, last_was_multiline) = expand_keywords(format, &sccs, delta, sfile);
        if last_was_multiline || !output.ends_with(b"\n") {
            output.push(b'\n');
        }
        io::stdout().write_all(&output)?;
    }

    Ok(true)
}

fn main() -> ExitCode {
    plib::diag::init_locale("prs");

    // The -r option-argument is optional and "cannot be presented as a
    // separate argument" (POSIX): the word after a bare -r is an operand.
    let (attached, rest) = optscan::scan(std::env::args_os(), Letters::of("prs"));
    let mut args =
        plib::optarg::parse_from::<Args>(plib::optarg::keep_leading_equals::<Args>(rest));
    if let Some(sid) = attached.get(b'r') {
        // A SID is ASCII; one that is not text is not a SID.
        args.sid = Some(sid.unwrap_or_default().to_string_lossy().into_owned());
    }

    let mut success = true;
    for path in operands::expand(&args.files) {
        match prs_file(&path, &args) {
            Ok(ok) => success = success && ok,
            Err(e) => {
                diag::error_path("prs", &path, &e.to_string());
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
