//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! prs - print SCCS file information

use std::io;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::sccsfile::{paths, DeltaEntry, DeltaType, SccsFile, SccsFlag, Sid};
use posixutils_sccs::{cutoff, diag, operands};

/// prs - print SCCS file information
#[derive(Parser)]
#[command(version, about = gettext("prs - print SCCS file information"))]
struct Args {
    #[arg(short = 'a', help = gettext("Include removed deltas"))]
    all_deltas: bool,

    #[arg(short = 'c', value_name = "CUTOFF", help = gettext("Cutoff date/time (YY[MM[DD[HH[MM[SS]]]]])"))]
    cutoff: Option<String>,

    #[arg(short = 'd', value_name = "DATASPEC", help = gettext("Data format specification"))]
    dataspec: Option<String>,

    #[arg(short = 'e', help = gettext("Select deltas earlier than or equal to -c or -r"))]
    earlier: bool,

    #[arg(short = 'l', help = gettext("Select deltas later than or equal to -c or -r"))]
    later: bool,

    #[arg(
        short = 'r',
        value_name = "SID",
        num_args = 0..=1,
        default_missing_value = "",
        help = gettext("SID to report on (defaults to most recent delta)")
    )]
    sid: Option<String>,

    #[arg(required = true, help = gettext("SCCS files to process (use - for stdin)"))]
    files: Vec<PathBuf>,
}

// Default output format (per POSIX, there's a header and per-delta format)
const DEFAULT_HEADER_FORMAT: &str = ":PN::\n\n";
const DEFAULT_DELTA_FORMAT: &str = ":Dt:\t:DL:\nMRs:\n:MR:COMMENTS:\n:C:\n";

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
    format: &str,
    sccs: &SccsFile,
    delta: &DeltaEntry,
    sfile_path: &Path,
) -> (String, bool) {
    let mut result = String::new();
    let mut last_was_multiline = false;
    let chars: Vec<char> = format.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let c = chars[i];
        if c == ':' {
            // Try to match a recognized keyword: ':' kw ':'.
            let mut j = i + 1;
            while j < chars.len() && chars[j].is_ascii_alphanumeric() {
                j += 1;
            }
            let keyword: String = chars[i + 1..j].iter().collect();
            if !keyword.is_empty()
                && j < chars.len()
                && chars[j] == ':'
                && is_known_keyword(&keyword)
            {
                // Recognized keyword - expand it.
                let value = expand_single_keyword(&keyword, sccs, delta, sfile_path);
                let multiline = is_multiline_keyword(&keyword);
                if multiline {
                    // Each value line is newline-terminated; an empty value
                    // (e.g. :MR: for a delta with no MRs) emits nothing, not a
                    // blank line.
                    if !value.is_empty() {
                        for line in value.split('\n') {
                            result.push_str(line);
                            result.push('\n');
                        }
                    }
                } else {
                    result.push_str(&value);
                }
                last_was_multiline = multiline && !value.is_empty();
                i = j + 1;
                continue;
            }
            // Not a recognized keyword - emit the ':' literally and continue
            // scanning from the next character (so "::" prints verbatim).
            result.push(':');
            last_was_multiline = false;
            i += 1;
        } else if c == '\\' {
            // Handle escape sequences.
            match chars.get(i + 1) {
                Some('n') => {
                    result.push('\n');
                    i += 2;
                }
                Some('t') => {
                    result.push('\t');
                    i += 2;
                }
                Some('\\') => {
                    result.push('\\');
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

fn expand_single_keyword(
    keyword: &str,
    sccs: &SccsFile,
    delta: &DeltaEntry,
    sfile_path: &Path,
) -> String {
    match keyword {
        // Global keywords
        "BF" => flag_yes_no(has_flag(&sccs.header.flags, |f| {
            matches!(f, SccsFlag::BranchEnabled)
        }))
        .to_string(),
        "CB" => sccs
            .header
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
        "F" => sfile_path
            .file_name()
            .map(|s| s.to_string_lossy().to_string())
            .unwrap_or_default(),
        "FB" => sccs
            .header
            .flags
            .iter()
            .find_map(|f| match f {
                SccsFlag::Floor(v) => Some(v.to_string()),
                _ => None,
            })
            .unwrap_or_else(|| "none".to_string()),
        "FD" => {
            if sccs.header.descriptive_text.is_empty() {
                "none".to_string()
            } else {
                sccs.header.descriptive_text.join("\n")
            }
        }
        "FL" => format_flags(&sccs.header.flags),
        "J" => flag_yes_no(has_flag(&sccs.header.flags, |f| {
            matches!(f, SccsFlag::JointEdit)
        }))
        .to_string(),
        "KF" => flag_yes_no(has_flag(&sccs.header.flags, |f| {
            matches!(f, SccsFlag::IdKeywordError(_))
        }))
        .to_string(),
        "KV" => sccs.id_keyword_check().flatten().unwrap_or("").to_string(),
        // An absent `l` flag is "none"; `l a` locks every release and renders
        // as "a", matching CSSC. Encoding "all" as an empty list made the most
        // restrictive setting a file can carry report as the least.
        "LK" => sccs
            .locked_releases()
            .map(|lock| lock.value_string())
            .unwrap_or_else(|| "none".to_string()),
        "M" => sccs
            .module_name()
            .map(|s| s.to_string())
            .unwrap_or_else(|| paths::module_name(sfile_path).unwrap_or_default()),
        "MF" => flag_yes_no(has_flag(&sccs.header.flags, |f| {
            matches!(f, SccsFlag::MrValidation(_))
        }))
        .to_string(),
        "MP" => sccs.mr_program().flatten().unwrap_or("").to_string(),
        "ND" => flag_yes_no(has_flag(&sccs.header.flags, |f| {
            matches!(f, SccsFlag::NullDelta)
        }))
        .to_string(),
        // POSIX: the SCCS-file pathname as given (the operand), not resolved.
        "PN" => sfile_path.to_string_lossy().to_string(),
        "Q" => sccs.q_text().unwrap_or("").to_string(),
        "UN" => {
            if sccs.header.users.is_empty() {
                "none".to_string()
            } else {
                sccs.header.users.join("\n")
            }
        }
        "Y" => sccs.module_type().unwrap_or("").to_string(),

        // Version-specific keywords
        "A" => format!(
            "{}{} {} {}{}",
            expand_single_keyword("Z", sccs, delta, sfile_path),
            expand_single_keyword("Y", sccs, delta, sfile_path),
            expand_single_keyword("M", sccs, delta, sfile_path),
            expand_single_keyword("I", sccs, delta, sfile_path),
            expand_single_keyword("Z", sccs, delta, sfile_path),
        ),
        "B" => {
            if delta.sid.br > 0 {
                delta.sid.br.to_string()
            } else {
                String::new()
            }
        }
        "C" => delta.comments.join("\n"),
        // Gotten body: reconstruct this delta's source text.
        "GB" => gotten_body(sccs, delta.serial),
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
        "Dt" => format!(
            "{} {} {} {} {} {} {}",
            expand_single_keyword("DT", sccs, delta, sfile_path),
            expand_single_keyword("I", sccs, delta, sfile_path),
            expand_single_keyword("D", sccs, delta, sfile_path),
            expand_single_keyword("T", sccs, delta, sfile_path),
            expand_single_keyword("P", sccs, delta, sfile_path),
            expand_single_keyword("DS", sccs, delta, sfile_path),
            expand_single_keyword("DP", sccs, delta, sfile_path),
        ),
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
        "MR" => delta.mr_numbers.join("\n"),
        "P" => delta.user.clone(),
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
        "W" => format!(
            "{}{}\t{}",
            expand_single_keyword("Z", sccs, delta, sfile_path),
            expand_single_keyword("M", sccs, delta, sfile_path),
            expand_single_keyword("I", sccs, delta, sfile_path),
        ),
        "Z" => "@(#)".to_string(),

        // Unknown keyword - return as-is
        _ => format!(":{}:", keyword),
    }
}

/// Reconstruct the gotten body (source text) of the delta with `serial`.
fn gotten_body(sccs: &SccsFile, serial: u16) -> String {
    let applied_set = match sccs.compute_applied_set(serial) {
        Ok(set) => set,
        Err(_) => return String::new(),
    };

    if sccs.is_encoded() {
        let encoded = sccs.evaluate_body(&applied_set);
        let raw = plib::sccsfile::uudecode_sccs(&encoded);
        let mut s = String::from_utf8_lossy(&raw).into_owned();
        // The M-format expander newline-terminates each line, so drop a single
        // trailing newline to avoid emitting a spurious blank line.
        if s.ends_with('\n') {
            s.pop();
        }
        s
    } else {
        sccs.evaluate_body(&applied_set).join("\n")
    }
}

fn has_flag<F>(flags: &[SccsFlag], predicate: F) -> bool
where
    F: Fn(&SccsFlag) -> bool,
{
    flags.iter().any(predicate)
}

fn format_flags(flags: &[SccsFlag]) -> String {
    flags
        .iter()
        .filter_map(|f| f.prs_fl_line())
        .collect::<Vec<_>>()
        .join("\n")
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
    let format = args.dataspec.as_deref().unwrap_or(DEFAULT_DELTA_FORMAT);

    // The default header (":PN::\n\n") is only emitted when neither -d nor -r
    // is specified (POSIX SYNOPSIS).
    if use_default_format && args.sid.is_none() {
        // We need a dummy delta to expand the header format
        // Since :PN: doesn't depend on delta, use the first one
        if let Some(first_delta) = sccs.header.deltas.first() {
            let (header, _) = expand_keywords(DEFAULT_HEADER_FORMAT, &sccs, first_delta, sfile);
            print!("{}", header);
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
        .header
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
        if last_was_multiline || !output.ends_with('\n') {
            output.push('\n');
        }
        print!("{}", output);
    }

    Ok(true)
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    // The -r option-argument is optional and "cannot be presented as a separate
    // argument" (POSIX). Rewrite a bare "-r" to "-r=" so clap treats it as an
    // empty attached value rather than greedily consuming the following operand.
    let argv = std::env::args().map(|a| if a == "-r" { "-r=".to_string() } else { a });
    let args = Args::parse_from(argv);

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
