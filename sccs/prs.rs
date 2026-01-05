//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! prs - print SCCS file information

use std::fs;
use std::io::{self, BufRead};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};
use plib::sccsfile::{DeltaEntry, DeltaType, SccsFile, SccsFlag, Sid, paths};

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

    #[arg(short = 'r', value_name = "SID", help = gettext("SID to report on"))]
    sid: Option<String>,

    #[arg(required = true, help = gettext("SCCS files to process (use - for stdin)"))]
    files: Vec<PathBuf>,
}

// Default output format (per POSIX, there's a header and per-delta format)
const DEFAULT_HEADER_FORMAT: &str = ":PN::\n\n";
const DEFAULT_DELTA_FORMAT: &str = ":Dt:\t:DL:\nMRs:\n:MR:COMMENTS:\n:C:\n";

fn format_sid(sid: &Sid) -> String {
    sid.to_string()
}

fn flag_yes_no(val: bool) -> &'static str {
    if val { "yes" } else { "no" }
}

/// Expand data keywords in format string for a specific delta
fn expand_keywords(format: &str, sccs: &SccsFile, delta: &DeltaEntry, sfile_path: &Path) -> String {
    let mut result = String::new();
    let mut chars = format.chars().peekable();

    while let Some(c) = chars.next() {
        if c == ':' {
            // Check for :: (literal colon)
            if chars.peek() == Some(&':') {
                chars.next();
                result.push(':');
                continue;
            }

            // Check if next char could start a keyword (alphanumeric)
            // If not, treat as literal colon
            if !chars
                .peek()
                .map(|c| c.is_ascii_alphanumeric())
                .unwrap_or(false)
            {
                result.push(':');
                continue;
            }

            // Collect keyword (only alphanumeric chars)
            let mut keyword = String::new();
            while let Some(&ch) = chars.peek() {
                if ch == ':' {
                    chars.next();
                    break;
                }
                if !ch.is_ascii_alphanumeric() {
                    // Not a valid keyword, treat opening colon as literal
                    result.push(':');
                    result.push_str(&keyword);
                    keyword.clear();
                    break;
                }
                keyword.push(chars.next().unwrap());
            }

            // Expand keyword if we collected one
            if !keyword.is_empty() {
                let expanded = expand_single_keyword(&keyword, sccs, delta, sfile_path);
                result.push_str(&expanded);
            }
        } else if c == '\\' {
            // Handle escape sequences
            if let Some(&next) = chars.peek() {
                match next {
                    'n' => {
                        chars.next();
                        result.push('\n');
                    }
                    't' => {
                        chars.next();
                        result.push('\t');
                    }
                    '\\' => {
                        chars.next();
                        result.push('\\');
                    }
                    _ => result.push(c),
                }
            } else {
                result.push(c);
            }
        } else {
            result.push(c);
        }
    }

    result
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
        "CB" => get_ceiling(&sccs.header.flags)
            .map(|v| v.to_string())
            .unwrap_or_default(),
        "Ds" => get_default_sid(&sccs.header.flags).unwrap_or_default(),
        "F" => sfile_path
            .file_name()
            .map(|s| s.to_string_lossy().to_string())
            .unwrap_or_default(),
        "FB" => get_floor(&sccs.header.flags)
            .map(|v| v.to_string())
            .unwrap_or_default(),
        "FD" => sccs.header.descriptive_text.join("\n"),
        "FL" => format_flags(&sccs.header.flags),
        "J" => flag_yes_no(has_flag(&sccs.header.flags, |f| {
            matches!(f, SccsFlag::JointEdit)
        }))
        .to_string(),
        "KF" => flag_yes_no(has_flag(&sccs.header.flags, |f| {
            matches!(f, SccsFlag::IdKeywordError(_))
        }))
        .to_string(),
        "KV" => get_keyword_validation(&sccs.header.flags).unwrap_or_default(),
        "LK" => get_locked_releases(&sccs.header.flags),
        "M" => sccs
            .module_name()
            .map(|s| s.to_string())
            .unwrap_or_else(|| paths::module_name(sfile_path).unwrap_or_default()),
        "MF" => flag_yes_no(has_flag(&sccs.header.flags, |f| {
            matches!(f, SccsFlag::MrValidation(_))
        }))
        .to_string(),
        "MP" => get_mr_program(&sccs.header.flags).unwrap_or_default(),
        "ND" => flag_yes_no(has_flag(&sccs.header.flags, |f| {
            matches!(f, SccsFlag::NullDelta)
        }))
        .to_string(),
        "PN" => sfile_path
            .canonicalize()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_else(|_| sfile_path.to_string_lossy().to_string()),
        "Q" => get_q_flag(&sccs.header.flags).unwrap_or_default(),
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
            "{}{}  {} {}{}",
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
            delta.stats.inserted, delta.stats.deleted, delta.stats.unchanged
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
        "I" => format_sid(&delta.sid),
        "L" => delta.sid.lev.to_string(),
        "Ld" => delta.stats.deleted.to_string(),
        "Li" => delta.stats.inserted.to_string(),
        "Lu" => delta.stats.unchanged.to_string(),
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

fn has_flag<F>(flags: &[SccsFlag], predicate: F) -> bool
where
    F: Fn(&SccsFlag) -> bool,
{
    flags.iter().any(predicate)
}

fn get_ceiling(flags: &[SccsFlag]) -> Option<u16> {
    for flag in flags {
        if let SccsFlag::Ceiling(v) = flag {
            return Some(*v);
        }
    }
    None
}

fn get_floor(flags: &[SccsFlag]) -> Option<u16> {
    for flag in flags {
        if let SccsFlag::Floor(v) = flag {
            return Some(*v);
        }
    }
    None
}

fn get_default_sid(flags: &[SccsFlag]) -> Option<String> {
    for flag in flags {
        if let SccsFlag::DefaultSid(sid) = flag {
            return Some(sid.to_string());
        }
    }
    None
}

fn get_locked_releases(flags: &[SccsFlag]) -> String {
    for flag in flags {
        if let SccsFlag::LockedReleases(releases) = flag {
            return releases
                .iter()
                .map(|r| r.to_string())
                .collect::<Vec<_>>()
                .join(" ");
        }
    }
    String::new()
}

fn get_mr_program(flags: &[SccsFlag]) -> Option<String> {
    for flag in flags {
        if let SccsFlag::MrValidation(prog) = flag {
            return prog.clone();
        }
    }
    None
}

fn get_q_flag(flags: &[SccsFlag]) -> Option<String> {
    for flag in flags {
        if let SccsFlag::QText(val) = flag {
            return Some(val.clone());
        }
    }
    None
}

fn get_keyword_validation(flags: &[SccsFlag]) -> Option<String> {
    for flag in flags {
        if let SccsFlag::IdKeywordError(val) = flag {
            return val.clone();
        }
    }
    None
}

fn format_flags(flags: &[SccsFlag]) -> String {
    flags
        .iter()
        .map(|f| format!("{:?}", f))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Parse a cutoff date/time string
fn parse_cutoff(cutoff: &str) -> Option<(u16, u8, u8, u8, u8, u8)> {
    // Remove any separators
    let digits: String = cutoff.chars().filter(|c| c.is_ascii_digit()).collect();

    let len = digits.len();
    if len < 2 {
        return None;
    }

    // Parse year (required)
    let (year, rest) = if len >= 4 && digits[0..2].parse::<u16>().unwrap_or(0) >= 19 {
        // 4-digit year
        (digits[0..4].parse().ok()?, &digits[4..])
    } else {
        // 2-digit year
        let yy: u16 = digits[0..2].parse().ok()?;
        let year = if yy < 69 { 2000 + yy } else { 1900 + yy };
        (year, &digits[2..])
    };

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

/// Compare delta datetime with cutoff
fn delta_before_cutoff(delta: &DeltaEntry, cutoff: (u16, u8, u8, u8, u8, u8)) -> bool {
    let dt = &delta.datetime;
    let delta_year = if dt.year < 100 {
        if dt.year < 69 {
            2000 + dt.year
        } else {
            1900 + dt.year
        }
    } else {
        dt.year
    };

    (delta_year, dt.month, dt.day, dt.hour, dt.minute, dt.second) <= cutoff
}

fn delta_after_cutoff(delta: &DeltaEntry, cutoff: (u16, u8, u8, u8, u8, u8)) -> bool {
    let dt = &delta.datetime;
    let delta_year = if dt.year < 100 {
        if dt.year < 69 {
            2000 + dt.year
        } else {
            1900 + dt.year
        }
    } else {
        dt.year
    };

    (delta_year, dt.month, dt.day, dt.hour, dt.minute, dt.second) >= cutoff
}

fn prs_file(sfile: &Path, args: &Args) -> io::Result<bool> {
    // Check if it's a valid s-file
    if !paths::is_sfile(sfile) {
        eprintln!("{}: not an SCCS file", sfile.display());
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

    // Print header when using default format (no -d specified)
    if use_default_format {
        // We need a dummy delta to expand the header format
        // Since :PN: doesn't depend on delta, use the first one
        if let Some(first_delta) = sccs.header.deltas.first() {
            let header = expand_keywords(DEFAULT_HEADER_FORMAT, &sccs, first_delta, sfile);
            print!("{}", header);
        }
    }

    // Parse target SID if specified
    let target_sid: Option<Sid> = if let Some(ref sid_str) = args.sid {
        Some(
            sid_str
                .parse()
                .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?,
        )
    } else {
        None
    };

    // Parse cutoff if specified
    let cutoff = args.cutoff.as_ref().and_then(|c| parse_cutoff(c));

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
                    if !sid_le(&d.sid, target) {
                        return false;
                    }
                } else if args.later {
                    // Include deltas at or after target SID
                    if !sid_ge(&d.sid, target) {
                        return false;
                    }
                } else {
                    // Exact match
                    if &d.sid != target {
                        return false;
                    }
                }
            }

            // Filter by cutoff
            if let Some(cutoff) = cutoff {
                if args.earlier {
                    if !delta_before_cutoff(d, cutoff) {
                        return false;
                    }
                } else if args.later {
                    if !delta_after_cutoff(d, cutoff) {
                        return false;
                    }
                } else {
                    // Find the last delta before cutoff
                    // This will be handled differently
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

    // Print output for each selected delta
    for delta in deltas_to_print {
        let output = expand_keywords(format, &sccs, delta, sfile);
        print!("{}", output);
    }

    Ok(true)
}

fn sid_le(a: &Sid, b: &Sid) -> bool {
    (a.rel, a.lev, a.br, a.seq) <= (b.rel, b.lev, b.br, b.seq)
}

fn sid_ge(a: &Sid, b: &Sid) -> bool {
    (a.rel, a.lev, a.br, a.seq) >= (b.rel, b.lev, b.br, b.seq)
}

fn process_directory(dir: &Path, args: &Args) -> io::Result<bool> {
    let mut success = true;

    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if paths::is_sfile(&path) {
                match prs_file(&path, args) {
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
                match process_directory(&path, &args) {
                    Ok(ok) => success = success && ok,
                    Err(e) => {
                        eprintln!("{}: {}", path.display(), e);
                        success = false;
                    }
                }
            } else {
                match prs_file(&path, &args) {
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
                match process_directory(file, &args) {
                    Ok(ok) => success = success && ok,
                    Err(e) => {
                        eprintln!("{}: {}", file.display(), e);
                        success = false;
                    }
                }
            } else {
                match prs_file(file, &args) {
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
