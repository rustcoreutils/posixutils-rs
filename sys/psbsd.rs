//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! procps' dashless BSD options, as in `ps aux`: `a` lists every user's
//! processes, `x` those without a terminal too, and `u` selects the
//! user-oriented format.  Only these three letters are accepted.

use std::ffi::OsString;
use std::io::{self, Write};
use std::process::ExitCode;

use gettextrs::gettext;

use crate::platform::{self, ProcessInfo};

/// The letters given in BSD-style option words.
#[derive(Debug, Default, PartialEq)]
pub struct BsdOptions {
    /// `a`: every user's processes, not only the invoker's.
    all_users: bool,
    /// `x`: processes without a controlling terminal too.
    no_tty: bool,
    /// `u`: the user-oriented format.
    user_format: bool,
}

/// `arg` as a BSD option word: one or more of the letters `a`, `u`, `x`.
fn bsd_word(arg: &OsString) -> Option<&str> {
    let word = arg.to_str()?;
    let is_bsd = !word.is_empty() && word.chars().all(|c| matches!(c, 'a' | 'u' | 'x'));
    is_bsd.then_some(word)
}

/// Read the command-line arguments (without the program name) as BSD option
/// words.  `Ok(None)` when the first is not one, so the arguments are POSIX
/// options; an error when it is one but a later argument is not.
pub fn parse(args: &[OsString]) -> Result<Option<BsdOptions>, String> {
    match args.first() {
        Some(first) if bsd_word(first).is_some() => {}
        _ => return Ok(None),
    }
    let mut opts = BsdOptions::default();
    for arg in args {
        let Some(word) = bsd_word(arg) else {
            return Err(gettext(
                "BSD-style options cannot be combined with other arguments",
            ));
        };
        for letter in word.chars() {
            match letter {
                'a' => opts.all_users = true,
                'x' => opts.no_tty = true,
                _ => opts.user_format = true,
            }
        }
    }
    Ok(Some(opts))
}

impl BsdOptions {
    /// Whether `proc` is listed: without `a` only the invoker's (effective
    /// UID `euid`), and without `x` only those with a controlling terminal.
    fn selects(&self, proc: &ProcessInfo, euid: u32) -> bool {
        (self.all_users || proc.uid == euid) && (self.no_tty || proc.tty.is_some())
    }
}

/// What a column's value is computed from besides the process.
struct Context {
    now: u64,          // current time, seconds since the Unix epoch
    total_mem_kb: u64, // physical memory, for %MEM
}

/// One output column: its header, minimum width, justification, and value.
struct Column {
    header: &'static str,
    width: usize,
    right: bool,
    value: fn(&ProcessInfo, &Context) -> String,
}

const fn column(
    header: &'static str,
    width: usize,
    right: bool,
    value: fn(&ProcessInfo, &Context) -> String,
) -> Column {
    Column {
        header,
        width,
        right,
        value,
    }
}

/// procps' `u` format.
const USER_COLUMNS: [Column; 11] = [
    column("USER", 8, false, |p, _| user_column(p.uid)),
    column("PID", 7, true, |p, _| p.pid.to_string()),
    column("%CPU", 4, true, |p, c| {
        let elapsed = c.now.saturating_sub(p.start_time);
        per_mille_text(p.cpu_ms.checked_div(elapsed).unwrap_or(0))
    }),
    column("%MEM", 4, true, |p, c| {
        per_mille_text((p.rss * 1000).checked_div(c.total_mem_kb).unwrap_or(0))
    }),
    column("VSZ", 6, true, |p, _| p.vsz.to_string()),
    column("RSS", 5, true, |p, _| p.rss.to_string()),
    column("TTY", 8, false, tty_column),
    column("STAT", 4, false, |p, _| stat_column(p)),
    column("START", 5, true, |p, c| {
        start_column(p.start_time, c.now, &chrono::Local)
    }),
    column("TIME", 6, true, |p, _| bsd_time(p.time)),
    column("COMMAND", 0, false, command_column),
];

/// procps' format for BSD options without `u`.
const DEFAULT_COLUMNS: [Column; 5] = [
    column("PID", 7, true, |p, _| p.pid.to_string()),
    column("TTY", 8, false, tty_column),
    column("STAT", 4, false, |p, _| stat_column(p)),
    column("TIME", 6, true, |p, _| bsd_time(p.time)),
    column("COMMAND", 0, false, command_column),
];

fn tty_column(proc: &ProcessInfo, _: &Context) -> String {
    proc.tty.as_deref().unwrap_or("?").to_string()
}

/// The command line, a control character shown as `?` as procps does, so
/// that each process stays on one line.
fn command_column(proc: &ProcessInfo, _: &Context) -> String {
    crate::mark_defunct(&proc.args, proc.state)
        .chars()
        .map(|c| if c.is_control() { '?' } else { c })
        .collect()
}

/// The user name for `uid`, or the number; a name longer than 8 characters
/// is cut to 7 and a `+`, as procps does.
fn user_column(uid: u32) -> String {
    cut_user_name(crate::uid_to_name(uid).unwrap_or_else(|| uid.to_string()))
}

fn cut_user_name(name: String) -> String {
    if name.chars().count() > 8 {
        name.chars().take(7).chain(['+']).collect()
    } else {
        name
    }
}

/// A percentage given in tenths: one decimal, or none from 100% up.
fn per_mille_text(per_mille: u64) -> String {
    if per_mille > 999 {
        (per_mille / 10).to_string()
    } else {
        format!("{}.{}", per_mille / 10, per_mille % 10)
    }
}

/// The state letter, then procps' flags: `<` raised priority, `N` lowered,
/// `L` locked pages, `s` session leader, `l` multi-threaded, and `+` in the
/// foreground process group of its terminal.
pub(crate) fn stat_column(proc: &ProcessInfo) -> String {
    let mut stat = String::from(proc.state);
    if proc.nice < 0 {
        stat.push('<');
    } else if proc.nice > 0 {
        stat.push('N');
    }
    if proc.locked {
        stat.push('L');
    }
    if proc.sid == proc.pid {
        stat.push('s');
    }
    if proc.threads > 1 {
        stat.push('l');
    }
    if proc.tpgid == proc.pgid {
        stat.push('+');
    }
    stat
}

/// The START column: `HH:MM` today, `MmmDD` this year, else the year.
fn start_column<Tz>(start_epoch: u64, now_epoch: u64, tz: &Tz) -> String
where
    Tz: chrono::TimeZone,
    Tz::Offset: std::fmt::Display,
{
    use chrono::Datelike;
    let year = |epoch: u64| tz.timestamp_opt(epoch as i64, 0).single().map(|t| t.year());
    match (year(start_epoch), year(now_epoch)) {
        (Some(started), Some(now)) if start_epoch != 0 && started != now => started.to_string(),
        _ => crate::format_stime(start_epoch, now_epoch, tz),
    }
}

/// CPU time as BSD shows it: minutes, then seconds.
fn bsd_time(secs: u64) -> String {
    format!("{}:{:02}", secs / 60, secs % 60)
}

/// One line of `columns`, each justified in its width and separated by a
/// blank.  As in procps, a value wider than its column pushes the rest of
/// the line right only until later padding absorbs it: each column starts
/// where it should if there is room, and one blank after the previous text
/// if not.
fn format_line(columns: &[Column], cell: impl Fn(&Column) -> String) -> String {
    let mut line = String::new();
    let mut intended = 0; // where this column should start
    for (i, col) in columns.iter().enumerate() {
        let text = cell(col);
        let len = text.chars().count();
        let left_pad = if col.right {
            col.width.saturating_sub(len)
        } else {
            0
        };
        let at = line.chars().count();
        let gap = (intended + left_pad)
            .saturating_sub(at)
            .max(usize::from(i > 0));
        line.extend(std::iter::repeat_n(' ', gap));
        line.push_str(&text);
        intended += col.width + 1;
    }
    line
}

fn write_listing(procs: &[ProcessInfo], columns: &[Column], ctx: &Context) -> io::Result<()> {
    let limit = crate::resolve_line_limit(0);
    let mut out = crate::listing_output();
    let header = format_line(columns, |c| c.header.to_string());
    writeln!(out, "{}", crate::truncate_line(&header, limit))?;
    for proc in procs {
        let line = format_line(columns, |c| (c.value)(proc, ctx));
        writeln!(out, "{}", crate::truncate_line(&line, limit))?;
    }
    out.flush()
}

/// List the processes `opts` selects, sorted by PID, in its format.
pub fn run(opts: &BsdOptions) -> ExitCode {
    let mut procs = match platform::list_processes() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("ps: {}", e);
            return ExitCode::from(1);
        }
    };
    let euid = unsafe { libc::geteuid() };
    procs.retain(|p| opts.selects(p, euid));
    procs.sort_by_key(|p| p.pid);

    let ctx = Context {
        now: std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0),
        total_mem_kb: platform::total_memory_kb(),
    };
    let columns: &[Column] = if opts.user_format {
        &USER_COLUMNS
    } else {
        &DEFAULT_COLUMNS
    };
    match write_listing(&procs, columns, &ctx) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!(
                "ps: {}: {}",
                gettext("write error"),
                plib::diag::io_error_text(&e)
            );
            ExitCode::from(1)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn words(args: &[&str]) -> Vec<OsString> {
        args.iter().map(OsString::from).collect()
    }

    #[test]
    fn parse_words() {
        assert_eq!(parse(&words(&[])), Ok(None));
        assert_eq!(parse(&words(&["-A"])), Ok(None));
        assert_eq!(parse(&words(&["auxf"])), Ok(None));
        let aux = BsdOptions {
            all_users: true,
            no_tty: true,
            user_format: true,
        };
        assert_eq!(parse(&words(&["aux"])), Ok(Some(aux)));
        let aux = BsdOptions {
            all_users: true,
            no_tty: true,
            user_format: true,
        };
        assert_eq!(parse(&words(&["x", "ua"])), Ok(Some(aux)));
        assert!(parse(&words(&["aux", "-f"])).is_err());
        assert!(parse(&words(&["a", "1"])).is_err());
    }

    #[test]
    fn percentages() {
        assert_eq!(per_mille_text(0), "0.0");
        assert_eq!(per_mille_text(7), "0.7");
        assert_eq!(per_mille_text(123), "12.3");
        assert_eq!(per_mille_text(999), "99.9");
        assert_eq!(per_mille_text(1000), "100");
        assert_eq!(per_mille_text(2345), "234");
    }

    #[test]
    fn cpu_time() {
        assert_eq!(bsd_time(0), "0:00");
        assert_eq!(bsd_time(188), "3:08");
        assert_eq!(bsd_time(6005), "100:05");
    }

    #[test]
    fn start_year() {
        use chrono::Utc;
        // 2021-01-01 00:00:00 UTC.
        let day = 1_609_459_200u64;
        assert_eq!(start_column(day + 3720, day + 7200, &Utc), "01:02");
        assert_eq!(start_column(day, day + 40 * 86_400, &Utc), "Jan01");
        assert_eq!(start_column(day - 86_400, day + 3600, &Utc), "2020");
        assert_eq!(start_column(0, day, &Utc), "-");
    }

    #[test]
    fn long_user_names_are_cut() {
        assert_eq!(cut_user_name("root".into()), "root");
        assert_eq!(cut_user_name("12345678".into()), "12345678");
        assert_eq!(cut_user_name("systemd-resolve".into()), "systemd+");
    }
}
