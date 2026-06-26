//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::io::input_stream_dashed;
use plib::locale::{mb_char_slices, wcwidth_char};
use std::io::{self, BufRead, Write};
use std::path::PathBuf;

/// unexpand - convert spaces to tabs
#[derive(Parser)]
#[command(version, about = gettext("unexpand - convert spaces to tabs"))]
struct Args {
    #[arg(short = 'a', help = gettext("Convert all sequences of two or more spaces to tabs, not just leading ones"))]
    all_spaces: bool,

    // Specifying -t also enables -a (POSIX): conversion is not limited to
    // leading blanks.
    #[arg(short = 't', help = gettext("Specify tab stops, comma- or blank-separated (implies -a)"))]
    tablist: Option<String>,

    #[arg(help = gettext("Input files"))]
    files: Vec<PathBuf>,
}

/// Tab stops, expressed as 0-based column positions on the same scale as the
/// uniform multiples (so `-t 4` and `-t 4,8` share their first stop).
enum TabStops {
    /// Stops every N columns: 0, N, 2N, ...
    Uniform(usize),
    /// Explicit ascending 0-based stop positions; no conversion past the last.
    List(Vec<usize>),
}

impl TabStops {
    /// Smallest tab stop strictly greater than 0-based column `c`, if any.
    fn next_stop(&self, c: usize) -> Option<usize> {
        match self {
            TabStops::Uniform(n) => Some(((c / n) + 1) * n),
            TabStops::List(v) => v.iter().copied().find(|&s| s > c),
        }
    }

    /// Column reached by a literal `<tab>` at 0-based column `c` (the next stop,
    /// or one column past the last explicit stop).
    fn tab_advance(&self, c: usize) -> usize {
        self.next_stop(c).unwrap_or(c + 1)
    }
}

fn parse_tablist(s: &str) -> Result<TabStops, String> {
    // A single integer sets a uniform, repeating tab width (must be positive).
    if let Ok(n) = s.trim().parse::<usize>() {
        if n == 0 {
            return Err("tab size must be a positive integer".to_string());
        }
        return Ok(TabStops::Uniform(n));
    }

    // Otherwise a comma- or blank-separated list of ascending positive stops.
    let mut v: Vec<usize> = Vec::new();
    for tok in s.split([' ', ',', '\t']) {
        if tok.is_empty() {
            continue;
        }
        let n: usize = tok
            .parse()
            .map_err(|_| "invalid tab stop in list".to_string())?;
        if n == 0 {
            return Err("tab stop must be a positive integer".to_string());
        }
        if let Some(&last) = v.last() {
            if n <= last {
                return Err("tab stops must be in strictly ascending order".to_string());
            }
        }
        v.push(n);
    }
    if v.is_empty() {
        return Err("invalid tab stop in list".to_string());
    }
    Ok(TabStops::List(v))
}

fn unexpand(args: &Args) -> Result<(), Box<dyn std::error::Error>> {
    let tabs = match &args.tablist {
        Some(s) => parse_tablist(s)?,
        None => TabStops::Uniform(8),
    };
    // -t implies -a (POSIX): conversion is not limited to leading blanks.
    let all_mode = args.all_spaces || args.tablist.is_some();

    // No operands read stdin; otherwise each operand is processed in order, with
    // a "-" reading stdin at its position (POSIX Guideline 13).
    let sources: Vec<PathBuf> = if args.files.is_empty() {
        vec![PathBuf::from("-")]
    } else {
        args.files.clone()
    };

    let mut stdout = io::stdout().lock();
    for source in &sources {
        let mut reader = io::BufReader::new(input_stream_dashed(source)?);
        // Read raw bytes per line to preserve exact line endings and any
        // non-UTF-8 bytes; multibyte characters are segmented by LC_CTYPE.
        let mut buf: Vec<u8> = Vec::new();
        loop {
            buf.clear();
            if reader.read_until(b'\n', &mut buf)? == 0 {
                break;
            }
            let had_newline = buf.last() == Some(&b'\n');
            let content = if had_newline {
                &buf[..buf.len() - 1]
            } else {
                &buf[..]
            };
            stdout.write_all(&unexpand_line(content, &tabs, all_mode))?;
            if had_newline {
                stdout.write_all(b"\n")?;
            }
        }
    }

    Ok(())
}

/// Emit a pending run of `run_len` spaces beginning at 0-based column
/// `run_start`. When `eligible`, replace as much of the run as possible with
/// tabs (advancing through tab stops) and keep the remainder as spaces;
/// otherwise emit the spaces verbatim.
fn flush_run(out: &mut Vec<u8>, tabs: &TabStops, run_start: usize, run_len: usize, eligible: bool) {
    if run_len == 0 {
        return;
    }
    if !eligible {
        out.extend(std::iter::repeat_n(b' ', run_len));
        return;
    }
    let end = run_start + run_len;
    let mut c = run_start;
    while let Some(stop) = tabs.next_stop(c) {
        if stop <= end {
            out.push(b'\t');
            c = stop;
        } else {
            break;
        }
    }
    out.extend(std::iter::repeat_n(b' ', end - c));
}

/// Convert the blanks of one line (without its trailing newline) to tabs.
///
/// In `-a`/`-t` mode any run of two or more spaces that spans a tab stop is
/// converted; otherwise only the leading run of blanks is converted. Existing
/// `<tab>` characters are preserved and advance the column; a single space is
/// never turned into a tab. Column positions follow `wcwidth(3)` under
/// `LC_CTYPE`, and a `<tab>` does not end the leading-blank region.
fn unexpand_line(line: &[u8], tabs: &TabStops, all_mode: bool) -> Vec<u8> {
    let mut out: Vec<u8> = Vec::new();
    let mut col: usize = 0;
    let mut run_start: usize = 0;
    let mut run_len: usize = 0;
    let mut seen_nonblank = false;

    let is_eligible = |run_len: usize, seen_nonblank: bool| {
        if all_mode {
            run_len >= 2
        } else {
            !seen_nonblank
        }
    };

    for ch in mb_char_slices(line) {
        match ch {
            b" " => {
                if run_len == 0 {
                    run_start = col;
                }
                run_len += 1;
                col += 1;
            }
            b"\t" => {
                flush_run(
                    &mut out,
                    tabs,
                    run_start,
                    run_len,
                    is_eligible(run_len, seen_nonblank),
                );
                run_len = 0;
                out.push(b'\t');
                col = tabs.tab_advance(col);
            }
            b"\x08" => {
                flush_run(
                    &mut out,
                    tabs,
                    run_start,
                    run_len,
                    is_eligible(run_len, seen_nonblank),
                );
                run_len = 0;
                out.push(0x08);
                col = col.saturating_sub(1);
                seen_nonblank = true;
            }
            _ => {
                flush_run(
                    &mut out,
                    tabs,
                    run_start,
                    run_len,
                    is_eligible(run_len, seen_nonblank),
                );
                run_len = 0;
                out.extend_from_slice(ch);
                let w = std::str::from_utf8(ch)
                    .ok()
                    .and_then(|s| s.chars().next())
                    .map(wcwidth_char)
                    .unwrap_or(1);
                if w > 0 {
                    col += w as usize;
                }
                seen_nonblank = true;
            }
        }
    }
    flush_run(
        &mut out,
        tabs,
        run_start,
        run_len,
        is_eligible(run_len, seen_nonblank),
    );

    out
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let mut exit_code = 0;

    if let Err(err) = unexpand(&args) {
        exit_code = 1;
        eprintln!("{}", err);
    }

    std::process::exit(exit_code)
}
