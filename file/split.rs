//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::cmp;
use std::ffi::CString;
use std::fs::{File, OpenOptions};
use std::io::{self, BufRead, Error, Read, Write};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::io::{input_reader, input_stream};
use plib::BUFSZ;

#[derive(Parser)]
#[command(version, about = gettext("split - split a file into pieces"))]
struct Args {
    #[arg(
        short = 'a',
        long,
        default_value_t = 2,
        value_parser = clap::value_parser!(u32).range(1..),
        help = gettext(
            "Use suffix_length letters to form the suffix portion of the filenames of the split file"
        )
    )]
    suffix_len: u32,

    #[arg(
        short,
        long,
        group = "mode",
        value_parser = clap::value_parser!(u64).range(1..),
        help = gettext(
            "Use suffix_length letters to form the suffix portion of the filenames of the split file"
        )
    )]
    lines: Option<u64>,

    #[arg(
        short,
        long,
        group = "mode",
        help = gettext("Split a file into pieces n bytes in size")
    )]
    bytes: Option<String>,

    #[arg(default_value = "-", help = gettext("File to be split ('-' or omitted: stdin)"))]
    file: PathBuf,

    #[arg(default_value = "x", help = gettext("Prefix of output files"))]
    prefix: String,
}

pub struct Suffix {
    /// The suffix the next call yields, or `None` once every suffix of this
    /// length has been handed out.
    ///
    /// Holding the pending value rather than incrementing after the yield is
    /// what makes the final all-`z` suffix reachable: the carry running off
    /// the left latches exhaustion instead of discarding a value that was
    /// never returned.
    next: Option<String>,
}

impl Suffix {
    pub fn new(len: usize) -> Self {
        debug_assert!(len > 0);
        Self {
            next: Some("a".repeat(len)),
        }
    }

    fn inc_char(ch: char) -> char {
        debug_assert!(('a'..='y').contains(&ch));
        ((ch as u8) + 1) as char
    }

    /// The suffix following `current`, or `None` when `current` is the last
    /// one of its length (all `'z'`).
    fn successor(current: &str) -> Option<String> {
        let mut chars: Vec<char> = current.chars().collect();
        for i in (0..chars.len()).rev() {
            if chars[i] != 'z' {
                chars[i] = Self::inc_char(chars[i]);
                return Some(chars.into_iter().collect());
            }
            chars[i] = 'a';
        }
        None
    }
}

impl Iterator for Suffix {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.next.take()?;
        self.next = Self::successor(&current);
        Some(current)
    }
}

struct OutputState {
    prefix: String,
    boundary: u64,

    suffix: Suffix,
    count: u64,
    outf: Option<File>,
}

impl OutputState {
    fn new(prefix: &str, boundary: u64, suffix_len: u32) -> OutputState {
        OutputState {
            prefix: String::from(prefix),
            boundary,
            suffix: Suffix::new(suffix_len as usize),
            count: 0,
            outf: None,
        }
    }

    /// The file the next write goes to, opening the next output file if the
    /// current one has been closed.
    ///
    /// Handing back the file is what keeps "a write needs an open file" a fact
    /// about the type rather than an ordering the callers have to remember.
    fn open_output(&mut self) -> io::Result<&mut File> {
        if self.outf.is_none() {
            let Some(suffix) = self.suffix.next() else {
                return Err(Error::other(gettext(
                    "too many files: output suffixes exhausted",
                )));
            };

            let out_fn = format!("{}{}", self.prefix, suffix);
            let f = OpenOptions::new()
                .read(false)
                .write(true)
                .create(true)
                .truncate(true)
                .open(&out_fn)
                .map_err(|e| named(Path::new(&out_fn), e))?;
            self.outf = Some(f);
        }

        Ok(self
            .outf
            .as_mut()
            .expect("the branch above leaves outf populated"))
    }

    fn close_output(&mut self) {
        if self.outf.is_some() {
            self.outf = None;
            self.count = 0;
        }
    }

    fn incr_output(&mut self, n: u64) {
        self.count += n;
        assert!(self.count <= self.boundary);

        if self.count == self.boundary {
            self.close_output();
        }
    }

    fn output_bytes(&mut self, buf: &[u8]) -> io::Result<()> {
        let mut consumed: usize = 0;
        while consumed < buf.len() {
            let remainder = buf.len() - consumed;
            let dist = self.boundary - self.count;
            let wlen = cmp::min(dist as usize, remainder);
            let slice = &buf[consumed..consumed + wlen];
            self.open_output()?.write_all(slice)?;

            consumed += wlen;

            self.incr_output(wlen as u64);
        }

        Ok(())
    }
}

/// The `{NAME_MAX}` for the directory that will hold the output files (the
/// parent of `prefix`), falling back to 255 if it cannot be determined.
fn name_max_for(prefix: &str) -> i64 {
    let dir = Path::new(prefix)
        .parent()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    let Ok(cdir) = CString::new(dir.as_os_str().as_bytes()) else {
        return 255;
    };
    let v = unsafe { libc::pathconf(cdir.as_ptr(), libc::_PC_NAME_MAX) };
    if v < 0 {
        255
    } else {
        v
    }
}

/// Render an `io::Error` as `<path>: <message>`.
///
/// split has two distinct failure surfaces -- the input operand and each
/// generated output file -- and `main` can name neither, so the name is
/// captured here at each origin.
fn named(path: &Path, e: io::Error) -> Error {
    Error::other(format!(
        "{}: {}",
        path.display(),
        plib::diag::io_error_text(&e)
    ))
}

fn split_by_bytes(args: &Args, bytesplit: String) -> io::Result<()> {
    let mul: u64 = {
        if bytesplit.ends_with("k") {
            1024
        } else if bytesplit.ends_with("m") {
            1024 * 1024
        } else if bytesplit.ends_with("g") {
            1024 * 1024 * 1024
        } else {
            1
        }
    };
    let bytestr = match mul {
        1 => &bytesplit[..],
        _ => &bytesplit[0..bytesplit.len() - 1],
    };
    let boundary: u64 = match bytestr.parse::<u64>() {
        Ok(n) => n
            .checked_mul(mul)
            .ok_or_else(|| Error::other(gettext("byte count too large")))?,
        Err(e) => {
            return Err(Error::other(format!(
                "{}: {}: {}",
                gettext("invalid byte count"),
                bytestr,
                e
            )));
        }
    };

    // A zero boundary makes every write advance by zero bytes, so the loop
    // below opens a fresh output file on each pass and never consumes the
    // input -- one empty file per available suffix, then "suffixes exhausted".
    // `-l` is bounded by clap's `1..`; `-b` parses its own operand, so the
    // bound belongs here.
    if boundary == 0 {
        return Err(Error::other(format!(
            "{}: {}",
            gettext("invalid byte count"),
            bytesplit
        )));
    }

    // open file, or stdin ("-" or no operand)
    let mut file = input_stream(&args.file, true).map_err(|e| named(&args.file, e))?;
    let mut raw_buffer = [0; BUFSZ];
    let mut state = OutputState::new(&args.prefix, boundary, args.suffix_len);

    loop {
        // read a chunk of file data
        let n_read = file.read(&mut raw_buffer[..])?;
        if n_read == 0 {
            break;
        }

        // slice of buffer containing file data
        let buf = &raw_buffer[0..n_read];

        state.output_bytes(buf)?;
    }

    Ok(())
}

fn split_by_lines(args: &Args, linesplit: u64) -> io::Result<()> {
    assert!(linesplit > 0);

    // open file, or stdin ("-" or no operand)
    let mut reader = input_reader(&args.file, true).map_err(|e| named(&args.file, e))?;
    let mut state = OutputState::new(&args.prefix, linesplit, args.suffix_len);

    loop {
        let mut buffer = String::new();
        let n_read = reader.read_line(&mut buffer)?;
        if n_read == 0 {
            break;
        }

        state.open_output()?.write_all(buffer.as_ref())?;

        state.incr_output(1);
    }

    Ok(())
}

fn split_main(mut args: Args) -> Result<(), Box<dyn std::error::Error>> {
    // {NAME_MAX} check: the basename of the prefix plus the suffix length must
    // fit in a filename. If not, fail with a diagnostic before creating files.
    let base_len = Path::new(&args.prefix)
        .file_name()
        .map(|s| s.len())
        .unwrap_or(args.prefix.len());
    if base_len as i64 + i64::from(args.suffix_len) > name_max_for(&args.prefix) {
        return Err(Box::new(Error::other(gettext("output filename too long"))));
    }

    if args.lines.is_none() && args.bytes.is_none() {
        args.lines = Some(1000);
    }

    if let Some(lines) = args.lines {
        split_by_lines(&args, lines)?;
    } else {
        split_by_bytes(&args, args.bytes.clone().unwrap())?;
    }

    Ok(())
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    let _ = textdomain("posixutils-rs");
    let _ = bind_textdomain_codeset("posixutils-rs", "UTF-8");

    // Diagnostics are written here rather than propagated out of `main`: the
    // `Termination` impl prints the `Debug` of a boxed error, which reaches the
    // user as `Error: Custom { kind: Other, error: "..." }`. Every failure has
    // to read as one `split: <message>` line, so each error site returns its
    // message and only this one prints it.
    match split_main(Args::parse()) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("split: {}", plib::diag::error_text(e.as_ref()));
            ExitCode::FAILURE
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_suffix_inc_char() {
        assert_eq!(Suffix::inc_char('a'), 'b');
        assert_eq!(Suffix::inc_char('b'), 'c');
        assert_eq!(Suffix::inc_char('y'), 'z');
    }

    #[test]
    fn test_suffix_iterable() {
        let suffix = Suffix::new(1);
        assert_eq!(suffix.count(), 26);
    }
}
