//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::io::{self, BufRead, Write};
use std::path::{Path, PathBuf};

use clap::Parser;
use gettextrs::gettext;
use plib::io::input_reader;
use plib::locale::next_char_offset;

/// asa - interpret carriage-control characters
///
/// POSIX.2024 COMPLIANCE:
/// - Interprets first character of each line as carriage-control character
/// - Supported control characters per POSIX:
///   - ' ' (space): Single spacing (advance one line)
///   - '0': Double spacing (advance two lines with blank line before)
///   - '1': New page (form-feed character)
///   - '+': Overprint (carriage return without line advance)
/// - Special rule: '+' as first character of first line treated as space
/// - Extension: '-' for triple-spacing (non-POSIX)
/// - Processes multiple files sequentially, each with independent state
/// - Reads from stdin if no files specified or "-" given
#[derive(Parser)]
#[command(version, about = gettext("asa - interpret carriage-control characters"))]
struct Args {
    #[arg(help = gettext("Files to read as input"))]
    files: Vec<PathBuf>,
}

/// Why a file stopped: reading it, or writing standard output.
#[derive(Debug)]
enum Failure {
    Input(io::Error),
    Output(io::Error),
}

/// Interpret one input stream, writing the result to `out`.
///
/// Each line's first character -- by `LC_CTYPE`, so one byte in the POSIX
/// locale, where every byte is a character -- is its carriage control, and
/// the rest of the line is written after the motion it asks for:
/// - `' '` and any character POSIX does not name: the next line
/// - `'0'`: a blank line, then the next line
/// - `'-'`: two blank lines (a non-POSIX extension)
/// - `'1'`: a new page
/// - `'+'`: back to the start of the current line, to overprint it; as the
///   first character of the input it is a space
///
/// A line's <newline> is held until the next line's control character is
/// known, since `'+'` replaces it with a <carriage-return>.
///
/// Bytes, not text: input need not be valid in any encoding, and what
/// follows the control character is copied unchanged.
fn asa_stream<R: BufRead, W: Write>(input: &mut R, out: &mut W) -> Result<(), Failure> {
    let mut first_line = true;
    let result = asa_lines(input, out, &mut first_line);
    // The last line's <newline>, held for a '+' that never came -- also
    // when a read failed partway, or the next file's first line would be
    // written onto this one.
    if !first_line {
        out.write_all(b"\n").map_err(Failure::Output)?;
    }
    result
}

/// [`asa_stream`]'s lines, all but the last one's held <newline>, with
/// `first_line` left false once one has been written.
fn asa_lines<R: BufRead, W: Write>(
    input: &mut R,
    out: &mut W,
    first_line: &mut bool,
) -> Result<(), Failure> {
    let mut line: Vec<u8> = Vec::new();
    loop {
        line.clear();
        if input.read_until(b'\n', &mut line).map_err(Failure::Input)? == 0 {
            break;
        }
        let end = line.len() - usize::from(line.ends_with(b"\n"));
        let control_end = next_char_offset(&line, 0).expect("a line is never empty");
        // A multibyte control character is none POSIX names.
        let control = if control_end == 1 { line[0] } else { b' ' };
        let control = if *first_line && control == b'+' {
            b' '
        } else {
            control
        };

        let separator: &[u8] = match (*first_line, control) {
            (true, _) => b"",
            (false, b'+') => b"\r",
            (false, _) => b"\n",
        };
        let advance: &[u8] = match control {
            b'0' => b"\n",
            b'-' => b"\n\n",
            b'1' => b"\x0c",
            _ => b"",
        };
        let text = &line[control_end.min(end)..end];
        for part in [separator, advance, text] {
            out.write_all(part).map_err(Failure::Output)?;
        }
        *first_line = false;
    }
    Ok(())
}

/// Interpret the file `pathname` (standard input for `-`).
fn asa_file<W: Write>(pathname: &Path, out: &mut W) -> Result<(), Failure> {
    let mut reader = input_reader(pathname, true).map_err(Failure::Input)?;
    asa_stream(&mut reader, out)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("asa");

    let mut args = plib::optarg::parse::<Args>();

    // if no files, read from stdin (use "-" to indicate stdin)
    if args.files.is_empty() {
        args.files.push(PathBuf::from("-"));
    }

    // Block-buffered: a write(2) per line made asa thirty times slower than
    // `cat` over the same file.
    let mut out = io::BufWriter::new(io::stdout().lock());
    for filename in &args.files {
        match asa_file(filename, &mut out) {
            Ok(()) => {}
            Err(Failure::Input(e)) => {
                // What came before the failure, before the diagnostic.
                flush_or_exit(&mut out);
                plib::diag::error(&format!(
                    "{}: {}",
                    filename.display(),
                    plib::diag::io_error_text(&e)
                ));
            }
            Err(Failure::Output(e)) => write_error_exit(&e),
        }
    }
    flush_or_exit(&mut out);
    drop(out);
    plib::diag::flush_stdout();

    std::process::exit(plib::diag::exit_status())
}

/// Write out what `out` holds, or report the failure and exit.
fn flush_or_exit<W: Write>(out: &mut W) {
    if let Err(e) = out.flush() {
        write_error_exit(&e);
    }
}

/// Report a failed write to standard output and exit 1: every later file
/// would fail the same way.
fn write_error_exit(e: &io::Error) -> ! {
    plib::diag::write_error(e);
    std::process::exit(1);
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::{BufReader, Read};

    /// Yields `data`, then fails every read, as a disk does when it gives
    /// out partway through a file.
    struct FailsAfter(&'static [u8]);

    impl Read for FailsAfter {
        fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
            if self.0.is_empty() {
                return Err(io::Error::other("the disk gave out"));
            }
            let n = self.0.len().min(buf.len());
            buf[..n].copy_from_slice(&self.0[..n]);
            self.0 = &self.0[n..];
            Ok(n)
        }
    }

    // A file that fails partway still ends its last line, or the next
    // file's first line would be written onto it.
    #[test]
    fn read_error_ends_the_held_line() {
        let mut out = Vec::new();
        let result = asa_stream(&mut BufReader::new(FailsAfter(b" a\n b\n")), &mut out);
        assert!(matches!(result, Err(Failure::Input(_))), "{result:?}");
        assert_eq!(out, b"a\nb\n");
    }

    #[test]
    fn read_error_before_any_line_writes_nothing() {
        let mut out = Vec::new();
        let result = asa_stream(&mut BufReader::new(FailsAfter(b"")), &mut out);
        assert!(matches!(result, Err(Failure::Input(_))), "{result:?}");
        assert_eq!(out, b"");
    }
}
