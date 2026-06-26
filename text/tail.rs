//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::collections::VecDeque;
use std::error::Error;
use std::fmt::Display;
use std::fs::File;
use std::io::{
    self, BufRead, BufReader, ErrorKind, Read, Seek, SeekFrom, StdinLock, StdoutLock, Write,
};
use std::path::PathBuf;
use std::str::FromStr;
use std::thread;
use std::time::Duration;

use clap::Parser;
use gettextrs::gettext;
use plib::BUFSZ;

/// Interval between read attempts in the `-f` (follow) poll loop.
const FOLLOW_POLL_INTERVAL: Duration = Duration::from_millis(100);

enum RelativeFrom {
    StartOfFile(usize),
    EndOfFile(usize),
}

#[derive(Debug)]
enum RelativeFromFromStrError {
    EmptyString,
    IntegerParseError,
}

impl Display for RelativeFromFromStrError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let st = match self {
            RelativeFromFromStrError::EmptyString => {
                "'number' argument following -c or -n was empty"
            }
            RelativeFromFromStrError::IntegerParseError => {
                "'number' argument following -c or -n could not be parsed as an integer"
            }
        };

        write!(f, "{st}")
    }
}

impl Error for RelativeFromFromStrError {}

impl FromStr for RelativeFrom {
    type Err = RelativeFromFromStrError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut chars = s.chars();

        let (chars_to_use, relative_from_start_of_file) = match chars.next() {
            Some('+') => (chars, true),
            Some('-') => (chars, false),
            // If no explicit sign is provided, then relative from end of file
            Some(_) => (s.chars(), false),
            None => {
                return Err(RelativeFromFromStrError::EmptyString);
            }
        };

        let usize = chars_to_use
            .as_str()
            .parse::<usize>()
            .map_err(|_| RelativeFromFromStrError::IntegerParseError)?;

        let relative_from = if relative_from_start_of_file {
            RelativeFrom::StartOfFile(usize)
        } else {
            RelativeFrom::EndOfFile(usize)
        };

        Ok(relative_from)
    }
}

/// tail - copy the last part of a file
/// If neither -n nor -c are specified, copies the last 10 lines (-n 10).
#[derive(Parser)]
#[command(version, about = gettext("tail - copy the last part of a file"))]
struct Args {
    #[arg(short = 'n', long = "lines", allow_hyphen_values = true,
          help = gettext("The number of lines to print from the end of the file"))]
    lines: Option<String>,

    #[arg(short = 'c', long = "bytes", allow_hyphen_values = true,
          help = gettext("The number of bytes to print from the end of the file"))]
    bytes: Option<String>,

    #[arg(short = 'f', help = gettext("Output appended data as the file grows"))]
    follow: bool,

    #[arg(short = 'r', help = gettext("Copy lines in reverse order"))]
    reverse: bool,

    #[arg(help = gettext("The file to read"))]
    file: Option<PathBuf>,
}

enum BytesOrLines {
    Lines(RelativeFrom),
    Bytes(RelativeFrom),
}

impl Args {
    fn get_bytes_or_lines(&self) -> Result<BytesOrLines, Box<dyn Error>> {
        let bytes_or_lines = match (&self.bytes, &self.lines) {
            (Some(st), None) => BytesOrLines::Bytes(RelativeFrom::from_str(st.as_str())?),
            (None, Some(st)) => BytesOrLines::Lines(RelativeFrom::from_str(st.as_str())?),
            (Some(_), Some(_)) => {
                // Check if conflicting options are used together
                return Err(Box::from(gettext(
                    "options '-c' and '-n' cannot be used together",
                )));
            }
            (None, None) => {
                if self.reverse {
                    // POSIX: with -r the default count is the WHOLE file. Lines
                    // from the start-of-file at offset 1 selects every line.
                    BytesOrLines::Lines(RelativeFrom::StartOfFile(1_usize))
                } else {
                    // The default behavior is the last 10 lines (-n 10)
                    // "If neither -c nor -n is specified, -n 10 shall be assumed."
                    // https://pubs.opengroup.org/onlinepubs/9699919799/utilities/tail.html
                    BytesOrLines::Lines(RelativeFrom::EndOfFile(10_usize))
                }
            }
        };

        Ok(bytes_or_lines)
    }
}

enum FileOrStdin {
    File(PathBuf, BufReader<File>),
    Stdin(StdinLock<'static>),
}

impl FileOrStdin {
    fn get_buf_read(&mut self) -> &mut dyn BufRead {
        match self {
            Self::File(_, ref mut bu) => bu,
            Self::Stdin(ref mut st) => st,
        }
    }
}

fn print_bytes(stdout_lock: &mut StdoutLock, bytes: &[u8]) -> io::Result<()> {
    stdout_lock.write_all(bytes)
}

/// Read every line from `read`, stripping a single trailing `\n` from each.
fn read_all_lines<R: Read + BufRead>(read: &mut R) -> Result<Vec<Vec<u8>>, Box<dyn Error>> {
    let mut lines = Vec::<Vec<u8>>::new();
    let mut line = Vec::<u8>::with_capacity(BUFSZ);

    loop {
        match read.read_until(b'\n', &mut line) {
            Ok(0) => break,
            Ok(_) => {
                if line.last() == Some(&b'\n') {
                    line.pop();
                }
                lines.push(std::mem::take(&mut line));
            }
            Err(er) => {
                if er.kind() == ErrorKind::Interrupted {
                    continue;
                }
                return Err(Box::from(er));
            }
        }
    }

    Ok(lines)
}

/// `-r`: copy the selected lines in reverse order.
///
/// With no count, the whole file is reversed. `-n` (lines) selects which lines
/// (counted from the end, or from the start with `+N`) are reversed.
fn print_reverse_lines<R: Read + BufRead>(
    stdout_lock: &mut StdoutLock,
    read: &mut R,
    relative_from: RelativeFrom,
) -> Result<(), Box<dyn Error>> {
    let lines = read_all_lines(read)?;

    let selected: &[Vec<u8>] = match relative_from {
        RelativeFrom::EndOfFile(n) => {
            if n == 0_usize {
                return Ok(());
            }
            let start = lines.len().saturating_sub(n);
            &lines[start..]
        }
        RelativeFrom::StartOfFile(n) => {
            // +N is 1-based; +0 and +1 both select the whole file.
            let start = n.saturating_sub(1).min(lines.len());
            &lines[start..]
        }
    };

    for line in selected.iter().rev() {
        stdout_lock.write_all(line.as_slice())?;
        stdout_lock.write_all(b"\n")?;
    }

    Ok(())
}

/// `-r` combined with `-c`: reverse the selected bytes.
fn print_reverse_bytes<R: Read>(
    stdout_lock: &mut StdoutLock,
    read: &mut R,
    relative_from: RelativeFrom,
) -> Result<(), Box<dyn Error>> {
    let mut all = Vec::<u8>::new();
    read.read_to_end(&mut all)?;

    let selected: &[u8] = match relative_from {
        RelativeFrom::EndOfFile(n) => {
            if n == 0_usize {
                return Ok(());
            }
            let start = all.len().saturating_sub(n);
            &all[start..]
        }
        RelativeFrom::StartOfFile(n) => {
            let start = n.saturating_sub(1).min(all.len());
            &all[start..]
        }
    };

    let reversed: Vec<u8> = selected.iter().rev().copied().collect();
    print_bytes(stdout_lock, &reversed)?;

    Ok(())
}

fn print_n_lines<R: Read + BufRead>(
    stdout_lock: &mut StdoutLock,
    read: &mut R,
    relative_from: RelativeFrom,
) -> Result<(), Box<dyn std::error::Error>> {
    match relative_from {
        RelativeFrom::EndOfFile(us) => {
            let n = if us == 0_usize {
                // Nothing to read or print
                return Ok(());
            } else {
                us
            };

            // Create a vector to store the last `n` lines. Preallocate memory for efficiency.
            let mut lines = VecDeque::<Vec<u8>>::with_capacity(n);

            // Temporary buffer for the line currently being processed
            let mut line = Vec::<u8>::with_capacity(BUFSZ);

            // Read each line and store the last `n` lines.
            loop {
                match read.read_until(b'\n', &mut line) {
                    Ok(us) => {
                        if us == 0_usize {
                            // Reached EOF
                            break;
                        }

                        let lines_len = lines.len();

                        if let Some(ve) = lines.pop_front() {
                            if lines_len == n {
                                // If the vector is full, remove the first (oldest) line.
                                let mut ve_mut = ve;

                                ve_mut.clear();

                                ve_mut.extend_from_slice(line.as_slice());

                                lines.push_back(ve_mut);
                            } else {
                                // Undo `pop_front`, since it was premature
                                lines.push_front(ve);

                                // Add the current line to the vector.
                                lines.push_back(line.clone());
                            }
                        } else {
                            // Add the current line to the vector.
                            lines.push_back(line.clone());
                        }

                        // Clear the temporary storage for the next line.
                        line.clear();
                    }
                    Err(er) => {
                        if er.kind() == ErrorKind::Interrupted {
                            continue;
                        }

                        return Err(Box::from(er));
                    }
                }
            }

            // Print the collected lines
            for line in lines {
                stdout_lock.write_all(line.as_slice())?;
            }
        }
        RelativeFrom::StartOfFile(us) => {
            // Ensure `n` is at least 1 to avoid unnecessary work when `n` is 0.
            let n = if us == 0_usize { 1_usize } else { us };

            // TODO: performance
            // Buffer to read a single byte at a time.
            let mut empty_buffer = [0_u8; 1_usize];
            let mut line_count = 0_usize;

            // Skip the first `n` lines.
            if n != 1_usize {
                'skip_lines: loop {
                    // Read a single byte from the reader.
                    let bytes_read = read.read(&mut empty_buffer)?;

                    if bytes_read == 0_usize {
                        break;
                    }

                    // Check if the byte is a newline character.
                    if empty_buffer[0_usize] == b'\n' {
                        line_count += 1;

                        // If the required number of lines are skipped, exit the loop.
                        if line_count == n - 1_usize {
                            break 'skip_lines;
                        }
                    }
                }
            }

            // Buffer to read chunks of data from the reader.
            let mut buffer = [0_u8; BUFSZ];

            // Read and print the remaining lines.
            loop {
                let bytes_read = read
                    .read(&mut buffer)
                    .map_err(|er| format!("Failed to read: {er}"))?;

                // If no more bytes are read, exit the loop.
                if bytes_read == 0 {
                    break;
                }

                // Print the bytes read.
                print_bytes(stdout_lock, &buffer[..bytes_read])?;
            }
        }
    }

    Ok(())
}

fn print_n_bytes<R: Read>(
    stdout_lock: &mut StdoutLock,
    read: &mut R,
    relative_from: RelativeFrom,
) -> Result<(), Box<dyn Error>> {
    match relative_from {
        RelativeFrom::EndOfFile(us) => {
            let n = if us == 0_usize {
                // Nothing to read or print
                return Ok(());
            } else {
                us
            };

            // Sliding window holding at most the last `n` bytes seen so far.
            let mut tail_buf = Vec::<u8>::with_capacity(n);

            // Buffer to read chunks of data from the reader.
            let mut chunk = [0_u8; BUFSZ];

            loop {
                let bytes_read = read
                    .read(&mut chunk)
                    .map_err(|er| format!("Failed to read: {er}"))?;

                // Only a zero-byte read signals true EOF; a short read on a slow
                // pipe must not terminate accumulation early.
                if bytes_read == 0_usize {
                    break;
                }

                tail_buf.extend_from_slice(&chunk[..bytes_read]);

                // Keep only the last `n` bytes to bound memory use.
                if tail_buf.len() > n {
                    let excess = tail_buf.len() - n;
                    tail_buf.drain(..excess);
                }
            }

            print_bytes(stdout_lock, &tail_buf)?;
        }
        RelativeFrom::StartOfFile(us) => {
            let mut skip = us;

            // TODO: performance
            // Buffer to read one byte at a time.
            let mut empty_buffer = [0_u8; 1_usize];

            // Skip the first `n` bytes.
            while skip > 1_usize {
                let bytes_read = read.read(&mut empty_buffer)?;

                if bytes_read == 0_usize {
                    break;
                }

                skip -= 1_usize;
            }

            // Buffer to read chunks of data from the reader.
            let mut buffer = [0_u8; BUFSZ];

            // Read and print the remaining bytes.
            loop {
                let bytes_read = read
                    .read(&mut buffer)
                    .map_err(|er| format!("Failed to read: {er}"))?;

                // If no more bytes are read, exit the loop.
                if bytes_read == 0_usize {
                    break;
                }

                // Print the bytes read.
                print_bytes(stdout_lock, &buffer[..bytes_read])?;
            }
        }
    }

    Ok(())
}

/// Follow a file with `-f`, using a sleep-poll loop.
///
/// This handles regular files (appended data appears on the next read), FIFOs
/// (a blocking read returns new data when a writer sends it, EOF when all
/// writers close), and removed/rotated files (a transient EOF, not fatal).
fn follow_file(
    stdout_lock: &mut StdoutLock,
    pa: PathBuf,
    mut bu: BufReader<File>,
) -> Result<(), Box<dyn Error>> {
    // Move the cursor to the end of the file so only new data is emitted.
    // FIFOs are not seekable; ignore the error in that case.
    let _ = bu.seek(SeekFrom::End(0_i64));

    let mut buffer = [0_u8; BUFSZ];

    loop {
        // Detect truncation of a regular file: if the file shrank below our
        // current offset, restart from the beginning.
        if let Ok(metadata) = bu.get_ref().metadata() {
            if metadata.is_file() {
                if let Ok(pos) = bu.stream_position() {
                    if metadata.len() < pos {
                        plib::diag::error(&format!(
                            "{}: {}",
                            pa.display(),
                            gettext("file truncated")
                        ));
                        bu.seek(SeekFrom::Start(0_u64))?;
                    }
                }
            }
        }

        match bu.read(&mut buffer) {
            Ok(0) => {
                // No new data right now. For a regular file this means we are
                // at EOF (more data may be appended later); for a FIFO it means
                // no writer is currently sending. In both cases - including when
                // the file has been removed/rotated - keep waiting.
                thread::sleep(FOLLOW_POLL_INTERVAL);
            }
            Ok(bytes_read) => {
                print_bytes(stdout_lock, &buffer[..bytes_read])?;
                stdout_lock.flush()?;
            }
            Err(er) => {
                if er.kind() == ErrorKind::Interrupted {
                    continue;
                }
                return Err(Box::from(er));
            }
        }
    }
}

/// The main logic for the `tail` command.
///
/// This function processes the command-line arguments to determine how many lines or bytes
/// to print from the end of a specified file or standard input. It supports an option to
/// follow the file, printing new data as it is appended to the file.
///
/// # Returns
/// * `Ok(())` - If the operation completes successfully.
/// * `Err(Box<dyn std::error::Error>)` - If an error occurs during the operation.
///
/// # Errors
/// This function will return an error if:
/// - The specified file cannot be opened.
/// - An error occurs while reading from the file or stdin.
/// - An error occurs while watching the file for changes.
fn tail(
    file: Option<PathBuf>,
    follow: bool,
    reverse: bool,
    bytes_or_lines: BytesOrLines,
) -> Result<(), Box<dyn Error>> {
    fn get_stdin() -> FileOrStdin {
        FileOrStdin::Stdin(io::stdin().lock())
    }

    let mut file_or_stdin = match file {
        Some(pa) => {
            if pa.as_os_str() == "-" {
                get_stdin()
            } else {
                let fi = File::open(pa.as_path())?;

                FileOrStdin::File(pa, BufReader::new(fi))
            }
        }
        None => get_stdin(),
    };

    let mut stdout_lock = io::stdout().lock();

    {
        let mut buf_reader = file_or_stdin.get_buf_read();

        match (reverse, bytes_or_lines) {
            (true, BytesOrLines::Bytes(re)) => {
                print_reverse_bytes(&mut stdout_lock, &mut buf_reader, re)?;
            }
            (true, BytesOrLines::Lines(re)) => {
                print_reverse_lines(&mut stdout_lock, &mut buf_reader, re)?;
            }
            (false, BytesOrLines::Bytes(re)) => {
                print_n_bytes(&mut stdout_lock, &mut buf_reader, re)?;
            }
            (false, BytesOrLines::Lines(re)) => {
                print_n_lines(&mut stdout_lock, &mut buf_reader, re)?;
            }
        }
    }

    // -f only applies to files, and is meaningless with -r (the whole input has
    // already been consumed and reversed).
    if follow && !reverse {
        if let FileOrStdin::File(pa, bu) = file_or_stdin {
            follow_file(&mut stdout_lock, pa, bu)?;
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("tail");

    let args = Args::parse();

    let bytes_or_lines = match args.get_bytes_or_lines() {
        Ok(by) => by,
        Err(bo) => {
            plib::diag::error(&format!("{bo}"));

            std::process::exit(1_i32)
        }
    };

    if let Err(er) = tail(args.file, args.follow, args.reverse, bytes_or_lines) {
        plib::diag::error(&format!("{er}"));
    }

    std::process::exit(plib::diag::exit_status())
}
