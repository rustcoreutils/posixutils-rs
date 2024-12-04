use std::collections::VecDeque;
use std::error::Error;
use std::fmt::Display;
use std::fs::File;
use std::io::{
    self, BufRead, BufReader, ErrorKind, Read, Seek, SeekFrom, StdinLock, StdoutLock, Write,
};
use std::path::PathBuf;
use std::str::FromStr;
use std::sync::mpsc;
use std::time::Duration;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use notify_debouncer_full::new_debouncer;
use notify_debouncer_full::notify::event::{ModifyKind, RemoveKind};
use notify_debouncer_full::notify::{EventKind, RecursiveMode, Watcher};
use plib::BUFSZ;

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
#[command(version, about)]
struct Args {
    /// The number of lines to print from the end of the file
    #[arg(short = 'n', long = "lines", allow_hyphen_values = true)]
    lines: Option<String>,

    /// The number of bytes to print from the end of the file
    #[arg(short = 'c', long = "bytes", allow_hyphen_values = true)]
    bytes: Option<String>,

    /// Output appended data as the file grows
    #[arg(short = 'f')]
    follow: bool,

    /// The file to read
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
                return Err(Box::from("options '-c' and '-n' cannot be used together"));
            }
            (None, None) => {
                // The default behavior is the last 10 lines (-n 10)
                // "If neither -c nor -n is specified, -n 10 shall be assumed."
                // https://pubs.opengroup.org/onlinepubs/9699919799/utilities/tail.html
                BytesOrLines::Lines(RelativeFrom::EndOfFile(10_usize))
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

            // Buffer to read `n` bytes at a time.
            let mut buffer_1 = vec![0_u8; n];

            // Buffer to store the last `n` bytes read.
            let mut buffer_2 = Vec::<u8>::new();

            // Continuously read bytes into buffer_1.
            loop {
                let bytes_read = read
                    .read(&mut buffer_1)
                    .map_err(|er| format!("Failed to read: {er}"))?;

                // If the number of bytes read is less than the buffer size, we're at the end of the file.
                if bytes_read < buffer_1.len() {
                    // Add the bytes read to buffer_2.
                    buffer_2.extend(&buffer_1[..bytes_read]);

                    // If buffer_2 contains fewer than `n` bytes, print all of them.
                    if buffer_2.len() < n {
                        print_bytes(stdout_lock, &buffer_2)?;
                    } else {
                        // Otherwise, print the last `n` bytes from buffer_2.
                        let start = buffer_2.len() - n;
                        print_bytes(stdout_lock, &buffer_2[start..])?;
                    }

                    // Exit the loop since we've reached the end of the file.
                    break;
                }

                // Clone buffer_1 into buffer_2 for the next iteration.
                buffer_2.clone_from(&buffer_1);
            }
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

        match bytes_or_lines {
            BytesOrLines::Bytes(re) => {
                print_n_bytes(&mut stdout_lock, &mut buf_reader, re)?;
            }
            BytesOrLines::Lines(re) => {
                print_n_lines(&mut stdout_lock, &mut buf_reader, re)?;
            }
        }
    }

    if follow {
        // If follow option is specified, continue monitoring the file
        if let FileOrStdin::File(pa, mut bu) = file_or_stdin {
            // Move the the cursor to the end of the file
            // Is this still necessary now that the `BufReader` and underlying `File` are being reused?
            bu.seek(SeekFrom::End(0_i64))?;

            let (tx, rx) = mpsc::channel();

            // Automatically select the best implementation for your platform.
            let mut debouncer = new_debouncer(Duration::from_millis(1_u64), None, tx).unwrap();

            // Add a path to be watched.
            // below will be monitored for changes.
            debouncer
                .watcher()
                .watch(pa.as_path(), RecursiveMode::NonRecursive)?;

            for res in rx {
                match res {
                    Ok(events) => {
                        let event = events.first().unwrap();
                        match event.kind {
                            EventKind::Modify(ModifyKind::Any)
                            | EventKind::Modify(ModifyKind::Data(_))
                            | EventKind::Modify(ModifyKind::Other) => {
                                // If the file has been modified, check if the file was truncated
                                let metadata = bu.get_mut().metadata()?;
                                let current_size = metadata.len();

                                if current_size < bu.stream_position()? {
                                    eprintln!("\ntail: {}: file truncated", pa.display());

                                    bu.seek(SeekFrom::Start(0_u64))?;
                                }

                                // Read the new lines and output them
                                let mut new_data = Vec::<u8>::new();

                                let bytes_read = bu.read_to_end(&mut new_data)?;

                                if bytes_read > 0_usize {
                                    print_bytes(&mut stdout_lock, &new_data)?;

                                    io::stdout().flush()?;
                                }
                            }
                            EventKind::Remove(RemoveKind::File) => {
                                debouncer.watcher().unwatch(pa.as_path())?
                            }
                            _ => {}
                        }
                    }
                    Err(ve) => {
                        eprintln!("tail: watch error: {ve:?}");
                    }
                }
            }
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let bytes_or_lines = match args.get_bytes_or_lines() {
        Ok(by) => by,
        Err(bo) => {
            eprintln!("tail: {bo}");

            std::process::exit(1_i32)
        }
    };

    let mut exit_code = 0_i32;

    if let Err(er) = tail(args.file, args.follow, bytes_or_lines) {
        exit_code = 1_i32;

        eprintln!("tail: {}", er);
    }

    std::process::exit(exit_code)
}
