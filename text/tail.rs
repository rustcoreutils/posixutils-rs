use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use notify_debouncer_full::new_debouncer;
use notify_debouncer_full::notify::event::{ModifyKind, RemoveKind};
use notify_debouncer_full::notify::{EventKind, RecursiveMode, Watcher};
use plib::PROJECT_NAME;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::time::Duration;

/// A wrapper type for `isize` that defaults to negative values if no sign is provided.
///
/// The `SignedIsize` struct is a simple wrapper around the `isize` type, designed to parse
/// a string into an `isize` value that defaults to negative if no explicit sign is provided.
#[derive(Debug, Clone)]
struct SignedIsize(isize);

impl FromStr for SignedIsize {
    type Err = <isize as FromStr>::Err;

    /// Parses a string slice into a `SignedIsize`.
    ///
    /// If the string starts with a '-' or '+' sign, it parses it as is. If no sign is provided,
    /// it defaults to a negative value.
    ///
    /// # Arguments
    /// * `s` - A string slice to parse into a `SignedIsize`.
    ///
    /// # Returns
    /// * `Ok(SignedIsize)` - If parsing is successful.
    /// * `Err(Self::Err)` - If parsing fails (e.g., due to an invalid integer string).
    ///
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with('-') || s.starts_with('+') {
            Ok(SignedIsize(s.parse()?))
        } else {
            Ok(SignedIsize(-s.parse::<isize>()?))
        }
    }
}

/// tail - copy the last part of a file
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// The number of lines to print from the end of the file
    #[arg(short = 'n')]
    lines: Option<SignedIsize>,

    /// The number of bytes to print from the end of the file
    #[arg(short = 'c')]
    bytes: Option<SignedIsize>,

    /// Output appended data as the file grows
    #[arg(short = 'f')]
    follow: bool,

    /// The file to read
    file: Option<PathBuf>,
}

impl Args {
    /// Validates the command-line arguments to ensure they meet the required constraints.
    ///
    /// # Returns
    /// * `Ok(())` if arguments are valid.
    /// * `Err(String)` if arguments are invalid, with an error message describing the issue.
    fn validate_args(&mut self) -> Result<(), String> {
        // Check if conflicting options are used together
        if self.bytes.is_some() && self.lines.is_some() {
            return Err("Options '-c' and '-n' cannot be used together".to_string());
        }

        if self.bytes.is_none() && self.lines.is_none() {
            self.lines = Some(SignedIsize(-10));
        }

        Ok(())
    }
}

/// Prints the last `n` lines from the given reader.
///
/// This function reads from a reader and prints the last `n` lines to standard output.
/// If `n` is negative, it treats `n` as counting from the end of the file. If `n` is
/// non-negative, it counts lines from the start of the file until `n` lines are skipped,
/// then it prints the rest of the file.
///
/// # Type Parameters
/// * `R` - A type that implements `Read` and `BufRead`.
///
/// # Arguments
/// * `reader` - A mutable reference to a reader to read bytes from.
/// * `n` - The number of lines to print from the end. Negative values indicate counting from the end.
///
/// # Returns
/// * `Ok(())` - If the operation completes successfully.
/// * `Err(Box<dyn std::error::Error>)` - If an error occurs during reading.
///
fn print_last_n_lines<R: Read + BufRead>(
    reader: &mut R,
    n: isize,
) -> Result<(), Box<dyn std::error::Error>> {
    // Check if `n` is negative. If so, treat `n` as counting from the end.
    if n < 0 {
        // Convert `n` to an unsigned integer.
        let n = n.unsigned_abs();

        // Create a vector to store the last `n` lines. Preallocate memory for efficiency.
        let mut lines = Vec::with_capacity(n);

        // Temporary storage for each line read from the reader.
        let mut line = String::new();

        // Read each line and store the last `n` lines.
        while reader.read_line(&mut line).map_err(|e| e.to_string())? != 0 {
            // If the vector is full, remove the first (oldest) line.
            if lines.len() == n {
                lines.remove(0);
            }

            // Add the current line to the vector.
            lines.push(line.clone());

            // Clear the temporary storage for the next line.
            line.clear();
        }

        // Print the collected lines, removing any trailing newline characters.
        for line in lines {
            println!("{}", line.trim_end());
        }
    } else {
        // If `n` is non-negative, count lines from the start of the file.
        let mut n = n as usize;

        // Ensure `n` is at least 1 to avoid unnecessary work when `n` is 0.
        if n == 0 {
            n = 1;
        }

        // Buffer to read a single byte at a time.
        let mut empty_buffer = [0; 1];
        let mut line_count = 0;

        // Skip the first `n` lines.
        if n != 1 {
            'skip_lines: loop {
                // Read a single byte from the reader.
                let bytes_read = reader.read(&mut empty_buffer)?;
                if bytes_read == 0 {
                    break;
                }

                // Check if the byte is a newline character.
                if empty_buffer[0] == b'\n' {
                    line_count += 1;

                    // If the required number of lines are skipped, exit the loop.
                    if line_count == n - 1 {
                        break 'skip_lines;
                    }
                }
            }
        }

        // Buffer to read chunks of data from the reader.
        let mut buffer = [0; 1024];

        // Read and print the remaining lines.
        loop {
            // Read up to 1024 bytes into the buffer.
            let bytes_read = reader
                .read(&mut buffer)
                .map_err(|e| format!("Failed to read: {}", e))?;

            // If no more bytes are read, exit the loop.
            if bytes_read == 0 {
                break;
            }

            // Print the bytes read as a string.
            print_bytes(&buffer[..bytes_read]);
        }
    }

    // Return `Ok(())` to indicate successful execution.
    Ok(())
}

/// Prints the last `n` bytes from the given reader.
///
/// This function reads from a reader and prints the last `n` bytes to standard output.
/// If `n` is negative, it treats `n` as counting from the end of the file. If `n` is
/// non-negative, it skips `n` bytes from the start of the file and then prints the rest
/// of the file.
///
/// # Type Parameters
/// * `R` - A type that implements `Read`.
///
/// # Arguments
/// * `buf_reader` - A mutable reference to a reader to read bytes from.
/// * `n` - The number of bytes to print from the end. Negative values indicate counting from the end.
///
/// # Returns
/// * `Ok(())` - If the operation completes successfully.
/// * `Err(Box<dyn std::error::Error>)` - If an error occurs during reading.
///
fn print_last_n_bytes<R: Read>(
    buf_reader: &mut R,
    n: isize,
) -> Result<(), Box<dyn std::error::Error>> {
    // Check if `n` is negative. If so, treat `n` as counting from the end.
    if n < 0 {
        // Convert `n` to an unsigned integer.
        let n = n.unsigned_abs();

        // Buffer to read `n` bytes at a time.
        let mut buffer_1 = vec![0; n];

        // Buffer to store the last `n` bytes read.
        let mut buffer_2 = vec![];

        // Continuously read bytes into buffer_1.
        loop {
            let bytes_read = buf_reader
                .read(&mut buffer_1)
                .map_err(|e| format!("Failed to read: {}", e))?;

            // If the number of bytes read is less than the buffer size, we're at the end of the file.
            if bytes_read < buffer_1.len() {
                // Add the bytes read to buffer_2.
                buffer_2.extend(&buffer_1[..bytes_read]);

                // If buffer_2 contains fewer than `n` bytes, print all of them.
                if buffer_2.len() < n {
                    print_bytes(&buffer_2);
                } else {
                    // Otherwise, print the last `n` bytes from buffer_2.
                    let start = buffer_2.len() - n;
                    print_bytes(&buffer_2[start..]);
                }

                // Exit the loop since we've reached the end of the file.
                break;
            }

            // Clone buffer_1 into buffer_2 for the next iteration.
            buffer_2.clone_from(&buffer_1);
        }
    } else {
        // If `n` is non-negative, skip the first `n` bytes.

        // Convert `n` to an unsigned integer.
        let mut skip = n as usize;

        // Buffer to read one byte at a time.
        let mut empty_buffer = [0; 1];

        // Skip the first `n` bytes.
        while skip > 1 {
            let bytes_read = buf_reader.read(&mut empty_buffer)?;
            if bytes_read == 0 {
                break;
            }
            skip -= 1;
        }

        // Buffer to read chunks of data from the reader.
        let mut buffer = [0; 1024];

        // Read and print the remaining bytes.
        loop {
            let bytes_read = buf_reader
                .read(&mut buffer)
                .map_err(|e| format!("Failed to read: {}", e))?;

            // If no more bytes are read, exit the loop.
            if bytes_read == 0 {
                break;
            }

            // Print the bytes read as a string.
            print_bytes(&buffer[..bytes_read]);
        }
    }

    // Return `Ok(())` to indicate successful execution.
    Ok(())
}

/// Prints a slice of bytes to standard output.
///
/// This function attempts to interpret the byte slice as a UTF-8 string and prints it.
/// If the byte slice is not valid UTF-8, it prints each byte as a character.
///
/// # Arguments
/// * `bytes` - A slice of bytes to print.
///
fn print_bytes(bytes: &[u8]) {
    match std::str::from_utf8(bytes) {
        Ok(valid_str) => print!("{}", valid_str),
        Err(_) => {
            for byte in bytes {
                print!("{}", *byte as char);
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
/// # Arguments
/// * `args` - The command-line arguments parsed into an `Args` struct.
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
///
fn tail(args: &Args) -> Result<(), Box<dyn std::error::Error>> {
    // open file, or stdin
    let file: Box<dyn Read> = {
        if args.file == Some(PathBuf::from("-")) || args.file.is_none() {
            Box::new(io::stdin().lock())
        } else {
            Box::new(File::open(args.file.as_ref().unwrap())?)
        }
    };

    let mut reader = io::BufReader::new(file);

    if let Some(bytes) = &args.bytes {
        print_last_n_bytes(&mut reader, bytes.0)?;
    } else {
        print_last_n_lines(&mut reader, args.lines.as_ref().unwrap().0)?;
    }

    // If follow option is specified, continue monitoring the file
    if args.follow && !(args.file == Some(PathBuf::from("-")) || args.file.is_none()) {
        let file_path = args.file.as_ref().unwrap();

        // Opening a file and placing the cursor at the end of the file
        let mut file = File::open(file_path)?;
        file.seek(SeekFrom::End(0))?;
        let mut reader = BufReader::new(&file);

        let (tx, rx) = std::sync::mpsc::channel();
        // Automatically select the best implementation for your platform.
        let mut debouncer = new_debouncer(Duration::from_millis(1), None, tx).unwrap();

        // Add a path to be watched.
        // below will be monitored for changes.
        debouncer
            .watcher()
            .watch(Path::new(file_path), RecursiveMode::NonRecursive)?;

        for res in rx {
            match res {
                Ok(events) => {
                    let event = events.first().unwrap();
                    match event.kind {
                        EventKind::Modify(ModifyKind::Any)
                        | EventKind::Modify(ModifyKind::Data(_))
                        | EventKind::Modify(ModifyKind::Other) => {
                            // If the file has been modified, check if the file was truncated
                            let metadata = file.metadata()?;
                            let current_size = metadata.len();

                            if current_size < reader.stream_position()? {
                                eprintln!("\ntail: {}: file truncated", file_path.display());
                                reader.seek(SeekFrom::Start(0))?;
                            }

                            // Read the new lines and output them
                            let mut new_data = vec![];
                            let bytes_read = reader.read_to_end(&mut new_data)?;
                            if bytes_read > 0 {
                                print_bytes(&new_data);
                                io::stdout().flush()?;
                            }
                        }
                        EventKind::Remove(RemoveKind::File) => {
                            debouncer.watcher().unwatch(Path::new(file_path))?
                        }
                        _ => {}
                    }
                }
                Err(e) => {
                    eprintln!("watch error: {:?}", e);
                }
            }
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;
    let mut args = Args::parse();
    args.validate_args()?;
    let mut exit_code = 0;

    if let Err(err) = tail(&args) {
        exit_code = 1;
        eprint!("{}", err);
    }

    std::process::exit(exit_code)
}
