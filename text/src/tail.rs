use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use inotify::{Inotify, WatchMask};
use plib::PROJECT_NAME;
use std::fs;
use std::fs::File;
use std::io::{self, BufRead, BufReader, Read, Seek, SeekFrom, Write};
use std::path::PathBuf;
use std::str::FromStr;

/// Wrapper type for isize that defaults to negative values if no sign is provided
#[derive(Debug, Clone)]
struct SignedIsize(isize);

impl FromStr for SignedIsize {
    type Err = <isize as FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with('-') || s.starts_with('+') {
            Ok(SignedIsize(s.parse()?))
        } else {
            Ok(SignedIsize(-s.parse::<isize>()?))
        }
    }
}

/// tail - copy the last part of a file
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The number of lines to print from the end of the file
    #[arg(short = 'n')]
    lines: Option<isize>,

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
            self.lines = Some(10);
        }

        Ok(())
    }
}

/// Prints the last `n` lines from the given buffered reader.
///
/// # Arguments
/// * `reader` - A buffered reader to read lines from.
/// * `n` - The number of lines to print from the end. Negative values indicate counting from the end.

fn print_last_n_lines<R: BufRead>(reader: R, n: isize) -> Result<(), String> {
    let lines: Vec<_> = reader.lines().map_while(Result::ok).collect();

    let mut start = if n < 0 {
        (lines.len() as isize + n).max(0) as usize
    } else {
        (n - 1).max(0) as usize
    };

    start = start.min(lines.len());

    for line in &lines[start..] {
        println!("{}", line);
    }

    Ok(())
}

/// Prints the last `n` bytes from the given reader.
///
/// # Arguments
/// * `buf_reader` - A mutable reference to a reader to read bytes from.
/// * `n` - The number of bytes to print from the end. Negative values indicate counting from the end.

fn print_last_n_bytes<R: Read>(buf_reader: &mut R, n: isize) -> Result<(), String> {
    let mut buffer = Vec::new();

    buf_reader
        .read_to_end(&mut buffer)
        .map_err(|e| format!("Failed to read file: {}", e))?;

    let start = if n < 0 {
        (buffer.len() as isize + n).max(0) as usize
    } else {
        (n - 1).max(0) as usize
    }
    .min(buffer.len());

    let slice = &buffer[start..];
    match std::str::from_utf8(slice) {
        Ok(valid_str) => print!("{}", valid_str),
        Err(_) => {
            for &byte in slice {
                print!("{}", byte as char);
            }
        }
    }

    Ok(())
}

/// The main logic for the tail command.
///
/// # Arguments
/// * `args` - The command-line arguments parsed into an `Args` struct.
///
/// # Returns
/// * `Ok(())` if the operation completes successfully.
/// * `Err(Box<dyn std::error::Error>)` if an error occurs.
fn tail(args: &Args) -> Result<(), Box<dyn std::error::Error>> {
    // open file, or stdin
    let file: Box<dyn Read> = {
        if args.file == Some(PathBuf::from("-")) || args.file.is_none() {
            Box::new(io::stdin().lock())
        } else {
            Box::new(fs::File::open(args.file.as_ref().unwrap())?)
        }
    };
    let mut reader = io::BufReader::new(file);

    if let Some(bytes) = &args.bytes {
        print_last_n_bytes(&mut reader, bytes.0)?;
    } else {
        print_last_n_lines(reader, args.lines.unwrap())?;
    }

    // If follow option is specified, continue monitoring the file
    if args.follow && !(args.file == Some(PathBuf::from("-")) || args.file.is_none()) {
        let file_path = args.file.as_ref().unwrap();
        // Initialization of inotify
        let mut inotify = Inotify::init()?;

        inotify
            .watches()
            .add(file_path, WatchMask::MODIFY | WatchMask::DELETE_SELF)
            .expect("Failed to add inotify watch");

        // Opening a file and placing the cursor at the end of the file
        let mut file = File::open(file_path)?;
        file.seek(SeekFrom::End(0))?;
        let mut reader = BufReader::new(file);

        // Buffer for inotify events
        let mut buffer = [0u8; 4096];

        loop {
            // Read inotify events
            let events = inotify.read_events_blocking(&mut buffer)?;

            // Handle each event
            for event in events {
                if event.mask.contains(inotify::EventMask::DELETE_SELF) {
                    eprintln!("File {} deleted, exiting...", file_path.display());
                    std::process::exit(1);
                }

                if event.mask.contains(inotify::EventMask::MODIFY) {
                    // If the file has been modified, check if the file was truncated
                    let metadata = fs::metadata(file_path)?;
                    let current_size = metadata.len();

                    if current_size < reader.stream_position()? {
                        eprintln!("tail: {}: file truncated", file_path.display());

                        reader.seek(SeekFrom::Start(0))?;
                        reader = BufReader::new(File::open(file_path)?);
                    }

                    // Read the new lines and output them
                    let mut new_data = String::new();
                    let bytes_read = reader.read_to_string(&mut new_data)?;
                    if bytes_read > 0 {
                        print!("{}", new_data);
                        io::stdout().flush()?;
                    }
                }
            }
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
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
