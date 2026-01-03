//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::io::input_stream;
use plib::lzw::UnixLZWReader;
use std::fs::{self, File};
use std::io::{self, Write};
use std::os::unix::fs::PermissionsExt;
use std::path::PathBuf;
use std::time::SystemTime;

/// uncompress - expand compressed data
#[derive(Parser)]
#[command(version, about = gettext("uncompress - expand compressed data"))]
struct Args {
    #[arg(short = 'c', long, help = gettext("Write to standard output; no files are changed"))]
    stdout: bool,

    #[arg(short, long, help = gettext("Do not prompt for overwriting files"))]
    force: bool,

    #[arg(short, long, help = gettext("Write messages to standard error concerning the expansion of each file"))]
    verbose: bool,

    #[arg(help = gettext("Files to read as input. Use \"-\" or no-args for stdin"))]
    files: Vec<PathBuf>,
}

/// Check if pathname represents stdin ("-")
fn is_stdin(pathname: &std::path::Path) -> bool {
    pathname.as_os_str() == "-"
}

fn prog_is_zcat() -> bool {
    let progname = std::env::args().next().unwrap();
    progname.ends_with("zcat")
}

fn prompt_user(prompt: &str) -> bool {
    eprint!("uncompress: {} ", prompt);
    let mut response = String::new();
    io::stdin().read_line(&mut response).unwrap();
    response.to_lowercase().starts_with('y')
}

/// Saved file metadata for preservation
struct FileMetadata {
    mode: u32,
    atime: SystemTime,
    mtime: SystemTime,
}

impl FileMetadata {
    fn from_path(path: &PathBuf) -> io::Result<Self> {
        let meta = fs::metadata(path)?;
        Ok(Self {
            mode: meta.permissions().mode(),
            atime: meta.accessed()?,
            mtime: meta.modified()?,
        })
    }

    fn apply_to(&self, path: &PathBuf) -> io::Result<()> {
        // Set permissions
        let perms = fs::Permissions::from_mode(self.mode);
        fs::set_permissions(path, perms)?;

        // Set access and modification times
        #[cfg(unix)]
        {
            use std::ffi::CString;
            use std::os::unix::ffi::OsStrExt;

            let path_cstr = CString::new(path.as_os_str().as_bytes())?;

            fn to_timeval(time: SystemTime) -> libc::timeval {
                match time.duration_since(std::time::UNIX_EPOCH) {
                    Ok(d) => libc::timeval {
                        tv_sec: d.as_secs() as libc::time_t,
                        tv_usec: d.subsec_micros() as libc::suseconds_t,
                    },
                    Err(_) => libc::timeval {
                        tv_sec: 0,
                        tv_usec: 0,
                    },
                }
            }

            let times = [to_timeval(self.atime), to_timeval(self.mtime)];
            unsafe {
                libc::utimes(path_cstr.as_ptr(), times.as_ptr());
            }
        }

        Ok(())
    }
}

/// Decompress data from decoder and return as bytes
fn decompress_data(mut decoder: UnixLZWReader) -> io::Result<Vec<u8>> {
    let mut output = Vec::new();
    loop {
        let buf = decoder.read()?;
        if buf.is_empty() {
            break;
        }
        output.extend_from_slice(&buf);
    }
    Ok(output)
}

fn uncompress_file(args: &Args, pathname: &std::path::Path, is_zcat: bool) -> io::Result<()> {
    let reading_stdin = is_stdin(pathname);
    let writing_to_stdout = args.stdout || reading_stdin || is_zcat;

    // Determine the actual input file path
    // POSIX: If file has .Z suffix, use as input; else append .Z to find input
    let input_path = if reading_stdin || pathname.extension().is_some_and(|ext| ext == "Z") {
        pathname.to_path_buf()
    } else {
        // Try appending .Z
        let mut with_z = pathname.to_path_buf();
        let mut new_name = pathname.file_name().unwrap_or_default().to_os_string();
        new_name.push(".Z");
        with_z.set_file_name(new_name);
        with_z
    };

    // Save metadata from input file before decompression (for file output mode)
    let orig_metadata = if !reading_stdin {
        Some(FileMetadata::from_path(&input_path)?)
    } else {
        None
    };

    // Read and decompress
    let file = input_stream(&input_path, true)?;
    let decoder = UnixLZWReader::new(file);
    let decompressed = decompress_data(decoder)?;
    let decompressed_size = decompressed.len();

    if writing_to_stdout {
        io::stdout().write_all(&decompressed)?;

        if args.verbose && !reading_stdin {
            eprintln!("{}: -- decompressed", input_path.display());
        }
        return Ok(());
    }

    // File output mode - determine output filename (remove .Z)
    let output_path = if input_path.extension().is_some_and(|ext| ext == "Z") {
        let mut output = input_path.clone();
        output.set_extension("");
        output
    } else {
        // Shouldn't happen since we added .Z above, but handle gracefully
        let mut output = input_path.clone();
        let stem = input_path.file_stem().unwrap_or_default().to_os_string();
        output.set_file_name(stem);
        output
    };

    // Check for existing output file and prompt if needed
    if output_path.exists() && !args.force {
        let is_affirm = prompt_user(&gettext!(
            "Do you want to overwrite {} (y)es or (n)o?",
            output_path.display()
        ));

        if !is_affirm {
            eprintln!("{} not overwritten", output_path.display());
            return Ok(());
        }
    }

    // Write decompressed data to output file
    let mut f = File::create(&output_path)?;
    f.write_all(&decompressed)?;
    drop(f);

    // Apply original file's metadata to new file
    if let Some(ref meta) = orig_metadata {
        let _ = meta.apply_to(&output_path);
    }

    // Remove original compressed file
    fs::remove_file(&input_path)?;

    // Verbose output
    if args.verbose {
        // Calculate compression ratio that was used
        let input_meta = orig_metadata.as_ref();
        if let Some(_meta) = input_meta {
            // We'd need original compressed size, but we already deleted it
            // Just report the expansion
            eprintln!(
                "{}: -- replaced with {} ({} bytes)",
                input_path.display(),
                output_path.display(),
                decompressed_size
            );
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args = Args::parse();

    // zcat is a special case: always write to stdout
    let is_zcat = prog_is_zcat();
    if is_zcat {
        args.stdout = true;
    }

    // If no file args, read from stdin
    if args.files.is_empty() {
        args.files.push(PathBuf::from("-"));
    }

    // POSIX: If stdin is not a terminal and -f is not given, error out
    // (This prevents accidental piping without explicit intent)
    // We check this per-file below for stdin case

    let mut exit_code = 0;

    for filename in &args.files {
        // For stdin without -f, check if stdin is a terminal
        if is_stdin(filename) && !args.force && !args.stdout {
            // Check if stdin is a tty
            if unsafe { libc::isatty(libc::STDIN_FILENO) } == 0 {
                // stdin is not a terminal and -f not given
                // According to POSIX, this should be an error for uncompress
                // But if reading from stdin, we're outputting to stdout anyway
            }
        }

        if let Err(e) = uncompress_file(&args, filename, is_zcat) {
            exit_code = 1;
            let display_name = if is_stdin(filename) {
                "stdin".to_string()
            } else {
                filename.display().to_string()
            };
            eprintln!("uncompress: {}: {}", display_name, e);
        }
    }

    std::process::exit(exit_code)
}
