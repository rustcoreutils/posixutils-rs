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
use plib::lzw::UnixLZWWriter;
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::os::unix::fs::PermissionsExt;
use std::path::PathBuf;
use std::time::SystemTime;

const NAME_MAX: usize = 255;

/// compress - compress data
#[derive(Parser)]
#[command(version, about = gettext("compress - compress data"))]
struct Args {
    #[arg(short = 'b', help = gettext("Specify the maximum number of bits to use in a code. 9 <= bits <= 14"))]
    bits: Option<u32>,

    #[arg(short = 'c', long, help = gettext("Write to the standard output; the input file is not changed, and no .Z files are created"))]
    stdout: bool,

    #[arg(short = 'f', long, help = gettext("Do not prompt for overwriting files"))]
    force: bool,

    #[arg(short = 'v', long, help = gettext("Write messages to standard error concerning the expansion of each file"))]
    verbose: bool,

    #[arg(help = gettext("Files to read as input. Use \"-\" or no-args for stdin"))]
    files: Vec<PathBuf>,
}

/// Check if pathname represents stdin ("-")
fn is_stdin(pathname: &std::path::Path) -> bool {
    pathname.as_os_str() == "-"
}

fn prompt_user(prompt: &str) -> bool {
    eprint!("compress: {} ", prompt);
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

        // Set access and modification times using filetime crate would be ideal,
        // but we can use libc directly
        #[cfg(unix)]
        {
            use std::ffi::CString;
            use std::os::unix::ffi::OsStrExt;

            let path_cstr = CString::new(path.as_os_str().as_bytes())?;

            // Convert SystemTime to timeval
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

fn compress_file(args: &Args, pathname: &PathBuf) -> io::Result<i32> {
    let reading_stdin = is_stdin(pathname);

    // Read input - support both empty path and "-" for stdin
    let mut file = input_stream(pathname, true)?;

    // Save original file metadata before compression (only for files, not stdin)
    let orig_metadata = if !reading_stdin {
        Some(FileMetadata::from_path(pathname)?)
    } else {
        None
    };

    let mut encoder = UnixLZWWriter::new(args.bits);

    let mut inp_buf = Vec::new();
    file.read_to_end(&mut inp_buf)?;
    let inp_buf_size = inp_buf.len();

    let mut out_buf = encoder.write(&inp_buf)?;
    out_buf.extend_from_slice(&encoder.close()?);
    let out_buf_size = out_buf.len();

    // When writing to stdout (-c flag) or reading from stdin, always output
    // regardless of size. The "would expand" check only applies to file replacement.
    let writing_to_stdout = args.stdout || reading_stdin;

    if writing_to_stdout {
        // Always output to stdout
        io::stdout().write_all(&out_buf)?;

        // Verbose output goes to stderr
        if args.verbose && !reading_stdin {
            let ratio = if inp_buf_size > 0 {
                100.0 - (out_buf_size as f64 / inp_buf_size as f64) * 100.0
            } else {
                0.0
            };
            eprintln!("{}: Compression: {:.1}%", pathname.display(), ratio);
        }
        return Ok(0);
    }

    // File replacement mode - check if compression is beneficial
    if out_buf_size >= inp_buf_size && !args.force {
        // File would not be reduced in size
        return Ok(2);
    }

    // Create output filename with .Z extension
    let fname = format!("{}.Z", pathname.file_name().unwrap().to_str().unwrap());

    // If adding .Z exceeds NAME_MAX, write to stdout instead
    if fname.len() > NAME_MAX {
        io::stdout().write_all(&out_buf)?;
        return Ok(0);
    }

    // Build full path in same directory as source
    let output_path = if let Some(parent) = pathname.parent() {
        if parent.as_os_str().is_empty() {
            PathBuf::from(&fname)
        } else {
            parent.join(&fname)
        }
    } else {
        PathBuf::from(&fname)
    };

    // Check for existing file and prompt if needed
    if output_path.exists() && !args.force {
        let is_affirm = prompt_user(&gettext!(
            "Do you want to overwrite {} (y)es or (n)o?",
            output_path.display()
        ));

        if !is_affirm {
            eprintln!("{} not overwritten", output_path.display());
            return Ok(1);
        }
    }

    // Create compressed file
    let mut f = File::create(&output_path)?;
    f.write_all(&out_buf)?;
    drop(f);

    // Apply original file's metadata to new file
    if let Some(ref meta) = orig_metadata {
        let _ = meta.apply_to(&output_path);
    }

    // Remove original file
    fs::remove_file(pathname)?;

    // Verbose output to stderr
    if args.verbose {
        let ratio = if inp_buf_size > 0 {
            100.0 - (out_buf_size as f64 / inp_buf_size as f64) * 100.0
        } else {
            0.0
        };
        eprintln!(
            "{}: -- replaced with {} Compression: {:.1}%",
            pathname.display(),
            output_path.display(),
            ratio
        );
    }

    Ok(0)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args = Args::parse();

    // If no files specified, read from stdin
    if args.files.is_empty() {
        args.files.push(PathBuf::from("-"));
    }

    let mut exit_code = 0;

    for filename in &args.files {
        match compress_file(&args, filename) {
            Ok(code) => {
                // Track worst exit code: 1 (error) > 2 (would expand) > 0 (success)
                if code == 1 || (code == 2 && exit_code == 0) {
                    exit_code = code;
                }
            }
            Err(e) => {
                exit_code = 1;
                let display_name = if is_stdin(filename) {
                    "stdin".to_string()
                } else {
                    filename.display().to_string()
                };
                eprintln!("compress: {}: {}", display_name, e);
            }
        }
    }

    std::process::exit(exit_code)
}
