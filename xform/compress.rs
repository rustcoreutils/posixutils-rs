//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use flate2::read::GzDecoder;
use flate2::write::GzEncoder;
use flate2::Compression;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::io::input_stream;
use plib::lzw::{UnixLZWReader, UnixLZWWriter};
use std::fs::{self, File};
use std::io::{self, Read, Write};
use std::os::unix::fs::{MetadataExt, PermissionsExt};
use std::path::{Path, PathBuf};
use std::time::SystemTime;

const NAME_MAX: usize = 255;

/// Compression algorithm
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Algorithm {
    Lzw,
    Deflate,
}

impl Algorithm {
    fn suffix(&self) -> &'static str {
        match self {
            Algorithm::Lzw => ".Z",
            Algorithm::Deflate => ".gz",
        }
    }

    fn from_magic(data: &[u8]) -> Option<Self> {
        if data.len() < 2 {
            return None;
        }
        match (data[0], data[1]) {
            (0x1F, 0x9D) => Some(Algorithm::Lzw),
            (0x1F, 0x8B) => Some(Algorithm::Deflate),
            _ => None,
        }
    }
}

/// Program invocation mode based on argv[0]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ProgramMode {
    Compress,
    Uncompress,
    Zcat,
}

impl ProgramMode {
    fn detect() -> Self {
        let prog = std::env::args().next().unwrap_or_default();
        if prog.ends_with("zcat") {
            ProgramMode::Zcat
        } else if prog.ends_with("uncompress") {
            ProgramMode::Uncompress
        } else {
            ProgramMode::Compress
        }
    }
}

/// compress - compress and decompress data
#[derive(Parser)]
#[command(version, about = gettext("compress - compress and decompress data"))]
struct Args {
    #[arg(short = 'b', help = gettext("For LZW: max bits (9-16). For DEFLATE: compression level (1-9)"))]
    bits: Option<u32>,

    #[arg(short = 'c', long, help = gettext("Write to standard output; no files are changed"))]
    stdout: bool,

    #[arg(short = 'd', long, help = gettext("Decompress files"))]
    decompress: bool,

    #[arg(short = 'f', long, help = gettext("Force compression/decompression; do not prompt"))]
    force: bool,

    #[arg(short = 'g', help = gettext("Equivalent to -m gzip"))]
    gzip: bool,

    #[arg(short = 'm', help = gettext("Use algorithm: lzw, deflate, or gzip"))]
    algo: Option<String>,

    #[arg(short = 'v', long, help = gettext("Write messages to standard error"))]
    verbose: bool,

    #[arg(help = gettext("Files to process. Use \"-\" or no args for stdin"))]
    files: Vec<PathBuf>,
}

/// Check if pathname represents stdin ("-")
fn is_stdin(pathname: &Path) -> bool {
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
    fn from_path(path: &Path) -> io::Result<Self> {
        let meta = fs::metadata(path)?;
        Ok(Self {
            mode: meta.permissions().mode(),
            atime: meta.accessed()?,
            mtime: meta.modified()?,
        })
    }

    fn apply_to(&self, path: &Path) -> io::Result<()> {
        let perms = fs::Permissions::from_mode(self.mode);
        fs::set_permissions(path, perms)?;

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

/// Check for multiple hard links
fn check_hard_links(path: &Path, force: bool) -> io::Result<bool> {
    let meta = fs::metadata(path)?;
    if meta.nlink() > 1 {
        eprintln!(
            "compress: {}: has {} hard links",
            path.display(),
            meta.nlink()
        );
        if !force {
            return Ok(false);
        }
    }
    Ok(true)
}

/// Check if output path would exceed PATH_MAX
fn check_path_max(path: &Path) -> io::Result<()> {
    let path_len = path.as_os_str().len();
    #[cfg(unix)]
    {
        if path_len > libc::PATH_MAX as usize {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "pathname too long",
            ));
        }
    }
    let _ = path_len; // silence unused warning on non-unix
    Ok(())
}

/// Compress data using LZW algorithm
fn compress_lzw(data: &[u8], bits: Option<u32>) -> io::Result<Vec<u8>> {
    let mut encoder = UnixLZWWriter::new(bits);
    let mut out = encoder.write(data)?;
    out.extend_from_slice(&encoder.close()?);
    Ok(out)
}

/// Compress data using DEFLATE/gzip algorithm
fn compress_gzip(data: &[u8], level: Option<u32>) -> io::Result<Vec<u8>> {
    let level = level.unwrap_or(6);
    let mut encoder = GzEncoder::new(Vec::new(), Compression::new(level));
    encoder.write_all(data)?;
    encoder.finish()
}

/// Decompress data using LZW algorithm
fn decompress_lzw(data: &[u8]) -> io::Result<Vec<u8>> {
    let cursor = io::Cursor::new(data.to_vec());
    let mut decoder = UnixLZWReader::new(Box::new(cursor));
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

/// Decompress data using DEFLATE/gzip algorithm
fn decompress_gzip(data: &[u8]) -> io::Result<Vec<u8>> {
    let mut decoder = GzDecoder::new(data);
    let mut output = Vec::new();
    decoder.read_to_end(&mut output)?;
    Ok(output)
}

/// Auto-detect algorithm from data and decompress
fn decompress_auto(data: &[u8]) -> io::Result<Vec<u8>> {
    match Algorithm::from_magic(data) {
        Some(Algorithm::Lzw) => decompress_lzw(data),
        Some(Algorithm::Deflate) => decompress_gzip(data),
        None => Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "unknown compression format",
        )),
    }
}

/// Parse algorithm from string
fn parse_algorithm(s: &str) -> io::Result<Algorithm> {
    match s.to_lowercase().as_str() {
        "lzw" => Ok(Algorithm::Lzw),
        "deflate" | "gzip" => Ok(Algorithm::Deflate),
        _ => Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("unknown algorithm: {}", s),
        )),
    }
}

/// Validate -b value for the given algorithm
fn validate_bits(algo: Algorithm, bits: u32) -> io::Result<()> {
    match algo {
        Algorithm::Lzw => {
            // POSIX compress specifies 9-14 bit range
            if !(9..=14).contains(&bits) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "LZW bits must be 9-14 (POSIX)",
                ));
            }
        }
        Algorithm::Deflate => {
            if !(1..=9).contains(&bits) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "DEFLATE level must be 1-9",
                ));
            }
        }
    }
    Ok(())
}

/// Determine algorithm for compression
fn get_compress_algorithm(args: &Args) -> io::Result<Algorithm> {
    if args.gzip {
        return Ok(Algorithm::Deflate);
    }
    if let Some(ref algo_str) = args.algo {
        return parse_algorithm(algo_str);
    }
    Ok(Algorithm::Lzw) // default
}

/// Build output path for compression
fn compress_output_path(input: &Path, algo: Algorithm) -> io::Result<PathBuf> {
    let file_name = input.file_name().ok_or_else(|| {
        io::Error::new(io::ErrorKind::InvalidInput, "input path has no filename")
    })?;
    let fname = format!("{}{}", file_name.to_string_lossy(), algo.suffix());

    if fname.len() > NAME_MAX {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "filename too long",
        ));
    }

    let output = if let Some(parent) = input.parent() {
        if parent.as_os_str().is_empty() {
            PathBuf::from(&fname)
        } else {
            parent.join(&fname)
        }
    } else {
        PathBuf::from(&fname)
    };

    check_path_max(&output)?;
    Ok(output)
}

/// Build output path for decompression (remove suffix)
fn decompress_output_path(input: &Path) -> PathBuf {
    let mut output = input.to_path_buf();
    // Remove .Z or .gz extension
    if let Some(ext) = input.extension() {
        if ext == "Z" || ext == "gz" {
            output.set_extension("");
        }
    }
    output
}

/// Find input file for decompression (try adding .Z if needed)
fn find_decompress_input(pathname: &Path) -> PathBuf {
    // If file exists as-is, use it
    if pathname.exists() {
        return pathname.to_path_buf();
    }

    // Check if it already has a known suffix
    if let Some(ext) = pathname.extension() {
        if ext == "Z" || ext == "gz" {
            return pathname.to_path_buf();
        }
    }

    // Try .Z suffix (per POSIX: default)
    let mut with_z = pathname.to_path_buf();
    let mut new_name = pathname.file_name().unwrap_or_default().to_os_string();
    new_name.push(".Z");
    with_z.set_file_name(new_name);
    if with_z.exists() {
        return with_z;
    }

    // Try .gz suffix
    let mut with_gz = pathname.to_path_buf();
    let mut new_name = pathname.file_name().unwrap_or_default().to_os_string();
    new_name.push(".gz");
    with_gz.set_file_name(new_name);
    if with_gz.exists() {
        return with_gz;
    }

    // Fall back to original with .Z appended (will error on open)
    with_z
}

/// Process a single file for compression
fn compress_file(args: &Args, pathname: &Path, algo: Algorithm) -> io::Result<i32> {
    let reading_stdin = is_stdin(pathname);

    // Warn if input already has compression suffix
    if !reading_stdin {
        if let Some(ext) = pathname.extension() {
            if ext == "Z" || ext == "gz" {
                eprintln!(
                    "compress: {}: already has {} suffix",
                    pathname.display(),
                    ext.to_str().unwrap()
                );
            }
        }
    }

    // Check hard links
    if !reading_stdin && !check_hard_links(pathname, args.force)? {
        return Ok(1);
    }

    // Read input
    let mut file = input_stream(&pathname.to_path_buf(), true)?;
    let orig_metadata = if !reading_stdin {
        Some(FileMetadata::from_path(pathname)?)
    } else {
        None
    };

    let mut inp_buf = Vec::new();
    file.read_to_end(&mut inp_buf)?;
    let inp_buf_size = inp_buf.len();

    // Validate bits if specified
    if let Some(bits) = args.bits {
        validate_bits(algo, bits)?;
    }

    // Compress
    let out_buf = match algo {
        Algorithm::Lzw => compress_lzw(&inp_buf, args.bits)?,
        Algorithm::Deflate => compress_gzip(&inp_buf, args.bits)?,
    };
    let out_buf_size = out_buf.len();

    let writing_to_stdout = args.stdout || reading_stdin;

    if writing_to_stdout {
        io::stdout().write_all(&out_buf)?;
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

    // File replacement mode
    if out_buf_size >= inp_buf_size && !args.force {
        return Ok(2);
    }

    let output_path = compress_output_path(pathname, algo)?;

    // Check for existing file
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

    // Write compressed file
    let mut f = File::create(&output_path)?;
    f.write_all(&out_buf)?;
    drop(f);

    // Apply metadata
    if let Some(ref meta) = orig_metadata {
        let _ = meta.apply_to(&output_path);
    }

    // Remove original
    fs::remove_file(pathname)?;

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

/// Process a single file for decompression
fn decompress_file(args: &Args, pathname: &Path) -> io::Result<i32> {
    let reading_stdin = is_stdin(pathname);
    let writing_to_stdout = args.stdout || reading_stdin;

    // Find actual input file
    let input_path = if reading_stdin {
        pathname.to_path_buf()
    } else {
        find_decompress_input(pathname)
    };

    // Check hard links
    if !reading_stdin && !check_hard_links(&input_path, args.force)? {
        return Ok(1);
    }

    // Save metadata
    let orig_metadata = if !reading_stdin {
        Some(FileMetadata::from_path(&input_path)?)
    } else {
        None
    };

    // Read compressed data
    let mut file = input_stream(&input_path, true)?;
    let mut compressed_data = Vec::new();
    file.read_to_end(&mut compressed_data)?;

    // Decompress with auto-detection
    let decompressed = decompress_auto(&compressed_data)?;
    let decompressed_size = decompressed.len();

    if writing_to_stdout {
        io::stdout().write_all(&decompressed)?;
        if args.verbose && !reading_stdin {
            eprintln!("{}: -- decompressed", input_path.display());
        }
        return Ok(0);
    }

    // File output mode
    let output_path = decompress_output_path(&input_path);

    // Check for existing output
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

    // Write decompressed file
    let mut f = File::create(&output_path)?;
    f.write_all(&decompressed)?;
    drop(f);

    // Apply metadata
    if let Some(ref meta) = orig_metadata {
        let _ = meta.apply_to(&output_path);
    }

    // Remove compressed file
    fs::remove_file(&input_path)?;

    if args.verbose {
        eprintln!(
            "{}: -- replaced with {} ({} bytes)",
            input_path.display(),
            output_path.display(),
            decompressed_size
        );
    }

    Ok(0)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let program_mode = ProgramMode::detect();
    let mut args = Args::parse();

    // Apply program mode defaults
    match program_mode {
        ProgramMode::Zcat => {
            args.stdout = true;
            args.decompress = true;
        }
        ProgramMode::Uncompress => {
            args.decompress = true;
        }
        ProgramMode::Compress => {}
    }

    // If no files specified, read from stdin
    if args.files.is_empty() {
        args.files.push(PathBuf::from("-"));
    }

    // Determine algorithm for compression
    let algo = if !args.decompress {
        get_compress_algorithm(&args)?
    } else {
        Algorithm::Lzw // not used for decompression (auto-detect)
    };

    let mut exit_code = 0;

    for filename in &args.files {
        let result = if args.decompress {
            decompress_file(&args, filename)
        } else {
            compress_file(&args, filename, algo)
        };

        match result {
            Ok(code) => {
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
