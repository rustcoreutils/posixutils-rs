//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
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
use gettextrs::gettext;
use plib::diag;
use plib::io::input_stream;
use plib::lzw::{UnixLZWReader, UnixLZWWriter};
use std::fs::{self, File};
use std::io::{self, IsTerminal, Read, Write};
use std::path::{Path, PathBuf};

/// Conventional fallback when {NAME_MAX} cannot be queried.
const NAME_MAX_FALLBACK: usize = 255;

/// Query `{NAME_MAX}` for the directory that will hold the output file, via
/// `pathconf(_PC_NAME_MAX)`; fall back to a conventional 255 when unavailable.
#[cfg(unix)]
fn name_max(dir: &Path) -> usize {
    use std::ffi::CString;
    use std::os::unix::ffi::OsStrExt;

    let dir = if dir.as_os_str().is_empty() {
        Path::new(".")
    } else {
        dir
    };
    if let Ok(c) = CString::new(dir.as_os_str().as_bytes()) {
        // SAFETY: c is a valid NUL-terminated C string for the lifetime of the call.
        let v = unsafe { libc::pathconf(c.as_ptr(), libc::_PC_NAME_MAX) };
        if v > 0 {
            return v as usize;
        }
    }
    NAME_MAX_FALLBACK
}

/// `{NAME_MAX}` on Windows, which has no `pathconf`: 255, NTFS's limit on a
/// path component.
#[cfg(windows)]
fn name_max(_dir: &Path) -> usize {
    NAME_MAX_FALLBACK
}

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
    if io::stdin().read_line(&mut response).is_err() {
        return false;
    }
    plib::locale::is_affirmative(response.trim_end_matches(['\r', '\n']))
}

/// Combine per-file exit codes into a single, order-independent status.
///
/// Severity order: 1 (error) outranks 2 (file not compressed because it would
/// grow) outranks 0 (success). Both 1 and 2 are non-zero per spec 90511-90516;
/// this just makes the final code deterministic regardless of file order.
fn merge_exit(current: i32, new: i32) -> i32 {
    match (current, new) {
        (1, _) | (_, 1) => 1,
        (2, _) | (_, 2) => 2,
        _ => 0,
    }
}

/// Decide whether an existing output file may be overwritten.
///
/// Returns `true` to proceed with the write, `false` to skip it (the caller
/// then returns a non-zero exit code). Per POSIX (90427-90432), the overwrite
/// prompt is issued **only** when standard input is a terminal; when stdin is
/// not a terminal and `-f` was not given, a diagnostic is written and the file
/// is not overwritten, with no prompt (so a pipeline's input stream is never
/// consumed by `read_line`).
fn may_overwrite(output_path: &Path, force: bool) -> bool {
    if force || !output_path.exists() {
        return true;
    }
    if io::stdin().is_terminal() {
        let yes = prompt_user(&gettext!(
            "Do you want to overwrite {} (y)es or (n)o?",
            output_path.display()
        ));
        if !yes {
            diag::warning(&format!(
                "{}: {}",
                output_path.display(),
                gettext("not overwritten")
            ));
        }
        yes
    } else {
        diag::error(&format!(
            "{}: {}",
            output_path.display(),
            gettext("already exists; not overwritten (use -f to force)")
        ));
        false
    }
}

/// Saved file metadata for preservation
struct FileMetadata {
    /// The mode on Unix; the read-only attribute on Windows.
    permissions: fs::Permissions,
    #[cfg(unix)]
    owner: (u32, u32),
    times: fs::FileTimes,
}

impl FileMetadata {
    fn from_path(path: &Path) -> io::Result<Self> {
        let meta = fs::metadata(path)?;
        Ok(Self {
            permissions: meta.permissions(),
            #[cfg(unix)]
            owner: {
                use std::os::unix::fs::MetadataExt;
                (meta.uid(), meta.gid())
            },
            times: fs::FileTimes::new()
                .set_accessed(meta.accessed()?)
                .set_modified(meta.modified()?),
        })
    }

    /// Apply the saved metadata to the output at `path`, whose write handle
    /// `file` is still open.
    fn apply_to(&self, file: &File, path: &Path) -> io::Result<()> {
        // Restore ownership before the mode bits: chown() clears the
        // set-user-ID / set-group-ID bits, so it must run first. Best effort —
        // only a sufficiently privileged process succeeds, so the result is
        // intentionally ignored (spec 90389-90392).
        #[cfg(unix)]
        {
            use std::ffi::CString;
            use std::os::unix::ffi::OsStrExt;

            let path_cstr = CString::new(path.as_os_str().as_bytes())?;
            unsafe {
                libc::chown(path_cstr.as_ptr(), self.owner.0, self.owner.1);
            }
        }

        // The times go through the write handle the caller already holds, so
        // neither the umask (which may have created the file without owner
        // write) nor the mode restored below can keep them from being set; a
        // reopen by path would be refused in either case. set_times keeps
        // nanoseconds (futimens on Unix), so a preserved time compares equal
        // to the one it came from. Best effort, like the chown: timestamp
        // preservation is a courtesy on top of an output file that is already
        // complete and correct, so a failure here must not turn into a
        // non-zero exit. Both call sites discard this function's result for
        // that reason.
        let _ = file.set_times(self.times);

        fs::set_permissions(path, self.permissions.clone())
    }
}

/// Check for multiple hard links
fn check_hard_links(path: &Path, force: bool) -> io::Result<bool> {
    let links = link_count(path)?;
    if links > 1 {
        diag::warning(&format!(
            "{}: {}",
            path.display(),
            gettext!("has {} hard links", links)
        ));
        if !force {
            return Ok(false);
        }
    }
    Ok(true)
}

/// The number of hard links to `path`.
#[cfg(unix)]
fn link_count(path: &Path) -> io::Result<u64> {
    use std::os::unix::fs::MetadataExt;
    Ok(fs::metadata(path)?.nlink())
}

/// The number of hard links to `path`. Stable Rust exposes no link count on
/// Windows, so a file there counts as its only link; a stat failure is still
/// reported.
#[cfg(windows)]
fn link_count(path: &Path) -> io::Result<u64> {
    fs::metadata(path)?;
    Ok(1)
}

/// Remove `path`. On Unix, removal is governed by the directory's
/// permissions, not the file's, so a read-only file is removed as is.
#[cfg(unix)]
fn remove_file(path: &Path) -> io::Result<()> {
    fs::remove_file(path)
}

/// Remove `path`, clearing its read-only attribute first. Windows will not
/// delete a read-only file wherever FILE_DISPOSITION_IGNORE_READONLY_ATTRIBUTE
/// is not honoured (Wine, older Windows), and compress copies that attribute
/// onto its output, so both the input removal and the back-out of the output
/// depend on this. A file that still cannot be removed gets its attribute
/// back, so a failure leaves it as it was.
#[cfg(windows)]
fn remove_file(path: &Path) -> io::Result<()> {
    let mut perms = fs::symlink_metadata(path)?.permissions();
    if !perms.readonly() {
        return fs::remove_file(path);
    }
    let original = perms.clone();
    #[expect(
        clippy::permissions_set_readonly_false,
        reason = "Windows only: clears the read-only attribute, no Unix mode bits"
    )]
    perms.set_readonly(false);
    fs::set_permissions(path, perms)?;
    fs::remove_file(path).inspect_err(|_| {
        let _ = fs::set_permissions(path, original);
    })
}

/// Check if output path would exceed PATH_MAX
fn check_path_max(path: &Path) -> io::Result<()> {
    let path_len = path.as_os_str().len();
    #[cfg(unix)]
    {
        if path_len > libc::PATH_MAX as usize {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                gettext("pathname too long"),
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
            gettext("unknown compression format"),
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
            gettext!("unknown algorithm: {}", s),
        )),
    }
}

/// Validate -b value for the given algorithm
fn validate_bits(algo: Algorithm, bits: u32) -> io::Result<()> {
    match algo {
        Algorithm::Lzw => {
            // The LZW writer's default (no -b) emits 16-bit codes, and the
            // RATIONALE (spec 90542-90544) encourages 15/16, so accept the full
            // historical 9-16 range rather than the normative-DESCRIPTION 14.
            if !(9..=16).contains(&bits) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    gettext("LZW bits must be 9-16"),
                ));
            }
        }
        Algorithm::Deflate => {
            if !(1..=9).contains(&bits) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    gettext("DEFLATE level must be 1-9"),
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
        io::Error::new(
            io::ErrorKind::InvalidInput,
            gettext("input path has no filename"),
        )
    })?;
    let fname = format!("{}{}", file_name.to_string_lossy(), algo.suffix());

    let parent = input.parent();
    let dir = parent.unwrap_or_else(|| Path::new(""));
    if fname.len() > name_max(dir) {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            gettext("filename too long"),
        ));
    }

    let output = match parent {
        Some(parent) if !parent.as_os_str().is_empty() => parent.join(&fname),
        _ => PathBuf::from(&fname),
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
    let writing_to_stdout = args.stdout || reading_stdin;

    // Warn if input already has compression suffix
    if !reading_stdin {
        if let Some(ext) = pathname.extension() {
            if ext == "Z" || ext == "gz" {
                diag::warning(&format!(
                    "{}: {}",
                    pathname.display(),
                    gettext!("already has {} suffix", ext.to_str().unwrap())
                ));
            }
        }
    }

    // Check hard links only when the input will actually be unlinked. Under
    // -c (or stdin) the input is never removed, so the multi-link guard
    // (spec 90403-90406, about files "to be removed after processing") does
    // not apply.
    if !writing_to_stdout && !check_hard_links(pathname, args.force)? {
        return Ok(1);
    }

    // Read input
    let mut file = input_stream(pathname, true)?;
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

    if writing_to_stdout {
        io::stdout().write_all(&out_buf)?;
        if args.verbose && !reading_stdin {
            let ratio = if inp_buf_size > 0 {
                100.0 - (out_buf_size as f64 / inp_buf_size as f64) * 100.0
            } else {
                0.0
            };
            eprintln!(
                "{}",
                gettext!("{}: Compression: {:.1}%", pathname.display(), ratio)
            );
        }
        return Ok(0);
    }

    // File replacement mode
    if out_buf_size >= inp_buf_size && !args.force {
        return Ok(2);
    }

    let output_path = compress_output_path(pathname, algo)?;

    // Check for existing file (terminal-gated prompt per #C1)
    if !may_overwrite(&output_path, args.force) {
        return Ok(1);
    }

    // Write compressed file, then apply metadata while the handle is open
    let mut f = File::create(&output_path)?;
    f.write_all(&out_buf)?;
    if let Some(ref meta) = orig_metadata {
        let _ = meta.apply_to(&f, &output_path);
    }
    drop(f);

    // Remove original. If it cannot be removed, back out the output so we do
    // not leave both files behind, and report a non-zero status
    // (spec 90393-90400).
    if let Err(e) = remove_file(pathname) {
        let _ = remove_file(&output_path);
        diag::error(&format!(
            "{}: {}: {}",
            pathname.display(),
            gettext("cannot remove input"),
            e
        ));
        return Ok(1);
    }

    if args.verbose {
        let ratio = if inp_buf_size > 0 {
            100.0 - (out_buf_size as f64 / inp_buf_size as f64) * 100.0
        } else {
            0.0
        };
        eprintln!(
            "{}",
            gettext!(
                "{}: -- replaced with {} Compression: {:.1}%",
                pathname.display(),
                output_path.display(),
                ratio
            )
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

    // Check hard links only when the input will actually be unlinked
    // (not under -c / stdin); see #C2.
    if !writing_to_stdout && !check_hard_links(&input_path, args.force)? {
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
            eprintln!("{}", gettext!("{}: -- decompressed", input_path.display()));
        }
        return Ok(0);
    }

    // File output mode
    let output_path = decompress_output_path(&input_path);

    // Refuse when the input has no known suffix to strip, so the output path
    // equals the input path: writing the decompressed bytes and then removing
    // the input would destroy the result (#C3, data loss).
    if output_path == input_path {
        diag::error(&format!(
            "{}: {}",
            input_path.display(),
            gettext("unknown suffix -- ignored")
        ));
        return Ok(1);
    }

    // Check for existing output (terminal-gated prompt per #C1)
    if !may_overwrite(&output_path, args.force) {
        return Ok(1);
    }

    // Write decompressed file, then apply metadata while the handle is open
    let mut f = File::create(&output_path)?;
    f.write_all(&decompressed)?;
    if let Some(ref meta) = orig_metadata {
        let _ = meta.apply_to(&f, &output_path);
    }
    drop(f);

    // Remove compressed file. If it cannot be removed, back out the output so
    // we do not leave both files behind, and report a non-zero status
    // (spec 90393-90400).
    if let Err(e) = remove_file(&input_path) {
        let _ = remove_file(&output_path);
        diag::error(&format!(
            "{}: {}: {}",
            input_path.display(),
            gettext("cannot remove input"),
            e
        ));
        return Ok(1);
    }

    if args.verbose {
        eprintln!(
            "{}",
            gettext!(
                "{}: -- replaced with {} ({} bytes)",
                input_path.display(),
                output_path.display(),
                decompressed_size
            )
        );
    }

    Ok(0)
}

fn main() {
    diag::init_locale("compress");

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
        match get_compress_algorithm(&args) {
            Ok(a) => a,
            Err(e) => {
                diag::error(&e.to_string());
                std::process::exit(1);
            }
        }
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
            Ok(code) => exit_code = merge_exit(exit_code, code),
            Err(e) => {
                exit_code = merge_exit(exit_code, 1);
                let display_name = if is_stdin(filename) {
                    gettext("standard input")
                } else {
                    filename.display().to_string()
                };
                diag::error(&format!("{}: {}", display_name, diag::io_error_text(&e)));
            }
        }
    }

    std::process::exit(exit_code)
}
