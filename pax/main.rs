//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod archive;
mod blocked_io;
mod compression;
mod error;
mod formats;
mod interactive;
mod modes;
mod multivolume;
mod options;
mod pattern;
mod subst;

use archive::{ArchiveFormat, ArchiveWriter};
use blocked_io::{parse_blocksize, BlockedReader, BlockedWriter, DEFAULT_RECORD_SIZE};
use clap::{Parser, ValueEnum};
use compression::{is_gzip, GzipReader, GzipWriter};
use error::{PaxError, PaxResult};
use gettextrs::gettext;
use modes::copy::CopyOptions;
use modes::list::ListOptions;
use modes::read::ReadOptions;
use modes::write::WriteOptions;
use multivolume::{MultiVolumeOptions, MultiVolumeReader};
use options::FormatOptions;
use pattern::Pattern;
use std::fs::File;
use std::io::{self, Read};
use std::path::PathBuf;
use std::process::ExitCode;
use subst::Substitution;

/// Archive formats supported by pax
#[derive(ValueEnum, Clone, Debug, Copy)]
enum Format {
    Cpio,
    Pax,
    Ustar,
}

impl From<Format> for ArchiveFormat {
    fn from(f: Format) -> Self {
        match f {
            Format::Cpio => ArchiveFormat::Cpio,
            Format::Pax => ArchiveFormat::Pax,
            Format::Ustar => ArchiveFormat::Ustar,
        }
    }
}

/// pax - portable archive interchange
#[derive(Parser, Debug)]
#[command(author, version, about = gettext("pax - portable archive interchange"), long_about)]
struct Args {
    #[arg(short, long = "read", help = gettext("Read an archive file from standard input"))]
    read_mode: bool,

    #[arg(short, long = "write", help = gettext("Write files to the standard output in the specified archive format"))]
    write_mode: bool,

    #[arg(short = 'a', long, help = gettext("Append files to the end of an existing archive"))]
    append: bool,

    #[arg(short, long, help = gettext("Block the output at a positive decimal integer number of bytes per write"))]
    blocksize: Option<u32>,

    #[arg(short = 'c', long, help = gettext("Match all file or archive members except those specified by the pattern or file operands"))]
    exclude: bool,

    #[arg(short, long, help = gettext("Cause files of type directory to match only the file or archive member itself"))]
    dir_no_follow: bool,

    #[arg(short = 'f', long, help = gettext("Specify the pathname of the input or output archive"))]
    archive: Option<PathBuf>,

    #[arg(short = 'H', help = gettext("Follow symlinks on the command line, rather than archiving the symlink itself"))]
    cli_dereference: bool,

    #[arg(short = 'i', long, help = gettext("Interactively rename files or archive members"))]
    interactive: bool,

    #[arg(short = 'k', long, help = gettext("Do not overwrite existing files"))]
    no_clobber: bool,

    #[arg(short, long, help = gettext("In copy mode, hard links shall be made between the source and destination"))]
    link: bool,

    #[arg(short = 'L', long, help = gettext("Follow symlinks"))]
    dereference: bool,

    #[arg(short = 'n', long, help = gettext("Select only the first archive member that matches each pattern operand"))]
    first_match: bool,

    #[arg(short = 'z', long = "gzip", help = gettext("Compress/decompress archive using gzip"))]
    gzip: bool,

    #[arg(short = 'o', long = "options", action = clap::ArgAction::Append, help = gettext("Format-specific options"))]
    format_options: Vec<String>,

    #[arg(short = 's', action = clap::ArgAction::Append, help = gettext("Modify file/archive member names using substitution expression"))]
    substitutions: Vec<String>,

    #[arg(short = 't', long, help = gettext("Reset access times of files after reading them"))]
    reset_atime: bool,

    #[arg(short = 'u', long, help = gettext("Ignore files older than existing files/archive members with same name"))]
    update: bool,

    #[arg(short, long, help = gettext("Specify one or more file characteristic options (privileges)"))]
    privs: Option<String>,

    #[arg(short, long, help = gettext("In list mode, produce a verbose table of contents"))]
    verbose: bool,

    #[arg(short = 'x', long, value_enum, default_value_t = Format::Ustar, help = gettext("Specify the output archive format"))]
    format: Format,

    #[arg(short = 'X', long, help = gettext("Do not cross filesystem boundaries"))]
    one_file_system: bool,

    #[arg(short = 'M', long, help = gettext("Create/read multi-volume archives"))]
    multi_volume: bool,

    #[arg(long, help = gettext("Specify the tape/volume length in bytes (used with -M)"))]
    tape_length: Option<u64>,

    #[arg(long, help = gettext("Run this script at end of each volume (for -M mode)"))]
    new_volume_script: Option<String>,

    #[arg(help = gettext("Pathnames, patterns and file operands to be processed"))]
    files_and_patterns: Vec<String>,
}

/// Operation mode
#[derive(Debug, Clone, Copy)]
enum PaxMode {
    List,
    Read,
    Write,
    Append,
    Copy,
}

fn main() -> ExitCode {
    let args = Args::parse();

    match run(args) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("pax: {}", e);
            ExitCode::FAILURE
        }
    }
}

fn run(args: Args) -> PaxResult<()> {
    // Validate mutually exclusive options
    if args.gzip && args.append {
        return Err(PaxError::InvalidFormat(
            "gzip compression (-z) is incompatible with append mode (-a)".to_string(),
        ));
    }

    let mode = determine_mode(&args);

    match mode {
        PaxMode::List => run_list(&args),
        PaxMode::Read => run_read(&args),
        PaxMode::Write => run_write(&args),
        PaxMode::Append => run_append(&args),
        PaxMode::Copy => run_copy(&args),
    }
}

/// Determine operation mode from arguments
fn determine_mode(args: &Args) -> PaxMode {
    if args.read_mode && args.write_mode {
        PaxMode::Copy
    } else if args.read_mode {
        PaxMode::Read
    } else if args.write_mode && args.append {
        PaxMode::Append
    } else if args.write_mode {
        PaxMode::Write
    } else {
        PaxMode::List
    }
}

/// Parse all -o format options from arguments
fn parse_format_options(args: &Args) -> PaxResult<FormatOptions> {
    let mut opts = FormatOptions::new();
    for opt_str in &args.format_options {
        opts.parse_into(opt_str)?;
    }
    Ok(opts)
}

/// Parse all -s substitution expressions from arguments
fn parse_substitutions(args: &Args) -> PaxResult<Vec<Substitution>> {
    args.substitutions
        .iter()
        .map(|s| Substitution::parse(s))
        .collect()
}

/// Run list mode
fn run_list(args: &Args) -> PaxResult<()> {
    let patterns = compile_patterns(&args.files_and_patterns)?;
    let format_options = parse_format_options(args)?;
    let substitutions = parse_substitutions(args)?;

    let options = ListOptions {
        verbose: args.verbose,
        patterns,
        exclude: args.exclude,
        format_options,
        substitutions,
        first_match: args.first_match,
    };

    // Check for multi-volume mode
    if args.multi_volume {
        return run_list_multi_volume(args, &options);
    }

    let (reader, format) = open_archive_for_read(args)?;
    let mut stdout = io::stdout().lock();

    modes::list_archive(reader, &mut stdout, format, &options)
}

/// Run list mode with multi-volume support
fn run_list_multi_volume(args: &Args, options: &ListOptions) -> PaxResult<()> {
    let archive_path = args.archive.as_ref().ok_or_else(|| {
        PaxError::InvalidFormat("multi-volume mode requires -f archive".to_string())
    })?;

    let mv_options = MultiVolumeOptions {
        volume_size: None, // Not needed for reading
        volume_script: args.new_volume_script.clone(),
        archive_path: archive_path.clone(),
        verbose: args.verbose,
    };

    let mut reader = MultiVolumeReader::new(mv_options)?;
    let mut stdout = io::stdout().lock();

    // Multi-volume is always ustar format
    modes::list::list_archive_from_reader(&mut reader, &mut stdout, options)
}

/// Run read/extract mode
fn run_read(args: &Args) -> PaxResult<()> {
    let patterns = compile_patterns(&args.files_and_patterns)?;
    let substitutions = parse_substitutions(args)?;

    let options = ReadOptions {
        patterns,
        exclude: args.exclude,
        no_clobber: args.no_clobber,
        verbose: args.verbose,
        preserve_perms: should_preserve_perms(&args.privs),
        preserve_mtime: should_preserve_mtime(&args.privs),
        preserve_atime: should_preserve_atime(&args.privs),
        preserve_owner: should_preserve_owner(&args.privs),
        interactive: args.interactive,
        update: args.update,
        substitutions,
        first_match: args.first_match,
    };

    // Check for multi-volume mode
    if args.multi_volume {
        return run_read_multi_volume(args, &options);
    }

    let (reader, format) = open_archive_for_read(args)?;
    modes::extract_archive(reader, format, &options)
}

/// Run read/extract mode with multi-volume support
fn run_read_multi_volume(args: &Args, options: &ReadOptions) -> PaxResult<()> {
    let archive_path = args.archive.as_ref().ok_or_else(|| {
        PaxError::InvalidFormat("multi-volume mode requires -f archive".to_string())
    })?;

    let mv_options = MultiVolumeOptions {
        volume_size: None, // Not needed for reading
        volume_script: args.new_volume_script.clone(),
        archive_path: archive_path.clone(),
        verbose: args.verbose,
    };

    let mut reader = MultiVolumeReader::new(mv_options)?;

    // Multi-volume is always ustar format
    modes::read::extract_archive_from_reader(&mut reader, options)
}

/// Run write/create mode
fn run_write(args: &Args) -> PaxResult<()> {
    let files = get_files_to_archive(args)?;
    let substitutions = parse_substitutions(args)?;
    let format_options = parse_format_options(args)?;

    let options = WriteOptions {
        cli_dereference: args.cli_dereference,
        dereference: args.dereference,
        no_recurse: args.dir_no_follow,
        verbose: args.verbose,
        one_file_system: args.one_file_system,
        interactive: args.interactive,
        reset_atime: args.reset_atime,
        substitutions,
        format_options,
    };

    let format = ArchiveFormat::from(args.format);

    // Check for multi-volume mode
    if args.multi_volume {
        return run_write_multi_volume(args, &files, format, &options);
    }

    // Determine record size for blocked I/O
    let record_size = args
        .blocksize
        .map(parse_blocksize)
        .unwrap_or(DEFAULT_RECORD_SIZE);

    if let Some(ref path) = args.archive {
        let file = File::create(path)?;
        if args.gzip {
            let gzip_writer = GzipWriter::new(file)?;
            let blocked_writer = BlockedWriter::new(gzip_writer, record_size);
            modes::create_archive(blocked_writer, &files, format, &options)
        } else {
            let blocked_writer = BlockedWriter::new(file, record_size);
            modes::create_archive(blocked_writer, &files, format, &options)
        }
    } else {
        let stdout = io::stdout().lock();
        if args.gzip {
            let gzip_writer = GzipWriter::new(stdout)?;
            let blocked_writer = BlockedWriter::new(gzip_writer, record_size);
            modes::create_archive(blocked_writer, &files, format, &options)
        } else {
            let blocked_writer = BlockedWriter::new(stdout, record_size);
            modes::create_archive(blocked_writer, &files, format, &options)
        }
    }
}

/// Run write mode with multi-volume support
fn run_write_multi_volume(
    args: &Args,
    files: &[PathBuf],
    format: ArchiveFormat,
    options: &WriteOptions,
) -> PaxResult<()> {
    // Multi-volume requires an archive file (not stdout)
    let archive_path = args.archive.as_ref().ok_or_else(|| {
        PaxError::InvalidFormat("multi-volume mode requires -f archive".to_string())
    })?;

    // Multi-volume only works with ustar format
    if format == ArchiveFormat::Cpio {
        return Err(PaxError::InvalidFormat(
            "multi-volume is not supported for cpio format".to_string(),
        ));
    }

    // Tape length is required for multi-volume
    let volume_size = args.tape_length.ok_or_else(|| {
        PaxError::InvalidFormat(
            "multi-volume mode requires --tape-length to specify volume size".to_string(),
        )
    })?;

    let mv_options = MultiVolumeOptions {
        volume_size: Some(volume_size),
        volume_script: args.new_volume_script.clone(),
        archive_path: archive_path.clone(),
        verbose: args.verbose,
    };

    let mut writer = multivolume::MultiVolumeWriter::new(mv_options)?;

    // Write each file to the multi-volume archive
    modes::write::write_files_to_archive(&mut writer, files, format, options)?;

    writer.finish()
}

/// Run append mode (-w -a)
fn run_append(args: &Args) -> PaxResult<()> {
    // Append mode requires an archive file (not stdin/stdout)
    let archive_path = args
        .archive
        .as_ref()
        .ok_or_else(|| PaxError::InvalidFormat("append mode requires -f archive".to_string()))?;

    // Check if archive exists - if not, create it instead of appending
    if !archive_path.exists() {
        // Fall back to create mode
        return run_write(args);
    }

    let files = get_files_to_archive(args)?;
    let substitutions = parse_substitutions(args)?;
    let format_options = parse_format_options(args)?;

    let options = WriteOptions {
        cli_dereference: args.cli_dereference,
        dereference: args.dereference,
        no_recurse: args.dir_no_follow,
        verbose: args.verbose,
        one_file_system: args.one_file_system,
        interactive: args.interactive,
        reset_atime: args.reset_atime,
        substitutions,
        format_options,
    };

    modes::append_to_archive(archive_path, &files, &options)
}

/// Run copy mode (-r -w)
fn run_copy(args: &Args) -> PaxResult<()> {
    // In copy mode, the last argument is the destination directory
    // All other arguments are files/directories to copy
    if args.files_and_patterns.is_empty() {
        return Err(PaxError::InvalidFormat(
            "copy mode requires a destination directory".to_string(),
        ));
    }

    let (files, dest_dir) = if args.files_and_patterns.len() == 1 {
        // Only destination provided, read file list from stdin
        let files = modes::copy::read_file_list(io::stdin())?;
        let dest = PathBuf::from(&args.files_and_patterns[0]);
        (files, dest)
    } else {
        // Last arg is destination, rest are files
        let dest = PathBuf::from(args.files_and_patterns.last().unwrap());
        let files: Vec<PathBuf> = args.files_and_patterns[..args.files_and_patterns.len() - 1]
            .iter()
            .map(PathBuf::from)
            .collect();
        (files, dest)
    };

    let patterns = compile_patterns(&[])?; // No patterns in copy mode for file selection
    let substitutions = parse_substitutions(args)?;

    let options = CopyOptions {
        patterns,
        exclude: args.exclude,
        no_clobber: args.no_clobber,
        verbose: args.verbose,
        preserve_perms: should_preserve_perms(&args.privs),
        preserve_mtime: should_preserve_mtime(&args.privs),
        link: args.link,
        cli_dereference: args.cli_dereference,
        dereference: args.dereference,
        no_recurse: args.dir_no_follow,
        one_file_system: args.one_file_system,
        interactive: args.interactive,
        update: args.update,
        substitutions,
    };

    modes::copy_files(&files, &dest_dir, &options)
}

/// Open archive for reading with format detection
fn open_archive_for_read(args: &Args) -> PaxResult<(Box<dyn Read>, ArchiveFormat)> {
    // Determine record size for blocked I/O
    let record_size = args
        .blocksize
        .map(parse_blocksize)
        .unwrap_or(DEFAULT_RECORD_SIZE);

    // Create the underlying reader
    let raw_reader: Box<dyn Read> = if let Some(ref path) = args.archive {
        Box::new(File::open(path)?)
    } else {
        Box::new(io::stdin())
    };

    // First, peek to detect if this is a gzip archive
    let mut peek_reader = PeekReader::new(raw_reader, 512);
    let peek_buf = peek_reader.peek()?;
    let is_gzip_archive = is_gzip(peek_buf);

    // If gzip detected or -z flag set, wrap in gzip decompressor
    let reader: Box<dyn Read> = if is_gzip_archive || args.gzip {
        Box::new(GzipReader::new(peek_reader)?)
    } else {
        Box::new(peek_reader)
    };

    // Wrap in blocked reader for proper tape drive support
    let blocked_reader = BlockedReader::new(reader, record_size);

    // For format detection, we need to peek at the (decompressed) archive
    let mut buf_reader = PeekReader::new(Box::new(blocked_reader), 512);
    let peek_buf = buf_reader.peek()?;

    let format = detect_format_from_bytes(peek_buf)?;

    Ok((Box::new(buf_reader), format))
}

/// Detect format from peek buffer
fn detect_format_from_bytes(buf: &[u8]) -> PaxResult<ArchiveFormat> {
    // Check for ustar magic at offset 257
    if buf.len() >= 263 && &buf[257..262] == b"ustar" {
        // Check typeflag at offset 156 for pax extended headers
        // 'x' (0x78) = per-file extended header
        // 'g' (0x67) = global extended header
        let typeflag = buf[156];
        if typeflag == b'x' || typeflag == b'g' {
            return Ok(ArchiveFormat::Pax);
        }
        return Ok(ArchiveFormat::Ustar);
    }

    // Check for cpio magic at offset 0
    // ASCII formats (6 bytes):
    //   070707 = POSIX octet-oriented (odc)
    //   070701 = SVR4 newc (no CRC)
    //   070702 = SVR4 newc with CRC
    // Binary format (2 bytes):
    //   0x71C7 = old binary cpio (little-endian)
    //   0xC771 = old binary cpio (big-endian)
    if buf.len() >= 6 {
        let magic = &buf[0..6];
        if magic == b"070707" || magic == b"070701" || magic == b"070702" {
            return Ok(ArchiveFormat::Cpio);
        }
    }
    if buf.len() >= 2 {
        // Binary cpio magic: octal 070707 = 0x71C7 (little-endian) or 0xC771 (big-endian)
        let magic16 = u16::from_le_bytes([buf[0], buf[1]]);
        if magic16 == 0o070707 {
            return Ok(ArchiveFormat::Cpio);
        }
        let magic16_be = u16::from_be_bytes([buf[0], buf[1]]);
        if magic16_be == 0o070707 {
            return Ok(ArchiveFormat::Cpio);
        }
    }

    // Check for old-style tar by validating checksum
    if buf.len() >= 512 && is_valid_tar_checksum(buf) {
        // Also check for pax extended headers in old-style tar
        let typeflag = buf[156];
        if typeflag == b'x' || typeflag == b'g' {
            return Ok(ArchiveFormat::Pax);
        }
        return Ok(ArchiveFormat::Ustar);
    }

    Err(PaxError::InvalidFormat(
        "unable to detect archive format".to_string(),
    ))
}

/// Verify tar checksum
fn is_valid_tar_checksum(buf: &[u8]) -> bool {
    if buf.len() < 512 {
        return false;
    }

    // Parse checksum field at offset 148
    let chksum_str = std::str::from_utf8(&buf[148..156]).unwrap_or("");
    let chksum_str = chksum_str.trim_matches(|c| c == ' ' || c == '\0');
    if chksum_str.is_empty() {
        return false;
    }

    // Reject if checksum contains a sign
    if chksum_str.starts_with('+') || chksum_str.starts_with('-') {
        return false;
    }

    let stored = match u32::from_str_radix(chksum_str, 8) {
        Ok(v) => v,
        Err(_) => return false,
    };

    // Calculate checksum
    let mut sum: u32 = 0;
    for (i, &byte) in buf[0..512].iter().enumerate() {
        if (148..156).contains(&i) {
            sum += b' ' as u32;
        } else {
            sum += byte as u32;
        }
    }

    sum == stored
}

/// Compile pattern strings into Pattern objects
fn compile_patterns(patterns: &[String]) -> PaxResult<Vec<Pattern>> {
    patterns.iter().map(|s| Pattern::new(s)).collect()
}

/// Get files to archive (from args or stdin)
fn get_files_to_archive(args: &Args) -> PaxResult<Vec<PathBuf>> {
    if args.files_and_patterns.is_empty() {
        // Read from stdin
        modes::write::read_file_list(io::stdin())
    } else {
        Ok(args.files_and_patterns.iter().map(PathBuf::from).collect())
    }
}

/// Parse -p privilege string and return preservation flags
/// Per POSIX: when conflicting characters appear, the last one wins.
/// Defaults: preserve atime, mtime, perms; do NOT preserve owner
fn parse_privs(privs: &Option<String>) -> (bool, bool, bool, bool) {
    // Defaults per POSIX:
    // - atime: preserved (so 'a' disables it)
    // - mtime: preserved (so 'm' disables it)
    // - perms: preserved (so absence of 'p' or 'e' disables it when -p is used)
    // - owner: NOT preserved (so 'o' or 'e' enables it)
    let mut preserve_atime = true;
    let mut preserve_mtime = true;
    let mut preserve_perms = true;
    let mut preserve_owner = false;

    if let Some(s) = privs {
        // Process each character in order, last one wins for conflicts
        for c in s.chars() {
            match c {
                'a' => preserve_atime = false,
                'm' => preserve_mtime = false,
                'o' => preserve_owner = true,
                'p' => preserve_perms = true,
                'e' => {
                    // 'e' means preserve everything
                    preserve_atime = true;
                    preserve_mtime = true;
                    preserve_perms = true;
                    preserve_owner = true;
                }
                _ => {} // Ignore unknown characters per POSIX
            }
        }

        // Per POSIX: if -p is specified but doesn't contain 'p' or 'e',
        // permissions are still preserved by default. The only way to
        // not preserve perms is to not specify -p at all (which we can't
        // detect here) or implementation-specific. We keep default behavior.
    }

    (
        preserve_atime,
        preserve_mtime,
        preserve_perms,
        preserve_owner,
    )
}

/// Check if permissions should be preserved
fn should_preserve_perms(privs: &Option<String>) -> bool {
    parse_privs(privs).2
}

/// Check if modification time should be preserved
fn should_preserve_mtime(privs: &Option<String>) -> bool {
    parse_privs(privs).1
}

/// Check if access time should be preserved
fn should_preserve_atime(privs: &Option<String>) -> bool {
    parse_privs(privs).0
}

/// Check if owner should be preserved
fn should_preserve_owner(privs: &Option<String>) -> bool {
    parse_privs(privs).3
}

/// Reader that can peek ahead without consuming bytes
struct PeekReader {
    reader: Box<dyn Read>,
    buffer: Vec<u8>,
    pos: usize,
    peek_size: usize,
    peeked: bool,
}

impl PeekReader {
    fn new(reader: Box<dyn Read>, peek_size: usize) -> Self {
        PeekReader {
            reader,
            buffer: Vec::new(),
            pos: 0,
            peek_size,
            peeked: false,
        }
    }

    fn peek(&mut self) -> PaxResult<&[u8]> {
        if !self.peeked {
            self.buffer = vec![0u8; self.peek_size];
            let n = self.reader.read(&mut self.buffer)?;
            self.buffer.truncate(n);
            self.peeked = true;
        }
        Ok(&self.buffer)
    }
}

impl Read for PeekReader {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        // First, drain the peek buffer
        if self.pos < self.buffer.len() {
            let remaining = &self.buffer[self.pos..];
            let to_copy = std::cmp::min(remaining.len(), buf.len());
            buf[..to_copy].copy_from_slice(&remaining[..to_copy]);
            self.pos += to_copy;
            return Ok(to_copy);
        }

        // Then read from underlying reader
        self.reader.read(buf)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_determine_mode() {
        // Default is list
        let args = Args::parse_from(["pax"]);
        assert!(matches!(determine_mode(&args), PaxMode::List));
    }

    #[test]
    fn test_preserve_flags() {
        // Default (no -p): preserve atime, mtime, perms; don't preserve owner
        assert!(should_preserve_atime(&None));
        assert!(should_preserve_mtime(&None));
        assert!(should_preserve_perms(&None));
        assert!(!should_preserve_owner(&None));

        // Individual flags
        assert!(!should_preserve_atime(&Some("a".to_string())));
        assert!(!should_preserve_mtime(&Some("m".to_string())));
        assert!(should_preserve_perms(&Some("p".to_string())));
        assert!(should_preserve_owner(&Some("o".to_string())));

        // 'e' preserves everything
        assert!(should_preserve_atime(&Some("e".to_string())));
        assert!(should_preserve_mtime(&Some("e".to_string())));
        assert!(should_preserve_perms(&Some("e".to_string())));
        assert!(should_preserve_owner(&Some("e".to_string())));

        // Combined flags
        assert!(!should_preserve_atime(&Some("am".to_string())));
        assert!(!should_preserve_mtime(&Some("am".to_string())));
        assert!(should_preserve_perms(&Some("am".to_string()))); // perms still default to true

        // Precedence: last wins
        // 'e' enables everything, then 'a' disables atime
        assert!(!should_preserve_atime(&Some("ea".to_string())));
        assert!(should_preserve_mtime(&Some("ea".to_string())));
        assert!(should_preserve_owner(&Some("ea".to_string())));

        // 'm' disables mtime, then 'e' enables everything
        assert!(should_preserve_mtime(&Some("me".to_string())));
    }
}
