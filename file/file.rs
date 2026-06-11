//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod magic;

use std::fs::{read_link, File};
use std::io::{Read, Seek};
use std::os::fd::FromRawFd;
use std::os::unix::fs::FileTypeExt;
use std::path::PathBuf;
use std::{fs, io};

use clap::{CommandFactory, FromArgMatches, Parser};
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};

use crate::magic::{get_type_from_magic_file_dbs, ReadSeek, DEFAULT_MAGIC_FILE};

/// Number of leading bytes inspected by the context-sensitive content tests.
const CONTENT_PREFIX_LEN: usize = 8192;

/// Upper bound on how much of a non-seekable stdin (a pipe/FIFO/tty) is spilled
/// to a temporary file for the position-sensitive magic tests. A pipe cannot be
/// rewound, so it must be captured to disk to allow seeking; bounding the
/// capture keeps a huge stream from filling the temp filesystem. Magic entries
/// reference small header offsets, so this window is comfortably sufficient
/// (this mirrors how file(1)/libmagic classifies a pipe from a fixed prefix).
const STDIN_SPILL_LIMIT: u64 = 1 << 20; // 1 MiB

#[derive(Parser)]
#[command(
    version,
    disable_help_flag = true,
    about = gettext("file - determine file type")
)]
struct Args {
    #[arg(long, action = clap::ArgAction::HelpLong)]
    help: Option<bool>,

    #[arg(
        short = 'd',
        long,
        help = gettext(
            "Apply default position-sensitive system tests and context-sensitive system tests to the file"
        )
    )]
    default_tests: bool,

    #[arg(
        short = 'h',
        long,
        help = gettext("Identify symbolic link with non existent file as symbolic link")
    )]
    identify_as_symbolic_link: bool,

    #[arg(
        short = 'i',
        long,
        help = gettext("Don't perform further classification on regular file")
    )]
    no_further_file_classification: bool,

    #[arg(
        short = 'm',
        help = gettext("File containing position-sensitive tests")
    )]
    test_file1: Option<PathBuf>,

    #[arg(
        short = 'M',
        help = gettext("File containing additional position-sensitive tests")
    )]
    test_file2: Option<PathBuf>,

    files: Vec<String>,
}

fn get_magic_files(args: &Args, matches: &clap::ArgMatches) -> Vec<PathBuf> {
    if args.no_further_file_classification {
        return Vec::new();
    }

    // Collect (index, path) for each present flag, then sort by CLI position
    let mut indexed_files: Vec<(usize, PathBuf)> = Vec::new();

    if let Some(ref path) = args.test_file2 {
        if let Some(idx) = matches.index_of("test_file2") {
            indexed_files.push((idx, path.clone()));
        }
    }
    if let Some(ref path) = args.test_file1 {
        if let Some(idx) = matches.index_of("test_file1") {
            indexed_files.push((idx, path.clone()));
        }
    }
    if args.default_tests {
        if let Some(idx) = matches.index_of("default_tests") {
            indexed_files.push((idx, PathBuf::from(DEFAULT_MAGIC_FILE)));
        }
    }

    indexed_files.sort_by_key(|(idx, _)| *idx);

    let mut magic_files: Vec<PathBuf> = indexed_files.into_iter().map(|(_, p)| p).collect();

    // Per POSIX: if only -m (no -M, no -d), append default magic file after
    if args.test_file1.is_some() && args.test_file2.is_none() && !args.default_tests {
        magic_files.push(PathBuf::from(DEFAULT_MAGIC_FILE));
    }

    // If nothing specified at all, use default magic file
    if magic_files.is_empty() {
        magic_files.push(PathBuf::from(DEFAULT_MAGIC_FILE));
    }

    magic_files
}

/// True if the default system tests are in effect (i.e. the default magic file
/// is part of the database list). The context-sensitive content tests are a
/// default system test and must not run when only `-M`/`-m` databases apply.
fn default_tests_active(magic_files: &[PathBuf]) -> bool {
    magic_files
        .iter()
        .any(|p| p.as_path() == std::path::Path::new(DEFAULT_MAGIC_FILE))
}

/// Conservative context-sensitive content tests (POSIX default system tests):
/// recognize shell scripts, C source, and FORTRAN source from a content prefix.
/// Returns `None` for binary content or anything not recognized.
fn content_type(prefix: &[u8]) -> Option<String> {
    // Treat content containing a NUL byte as binary, not source text.
    if prefix.is_empty() || prefix.contains(&0) {
        return None;
    }
    let text = String::from_utf8_lossy(prefix);

    if text.starts_with("#!") {
        return Some(gettext("commands text"));
    }
    if text.contains("#include") || text.contains("#define ") || text.contains("int main") {
        return Some(gettext("c program text"));
    }
    if looks_like_fortran(&text) {
        return Some(gettext("fortran program text"));
    }
    None
}

/// Very conservative FORTRAN heuristic: a line beginning with a distinctive
/// FORTRAN keyword. Kept narrow to avoid misclassifying ordinary prose.
fn looks_like_fortran(text: &str) -> bool {
    text.lines().any(|line| {
        let l = line.trim_start().to_ascii_lowercase();
        l.starts_with("program ")
            || l.starts_with("subroutine ")
            || l.starts_with("implicit none")
            || l.starts_with("end program")
    })
}

/// Classify regular-file content. `is_empty` short-circuits to "empty";
/// otherwise position-sensitive magic tests run first (via `make_reader`), then
/// — only when the default tests are active — the context-sensitive content
/// tests on `prefix`; failing both yields "data".
fn classify_content<F>(
    is_empty: bool,
    prefix: &[u8],
    default_active: bool,
    magic_files: &[PathBuf],
    make_reader: F,
) -> String
where
    F: Fn() -> io::Result<Box<dyn ReadSeek>>,
{
    if is_empty {
        return gettext("empty");
    }
    if let Some(f_type) = get_type_from_magic_file_dbs(make_reader, magic_files) {
        return f_type;
    }
    if default_active {
        if let Some(t) = content_type(prefix) {
            return t;
        }
    }
    gettext("data")
}

/// Classify a non-symlink file given its (followed) metadata, returning the
/// `<type>` string for the `"%s: %s"` output. `met` describes the file whose
/// contents `path` opens (the symlink target, when following a link).
fn classify(path: &str, met: &fs::Metadata, args: &Args, magic_files: &[PathBuf]) -> String {
    let file_type = met.file_type();

    if file_type.is_char_device() {
        return gettext("character special");
    }
    if file_type.is_dir() {
        return gettext("directory");
    }
    if file_type.is_fifo() {
        return gettext("fifo");
    }
    if file_type.is_socket() {
        return gettext("socket");
    }
    if file_type.is_block_device() {
        return gettext("block special");
    }
    if file_type.is_file() {
        if args.no_further_file_classification {
            return gettext("regular file");
        }
        let mut prefix = Vec::new();
        match File::open(path) {
            Ok(f) => {
                let _ = f.take(CONTENT_PREFIX_LEN as u64).read_to_end(&mut prefix);
            }
            // Consistent with analyze_file()'s handling of other open failures:
            // an unreadable regular file is reported as "cannot open" rather
            // than being silently classified from empty content.
            Err(_) => return gettext("cannot open"),
        }
        let owned = path.to_string();
        return classify_content(
            met.len() == 0,
            &prefix,
            default_tests_active(magic_files),
            magic_files,
            move || Ok(Box::new(File::open(&owned)?) as Box<dyn ReadSeek>),
        );
    }
    // Any other (unknown) type.
    gettext("data")
}

fn analyze_file(path: &str, args: &Args, magic_files: &[PathBuf]) {
    // A '-' operand classifies the content of standard input.
    if path == "-" {
        analyze_stdin(args, magic_files);
        return;
    }

    let lmet = match fs::symlink_metadata(path) {
        Ok(met) => met,
        Err(_) => {
            // Per spec this is reported but does not affect the exit status.
            println!("{path}: {}", gettext("cannot open"));
            return;
        }
    };

    if lmet.file_type().is_symlink() {
        // `metadata` follows the link; Ok means the target exists.
        let target_meta = fs::metadata(path);
        let target = read_link(path).ok();

        // Identify the link itself when -h is given, or by default when the
        // link is broken (POSIX: a dangling link is treated as if -h).
        if args.identify_as_symbolic_link || target_meta.is_err() {
            match (&target, target_meta.is_ok()) {
                (Some(t), true) => {
                    println!("{path}: {} {}", gettext("symbolic link to"), t.display())
                }
                (Some(t), false) => println!(
                    "{path}: {} {}",
                    gettext("broken symbolic link to"),
                    t.display()
                ),
                (None, _) => println!("{path}: {}", gettext("symbolic link")),
            }
            return;
        }

        // Default: resolve the link and classify the referenced file's type.
        let tmet = target_meta.unwrap();
        println!("{path}: {}", classify(path, &tmet, args, magic_files));
        return;
    }

    println!("{path}: {}", classify(path, &lmet, args, magic_files));
}

/// Obtain a seekable handle to standard input without buffering it in memory.
///
/// Position-sensitive magic tests need random access, but stdin is not always
/// seekable. When fd 0 is already seekable (e.g. `file - < file`), it is
/// duplicated and used directly — zero extra memory and full random access,
/// regardless of size. Otherwise (a pipe, FIFO, or terminal) the leading
/// `STDIN_SPILL_LIMIT` bytes are spilled once to an anonymous temporary file;
/// the rest of the stream is left unread so an arbitrarily large pipe neither
/// fills the temp filesystem nor is drained in full.
fn seekable_stdin() -> io::Result<File> {
    // Duplicate fd 0 so we own a handle without closing the process's stdin.
    let dup = unsafe { libc::dup(libc::STDIN_FILENO) };
    if dup < 0 {
        return Err(io::Error::last_os_error());
    }
    let mut f = unsafe { File::from_raw_fd(dup) };

    // A regular-file redirect is seekable; a pipe/FIFO/tty yields ESPIPE.
    if f.stream_position().is_ok() {
        f.rewind()?;
        return Ok(f);
    }

    // Non-seekable input: spill a bounded prefix to a temp file. io::copy uses
    // a small internal buffer, so memory stays O(1); take() caps the on-disk
    // capture so a 1 TiB pipe does not write 1 TiB to the temp directory.
    let mut tmp = tempfile::tempfile()?;
    io::copy(&mut f.take(STDIN_SPILL_LIMIT), &mut tmp)?;
    tmp.rewind()?;
    Ok(tmp)
}

/// Classify the content of standard input (the `-` operand).
fn analyze_stdin(args: &Args, magic_files: &[PathBuf]) {
    let type_str = if args.no_further_file_classification {
        gettext("regular file")
    } else {
        match seekable_stdin() {
            Ok(file) => {
                // Read the content prefix for the context-sensitive tests, then
                // hand each magic database a fresh handle rewound to the start.
                let mut prefix = Vec::new();
                let _ = (&file)
                    .take(CONTENT_PREFIX_LEN as u64)
                    .read_to_end(&mut prefix);
                classify_content(
                    prefix.is_empty(),
                    &prefix,
                    default_tests_active(magic_files),
                    magic_files,
                    move || {
                        let mut f = file.try_clone()?;
                        f.rewind()?;
                        Ok(Box::new(f) as Box<dyn ReadSeek>)
                    },
                )
            }
            Err(e) => {
                eprintln!("file: {e}");
                gettext("cannot open")
            }
        }
    };
    println!("/dev/stdin: {type_str}");
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let matches = Args::command().get_matches();
    let args = Args::from_arg_matches(&matches)?;

    let magic_files = get_magic_files(&args, &matches);

    // A magic file named explicitly via -m/-M that cannot be opened is a
    // genuine error and sets a non-zero exit status. (A missing or unreadable
    // operand file is NOT an error per the spec.)
    let mut had_error = false;
    for mf in [&args.test_file1, &args.test_file2].into_iter().flatten() {
        // Probe with File::open (matching how the magic parser reads the file)
        // so an unreadable magic file is detected; fs::metadata would succeed.
        if File::open(mf).is_err() {
            eprintln!(
                "file: {}: {}",
                mf.display(),
                gettext("cannot open magic file")
            );
            had_error = true;
        }
    }

    for file in &args.files {
        analyze_file(file, &args, &magic_files);
    }

    if had_error {
        std::process::exit(1);
    }
    Ok(())
}
