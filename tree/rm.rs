//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod common;

use self::common::error_string;
use clap::Parser;
use ftw::{self, traverse_directory};
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::{
    ffi::CString,
    fs,
    io::{self, IsTerminal},
    os::{
        fd::AsRawFd,
        unix::{ffi::OsStrExt, fs::MetadataExt},
    },
    path::{Path, PathBuf},
};

/// rm - remove directory entries
#[derive(Parser)]
#[command(version, about = gettext("rm - remove directory entries"))]
struct Args {
    #[arg(short, long, overrides_with_all = ["force", "interactive"], help = gettext("Do not prompt for confirmation"))]
    force: bool,

    #[arg(short, long, overrides_with_all = ["force", "interactive"], help = gettext("Prompt for confirmation"))]
    interactive: bool,

    #[arg(short, long, help = gettext("Remove empty directories"))]
    dir: bool,

    #[arg(short, visible_short_alias = 'R', long, help = gettext("Remove file hierarchies"))]
    recurse: bool,

    #[arg(short, long, help = gettext("Write the name of each removed file to standard output"))]
    verbose: bool,

    #[arg(value_parser = parse_pathbuf, help = gettext("Filepaths to remove"))]
    files: Vec<PathBuf>,
}

// Parser for `PathBuf` that allows empty strings
fn parse_pathbuf(s: &str) -> Result<PathBuf, String> {
    Ok(PathBuf::from(s))
}

struct RmConfig {
    args: Args,
    is_tty: bool,
}

fn prompt_user(prompt: &str) -> bool {
    eprint!("rm: {prompt} ");
    let mut response = String::new();
    // A read error or EOF is a non-affirmative response, not a panic.
    if io::stdin().read_line(&mut response).unwrap_or(0) == 0 {
        return false;
    }
    plib::locale::is_affirmative(response.trim_end_matches(['\r', '\n']))
}

// Simplifies trailing slashes
fn display_cleaned(filepath: &Path) -> String {
    let mut s = format!("{}", filepath.display());
    while s.ends_with("//") {
        s.pop();
    }
    s
}

fn ask_for_prompt(cfg: &RmConfig, writable: bool) -> bool {
    !cfg.args.force && ((!writable && cfg.is_tty) || cfg.args.interactive)
}

// With `-v`, write the name of each removed entry to standard output (format unspecified by POSIX;
// matches the `removed '…'` / `removed directory '…'` wording of common implementations).
fn report_removed(cfg: &RmConfig, is_dir: bool, name: &str) {
    if cfg.args.verbose {
        let msg = if is_dir {
            gettext!("removed directory '{}'", name)
        } else {
            gettext!("removed '{}'", name)
        };
        println!("{msg}");
    }
}

// rm shall refuse `.`/`..` (as the basename) and an operand resolving to the root directory
// (POSIX rm DESCRIPTION 113360-113362, APPLICATION USAGE 113466-113468).
fn refuse_dot_dotdot_root(filepath: &Path) -> io::Result<()> {
    let dot_dotdot_pattern = regex::bytes::Regex::new(r"(?:\.\/*|\.\.\/*)$").unwrap();
    if dot_dotdot_pattern.is_match(filepath.as_os_str().as_bytes()) {
        let err_str = gettext!(
            "refusing to remove '.' or '..' directory: skipping '{}'",
            display_cleaned(filepath)
        );
        return Err(io::Error::other(err_str));
    }

    if let Ok(abspath) = fs::canonicalize(filepath) {
        if abspath.as_os_str() == "/" {
            let err_str = if filepath.as_os_str() == "/" {
                gettext("it is dangerous to operate recursively on '/'")
            } else {
                gettext!(
                    "it is dangerous to operate recursively on '{}' (same as '/')",
                    filepath.display()
                )
            };
            return Err(io::Error::other(err_str));
        }
    }

    Ok(())
}

fn descend_into_directory(cfg: &RmConfig, entry: &ftw::Entry, metadata: &ftw::Metadata) -> bool {
    let writable = metadata.is_writable();
    if ask_for_prompt(cfg, writable) {
        let prompt = if writable {
            gettext!(
                "descend into directory '{}'?",
                entry.path().clean_trailing_slashes()
            )
        } else {
            gettext!(
                "descend into write-protected directory '{}'?",
                entry.path().clean_trailing_slashes()
            )
        };
        if !prompt_user(&prompt) {
            return false;
        }
    }
    true
}

fn should_remove_directory(cfg: &RmConfig, entry: &ftw::Entry, metadata: &ftw::Metadata) -> bool {
    let writable = metadata.is_writable();
    if ask_for_prompt(cfg, writable) {
        let prompt = if writable {
            gettext!(
                "remove directory '{}'?",
                entry.path().clean_trailing_slashes()
            )
        } else {
            gettext!(
                "remove write-protected directory '{}'?",
                entry.path().clean_trailing_slashes(),
            )
        };
        if !prompt_user(&prompt) {
            return false;
        }
    }
    true
}

// The signature of `filename_fn` is to prevent unnecessarily building the filename when a prompt
// is not required.
fn should_remove_file<F>(cfg: &RmConfig, metadata: &ftw::Metadata, filename_fn: F) -> bool
where
    F: Fn() -> String,
{
    let writable = metadata.is_writable();
    if ask_for_prompt(cfg, writable) {
        let file_type = metadata.file_type();
        let prompt = match file_type {
            ftw::FileType::Socket => {
                gettext!("remove socket '{}'?", filename_fn())
            }
            ftw::FileType::SymbolicLink => {
                gettext!("remove symbolic link '{}'?", filename_fn())
            }
            ftw::FileType::BlockDevice => {
                gettext!("remove block special file '{}'?", filename_fn())
            }
            ftw::FileType::CharacterDevice => {
                gettext!("remove character special file '{}'?", filename_fn())
            }
            ftw::FileType::Fifo => {
                gettext!("remove fifo '{}'?", filename_fn())
            }
            ftw::FileType::RegularFile => {
                let is_empty = metadata.size() == 0;
                if writable {
                    if is_empty {
                        gettext!("remove regular empty file '{}'?", filename_fn())
                    } else {
                        gettext!("remove regular file '{}'?", filename_fn())
                    }
                } else if is_empty {
                    gettext!(
                        "remove write-protected regular empty file '{}'?",
                        filename_fn()
                    )
                } else {
                    gettext!("remove write-protected regular file '{}'?", filename_fn())
                }
            }
            // Directories are handled by the caller before reaching here; fall back to a generic
            // prompt rather than panicking if that ever changes.
            ftw::FileType::Directory => {
                gettext!("remove directory '{}'?", filename_fn())
            }
            ftw::FileType::Unknown => {
                gettext!("remove '{}'?", filename_fn())
            }
        };

        if !prompt_user(&prompt) {
            return false;
        }
    }

    true
}

enum DirAction {
    Removed,
    Entered,
    Skipped,
}

/// Directly remove a directory or enter it.
fn process_directory(
    cfg: &RmConfig,
    entry: &ftw::Entry,
    metadata: &ftw::Metadata,
) -> io::Result<DirAction> {
    let dir_is_empty = entry.is_empty_dir();

    // If directory is empty or the directory is inaccessible, try to remove it directly
    if (dir_is_empty.is_ok() && dir_is_empty.as_ref().unwrap() == &true) || dir_is_empty.is_err() {
        if should_remove_directory(cfg, entry, metadata) {
            let ret = unsafe {
                libc::unlinkat(
                    entry.dir_fd(),
                    entry.file_name().as_ptr(),
                    libc::AT_REMOVEDIR,
                )
            };
            if ret != 0 {
                let err_str = if let Err(e1) = dir_is_empty {
                    gettext!(
                        "cannot remove '{}': {}",
                        entry.path().clean_trailing_slashes(),
                        error_string(&e1)
                    )
                } else {
                    let e2 = io::Error::last_os_error();
                    gettext!(
                        "cannot remove directory '{}': {}",
                        entry.path().clean_trailing_slashes(),
                        error_string(&e2)
                    )
                };
                Err(io::Error::other(err_str))
            } else {
                report_removed(cfg, true, &entry.path().clean_trailing_slashes());
                Ok(DirAction::Removed)
            }
        } else {
            Ok(DirAction::Skipped)
        }

    // Else, manually traverse the directory to remove the contents one-by-one
    } else if descend_into_directory(cfg, entry, metadata) {
        Ok(DirAction::Entered)
    } else {
        Ok(DirAction::Skipped)
    }
}

/// Recursively removes a directory.
///
/// This function returns `Ok(true)` on success. The return value of `Ok(false)`
/// denotes that the error message is already printed to stderr to is used to
/// change the exit code in `main`.
fn rm_directory(cfg: &RmConfig, filepath: &Path) -> io::Result<bool> {
    if !cfg.args.recurse {
        let err_str = gettext!(
            "cannot remove '{}': Is a directory",
            display_cleaned(filepath)
        );
        return Err(io::Error::other(err_str));
    }

    // It's not allowed to `rm` . and .. or the root directory.
    refuse_dot_dotdot_root(filepath)?;

    let success = traverse_directory(
        filepath,
        |entry| {
            let md = entry.metadata().unwrap();

            if md.file_type() == ftw::FileType::Directory {
                match process_directory(cfg, &entry, md) {
                    Ok(dir_action) => match dir_action {
                        DirAction::Entered => Ok(true),
                        DirAction::Removed | DirAction::Skipped => Ok(false),
                    },
                    Err(e) => {
                        eprintln!("rm: {}", error_string(&e));
                        Err(())
                    }
                }
            } else {
                if should_remove_file(cfg, md, || entry.path().clean_trailing_slashes()) {
                    // Remove the file
                    let ret =
                        unsafe { libc::unlinkat(entry.dir_fd(), entry.file_name().as_ptr(), 0) };

                    if ret != 0 {
                        let e = io::Error::last_os_error();
                        eprintln!(
                            "rm: {}",
                            gettext!(
                                "cannot remove '{}': {}",
                                entry.path().clean_trailing_slashes(),
                                error_string(&e)
                            )
                        );
                        return Err(());
                    }
                    report_removed(cfg, false, &entry.path().clean_trailing_slashes());
                }
                Ok(true)
            }
        },
        |entry| {
            let md = entry.metadata().unwrap();
            if should_remove_directory(cfg, &entry, md) {
                // Remove the directory
                let ret = unsafe {
                    libc::unlinkat(
                        entry.dir_fd(),
                        entry.file_name().as_ptr(),
                        libc::AT_REMOVEDIR,
                    )
                };

                if ret != 0 {
                    let e = io::Error::last_os_error();

                    // `ENOTEMPTY` means one or more subdirectories were not
                    // removed. Do not flood the output by recursively
                    // printing `Directory not empty` errors.
                    if e.raw_os_error() != Some(libc::ENOTEMPTY) {
                        let err_str = gettext!(
                            "cannot remove directory '{}': {}",
                            entry.path().clean_trailing_slashes(),
                            error_string(&e)
                        );
                        eprintln!("rm: {}", err_str);
                        return Err(());
                    }
                } else {
                    report_removed(cfg, true, &entry.path().clean_trailing_slashes());
                }
            }

            Ok(())
        },
        |entry, error| match error.kind() {
            ftw::ErrorKind::OpenDir => {
                eprintln!(
                    "rm: {}",
                    gettext!(
                        "cannot access directory '{}': {}",
                        entry.path().clean_trailing_slashes(),
                        error_string(&error.inner())
                    )
                );
            }
            ftw::ErrorKind::ReadDir => {
                eprintln!(
                    "rm: {}",
                    gettext!(
                        "error accessing directory entry: {}",
                        entry.path().clean_trailing_slashes(),
                    )
                );
            }
            ftw::ErrorKind::Stat => {
                eprintln!(
                    "rm: {}",
                    gettext!(
                        "cannot stat '{}': {}",
                        entry.path().clean_trailing_slashes(),
                        error_string(&error.inner())
                    )
                );
            }
            ftw::ErrorKind::Open | ftw::ErrorKind::DirNotSearchable => {
                eprintln!(
                    "rm: {}",
                    gettext!(
                        "cannot remove '{}': {}",
                        entry.path().clean_trailing_slashes(),
                        error_string(&error.inner())
                    )
                );
            }
            // rm never follows symlinks, so this is not expected; report rather than panic.
            ftw::ErrorKind::ReadLink => {
                eprintln!(
                    "rm: {}",
                    gettext!(
                        "cannot read symbolic link '{}': {}",
                        entry.path().clean_trailing_slashes(),
                        error_string(&error.inner())
                    )
                );
            }
        },
        ftw::TraverseDirectoryOpts::default(),
    );

    Ok(success)
}

/// Open the parent directory of `filepath` and return its descriptor plus the basename.
///
/// This lets the top-level single-file removal stat and unlink relative to a pinned parent
/// directory fd (audit #R4) instead of re-resolving the whole operand path twice, narrowing the
/// TOCTOU window between classification and removal. A `None` parent (operand with no directory
/// component) resolves against the current working directory.
fn open_parent(filepath: &Path) -> io::Result<(ftw::FileDescriptor, CString)> {
    let basename = filepath
        .file_name()
        .ok_or_else(|| io::Error::other(gettext!("invalid path: {}", display_cleaned(filepath))))?;
    let basename_cstr = CString::new(basename.as_bytes())?;

    let parent = filepath.parent().filter(|p| !p.as_os_str().is_empty());
    let parent_fd = match parent {
        Some(p) => {
            let parent_cstr = CString::new(p.as_os_str().as_bytes())?;
            ftw::FileDescriptor::open_at(
                &ftw::FileDescriptor::cwd(),
                &parent_cstr,
                libc::O_RDONLY | libc::O_DIRECTORY,
            )?
        }
        None => ftw::FileDescriptor::cwd(),
    };

    Ok((parent_fd, basename_cstr))
}

/// Removes a file.
///
/// This function returns `Ok(true)` on success. This never returns `Ok(false)` and the function
/// signature is only to match `rm_directory`.
fn rm_file(cfg: &RmConfig, filepath: &Path) -> io::Result<bool> {
    let (parent_fd, basename_cstr) = open_parent(filepath)?;
    let metadata = ftw::Metadata::new(parent_fd.as_raw_fd(), &basename_cstr, false)?;

    if should_remove_file(cfg, &metadata, || display_cleaned(filepath)) {
        let ret = unsafe { libc::unlinkat(parent_fd.as_raw_fd(), basename_cstr.as_ptr(), 0) };
        if ret != 0 {
            let e = io::Error::last_os_error();
            let err_str = gettext!(
                "cannot remove '{}': {}",
                display_cleaned(filepath),
                error_string(&e)
            );
            return Err(io::Error::other(err_str));
        }
        report_removed(cfg, false, &display_cleaned(filepath));
    }

    Ok(true)
}

/// Removes an empty directory (the `-d` option, without `-r`/`-R`), like `rmdir`.
///
/// Per POSIX rm DESCRIPTION 113369-113370 and RATIONALE 113532-113535, `-d` proceeds straight to
/// the removal step for a directory operand (no recursion); a non-empty directory fails with the
/// `remove_dir`/`rmdir` error, avoiding the type-check race of deciding what to do by file type.
fn rm_dir_empty(cfg: &RmConfig, filepath: &Path) -> io::Result<bool> {
    refuse_dot_dotdot_root(filepath)?;

    let filename_cstr = CString::new(filepath.as_os_str().as_bytes())?;
    let metadata = ftw::Metadata::new(libc::AT_FDCWD, &filename_cstr, false)?;

    let writable = metadata.is_writable();
    if ask_for_prompt(cfg, writable) {
        let prompt = if writable {
            gettext!("remove directory '{}'?", display_cleaned(filepath))
        } else {
            gettext!(
                "remove write-protected directory '{}'?",
                display_cleaned(filepath)
            )
        };
        if !prompt_user(&prompt) {
            return Ok(true);
        }
    }

    fs::remove_dir(filepath).map_err(|e| {
        let err_str = gettext!(
            "cannot remove '{}': {}",
            display_cleaned(filepath),
            error_string(&e)
        );
        io::Error::other(err_str)
    })?;
    report_removed(cfg, true, &display_cleaned(filepath));

    Ok(true)
}

fn rm_path(cfg: &RmConfig, filepath: &Path) -> io::Result<bool> {
    let metadata = match fs::symlink_metadata(filepath) {
        Ok(md) => md,
        Err(e) => {
            // Not an error with -f in the case of operands that do not exist
            if e.kind() == io::ErrorKind::NotFound && cfg.args.force {
                return Ok(true);
            } else {
                let err_str = gettext!(
                    "cannot remove '{}': {}",
                    display_cleaned(filepath),
                    error_string(&e)
                );
                return Err(io::Error::other(err_str));
            }
        }
    };

    if metadata.is_dir() {
        // `-r`/`-R` take precedence over `-d` (113534-113535). With only `-d`, remove an empty
        // directory like `rmdir`; otherwise the recursive path errors when `-r` is absent.
        if cfg.args.dir && !cfg.args.recurse {
            rm_dir_empty(cfg, filepath)
        } else {
            rm_directory(cfg, filepath)
        }
    } else {
        rm_file(cfg, filepath)
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let is_tty = io::stdin().is_terminal();
    let cfg = RmConfig { args, is_tty };

    // POSIX rm SYNOPSIS form 1 requires at least one operand; only the `-f` form permits none, in
    // which case rm is silent and successful (113405-113407).
    if cfg.args.files.is_empty() {
        if cfg.args.force {
            std::process::exit(0);
        }
        eprintln!("rm: {}", gettext("missing operand"));
        std::process::exit(1);
    }

    let mut exit_code = 0;

    for filepath in &cfg.args.files {
        match rm_path(&cfg, filepath) {
            Ok(success) => {
                if !success {
                    exit_code = 1;
                }
            }
            Err(e) => {
                exit_code = 1;
                eprintln!("rm: {}", error_string(&e));
            }
        }
    }

    std::process::exit(exit_code)
}
