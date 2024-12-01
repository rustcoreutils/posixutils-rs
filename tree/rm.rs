//
// Copyright (c) 2024 Hemi Labs, Inc.
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
    os::unix::{ffi::OsStrExt, fs::MetadataExt},
    path::{Path, PathBuf},
};

/// rm - remove directory entries
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Do not prompt for confirmation.
    #[arg(short, long, overrides_with_all = ["force", "interactive"])]
    force: bool,

    /// Prompt for confirmation.
    #[arg(short, long, overrides_with_all = ["force", "interactive"])]
    interactive: bool,

    /// Remove file hierarchies.
    #[arg(short, visible_short_alias = 'R', long)]
    recurse: bool,

    /// Filepaths to remove
    #[arg(value_parser = parse_pathbuf)]
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
    eprint!("rm: {} ", prompt);
    let mut response = String::new();
    io::stdin().read_line(&mut response).unwrap();
    response.to_lowercase().starts_with('y')
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
                } else {
                    if is_empty {
                        gettext!(
                            "remove write-protected regular empty file '{}'?",
                            filename_fn()
                        )
                    } else {
                        gettext!("remove write-protected regular file '{}'?", filename_fn())
                    }
                }
            }
            ftw::FileType::Directory => unreachable!(), // Handled in the caller
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
                Ok(DirAction::Removed)
            }
        } else {
            Ok(DirAction::Skipped)
        }

    // Else, manually traverse the directory to remove the contents one-by-one
    } else {
        if descend_into_directory(cfg, entry, metadata) {
            Ok(DirAction::Entered)
        } else {
            Ok(DirAction::Skipped)
        }
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

    // It's not allowed to `rm` . and ..
    let dot_dotdot_pattern = regex::bytes::Regex::new(r"(?:\.\/*|\.\.\/*)$").unwrap();
    if dot_dotdot_pattern.is_match(filepath.as_os_str().as_bytes()) {
        let err_str = gettext!(
            "refusing to remove '.' or '..' directory: skipping '{}'",
            display_cleaned(filepath)
        );
        return Err(io::Error::other(err_str));
    }

    // Also forbidden to `rm` the root directory
    if let Ok(abspath) = fs::canonicalize(filepath) {
        if abspath.as_os_str() == "/" {
            // If the arg is verbatim "/"
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
            ftw::ErrorKind::ReadLink => unreachable!(), // rm doesn't follow symlinks
        },
        ftw::TraverseDirectoryOpts::default(),
    );

    Ok(success)
}

/// Removes a file.
///
/// This function returns `Ok(true)` on success. This never returns `Ok(false)` and the function
/// signature is only to match `rm_directory`.
fn rm_file(cfg: &RmConfig, filepath: &Path) -> io::Result<bool> {
    let filename_cstr = CString::new(filepath.as_os_str().as_bytes())?;
    let metadata = ftw::Metadata::new(libc::AT_FDCWD, &filename_cstr, false)?;

    if should_remove_file(cfg, &metadata, || display_cleaned(filepath)) {
        fs::remove_file(filepath).map_err(|e| {
            let err_str = gettext!(
                "cannot remove '{}': {}",
                display_cleaned(filepath),
                error_string(&e)
            );
            io::Error::other(err_str)
        })?;
    }

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
        rm_directory(cfg, filepath)
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
