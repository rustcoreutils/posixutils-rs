//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate atty;
extern crate clap;
extern crate plib;

mod common;

use self::common::{error_string, is_file_writable};
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, textdomain};
use plib::PROJECT_NAME;
use std::collections::HashSet;
use std::ffi::{OsStr, OsString};
use std::fs;
use std::io;
use std::os::unix::{
    ffi::OsStrExt,
    fs::{FileTypeExt, MetadataExt},
};
use std::path::{Path, PathBuf};

/// rm - remove directory entries
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
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

fn descend_into_directory(cfg: &RmConfig, dir: &Path, metadata: &fs::Metadata) -> bool {
    let writable = is_file_writable(Some(metadata));
    if ask_for_prompt(cfg, writable) {
        let prompt = if writable {
            gettext!("descend into directory '{}'?", display_cleaned(dir))
        } else {
            gettext!(
                "descend into write-protected directory '{}'?",
                display_cleaned(dir)
            )
        };
        if !prompt_user(&prompt) {
            return false;
        }
    }
    true
}

fn should_remove_directory(cfg: &RmConfig, dir: &Path, metadata: &fs::Metadata) -> bool {
    let writable = is_file_writable(Some(metadata));
    if ask_for_prompt(cfg, writable) {
        let prompt = if writable {
            gettext!("remove directory '{}'?", display_cleaned(dir))
        } else {
            gettext!(
                "remove write-protected directory '{}'?",
                display_cleaned(dir)
            )
        };
        if !prompt_user(&prompt) {
            return false;
        }
    }
    true
}

fn should_remove_file(cfg: &RmConfig, filepath: &Path, metadata: &fs::Metadata) -> bool {
    let writable = is_file_writable(Some(metadata));
    if ask_for_prompt(cfg, writable) {
        let file_type = metadata.file_type();
        let prompt = if file_type.is_block_device() {
            gettext!("remove block special file '{}'?", display_cleaned(filepath))
        } else if file_type.is_char_device() {
            gettext!(
                "remove character special file '{}'?",
                display_cleaned(filepath)
            )
        } else if file_type.is_fifo() {
            gettext!("remove fifo '{}'?", display_cleaned(filepath))
        } else if file_type.is_socket() {
            gettext!("remove socket '{}'?", display_cleaned(filepath))
        } else if file_type.is_symlink() {
            gettext!("remove symbolic link '{}'?", display_cleaned(filepath))
        } else {
            let is_empty = metadata.size() == 0;
            if writable {
                if is_empty {
                    gettext!("remove regular empty file '{}'?", display_cleaned(filepath))
                } else {
                    gettext!("remove regular file '{}'?", display_cleaned(filepath))
                }
            } else {
                if is_empty {
                    gettext!(
                        "remove write-protected regular empty file '{}'?",
                        display_cleaned(filepath)
                    )
                } else {
                    gettext!(
                        "remove write-protected regular file '{}'?",
                        display_cleaned(filepath)
                    )
                }
            }
        };

        if !prompt_user(&prompt) {
            return false;
        }
    }

    true
}

fn is_dir_empty<P>(dir: P) -> io::Result<bool>
where
    P: AsRef<Path>,
{
    let mut dir_iter = fs::read_dir(dir)?;
    Ok(dir_iter.next().is_none())
}

/// Removes a file.
///
/// This function returns `Ok(true)` on success. This never returns `Ok(false)`
/// and the function signature is only to match `rm_directory`.
fn rm_file(cfg: &RmConfig, filepath: &Path, metadata: &fs::Metadata) -> io::Result<bool> {
    if should_remove_file(cfg, filepath, metadata) {
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

/// Used for RAII resetting of the current working directory
struct CurrentDirectoryResetter {
    original: OsString,
}

impl Drop for CurrentDirectoryResetter {
    fn drop(&mut self) {
        let _ = std::env::set_current_dir(&self.original);
    }
}

impl CurrentDirectoryResetter {
    fn new() -> io::Result<Self> {
        Ok(Self {
            original: std::env::current_dir()?.into_os_string(),
        })
    }
}

enum DirAction {
    Removed,
    Entered,
    Skipped,
}

/// Directly remove a directory or enter it.
fn process_directory(
    cfg: &RmConfig,
    dir_name: &OsStr,
    dir_path: &Path,
    metadata: &fs::Metadata,
    cwd_backup: &mut Option<CurrentDirectoryResetter>,
) -> io::Result<DirAction> {
    // If directory is empty or the directory is inaccessible, try to remove it
    // directly
    let dir_is_empty = is_dir_empty(dir_name);
    if (dir_is_empty.is_ok() && dir_is_empty.as_ref().unwrap() == &true) || dir_is_empty.is_err() {
        if should_remove_directory(cfg, dir_path, metadata) {
            if let Err(e2) = fs::remove_dir(dir_name) {
                let err_str = if let Err(e1) = dir_is_empty {
                    gettext!(
                        "cannot remove '{}': {}",
                        display_cleaned(dir_path),
                        error_string(&e1)
                    )
                } else {
                    gettext!(
                        "cannot remove directory '{}': {}",
                        display_cleaned(dir_path),
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
        if descend_into_directory(cfg, dir_path, metadata) {
            if cwd_backup.is_none() {
                let backup = CurrentDirectoryResetter::new().map_err(|e| {
                    let err_str = gettext!(
                        "unable to get the current working directory: {}",
                        error_string(&e)
                    );
                    io::Error::other(err_str)
                })?;
                *cwd_backup = Some(backup);
            }

            if let Err(e) = std::env::set_current_dir(dir_name) {
                // Should not be "cannot remove directory"
                let err_str = gettext!(
                    "cannot remove '{}': {}",
                    display_cleaned(dir_path),
                    error_string(&e)
                );
                Err(io::Error::other(err_str))
            } else {
                Ok(DirAction::Entered)
            }
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
fn rm_directory(cfg: &RmConfig, filepath: &Path, metadata: &fs::Metadata) -> io::Result<bool> {
    let mut success = true;

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
    if let Ok(abspath) = fs::canonicalize(&filepath) {
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

    // Vec<(directory, visited_files)>
    let mut stack: Vec<(OsString, HashSet<OsString>)> = Vec::new();
    // Used to build the path that is used in prompts/error messages
    let mut current_path = PathBuf::new();

    let mut cwd_backup: Option<CurrentDirectoryResetter> = None;

    match process_directory(
        cfg,
        filepath.as_os_str(),
        filepath,
        metadata,
        &mut cwd_backup,
    )? {
        DirAction::Removed | DirAction::Skipped => return Ok(true),
        DirAction::Entered => {
            let name = match filepath.file_name() {
                Some(name) => name.to_os_string(),
                None => {
                    let canonical_path = fs::canonicalize(filepath).map_err(|e| {
                        let err_str = gettext!(
                            "cannot remove '{}': {}",
                            display_cleaned(filepath),
                            error_string(&e)
                        );
                        io::Error::other(err_str)
                    })?;

                    if let Some(name) = canonical_path.file_name() {
                        name.to_os_string()
                    } else {
                        canonical_path.into_os_string()
                    }
                }
            };

            current_path.push(filepath);
            stack.push((name, HashSet::new()));
        }
    }

    // Non-recursive directory traversal.
    //
    // `rm` needs to be able to remove files whose paths exceed the limit of the
    // filesystem. The naive way of using `fs::read_dir` or the `walkdir` is not
    // able to handle this and would error when getting the metadata or removing
    // the directory/file. The goal is achieved in this implementation by
    // entering sub-directories thereby keeping the relative paths below the
    // limit.
    'outer: loop {
        current_path.pop();
        let (dir, mut visited) = match stack.pop() {
            Some(last) => last,
            None => break, // Stack is empty
        };
        current_path.push(&dir);

        let read_dir_iterator = match fs::read_dir(".") {
            Ok(rd) => rd,
            Err(e) => {
                eprintln!(
                    "rm: {}",
                    gettext!(
                        "cannot access directory '{}': {}",
                        display_cleaned(&current_path),
                        error_string(&e)
                    )
                );
                success = false;
                continue;
            }
        };

        for entry in read_dir_iterator {
            let subname = match entry {
                Ok(x) => x.file_name(),
                Err(e) => {
                    eprintln!(
                        "rm: {}",
                        gettext!("error accessing directory entry: {}", error_string(&e))
                    );
                    success = false;
                    continue;
                }
            };

            // Need to manually keep track of visited files. The alternative of
            // keeping multiple directory iterators would produce "too many open
            // files" error.
            if visited.contains(&subname) {
                continue;
            }
            visited.insert(subname.clone());

            let entry_path = current_path.join(&subname);

            let sub_metadata = match fs::symlink_metadata(&subname) {
                Ok(md) => md,
                Err(e) => {
                    eprintln!(
                        "rm: {}",
                        gettext!(
                            "cannot stat '{}': {}",
                            display_cleaned(&entry_path),
                            error_string(&e)
                        )
                    );
                    success = false;
                    continue;
                }
            };

            if sub_metadata.is_dir() {
                match process_directory(cfg, &subname, &entry_path, &sub_metadata, &mut cwd_backup)
                {
                    Ok(action) => match action {
                        DirAction::Removed | DirAction::Skipped => (),
                        DirAction::Entered => {
                            // Counteracts the `PathBuf::pop` at the start of
                            // the outer loop
                            current_path.push(&dir);

                            // Depth first traversal. Process the subdirectory
                            // `subname` first before finishing `dir`
                            stack.push((dir, visited));
                            stack.push((subname, HashSet::new()));

                            continue 'outer;
                        }
                    },
                    Err(e) => {
                        success = false;
                        eprintln!("rm: {}", error_string(&e));
                    }
                }
            } else {
                if should_remove_file(cfg, &entry_path, &sub_metadata) {
                    if let Err(e) = fs::remove_file(&subname) {
                        eprintln!(
                            "rm: {}",
                            gettext!(
                                "cannot remove '{}': {}",
                                display_cleaned(&entry_path),
                                error_string(&e)
                            )
                        );
                        success = false;
                    }
                }
            }
        }

        let dir_metadata = fs::symlink_metadata(".");

        // Ascend up a level no matter the return value of the previous
        // `fs::symlink_metadata`
        std::env::set_current_dir("..")?;

        match dir_metadata {
            Ok(md) => {
                if should_remove_directory(cfg, &current_path, &md) {
                    if let Err(e) = fs::remove_dir(&dir) {
                        // `ENOTEMPTY` means one or more subdirectories were not
                        // removed. Do not flood the output by recursively
                        // printing `Directory not empty` errors.
                        if e.raw_os_error() != Some(libc::ENOTEMPTY) {
                            // The two different error messages here is purely
                            // to match coreutils' `rm` output.
                            let err_str = if e.kind() == io::ErrorKind::PermissionDenied {
                                gettext!(
                                    "cannot remove '{}': {}", // Not "cannot remove directory"
                                    display_cleaned(&current_path),
                                    error_string(&e)
                                )
                            } else {
                                gettext!(
                                    "cannot remove directory '{}': {}",
                                    display_cleaned(&current_path),
                                    error_string(&e)
                                )
                            };
                            eprintln!("rm: {}", err_str);
                            success = false;
                        }
                    }
                }
            }
            Err(e) => {
                eprintln!(
                    "rm: {}",
                    gettext!(
                        "cannot stat '{}': {}",
                        display_cleaned(&current_path),
                        error_string(&e)
                    )
                );
                success = false;
            }
        };
    }

    Ok(success)
}

fn rm_path(cfg: &RmConfig, filepath: &Path) -> io::Result<bool> {
    let metadata = match fs::symlink_metadata(&filepath) {
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
        rm_directory(cfg, filepath, &metadata)
    } else {
        rm_file(cfg, filepath, &metadata)
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let is_tty = atty::is(atty::Stream::Stdin);
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
