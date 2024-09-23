//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//

mod common;

use self::common::{copy_file, error_string};
use clap::Parser;
use common::CopyConfig;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;
use std::{
    collections::{HashMap, HashSet},
    ffi::CString,
    fs,
    io::{self, IsTerminal},
    os::unix::{ffi::OsStrExt, fs::MetadataExt},
    path::{Path, PathBuf},
};

/// mv - move files
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Do not prompt for confirmation if the destination path exists
    #[arg(short, long, overrides_with_all = ["force", "interactive"])]
    force: bool,

    /// Prompt for confirmation if the destination path exists.
    #[arg(short, long, overrides_with_all = ["force", "interactive"])]
    interactive: bool,

    /// Source(s) and target of move(s)
    // `PathBuf` instead of `String` avoids the inefficient reconverting of a
    // `String` to a `&Path` when calling the `std::fs` functions. It also
    // facilitates processing filenames that are non-UTF8 but are still valid in
    // Unix.
    files: Vec<PathBuf>,
}

struct MvConfig {
    force: bool,
    interactive: bool,
    is_terminal: bool,
}

impl MvConfig {
    fn new(args: &Args) -> Self {
        MvConfig {
            force: args.force,
            interactive: args.interactive,
            is_terminal: io::stdin().is_terminal(),
        }
    }
}

fn prompt_user(prompt: &str) -> bool {
    eprint!("mv: {} ", prompt);
    let mut response = String::new();
    io::stdin().read_line(&mut response).unwrap();
    response.to_lowercase().starts_with('y')
}

// Copy the file or directory hierarchy from `src` to `dst`.
fn copy_hierarchy(
    cfg: &MvConfig,
    src: &Path,
    dst: &Path,
    inode_map: &mut HashMap<(u64, u64), (ftw::FileDescriptor, CString)>,
    created_files: &mut HashSet<PathBuf>,
) -> io::Result<()> {
    let copy_cfg = CopyConfig {
        force: cfg.force,
        follow_cli: true,   // Follow symlink if passed as an argument
        dereference: false, // Don't follow symlinks
        interactive: cfg.interactive,
        preserve: true,  // Always copy file attributes
        recursive: true, // Recursively copy
    };

    copy_file(
        &copy_cfg,
        src,
        dst,
        created_files,
        Some(inode_map),
        prompt_user,
    )
}

/// Handles moving the file.
///
/// Returns `Ok(true)` if the source was deleted and `Ok(false)` if it's not.
fn move_file(
    cfg: &MvConfig,
    source: &Path,
    target: &Path,
    inode_map: &mut HashMap<(u64, u64), (ftw::FileDescriptor, CString)>,
    created_files: Option<&mut HashSet<PathBuf>>,
) -> io::Result<bool> {
    let source_filename = CString::new(source.as_os_str().as_bytes()).unwrap();
    let target_filename = CString::new(target.as_os_str().as_bytes()).unwrap();

    let target_md =
        match unsafe { ftw::Metadata::new(libc::AT_FDCWD, target_filename.as_ptr(), true) } {
            Ok(md) => Some(md),
            Err(e) => {
                if e.kind() == io::ErrorKind::NotFound {
                    None
                } else {
                    let err_str = format!("{}: {}", target.display(), error_string(&e));
                    return Err(io::Error::other(err_str));
                }
            }
        };
    let target_exists = target_md.is_some();
    let target_is_dir = match &target_md {
        Some(md) => md.file_type() == ftw::FileType::Directory,
        None => false,
    };
    let target_is_writable = target_md.map(|md| md.is_writable()).unwrap_or(false);

    let source_md =
        match unsafe { ftw::Metadata::new(libc::AT_FDCWD, source_filename.as_ptr(), true) } {
            Ok(md) => Some(md),
            Err(e) => {
                if e.kind() == io::ErrorKind::NotFound {
                    None
                } else {
                    let err_str = format!("{}: {}", source.display(), error_string(&e));
                    return Err(io::Error::other(err_str));
                }
            }
        };
    let source_exists = source_md.is_some();
    let source_is_dir = match &source_md {
        Some(md) => md.file_type() == ftw::FileType::Directory,
        None => false,
    };

    // 1. If the destination path exists, conditionally prompt user
    if target_exists && !cfg.force && ((!target_is_writable && cfg.is_terminal) || cfg.interactive)
    {
        let is_affirm = prompt_user(&gettext!("overwrite '{}'?", target.display()));
        if !is_affirm {
            return Ok(true);
        }
    }

    // 2. source and target are same dirent
    if let (Ok(smd), Ok(tmd), Some(deref_smd)) = (
        unsafe { ftw::Metadata::new(libc::AT_FDCWD, source_filename.as_ptr(), false) },
        unsafe { ftw::Metadata::new(libc::AT_FDCWD, target_filename.as_ptr(), false) },
        &source_md,
    ) {
        // `true` for hard links to the same file and when `source == target`
        let same_file = smd.dev() == tmd.dev() && smd.ino() == tmd.ino();

        // Forbids overwriting a file with a symlink to it.
        let source_is_symlink_to_target =
            deref_smd.dev() == tmd.dev() && deref_smd.ino() == tmd.ino();

        if same_file || source_is_symlink_to_target {
            // 2.b. Issue a diagnostic, target and source are both untouched.
            // This matches coreutils mv behavior.
            let err_str = gettext!(
                "'{}' and '{}' are the same file",
                source.display(),
                target.display()
            );
            return Err(io::Error::other(err_str));
        }
    }

    // 4. handle source/target dir mismatch
    //
    // It doesn't make sense for (4) to be after (3) as stated in the
    // specification since renaming file -> dir or dir -> file are errors for
    // `libc::rename` (EISDIR and ENOTDIR, respectively). Same with overwriting
    // previously moved file which must be checked beforehand since it's hard to
    // undo.
    //
    // `source_exists` is to let the error formatting in (3) handle missing
    // source files
    if source_exists && target_exists {
        match (source_is_dir, target_is_dir) {
            (true, false) => {
                let err_str = gettext!(
                    "cannot overwrite non-directory '{}' with directory '{}'",
                    target.display(),
                    source.display(),
                );
                return Err(io::Error::other(err_str));
            }
            (false, true) => {
                let err_str = gettext!(
                    "cannot overwrite directory '{}' with non-directory '{}'",
                    target.display(),
                    source.display(),
                );
                return Err(io::Error::other(err_str));
            }
            _ => (), // Both directories or both files
        }

        // This concerns whether to allow `mv` to potentially destroy user data
        // by overwriting a previously moved file with another file.
        //
        // It is unspecified in the standard whether this is an error or not.
        // GNU coreutils `mv` treats it as an error.
        //
        // `created_files` is `None` when `move_file` is called directly from
        // `main`.
        if let Some(created_files) = created_files.as_ref() {
            if created_files.contains(target) {
                let err_str = gettext!(
                    "will not overwrite just-created '{}' with '{}'",
                    target.display(),
                    source.display(),
                );
                return Err(io::Error::other(err_str));
            }
        }
    }

    // 3. call rename(2) to move source to target
    match fs::rename(source, target) {
        Ok(_) => return Ok(true),
        Err(e) => {
            // use ErrorKind::CrossesDevices in the future, when it is stable
            let errno = std::io::Error::last_os_error().raw_os_error().unwrap();
            if errno != libc::EXDEV {
                let err_str = match errno {
                    // The new directory pathname contains a path prefix that
                    // names the old directory.
                    libc::EINVAL => {
                        gettext!(
                            "cannot move '{}' to a subdirectory of itself, '{}'",
                            source.display(),
                            target.display()
                        )
                    }
                    // Generic error message
                    _ => {
                        gettext!(
                            "cannot move '{}' to '{}': {}",
                            source.display(),
                            target.display(),
                            error_string(&e)
                        )
                    }
                };
                return Err(io::Error::other(err_str));
            }
        }
    }

    // Fall through: source and target are on different filesystems; must copy.

    let err_reason = |e: io::Error| -> io::Error {
        let from_to = gettext!("'{}' to '{}'", source.display(), target.display(),);
        let err_str = format!("{}: {}", from_to, error_string(&e));
        io::Error::other(err_str)
    };

    let err_inter_device = |e: io::Error| -> io::Error {
        let err_str = gettext!("inter-device move failed: {}", e);
        io::Error::other(err_str)
    };

    // 5. remove destination path
    if target_exists {
        let remove_result = if target_is_dir {
            fs::remove_dir(target)
        } else {
            fs::remove_file(target)
        };
        remove_result
            .map_err(|e| {
                let err_str = gettext!("unable to remove target: {}", error_string(&e));
                io::Error::other(err_str)
            })
            .map_err(err_reason)
            .map_err(err_inter_device)?;
    }

    let created_files = match created_files {
        Some(set) => set,
        None => &mut HashSet::new(),
    };
    copy_hierarchy(cfg, source, target, inode_map, created_files).map_err(err_inter_device)?;

    Ok(false)
}

fn move_files(cfg: &MvConfig, sources: &[PathBuf], target: &Path) -> Option<()> {
    let mut result = Some(());

    let mut created_files = HashSet::new();

    // inode of source -> target path
    let mut inode_map = HashMap::new();

    // Postpone deletion when moving across filesystems because it would
    // otherwise error when copying dangling hard links
    let mut sources_to_delete = Vec::new();

    // loop through sources, moving each to target
    for source in sources {
        match source.file_name() {
            Some(file_name) => {
                // Concatenation of the target directory, a single <slash>
                // character if the target did not end in a <slash>, and the
                // last pathname component of the source_file.
                let new_target = target.join(file_name);

                // Don't immediately bubble up the error with `?` to allow the
                // remaining files to be processed.
                match move_file(
                    cfg,
                    source,
                    &new_target,
                    &mut inode_map,
                    Some(&mut created_files),
                ) {
                    Ok(is_source_deleted) => {
                        created_files.insert(new_target);

                        if !is_source_deleted {
                            sources_to_delete.push(source);
                        }
                    }
                    Err(e) => {
                        eprintln!("mv: {}", error_string(&e));
                        result = None;
                    }
                }
            }

            // Ends in `/..`
            None => {
                let err_str = gettext!("invalid filename: {}", source.display());
                eprintln!("mv: {}", err_str);
                result = None;
            }
        }
    }

    // 7. Remove source file hierarchy
    for source in sources_to_delete {
        let remove_result = if source.is_dir() {
            fs::remove_dir_all(source)
        } else {
            fs::remove_file(source)
        };
        if let Err(e) = remove_result {
            eprintln!(
                "mv: {}: {}",
                gettext!("cannot remove '{}'", source.display()),
                error_string(&e)
            );
            result = None;
        }
    }

    result
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    // Initialize translation system
    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    if args.files.len() < 2 {
        eprintln!(
            "mv: {}",
            gettext("Must supply a source and target for move")
        );
        std::process::exit(1);
    }

    // split sources and target
    let sources = &args.files[0..args.files.len() - 1];
    let target = &args.files[args.files.len() - 1];

    // choose mode based on whether target is a directory
    let dir_exists = {
        match fs::metadata(target) {
            Ok(md) => md.is_dir(),
            Err(e) => {
                if e.kind() == io::ErrorKind::NotFound {
                    false
                } else {
                    eprintln!("mv: {}: {}", target.display(), error_string(&e));
                    std::process::exit(1);
                }
            }
        }
    };

    let cfg = MvConfig::new(&args);
    if dir_exists {
        match move_files(&cfg, sources, target) {
            Some(_) => Ok(()),
            None => {
                // Already eprintln'd the errors
                std::process::exit(1);
            }
        }
    } else {
        let source = &sources[0];
        let mut dummy = HashMap::new();
        match move_file(&cfg, source, target, &mut dummy, None) {
            Ok(is_source_deleted) => {
                // 7. Remove source file hierarchy
                if !is_source_deleted {
                    if source.is_dir() {
                        fs::remove_dir_all(source)?;
                    } else {
                        fs::remove_file(source)?;
                    }
                }
                Ok(())
            }
            Err(e) => {
                eprintln!("mv: {}", e);
                std::process::exit(1);
            }
        }
    }
}
