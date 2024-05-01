//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//

extern crate atty;
extern crate clap;
extern crate libc;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, textdomain};
use plib::PROJECT_NAME;
use std::collections::{HashMap, HashSet};
use std::ffi::{CStr, CString};
use std::os::unix::fs::FileTypeExt;
use std::os::{fd::AsRawFd, unix::ffi::OsStrExt, unix::fs::MetadataExt};
use std::path::{Path, PathBuf};
use std::{fs, io};

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

struct Config {
    force: bool,
    interactive: bool,
    is_terminal: bool,
}

impl Config {
    fn new(args: &Args) -> Self {
        Config {
            force: args.force,
            interactive: args.interactive,
            is_terminal: atty::is(atty::Stream::Stdin),
        }
    }
}

/// Return the error message.
///
/// This is for compatibility with coreutils mv. `format!("{e}")` will append
/// the error code after the error message which we do not want.
fn error_string(e: io::Error) -> String {
    let s = match e.raw_os_error() {
        // Like `format!("{e}")` except without the error code.
        //
        // `std` doesn't expose `sys::os::error_string` so this was copied from:
        // https://github.com/rust-lang/rust/blob/72f616273cbbacc06918ef50470d052d39d9b514/library/std/src/sys/pal/unix/os.rs#L124-L149
        Some(errno) => {
            let mut buf = [0; 128];

            unsafe {
                if libc::strerror_r(errno as _, buf.as_mut_ptr(), buf.len()) == 0 {
                    String::from_utf8_lossy(CStr::from_ptr(buf.as_ptr()).to_bytes()).to_string()
                } else {
                    // `std` just panics here
                    String::from("Unknown error")
                }
            }
        }
        None => format!("{e}"),
    };

    // Translate the error string
    gettext(s)
}

fn copy_characteristics(source: &Path, target: &Path, target_file: &fs::File) -> io::Result<()> {
    let source_md = source.metadata()?;

    // [last_access_time, last_modified_time]
    let times = [
        libc::timespec {
            tv_sec: source_md.atime(),
            tv_nsec: source_md.atime_nsec(),
        },
        libc::timespec {
            tv_sec: source_md.mtime(),
            tv_nsec: source_md.mtime_nsec(),
        },
    ];

    unsafe {
        // Copy last access and last modified times
        let ret = libc::futimens(target_file.as_raw_fd(), times.as_ptr());
        if ret != 0 {
            return Err(io::Error::last_os_error());
        }
    }

    let target_cstr = CString::new(target.as_os_str().as_bytes())?;

    unsafe {
        // Copy user and group
        let ret = libc::chown(target_cstr.as_ptr(), source_md.uid(), source_md.gid());
        if ret != 0 {
            return Err(io::Error::last_os_error());
        }

        // Copy permissions
        let ret = libc::chmod(target_cstr.as_ptr(), source_md.mode() as libc::mode_t);
        if ret != 0 {
            return Err(io::Error::last_os_error());
        }
    }
    Ok(())
}

fn copy_file_and_attributes(
    source: &Path,
    target: &Path,
    source_md: &fs::Metadata,
) -> io::Result<()> {
    let file_type = source_md.file_type();

    // These will cause `fs::copy` to hang
    if file_type.is_block_device()
        || file_type.is_char_device()
        || file_type.is_fifo()
        || file_type.is_socket()
    {
        unsafe {
            let target_cstr = CString::new(target.as_os_str().as_bytes())?;

            // The types of these two are not the same for all platforms. This
            // should just be converting them back to their original type that
            // `libc::stat` returned.
            let mode = source_md.mode() as libc::mode_t;
            let dev = source_md.rdev() as libc::dev_t;

            let ret = libc::mknod(target_cstr.as_ptr(), mode, dev);
            if ret == 0 {
                return Ok(());
            } else {
                return Err(io::Error::last_os_error());
            }
        }
    }

    fs::copy(&source, &target)?;

    const MAX_TRIES: i32 = 5;
    let mut tries = 0;

    // Loop until `fs::copy` creates the file
    let target_file = loop {
        match fs::File::open(&target) {
            Ok(f) => break f,
            Err(e) => {
                if e.kind() == io::ErrorKind::NotFound && tries < MAX_TRIES {
                    tries += 1;
                    continue;
                } else {
                    return Err(e);
                }
            }
        }
    };

    // Error while copying file characteristics does not change the exit status
    let _ = copy_characteristics(source, target, &target_file);
    Ok(())
}

fn copy_or_hard_link(
    source: &Path,
    target: &Path,
    source_md: &fs::Metadata,
    inode_map: &mut HashMap<(u64, u64), PathBuf>,
) -> io::Result<()> {
    // Same (device ID, inode) means a hard link.
    // This pessimistically assumes that the file hierarchy of the source may
    // contain more than one filesystem. The addition of the device ID is
    // because inode is not unique across filesystems.
    let identifier = (source_md.dev(), source_md.ino());

    if let Some(prev) = inode_map.get(&identifier) {
        // Preserve hard links like coreutils mv. Creating a copy is also
        // allowed by the standard.
        fs::hard_link(prev, &target)?;
    } else {
        // 6. copy source to target
        copy_file_and_attributes(&source, &target, source_md)?;

        // Don't include every file, just those with hard links
        if source_md.nlink() > 1 {
            inode_map.insert(identifier, target.to_path_buf());
        }
    }
    Ok(())
}

fn copy_dir_all(
    src: &Path,
    dst: &Path,
    inode_map: &mut HashMap<(u64, u64), PathBuf>,
) -> io::Result<()> {
    fs::create_dir_all(&dst)?;

    for entry in fs::read_dir(src)? {
        let entry = entry?;

        let source = entry.path();
        let target = dst.join(entry.file_name());

        if source.is_dir() {
            copy_dir_all(&source, &target, inode_map)?;
        } else {
            let source_md = entry.metadata()?;

            copy_or_hard_link(&source, &target, &source_md, inode_map).map_err(
                |e: io::Error| -> io::Error {
                    let from_to = gettext!("'{}' to '{}'", source.display(), target.display());
                    let err_str = format!("{}: {}", from_to, error_string(e));
                    io::Error::other(err_str)
                },
            )?;
        }
    }

    Ok(())
}

fn prompt_user(prompt: &str) -> bool {
    eprint!("mv: {} ", prompt);
    let mut response = String::new();
    io::stdin().read_line(&mut response).unwrap();
    response.to_lowercase().starts_with('y')
}

fn is_file_writable(md: &Option<fs::Metadata>) -> bool {
    match md {
        Some(md) => {
            // These are "effective" IDs and not "real" to allow for things like
            // sudo
            let uid = unsafe { libc::geteuid() };
            let gid = unsafe { libc::getegid() };

            let same_user = md.uid() == uid;
            let same_group = md.gid() == gid;

            // `libc::mode_t` is not the same for all platforms while
            // `unix::fs::MetadataExt::mode` is always a `u32`.
            let mode = md.mode() as libc::mode_t;

            if same_user && (mode & libc::S_IWUSR != 0) {
                true
            } else if same_group && (mode & libc::S_IWGRP != 0) {
                true
            } else {
                mode & libc::S_IWOTH != 0
            }
        }
        None => false,
    }
}

/// Handles moving the file.
///
/// Returns `Ok(true)` if the source was deleted and `Ok(false)` if it's not.
fn move_file(
    cfg: &Config,
    source: &Path,
    target: &Path,
    inode_map: &mut HashMap<(u64, u64), PathBuf>,
    created_files: Option<&HashSet<PathBuf>>,
) -> io::Result<bool> {
    let target_md = match fs::metadata(target) {
        Ok(md) => Some(md),
        Err(e) => {
            if e.kind() == io::ErrorKind::NotFound {
                None
            } else {
                let err_str = format!("{}: {}", target.display(), error_string(e));
                return Err(io::Error::other(err_str));
            }
        }
    };
    let target_exists = target_md.is_some();
    let target_is_dir = match &target_md {
        Some(md) => md.is_dir(),
        None => false,
    };
    let target_is_writable = is_file_writable(&target_md);

    let source_md = match fs::metadata(source) {
        Ok(md) => Some(md),
        Err(e) => {
            if e.kind() == io::ErrorKind::NotFound {
                None
            } else {
                let err_str = format!("{}: {}", source.display(), error_string(e));
                return Err(io::Error::other(err_str));
            }
        }
    };
    let source_exists = source_md.is_some();
    let source_is_dir = match &source_md {
        Some(md) => md.is_dir(),
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
        fs::symlink_metadata(source),
        fs::symlink_metadata(target),
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
        if let Some(created_files) = created_files {
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
                            error_string(e)
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
        let err_str = format!("{}: {}", from_to, error_string(e));
        io::Error::other(err_str)
    };

    let err_inter_device = |e: io::Error| -> io::Error {
        let err_str = gettext!("inter-device move failed: {}", e);
        io::Error::other(err_str)
    };

    // 5. remove destination path
    if target.exists() {
        let remove_result = if target_is_dir {
            fs::remove_dir(target)
        } else {
            fs::remove_file(target)
        };
        remove_result
            .map_err(|e| {
                let err_str = gettext!("unable to remove target: {}", error_string(e));
                io::Error::other(err_str)
            })
            .map_err(err_reason)
            .map_err(err_inter_device)?;
    }

    if source_is_dir {
        copy_dir_all(source, target, inode_map).map_err(err_inter_device)?;
    } else {
        // This is a `Some` otherwise `fs::rename` in (3) would have caught it.
        let source_md = source_md.unwrap();

        copy_or_hard_link(source, target, &source_md, inode_map)
            .map_err(err_reason)
            .map_err(err_inter_device)?;
    }

    Ok(false)
}

fn move_files(cfg: &Config, sources: &[PathBuf], target: &Path) -> Option<()> {
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
                    Some(&created_files),
                ) {
                    Ok(is_source_deleted) => {
                        created_files.insert(new_target);

                        if !is_source_deleted {
                            sources_to_delete.push(source);
                        }
                    }
                    Err(e) => {
                        eprintln!("mv: {}", error_string(e));
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
                error_string(e)
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
                    eprintln!("mv: {}: {}", target.display(), error_string(e));
                    std::process::exit(1);
                }
            }
        }
    };

    let cfg = Config::new(&args);
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
