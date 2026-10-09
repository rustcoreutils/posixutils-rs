//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod common;

use self::common::{error_string, exit_after_verbose, report_verbose};
use clap::Parser;
use ftw::{self, traverse_directory};
use gettextrs::gettext;
use std::{
    cell::Cell,
    ffi::{CStr, CString},
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
    /// `(st_dev, st_ino)` of the root directory, which a recursive removal refuses to enter.
    /// `None` when `/` could not be stat'ed: every recursive removal is then refused.
    root_identity: Option<(u64, u64)>,
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

/// Whether `e` only says that the file is not there, which with `-f` is no error and no
/// diagnostic wherever rm finds it out (POSIX rm -f: "Do not write diagnostic messages ... for
/// nonexistent operands"): another process may remove the file between any two of rm's steps,
/// as parallel `rm -f *.o` cleans do.
fn is_already_gone(cfg: &RmConfig, e: &io::Error) -> bool {
    cfg.args.force && e.raw_os_error() == Some(libc::ENOENT)
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
        report_verbose(&msg);
    }
}

// rm shall refuse `.`/`..` (as the basename) and an operand resolving to the root directory
// (POSIX rm DESCRIPTION 113360-113362, APPLICATION USAGE 113466-113468).
fn refuse_dot_dotdot_root(filepath: &Path) -> io::Result<()> {
    // The last component, trailing slashes aside, is exactly `.` or `..`.
    let dot_dotdot_pattern = regex::bytes::Regex::new(r"(?:^|/)\.{1,2}/*$").unwrap();
    if dot_dotdot_pattern.is_match(filepath.as_os_str().as_bytes()) {
        let err_str = gettext!(
            "refusing to remove '.' or '..' directory: skipping '{}'",
            display_cleaned(filepath)
        );
        return Err(io::Error::other(err_str));
    }

    if let Ok(abspath) = fs::canonicalize(filepath) {
        if abspath.as_os_str() == "/" {
            return Err(io::Error::other(dangerous_root_message(
                &filepath.display().to_string(),
            )));
        }
    }

    Ok(())
}

/// The refusal of an operand that is the root directory, named as `shown`.
fn dangerous_root_message(shown: &str) -> String {
    if shown == "/" {
        gettext("it is dangerous to operate recursively on '/'")
    } else {
        gettext!(
            "it is dangerous to operate recursively on '{}' (same as '/')",
            shown
        )
    }
}

/// Whether the walk is about to enter the root directory itself. The pathname check in
/// `refuse_dot_dotdot_root` is only a first answer: the operand is resolved again when the walk
/// opens it (a symbolic link named with a trailing slash is followed then), so this compares the
/// identity of the directory the walk actually stat'ed and will open -- `ftw` checks the opened
/// descriptor against that same `(dev, ino)` -- with the root's.
fn is_root_directory(cfg: &RmConfig, md: &ftw::Metadata) -> bool {
    cfg.root_identity == Some((md.dev(), md.ino()))
}

/// Whether removing this file counts as unprotected, which is what decides the wording of the
/// prompt and, without `-f`, whether there is one at all.
///
/// A symbolic link is never treated as write-protected: what `rm` unlinks is the link, whose own
/// mode bits carry no meaning, and the permissions of whatever it points at are not the ones
/// being overridden. Following it would also make a dangling link look unwritable. GNU `rm` makes
/// the same exception.
fn is_writable_for_removal(dirfd: libc::c_int, file_name: &CStr, metadata: &ftw::Metadata) -> bool {
    metadata.file_type() == ftw::FileType::SymbolicLink || ftw::is_writable_at(dirfd, file_name)
}

fn descend_into_directory(cfg: &RmConfig, entry: &ftw::Entry) -> bool {
    let writable = entry.is_writable();
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

fn should_remove_directory(cfg: &RmConfig, entry: &ftw::Entry) -> bool {
    let writable = entry.is_writable();
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
fn should_remove_file<F>(
    cfg: &RmConfig,
    dirfd: libc::c_int,
    file_name: &CStr,
    metadata: &ftw::Metadata,
    filename_fn: F,
) -> bool
where
    F: Fn() -> String,
{
    let writable = is_writable_for_removal(dirfd, file_name, metadata);
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
fn process_directory(cfg: &RmConfig, entry: &ftw::Entry) -> io::Result<DirAction> {
    let dir_is_empty = entry.is_empty_dir();

    // If directory is empty or the directory is inaccessible, try to remove it directly
    if (dir_is_empty.is_ok() && dir_is_empty.as_ref().unwrap() == &true) || dir_is_empty.is_err() {
        if should_remove_directory(cfg, entry) {
            if let Err(e2) = entry.unlink(libc::AT_REMOVEDIR) {
                if is_already_gone(cfg, &e2) {
                    return Ok(DirAction::Skipped);
                }
                let err_str = if let Err(e1) = dir_is_empty {
                    gettext!(
                        "cannot remove '{}': {}",
                        entry.path().clean_trailing_slashes(),
                        error_string(&e1)
                    )
                } else {
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
    } else if descend_into_directory(cfg, entry) {
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

    // The walk refuses the root directory by its identity; without that identity it cannot, so
    // the removal is refused rather than walked unguarded.
    if cfg.root_identity.is_none() {
        let err_str = gettext!(
            "cannot remove '{}': the root directory could not be identified",
            display_cleaned(filepath)
        );
        return Err(io::Error::other(err_str));
    }

    // Set by every diagnostic. The walk's own result cannot stand for the exit status: it also
    // counts the errors `is_already_gone` excuses.
    let had_error = Cell::new(false);
    traverse_directory(
        filepath,
        |entry| {
            let result = remove_walked(cfg, &entry);
            if result.is_err() {
                had_error.set(true);
            }
            result
        },
        |entry, exit| {
            let result = remove_left_directory(cfg, &entry, exit);
            if result.is_err() {
                had_error.set(true);
            }
            result
        },
        |entry, error| {
            let kind = error.kind();
            let error = error.inner();
            if is_already_gone(cfg, &error) {
                return;
            }
            had_error.set(true);
            report_walk_error(&entry, kind, &error);
        },
        ftw::TraverseDirectoryOpts::default(),
    );

    Ok(!had_error.get())
}

/// The walk's `file_handler` for `rm_directory`: remove `entry`, or enter it if it is a
/// directory with contents. `Err` means a diagnostic was written.
fn remove_walked(cfg: &RmConfig, entry: &ftw::Entry) -> Result<bool, ()> {
    let md = entry.metadata().unwrap();

    if md.file_type() == ftw::FileType::Directory {
        // `link/` names the directory the link points to, which no removal takes away
        // by that name. Refuse it before descending rather than empty that directory
        // (GNU does): a directory operand swapped for a symlink would otherwise redirect
        // the whole removal.
        if entry.reached_through_symlink() {
            eprintln!(
                "rm: {}",
                gettext!(
                    "cannot remove '{}': {}",
                    entry.path().clean_trailing_slashes(),
                    error_string(&io::Error::from_raw_os_error(libc::ENOTDIR))
                )
            );
            return Err(());
        }
        if is_root_directory(cfg, md) {
            let shown = entry.path().clean_trailing_slashes();
            eprintln!("rm: {}", dangerous_root_message(&shown));
            return Err(());
        }
        match process_directory(cfg, entry) {
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
        if let Err(e) = remove_nondir_at(cfg, entry.dir_fd(), entry.file_name(), md, || {
            entry.path().clean_trailing_slashes()
        }) {
            eprintln!("rm: {}", error_string(&e));
            return Err(());
        }
        Ok(true)
    }
}

/// The walk's `postprocess_dir` for `rm_directory`: remove the directory `entry` once its
/// contents are gone. `Err` means a diagnostic was written.
fn remove_left_directory(cfg: &RmConfig, entry: &ftw::Entry, exit: ftw::DirExit) -> Result<(), ()> {
    // A directory the traversal could not descend into still has its contents, so
    // prompting for it and attempting the removal would only produce a second diagnostic
    // on top of the one already reported.
    if exit == ftw::DirExit::NotDescended {
        return Ok(());
    }

    if should_remove_directory(cfg, entry) {
        // Remove the directory
        if let Err(e) = entry.unlink(libc::AT_REMOVEDIR) {
            // `ENOTEMPTY` means one or more subdirectories were not
            // removed. Do not flood the output by recursively
            // printing `Directory not empty` errors. With -f a directory
            // someone else removed meanwhile is no error either.
            if e.raw_os_error() != Some(libc::ENOTEMPTY) && !is_already_gone(cfg, &e) {
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
}

/// Report an error the walk of `rm_directory` met at `entry`.
fn report_walk_error(entry: &ftw::Entry, kind: ftw::ErrorKind, error: &io::Error) {
    match kind {
        ftw::ErrorKind::OpenDir => {
            eprintln!(
                "rm: {}",
                gettext!(
                    "cannot access directory '{}': {}",
                    entry.path().clean_trailing_slashes(),
                    error_string(error)
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
        ftw::ErrorKind::Stat | ftw::ErrorKind::Cycle => {
            eprintln!(
                "rm: {}",
                gettext!(
                    "cannot remove '{}': {}",
                    entry.path().clean_trailing_slashes(),
                    error_string(error)
                )
            );
        }
        ftw::ErrorKind::Open => {
            eprintln!(
                "rm: {}",
                gettext!(
                    "cannot remove '{}': {}",
                    entry.path().clean_trailing_slashes(),
                    error_string(error)
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
                    error_string(error)
                )
            );
        }
    }
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
    let classified = open_parent(filepath).and_then(|(parent_fd, basename_cstr)| {
        let metadata = ftw::Metadata::new(parent_fd.as_raw_fd(), &basename_cstr, false)?;
        Ok((parent_fd, basename_cstr, metadata))
    });
    let (parent_fd, basename_cstr, metadata) = match classified {
        Ok(found) => found,
        Err(e) if is_already_gone(cfg, &e) => return Ok(true),
        Err(e) => return Err(e),
    };

    remove_nondir_at(
        cfg,
        parent_fd.as_raw_fd(),
        &basename_cstr,
        &metadata,
        || display_cleaned(filepath),
    )?;
    Ok(true)
}

/// Removes the non-directory `file_name` in the directory open on `dirfd`, which `metadata`
/// describes, after prompting as the options require; `shown` names it in messages.
///
/// Declining the prompt is not an error. The returned error carries the full diagnostic.
fn remove_nondir_at<F>(
    cfg: &RmConfig,
    dirfd: libc::c_int,
    file_name: &CStr,
    metadata: &ftw::Metadata,
    shown: F,
) -> io::Result<()>
where
    F: Fn() -> String,
{
    if !should_remove_file(cfg, dirfd, file_name, metadata, &shown) {
        return Ok(());
    }
    let ret = unsafe { libc::unlinkat(dirfd, file_name.as_ptr(), 0) };
    if ret != 0 {
        let e = io::Error::last_os_error();
        if is_already_gone(cfg, &e) {
            return Ok(());
        }
        let err_str = gettext!("cannot remove '{}': {}", shown(), error_string(&e));
        return Err(io::Error::other(err_str));
    }
    report_removed(cfg, false, &shown());
    Ok(())
}

/// Removes an empty directory (the `-d` option, without `-r`/`-R`), like `rmdir`.
///
/// Per POSIX rm DESCRIPTION 113369-113370 and RATIONALE 113532-113535, `-d` proceeds straight to
/// the removal step for a directory operand (no recursion); a non-empty directory fails with the
/// `remove_dir`/`rmdir` error, avoiding the type-check race of deciding what to do by file type.
fn rm_dir_empty(cfg: &RmConfig, filepath: &Path) -> io::Result<bool> {
    refuse_dot_dotdot_root(filepath)?;

    let filename_cstr = CString::new(filepath.as_os_str().as_bytes())?;

    let writable = ftw::is_writable_at(libc::AT_FDCWD, &filename_cstr);
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

    match fs::remove_dir(filepath) {
        Ok(()) => report_removed(cfg, true, &display_cleaned(filepath)),
        Err(e) if is_already_gone(cfg, &e) => (),
        Err(e) => {
            let err_str = gettext!(
                "cannot remove '{}': {}",
                display_cleaned(filepath),
                error_string(&e)
            );
            return Err(io::Error::other(err_str));
        }
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
    plib::diag::init_locale("rm");

    let args = Args::parse();

    let is_tty = io::stdin().is_terminal();
    let root_identity = fs::metadata("/").ok().map(|md| (md.dev(), md.ino()));
    let cfg = RmConfig {
        args,
        is_tty,
        root_identity,
    };

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

    exit_after_verbose(exit_code == 0)
}

#[cfg(test)]
mod tests {
    use super::{
        refuse_dot_dotdot_root, remove_nondir_at, rm_dir_empty, rm_directory, rm_file, Args,
        RmConfig,
    };
    use clap::Parser;
    use std::{ffi::CString, fs, os::fd::AsRawFd, os::unix::fs::MetadataExt, path::Path};

    fn config(flags: &str) -> RmConfig {
        RmConfig {
            args: Args::parse_from(["rm", flags, "operand"]),
            is_tty: false,
            root_identity: Some((0, 0)),
        }
    }

    /// Only a last component that is `.` or `..` is refused, with or without trailing slashes:
    /// a name that merely ends in dots is an ordinary name.
    #[test]
    fn only_dot_and_dot_dot_are_refused() {
        for refused in [
            ".", "..", "./", "../", ".//", "a/.", "a/..", "a/./", "/x/../",
        ] {
            assert!(
                refuse_dot_dotdot_root(Path::new(refused)).is_err(),
                "{refused}"
            );
        }
        // None of these exists, so the root-directory check after the name check passes too.
        for allowed in [
            "nonexistent-foo.",
            "nonexistent-x..",
            "a-nonexistent/foo.",
            "nonexistent-x../",
            ".nonexistent-hidden",
            "..nonexistent-x",
        ] {
            assert!(
                refuse_dot_dotdot_root(Path::new(allowed)).is_ok(),
                "{allowed}"
            );
        }
    }

    /// The refusal of the root directory binds to the directory the walk opens, not to the
    /// operand's pathname: with a stand-in directory as "root", `rm -r link/` (link -> it) is
    /// refused although the pathname check, which canonicalizes to the real root only, passes,
    /// and nothing in it is removed.
    #[test]
    fn root_refusal_checks_the_walked_directory() {
        let tmp = plib::tmp::tempdir().unwrap();
        let fake_root = tmp.path().join("root");
        fs::create_dir(&fake_root).unwrap();
        fs::write(fake_root.join("f"), b"x").unwrap();
        let link = tmp.path().join("rootlink");
        std::os::unix::fs::symlink(&fake_root, &link).unwrap();
        let md = fs::metadata(&fake_root).unwrap();

        let operand = format!("{}/", link.display());
        let cfg = RmConfig {
            args: Args::parse_from(["rm", "-rf", operand.as_str()]),
            is_tty: false,
            root_identity: Some((md.dev(), md.ino())),
        };
        let walked_ok = rm_directory(&cfg, operand.as_ref()).unwrap();

        assert!(!walked_ok);
        assert!(fake_root.join("f").exists());
        assert!(fs::symlink_metadata(&link).unwrap().is_symlink());
    }

    /// Without the root's identity there is nothing to refuse the root by, so a recursive
    /// removal is refused outright, removing nothing, rather than walked unguarded.
    #[test]
    fn unknown_root_identity_refuses_recursive_removal() {
        let tmp = plib::tmp::tempdir().unwrap();
        let dir = tmp.path().join("d");
        fs::create_dir(&dir).unwrap();
        fs::write(dir.join("f"), b"x").unwrap();

        let cfg = RmConfig {
            args: Args::parse_from(["rm", "-rf", dir.to_str().unwrap()]),
            is_tty: false,
            root_identity: None,
        };
        assert!(rm_directory(&cfg, &dir).is_err());
        assert!(dir.join("f").exists());
    }

    /// With -f a file that is gone when rm reaches it is no error, at whichever step it is found
    /// missing: here it vanishes between being classified and being unlinked, as when parallel
    /// `rm -f *.o` runs race. Without -f the unlink failure is reported.
    #[test]
    fn force_ignores_a_file_gone_before_unlink() {
        let tmp = plib::tmp::tempdir().unwrap();
        let dir = CString::new(tmp.path().as_os_str().as_encoded_bytes()).unwrap();
        let dir_fd = ftw::FileDescriptor::open_at(
            &ftw::FileDescriptor::cwd(),
            &dir,
            libc::O_RDONLY | libc::O_DIRECTORY,
        )
        .unwrap();
        fs::write(tmp.path().join("x.o"), b"x").unwrap();
        let md = ftw::Metadata::new(dir_fd.as_raw_fd(), c"x.o", false).unwrap();
        fs::remove_file(tmp.path().join("x.o")).unwrap();

        let shown = || String::from("x.o");
        assert!(remove_nondir_at(&config("-f"), dir_fd.as_raw_fd(), c"x.o", &md, shown).is_ok());
        let err = remove_nondir_at(&config("-v"), dir_fd.as_raw_fd(), c"x.o", &md, shown)
            .unwrap_err()
            .to_string();
        assert!(err.contains("cannot remove 'x.o'"), "{err}");
    }

    /// The same at the other steps: the file's own stat, its parent's open, an empty directory's
    /// removal, and the start of a recursive walk all find nothing, silently and successfully.
    #[test]
    fn force_ignores_an_operand_gone_after_the_first_stat() {
        let tmp = plib::tmp::tempdir().unwrap();
        let gone = tmp.path().join("gone");
        let in_gone = gone.join("x.o");

        assert!(rm_file(&config("-f"), &gone).unwrap());
        assert!(rm_file(&config("-f"), &in_gone).unwrap());
        assert!(rm_dir_empty(&config("-fd"), &gone).unwrap());
        assert!(rm_directory(&config("-rf"), &gone).unwrap());
        assert!(rm_directory(&config("-rf"), &in_gone).unwrap());

        assert!(rm_file(&config("-v"), &gone).is_err());
        assert!(rm_dir_empty(&config("-d"), &gone).is_err());
        assert!(!rm_directory(&config("-r"), &gone).unwrap());
    }
}
