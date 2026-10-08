//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
//

mod common;
mod remove_moved;

use self::common::{copy_moved_file, error_string};
use clap::Parser;
use common::{
    CopiedSources, CopyConfig, DerefMode, Destination, InodeMap, MoveSource, PinnedDir, PinnedDirs,
    PinnedEntry,
};
use gettextrs::gettext;
use remove_moved::remove_moved_source;
use std::{
    collections::{HashMap, HashSet},
    fs,
    io::{self, IsTerminal},
    os::unix::{ffi::OsStrExt, fs::MetadataExt},
    path::{Path, PathBuf},
};

/// mv - move files
#[derive(Parser)]
#[command(version, about = gettext("mv - move files"))]
struct Args {
    #[arg(short, long, overrides_with_all = ["force", "interactive"], help = gettext("Do not prompt for confirmation if the destination path exists"))]
    force: bool,

    #[arg(short, long, overrides_with_all = ["force", "interactive"], help = gettext("Prompt for confirmation if the destination path exists"))]
    interactive: bool,

    // `PathBuf` instead of `String` avoids the inefficient reconverting of a
    // `String` to a `&Path` when calling the `std::fs` functions. It also
    // facilitates processing filenames that are non-UTF8 but are still valid in
    // Unix.
    #[arg(help = gettext("Source(s) and target of move(s)"))]
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
    eprint!("mv: {prompt} ");
    let mut response = String::new();
    // A read error or EOF is a non-affirmative response, not a panic.
    if io::stdin().read_line(&mut response).unwrap_or(0) == 0 {
        return false;
    }
    plib::locale::is_affirmative(response.trim_end_matches(['\r', '\n']))
}

/// A source operand that was copied across filesystems and is still to be removed (POSIX mv
/// step 7).
struct CopiedOperand {
    source: PinnedEntry,
    copied: CopiedSources,
}

impl CopiedOperand {
    /// Remove the source hierarchy the copy duplicated. Returns whether all of it was removed;
    /// what was not has been reported.
    fn remove(&self) -> bool {
        remove_moved_source(&self.source, &self.copied)
    }
}

/// What became of one source operand.
enum Moved {
    /// Renamed into place, or left alone at the user's request: nothing remains to be done.
    Done,
    /// Copied across filesystems; the source remains to be removed.
    Copied(CopiedOperand),
}

/// Copy the file or directory hierarchy `source`, which must still be the file `identity`
/// names, to `dst`.
fn copy_hierarchy(
    source: PinnedEntry,
    identity: (u64, u64),
    dst: &PinnedEntry,
    inode_map: &mut InodeMap,
    created_files: &mut HashSet<PathBuf>,
) -> io::Result<CopiedOperand> {
    let copy_cfg = CopyConfig {
        // `mv` already asked its own POSIX step-1 question (108060-108064) and step 5 removed the
        // destination, so the copy engine must not ask again for the same file. `force` here
        // carries only the cp step-3.a.iii meaning: unlink a destination that cannot be opened
        // and retry.
        force: true,
        interactive: false,
        no_clobber: false,
        // POSIX mv step 6 (108097-108099): links are duplicated as links, including a link
        // named as an operand -- moving one across a filesystem must not turn it into a copy of
        // whatever it points at.
        deref: DerefMode::Never,
        preserve: true,  // Always copy file attributes
        recursive: true, // Recursively copy
        prog: "mv",
        // mv must stop the duplication on the first structural error so the source is not removed.
        continue_on_error: false,
        // Step 5 removed the destination, or found none: anything at its name now appeared
        // during the move, and is neither written into nor filled.
        destination: Destination::MustCreate,
    };

    let mut copied = CopiedSources::default();
    copy_moved_file(
        &copy_cfg,
        MoveSource {
            entry: &source,
            identity,
            copied: &mut copied,
        },
        dst,
        created_files,
        inode_map,
    )?;
    Ok(CopiedOperand { source, copied })
}

/// The diagnostic for a move that failed before anything was done.
fn cannot_move(source: &Path, target: &Path, e: &io::Error) -> io::Error {
    io::Error::other(gettext!(
        "cannot move '{}' to '{}': {}",
        source.display(),
        target.display(),
        error_string(e)
    ))
}

/// Handles moving the file.
///
/// Source and destination are pinned (`PinnedDirs`, `PinnedDir`) before anything else: from the
/// checks to the removal of a copied source, every operation on either is relative to the
/// directory it was found in then.
fn move_file(
    cfg: &MvConfig,
    pinned_dirs: &mut PinnedDirs,
    source: &Path,
    target_entry: &PinnedEntry,
    inode_map: &mut InodeMap,
    created_files: Option<&mut HashSet<PathBuf>>,
) -> io::Result<Moved> {
    let target = target_entry.path();
    let source_entry = pinned_dirs
        .pin(source)
        .map_err(|e| cannot_move(source, target, &e))?;

    // The destination itself, not followed, as rename(2) replaces it: a symbolic link there,
    // dangling or not, is a non-directory that the move replaces (step 5 removes it before a
    // copy, which must then create the destination itself).
    let target_md = match target_entry.metadata(false) {
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
    let target_is_dir = target_md.as_ref().is_some_and(|md| md.is_dir());
    // As in `rm`, a symbolic link destination is not write-protected: `mv` replaces the link
    // itself, not what it points at, so neither the link's own mode bits nor the referent's
    // apply.
    let target_is_symlink = target_md.as_ref().is_some_and(|md| md.is_symlink());
    let target_is_writable =
        target_is_symlink || ftw::is_writable_at(target_entry.dir_fd(), target_entry.name());

    // The operand itself, not followed: what a rename, or the copy and removal, act on.
    let source_lstat = source_entry.metadata(false);
    let source_md = match source_entry.metadata(true) {
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
            return Ok(Moved::Done);
        }
    }

    // 2. source and target are same dirent
    if let (Ok(smd), Some(tmd), Some(deref_smd)) = (&source_lstat, &target_md, &source_md) {
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
    match rename_pinned(&source_entry, target_entry) {
        Ok(_) => return Ok(Moved::Done),
        Err(e) => {
            // use ErrorKind::CrossesDevices in the future, when it is stable.
            // Use the captured error's errno rather than re-reading the global errno.
            let errno = e.raw_os_error().unwrap_or(0);
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

    // The copy must start from the file examined above, and is what step 7 removes.
    let identity = match &source_lstat {
        Ok(md) => (md.dev(), md.ino()),
        Err(e) => return Err(cannot_move(source, target, e)),
    };

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
        remove_target(target_entry, target_is_dir)
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
    let copied = copy_hierarchy(
        source_entry,
        identity,
        target_entry,
        inode_map,
        created_files,
    )
    .map_err(err_inter_device)?;

    Ok(Moved::Copied(copied))
}

/// rename(2) of the pinned source to the pinned target.
fn rename_pinned(source: &PinnedEntry, target: &PinnedEntry) -> io::Result<()> {
    let ret = unsafe {
        libc::renameat(
            source.dir_fd(),
            source.name().as_ptr(),
            target.dir_fd(),
            target.name().as_ptr(),
        )
    };
    if ret == 0 {
        Ok(())
    } else {
        Err(io::Error::last_os_error())
    }
}

/// Step 5: remove the destination, in the directory it was pinned in. A directory is removed
/// only if it is empty.
fn remove_target(target: &PinnedEntry, is_dir: bool) -> io::Result<()> {
    let flags = if is_dir { libc::AT_REMOVEDIR } else { 0 };
    if unsafe { libc::unlinkat(target.dir_fd(), target.name().as_ptr(), flags) } == 0 {
        Ok(())
    } else {
        Err(io::Error::last_os_error())
    }
}

fn move_files(cfg: &MvConfig, sources: &[PathBuf], target: &Path) -> Option<()> {
    let mut result = Some(());

    let mut created_files = HashSet::new();
    let mut pinned_dirs = PinnedDirs::default();

    // The target directory is resolved once, here: every operand goes into this directory.
    let target_dir = match PinnedDir::open(target) {
        Ok(dir) => dir,
        Err(e) => {
            eprintln!("mv: {}: {}", target.display(), error_string(&e));
            return None;
        }
    };

    // inode of source -> target path
    let mut inode_map = HashMap::with_capacity(sources.len());

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
                let new_target = match target_dir.entry(file_name) {
                    Ok(entry) => entry,
                    Err(e) => {
                        eprintln!("mv: {}: {}", source.display(), error_string(&e));
                        result = None;
                        continue;
                    }
                };

                // Don't immediately bubble up the error with `?` to allow the
                // remaining files to be processed.
                match move_file(
                    cfg,
                    &mut pinned_dirs,
                    source,
                    &new_target,
                    &mut inode_map,
                    Some(&mut created_files),
                ) {
                    Ok(moved) => {
                        created_files.insert(new_target.path().to_path_buf());

                        if let Moved::Copied(copied) = moved {
                            sources_to_delete.push(copied);
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
    for copied in sources_to_delete {
        if !copied.remove() {
            result = None;
        }
    }

    result
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("mv");

    let args = Args::parse();

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

    // More than one source requires the target to be an existing directory (mv synopsis form 2).
    if !dir_exists && sources.len() > 1 {
        eprintln!(
            "mv: {}",
            gettext!("target '{}' is not a directory", target.display())
        );
        std::process::exit(1);
    }

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

        // First synopsis form (108049-108050): "if source_file names a non-directory file and
        // target_file ends with a trailing <slash> character, mv shall treat this as an error and
        // no source_file operands shall be processed."
        if target.as_os_str().as_bytes().ends_with(b"/") {
            if let Ok(md) = fs::symlink_metadata(source) {
                if !md.is_dir() {
                    eprintln!(
                        "mv: {}",
                        gettext!(
                            "cannot move '{}' to '{}': Not a directory",
                            source.display(),
                            target.display()
                        )
                    );
                    std::process::exit(1);
                }
            }
        }
        let mut dummy = HashMap::new();
        let mut pinned_dirs = PinnedDirs::default();
        let target_entry = match pinned_dirs.pin(target) {
            Ok(entry) => entry,
            Err(e) => {
                eprintln!("mv: {}", cannot_move(source, target, &e));
                std::process::exit(1);
            }
        };
        match move_file(
            &cfg,
            &mut pinned_dirs,
            source,
            &target_entry,
            &mut dummy,
            None,
        ) {
            Ok(Moved::Done) => Ok(()),
            // 7. Remove source file hierarchy
            Ok(Moved::Copied(copied)) => {
                if !copied.remove() {
                    std::process::exit(1);
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
