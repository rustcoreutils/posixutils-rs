//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the pax-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Copy mode implementation - copy files between directories
//!
//! In copy mode (-r -w), pax copies files to a destination directory
//! without creating an intermediate archive. Hard links are created
//! between source and destination when possible (with -l option).

use crate::archive::HardLinkTracker;
use crate::error::{PaxError, PaxResult};
use crate::interactive::{InteractivePrompter, RenameResult};
use crate::modes::anchored::{
    create_replacing, open_dir_at, set_attrs_fd, set_link_attrs_at, stat_at, AttrPolicy, Attrs,
    DirTree, MemberPath,
};
use crate::pattern::{matches_any, Pattern};
use crate::subst::{apply_substitutions, SubstResult, Substitution};
use std::collections::HashSet;
use std::ffi::{CStr, CString};
use std::fs::{self, File};
use std::io::{Read, Write};
use std::os::fd::{AsFd, AsRawFd, BorrowedFd, FromRawFd};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::MetadataExt;
use std::path::{Path, PathBuf};

/// Options for copy mode
#[derive(Default)]
pub struct CopyOptions {
    /// Patterns to match (empty means match all)
    pub patterns: Vec<Pattern>,
    /// Match all except patterns
    pub exclude: bool,
    /// Don't overwrite existing files
    pub no_clobber: bool,
    /// Verbose output
    pub verbose: bool,
    /// Preserve permissions
    pub preserve_perms: bool,
    /// Preserve modification time
    pub preserve_mtime: bool,
    /// Preserve access time
    pub preserve_atime: bool,
    /// Preserve owner and group
    pub preserve_owner: bool,
    /// Create hard links instead of copying
    pub link: bool,
    /// Follow symlinks on command line
    pub cli_dereference: bool,
    /// Follow all symlinks
    pub dereference: bool,
    /// Don't descend into directories
    pub no_recurse: bool,
    /// Stay on one filesystem
    pub one_file_system: bool,
    /// Interactive rename mode
    pub interactive: bool,
    /// Update mode - only copy if source is newer than destination
    pub update: bool,
    /// Path substitutions (-s option)
    pub substitutions: Vec<Substitution>,
    /// Process file-creation mask, applied to the mode of copied files when the
    /// mode is not explicitly preserved (no `-p p`/`-p e`).
    pub umask: u32,
}

/// Copy files to a destination directory
pub fn copy_files(files: &[PathBuf], dest_dir: &Path, options: &CopyOptions) -> PaxResult<()> {
    // Verify destination is a directory
    if !dest_dir.exists() {
        return Err(PaxError::Io(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!(
                "destination directory does not exist: {}",
                dest_dir.display()
            ),
        )));
    }

    if !dest_dir.is_dir() {
        return Err(PaxError::Io(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("destination is not a directory: {}", dest_dir.display()),
        )));
    }

    // Everything below is written relative to this descriptor. POSIX defines a
    // copy as an archive round-trip, so the destination gets the same treatment
    // extraction gives it: each component of a member name is opened with
    // O_NOFOLLOW, and the leaf is created fresh rather than written through.
    let tree = DirTree::open_path(dest_dir)?;

    let mut state = CopyState {
        link_tracker: HardLinkTracker::new(),
        dest_ids: HashSet::new(),
        // Create interactive prompter if needed
        prompter: if options.interactive {
            Some(InteractivePrompter::new()?)
        } else {
            None
        },
    };
    if let Some(st) = stat_at(tree.root(), c".") {
        // Casts needed: `dev_t` is i32 on macOS and u64 on Linux.
        #[allow(clippy::unnecessary_cast)]
        state.dest_ids.insert((st.st_dev as u64, st.st_ino as u64));
    }

    // No filesystem is established until a directory is descended into; the
    // per-directory splits below are the ones that matter for -X.
    let initial_dev: Option<u64> = None;

    for path in files {
        // Diagnose a per-operand failure and set a non-zero exit, but continue
        // copying the remaining operands (POSIX CONSEQUENCES OF ERRORS).
        if let Err(e) = copy_member(
            path,
            &member_name(path),
            &tree,
            options,
            &mut state,
            initial_dev,
            true,
        ) {
            crate::error::report_error(path.display(), e);
        }
    }

    Ok(())
}

/// State threaded through the whole copy.
struct CopyState {
    link_tracker: HardLinkTracker,
    /// `(st_dev, st_ino)` of every destination directory this copy has created
    /// or entered. A source directory found in here is one being copied *into*.
    dest_ids: HashSet<(u64, u64)>,
    prompter: Option<InteractivePrompter>,
}

/// The archive-relative name a source path would be stored under, and so the
/// name it is restored to beneath the destination directory.
///
/// POSIX defines a copy as an archive round-trip, and write mode stores an
/// operand under the path the user gave it. Naming the destination after the
/// basename instead put `pax -r -w a/b/c dest` at `dest/c`, which no round trip
/// through an archive could produce. Leading slashes and `.`/`..` components
/// are dropped, exactly as extraction sanitizes a member name.
fn member_name(src: &Path) -> PathBuf {
    use std::path::Component;

    let mut out = PathBuf::new();
    for comp in src.components() {
        match comp {
            Component::Normal(c) => out.push(c),
            Component::ParentDir => {
                out.pop();
            }
            Component::CurDir | Component::RootDir | Component::Prefix(_) => {}
        }
    }
    out
}

/// Copy one source path, and its subtree if it is a directory, to `member`
/// beneath the destination anchor.
///
/// This is the single walk for both command-line operands and everything found
/// by recursion. It used to be two functions -- `copy_path` for operands and
/// `copy_path_to_dest` for their descendants -- and the second had never
/// acquired the first's `-s` substitution and pattern selection, so both
/// applied only to paths typed on the command line.
fn copy_member(
    src: &Path,
    member: &Path,
    tree: &DirTree,
    options: &CopyOptions,
    state: &mut CopyState,
    initial_dev: Option<u64>,
    is_cli_arg: bool,
) -> PaxResult<()> {
    let follow = should_follow_symlink(options, is_cli_arg);
    let metadata = if follow {
        fs::metadata(src)
    } else {
        fs::symlink_metadata(src)
    };
    let metadata = match metadata {
        Ok(m) => m,
        Err(e) => {
            crate::error::report_error(src.display(), e);
            return Ok(());
        }
    };

    // Selection and substitution both act on the member name, so they reach
    // every file in the subtree rather than only the operands.
    let member_str = member.to_string_lossy().to_string();
    if !options.patterns.is_empty() {
        let matches = matches_any(&options.patterns, &member_str);
        if options.exclude == matches {
            return Ok(());
        }
    }

    #[cfg(unix)]
    {
        if options.one_file_system {
            if let Some(initial) = initial_dev {
                if metadata.dev() != initial {
                    return Ok(());
                }
            }
        }
    }

    // -s applies before -i (POSIX: the order of -o, -p and -s is significant).
    let member = if options.substitutions.is_empty() {
        member.to_path_buf()
    } else {
        match apply_substitutions(&options.substitutions, &member_str) {
            SubstResult::Unchanged => member.to_path_buf(),
            SubstResult::Changed(new_name) => PathBuf::from(new_name),
            SubstResult::Empty => return Ok(()), // a null name means skip
        }
    };

    let member = if let Some(ref mut p) = state.prompter {
        match p.prompt(&member.to_string_lossy())? {
            RenameResult::Skip => return Ok(()),
            RenameResult::UseOriginal => member,
            RenameResult::Rename(new_name) => new_name,
        }
    } else {
        member
    };

    // A source directory that *is* one of this copy's destinations is one being
    // copied into. Following it walks the copy's own output back into itself
    // until the pathname runs out of room; identity cannot be spelled two ways,
    // where a path comparison could be defeated by any other spelling.
    if metadata.is_dir() && state.dest_ids.contains(&(metadata.dev(), metadata.ino())) {
        return Err(PaxError::InvalidFormat(format!(
            "cannot copy directory {} into itself",
            src.display()
        )));
    }

    if member.as_os_str().is_empty() {
        // The operand was `.`: its children carry the names, not itself.
        return copy_directory(src, tree, member.as_path(), options, state, &metadata);
    }

    let Some(mp) = MemberPath::parse(&member)? else {
        return Ok(());
    };

    // A member may name directories the walk has not created yet (`a/b/c` given
    // as an operand). POSIX requires the intermediate directories be made with
    // the normal file-creation action.
    let parent = tree.parent_of(&mp, true)?;
    let pfd = parent.as_fd();
    let name = mp.leaf.as_c_str();

    let existing = stat_at(pfd, name);
    if options.no_clobber && existing.is_some() {
        return Ok(());
    }
    if options.update && !is_source_newer(&metadata, existing.as_ref()) {
        return Ok(());
    }

    if options.verbose {
        eprintln!("{}", src.display());
    }

    if metadata.is_dir() {
        copy_directory(src, tree, &member, options, state, &metadata)?;
    } else if metadata.is_symlink() {
        copy_symlink(src, pfd, name, &metadata, options)?;
    } else if metadata.is_file() {
        copy_file(src, tree, pfd, name, &member, options, state, &metadata)?;
    } else if let Err(e) = copy_special_file(pfd, name, &metadata, options) {
        crate::error::report_error(src.display(), e);
    }

    Ok(())
}

/// Check if we should follow symlinks
fn should_follow_symlink(options: &CopyOptions, is_cli_arg: bool) -> bool {
    options.dereference || (is_cli_arg && options.cli_dereference)
}

/// Check if source is newer than the destination that is already there (`-u`).
fn is_source_newer(src_metadata: &fs::Metadata, dest: Option<&libc::stat>) -> bool {
    // If destination doesn't exist, always copy
    let Some(dest) = dest else {
        return true;
    };
    src_metadata.mtime() > dest.st_mtime
}

/// Copy a directory and its contents
fn copy_directory(
    src: &Path,
    tree: &DirTree,
    member: &Path,
    options: &CopyOptions,
    state: &mut CopyState,
    metadata: &fs::Metadata,
) -> PaxResult<()> {
    // Create the destination directory and hold a descriptor for it. Its own
    // attributes are applied on the way back out, not here: a source mode
    // without write or search permission (0555, say) would otherwise stop us
    // creating the very files that belong inside it, and any mode/owner/time
    // set now would be invalidated by populating it anyway.
    //
    // `open_dir_at` creates it when missing and otherwise opens what is there
    // with O_DIRECTORY|O_NOFOLLOW, so a symbolic link left in the destination
    // is refused rather than descended through.
    let dir = if member.as_os_str().is_empty() {
        tree.root().try_clone_to_owned()?
    } else {
        let Some(mp) = MemberPath::parse(member)? else {
            return Ok(());
        };
        let parent = tree.parent_of(&mp, true)?;
        open_dir_at(parent.as_fd(), &mp.leaf, true)?
    };

    // Remember what this destination directory *is*, so the walk can recognise
    // it if the source tree leads back here.
    if let Some(st) = stat_at(dir.as_fd(), c".") {
        // Casts needed: `dev_t` is i32 on macOS and u64 on Linux.
        #[allow(clippy::unnecessary_cast)]
        state.dest_ids.insert((st.st_dev as u64, st.st_ino as u64));
    }

    // Recurse into directory unless no_recurse
    if !options.no_recurse {
        #[cfg(unix)]
        let initial_dev = if options.one_file_system {
            Some(metadata.dev())
        } else {
            None
        };
        #[cfg(not(unix))]
        let initial_dev: Option<u64> = None;

        let entries = match fs::read_dir(src) {
            Ok(e) => e,
            Err(e) => {
                crate::error::report_error(src.display(), e);
                return Ok(());
            }
        };

        for entry in entries {
            let entry = match entry {
                Ok(e) => e,
                Err(e) => {
                    crate::error::report_error(src.display(), e);
                    continue;
                }
            };

            // The child's member name extends its parent's, so selection and
            // substitution see the same pathname an archive would record.
            if let Err(e) = copy_member(
                &entry.path(),
                &member.join(entry.file_name()),
                tree,
                options,
                state,
                initial_dev,
                false,
            ) {
                crate::error::report_error(entry.path().display(), e);
            }
        }
    }

    // `.` as an operand has no directory of its own to stamp: its children were
    // copied straight into the destination root.
    if member.as_os_str().is_empty() {
        return Ok(());
    }

    // Now that the subtree exists, give the directory its source attributes,
    // through the descriptor rather than by name.
    set_attrs_fd(dir.as_fd(), &attrs_of(metadata), &policy_of(options))
}

/// Recreate a special file (FIFO or device node) below `dirfd`.
///
/// FIFOs are recreated with `mkfifoat` and block/character devices with
/// `mknodat` (the latter typically requires privilege). Sockets cannot be
/// meaningfully recreated and are reported as an unsupported type. The error
/// message is context-free; the caller adds the pathname via `report_error`.
fn copy_special_file(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    metadata: &fs::Metadata,
    options: &CopyOptions,
) -> PaxResult<()> {
    use std::os::unix::fs::FileTypeExt;

    let ft = metadata.file_type();
    let perm = (metadata.mode() & 0o7777) as libc::mode_t;

    let created = if ft.is_fifo() {
        create_replacing(dirfd, name, options.no_clobber, || {
            let r = unsafe { libc::mkfifoat(dirfd.as_raw_fd(), name.as_ptr(), perm) };
            if r != 0 {
                return Err(std::io::Error::last_os_error());
            }
            Ok(())
        })?
    } else if ft.is_block_device() || ft.is_char_device() {
        let type_bits = if ft.is_block_device() {
            libc::S_IFBLK
        } else {
            libc::S_IFCHR
        };
        create_replacing(dirfd, name, options.no_clobber, || {
            let r = unsafe {
                libc::mknodat(
                    dirfd.as_raw_fd(),
                    name.as_ptr(),
                    perm | type_bits,
                    metadata.rdev() as libc::dev_t,
                )
            };
            if r != 0 {
                return Err(std::io::Error::last_os_error());
            }
            Ok(())
        })?
    } else {
        return Err(PaxError::InvalidFormat(gettextrs::gettext(
            "unsupported file type",
        )));
    };

    if !created {
        return Ok(());
    }

    // mkfifoat and mknodat both apply the process umask, so the mode they were
    // given is not necessarily the mode on disk; and neither carries ownership
    // or times. Extraction restores all three here, so a copy must too. A FIFO
    // cannot be opened for the purpose without blocking on a writer, so this is
    // the one place a name is used -- and set_permissions_at refuses a link.
    set_node_attrs_at(dirfd, name, metadata, options)
}

/// Copy a symlink
fn copy_symlink(
    src: &Path,
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    metadata: &fs::Metadata,
    options: &CopyOptions,
) -> PaxResult<()> {
    let target = fs::read_link(src)?;
    let target_c = CString::new(target.as_os_str().as_bytes())
        .map_err(|_| PaxError::InvalidHeader("link target contains null".to_string()))?;

    let created = create_replacing(dirfd, name, options.no_clobber, || {
        let r = unsafe { libc::symlinkat(target_c.as_ptr(), dirfd.as_raw_fd(), name.as_ptr()) };
        if r != 0 {
            return Err(std::io::Error::last_os_error());
        }
        Ok(())
    })?;
    if !created {
        return Ok(());
    }

    // A symlink's own mode is meaningless and there is no portable way to chmod
    // one, so only owner and times are restored.
    set_link_attrs_at(dirfd, name, &attrs_of(metadata), &policy_of(options))
}

/// Copy a regular file
#[allow(clippy::too_many_arguments)]
fn copy_file(
    src: &Path,
    tree: &DirTree,
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    member: &Path,
    options: &CopyOptions,
    state: &mut CopyState,
    metadata: &fs::Metadata,
) -> PaxResult<()> {
    // -l: link to the source rather than copying it.
    if options.link {
        let src_c = CString::new(src.as_os_str().as_bytes())
            .map_err(|_| PaxError::InvalidHeader("path contains null".to_string()))?;
        let linked = create_replacing(dirfd, name, options.no_clobber, || {
            // flags 0: link the source itself, never what it points at.
            let r = unsafe {
                libc::linkat(
                    libc::AT_FDCWD,
                    src_c.as_ptr(),
                    dirfd.as_raw_fd(),
                    name.as_ptr(),
                    0,
                )
            };
            if r != 0 {
                return Err(std::io::Error::last_os_error());
            }
            Ok(())
        });
        match linked {
            Ok(_) => return Ok(()),
            Err(e) => {
                // Hard link failed (maybe cross-device), fall back to copy
                eprintln!("pax: hard link failed, copying: {}: {}", src.display(), e);
            }
        }
    }

    // A second name for a file already copied becomes a link to that copy.
    if let Some(link_target) = state.link_tracker.check_ids(
        metadata.dev(),
        metadata.ino(),
        metadata.nlink() as u32,
        member,
    ) {
        let target_c = CString::new(link_target.as_os_str().as_bytes())
            .map_err(|_| PaxError::InvalidHeader("path contains null".to_string()))?;
        create_replacing(dirfd, name, options.no_clobber, || {
            // The first copy's name is recorded relative to the destination
            // root, so it resolves from the root's descriptor. flags 0: link
            // that file itself, never anything it might point at.
            let r = unsafe {
                libc::linkat(
                    tree.root().as_raw_fd(),
                    target_c.as_ptr(),
                    dirfd.as_raw_fd(),
                    name.as_ptr(),
                    0,
                )
            };
            if r != 0 {
                return Err(std::io::Error::last_os_error());
            }
            Ok(())
        })?;
        return Ok(());
    }

    do_copy_file(src, dirfd, name, metadata, options)
}

/// Actually copy file contents
fn do_copy_file(
    src: &Path,
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    metadata: &fs::Metadata,
    options: &CopyOptions,
) -> PaxResult<()> {
    let mut src_file = File::open(src)?;

    // O_EXCL|O_NOFOLLOW, retried once after unlinking whatever is in the way:
    // the destination is always a freshly created file, never a write *through*
    // a name someone else put there. `exists()` used to stand in for this, and
    // it follows symbolic links -- so a *dangling* one read as "nothing here",
    // nothing was removed, and the create followed it out of the tree.
    let flags = libc::O_WRONLY | libc::O_CREAT | libc::O_EXCL | libc::O_NOFOLLOW | libc::O_CLOEXEC;
    let mut opened: Option<File> = None;
    let created = create_replacing(dirfd, name, options.no_clobber, || {
        let fd = unsafe {
            libc::openat(
                dirfd.as_raw_fd(),
                name.as_ptr(),
                flags,
                (metadata.mode() & 0o777) as libc::c_uint,
            )
        };
        if fd < 0 {
            return Err(std::io::Error::last_os_error());
        }
        opened = Some(unsafe { File::from_raw_fd(fd) });
        Ok(())
    })?;

    let Some(mut dest_file) = opened else {
        debug_assert!(!created);
        return Ok(());
    };

    let mut buf = [0u8; 8192];
    loop {
        let n = src_file.read(&mut buf)?;
        if n == 0 {
            break;
        }
        dest_file.write_all(&buf[..n])?;
    }

    set_attrs_fd(dest_file.as_fd(), &attrs_of(metadata), &policy_of(options))
}

/// A source file's attributes, in the shape the anchored helpers take.
fn attrs_of(metadata: &fs::Metadata) -> Attrs {
    Attrs {
        mode: metadata.mode() & 0o7777,
        uid: metadata.uid(),
        gid: metadata.gid(),
        mtime: metadata.mtime(),
        mtime_nsec: metadata.mtime_nsec(),
        atime: Some(metadata.atime()),
        atime_nsec: metadata.atime_nsec(),
    }
}

/// What `-p` asked to keep, in the shape the anchored helpers take.
fn policy_of(options: &CopyOptions) -> AttrPolicy {
    AttrPolicy {
        // A copy takes ownership from the source only when asked; otherwise the
        // new file belongs to whoever ran pax.
        preserve_owner: options.preserve_owner,
        preserve_perms: options.preserve_perms,
        preserve_mtime: options.preserve_mtime,
        preserve_atime: options.preserve_atime,
        umask: options.umask,
    }
}

/// Owner, mode and times for a node that cannot be opened for the purpose.
fn set_node_attrs_at(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    metadata: &fs::Metadata,
    options: &CopyOptions,
) -> PaxResult<()> {
    let attrs = attrs_of(metadata);
    let policy = policy_of(options);

    // Owner and times take AT_SYMLINK_NOFOLLOW; the mode check is in
    // set_permissions_at, which refuses a name that is a symbolic link.
    set_link_attrs_at(dirfd, name, &attrs, &policy)?;

    let Some(st) = stat_at(dirfd, name) else {
        return Err(std::io::Error::last_os_error().into());
    };
    if st.st_mode & libc::S_IFMT == libc::S_IFLNK {
        return Err(PaxError::InvalidHeader(
            "refusing to set permissions through a symbolic link".to_string(),
        ));
    }
    let r = unsafe {
        libc::fchmodat(
            dirfd.as_raw_fd(),
            name.as_ptr(),
            policy.mode(&attrs) as libc::mode_t,
            0,
        )
    };
    if r != 0 {
        return Err(std::io::Error::last_os_error().into());
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use plib::tmp::TempDir;
    use std::fs;

    #[test]
    fn test_copy_file() {
        let src_dir = TempDir::new().unwrap();
        let dest_dir = TempDir::new().unwrap();

        // Create source file
        let src_file = src_dir.path().join("test.txt");
        fs::write(&src_file, "hello world").unwrap();

        let options = CopyOptions {
            preserve_perms: true,
            preserve_mtime: true,
            ..Default::default()
        };

        copy_files(std::slice::from_ref(&src_file), dest_dir.path(), &options).unwrap();

        // An absolute operand is stored under its path with the leading slash
        // removed, exactly as an archive would record it, so that is where it
        // is restored beneath the destination.
        let dest_file = dest_dir.path().join(member_name(&src_file));
        assert!(dest_file.exists());
        assert_eq!(fs::read_to_string(&dest_file).unwrap(), "hello world");
    }

    #[test]
    fn test_copy_directory() {
        let src_dir = TempDir::new().unwrap();
        let dest_dir = TempDir::new().unwrap();

        // Create source directory with files
        let subdir = src_dir.path().join("subdir");
        fs::create_dir(&subdir).unwrap();
        fs::write(subdir.join("file1.txt"), "content1").unwrap();
        fs::write(subdir.join("file2.txt"), "content2").unwrap();

        let options = CopyOptions::default();

        copy_files(std::slice::from_ref(&subdir), dest_dir.path(), &options).unwrap();

        let copied_subdir = dest_dir.path().join(member_name(&subdir));
        assert!(copied_subdir.is_dir());
        assert_eq!(
            fs::read_to_string(copied_subdir.join("file1.txt")).unwrap(),
            "content1"
        );
        assert_eq!(
            fs::read_to_string(copied_subdir.join("file2.txt")).unwrap(),
            "content2"
        );
    }

    #[test]
    fn test_no_clobber() {
        let src_dir = TempDir::new().unwrap();
        let dest_dir = TempDir::new().unwrap();

        // Create source file
        let src_file = src_dir.path().join("test.txt");
        fs::write(&src_file, "new content").unwrap();

        // Create existing dest file at the member path the copy will target.
        let dest_file = dest_dir.path().join(member_name(&src_file));
        fs::create_dir_all(dest_file.parent().unwrap()).unwrap();
        fs::write(&dest_file, "existing content").unwrap();

        let options = CopyOptions {
            no_clobber: true,
            ..Default::default()
        };

        copy_files(&[src_file], dest_dir.path(), &options).unwrap();

        // Destination should still have original content
        assert_eq!(fs::read_to_string(&dest_file).unwrap(), "existing content");
    }

    #[cfg(unix)]
    #[test]
    fn test_copy_symlink() {
        let src_dir = TempDir::new().unwrap();
        let dest_dir = TempDir::new().unwrap();

        // Create source file and symlink
        let src_file = src_dir.path().join("target.txt");
        fs::write(&src_file, "target content").unwrap();

        let src_link = src_dir.path().join("link.txt");
        std::os::unix::fs::symlink("target.txt", &src_link).unwrap();

        let options = CopyOptions::default();

        copy_files(std::slice::from_ref(&src_link), dest_dir.path(), &options).unwrap();

        let dest_link = dest_dir.path().join(member_name(&src_link));
        assert!(dest_link.symlink_metadata().unwrap().is_symlink());
        assert_eq!(
            fs::read_link(&dest_link).unwrap().to_str().unwrap(),
            "target.txt"
        );
    }
}
