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
use std::cell::RefCell;
use std::collections::HashSet;
use std::ffi::{CStr, CString};
use std::fs::File;
use std::io::{Read, Write};
use std::os::fd::OwnedFd;
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

    let walk = CopyWalk {
        tree: &tree,
        options,
        link_tracker: RefCell::new(HardLinkTracker::new()),
        dest_ids: RefCell::new(HashSet::new()),
        prompter: RefCell::new(if options.interactive {
            Some(InteractivePrompter::new()?)
        } else {
            None
        }),
        dest_stack: RefCell::new(Vec::new()),
        member_stack: RefCell::new(Vec::new()),
        dev_stack: RefCell::new(Vec::new()),
        fatal: RefCell::new(None),
    };
    if let Some(st) = stat_at(tree.root(), c".") {
        // Casts needed: `dev_t` is i32 on macOS and u64 on Linux.
        #[allow(clippy::unnecessary_cast)]
        walk.dest_ids
            .borrow_mut()
            .insert((st.st_dev as u64, st.st_ino as u64));
    }

    for path in files {
        let _ = ftw::traverse_directory(
            path,
            |entry| walk.visit(entry),
            |_, _| walk.leave_directory(),
            |entry, err| crate::error::report_error(entry.path().as_inner(), err.inner()),
            ftw::TraverseDirectoryOpts {
                follow_symlinks_on_args: options.cli_dereference,
                follow_symlinks: options.dereference,
                // One destination-directory descriptor is held per source
                // level, so that postprocess_dir can stamp it.
                caller_fds_per_level: 1,
                ..Default::default()
            },
        );

        if let Some(e) = walk.fatal.borrow_mut().take() {
            return Err(e);
        }
        // Each operand starts its own member naming.
        walk.member_stack.borrow_mut().clear();
        walk.dest_stack.borrow_mut().clear();
        walk.dev_stack.borrow_mut().clear();
    }

    Ok(())
}

/// State the three traversal callbacks share.
///
/// The destination side is untouched by this: every leaf is still resolved
/// with `MemberPath::parse` and `DirTree::parent_of` from the anchor, because
/// `-s` can rewrite a member to a path that is not under the current
/// destination directory at all. `dest_stack` exists only to hold each created
/// directory's descriptor so its attributes can be applied on the way out.
struct CopyWalk<'a> {
    tree: &'a DirTree,
    options: &'a CopyOptions,
    link_tracker: RefCell<HardLinkTracker>,
    /// `(st_dev, st_ino)` of every destination directory this copy has created
    /// or entered. A source directory found in here is one being copied *into*.
    dest_ids: RefCell<HashSet<(u64, u64)>>,
    prompter: RefCell<Option<InteractivePrompter>>,
    /// Per descended directory: its destination descriptor and the source
    /// attributes to stamp on it, or `None` for the `.` operand, whose
    /// children go straight into the destination root.
    dest_stack: RefCell<Vec<Option<(OwnedFd, Attrs)>>>,
    /// Member names, built by joining as the walk descends rather than derived
    /// from the filesystem path, so selection and substitution see the name an
    /// archive would record.
    member_stack: RefCell<Vec<PathBuf>>,
    /// `st_dev` of each directory descended into, for `-X`.
    dev_stack: RefCell<Vec<u64>>,
    fatal: RefCell<Option<PaxError>>,
}

impl CopyWalk<'_> {
    /// The member name for `entry`: the operand's own name at the root of a
    /// traversal, and the parent's name joined with this component below it.
    fn member_for(&self, entry: &ftw::Entry<'_>) -> PathBuf {
        match self.member_stack.borrow().last() {
            Some(parent) => {
                let name = crate::rawpath::from_bytes(entry.file_name().to_bytes());
                parent.join(name)
            }
            None => member_name(entry.path().as_inner()),
        }
    }

    fn visit(&self, entry: ftw::Entry<'_>) -> Result<bool, ()> {
        if self.fatal.borrow().is_some() {
            return Ok(false);
        }
        let path = entry.path();
        let src = path.as_inner();
        let member = self.member_for(&entry);

        match self.copy_one(&entry, src, member) {
            Ok(descend) => Ok(descend),
            Err(e) if crate::modes::is_fatal(&e) => {
                *self.fatal.borrow_mut() = Some(e);
                Ok(false)
            }
            Err(e) => {
                crate::error::report_error(src, e);
                Ok(false)
            }
        }
    }

    /// Apply a descended directory's source attributes, now that its contents
    /// exist.
    ///
    /// Runs for `DirExit::NotDescended` as well: if reading the source
    /// directory failed, the destination directory still exists and still
    /// wants its mode. The old code returned early on that path and left it
    /// with the creation mode.
    fn leave_directory(&self) -> Result<(), ()> {
        // Taken before the pop: a failure here has to name the directory it
        // was about, and this is the only place that still knows.
        let member = self.member_stack.borrow().last().cloned();
        self.member_stack.borrow_mut().pop();
        self.dev_stack.borrow_mut().pop();
        if let Some(Some((dir, attrs))) = self.dest_stack.borrow_mut().pop() {
            if let Err(e) = set_attrs_fd(dir.as_fd(), &attrs, &policy_of(self.options)) {
                match member {
                    Some(ref m) => crate::error::report_error(m, e),
                    None => crate::error::report_error("destination directory", e),
                }
            }
        }
        Ok(())
    }

    fn copy_one(&self, entry: &ftw::Entry<'_>, src: &Path, member: PathBuf) -> PaxResult<bool> {
        let Some(metadata) = entry.metadata() else {
            return Ok(false);
        };

        // -L, and -H on an operand, ask for the target, not the link; ftw
        // falls back to the link's own metadata when the target cannot be
        // stat'ed. See the matching note in write mode.
        let at_operand = self.member_stack.borrow().is_empty();
        let asked_to_follow =
            self.options.dereference || (self.options.cli_dereference && at_operand);
        if asked_to_follow && entry.is_symlink() == Some(true) && metadata.is_symlink() {
            crate::error::report_error(src, std::io::Error::from_raw_os_error(libc::ENOENT));
            return Ok(false);
        }

        // Selection and substitution both act on the member name, so they
        // reach every file in the subtree rather than only the operands.
        if !self.options.patterns.is_empty() {
            let name = crate::rawpath::MatchName::of(&member);
            let matches = matches_any(&self.options.patterns, name.as_str());
            if self.options.exclude == matches {
                return Ok(false);
            }
        }

        if self.options.one_file_system {
            if let Some(&parent_dev) = self.dev_stack.borrow().last() {
                if metadata.dev() != parent_dev {
                    return Ok(false);
                }
            }
        }

        // -s applies before -i (POSIX: the order of -o, -p and -s is
        // significant).
        let member = if self.options.substitutions.is_empty() {
            member
        } else {
            match apply_substitutions(&self.options.substitutions, &member) {
                SubstResult::Unchanged => member,
                SubstResult::Changed(new_name) => crate::rawpath::from_substituted(&new_name),
                SubstResult::Empty => return Ok(false), // a null name means skip
            }
        };

        let member = {
            let mut prompter = self.prompter.borrow_mut();
            if let Some(ref mut p) = *prompter {
                match p.prompt(&member)? {
                    RenameResult::Skip => return Ok(false),
                    RenameResult::UseOriginal => member,
                    RenameResult::Rename(new_name) => new_name,
                }
            } else {
                member
            }
        };

        // A source directory that *is* one of this copy's destinations is one
        // being copied into. Following it walks the copy's own output back
        // into itself until the pathname runs out of room; identity cannot be
        // spelled two ways, where a path comparison could be defeated by any
        // other spelling.
        if metadata.is_dir()
            && self
                .dest_ids
                .borrow()
                .contains(&(metadata.dev(), metadata.ino()))
        {
            // The path is not repeated in the message: `visit` reports this
            // against `src`, byte-accurately, as the diagnostic's subject.
            // Interpolating `src.display()` here both duplicated it and
            // reintroduced the lossy rendering the rest of this branch removed.
            return Err(PaxError::InvalidFormat(
                "cannot copy directory into itself".to_string(),
            ));
        }

        if metadata.is_dir() {
            return self.enter_directory(src, member, metadata);
        }

        let Some(mp) = MemberPath::parse(&member)? else {
            return Ok(false);
        };

        // A member may name directories the walk has not created yet (`a/b/c`
        // given as an operand). POSIX requires the intermediate directories be
        // made with the normal file-creation action.
        let parent = self.tree.parent_of(&mp, true)?;
        let pfd = parent.as_fd();
        let name = mp.leaf.as_c_str();

        let existing = stat_at(pfd, name);
        if self.options.no_clobber && existing.is_some() {
            return Ok(false);
        }
        if self.options.update && !is_source_newer(metadata, existing.as_ref()) {
            return Ok(false);
        }

        if self.options.verbose {
            let mut line = Vec::new();
            crate::escape::push_escaped(
                &mut line,
                crate::rawpath::as_bytes(src),
                crate::escape::stderr_style(),
            );
            line.push(b'\n');
            let _ = std::io::Write::write_all(&mut std::io::stderr().lock(), &line);
        }

        if metadata.is_symlink() {
            // ftw has already done the readlinkat from the descriptor of the
            // directory the link was found in.
            let target = entry
                .read_link()
                .map(|t| crate::rawpath::from_bytes(t.to_bytes()))
                .ok_or_else(|| {
                    PaxError::InvalidHeader("symbolic link with no target".to_string())
                })?;
            copy_symlink(target, pfd, name, metadata, self.options)?;
        } else if metadata.is_file() {
            copy_file(
                entry,
                self.tree,
                pfd,
                name,
                &mp.display,
                self.options,
                &mut self.link_tracker.borrow_mut(),
                metadata,
            )?;
        } else if let Err(e) = copy_special_file(pfd, name, metadata, self.options) {
            crate::error::report_error(src, e);
        }

        Ok(false)
    }

    /// Create the destination directory and arrange for its attributes to be
    /// applied once its contents exist.
    ///
    /// Not here: a source mode without write or search permission (0555, say)
    /// would stop us creating the very files that belong inside it, and any
    /// mode, owner or time set now would be invalidated by populating it
    /// anyway. `leave_directory` does it on the way out.
    fn enter_directory(
        &self,
        src: &Path,
        member: PathBuf,
        metadata: &ftw::Metadata,
    ) -> PaxResult<bool> {
        // `open_dir_at` creates it when missing and otherwise opens what is
        // there with O_DIRECTORY|O_NOFOLLOW, so a symbolic link left in the
        // destination is refused rather than descended through.
        let dir = if member.as_os_str().is_empty() {
            self.tree.root().try_clone_to_owned()?
        } else {
            let Some(mp) = MemberPath::parse(&member)? else {
                return Ok(false);
            };
            let parent = self.tree.parent_of(&mp, true)?;
            open_dir_at(parent.as_fd(), &mp.leaf, true)?
        };

        // Remember what this destination directory *is*, so the walk can
        // recognise it if the source tree leads back here.
        if let Some(st) = stat_at(dir.as_fd(), c".") {
            // Casts needed: `dev_t` is i32 on macOS and u64 on Linux.
            #[allow(clippy::unnecessary_cast)]
            self.dest_ids
                .borrow_mut()
                .insert((st.st_dev as u64, st.st_ino as u64));
        }

        if self.options.verbose {
            let mut line = Vec::new();
            crate::escape::push_escaped(
                &mut line,
                crate::rawpath::as_bytes(src),
                crate::escape::stderr_style(),
            );
            line.push(b'\n');
            let _ = std::io::Write::write_all(&mut std::io::stderr().lock(), &line);
        }

        // `.` as an operand has no directory of its own to stamp: its children
        // are copied straight into the destination root.
        let pending = if member.as_os_str().is_empty() {
            None
        } else {
            Some((dir, attrs_of(metadata)))
        };

        if self.options.no_recurse {
            // No postprocess_dir will fire, so stamp it now.
            if let Some((dir, attrs)) = pending {
                set_attrs_fd(dir.as_fd(), &attrs, &policy_of(self.options))?;
            }
            return Ok(false);
        }

        self.dest_stack.borrow_mut().push(pending);
        self.member_stack.borrow_mut().push(member);
        self.dev_stack.borrow_mut().push(metadata.dev());
        Ok(true)
    }
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

/// Check if source is newer than the destination that is already there (`-u`).
fn is_source_newer(src_metadata: &ftw::Metadata, dest: Option<&libc::stat>) -> bool {
    // If destination doesn't exist, always copy
    let Some(dest) = dest else {
        return true;
    };
    src_metadata.mtime() > dest.st_mtime
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
    metadata: &ftw::Metadata,
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
    target: PathBuf,
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    metadata: &ftw::Metadata,
    options: &CopyOptions,
) -> PaxResult<()> {
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
    entry: &ftw::Entry<'_>,
    tree: &DirTree,
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    member: &Path,
    options: &CopyOptions,
    link_tracker: &mut HardLinkTracker,
    metadata: &ftw::Metadata,
) -> PaxResult<()> {
    let src_path = entry.path();
    let src = src_path.as_inner();

    // -l: link to the source rather than copying it.
    if options.link {
        let linked = create_replacing(dirfd, name, options.no_clobber, || {
            // From the descriptor of the directory the walk found it in, not
            // by re-resolving the whole source path. flags 0: link the source
            // itself, never what it points at.
            let r = unsafe {
                libc::linkat(
                    entry.dir_fd(),
                    entry.file_name().as_ptr(),
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
                crate::error::report_error(src, format!("hard link failed, copying: {e}"));
            }
        }
    }

    // A second name for a file already copied becomes a link to that copy.
    //
    // `member` is the name the first copy was actually created under -- the
    // parsed one, with any leading `/` and `..` already removed. Recording the
    // raw name instead let a `-s` rename to an absolute path be handed to
    // `linkat`, which resolves an absolute path from the root of the filesystem
    // and ignores the anchor descriptor entirely.
    if let Some(link_target) = link_tracker.check_ids(
        metadata.dev(),
        metadata.ino(),
        metadata.nlink() as u32,
        member,
    ) {
        let Some(target) = MemberPath::parse(&link_target)? else {
            return do_copy_file(entry, dirfd, name, metadata, options);
        };
        let target_dir = tree.parent_of(&target, false)?;
        create_replacing(dirfd, name, options.no_clobber, || {
            // Resolved one component at a time from the destination anchor, the
            // same way the file itself was created. flags 0: link that file
            // itself, never anything it might point at.
            let r = unsafe {
                libc::linkat(
                    target_dir.as_raw_fd(),
                    target.leaf.as_ptr(),
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

    do_copy_file(entry, dirfd, name, metadata, options)
}

/// Actually copy file contents
fn do_copy_file(
    entry: &ftw::Entry<'_>,
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    metadata: &ftw::Metadata,
    options: &CopyOptions,
) -> PaxResult<()> {
    // From the descriptor of the directory the walk found it in, and re-checked
    // against the (dev, ino) the walk saw, rather than re-resolving the whole
    // source path. Whether the walk dereferenced this entry is observable from
    // the entry itself, so -H/-L stays decided in the traversal options.
    let followed = entry.is_symlink() == Some(true) && !metadata.is_symlink();
    let mut src_file = crate::modes::anchored::open_source_file(
        entry.dir_fd(),
        entry.file_name(),
        followed,
        (metadata.dev(), metadata.ino()),
    )?;

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
                policy_of(options).creation_mode(&attrs_of(metadata)) as libc::c_uint,
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
fn attrs_of(metadata: &ftw::Metadata) -> Attrs {
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
    metadata: &ftw::Metadata,
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
