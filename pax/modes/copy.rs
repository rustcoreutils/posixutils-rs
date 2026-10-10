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
    attrs_withheld, create_replacing, file_id, link_replacing_with, make_dir_at, restore_atime,
    restore_dir_atime, set_attrs_fd, set_made_node_attrs, AttrPolicy, Attrs, DirAttrs, DirTree,
    Expected, MemberPath, PendingDirs,
};
use crate::modes::followed_link;
use crate::modes::pins::{MadeFile, PinBudget};
use crate::modes::write::FileNames;
use crate::subst::{substitute_name, Substitution};
use plib::madefs::lstat_at;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::ffi::{CStr, CString};
use std::fs::File;
use std::io::{Read, Write};
use std::os::fd::{AsFd, AsRawFd, BorrowedFd, FromRawFd};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::MetadataExt;
use std::path::{Path, PathBuf};

/// Options for copy mode
#[derive(Default)]
pub struct CopyOptions {
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
    /// Put back the access time of each source file and directory read (-t)
    pub reset_atime: bool,
    /// Path substitutions (-s option)
    pub substitutions: Vec<Substitution>,
    /// Process file-creation mask, applied to the mode of copied files when the
    /// mode is not explicitly preserved (no `-p p`/`-p e`).
    pub umask: u32,
}

/// Copy files to a destination directory
pub fn copy_files(
    files: &mut FileNames<'_>,
    dest_dir: &Path,
    options: &CopyOptions,
) -> PaxResult<()> {
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
    let preserving = options.preserve_perms || options.preserve_owner;
    let tree = DirTree::open_dest(dest_dir, preserving)?;

    let walk = CopyWalk {
        tree: &tree,
        options,
        link_tracker: RefCell::new(CopiedLinks::new()),
        dest_ids: RefCell::new(HashSet::new()),
        made_files: RefCell::new(HashMap::new()),
        prompter: RefCell::new(if options.interactive {
            Some(InteractivePrompter::new()?)
        } else {
            None
        }),
        pending_dirs: RefCell::new(PendingDirs::default()),
        member_stack: RefCell::new(Vec::new()),
        dev_stack: RefCell::new(Vec::new()),
        fatal: RefCell::new(None),
    };
    if let Ok(st) = lstat_at(tree.root().as_raw_fd(), c".") {
        walk.dest_ids.borrow_mut().insert(file_id(&st));
    }

    // Directories take their attributes once everything has been copied --
    // and still do when a fatal error ends the copy early.
    let result = walk.copy_operands(files);
    walk.pending_dirs
        .borrow_mut()
        .apply(&tree, &policy_of(options));
    result
}

impl CopyWalk<'_> {
    /// Walk each operand in turn, stopping at the first fatal error.
    fn copy_operands(&self, files: &mut FileNames<'_>) -> PaxResult<()> {
        for path in files {
            self.copy_operand(&path)?;
        }
        Ok(())
    }

    fn copy_operand(&self, path: &Path) -> PaxResult<()> {
        let options = self.options;
        let _ = ftw::traverse_directory(
            path,
            |entry| self.visit(entry),
            |entry, exit| self.leave_directory(&entry, exit),
            |entry, err| crate::error::report_error(entry.path().as_inner(), err.inner()),
            ftw::TraverseDirectoryOpts {
                follow_symlinks_on_args: options.cli_dereference,
                follow_symlinks: options.dereference,
                // The destination tree keeps a descriptor open per level it
                // has walked (see `DirTree`), which the walk has to leave
                // room for.
                caller_fds_per_level: 1,
                ..Default::default()
            },
        );

        if let Some(e) = self.fatal.borrow_mut().take() {
            return Err(e);
        }
        // Each operand starts its own member naming.
        self.member_stack.borrow_mut().clear();
        self.dev_stack.borrow_mut().clear();
        Ok(())
    }
}

/// The first copy of each multiply-linked file: the member path it was made
/// at, and the copy made there (`MadeFile`, pinned within the budget).
struct CopiedLinks {
    names: HardLinkTracker<(PathBuf, MadeFile)>,
    pins: PinBudget<(u64, u64)>,
}

impl CopiedLinks {
    fn new() -> Self {
        CopiedLinks {
            names: HardLinkTracker::new(),
            pins: PinBudget::new(),
        }
    }

    /// Record the first copy of the source file `(dev, ino)`, `made` at
    /// `member`, within the budget of pins. Nothing is recorded where -k
    /// kept a file already at the name (`made` is `None`) -- found there
    /// before the copy, or at the moment it was made: its later names are
    /// then copied themselves, not linked to that file.
    fn record(
        &mut self,
        (dev, ino): (u64, u64),
        nlink: u32,
        member: &Path,
        made: Option<MadeFile>,
    ) {
        // A file with one name has no later name to be linked to it.
        let Some(made) = made.filter(|_| nlink > 1) else {
            return;
        };
        let pinned = made.is_pinned();
        self.names
            .record(dev, ino, nlink, (member.to_path_buf(), made));
        let names = &mut self.names;
        self.pins.note((dev, ino), pinned, |key| {
            if let Some((_, made)) = names.by_key_mut(*key) {
                made.unpin();
            }
        });
    }
}

/// State the three traversal callbacks share.
///
/// The destination side is untouched by this: every leaf is still resolved
/// with `MemberPath::parse` and `DirTree::parent_of` from the anchor, because
/// `-s` can rewrite a member to a path that is not under the current
/// destination directory at all. `DirTree` keeps the directories it last
/// walked through open, so following the depth-first walk costs a directory
/// per member rather than a walk from the anchor.
struct CopyWalk<'a> {
    tree: &'a DirTree,
    options: &'a CopyOptions,
    link_tracker: RefCell<CopiedLinks>,
    /// `(st_dev, st_ino)` of every destination directory this copy has created
    /// or entered, and of every directory that maps onto itself. A source
    /// directory found in here is one being copied *into* -- unless this copy
    /// made it (`DirTree::made_by_run`).
    dest_ids: RefCell<HashSet<(u64, u64)>>,
    /// The names of the files this copy has made under -s or -i, by the
    /// `(st_dev, st_ino)` of the directory they were made in. -s and -i can
    /// put a file inside a directory the walk has still to read -- the very
    /// one being read, when that maps onto itself -- and the walk then meets
    /// the copy's own output, which is not copied again. A name, not the
    /// file's identity: under -l the file made is the source itself, linked,
    /// and another name the source already had is still to be copied.
    made_files: RefCell<HashMap<(u64, u64), HashSet<CString>>>,
    prompter: RefCell<Option<InteractivePrompter>>,
    /// Destination directories still to take their source attributes, which
    /// wait until everything -- not only the walk below them, but any later
    /// operand naming a file inside -- has been copied: a read-only mode
    /// applied any sooner refuses the rest of its contents.
    pending_dirs: RefCell<PendingDirs>,
    /// Member names, built by joining as the walk descends rather than derived
    /// from the filesystem path, so substitution sees the name an archive
    /// would record. These are the names *before* -s and -i: a copy is an
    /// archive round trip, so each name is substituted once, by itself --
    /// building a child's name from its parent's substituted one applied the
    /// substitution again at every level.
    member_stack: RefCell<Vec<PathBuf>>,
    /// `st_dev` of each directory descended into; the first is the operand's,
    /// which `-X` compares against.
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

    /// Leave a descended source directory. It has been read by now, so this
    /// is where -t puts back its access time.
    fn leave_directory(&self, entry: &ftw::Entry<'_>, exit: ftw::DirExit) -> Result<(), ()> {
        if self.options.reset_atime && exit == ftw::DirExit::Descended {
            restore_dir_atime(entry);
        }
        self.member_stack.borrow_mut().pop();
        self.dev_stack.borrow_mut().pop();
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

        // A source directory that *is* one of this copy's destinations is one
        // being copied into. Following it walks the copy's own output back
        // into itself until the pathname runs out of room; identity cannot be
        // spelled two ways, where a path comparison could be defeated by any
        // other spelling.
        //
        // A source this copy made is its own output, met again by a walk that
        // is still reading the directory -s or -i put it in: a file
        // (`made_here`), or a directory, made for a member or only to hold
        // members below it. Walked, such a directory is copied again under
        // the same substitution, one level deeper each time, without end.
        if self.made_here(entry, metadata) {
            return Ok(false);
        }
        let id = (metadata.dev(), metadata.ino());
        if metadata.is_dir() && self.tree.made_by_run(id) {
            return Ok(false);
        }
        if metadata.is_dir() && self.dest_ids.borrow().contains(&id) {
            // The path is not repeated in the message: `visit` reports this
            // against `src`, byte-accurately, as the diagnostic's subject.
            // Interpolating `src.display()` here both duplicated it and
            // reintroduced the lossy rendering the rest of this branch removed.
            return Err(PaxError::InvalidFormat(
                "cannot copy directory into itself".to_string(),
            ));
        }

        // -s applies before -i (POSIX: the order of -o, -p and -s is
        // significant). A name that becomes empty is ignored -- that name
        // only: a directory's descendants are still copied, each under its
        // own substitution, as they would be through an archive.
        let Some(dest) = substitute_name(&self.options.substitutions, &member) else {
            return if metadata.is_dir() {
                self.descend(member, metadata)
            } else {
                Ok(false)
            };
        };

        let dest = {
            let mut prompter = self.prompter.borrow_mut();
            if let Some(ref mut p) = *prompter {
                match p.prompt(&dest)? {
                    // POSIX: "the file ... shall be skipped" -- that name
                    // alone, as with an empty -s replacement.
                    RenameResult::Skip if metadata.is_dir() => {
                        return self.descend(member, metadata)
                    }
                    RenameResult::Skip => return Ok(false),
                    RenameResult::UseOriginal => dest,
                    RenameResult::Rename(new_name) => new_name,
                }
            } else {
                dest
            }
        };

        if metadata.is_dir() {
            return self.enter_directory(entry, member, &dest, metadata);
        }

        let Some(mp) = MemberPath::parse(&dest)? else {
            return Ok(false);
        };

        // A member may name directories the walk has not created yet (`a/b/c`
        // given as an operand). POSIX requires the intermediate directories be
        // made with the normal file-creation action.
        let parent = self.tree.parent_of(&mp, true)?;
        let pfd = parent.as_fd();
        let name = mp.leaf.as_c_str();

        let existing = lstat_at(pfd.as_raw_fd(), name).ok();
        if self.options.no_clobber && existing.is_some() {
            return Ok(false);
        }
        if self.options.update && !self.is_source_newer(metadata, existing.as_ref()) {
            return Ok(false);
        }
        // The destination name already *is* the source (`pax -rw tree .`):
        // replacing it would rewrite the file from itself and split its links.
        if existing.is_some_and(|st| is_source(&st, entry, metadata)) && !self.options.link {
            return overwrites_itself(src);
        }

        self.print_verbose(src);

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
            return Ok(false);
        }
        self.record_made_file(pfd, name);

        Ok(false)
    }

    /// Under -s or -i, record `name` in `dirfd` as a file this copy made
    /// (`made_files`). Only a renamed member can land in a directory the walk
    /// has still to read; without -s or -i every member lands at its source's
    /// own name below the destination, which the walk never enters
    /// (`dest_ids`).
    fn record_made_file(&self, dirfd: BorrowedFd<'_>, name: &CStr) {
        if self.options.substitutions.is_empty() && !self.options.interactive {
            return;
        }
        if let Ok(dir) = lstat_at(dirfd.as_raw_fd(), c".") {
            self.made_files
                .borrow_mut()
                .entry(file_id(&dir))
                .or_default()
                .insert(name.to_owned());
        }
    }

    /// Whether the non-directory `entry` is a file this copy made, at the name
    /// and in the directory the walk found it.
    fn made_here(&self, entry: &ftw::Entry<'_>, metadata: &ftw::Metadata) -> bool {
        let made = self.made_files.borrow();
        if made.is_empty() || metadata.is_dir() {
            return false;
        }
        // SAFETY: the walk keeps the entry's directory open while it is visited.
        let dir = unsafe { BorrowedFd::borrow_raw(entry.dir_fd()) };
        lstat_at(dir.as_raw_fd(), c".")
            .ok()
            .and_then(|st| made.get(&file_id(&st)))
            .is_some_and(|names| names.contains(entry.file_name()))
    }

    /// Create the destination directory and arrange for its attributes to be
    /// applied once its contents exist.
    ///
    /// Not here: a source mode without write or search permission (0555, say)
    /// would stop us creating the very files that belong inside it, and any
    /// mode, owner or time set now would be invalidated by populating it
    /// anyway. `pending_dirs` applies them once the copy is done.
    ///
    /// -k and -u treat a directory as they do any file: one already there --
    /// unless this copy made it only to hold earlier names -- keeps its own
    /// attributes, under -u if it is not older than the source. Its contents
    /// are still copied, each subject to the same test.
    fn enter_directory(
        &self,
        entry: &ftw::Entry<'_>,
        member: PathBuf,
        dest: &Path,
        metadata: &ftw::Metadata,
    ) -> PaxResult<bool> {
        let src_path = entry.path();
        let src = src_path.as_inner();
        // `.` as an operand, or a -s result naming it (or nothing below the
        // destination at all), has no directory of its own to stamp. Its
        // children are still copied, each under its own name, as they would
        // be extracted from an archive.
        let Some(mp) = MemberPath::parse(dest)? else {
            self.print_verbose(src);
            return self.descend(member, metadata);
        };
        let parent = self.tree.parent_of(&mp, true)?;
        let existing = lstat_at(parent.as_raw_fd(), &mp.leaf).ok();
        if existing.is_some_and(|st| is_source(&st, entry, metadata)) {
            return self.dir_onto_itself(src, member, metadata);
        }
        let keep = existing.is_some_and(|st| self.keeps_existing_dir(metadata, &st, &mp));
        // Created no more open than its source, and reopened with
        // O_DIRECTORY|O_NOFOLLOW, so a symbolic link left in the destination
        // is refused rather than descended through.
        //
        // The directory it is to be: one just made is identified from a
        // descriptor checked to be the one made (`make_dir_at`), one kept by
        // the `lstat` that decided to keep it. The descriptor opened here
        // must be that directory, or something was renamed over it.
        let decided = match existing {
            // -k or -u keeps it, as it is.
            Some(_) if keep => DirAttrs::Keep,
            _ => make_dir_at(self.tree, parent.as_fd(), &mp, metadata.mode(), false)?,
        };
        let expected = match decided {
            DirAttrs::Apply(id) | DirAttrs::Withheld(id) => Some(id),
            DirAttrs::Keep => existing.map(|st| file_id(&st)),
        };
        let dir = self.tree.open_dir(parent.as_fd(), &mp.leaf, false)?;
        let dest_st = lstat_at(dir.as_raw_fd(), c".").map_err(PaxError::Io)?;
        if expected.is_some_and(|id| id != file_id(&dest_st)) {
            return Err(PaxError::Io(std::io::Error::other(
                "directory was replaced after it was checked",
            )));
        }

        // Remember what this destination directory *is*, so the walk can
        // recognise it if the source tree leads back here.
        self.dest_ids.borrow_mut().insert(file_id(&dest_st));

        self.print_verbose(src);

        match decided {
            DirAttrs::Apply(id) => {
                self.pending_dirs
                    .borrow_mut()
                    .push(&mp, id, attrs_of(metadata));
            }
            // Copied into all the same.
            DirAttrs::Withheld(_) => crate::error::report_error(src, attrs_withheld()),
            DirAttrs::Keep => {}
        }
        self.descend(member, metadata)
    }

    /// A directory whose destination is the directory itself (`pax -rw tree
    /// .`) is neither created nor stamped. Without -s or -i everything below
    /// it maps onto itself too, and one diagnostic covers the lot. With them
    /// its contents may be renamed elsewhere, and are each copied under their
    /// own name, as they would be extracted from an archive; any that still
    /// map onto themselves are diagnosed one by one.
    fn dir_onto_itself(
        &self,
        src: &Path,
        member: PathBuf,
        metadata: &ftw::Metadata,
    ) -> PaxResult<bool> {
        if self.options.substitutions.is_empty() && !self.options.interactive {
            return overwrites_itself(src);
        }
        // It is a destination, and is being read while it is written to.
        self.dest_ids
            .borrow_mut()
            .insert((metadata.dev(), metadata.ino()));
        self.print_verbose(src);
        self.descend(member, metadata)
    }

    /// Whether -k or -u leaves the directory already at a destination name
    /// with its own attributes.
    fn keeps_existing_dir(
        &self,
        metadata: &ftw::Metadata,
        st: &libc::stat,
        mp: &MemberPath,
    ) -> bool {
        if self.tree.claim_implicit(st, mp) {
            return false;
        }
        self.options.no_clobber
            || (self.options.update && !self.is_source_newer(metadata, Some(st)))
    }

    /// Whether the source is newer than the destination already there (`-u`)
    /// -- as it was before this run, which a `find -depth` list has already
    /// written into by the time it names the directory. Times compare to the
    /// nanosecond.
    fn is_source_newer(&self, src_metadata: &ftw::Metadata, dest: Option<&libc::stat>) -> bool {
        let src = (src_metadata.mtime(), src_metadata.mtime_nsec());
        dest.is_none_or(|st| src > self.tree.mtime_before_run(st))
    }

    /// -v: name the source file on standard error.
    fn print_verbose(&self, src: &Path) {
        if !self.options.verbose {
            return;
        }
        let mut line = Vec::new();
        crate::escape::push_escaped(
            &mut line,
            crate::rawpath::as_bytes(src),
            crate::escape::stderr_style(),
        );
        line.push(b'\n');
        let _ = std::io::Write::write_all(&mut std::io::stderr().lock(), &line);
    }

    /// Walk into the source directory `member`, unless -d says not to or it
    /// is a mount point -X stops at.
    fn descend(&self, member: PathBuf, metadata: &ftw::Metadata) -> PaxResult<bool> {
        // -X copies a directory on another device but nothing below it.
        let operand_dev = self.dev_stack.borrow().first().copied();
        if self.options.no_recurse
            || !crate::modes::may_descend(self.options.one_file_system, operand_dev, metadata.dev())
        {
            return Ok(false);
        }

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

/// Whether the destination `st` is the very file being copied -- or, when
/// -H or -L followed a symbolic link to reach it, that link: it is just as
/// much the source, and replacing it destroys it (`pax -rw -H link .`).
fn is_source(st: &libc::stat, entry: &ftw::Entry<'_>, metadata: &ftw::Metadata) -> bool {
    let id = file_id(st);
    if id == (metadata.dev(), metadata.ino()) {
        return true;
    }
    if (st.st_mode & libc::S_IFMT) != libc::S_IFLNK || !followed_link(entry, metadata) {
        return false;
    }
    // The link the walk examined, by the identity it saw.
    entry.symlink_id() == Some(id)
}

/// The failure for a followed symbolic link the walk gave no identity for --
/// which it gives every link it reports (`ftw::Entry::symlink_id`).
fn link_not_identified() -> PaxError {
    PaxError::Io(std::io::Error::other(
        "symbolic link not identified by the walk",
    ))
}

/// Diagnose copying a file to its own name, as BSD pax words it, and skip it.
fn overwrites_itself(src: &Path) -> PaxResult<bool> {
    crate::error::report_error(src, "file would overwrite itself; not copied");
    Ok(false)
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
    // The normal file-creation action, never a set-id bit (`creation_mode`):
    // those, and under -p p the exact mode, come with the attributes below.
    let perm = policy_of(options).creation_mode(&attrs_of(metadata)) as libc::mode_t;
    let made_type = metadata.mode() as libc::mode_t & libc::S_IFMT;

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

    // mkfifoat and mknodat both apply the process umask (or a default ACL), the
    // normal file-creation action, so under -p p the mode they were given is
    // not necessarily the mode on disk; and neither carries ownership, times or
    // set-id bits. Extraction restores those here, so a copy must too --
    // through the node just made, never by name.
    set_made_node_attrs(
        dirfd,
        name,
        made_type,
        &attrs_of(metadata),
        &policy_of(options),
    )
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

    // A symlink's own mode is meaningless, so only owner and times are
    // restored.
    let (attrs, policy) = (attrs_of(metadata), policy_of(options));
    set_made_node_attrs(dirfd, name, libc::S_IFLNK, &attrs, &policy)
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
    link_tracker: &mut CopiedLinks,
    metadata: &ftw::Metadata,
) -> PaxResult<()> {
    let src_path = entry.path();
    let src = src_path.as_inner();

    // -l: link to the source rather than copying it.
    if options.link {
        // From the descriptor of the directory the walk found it in, not by
        // re-resolving the whole source path. Never by unlinking the source:
        // `pax -rwl tree .` names every file as its own destination. Under
        // -H/-L the walk followed a symbolic link here, and the link made is
        // to the file it refers to, as POSIX requires of -l.
        let follow = followed_link(entry, metadata)
            .then(|| entry.symlink_id().ok_or_else(link_not_identified))
            .transpose()?;
        let linked = link_replacing_with(
            entry.dir_fd(),
            entry.file_name(),
            follow,
            Some(Expected::id((metadata.dev(), metadata.ino()))),
            dirfd,
            name,
            options.no_clobber,
        );
        match linked {
            // The name is resolved again by linkat, so a link to anything but
            // the file the walk saw is removed again (an error here); the
            // copy below then copies that file, if the name still holds it.
            Ok(false) if is_file_at(dirfd, name, metadata) => return Ok(()),
            Ok(true) => {
                crate::error::report_error(src, "Unable to link file to itself");
                return Ok(());
            }
            // POSIX: links are made "whenever possible". One that cannot be
            // made -- across devices, most often -- means the file is copied
            // instead, which is the expected outcome and not an error.
            Ok(false) | Err(_) => {}
        }
    }

    // A second name for a file already copied becomes a link to that copy.
    //
    // `member` is the name the first copy was actually created under -- the
    // parsed one, with any leading `/` and `..` already removed. Recording the
    // raw name instead let a `-s` rename to an absolute path be handed to
    // `linkat`, which resolves an absolute path from the root of the filesystem
    // and ignores the anchor descriptor entirely.
    let (dev, ino, nlink) = (metadata.dev(), metadata.ino(), metadata.nlink() as u32);
    if let Some((link_target, copy)) = link_tracker.names.lookup_mut(dev, ino, nlink) {
        let Some(target) = MemberPath::parse(link_target)? else {
            return do_copy_file(entry, dirfd, name, metadata, options, false).map(|_| ());
        };
        let target_dir = tree.parent_of(&target, false)?;
        // Resolved one component at a time from the destination anchor, the
        // same way the file itself was created. A name that already is that
        // copy -- this very name visited again, from the list and from the
        // walk -- is left alone rather than unlinked out from under itself.
        // The link is to the copy made, pinned: someone who can write the
        // copy's directory can have put another file at its name since.
        link_replacing_with(
            target_dir.as_raw_fd(),
            &target.leaf,
            None,
            Some(copy.expected()),
            dirfd,
            name,
            options.no_clobber,
        )?;
        // Linking a name changed its ctime.
        copy.linked(dirfd, name);
        return Ok(());
    }

    let copy = do_copy_file(entry, dirfd, name, metadata, options, nlink > 1)?;
    // Only a copy that exists can be linked to by the file's later names.
    link_tracker.record((dev, ino), nlink, member, copy);
    Ok(())
}

/// Whether `name` in `dirfd` is the file `metadata` describes.
fn is_file_at(dirfd: BorrowedFd<'_>, name: &CStr, metadata: &ftw::Metadata) -> bool {
    lstat_at(dirfd.as_raw_fd(), name)
        .ok()
        .is_some_and(|st| file_id(&st) == (metadata.dev(), metadata.ino()))
}

/// Actually copy file contents, returning the copy made, known from its
/// descriptor (`MadeFile`); `None` where -k kept a file already at the name.
fn do_copy_file(
    entry: &ftw::Entry<'_>,
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    metadata: &ftw::Metadata,
    options: &CopyOptions,
    pin: bool,
) -> PaxResult<Option<MadeFile>> {
    // From the descriptor of the directory the walk found it in, and re-checked
    // against the (dev, ino) the walk saw, rather than re-resolving the whole
    // source path. Whether the walk dereferenced this entry is observable from
    // the entry itself, so -H/-L stays decided in the traversal options.
    let mut src_file = crate::modes::anchored::open_source_file(
        entry.dir_fd(),
        entry.file_name(),
        followed_link(entry, metadata),
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
        return Ok(None);
    };

    copy_contents(&mut src_file, &mut dest_file, metadata.size())?;
    if options.reset_atime {
        restore_atime(src_file.as_fd(), entry.path().as_inner(), metadata);
    }

    set_attrs_fd(dest_file.as_fd(), &attrs_of(metadata), &policy_of(options))?;
    // Known once its attributes are set, which change its ctime.
    let copy = MadeFile::of(dest_file.as_fd(), pin)?;
    // A filesystem that defers writes reports their failure on close.
    crate::blocked_io::close_file(dest_file)?;
    Ok(Some(copy))
}

/// The largest buffer `copy_contents` reads through.
const COPY_BUFFER: u64 = 128 * 1024;

/// Copy everything left to read in `src` to `dest`.
///
/// On Linux the kernel moves the data (`copy_file_range`), which spares the
/// round trip through user space and lets a filesystem that can share or
/// clone extents do so. Where it cannot -- across filesystems on an older
/// kernel, or a filesystem that does not support it -- what is left goes
/// through a buffer instead, as it does everywhere else. `size` is only the
/// size the walk saw, for sizing that buffer: the copy runs to end of file.
fn copy_contents(src: &mut File, dest: &mut File, size: u64) -> std::io::Result<()> {
    #[cfg(target_os = "linux")]
    if kernel_copy(src, dest)? {
        return Ok(());
    }

    // No larger than the file needs, so copying many small files does not
    // allocate a large buffer for each; one more byte than its size reads
    // end of file in the same pass.
    let mut buf = vec![0u8; size.saturating_add(1).clamp(512, COPY_BUFFER) as usize];
    loop {
        let n = match src.read(&mut buf) {
            Ok(0) => return Ok(()),
            Ok(n) => n,
            Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
            Err(e) => return Err(e),
        };
        dest.write_all(&buf[..n])?;
    }
}

/// `copy_file_range` from `src` to `dest` until end of file: `Ok(false)` when
/// the kernel cannot copy between these two files, with whatever it did copy
/// already reflected in both file offsets, so the caller carries on from there.
#[cfg(target_os = "linux")]
fn kernel_copy(src: &File, dest: &File) -> std::io::Result<bool> {
    let mut copied_any = false;
    loop {
        let n = unsafe {
            libc::copy_file_range(
                src.as_raw_fd(),
                std::ptr::null_mut(),
                dest.as_raw_fd(),
                std::ptr::null_mut(),
                1 << 30,
                0,
            )
        };
        let errno = (n < 0).then(std::io::Error::last_os_error);
        match kernel_copy_step(n, errno.as_ref().and_then(|e| e.raw_os_error()), copied_any) {
            KernelCopyStep::Again => copied_any |= n > 0,
            KernelCopyStep::Done => return Ok(true),
            KernelCopyStep::Fallback => return Ok(false),
            KernelCopyStep::Fail => return Err(errno.expect("only a failed call fails")),
        }
    }
}

/// What one `copy_file_range` result means for `kernel_copy`.
#[cfg(target_os = "linux")]
#[derive(Debug, PartialEq)]
enum KernelCopyStep {
    /// Call again.
    Again,
    /// End of file.
    Done,
    /// The kernel cannot copy these; read and write what is left instead.
    Fallback,
    /// A real error.
    Fail,
}

/// Classify the result `n` (and its errno) of one `copy_file_range` call.
///
/// 0 is end of file -- except from the very first call: Linux 5.3 to 5.18
/// return 0 for pseudo-files whose size reads as 0 (procfs, sysfs, tracefs),
/// and overlayfs has done the same, so a /proc file came out empty with a
/// zero exit status. As Rust's std does, nothing copied yet means "try the
/// ordinary way", which then finds the real end of file for a file that is
/// truly empty.
#[cfg(target_os = "linux")]
fn kernel_copy_step(n: isize, errno: Option<i32>, copied_any: bool) -> KernelCopyStep {
    match (n, errno) {
        (0, _) if copied_any => KernelCopyStep::Done,
        (0, _) => KernelCopyStep::Fallback,
        (n, _) if n > 0 => KernelCopyStep::Again,
        (_, Some(libc::EINTR)) => KernelCopyStep::Again,
        (_, Some(libc::EXDEV | libc::ENOSYS | libc::EINVAL | libc::EOPNOTSUPP | libc::EPERM)) => {
            KernelCopyStep::Fallback
        }
        _ => KernelCopyStep::Fail,
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use plib::tmp::TempDir;
    use std::fs;

    /// A later name of a file already copied is linked to that copy, found by
    /// the name the copy was made at. In a destination directory others can
    /// write, someone can rename their own file over that name first, and the
    /// later name -- in a directory only pax's user writes -- became a link
    /// to theirs. The link is made to the copy itself, or not at all.
    #[cfg(target_os = "linux")]
    #[test]
    fn test_copy_links_later_names_only_to_the_copy_made() {
        use crate::modes::race_hook::{with_hook, Point};
        let src = TempDir::new().unwrap();
        let dest = TempDir::new().unwrap();
        fs::create_dir(src.path().join("pub")).unwrap();
        fs::create_dir(src.path().join("priv")).unwrap();
        fs::write(src.path().join("pub/f"), "source\n").unwrap();
        fs::hard_link(src.path().join("pub/f"), src.path().join("priv/g")).unwrap();
        let f = src.path().join("pub/f");
        let g = src.path().join("priv/g");
        let copied = |path: &std::path::Path| dest.path().join(member_name(path));

        let (copy_of_f, planted) = (copied(&f), dest.path().join("planted"));
        let swap = move |point, _: libc::c_int, name: &CStr| {
            if point == Point::Linking && name == c"f" {
                fs::write(&planted, "planted\n").unwrap();
                fs::rename(&planted, &copy_of_f).unwrap();
            }
        };
        let options = CopyOptions::default();
        let mut operands = [f.clone(), g.clone()].into_iter();
        let _ = with_hook(swap, || copy_files(&mut operands, dest.path(), &options));
        let later = fs::read_to_string(copied(&g)).unwrap_or_default();
        assert_ne!(
            later, "planted\n",
            "the later name was linked to the planted file"
        );
    }

    /// A file with one name has no later name to be linked to it: its copy is
    /// not remembered, and takes nothing from the budget of pins.
    #[cfg(target_os = "linux")]
    #[test]
    fn test_a_copy_with_one_name_takes_no_pin() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("f");
        fs::write(&path, "x").unwrap();
        let file = fs::File::open(&path).unwrap();
        let made = MadeFile::of(file.as_fd(), true).unwrap();
        assert!(made.is_pinned());
        let mut links = CopiedLinks::new();
        links.record((1, 2), 1, Path::new("f"), Some(made));
        assert_eq!(links.pins.held(), 0);
        assert!(links.names.lookup(1, 2, 2).is_none());
    }

    /// Under -k a copy that finds its name taken at the moment it is made --
    /// the check before it saw nothing there -- makes nothing, and its later
    /// names are not linked to whatever holds that name: nothing is
    /// remembered, as when -k finds the name taken beforehand.
    #[test]
    fn test_a_copy_kept_by_k_is_not_remembered() {
        let mut links = CopiedLinks::new();
        links.record((1, 2), 2, Path::new("f"), None);
        assert!(links.names.lookup(1, 2, 2).is_none());
    }

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

        copy_files(
            &mut std::iter::once(src_file.clone()),
            dest_dir.path(),
            &options,
        )
        .unwrap();

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

        copy_files(
            &mut std::iter::once(subdir.clone()),
            dest_dir.path(),
            &options,
        )
        .unwrap();

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

        copy_files(&mut std::iter::once(src_file), dest_dir.path(), &options).unwrap();

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

        copy_files(
            &mut std::iter::once(src_link.clone()),
            dest_dir.path(),
            &options,
        )
        .unwrap();

        let dest_link = dest_dir.path().join(member_name(&src_link));
        assert!(dest_link.symlink_metadata().unwrap().is_symlink());
        assert_eq!(
            fs::read_link(&dest_link).unwrap().to_str().unwrap(),
            "target.txt"
        );
    }

    /// A first `copy_file_range` that returns 0 is not trusted as end of
    /// file: /proc and sysfs files report size 0 to it on some kernels.
    #[cfg(target_os = "linux")]
    #[test]
    fn test_kernel_copy_first_zero_falls_back() {
        assert_eq!(kernel_copy_step(0, None, false), KernelCopyStep::Fallback);
        assert_eq!(kernel_copy_step(0, None, true), KernelCopyStep::Done);
        assert_eq!(kernel_copy_step(4096, None, false), KernelCopyStep::Again);
        assert_eq!(
            kernel_copy_step(-1, Some(libc::EINTR), true),
            KernelCopyStep::Again
        );
        assert_eq!(
            kernel_copy_step(-1, Some(libc::EXDEV), false),
            KernelCopyStep::Fallback
        );
        assert_eq!(
            kernel_copy_step(-1, Some(libc::EIO), true),
            KernelCopyStep::Fail
        );
    }
}

#[cfg(test)]
mod race_tests;
