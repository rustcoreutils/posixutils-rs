//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Resolving a member pathname without ever following a symbolic link.
//!
//! Both read mode and copy mode write a tree of member names below a directory
//! the user named, and both have to assume the names come from somewhere
//! untrusted -- an archive in one case, a source tree someone else can modify in
//! the other. Resolving such a name through the ordinary filesystem namespace
//! means every component is re-resolved on every call and any of them can be a
//! symbolic link pointing anywhere.
//!
//! So a member is walked one component at a time from a descriptor for the
//! directory it is anchored to, each `openat` refusing links, and the leaf is
//! always created fresh rather than written through.

use crate::error::{PaxError, PaxResult};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::ffi::{CStr, CString, OsStr};
use std::fs::File;
use std::os::fd::{AsFd, AsRawFd, BorrowedFd, FromRawFd, OwnedFd};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::rc::Rc;

/// A member pathname reduced to the directory components that must be walked
/// and the final component to create.
pub(crate) struct MemberPath {
    /// The directory components, each ended by a NUL, in one buffer: a name
    /// of many components costs one allocation rather than one per component.
    dirs: Vec<u8>,
    /// How many components `dirs` holds.
    depth: usize,
    pub(crate) leaf: CString,
    /// The same path as text, for diagnostics and hard-link bookkeeping.
    pub(crate) display: PathBuf,
}

impl MemberPath {
    /// `Ok(None)` for a member that names nothing to create -- `.`, or a name
    /// made up entirely of `.`, `..` and root components.
    ///
    /// `..` still pops and a leading `/` is still dropped, but this lexical
    /// pass is no longer the security boundary it used to be: it cannot see
    /// that `a/b` escapes when `a` is a symlink. Resolution opens each
    /// component with O_NOFOLLOW instead, which does not care how the name is
    /// spelled. What remains here is naming policy, plus the one check that
    /// must happen before any syscall: an embedded NUL.
    pub(crate) fn parse(path: &Path) -> PaxResult<Option<Self>> {
        use std::path::Component;

        let mut parts: Vec<&OsStr> = Vec::new();
        for comp in path.components() {
            match comp {
                Component::Normal(c) => parts.push(c),
                Component::ParentDir => {
                    parts.pop();
                }
                Component::CurDir | Component::RootDir | Component::Prefix(_) => {}
            }
        }

        let Some(leaf_os) = parts.pop() else {
            return Ok(None);
        };
        if path.as_os_str().as_bytes().contains(&0) {
            return Err(PaxError::InvalidHeader("path contains null".to_string()));
        }

        let mut dirs = Vec::with_capacity(parts.iter().map(|p| p.len() + 1).sum());
        let mut display = PathBuf::with_capacity(dirs.capacity() + leaf_os.len());
        for p in &parts {
            dirs.extend_from_slice(p.as_bytes());
            dirs.push(0);
            display.push(p);
        }
        display.push(leaf_os);

        Ok(Some(MemberPath {
            dirs,
            depth: parts.len(),
            leaf: CString::new(leaf_os.as_bytes()).expect("NUL was checked for above"),
            display,
        }))
    }

    /// The directory components to walk, in order.
    fn dirs(&self) -> impl Iterator<Item = &CStr> {
        self.dirs
            .split_inclusive(|&b| b == 0)
            .map(|c| CStr::from_bytes_with_nul(c).expect("each component ends in its NUL"))
    }

    /// Whether this name refers to the extraction directory itself rather
    /// than naming nothing at all.
    ///
    /// `pax -w .` records a `.` member, so every archive built that way
    /// carries one and there is nothing wrong with it -- it just has no file
    /// to create below the anchor. An empty name, or one made only of `..`
    /// and root components, is a different thing and worth saying out loud.
    pub(crate) fn names_current_directory(path: &Path) -> bool {
        use std::path::Component;
        let mut saw_something = false;
        for comp in path.components() {
            match comp {
                Component::CurDir => saw_something = true,
                _ => return false,
            }
        }
        saw_something
    }

    /// How deep the member sits, for ordering the deferred directory pass.
    pub(crate) fn depth(&self) -> usize {
        self.depth
    }
}

/// Open flags for a directory that is only ever walked through or used as the
/// `dirfd` of an `*at` call.
///
/// Reaching a name below a directory takes search permission only, so opening
/// each component for reading refused a path through a directory the user may
/// search and write but not list (mode 0300) where `mkdir` or `open` by name
/// would have succeeded. `O_PATH` (Linux) and `O_SEARCH` (macOS, the BSDs) open
/// it for exactly that. Elsewhere `O_RDONLY` is the only option there is.
#[cfg(any(target_os = "linux", target_os = "android"))]
const SEARCH_ONLY: libc::c_int = libc::O_PATH;
#[cfg(any(target_os = "macos", target_os = "freebsd", target_os = "netbsd"))]
const SEARCH_ONLY: libc::c_int = libc::O_SEARCH;
#[cfg(not(any(
    target_os = "linux",
    target_os = "android",
    target_os = "macos",
    target_os = "freebsd",
    target_os = "netbsd"
)))]
const SEARCH_ONLY: libc::c_int = libc::O_RDONLY;

/// Flags for walking one directory component: search only, and never through
/// a symbolic link or anything that is not a directory.
const WALK_FLAGS: libc::c_int =
    SEARCH_ONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC;

/// Extraction anchored at an open descriptor for the working directory.
///
/// Member paths are walked one component at a time with
/// `O_DIRECTORY|O_NOFOLLOW`, so a symlink planted anywhere along the path fails
/// the descent rather than redirecting the write outside the extraction
/// directory. The previous code resolved whole paths through the ordinary
/// filesystem namespace, where `create_dir_all` on `sub/file` was happy to
/// follow `sub -> /elsewhere`.
pub(crate) struct DirTree {
    root: OwnedFd,
    /// The directories most recently walked through, one descriptor per
    /// level from the anchor down, so the next member reopens only the
    /// components its path does not share with the last one's. Consecutive
    /// members of one directory share the whole walk, and a depth-first tree
    /// -- copy mode, or an archive written by one -- opens one directory per
    /// member instead of re-walking its whole chain, which made a deep tree
    /// cost the square of its depth.
    ///
    /// Only the last member's own chain is ever kept. A member that could
    /// replace one of its components names that component as its leaf, so it
    /// shares less of the chain than that, and cuts the chain off there before
    /// anything below it can be reused.
    chain: RefCell<Chain>,
    /// How many levels `chain` may hold: each is an open descriptor.
    max_levels: usize,
    /// The parent most recently walked to, so consecutive members of one
    /// directory deeper than `max_levels` still share one walk.
    last_parent: RefCell<Option<(Vec<u8>, Rc<OwnedFd>)>>,
    /// `(st_dev, st_ino)` of the directories this run created only to hold a
    /// member below them. Such a directory is not a pre-existing file: a member
    /// that names it later (`find -depth` order) still gives it its attributes.
    implicit: RefCell<HashSet<(u64, u64)>>,
    /// The mtime each pre-existing directory had when this run first walked
    /// into it, before any member created below it changed that. -u compares
    /// against this: a `find -depth` list names a directory after its
    /// contents, by which time its own mtime is this run's doing.
    pre_run_mtimes: RefCell<HashMap<(u64, u64), (i64, i64)>>,
}

impl DirTree {
    pub(crate) fn open_cwd() -> PaxResult<Self> {
        Self::open_path(Path::new("."))
    }

    /// Anchor at a directory named by the caller, for copy mode's destination.
    pub(crate) fn open_path(path: &Path) -> PaxResult<Self> {
        let c = CString::new(path.as_os_str().as_bytes())
            .map_err(|_| PaxError::InvalidHeader("path contains null".to_string()))?;
        let fd = unsafe {
            libc::openat(
                libc::AT_FDCWD,
                c.as_ptr(),
                SEARCH_ONLY | libc::O_DIRECTORY | libc::O_CLOEXEC,
            )
        };
        if fd < 0 {
            return Err(std::io::Error::last_os_error().into());
        }
        Ok(DirTree {
            root: unsafe { OwnedFd::from_raw_fd(fd) },
            chain: RefCell::new(Chain::default()),
            max_levels: cached_levels_budget(),
            last_parent: RefCell::new(None),
            implicit: RefCell::new(HashSet::new()),
            pre_run_mtimes: RefCell::new(HashMap::new()),
        })
    }

    /// The descriptor for the anchor itself.
    pub(crate) fn root(&self) -> BorrowedFd<'_> {
        self.root.as_fd()
    }

    /// Open the directory that will hold `member`, creating any missing
    /// intermediate components.
    pub(crate) fn parent_of(
        &self,
        member: &MemberPath,
        create_missing: bool,
    ) -> PaxResult<Rc<OwnedFd>> {
        if let Some((dirs, fd)) = &*self.last_parent.borrow() {
            if *dirs == member.dirs {
                return Ok(Rc::clone(fd));
            }
        }

        let walked = self.walk_chain(member, create_missing);
        // A walk that fails part-way has still extended the chain past the
        // last parent, which the shortcut above would then skip truncating:
        // a later member naming one of those directories as its leaf could
        // replace it while the chain kept its descriptor. With no last parent
        // the next member goes through the chain, and cuts it as it should.
        *self.last_parent.borrow_mut() = match &walked {
            Ok(fd) => Some((member.dirs.clone(), Rc::clone(fd))),
            Err(_) => None,
        };
        walked
    }

    /// The walk `parent_of` does through the chain, reopening only the
    /// components `member` does not share with it.
    fn walk_chain(&self, member: &MemberPath, create_missing: bool) -> PaxResult<Rc<OwnedFd>> {
        let mut chain = self.chain.borrow_mut();
        let shared = chain.shared_with(member);
        chain.truncate(shared);
        let mut cur = chain.levels.last().map(|(_, fd)| Rc::clone(fd));

        for comp in member.dirs().skip(shared) {
            let at = cur.as_ref().map_or(self.root.as_fd(), |fd| fd.as_fd());
            let (next, created) = open_or_create_dir_at(at, comp, create_missing)?;
            if let Some(st) = fstat(next.as_fd()) {
                if created {
                    self.implicit.borrow_mut().insert(file_id(&st));
                } else {
                    self.pre_run_mtimes
                        .borrow_mut()
                        .entry(file_id(&st))
                        .or_insert(mtime_of(&st));
                }
            }
            let next = Rc::new(next);
            if chain.levels.len() < self.max_levels {
                chain.push(comp, Rc::clone(&next));
            }
            cur = Some(next);
        }
        match cur {
            Some(fd) => Ok(fd),
            None => Ok(Rc::new(self.root.try_clone()?)),
        }
    }

    /// Whether `st` is a directory this run created only to hold members
    /// below it, rather than one that was there before.
    pub(crate) fn is_implicit(&self, st: &libc::stat) -> bool {
        self.implicit.borrow().contains(&file_id(st))
    }

    /// `is_implicit`, for the member that names the directory and so gives it
    /// its attributes. That member makes it an ordinary existing directory:
    /// a later member of the same name meets it the way it would meet any
    /// other -- left alone under -k -- rather than as one still waiting for
    /// its attributes.
    pub(crate) fn claim_implicit(&self, st: &libc::stat) -> bool {
        self.implicit.borrow_mut().remove(&file_id(st))
    }

    /// The mtime `st` had before this run put anything below it, for -u, as
    /// seconds and nanoseconds.
    pub(crate) fn mtime_before_run(&self, st: &libc::stat) -> (i64, i64) {
        self.pre_run_mtimes
            .borrow()
            .get(&file_id(st))
            .copied()
            .unwrap_or_else(|| mtime_of(st))
    }
}

/// `st`'s modification time, as seconds and nanoseconds.
fn mtime_of(st: &libc::stat) -> (i64, i64) {
    // Casts needed: the field types are i64 on both platforms, but not by name.
    #[allow(clippy::unnecessary_cast)]
    (st.st_mtime as i64, st.st_mtime_nsec as i64)
}

/// The directories `DirTree` walked to last, a descriptor per level.
#[derive(Default)]
struct Chain {
    /// The components, each ended by a NUL, as in `MemberPath::dirs`.
    names: Vec<u8>,
    /// For each level, where its name ends in `names` and its descriptor.
    levels: Vec<(usize, Rc<OwnedFd>)>,
}

impl Chain {
    /// How many of `member`'s leading directories this chain holds.
    fn shared_with(&self, member: &MemberPath) -> usize {
        self.names
            .split_inclusive(|&b| b == 0)
            .zip(member.dirs.split_inclusive(|&b| b == 0))
            .take_while(|(a, b)| a == b)
            .count()
    }

    /// Forget every level below the first `depth`.
    fn truncate(&mut self, depth: usize) {
        self.levels.truncate(depth);
        let end = self.levels.last().map_or(0, |(end, _)| *end);
        self.names.truncate(end);
    }

    fn push(&mut self, name: &CStr, fd: Rc<OwnedFd>) {
        self.names.extend_from_slice(name.to_bytes_with_nul());
        self.levels.push((self.names.len(), fd));
    }
}

/// How many directory descriptors a `DirTree` may hold open for reuse.
///
/// Half of what the descriptor limit leaves once the reserve `ftw` keeps for
/// its callers is set aside -- the same figure `ftw` derives its own budget
/// from. Copy mode's walk takes the other half: it is told this tree holds a
/// descriptor per level (`caller_fds_per_level`), and conserves its own once
/// the two together would not fit.
fn cached_levels_budget() -> usize {
    const RESERVE: u64 = 16;
    const MAX: u64 = 4096;
    let mut rl = libc::rlimit {
        rlim_cur: 0,
        rlim_max: 0,
    };
    // Casts needed: `rlim_t` is u64 on both platforms, but not by name.
    #[allow(clippy::unnecessary_cast)]
    let limit = if unsafe { libc::getrlimit(libc::RLIMIT_NOFILE, &mut rl) } == 0 {
        rl.rlim_cur as u64
    } else {
        1024
    };
    (limit.saturating_sub(RESERVE).clamp(1, MAX) / 2) as usize
}

/// `(st_dev, st_ino)` of a stat result.
pub(crate) fn file_id(st: &libc::stat) -> (u64, u64) {
    // Casts needed: `dev_t` is i32 on macOS and u64 on Linux.
    #[allow(clippy::unnecessary_cast)]
    (st.st_dev as u64, st.st_ino as u64)
}

/// Directories whose archived attributes wait until everything below them
/// exists.
///
/// An archived mode denying write or search would stop the directory's own
/// contents being created, and every child created afterwards changes its
/// mtime, so both are applied once at the end, deepest first. Only the name,
/// the identity and the attributes are kept: the directory is reopened when its
/// turn comes, and stamped only if the name still refers to it.
#[derive(Default)]
pub(crate) struct PendingDirs(Vec<PendingDir>);

struct PendingDir {
    path: PathBuf,
    depth: usize,
    /// `(st_dev, st_ino)` of the directory the attributes are for.
    id: (u64, u64),
    attrs: Attrs,
}

impl PendingDirs {
    /// Defer `attrs` for the directory `member` names, `st` being what that
    /// directory is.
    pub(crate) fn push(&mut self, member: &MemberPath, st: &libc::stat, attrs: Attrs) {
        self.0.push(PendingDir {
            path: member.display.clone(),
            depth: member.depth(),
            id: file_id(st),
            attrs,
        });
    }

    /// Apply every pending directory's attributes, deepest first.
    ///
    /// Siblings are grouped so they share one walk to their parent; the sort
    /// is stable, so of two entries for one name the later still wins.
    pub(crate) fn apply(&mut self, tree: &DirTree, policy: &AttrPolicy) {
        self.0
            .sort_by(|a, b| b.depth.cmp(&a.depth).then_with(|| a.path.cmp(&b.path)));
        for dir in self.0.drain(..) {
            if let Err(e) = apply_dir_attrs(tree, &dir, policy) {
                crate::error::report_error(&dir.path, e);
            }
        }
    }
}

/// Reopen one pending directory and apply its attributes through that
/// descriptor.
///
/// The name need not still be the directory that was created for it -- a
/// later member can have replaced it with a file, a symbolic link or another
/// directory, and applying the mode by name would then chmod whatever the link
/// points at. `O_NOFOLLOW` refuses the link, `O_DIRECTORY` anything else that
/// is not a directory, and the identity check a directory that is not this
/// one. Each means the directory these attributes were for is gone, which is
/// not an error: whatever replaced it brought its own.
fn apply_dir_attrs(tree: &DirTree, dir: &PendingDir, policy: &AttrPolicy) -> PaxResult<()> {
    let Some(member) = MemberPath::parse(&dir.path)? else {
        return Ok(());
    };
    let opened = tree
        .parent_of(&member, false)
        .and_then(|parent| open_dir_for_attrs(parent.as_fd(), &member.leaf));
    let (fd, search_only) = match opened {
        Ok(opened) => opened,
        Err(PaxError::Io(e)) if is_superseded(&e) => return Ok(()),
        Err(e) => return Err(e),
    };
    if fstat(fd.as_fd()).is_none_or(|st| file_id(&st) != dir.id) {
        return Ok(());
    }
    if search_only {
        return set_attrs_search_only(fd.as_fd(), &dir.attrs, policy);
    }
    set_attrs_fd(fd.as_fd(), &dir.attrs, policy)
}

/// Whether failing to reach a pending directory means it was replaced.
fn is_superseded(e: &std::io::Error) -> bool {
    matches!(
        e.raw_os_error(),
        Some(libc::ENOENT) | Some(libc::ENOTDIR) | Some(libc::ELOOP)
    )
}

/// Open a directory to apply attributes through, never following a symbolic
/// link. `true` alongside it when it could only be opened for search.
///
/// None of the attributes needs read permission, and a directory whose mode
/// denies its owner reading (0300) still takes them -- through a descriptor
/// opened for search alone, where the platform has one.
fn open_dir_for_attrs(dirfd: BorrowedFd<'_>, name: &CStr) -> PaxResult<(OwnedFd, bool)> {
    let flags = libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC;
    let fd = unsafe { libc::openat(dirfd.as_raw_fd(), name.as_ptr(), flags) };
    if fd >= 0 {
        return Ok((unsafe { OwnedFd::from_raw_fd(fd) }, false));
    }
    let err = std::io::Error::last_os_error();
    if err.raw_os_error() != Some(libc::EACCES) || SEARCH_ONLY == libc::O_RDONLY {
        return Err(err.into());
    }
    let fd = unsafe { libc::openat(dirfd.as_raw_fd(), name.as_ptr(), WALK_FLAGS) };
    if fd < 0 {
        return Err(std::io::Error::last_os_error().into());
    }
    Ok((unsafe { OwnedFd::from_raw_fd(fd) }, true))
}

/// `set_attrs_fd` for a descriptor opened for search only.
///
/// Linux's `O_PATH` descriptor is refused by `fchown`, `fchmod` and `futimens`
/// alike. Its `/proc/self/fd` entry is not: it resolves to the very file the
/// descriptor holds, whatever has happened to the name since, so the calls
/// made through it by name cannot be redirected.
#[cfg(any(target_os = "linux", target_os = "android"))]
fn set_attrs_search_only(fd: BorrowedFd<'_>, attrs: &Attrs, policy: &AttrPolicy) -> PaxResult<()> {
    let path = CString::new(format!("/proc/self/fd/{}", fd.as_raw_fd()))
        .expect("a formatted number has no NUL");
    set_attrs(&AttrTarget::Path(&path), attrs, policy)
}

/// Elsewhere a search-only descriptor (`O_SEARCH`) takes the same calls as any
/// other.
#[cfg(not(any(target_os = "linux", target_os = "android")))]
fn set_attrs_search_only(fd: BorrowedFd<'_>, attrs: &Attrs, policy: &AttrPolicy) -> PaxResult<()> {
    set_attrs_fd(fd, attrs, policy)
}

/// Open one directory component below `dirfd` without following a symlink.
pub(crate) fn open_dir_at(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    create_missing: bool,
) -> PaxResult<OwnedFd> {
    open_or_create_dir_at(dirfd, name, create_missing).map(|(fd, _)| fd)
}

/// `open_dir_at`, also saying whether the directory had to be created.
fn open_or_create_dir_at(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    create_missing: bool,
) -> PaxResult<(OwnedFd, bool)> {
    let fd = unsafe { libc::openat(dirfd.as_raw_fd(), name.as_ptr(), WALK_FLAGS) };
    if fd >= 0 {
        return Ok((unsafe { OwnedFd::from_raw_fd(fd) }, false));
    }

    let err = std::io::Error::last_os_error();
    if err.raw_os_error() != Some(libc::ENOENT) || !create_missing {
        return Err(err.into());
    }

    // Intermediate directories are created with the normal file-creation
    // action, per POSIX read/copy mode: mode 0777 modified by the umask.
    let r = unsafe { libc::mkdirat(dirfd.as_raw_fd(), name.as_ptr(), 0o777) };
    let created = r == 0;
    if !created {
        let e = std::io::Error::last_os_error();
        if e.raw_os_error() != Some(libc::EEXIST) {
            return Err(e.into());
        }
    }

    let fd = unsafe { libc::openat(dirfd.as_raw_fd(), name.as_ptr(), WALK_FLAGS) };
    if fd < 0 {
        return Err(std::io::Error::last_os_error().into());
    }
    Ok((unsafe { OwnedFd::from_raw_fd(fd) }, created))
}

/// Create the directory a member names, replacing a non-directory in the
/// way the way a file member replaces a file. `true` when the member's
/// attributes are to be applied to it once its contents exist; `false` when
/// -k (`no_clobber`) leaves what is there alone.
///
/// It is created with `mode` (the umask applies) plus owner read, write and
/// search: enough to populate it, and never more than the final mode lets
/// anyone else do. A directory made 0777 first and given its own mode only at
/// the end was open to everyone in between -- for good, if pax never got
/// there.
pub(crate) fn make_dir_at(
    tree: &DirTree,
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    mode: u32,
    no_clobber: bool,
) -> PaxResult<bool> {
    // An archived 0555 used to be set immediately and then rejected every
    // child with EACCES.
    let mode = ((mode & 0o7777) | 0o700) as libc::mode_t;
    let mkdir = || {
        if unsafe { libc::mkdirat(dirfd.as_raw_fd(), name.as_ptr(), mode) } == 0 {
            Ok(())
        } else {
            Err(std::io::Error::last_os_error())
        }
    };
    let exists = |e: &std::io::Error| e.raw_os_error() == Some(libc::EEXIST);

    match mkdir() {
        Ok(()) => return Ok(true),
        Err(e) if !exists(&e) => return Err(e.into()),
        Err(_) => {}
    }
    // A directory this run created only to hold earlier members is not a
    // pre-existing file: the member naming it (`find -depth` order) brings
    // its attributes. With -k anything else there is left entirely alone.
    // Otherwise extracting onto an existing directory is not an error
    // (POSIX), and it is kept.
    let existing_dir = stat_at(dirfd, name).filter(|st| st.st_mode & libc::S_IFMT == libc::S_IFDIR);
    if existing_dir.is_some_and(|st| tree.claim_implicit(&st)) {
        return Ok(true);
    }
    if no_clobber {
        return Ok(false);
    }
    if existing_dir.is_some() {
        return Ok(true);
    }

    // A non-directory is in the way, and is replaced the way a file member
    // replaces a file. unlinkat with no flags removes the name itself -- never
    // what a symlink points at -- and refuses a directory, so one that
    // appeared meanwhile survives and is used.
    let unlinked = unsafe { libc::unlinkat(dirfd.as_raw_fd(), name.as_ptr(), 0) } == 0;
    let unlink_err = (!unlinked).then(std::io::Error::last_os_error);
    match mkdir() {
        Ok(()) => Ok(true),
        Err(e) if exists(&e) && is_directory_at(dirfd, name) => Ok(true),
        Err(e) => Err(unlink_err.unwrap_or(e).into()),
    }
}

/// Whether `name` below `dirfd` is a directory, not following a symlink.
fn is_directory_at(dirfd: BorrowedFd<'_>, name: &CStr) -> bool {
    stat_at(dirfd, name).is_some_and(|st| st.st_mode & libc::S_IFMT == libc::S_IFDIR)
}

/// Remove whatever currently occupies `name`, so an exclusive create can win.
pub(crate) fn unlink_at(dirfd: BorrowedFd<'_>, name: &CStr) -> PaxResult<()> {
    let r = unsafe { libc::unlinkat(dirfd.as_raw_fd(), name.as_ptr(), 0) };
    if r != 0 {
        let err = std::io::Error::last_os_error();
        match err.raw_os_error() {
            Some(libc::ENOENT) => return Ok(()),
            // A directory in the way needs the directory flag instead.
            Some(libc::EISDIR) | Some(libc::EPERM) => {
                let r =
                    unsafe { libc::unlinkat(dirfd.as_raw_fd(), name.as_ptr(), libc::AT_REMOVEDIR) };
                if r == 0 {
                    return Ok(());
                }
                return Err(std::io::Error::last_os_error().into());
            }
            _ => return Err(err.into()),
        }
    }
    Ok(())
}

/// Create a member, retrying once after clearing whatever is in the way.
///
/// `create` reports `EEXIST` by returning `Err`; that is the whole point. With
/// -k an existing name means skip, atomically and with no window. Otherwise the
/// old entry is unlinked and the create retried, so the member is always a
/// freshly created object -- never a write *through* a symlink or a hard link
/// an attacker left behind, which `O_TRUNC` on an existing name would allow.
pub(crate) fn create_replacing<F>(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    no_clobber: bool,
    mut create: F,
) -> PaxResult<bool>
where
    F: FnMut() -> std::io::Result<()>,
{
    match create() {
        Ok(()) => return Ok(true),
        Err(e) if e.raw_os_error() == Some(libc::EEXIST) => {}
        Err(e) => return Err(e.into()),
    }

    if no_clobber {
        return Ok(false);
    }

    unlink_at(dirfd, name)?;
    match create() {
        Ok(()) => Ok(true),
        Err(e) => Err(e.into()),
    }
}

/// Hard-link `from_name` (in `from_dir`) to `name` (in `dirfd`), replacing
/// whatever holds `name` the way `create_replacing` does -- unless it already
/// *is* the file being linked.
///
/// That case cannot go through the unlink-and-retry: the name in the way may
/// be the link source itself (`pax -rwl tree .`, or a member linked to its own
/// name), and unlinking it destroys the only thing there was to link. Nothing
/// is changed and `true` is returned; the caller decides whether that merits a
/// diagnostic. Identity is (dev, ino) of both names, the source resolved the
/// way `linkat` resolves it: the name itself, or with `follow` the file a
/// symbolic link refers to -- which may be the very file at `name` -- and the
/// link too, which is just as much the source (`pax -rwl -H link .`).
pub(crate) fn link_replacing(
    from_dir: libc::c_int,
    from_name: &CStr,
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    no_clobber: bool,
) -> PaxResult<bool> {
    link_replacing_with(from_dir, from_name, false, dirfd, name, no_clobber)
}

/// `link_replacing`, linking the file a symbolic link `from_name` refers to
/// when `follow` is set -- copy mode's `-l` under `-H`/`-L`, where POSIX says
/// "the hard link created ... shall be to the file referenced by the symbolic
/// link". Without it, `from_name` itself is linked, whatever it is.
pub(crate) fn link_replacing_with(
    from_dir: libc::c_int,
    from_name: &CStr,
    follow: bool,
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    no_clobber: bool,
) -> PaxResult<bool> {
    let flags = if follow { libc::AT_SYMLINK_FOLLOW } else { 0 };
    let link = || {
        let r = unsafe {
            libc::linkat(
                from_dir,
                from_name.as_ptr(),
                dirfd.as_raw_fd(),
                name.as_ptr(),
                flags,
            )
        };
        if r != 0 {
            return Err(std::io::Error::last_os_error());
        }
        Ok(())
    };

    match link() {
        Ok(()) => return Ok(false),
        Err(e) if e.raw_os_error() == Some(libc::EEXIST) => {}
        Err(e) => return Err(e.into()),
    }

    if no_clobber {
        return Ok(false);
    }
    if let Some(dst) = stat_at(dirfd, name) {
        let src_flags = if follow { 0 } else { libc::AT_SYMLINK_NOFOLLOW };
        let resolved = fstatat(from_dir, from_name, src_flags);
        let link = follow
            .then(|| fstatat(from_dir, from_name, libc::AT_SYMLINK_NOFOLLOW))
            .flatten();
        if [resolved, link]
            .iter()
            .flatten()
            .any(|src| file_id(src) == file_id(&dst))
        {
            return Ok(true);
        }
    }

    unlink_at(dirfd, name)?;
    link()?;
    Ok(false)
}

/// Open a source file from the descriptor of the directory it was found in.
///
/// One component, resolved once, rather than a whole pathname re-resolved by
/// the kernel on every call -- which is what let a source tree another process
/// could modify redirect a read outside the tree pax was asked to archive.
///
/// `O_NONBLOCK` is not optional here. `O_NOFOLLOW` refuses a symbolic link, but
/// it does not stop a regular file being replaced by a FIFO between the walk's
/// `fstatat` and this `openat`, and a blocking `open` of a FIFO with no writer
/// never returns. Opening non-blocking and then checking what was actually
/// opened turns that from a hang into a diagnostic.
///
/// The `fstat` on the descriptor doubles as the check that the name still
/// refers to the file the walk saw: a mismatch fails closed.
pub(crate) fn open_source_file(
    dir_fd: libc::c_int,
    name: &CStr,
    follow: bool,
    expected: (u64, u64),
) -> std::io::Result<File> {
    let mut flags = libc::O_RDONLY | libc::O_CLOEXEC | libc::O_NONBLOCK;
    if !follow {
        flags |= libc::O_NOFOLLOW;
    }

    let fd = unsafe { libc::openat(dir_fd, name.as_ptr(), flags) };
    if fd < 0 {
        return Err(std::io::Error::last_os_error());
    }
    let file = unsafe { File::from_raw_fd(fd) };

    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    if unsafe { libc::fstat(file.as_raw_fd(), &mut st) } != 0 {
        return Err(std::io::Error::last_os_error());
    }

    if st.st_mode & libc::S_IFMT != libc::S_IFREG {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            "source file changed type before it could be read",
        ));
    }
    if file_id(&st) != expected {
        return Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "source file was replaced before it could be read",
        ));
    }

    // Nothing below reads this descriptor expecting non-blocking semantics.
    let cur = unsafe { libc::fcntl(file.as_raw_fd(), libc::F_GETFL) };
    if cur >= 0 {
        unsafe { libc::fcntl(file.as_raw_fd(), libc::F_SETFL, cur & !libc::O_NONBLOCK) };
    }

    Ok(file)
}

/// `-t`: put back the access time that reading a file disturbed.
///
/// Through the descriptor the data came from rather than by name: resolving
/// the name again could stamp a different file -- a symbolic link's target, or
/// whatever replaced the name meanwhile. `UTIME_OMIT` leaves the modification
/// time as it is instead of reading it back to write it again.
///
/// POSIX makes -t conditional on the user having "the permissions required by
/// futimens()", so a file the user may not stamp (EPERM) is left alone without
/// comment. Any other failure is a warning: the file itself was read.
pub(crate) fn restore_atime(fd: BorrowedFd<'_>, path: &Path, metadata: &ftw::Metadata) {
    use std::os::unix::fs::MetadataExt;

    let times = [
        libc::timespec {
            tv_sec: metadata.atime() as libc::time_t,
            tv_nsec: metadata.atime_nsec() as libc::c_long,
        },
        libc::timespec {
            tv_sec: 0,
            tv_nsec: libc::UTIME_OMIT,
        },
    ];
    if unsafe { libc::futimens(fd.as_raw_fd(), times.as_ptr()) } == 0 {
        return;
    }
    let err = std::io::Error::last_os_error();
    if err.raw_os_error() != Some(libc::EPERM) {
        let mut line = b"pax: warning: cannot reset atime on ".to_vec();
        line.extend_from_slice(crate::rawpath::as_bytes(path));
        line.extend_from_slice(format!(": {}", err).as_bytes());
        crate::escape::write_stderr_line(&line);
    }
}

/// `-t` for a directory, once the walk has finished reading it.
///
/// The walk's own descriptor for it is gone by then, so it is opened again
/// from the directory it was found in -- opening a directory does not touch
/// its access time, only reading it does -- and stamped only if it is still
/// the directory the walk saw.
///
/// The name is followed: under -H/-L it may be a symbolic link the walk
/// followed to reach the directory, and the walk's entry no longer says so
/// once the directory has been left. Following is safe because the identity
/// check is what decides: a link that leads anywhere but the directory the
/// walk read stamps nothing.
pub(crate) fn restore_dir_atime(entry: &ftw::Entry<'_>) {
    use std::os::unix::fs::MetadataExt;

    let Some(metadata) = entry.metadata() else {
        return;
    };
    let flags = libc::O_RDONLY | libc::O_DIRECTORY | libc::O_CLOEXEC;
    let fd = unsafe { libc::openat(entry.dir_fd(), entry.file_name().as_ptr(), flags) };
    if fd < 0 {
        return;
    }
    let dir = unsafe { OwnedFd::from_raw_fd(fd) };
    if fstat(dir.as_fd()).is_some_and(|st| file_id(&st) == (metadata.dev(), metadata.ino())) {
        restore_atime(dir.as_fd(), entry.path().as_inner(), metadata);
    }
}

/// The attributes a copied or extracted file takes from its source, whether that
/// source is an archive member or a file on disk.
pub(crate) struct Attrs {
    pub mode: u32,
    pub uid: u32,
    pub gid: u32,
    pub mtime: i64,
    pub mtime_nsec: i64,
    /// `None` when the source recorded no access time, in which case the
    /// modification time stands in for it.
    pub atime: Option<i64>,
    pub atime_nsec: i64,
}

/// Which of those attributes the user asked to keep (`-p`).
pub(crate) struct AttrPolicy {
    pub preserve_owner: bool,
    pub preserve_perms: bool,
    pub preserve_mtime: bool,
    pub preserve_atime: bool,
    pub umask: u32,
}

impl AttrPolicy {
    /// The permission bits to apply, given whether the owner was set.
    ///
    /// The set-user-ID and set-group-ID bits are kept only when `owner_set`
    /// says the file now belongs to the user and group recorded in the source.
    /// POSIX, -p: "if ... the user ID and group ID are not preserved for any
    /// reason, pax shall not set the S_ISUID and S_ISGID bits". Without
    /// `-p o`/`-p e` that is never, and a chown refused with EPERM leaves the
    /// file belonging to whoever ran pax -- a set-id bit there would hand out
    /// that user's identity, not the archived one. Without `-p p`/`-p e` the
    /// file is created by the normal file-creation action, so the mode is
    /// modified by the umask exactly as `open()` or `mkdir()` would do.
    pub fn mode(&self, attrs: &Attrs, owner_set: bool) -> u32 {
        let mut mode = attrs.mode;
        if !(self.preserve_owner && owner_set) {
            #[allow(clippy::unnecessary_cast)] // u16 on macOS, u32 on Linux
            let setid = !((libc::S_ISUID | libc::S_ISGID) as u32);
            mode &= setid;
        }
        if !self.preserve_perms {
            mode &= !self.umask;
        }
        mode
    }

    /// The permission bits to *create* with, as opposed to the ones the
    /// finished file ends up with.
    ///
    /// Never set-user-ID or set-group-ID, whatever `-p` asked for. A member is
    /// created before its contents are written, so passing the archived mode
    /// straight to `open()` leaves a set-id file -- owned by whoever is running
    /// pax -- executable for the whole duration of the copy. Extracting an
    /// untrusted archive as root that way hands out a root shell to anyone who
    /// wins the race to `exec` it.
    ///
    /// `set_attrs_fd` applies [`AttrPolicy::mode`] through the descriptor once
    /// the data is complete, so any set-id bit the archive legitimately carries
    /// arrives then, on a file whose contents are already final.
    pub fn creation_mode(&self, attrs: &Attrs) -> u32 {
        self.mode(attrs, false) & 0o777
    }

    /// The times to apply, or `None` when neither was asked for.
    ///
    /// `UTIME_OMIT` leaves the one that was not asked for exactly as it is,
    /// which is both simpler and more accurate than reading it back first.
    pub fn times(&self, attrs: &Attrs) -> Option<[libc::timespec; 2]> {
        if !self.preserve_mtime && !self.preserve_atime {
            return None;
        }
        let omit = libc::timespec {
            tv_sec: 0,
            tv_nsec: libc::UTIME_OMIT,
        };
        let atime = if self.preserve_atime {
            match attrs.atime {
                Some(sec) => libc::timespec {
                    tv_sec: sec as libc::time_t,
                    tv_nsec: attrs.atime_nsec as _,
                },
                None => libc::timespec {
                    tv_sec: attrs.mtime as libc::time_t,
                    tv_nsec: attrs.mtime_nsec as _,
                },
            }
        } else {
            omit
        };
        let mtime = if self.preserve_mtime {
            libc::timespec {
                tv_sec: attrs.mtime as libc::time_t,
                tv_nsec: attrs.mtime_nsec as _,
            }
        } else {
            omit
        };
        Some([atime, mtime])
    }
}

/// Apply owner, mode and times to an already-open file.
///
/// Nothing here names a path, so none of it can be redirected by a name that
/// changed after the file was created.
pub(crate) fn set_attrs_fd(
    fd: BorrowedFd<'_>,
    attrs: &Attrs,
    policy: &AttrPolicy,
) -> PaxResult<()> {
    set_attrs(&AttrTarget::Fd(fd), attrs, policy)
}

/// What the attribute calls of `set_attrs` act on.
enum AttrTarget<'a> {
    Fd(BorrowedFd<'a>),
    /// A name that cannot be redirected: see `set_attrs_search_only`.
    #[cfg(any(target_os = "linux", target_os = "android"))]
    Path(&'a CStr),
}

impl AttrTarget<'_> {
    fn chown(&self, uid: libc::uid_t, gid: libc::gid_t) -> libc::c_int {
        match self {
            AttrTarget::Fd(fd) => unsafe { libc::fchown(fd.as_raw_fd(), uid, gid) },
            #[cfg(any(target_os = "linux", target_os = "android"))]
            AttrTarget::Path(p) => unsafe { libc::chown(p.as_ptr(), uid, gid) },
        }
    }

    fn chmod(&self, mode: libc::mode_t) -> libc::c_int {
        match self {
            AttrTarget::Fd(fd) => unsafe { libc::fchmod(fd.as_raw_fd(), mode) },
            #[cfg(any(target_os = "linux", target_os = "android"))]
            AttrTarget::Path(p) => unsafe { libc::chmod(p.as_ptr(), mode) },
        }
    }

    fn utimens(&self, times: &[libc::timespec; 2]) -> libc::c_int {
        match self {
            AttrTarget::Fd(fd) => unsafe { libc::futimens(fd.as_raw_fd(), times.as_ptr()) },
            #[cfg(any(target_os = "linux", target_os = "android"))]
            AttrTarget::Path(p) => unsafe {
                libc::utimensat(libc::AT_FDCWD, p.as_ptr(), times.as_ptr(), 0)
            },
        }
    }
}

/// `set_attrs_fd`, for any `AttrTarget`.
fn set_attrs(target: &AttrTarget<'_>, attrs: &Attrs, policy: &AttrPolicy) -> PaxResult<()> {
    // Owner first: a successful chown may clear the set-id bits, and whether it
    // succeeded decides whether they may be set at all.
    let owner_set = policy.preserve_owner
        && set_owner(attrs.uid, attrs.gid, |uid, gid| target.chown(uid, gid))?;

    if target.chmod(policy.mode(attrs, owner_set) as libc::mode_t) != 0 {
        return Err(std::io::Error::last_os_error().into());
    }

    if let Some(times) = policy.times(attrs) {
        if target.utimens(&times) != 0 {
            eprintln!(
                "pax: warning: cannot set times: {}",
                std::io::Error::last_os_error()
            );
            crate::error::note_error();
        }
    }

    Ok(())
}

/// Set the owner to `uid`/`gid` with `chown`, one of the chown calls, and say
/// whether it was set.
///
/// `(uid_t)-1` and `(gid_t)-1` are not ids: every chown call reads them as
/// "leave this one alone" and then succeeds having changed nothing. A source
/// recording one cannot have its owner restored, and is diagnosed as such --
/// a success here would let the set-id bits through on a file still owned by
/// whoever ran pax.
pub(crate) fn set_owner<F>(uid: u32, gid: u32, chown: F) -> PaxResult<bool>
where
    F: FnOnce(libc::uid_t, libc::gid_t) -> libc::c_int,
{
    if uid == u32::MAX || gid == u32::MAX {
        eprintln!("pax: cannot change owner: invalid user or group ID");
        crate::error::note_error();
        return Ok(false);
    }
    chown_result(chown(uid, gid))
}

/// Whether a `chown` call, given its return value, set the owner.
///
/// EPERM -- usually not being root -- is diagnosed and otherwise tolerated: the
/// file keeps whoever ran pax as its owner, and the caller must then withhold
/// the set-id bits (see [`AttrPolicy::mode`]). Any other failure is an error.
fn chown_result(r: libc::c_int) -> PaxResult<bool> {
    if r == 0 {
        return Ok(true);
    }
    let err = std::io::Error::last_os_error();
    if err.raw_os_error() == Some(libc::EPERM) {
        eprintln!("pax: cannot change owner: Operation not permitted");
        crate::error::note_error();
        return Ok(false);
    }
    Err(err.into())
}

/// Apply owner and times to a name that cannot be opened for the purpose -- a
/// symbolic link, whose own mode bits carry no meaning and which must never be
/// followed to reach them.
///
/// Returns whether the owner was set, for a caller that goes on to apply a mode.
pub(crate) fn set_link_attrs_at(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    attrs: &Attrs,
    policy: &AttrPolicy,
) -> PaxResult<bool> {
    let owner_set = policy.preserve_owner
        && set_owner(attrs.uid, attrs.gid, |uid, gid| unsafe {
            libc::fchownat(
                dirfd.as_raw_fd(),
                name.as_ptr(),
                uid,
                gid,
                libc::AT_SYMLINK_NOFOLLOW,
            )
        })?;

    if let Some(times) = policy.times(attrs) {
        let r = unsafe {
            libc::utimensat(
                dirfd.as_raw_fd(),
                name.as_ptr(),
                times.as_ptr(),
                libc::AT_SYMLINK_NOFOLLOW,
            )
        };
        if r != 0 {
            eprintln!(
                "pax: warning: cannot set times: {}",
                std::io::Error::last_os_error()
            );
            crate::error::note_error();
        }
    }

    Ok(owner_set)
}

/// Set the mode of a name below `dirfd` that cannot be opened for the purpose
/// -- a FIFO, whose open blocks, or a device, whose open can act on the device
/// -- never following a symbolic link: one found there is refused, and one
/// swapped in after that check is not followed either.
pub(crate) fn chmod_at(dirfd: BorrowedFd<'_>, name: &CStr, mode: u32) -> PaxResult<()> {
    match stat_at(dirfd, name) {
        None => return Err(std::io::Error::last_os_error().into()),
        Some(st) if st.st_mode & libc::S_IFMT == libc::S_IFLNK => {
            return Err(PaxError::InvalidHeader(
                "refusing to set permissions through a symbolic link".to_string(),
            ))
        }
        Some(_) => {}
    }
    chmod_nofollow(dirfd, name, mode as libc::mode_t).map_err(Into::into)
}

/// The race-free half of `chmod_at`: at worst, a symbolic link put at `name`
/// since it was checked is refused or has its own mode set.
///
/// Linux's `fchmodat` refuses `AT_SYMLINK_NOFOLLOW` (until `fchmodat2`), and
/// `fchmod` refuses an `O_PATH` descriptor. So the name is opened `O_PATH`,
/// which opens nothing on the device or FIFO itself, without following a
/// link; the descriptor is checked to be no link, and its `/proc/self/fd`
/// entry, which resolves to the very file it holds, takes the chmod -- as in
/// `set_attrs_search_only`.
#[cfg(any(target_os = "linux", target_os = "android"))]
fn chmod_nofollow(dirfd: BorrowedFd<'_>, name: &CStr, mode: libc::mode_t) -> std::io::Result<()> {
    let flags = libc::O_PATH | libc::O_NOFOLLOW | libc::O_CLOEXEC;
    let fd = unsafe { libc::openat(dirfd.as_raw_fd(), name.as_ptr(), flags) };
    if fd < 0 {
        return Err(std::io::Error::last_os_error());
    }
    let fd = unsafe { OwnedFd::from_raw_fd(fd) };
    match fstat(fd.as_fd()) {
        None => return Err(std::io::Error::last_os_error()),
        Some(st) if st.st_mode & libc::S_IFMT == libc::S_IFLNK => {
            return Err(std::io::Error::from_raw_os_error(libc::ELOOP))
        }
        Some(_) => {}
    }
    let path = CString::new(format!("/proc/self/fd/{}", fd.as_raw_fd()))
        .expect("a formatted number has no NUL");
    if AttrTarget::Path(&path).chmod(mode) != 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(())
}

/// Elsewhere `fchmodat` takes `AT_SYMLINK_NOFOLLOW`.
#[cfg(not(any(target_os = "linux", target_os = "android")))]
fn chmod_nofollow(dirfd: BorrowedFd<'_>, name: &CStr, mode: libc::mode_t) -> std::io::Result<()> {
    let flags = libc::AT_SYMLINK_NOFOLLOW;
    if unsafe { libc::fchmodat(dirfd.as_raw_fd(), name.as_ptr(), mode, flags) } != 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(())
}

/// `fstatat` with `AT_SYMLINK_NOFOLLOW`, for asking what a name *is* without
/// following it anywhere.
pub(crate) fn stat_at(dirfd: BorrowedFd<'_>, name: &CStr) -> Option<libc::stat> {
    fstatat(dirfd.as_raw_fd(), name, libc::AT_SYMLINK_NOFOLLOW)
}

/// `fstat` of an open descriptor.
fn fstat(fd: BorrowedFd<'_>) -> Option<libc::stat> {
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    let r = unsafe { libc::fstat(fd.as_raw_fd(), &mut st) };
    (r == 0).then_some(st)
}

/// `fstatat` from a raw directory descriptor such as a walk entry's.
fn fstatat(dirfd: libc::c_int, name: &CStr, flags: libc::c_int) -> Option<libc::stat> {
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    let r = unsafe { libc::fstatat(dirfd, name.as_ptr(), &mut st, flags) };
    (r == 0).then_some(st)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn member(path: &str) -> MemberPath {
        MemberPath::parse(Path::new(path)).unwrap().unwrap()
    }

    /// The inode a descriptor refers to.
    fn ino_of(fd: &OwnedFd) -> u64 {
        stat_at(fd.as_fd(), c".").unwrap().st_ino
    }

    fn ino_at(path: &Path) -> u64 {
        use std::os::unix::fs::MetadataExt;
        std::fs::symlink_metadata(path).unwrap().ino()
    }

    /// A symbolic link swapped in for a FIFO or device between the check and
    /// the chmod must not carry the mode to the file it points at.
    #[test]
    fn test_chmod_nofollow_never_reaches_through_a_symlink() {
        use std::os::unix::fs::PermissionsExt;
        let tmp = plib::tmp::TempDir::new().unwrap();
        let target = tmp.path().join("target");
        std::fs::write(&target, "").unwrap();
        std::fs::set_permissions(&target, std::fs::Permissions::from_mode(0o644)).unwrap();
        std::os::unix::fs::symlink(&target, tmp.path().join("link")).unwrap();
        let dir = File::open(tmp.path()).unwrap();

        // Refused (Linux) or applied to the link itself (macOS, the BSDs).
        let _ = chmod_nofollow(dir.as_fd(), c"link", 0o600);
        let mode = std::fs::metadata(&target).unwrap().permissions().mode();
        assert_eq!(mode & 0o7777, 0o644);

        // What it is for: a FIFO, which cannot be opened without blocking.
        let fifo = tmp.path().join("fifo");
        let fifo_c = CString::new(fifo.as_os_str().as_bytes()).unwrap();
        assert_eq!(unsafe { libc::mkfifo(fifo_c.as_ptr(), 0o644) }, 0);
        chmod_nofollow(dir.as_fd(), c"fifo", 0o604).unwrap();
        let mode = std::fs::metadata(&fifo).unwrap().permissions().mode();
        assert_eq!(mode & 0o7777, 0o604);
    }

    #[test]
    fn test_member_path_components() {
        let m = member("/x/./y/../z/leaf");
        assert_eq!(m.depth(), 2);
        assert_eq!(m.dirs().collect::<Vec<_>>(), [c"x", c"z"]);
        assert_eq!(m.leaf.as_c_str(), c"leaf");
        assert_eq!(m.display, Path::new("x/z/leaf"));
        assert!(MemberPath::parse(Path::new("a/../..")).unwrap().is_none());
        assert!(MemberPath::parse(Path::new("a\0b/c")).is_err());
    }

    /// Each member reopens only the directories its path does not share with
    /// the last one's, so a depth-first walk opens one directory per member.
    #[test]
    fn test_parent_of_follows_the_last_chain() {
        let dir = plib::tmp::TempDir::new().unwrap();
        let tree = DirTree::open_path(dir.path()).unwrap();
        let levels = || tree.chain.borrow().levels.len();

        let fd = tree.parent_of(&member("a/b/c/x"), true).unwrap();
        assert_eq!(ino_of(&fd), ino_at(&dir.path().join("a/b/c")));
        assert_eq!(levels(), 3);

        let fd = tree.parent_of(&member("a/b/y"), true).unwrap();
        assert_eq!(ino_of(&fd), ino_at(&dir.path().join("a/b")));
        assert_eq!(levels(), 2);

        let fd = tree.parent_of(&member("a/b/c/d/z"), true).unwrap();
        assert_eq!(ino_of(&fd), ino_at(&dir.path().join("a/b/c/d")));
        assert_eq!(levels(), 4);

        let fd = tree.parent_of(&member("e/z"), true).unwrap();
        assert_eq!(ino_of(&fd), ino_at(&dir.path().join("e")));
        assert_eq!(levels(), 1);
    }

    /// A member naming one of the chain's directories as its leaf -- the
    /// member that could replace it -- cuts the chain there, so a symbolic
    /// link put in its place is met by a fresh `O_NOFOLLOW` open.
    #[test]
    fn test_parent_of_never_reuses_a_replaceable_directory() {
        let dir = plib::tmp::TempDir::new().unwrap();
        let outside = plib::tmp::TempDir::new().unwrap();
        let tree = DirTree::open_path(dir.path()).unwrap();

        tree.parent_of(&member("a/b/x"), true).unwrap();
        tree.parent_of(&member("a/b"), true).unwrap();
        std::fs::remove_dir(dir.path().join("a/b")).unwrap();
        std::os::unix::fs::symlink(outside.path(), dir.path().join("a/b")).unwrap();

        assert!(tree.parent_of(&member("a/b/z"), true).is_err());
        assert_eq!(std::fs::read_dir(outside.path()).unwrap().count(), 0);
    }

    /// Past the descriptor budget the chain stops growing, and a walk still
    /// reaches the right directory.
    #[test]
    fn test_parent_of_past_the_level_budget() {
        let dir = plib::tmp::TempDir::new().unwrap();
        let mut tree = DirTree::open_path(dir.path()).unwrap();
        tree.max_levels = 2;

        for (path, parent) in [
            ("a/b/c/d/x", "a/b/c/d"),
            ("a/b/c/d/y", "a/b/c/d"),
            ("a/b/c/d/e/z", "a/b/c/d/e"),
            ("a/b/c/w", "a/b/c"),
            ("a/v", "a"),
        ] {
            let fd = tree.parent_of(&member(path), true).unwrap();
            assert_eq!(ino_of(&fd), ino_at(&dir.path().join(parent)), "{path}");
            assert!(tree.chain.borrow().levels.len() <= 2, "{path}");
        }
    }

    fn attrs(mode: u32) -> Attrs {
        Attrs {
            mode,
            uid: 0,
            gid: 0,
            mtime: 0,
            mtime_nsec: 0,
            atime: None,
            atime_nsec: 0,
        }
    }

    fn policy(preserve_owner: bool, preserve_perms: bool) -> AttrPolicy {
        AttrPolicy {
            preserve_owner,
            preserve_perms,
            preserve_mtime: false,
            preserve_atime: false,
            umask: 0o022,
        }
    }

    /// The whole point of the method: a member is created before its contents
    /// are written, so a set-id bit present at creation time is executable by
    /// anyone for the duration of the copy. No `-p` combination may produce
    /// one -- including `-p e`, which is the combination that asks for set-id
    /// to be preserved and so is the one that used to leave the window open.
    #[test]
    fn test_creation_mode_never_carries_set_id() {
        for mode in [0o4755, 0o2755, 0o6755, 0o4777] {
            for owner in [false, true] {
                for perms in [false, true] {
                    let created = policy(owner, perms).creation_mode(&attrs(mode));
                    assert_eq!(
                        created & 0o7000,
                        0,
                        "mode {mode:o} created as {created:o} with \
                         preserve_owner={owner} preserve_perms={perms}"
                    );
                }
            }
        }
    }

    /// It must also never be *wider* than the mode the file ends up with,
    /// or the window trades one exposure for another.
    #[test]
    fn test_creation_mode_is_never_wider_than_the_final_mode() {
        for mode in [0o4755, 0o755, 0o600, 0o777, 0o000, 0o2750] {
            for owner in [false, true] {
                for perms in [false, true] {
                    let p = policy(owner, perms);
                    let created = p.creation_mode(&attrs(mode));
                    let final_mode = p.mode(&attrs(mode), true);
                    assert_eq!(
                        created & !final_mode,
                        0,
                        "mode {mode:o}: created {created:o} grants what final {final_mode:o} does not"
                    );
                }
            }
        }
    }

    /// And it must still carry the ordinary permission bits, or extraction
    /// would produce unreadable files and the fix would be a regression.
    #[test]
    fn test_creation_mode_keeps_the_permission_bits() {
        // -p p preserves the mode exactly; the umask does not apply.
        assert_eq!(policy(true, true).creation_mode(&attrs(0o4755)), 0o755);
        // Without -p p the normal file-creation action applies the umask.
        assert_eq!(policy(false, false).creation_mode(&attrs(0o4777)), 0o755);
    }

    /// Set-id bits survive only when ownership was asked for *and* the chown
    /// took: a refused chown leaves the file owned by whoever ran pax.
    #[test]
    fn test_mode_keeps_set_id_only_when_the_owner_was_set() {
        assert_eq!(policy(true, true).mode(&attrs(0o6755), true), 0o6755);
        assert_eq!(policy(true, true).mode(&attrs(0o6755), false), 0o755);
        assert_eq!(policy(false, true).mode(&attrs(0o6755), true), 0o755);
    }

    /// A link onto a name that already is the source must keep the file:
    /// unlinking it to retry would destroy what was to be linked.
    #[test]
    fn test_link_replacing_onto_itself_keeps_the_file() {
        let temp = plib::tmp::TempDir::new().unwrap();
        std::fs::write(temp.path().join("f"), "DATA\n").unwrap();
        let dir = DirTree::open_path(temp.path()).unwrap();
        let f = CString::new("f").unwrap();

        let same = link_replacing(dir.root().as_raw_fd(), &f, dir.root(), &f, false).unwrap();
        assert!(same, "the name was already the source");
        assert_eq!(
            std::fs::read_to_string(temp.path().join("f")).unwrap(),
            "DATA\n"
        );
    }

    /// Any other file in the way is still replaced by the link.
    #[test]
    fn test_link_replacing_replaces_another_file() {
        let temp = plib::tmp::TempDir::new().unwrap();
        std::fs::write(temp.path().join("f"), "DATA\n").unwrap();
        std::fs::write(temp.path().join("g"), "old\n").unwrap();
        let dir = DirTree::open_path(temp.path()).unwrap();
        let f = CString::new("f").unwrap();
        let g = CString::new("g").unwrap();

        let same = link_replacing(dir.root().as_raw_fd(), &f, dir.root(), &g, false).unwrap();
        assert!(!same);
        assert_eq!(
            std::fs::read_to_string(temp.path().join("g")).unwrap(),
            "DATA\n"
        );
    }
}
