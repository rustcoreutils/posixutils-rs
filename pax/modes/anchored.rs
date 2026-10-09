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
use crate::modes::made::{self, verify_made_dir, MadeNode, MadeTrust};
use crate::modes::pins::MadeFile;
use plib::madefs::{cvt, fstat, fstatat, lstat_at};
use plib::madefs::{ChainTrust, FoundDir, NamedAnchor, Preserve, SEARCH_ONLY};
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

    /// The member's own path as a registry key: its components, each ended
    /// by a NUL, as `Chain` and `dir_key` spell them.
    pub(crate) fn key(&self) -> Vec<u8> {
        let mut key = self.dirs.clone();
        key.extend_from_slice(self.leaf.to_bytes_with_nul());
        key
    }

    /// The key (`key`) of the directory made of the member's first `n`
    /// directory components.
    fn dir_key(&self, n: usize) -> &[u8] {
        let end = self
            .dirs
            .iter()
            .enumerate()
            .filter(|&(_, &b)| b == 0)
            .nth(n - 1)
            .map_or(0, |(i, _)| i + 1);
        &self.dirs[..end]
    }
}

// Directories walked through are opened `plib::madefs::SEARCH_ONLY`. (An
// `O_PATH` descriptor takes attributes only through a verified procfs,
// `set_attrs_search_only`, which is Linux's alone.)

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
    /// The anchor, shared with the trust that holds it weakly (`root_trust`).
    root: Rc<OwnedFd>,
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
    /// How many levels `chain` may hold: each is an open descriptor. A directory deeper than
    /// that is closed once walked through, and its trust (`ChainTrust`), worked out only when a
    /// directory found below it is to be given a mode or owner, then counts as one others may
    /// write: an existing directory that deep keeps its attributes under -p, failing closed.
    max_levels: usize,
    /// The parent most recently walked to, and the trust it hands the
    /// directories found in it, so consecutive members of one directory
    /// deeper than `max_levels` still share one walk.
    last_parent: RefCell<Option<LastParent>>,
    /// The trust the anchor hands the directories found in it: the root of
    /// the trust every walk carries down (`ChainTrust`).
    root_trust: ChainTrust,
    /// The anchor as the user named it, held while `root_trust` may be asked (`open_dest`).
    _named: Option<NamedAnchor>,
    /// `(st_dev, st_ino)` of the directories this run created only to hold a
    /// member below them, each with the member path (`MemberPath::key`) it
    /// was made at. Such a directory is not a pre-existing file: a member
    /// that names it later (`find -depth` order) still gives it its
    /// attributes -- a member of that name: met under any other, it was
    /// renamed there, and is one found existing.
    implicit: RefCell<HashMap<(u64, u64), Vec<u8>>>,
    /// `(st_dev, st_ino)` of directories found where this run had just made
    /// one: renamed over it by someone else. A member naming one later does
    /// not take it for a pre-existing directory and give it attributes.
    replaced: RefCell<HashSet<(u64, u64)>>,
    /// `(st_dev, st_ino)` of directories this run made only to hold a member
    /// below them, whose owner could not be verified (`MadeTrust::
    /// ParentOwnerOnly`). They are used, but a member naming one later gives
    /// it no attributes, as it would not to one made for that member.
    unverified: RefCell<HashSet<(u64, u64)>>,
    /// `(st_dev, st_ino)` of directories this run made, verified through a
    /// descriptor to be the ones made, for a member naming them -- or made
    /// to hold members below them and since claimed by the member naming
    /// them -- each with the member path it was made at. Unlike a directory
    /// found existing, one of these takes a member's attributes without the
    /// existing-directory rule (`Standing::Made`), but only when met at that
    /// path and still as this run left it (`left_as`). Met at any other
    /// path, it is one found existing there: otherwise someone who can
    /// rename in the destination could rename a directory this run made to
    /// another member's name, and have that member's mode or owner given to
    /// it without the existing-directory rule.
    made: RefCell<HashMap<(u64, u64), Vec<u8>>>,
    /// What each directory in `implicit` and `made` was left as by this run
    /// (`LeftAs`): its inode number alone cannot tell it from a directory
    /// someone else made after removing it, which can be given the same
    /// number (`standing`).
    left_as: RefCell<HashMap<(u64, u64), LeftAs>>,
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

    /// Anchor at a directory named by the caller, trusted as named.
    pub(crate) fn open_path(path: &Path) -> PaxResult<Self> {
        Self::open_dest(path, false)
    }

    /// Anchor at copy mode's destination directory, named by the user as `path`. When a mode
    /// or owner is to be preserved (`preserving`), how `path` reaches it matters: through a
    /// symbolic link in a directory others can write, it is wherever the link's owner chose,
    /// and trusts nothing (`ChainTrust::named`).
    pub(crate) fn open_dest(path: &Path, preserving: bool) -> PaxResult<Self> {
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
        let root = Rc::new(unsafe { OwnedFd::from_raw_fd(fd) });
        let named = if preserving {
            Some(ChainTrust::named(path, &root)?)
        } else {
            None
        };
        Ok(DirTree {
            root_trust: match &named {
                Some(named) => named.hands.clone(),
                None => ChainTrust::anchor(&root)?,
            },
            _named: named,
            root,
            chain: RefCell::new(Chain::default()),
            max_levels: cached_levels_budget(),
            last_parent: RefCell::new(None),
            implicit: RefCell::new(HashMap::new()),
            replaced: RefCell::new(HashSet::new()),
            unverified: RefCell::new(HashSet::new()),
            made: RefCell::new(HashMap::new()),
            left_as: RefCell::new(HashMap::new()),
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
        self.parent_and_trust(member, create_missing)
            .map(|(fd, _)| fd)
    }

    /// `parent_of`, with the trust that parent hands the directories found
    /// in it (`ChainTrust`), carried down the walk from the anchor.
    fn parent_and_trust(
        &self,
        member: &MemberPath,
        create_missing: bool,
    ) -> PaxResult<(Rc<OwnedFd>, ChainTrust)> {
        if let Some(last) = &*self.last_parent.borrow() {
            if last.dirs == member.dirs {
                return Ok((Rc::clone(&last.fd), last.trust.clone()));
            }
        }

        let walked = self.walk_chain(member, create_missing);
        // A walk that fails part-way has still extended the chain past the
        // last parent, which the shortcut above would then skip truncating:
        // a later member naming one of those directories as its leaf could
        // replace it while the chain kept its descriptor. With no last parent
        // the next member goes through the chain, and cuts it as it should.
        *self.last_parent.borrow_mut() = match &walked {
            Ok((fd, trust)) => Some(LastParent {
                dirs: member.dirs.clone(),
                fd: Rc::clone(fd),
                trust: trust.clone(),
            }),
            Err(_) => None,
        };
        walked
    }

    /// The walk `parent_and_trust` does through the chain, reopening only the
    /// components `member` does not share with it, and carrying the trust
    /// down from the anchor: each directory found existing hands on what it
    /// was handed and its own (`ChainTrust::found`); one this run made, at
    /// that path, starts afresh (`ChainTrust::made`).
    fn walk_chain(
        &self,
        member: &MemberPath,
        create_missing: bool,
    ) -> PaxResult<(Rc<OwnedFd>, ChainTrust)> {
        let mut chain = self.chain.borrow_mut();
        let shared = chain.shared_with(member);
        chain.truncate(shared);
        let mut cur = chain.levels.last().map(|level| Rc::clone(&level.fd));
        let mut trust = chain
            .levels
            .last()
            .map_or(&self.root_trust, |level| &level.trust)
            .clone();

        for (level, comp) in member.dirs().enumerate().skip(shared) {
            let at = cur.as_ref().map_or(self.root.as_fd(), |fd| fd.as_fd());
            let key = member.dir_key(level + 1);
            let (next, origin) = open_or_create_dir_at(at, comp, create_missing)?;
            let st = match self.admit(next.as_fd(), origin) {
                Ok(st) => st,
                Err(e) => {
                    // `refuse_replaced` cannot reach the chain from here.
                    chain.truncate(0);
                    return Err(e);
                }
            };
            let id = file_id(&st);
            match origin {
                DirOrigin::Made(fresh) => self.record_made(fresh, key, true),
                DirOrigin::Found | DirOrigin::Replaced => {
                    self.pre_run_mtimes
                        .borrow_mut()
                        .entry(id)
                        .or_insert(mtime_of(&st));
                }
            }
            let next = Rc::new(next);
            trust = match self.standing(&st, key) {
                Standing::Implicit | Standing::Made => ChainTrust::made(&next)?,
                _ => trust.found(&next)?,
            };
            if chain.levels.len() < self.max_levels {
                chain.push(comp, Rc::clone(&next), trust.clone());
            }
            cur = Some(next);
        }
        match cur {
            Some(fd) => Ok((fd, trust)),
            None => Ok((Rc::clone(&self.root), trust)),
        }
    }

    /// What this run knows about the directory with `(st_dev, st_ino)` `id`,
    /// met at the member path `key` (`MemberPath::key`).
    ///
    /// The one registry every site that enters, merges into or stamps a
    /// directory consults: the walk and `open_dir` (through `admit`),
    /// `make_dir_at` and `apply_dir_attrs`. A directory this run made counts
    /// as made only at the path it was made at: one someone renamed to
    /// another member's name is one found existing there. Nor where it is no
    /// longer owned, grouped or moded as this run left it (`LeftAs`): someone
    /// who can write its parent can remove it and make another there, which
    /// the filesystem can give the same inode number.
    fn standing(&self, st: &libc::stat, key: &[u8]) -> Standing {
        let id = file_id(st);
        // Made there, and still as this run left it: an inode number can be
        // handed on to a directory someone else makes in its place (`LeftAs`).
        let made_at = |made: &RefCell<HashMap<(u64, u64), Vec<u8>>>| {
            made.borrow()
                .get(&id)
                .is_some_and(|at| at.as_slice() == key)
                && self.left_as.borrow().get(&id) == Some(&LeftAs::of(st))
        };
        if self.is_replaced(id) {
            Standing::Replaced
        } else if self.unverified.borrow().contains(&id) {
            Standing::Unverified
        } else if made_at(&self.implicit) {
            Standing::Implicit
        } else if made_at(&self.made) {
            Standing::Made
        } else {
            Standing::Ordinary
        }
    }

    /// Whether the directory with `id` was found in place of one this run
    /// made, wherever it is met.
    fn is_replaced(&self, id: (u64, u64)) -> bool {
        self.replaced.borrow().contains(&id)
    }

    /// Record `fresh`, a directory this run has just made at the member path
    /// `key` and verified (`FreshDir::verify`), with the one standing its
    /// trust gives it: made only to hold members below it (`implicit`), or
    /// for a member naming it, or unverified.
    /// The one place the walk and `make_dir_at` both record what they made.
    ///
    /// A directory just made is new, whatever its inode number stood for
    /// before: one removed during the run can hand its number on to the next
    /// directory made. So everything the registry held for the number is
    /// forgotten first -- a stale `made` entry for another path would
    /// otherwise make this one, renamed to that path, pass for that member's.
    ///
    /// It takes a `FreshDir` -- a directory a successful `mkdirat` made and
    /// `verify_made_dir` accepted -- and nothing else, so the forgetting and
    /// the one standing recorded in its place happen together, and only for
    /// a directory proven new. Nothing else ever removes an entry from
    /// `replaced` or `unverified`; a directory found in place of one made is
    /// recorded by `refuse_replaced` instead, which removes nothing.
    fn record_made(&self, fresh: FreshDir, key: &[u8], implicit: bool) {
        let id = fresh.id;
        self.implicit.borrow_mut().remove(&id);
        self.made.borrow_mut().remove(&id);
        self.unverified.borrow_mut().remove(&id);
        self.replaced.borrow_mut().remove(&id);
        self.left_as.borrow_mut().remove(&id);
        let made = match fresh.trust {
            MadeTrust::ParentOwnerOnly => {
                self.unverified.borrow_mut().insert(id);
                return;
            }
            MadeTrust::Full if implicit => &self.implicit,
            MadeTrust::Full => &self.made,
        };
        made.borrow_mut().insert(id, key.to_vec());
        self.left_as.borrow_mut().insert(id, fresh.left_as);
    }

    /// Note what this run has just left the directory `st` it made as, once it has given it
    /// attributes itself (`apply_dir_attrs`).
    ///
    /// Unless that gave it away: owned now by someone other than the user pax runs as and than
    /// the owner it was made with (root under -p o), it is theirs, and no longer counts as made.
    /// They can remove it and make another at its name, which the filesystem can give the same
    /// inode number, owner and mode, so `LeftAs` cannot tell the two apart; a second pending
    /// apply for that name (a member named twice, two sources -s maps onto one) judges what it
    /// finds as found existing (`found_dir_with_mode`). The owner it was made with stays: on a
    /// filesystem that stores no owners that is the mount's, and pax gives it nothing else.
    fn note_left_as(&self, st: &libc::stat) {
        let id = file_id(st);
        let euid = unsafe { libc::geteuid() };
        let mut left_as = self.left_as.borrow_mut();
        let Some(left) = left_as.get_mut(&id) else {
            return;
        };
        if st.st_uid != euid && st.st_uid != left.uid {
            left_as.remove(&id);
            self.implicit.borrow_mut().remove(&id);
            self.made.borrow_mut().remove(&id);
            return;
        }
        *left = LeftAs::of(st);
    }

    /// Record the directory with `id` as found in place of one this run
    /// made, and fail.
    ///
    /// It may be one the walk already holds a descriptor for -- moved away
    /// from its name and renamed back over the new directory made there --
    /// and the chain and `last_parent` reuse their descriptors without asking
    /// again. So every cached descriptor is dropped here, and the next member
    /// walks afresh, through `admit`. (Inside `walk_chain`, which holds the
    /// chain, the walk drops it itself on the way out.)
    fn refuse_replaced(&self, id: (u64, u64)) -> PaxError {
        self.replaced.borrow_mut().insert(id);
        *self.last_parent.borrow_mut() = None;
        if let Ok(mut chain) = self.chain.try_borrow_mut() {
            chain.truncate(0);
        }
        PaxError::Io(made::replaced())
    }

    /// Admit the directory just opened on `fd`, which `open_or_create_dir_at`
    /// says came from `origin`, to be entered: never one found in place of a
    /// directory this run made, now or earlier, whichever member reaches it.
    fn admit(&self, fd: BorrowedFd<'_>, origin: DirOrigin) -> PaxResult<libc::stat> {
        let st = fstat(fd.as_raw_fd())?;
        if origin == DirOrigin::Replaced {
            return Err(self.refuse_replaced(file_id(&st)));
        }
        if self.is_replaced(file_id(&st)) {
            return Err(PaxError::Io(made::replaced()));
        }
        Ok(st)
    }

    /// Open one directory component below `dirfd` without following a
    /// symlink, admitted as the walk admits one (`admit`).
    pub(crate) fn open_dir(
        &self,
        dirfd: BorrowedFd<'_>,
        name: &CStr,
        create_missing: bool,
    ) -> PaxResult<OwnedFd> {
        let (fd, origin) = open_or_create_dir_at(dirfd, name, create_missing)?;
        self.admit(fd.as_fd(), origin)?;
        Ok(fd)
    }

    /// Whether the directory with `id` is one this run made -- to hold
    /// members below it, for a member naming it, or unverified -- wherever it
    /// is met.
    pub(crate) fn made_by_run(&self, id: (u64, u64)) -> bool {
        self.implicit.borrow().contains_key(&id)
            || self.made.borrow().contains_key(&id)
            || self.unverified.borrow().contains(&id)
    }

    /// Whether `st`, met at `member`, is a directory this run created there
    /// only to hold members below it, rather than one that was there before.
    pub(crate) fn is_implicit(&self, st: &libc::stat, member: &MemberPath) -> bool {
        self.standing(st, &member.key()) == Standing::Implicit
    }

    /// `is_implicit`, for the member that names the directory and so gives it
    /// its attributes. That member makes it a directory this run made for a
    /// member (`Standing::Made`): a later member of the same name meets it as
    /// an existing directory -- left alone under -k -- rather than as one
    /// still waiting for its attributes, and it still takes attributes
    /// wherever it is, being verified as made.
    pub(crate) fn claim_implicit(&self, st: &libc::stat, member: &MemberPath) -> bool {
        let id = file_id(st);
        let key = member.key();
        if self.standing(st, &key) != Standing::Implicit {
            return false;
        }
        self.implicit.borrow_mut().remove(&id);
        self.made.borrow_mut().insert(id, key);
        true
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
    levels: Vec<Level>,
}

/// The parent `DirTree` walked to last (`DirTree::last_parent`).
struct LastParent {
    /// The member's directory components, as in `MemberPath::dirs`.
    dirs: Vec<u8>,
    fd: Rc<OwnedFd>,
    /// The trust it hands the directories found in it (`ChainTrust`).
    trust: ChainTrust,
}

/// One directory of `Chain`.
struct Level {
    /// Where its name ends in `Chain::names`.
    end: usize,
    fd: Rc<OwnedFd>,
    /// The trust it hands the directories found in it (`ChainTrust`).
    trust: ChainTrust,
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
        let end = self.levels.last().map_or(0, |level| level.end);
        self.names.truncate(end);
    }

    fn push(&mut self, name: &CStr, fd: Rc<OwnedFd>, trust: ChainTrust) {
        self.names.extend_from_slice(name.to_bytes_with_nul());
        let end = self.names.len();
        self.levels.push(Level { end, fd, trust });
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
    /// Defer `attrs` for the directory `member` names, `id` being the
    /// `(st_dev, st_ino)` of that directory.
    pub(crate) fn push(&mut self, member: &MemberPath, id: (u64, u64), attrs: Attrs) {
        self.0.push(PendingDir {
            path: member.display.clone(),
            depth: member.depth(),
            id,
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
///
/// A directory this run made and verified takes its attributes. One that was
/// already there takes, as libarchive's does, its times by default, its mode
/// only under `-p p` and its owner only under `-p o` -- and those only where
/// nobody but pax's user could have created its name, in its parent or any
/// directory above it up to the anchor (`ChainTrust`, carried down the
/// walk); elsewhere it keeps all its own, and that is diagnosed
/// (`found_dir_with_mode`).
fn apply_dir_attrs(tree: &DirTree, dir: &PendingDir, policy: &AttrPolicy) -> PaxResult<()> {
    let Some(member) = MemberPath::parse(&dir.path)? else {
        return Ok(());
    };
    let opened = tree
        .parent_and_trust(&member, false)
        .and_then(|(parent, trust)| {
            open_dir_for_attrs(parent.as_fd(), &member.leaf).map(|opened| (trust, opened))
        });
    let (trust, (fd, search_only)) = match opened {
        Ok(opened) => opened,
        Err(PaxError::Io(e)) if is_superseded(&e) => return Ok(()),
        Err(e) => return Err(e),
    };
    let Some(st) = fstat(fd.as_raw_fd())
        .ok()
        .filter(|st| file_id(st) == dir.id)
    else {
        return Ok(());
    };
    let (with_mode, ours) = match tree.standing(&st, &member.key()) {
        Standing::Replaced => return Err(PaxError::Io(made::replaced())),
        Standing::Unverified => return Err(attrs_withheld()),
        Standing::Implicit | Standing::Made => (true, true),
        Standing::Ordinary => (found_dir_with_mode(trust, &fd, policy)?, false),
    };
    let set = if search_only {
        set_attrs_search_only(fd.as_fd(), &dir.attrs, policy, with_mode)
    } else {
        set_attrs_with(&AttrTarget::Fd(fd.as_fd()), &dir.attrs, policy, with_mode)
    };
    // What it was given is this run's doing: it is still the directory made.
    if ours {
        if let Ok(st) = fstat(fd.as_raw_fd()) {
            tree.note_left_as(&st);
        }
    }
    set
}

/// For a directory found existing at a member's name, held as `fd`, in a
/// parent handing it `trust`: whether it takes the member's mode, or an error when it is to
/// take nothing at all (`ChainTrust::found_dir`). Its owner it takes only
/// under `-p o`, which `set_attrs_with` already follows; its times, as by
/// default, whenever it takes anything.
fn found_dir_with_mode(
    trust: ChainTrust,
    fd: &impl AsRawFd,
    policy: &AttrPolicy,
) -> PaxResult<bool> {
    let requested = Preserve {
        mode: policy.preserve_perms,
        owner: policy.preserve_owner,
    };
    match trust.found_dir(fd, requested) {
        FoundDir::TimesOnly => Ok(false),
        FoundDir::AsRequested => Ok(policy.preserve_perms),
        FoundDir::LeaveAlone => Err(PaxError::Io(std::io::Error::other(
            "not applying owner, mode or times: the directory was already there, \
             and others can create entries beside it",
        ))),
    }
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
/// alike. Its `self/fd/N` entry, under a `/proc` verified to be procfs
/// (`made::procfs_dir`), is not: it resolves to the very file the descriptor
/// holds, whatever has happened to the name since, so the calls made through
/// it by name cannot be redirected -- not by a `/proc` that is something else
/// either.
#[cfg(target_os = "linux")]
fn set_attrs_search_only(
    fd: BorrowedFd<'_>,
    attrs: &Attrs,
    policy: &AttrPolicy,
    with_mode: bool,
) -> PaxResult<()> {
    let proc_dir = made::procfs_dir()?;
    let name = made::proc_fd_name(fd.as_raw_fd());
    let target = AttrTarget::Proc {
        dir: proc_dir.as_fd(),
        name: &name,
    };
    set_attrs_with(&target, attrs, policy, with_mode)
}

/// Elsewhere a search-only descriptor (`O_SEARCH`) takes the same calls as any
/// other.
#[cfg(not(target_os = "linux"))]
fn set_attrs_search_only(
    fd: BorrowedFd<'_>,
    attrs: &Attrs,
    policy: &AttrPolicy,
    with_mode: bool,
) -> PaxResult<()> {
    set_attrs_with(&AttrTarget::Fd(fd), attrs, policy, with_mode)
}

/// What this run knows about a directory (`DirTree::standing`).
#[derive(Clone, Copy, PartialEq, Eq)]
enum Standing {
    /// Found in place of one this run made: never entered nor stamped.
    Replaced,
    /// Made by this run where its owner could not be verified: entered,
    /// never stamped.
    Unverified,
    /// Made by this run only to hold members below it, awaiting the member
    /// that names it.
    Implicit,
    /// Made and verified by this run for a member naming it, or implicit and
    /// since claimed; met at the member path it was made at, and still owned,
    /// grouped and moded as this run left it (`LeftAs`), which it no longer
    /// is once this run has given it to someone else. Takes a member's
    /// attributes without the existing-directory rule. A directory this run
    /// made, met anywhere else -- renamed to another member's name -- or no
    /// longer as it was left, is `Ordinary`.
    Made,
    /// Found existing: takes its times, and its mode and owner only when
    /// asked for -- and then only where nobody else could have created its
    /// name, here or above it (`plib::madefs::ChainTrust::found_dir`).
    Ordinary,
}

/// Where the directory `open_or_create_dir_at` opened came from.
#[derive(Clone, Copy, PartialEq, Eq)]
enum DirOrigin {
    /// It was already there.
    Found,
    /// This run made it, and it is checked to be the one made, trusted as
    /// far as the `FreshDir` says (`MadeTrust::ParentOwnerOnly`: used, never
    /// given attributes).
    Made(FreshDir),
    /// This run made one, and found another in its place: never to be used,
    /// nor given attributes later.
    Replaced,
}

/// A directory this run has just made with `mkdirat`, accepted by
/// `verify_made_dir` from the descriptor it was opened on: its identity and
/// how far it is trusted. Only `FreshDir::verify` makes one, and only
/// `DirTree::record_made` takes one -- the one place the registry forgets
/// what an inode number stood for, which only a directory proven new may
/// make it do.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct FreshDir {
    id: (u64, u64),
    trust: MadeTrust,
    left_as: LeftAs,
}

/// What this run left a directory it made as: owner, group and permission
/// bits, from its `fstat`. A directory someone else made at the same name
/// after removing this one can have the same `(st_dev, st_ino)`, but not this
/// owner unless they are the user pax runs as -- who could have changed the
/// directory anyway.
///
/// Not its ctime: every entry this run adds below the directory changes that,
/// at too many sites to note each one.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct LeftAs {
    uid: u32,
    gid: u32,
    mode: u32,
}

impl LeftAs {
    fn of(st: &libc::stat) -> Self {
        // Cast needed: `mode_t` is u16 on macOS and u32 on Linux.
        #[allow(clippy::unnecessary_cast)]
        let mode = st.st_mode as u32 & 0o7777;
        LeftAs {
            uid: st.st_uid,
            gid: st.st_gid,
            mode,
        }
    }
}

impl FreshDir {
    /// The directory a successful `mkdirat` in `parent` has just made, opened
    /// on `dir`, if `verify_made_dir` accepts it; `None` when what is there
    /// is not the directory made.
    fn verify(parent: BorrowedFd<'_>, dir: BorrowedFd<'_>) -> PaxResult<Option<Self>> {
        let Some(trust) = verify_made_dir(parent, dir)? else {
            return Ok(None);
        };
        let st = fstat(dir.as_raw_fd())?;
        Ok(Some(FreshDir {
            id: file_id(&st),
            trust,
            left_as: LeftAs::of(&st),
        }))
    }
}

/// Open one directory component below `dirfd` without following a symlink,
/// creating it if asked to, and say where it came from. Callers go through
/// `DirTree::admit` before using it.
fn open_or_create_dir_at(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    create_missing: bool,
) -> PaxResult<(OwnedFd, DirOrigin)> {
    let fd = unsafe { libc::openat(dirfd.as_raw_fd(), name.as_ptr(), WALK_FLAGS) };
    if fd >= 0 {
        return Ok((unsafe { OwnedFd::from_raw_fd(fd) }, DirOrigin::Found));
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
    #[cfg(test)]
    if created {
        reached_made_dir(dirfd, name);
    }

    let fd = unsafe { libc::openat(dirfd.as_raw_fd(), name.as_ptr(), WALK_FLAGS) };
    if fd < 0 {
        return Err(std::io::Error::last_os_error().into());
    }
    let fd = unsafe { OwnedFd::from_raw_fd(fd) };
    // One this run made is checked to be that one, not a directory renamed
    // over it -- which would otherwise count as made here, and be given the
    // attributes of the member naming it later.
    if !created {
        return Ok((fd, DirOrigin::Found));
    }
    let origin = match FreshDir::verify(dirfd, fd.as_fd())? {
        Some(fresh) => DirOrigin::Made(fresh),
        None => DirOrigin::Replaced,
    };
    Ok((fd, origin))
}

/// What `make_dir_at` decided about the attributes of the directory a member
/// names.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum DirAttrs {
    /// Apply the member's attributes, once its contents exist, to the
    /// directory with this `(st_dev, st_ino)`.
    Apply((u64, u64)),
    /// -k leaves what is there alone.
    Keep,
    /// The directory with this `(st_dev, st_ino)` is extracted into, but its
    /// owner could not be verified (`MadeTrust::ParentOwnerOnly`): it gets no
    /// attributes, and the caller says so (`attrs_withheld`).
    Withheld((u64, u64)),
}

/// The diagnostic for `DirAttrs::Withheld`.
pub(crate) fn attrs_withheld() -> PaxError {
    PaxError::Io(std::io::Error::other(
        "not applying owner, mode or times: its owner could not be verified",
    ))
}

/// Create the directory a member names, replacing a non-directory in the
/// way the way a file member replaces a file, and decide what becomes of the
/// member's attributes (`DirAttrs`).
///
/// The identity of a directory made here comes from a descriptor for it,
/// checked to be the one made (`made_dir_id`), never from its name: a
/// directory renamed over it in between would otherwise take the member's
/// attributes. One that was already there (POSIX lets read mode merge into
/// it) is identified by the `lstat` that found it, and a caller that opens it
/// checks the descriptor against that.
///
/// It is created with `mode` (the umask applies) plus owner read, write and
/// search: enough to populate it, and never more than the final mode lets
/// anyone else do. A directory made 0777 first and given its own mode only at
/// the end was open to everyone in between -- for good, if pax never got
/// there.
pub(crate) fn make_dir_at(
    tree: &DirTree,
    dirfd: BorrowedFd<'_>,
    member: &MemberPath,
    mode: u32,
    no_clobber: bool,
) -> PaxResult<DirAttrs> {
    let name = member.leaf.as_c_str();
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
        Ok(()) => return made_dir_id(tree, dirfd, member),
        Err(e) if !exists(&e) => return Err(e.into()),
        Err(_) => {}
    }
    // A directory this run created only to hold earlier members is not a
    // pre-existing file: the member naming it (`find -depth` order) brings
    // its attributes. With -k anything else there is left entirely alone.
    // Otherwise extracting onto an existing directory is not an error
    // (POSIX), and it is kept.
    let existing_dir = lstat_at(dirfd.as_raw_fd(), name)
        .ok()
        .filter(|st| st.st_mode & libc::S_IFMT == libc::S_IFDIR);
    if let Some(st) = existing_dir {
        let id = file_id(&st);
        return match tree.standing(&st, &member.key()) {
            Standing::Replaced => Err(PaxError::Io(made::replaced())),
            Standing::Implicit => {
                tree.claim_implicit(&st, member);
                Ok(DirAttrs::Apply(id))
            }
            // -k leaves it alone, as it does any directory found existing;
            // there is nothing to withhold.
            Standing::Unverified | Standing::Made | Standing::Ordinary if no_clobber => {
                Ok(DirAttrs::Keep)
            }
            Standing::Unverified => Ok(DirAttrs::Withheld(id)),
            // Whether one found existing may take them is decided when they
            // are applied, from its parent (`apply_dir_attrs`).
            Standing::Made | Standing::Ordinary => Ok(DirAttrs::Apply(id)),
        };
    }
    if no_clobber {
        return Ok(DirAttrs::Keep);
    }

    // A non-directory is in the way, and is replaced the way a file member
    // replaces a file. unlinkat with no flags removes the name itself -- never
    // what a symlink points at -- and refuses a directory. A directory that
    // appears in between was put there by someone else, while this run was
    // replacing the name: it is not given the member's attributes.
    let unlinked = unsafe { libc::unlinkat(dirfd.as_raw_fd(), name.as_ptr(), 0) } == 0;
    let unlink_err = (!unlinked).then(std::io::Error::last_os_error);
    match mkdir() {
        Ok(()) => made_dir_id(tree, dirfd, member),
        Err(e) if exists(&e) && is_directory_at(dirfd, name) => Err(PaxError::Io(
            std::io::Error::other("a directory appeared in its place while it was replaced"),
        )),
        Err(e) => Err(unlink_err.unwrap_or(e).into()),
    }
}

/// The attributes decision for the directory `make_dir_at` has just made at
/// `name`, its `(st_dev, st_ino)` read from a descriptor for it once that is
/// checked to be the directory made (`verify_made_dir`); `Withheld` when it
/// can be trusted only so far, and so is not to be given the member's
/// attributes. One
/// found in its place is remembered in `tree`, so that no later member takes
/// it for a pre-existing directory either.
fn made_dir_id(tree: &DirTree, dirfd: BorrowedFd<'_>, member: &MemberPath) -> PaxResult<DirAttrs> {
    let name = member.leaf.as_c_str();
    #[cfg(test)]
    reached_made_dir(dirfd, name);
    let (dir, _) = open_dir_for_attrs(dirfd, name)?;
    let Some(fresh) = FreshDir::verify(dirfd, dir.as_fd())? else {
        let st = fstat(dir.as_raw_fd())?;
        return Err(tree.refuse_replaced(file_id(&st)));
    };
    let id = fresh.id;
    // Recorded either way, as the walk records what it makes: a later member
    // of the same name meets it with this standing.
    tree.record_made(fresh, &member.key(), false);
    match fresh.trust {
        MadeTrust::Full => Ok(DirAttrs::Apply(id)),
        MadeTrust::ParentOwnerOnly => Ok(DirAttrs::Withheld(id)),
    }
}

/// A directory has just been made at `name`: under test, where a writer of
/// the parent gets to replace it.
#[cfg(test)]
fn reached_made_dir(dirfd: BorrowedFd<'_>, name: &CStr) {
    use crate::modes::race_hook::{reached, Point};
    reached(Point::MadeDir, dirfd.as_raw_fd(), name);
}

/// Whether `name` below `dirfd` is a directory, not following a symlink.
fn is_directory_at(dirfd: BorrowedFd<'_>, name: &CStr) -> bool {
    lstat_at(dirfd.as_raw_fd(), name)
        .ok()
        .is_some_and(|st| st.st_mode & libc::S_IFMT == libc::S_IFDIR)
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
        Ok(()) => return Ok(created(dirfd, name)),
        Err(e) if e.raw_os_error() == Some(libc::EEXIST) => {}
        Err(e) => return Err(e.into()),
    }

    if no_clobber {
        return Ok(false);
    }

    unlink_at(dirfd, name)?;
    match create() {
        Ok(()) => Ok(created(dirfd, name)),
        Err(e) => Err(e.into()),
    }
}

/// `create_replacing` has made `name`: always `true`. Under test this is where
/// a writer of the destination directory gets to replace it.
fn created(dirfd: BorrowedFd<'_>, name: &CStr) -> bool {
    #[cfg(test)]
    crate::modes::race_hook::reached(
        crate::modes::race_hook::Point::Made,
        dirfd.as_raw_fd(),
        name,
    );
    let _ = (dirfd, name);
    true
}

/// What a caller knows of the file it means to link (`link_replacing_with`):
/// its `(st_dev, st_ino)`; for a file this run made, the `ctime` it was left
/// with, which an inode number reused for someone else's file does not have;
/// and a descriptor pinning the file itself, where one is held.
#[derive(Clone, Copy, Debug)]
pub(crate) struct Expected<'a> {
    pub(crate) id: (u64, u64),
    pub(crate) ctime: Option<(i64, i64)>,
    pub(crate) pin: Option<BorrowedFd<'a>>,
}

impl Expected<'_> {
    /// Whether `st` shows the file meant: its identity, and its ctime where
    /// that is known.
    fn matches(&self, st: &libc::stat) -> bool {
        file_id(st) == self.id
            && self
                .ctime
                .is_none_or(|ctime| ctime == crate::modes::pins::ctime_of(st))
    }
}

impl Expected<'static> {
    /// Only the identity the caller examined.
    pub(crate) fn id(id: (u64, u64)) -> Self {
        Expected {
            id,
            ctime: None,
            pin: None,
        }
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
/// diagnostic. Identity is (dev, ino): the source's is the pinned inode's, or
/// `expected`; only for a source known by neither is the name resolved again,
/// the way `linkat` resolves it -- the name itself, or with `follow` the file
/// a symbolic link refers to. With `follow` the link itself counts too, being
/// just as much the source (`pax -rwl -H link .`).
///
/// `follow` is the `(st_dev, st_ino)` of the symbolic link `from_name` the
/// caller examined, when the link is to be followed (the walk's own `lstat`,
/// `ftw::Entry::symlink_id`); the file it refers to is then linked --
/// copy mode's `-l` under `-H`/`-L`, where POSIX says "the hard link created
/// ... shall be to the file referenced by the symbolic link". Without it,
/// `from_name` itself is linked, whatever it is.
///
/// `linkat` by name resolves `from_name` again -- and with `follow`, the
/// link's target too -- so it can link a file other than the one the caller
/// examined, whose `(st_dev, st_ino)` is `expected`. Given `expected`, the
/// source is pinned first (`LinkSource`) and checked to be that file, and the
/// link is made to the pinned inode itself, so no other file is ever linked.
/// Where it cannot be pinned, a link made by name to anything else is removed
/// again and the call fails (`linked_expected`), rather than leave the
/// destination a second name for a file nobody asked to copy. Without
/// `expected` -- a source this run did not make, such as a tar link member's
/// target already there before it -- the link is made by name.
pub(crate) fn link_replacing_with(
    from_dir: libc::c_int,
    from_name: &CStr,
    follow: Option<(u64, u64)>,
    expected: Option<Expected<'_>>,
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    no_clobber: bool,
) -> PaxResult<bool> {
    #[cfg(test)]
    crate::modes::race_hook::reached(crate::modes::race_hook::Point::Linking, from_dir, from_name);
    let source = LinkSource::new(from_dir, from_name, follow.is_some(), expected)?;
    let link = || source.link_to(dirfd, name);

    match link() {
        Ok(()) => return source.check_linked(dirfd, name).map(|()| false),
        Err(e) if e.raw_os_error() == Some(libc::EEXIST) => {}
        Err(e) => return Err(e.into()),
    }

    if no_clobber {
        return Ok(false);
    }
    #[cfg(test)]
    crate::modes::race_hook::reached(
        crate::modes::race_hook::Point::LinkExists,
        dirfd.as_raw_fd(),
        name,
    );
    if let Ok(dst) = lstat_at(dirfd.as_raw_fd(), name) {
        // The source is the file pinned, or the one the caller examined: its
        // name, resolved again, can be pointed at the destination's file by
        // now, which would skip the member as linked to itself. Only a source
        // known by neither is resolved again.
        let resolved = source.identity().or_else(|| {
            let src_flags = if follow.is_some() {
                0
            } else {
                libc::AT_SYMLINK_NOFOLLOW
            };
            fstatat(from_dir, from_name, src_flags)
                .ok()
                .map(|st| file_id(&st))
        });
        // Followed, the link itself is the source just as much
        // (`pax -rwl -H link .`): the link the caller examined, by the
        // identity it saw, not by its name resolved again.
        if [resolved, follow]
            .into_iter()
            .flatten()
            .any(|src| src == file_id(&dst))
        {
            return Ok(true);
        }
    }

    unlink_at(dirfd, name)?;
    link()?;
    source.check_linked(dirfd, name)?;
    Ok(false)
}

/// What `link_replacing_with` links.
enum LinkSource<'a> {
    /// The inode the caller examined, pinned by an `O_PATH` descriptor and
    /// linked through its `self/fd/N` entry in a procfs-verified `/proc`
    /// (Linux).
    #[cfg(target_os = "linux")]
    Pinned {
        proc_dir: BorrowedFd<'static>,
        pin: PinFd<'a>,
    },
    /// A name, resolved again by `linkat`: `flags` is `AT_SYMLINK_FOLLOW` or
    /// 0, and `expected` what the link made is checked against afterwards.
    Name {
        from_dir: libc::c_int,
        from_name: &'a CStr,
        flags: libc::c_int,
        expected: Option<(u64, u64)>,
    },
}

impl<'a> LinkSource<'a> {
    /// `from_name` in `from_dir`, followed if `follow`, as `expected` knows
    /// it: the caller's own pin of it, where it holds one and the platform
    /// can link through it; otherwise pinned here, by name, and checked to be
    /// that file; otherwise the name, checked before and after the link.
    fn new(
        from_dir: libc::c_int,
        from_name: &'a CStr,
        follow: bool,
        expected: Option<Expected<'a>>,
    ) -> PaxResult<Self> {
        #[cfg(target_os = "linux")]
        if let Some(expected) = expected {
            if let Some(pinned) = Self::pin(from_dir, from_name, follow, expected)? {
                return Ok(pinned);
            }
        }
        let src_flags = if follow { 0 } else { libc::AT_SYMLINK_NOFOLLOW };
        // A pin the caller holds, not linked through, must still be the file.
        if let Some(pin) = expected.and_then(|e| e.pin) {
            let st = fstat(pin.as_raw_fd()).map_err(|_| source_changed())?;
            if expected.is_some_and(|e| file_id(&st) != e.id) {
                return Err(source_changed());
            }
        }
        if let Some(expected) = expected.filter(|e| e.ctime.is_some()) {
            // The residual without procfs: a file removed and made again
            // with this number and ctime between this check and the link.
            let st = fstatat(from_dir, from_name, src_flags).map_err(|_| source_changed())?;
            if !expected.matches(&st) {
                return Err(source_changed());
            }
        }
        let flags = if follow { libc::AT_SYMLINK_FOLLOW } else { 0 };
        Ok(LinkSource::Name {
            from_dir,
            from_name,
            flags,
            expected: expected.map(|e| e.id),
        })
    }

    /// The source as a descriptor to link through: the caller's pin, or one
    /// opened here with `O_PATH` -- following a symbolic link exactly when
    /// `linkat` would -- and required to be the file `expected`. `None`
    /// without a verified procfs, where no descriptor can be linked.
    #[cfg(target_os = "linux")]
    fn pin(
        from_dir: libc::c_int,
        from_name: &CStr,
        follow: bool,
        expected: Expected<'a>,
    ) -> PaxResult<Option<Self>> {
        let Ok(proc_dir) = made::procfs_dir() else {
            return Ok(None);
        };
        let pin = match expected.pin {
            Some(pin) => PinFd::Borrowed(pin),
            None => {
                let nofollow = if follow { 0 } else { libc::O_NOFOLLOW };
                let flags = libc::O_PATH | libc::O_CLOEXEC | nofollow;
                let fd = unsafe { libc::openat(from_dir, from_name.as_ptr(), flags) };
                if fd < 0 {
                    return Err(std::io::Error::last_os_error().into());
                }
                PinFd::Owned(unsafe { OwnedFd::from_raw_fd(fd) })
            }
        };
        // A pin the caller holds is the file itself; one opened by name must
        // show the identity, and the ctime where known, of the file meant.
        let st = fstat(pin.as_raw_fd()).map_err(|_| source_changed())?;
        // A symbolic link cannot be linked through its `self/fd/N` entry:
        // `AT_SYMLINK_FOLLOW` goes on through the link to what it names. It
        // is linked by name, checked by identity and ctime before and after;
        // a pin the caller holds keeps its number from being reused
        // meanwhile.
        if st.st_mode & libc::S_IFMT == libc::S_IFLNK && !follow {
            return Ok(None);
        }
        let is_expected = match pin {
            PinFd::Borrowed(_) => file_id(&st) == expected.id,
            PinFd::Owned(_) => expected.matches(&st),
        };
        if !is_expected {
            return Err(source_changed());
        }
        Ok(Some(LinkSource::Pinned { proc_dir, pin }))
    }

    /// The source's `(st_dev, st_ino)`: the pinned inode's, or the one the
    /// caller expects of a name; `None` for a name nothing is known of.
    fn identity(&self) -> Option<(u64, u64)> {
        match self {
            #[cfg(target_os = "linux")]
            LinkSource::Pinned { pin, .. } => fstat(pin.as_raw_fd()).ok().map(|st| file_id(&st)),
            LinkSource::Name { expected, .. } => *expected,
        }
    }

    /// Make `name` in `dirfd` a hard link to the source.
    fn link_to(&self, dirfd: BorrowedFd<'_>, name: &CStr) -> std::io::Result<()> {
        let to = dirfd.as_raw_fd();
        let r = match self {
            #[cfg(target_os = "linux")]
            LinkSource::Pinned { proc_dir, pin } => {
                // The magic link is followed to the pinned inode itself.
                let pinned = made::proc_fd_name(pin.as_raw_fd());
                let (from, follow) = (proc_dir.as_raw_fd(), libc::AT_SYMLINK_FOLLOW);
                unsafe { libc::linkat(from, pinned.as_ptr(), to, name.as_ptr(), follow) }
            }
            LinkSource::Name {
                from_dir,
                from_name,
                flags,
                ..
            } => unsafe { libc::linkat(*from_dir, from_name.as_ptr(), to, name.as_ptr(), *flags) },
        };
        cvt(r)
    }

    /// After `link_to` made `name`: a pinned source can only have linked the
    /// file it pins. A name, resolved again, may have linked another file:
    /// unless the link is the file expected (when given), remove it again
    /// and fail. The residual: a writer of the destination who renames the
    /// link away before this check keeps it.
    fn check_linked(&self, dirfd: BorrowedFd<'_>, name: &CStr) -> PaxResult<()> {
        match self {
            #[cfg(target_os = "linux")]
            LinkSource::Pinned { .. } => Ok(()),
            LinkSource::Name { expected, .. } => linked_expected(dirfd, name, *expected),
        }
    }
}

/// A descriptor `LinkSource` links through: opened by it, or the caller's.
#[cfg(target_os = "linux")]
enum PinFd<'a> {
    Owned(OwnedFd),
    Borrowed(BorrowedFd<'a>),
}

#[cfg(target_os = "linux")]
impl AsRawFd for PinFd<'_> {
    fn as_raw_fd(&self) -> libc::c_int {
        match self {
            PinFd::Owned(fd) => fd.as_raw_fd(),
            PinFd::Borrowed(fd) => fd.as_raw_fd(),
        }
    }
}

/// The failure for a source that is no longer the file the walk examined.
fn source_changed() -> PaxError {
    PaxError::Io(std::io::Error::other(
        "source file changed before it could be linked",
    ))
}

/// After `link_replacing_with` made `name` by name: unless it is the file
/// `expected` (when given), remove it again and fail.
fn linked_expected(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    expected: Option<(u64, u64)>,
) -> PaxResult<()> {
    let Some(expected) = expected else {
        return Ok(());
    };
    #[cfg(test)]
    crate::modes::race_hook::reached(
        crate::modes::race_hook::Point::Linked,
        dirfd.as_raw_fd(),
        name,
    );
    if lstat_at(dirfd.as_raw_fd(), name)
        .ok()
        .is_some_and(|st| file_id(&st) == expected)
    {
        return Ok(());
    }
    unlink_at(dirfd, name)?;
    Err(source_changed())
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
    // O_NOCTTY: a terminal swapped in for the file is refused below, but a
    // pax with no controlling terminal would adopt it in the open itself.
    let mut flags = libc::O_RDONLY | libc::O_NOCTTY | libc::O_CLOEXEC;
    if !follow {
        flags |= libc::O_NOFOLLOW;
    }
    let file = open_regular_guarded(dir_fd, name, flags)?;

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

/// `openat(dir_fd, name, flags)` for a file expected to be regular, with
/// `O_NONBLOCK` added so that a FIFO swapped in for it cannot hold the open.
///
/// A regular file under a lease (a Samba oplock, a knfsd delegation) fails a
/// non-blocking open with EAGAIN/EWOULDBLOCK instead of waiting for the lease
/// to break, so that open is retried blocking (`reopen_regular_blocking`) --
/// without letting the retry reach a FIFO swapped in after the first attempt.
/// The caller's identity and type check follows either open.
fn open_regular_guarded(
    dir_fd: libc::c_int,
    name: &CStr,
    flags: libc::c_int,
) -> std::io::Result<File> {
    let fd = unsafe { libc::openat(dir_fd, name.as_ptr(), flags | libc::O_NONBLOCK) };
    if fd >= 0 {
        return Ok(unsafe { File::from_raw_fd(fd) });
    }
    let err = std::io::Error::last_os_error();
    let leased =
        matches!(err.raw_os_error(), Some(e) if e == libc::EAGAIN || e == libc::EWOULDBLOCK);
    if !leased {
        return Err(err);
    }
    reopen_regular_blocking(dir_fd, name, flags)
}

/// The blocking retry of `open_regular_guarded`, which may wait for a lease to
/// break and so must only ever open a regular file.
///
/// On Linux the name is pinned with `O_PATH` (plus the caller's `O_NOFOLLOW`),
/// the pinned inode must be a regular file, and the blocking open is of that
/// inode itself, through `self/fd/N` in a `/proc` verified to be procfs: no
/// name is resolved again, so nothing swapped in after the pin is reached.
#[cfg(target_os = "linux")]
fn reopen_regular_blocking(
    dir_fd: libc::c_int,
    name: &CStr,
    flags: libc::c_int,
) -> std::io::Result<File> {
    let Ok(proc_dir) = made::procfs_dir() else {
        return reopen_regular_by_name(dir_fd, name, flags);
    };
    let pin_flags = libc::O_PATH | libc::O_CLOEXEC | (flags & libc::O_NOFOLLOW);
    let pin = unsafe { libc::openat(dir_fd, name.as_ptr(), pin_flags) };
    if pin < 0 {
        return Err(std::io::Error::last_os_error());
    }
    let pin = unsafe { OwnedFd::from_raw_fd(pin) };
    if !fstat(pin.as_raw_fd())
        .ok()
        .is_some_and(|st| st.st_mode & libc::S_IFMT == libc::S_IFREG)
    {
        return Err(std::io::Error::from_raw_os_error(libc::ENXIO));
    }
    // The magic link is followed to the pinned inode: `O_NOFOLLOW` would
    // refuse it.
    let pinned = made::proc_fd_name(pin.as_raw_fd());
    let fd = unsafe {
        libc::openat(
            proc_dir.as_raw_fd(),
            pinned.as_ptr(),
            flags & !libc::O_NOFOLLOW,
        )
    };
    if fd < 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(unsafe { File::from_raw_fd(fd) })
}

/// Elsewhere, and without procfs, there is no pinning an inode without
/// opening it, so the name is `fstatat`'ed -- following a symbolic link
/// exactly when the caller's flags do -- and must be a regular file just
/// before the blocking open. The residual is a FIFO swapped in between those
/// two calls, which can hold the open until a writer comes; it cannot make
/// pax read anything else, since the caller's identity check still refuses
/// the descriptor.
#[cfg(not(target_os = "linux"))]
fn reopen_regular_blocking(
    dir_fd: libc::c_int,
    name: &CStr,
    flags: libc::c_int,
) -> std::io::Result<File> {
    reopen_regular_by_name(dir_fd, name, flags)
}

/// The by-name half of `reopen_regular_blocking`; see the residual there.
fn reopen_regular_by_name(
    dir_fd: libc::c_int,
    name: &CStr,
    flags: libc::c_int,
) -> std::io::Result<File> {
    let stat_flags = if flags & libc::O_NOFOLLOW != 0 {
        libc::AT_SYMLINK_NOFOLLOW
    } else {
        0
    };
    match fstatat(dir_fd, name, stat_flags).ok() {
        Some(st) if st.st_mode & libc::S_IFMT == libc::S_IFREG => {}
        Some(_) => return Err(std::io::Error::from_raw_os_error(libc::ENXIO)),
        None => return Err(std::io::Error::last_os_error()),
    }
    let fd = unsafe { libc::openat(dir_fd, name.as_ptr(), flags) };
    if fd < 0 {
        return Err(std::io::Error::last_os_error());
    }
    Ok(unsafe { File::from_raw_fd(fd) })
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
    if fstat(dir.as_raw_fd())
        .ok()
        .is_some_and(|st| file_id(&st) == (metadata.dev(), metadata.ino()))
    {
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
    /// A `self/fd/N` entry under a verified procfs `dir`, which cannot be
    /// redirected: see `set_attrs_search_only`.
    #[cfg(target_os = "linux")]
    Proc {
        dir: BorrowedFd<'a>,
        name: &'a CStr,
    },
}

impl AttrTarget<'_> {
    fn chown(&self, uid: libc::uid_t, gid: libc::gid_t) -> libc::c_int {
        match self {
            AttrTarget::Fd(fd) => unsafe { libc::fchown(fd.as_raw_fd(), uid, gid) },
            #[cfg(target_os = "linux")]
            AttrTarget::Proc { dir, name } => unsafe {
                libc::fchownat(dir.as_raw_fd(), name.as_ptr(), uid, gid, 0)
            },
        }
    }

    fn chmod(&self, mode: libc::mode_t) -> libc::c_int {
        match self {
            AttrTarget::Fd(fd) => unsafe { libc::fchmod(fd.as_raw_fd(), mode) },
            #[cfg(target_os = "linux")]
            AttrTarget::Proc { dir, name } => unsafe {
                libc::fchmodat(dir.as_raw_fd(), name.as_ptr(), mode, 0)
            },
        }
    }

    fn utimens(&self, times: &[libc::timespec; 2]) -> libc::c_int {
        match self {
            AttrTarget::Fd(fd) => unsafe { libc::futimens(fd.as_raw_fd(), times.as_ptr()) },
            #[cfg(target_os = "linux")]
            AttrTarget::Proc { dir, name } => unsafe {
                libc::utimensat(dir.as_raw_fd(), name.as_ptr(), times.as_ptr(), 0)
            },
        }
    }
}

/// `set_attrs_fd`, for any `AttrTarget`.
fn set_attrs(target: &AttrTarget<'_>, attrs: &Attrs, policy: &AttrPolicy) -> PaxResult<()> {
    set_attrs_with(target, attrs, policy, true)
}

/// `set_attrs`, setting the mode only `with_mode`: a directory found
/// existing takes a member's mode only under `-p p` (`DirStamp::Found`).
fn set_attrs_with(
    target: &AttrTarget<'_>,
    attrs: &Attrs,
    policy: &AttrPolicy,
    with_mode: bool,
) -> PaxResult<()> {
    // Owner first: a successful chown may clear the set-id bits, and whether it
    // succeeded decides whether they may be set at all.
    let owner_set = policy.preserve_owner
        && set_owner(attrs.uid, attrs.gid, |uid, gid| cvt(target.chown(uid, gid)))?;

    if with_mode && target.chmod(policy.mode(attrs, owner_set) as libc::mode_t) != 0 {
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
    F: FnOnce(libc::uid_t, libc::gid_t) -> std::io::Result<()>,
{
    if uid == u32::MAX || gid == u32::MAX {
        eprintln!("pax: cannot change owner: invalid user or group ID");
        crate::error::note_error();
        return Ok(false);
    }
    chown_result(chown(uid, gid))
}

/// Whether a chown call, given its result, set the owner.
///
/// EPERM -- usually not being root -- is diagnosed and otherwise tolerated: the
/// file keeps whoever ran pax as its owner, and the caller must then withhold
/// the set-id bits (see [`AttrPolicy::mode`]). Any other failure is an error.
fn chown_result(r: std::io::Result<()>) -> PaxResult<bool> {
    let err = match r {
        Ok(()) => return Ok(true),
        Err(e) => e,
    };
    if err.raw_os_error() == Some(libc::EPERM) {
        eprintln!("pax: cannot change owner: Operation not permitted");
        crate::error::note_error();
        return Ok(false);
    }
    Err(err.into())
}

/// Owner, mode and times for a FIFO, device or symbolic link this run has just
/// made at `name` below `dirfd`, as a node of type `made_type` (`S_IFIFO`,
/// `S_IFCHR`, `S_IFBLK` or `S_IFLNK`).
///
/// None of them can be opened for the purpose -- a FIFO's open blocks, a
/// device's acts on the device -- and applying the attributes by name reaches
/// whatever the name holds by then: a writer of the directory who swaps in a
/// hard link to another file hands that file the member's owner, mode and
/// times. So the node is pinned and checked to be the one just made
/// (`MadeNode`), and everything goes through the pin, in this order: owner,
/// then mode, with the set-id bits only if the owner took, then times. A
/// symbolic link's own mode means nothing and is left alone.
pub(crate) fn set_made_node_attrs(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    made_type: libc::mode_t,
    attrs: &Attrs,
    policy: &AttrPolicy,
) -> PaxResult<()> {
    set_made_node_attrs_recording(dirfd, name, made_type, attrs, policy, &mut None)
}

/// `set_made_node_attrs`, leaving in `made` the node as a later name of it
/// is to be linked to it (`MadeFile`) once it is pinned and checked to be the
/// one made -- as it is left, whether or not its attributes all took.
pub(crate) fn set_made_node_attrs_recording(
    dirfd: BorrowedFd<'_>,
    name: &CStr,
    made_type: libc::mode_t,
    attrs: &Attrs,
    policy: &AttrPolicy,
    made: &mut Option<MadeFile>,
) -> PaxResult<()> {
    let node = MadeNode::pin(dirfd, name, made_type)?;
    let applied = apply_node_attrs(&node, made_type, attrs, policy);
    *made = node.made_file();
    // A node made is always known; one that cannot be is a failure, never a
    // name left as it was.
    if made.is_none() {
        applied?;
        return Err(PaxError::Io(made::replaced()));
    }
    applied
}

/// The attributes `set_made_node_attrs` gives a node, through its pin.
fn apply_node_attrs(
    node: &MadeNode<'_>,
    made_type: libc::mode_t,
    attrs: &Attrs,
    policy: &AttrPolicy,
) -> PaxResult<()> {
    if node.trust() == MadeTrust::ParentOwnerOnly {
        set_node_times(node, attrs, policy);
        return owner_unverified(policy);
    }

    let owner_set =
        policy.preserve_owner && set_owner(attrs.uid, attrs.gid, |uid, gid| node.chown(uid, gid))?;
    if made_type != libc::S_IFLNK {
        node.chmod(policy.mode(attrs, owner_set) as libc::mode_t)?;
    }
    set_node_times(node, attrs, policy);
    Ok(())
}

/// The times `policy` asks for, through `node`; a failure is a warning.
fn set_node_times(node: &MadeNode<'_>, attrs: &Attrs, policy: &AttrPolicy) {
    let Some(times) = policy.times(attrs) else {
        return;
    };
    if let Err(e) = node.utimens(&times) {
        eprintln!("pax: warning: cannot set times: {}", e);
        crate::error::note_error();
    }
}

/// A node trusted only as `ParentOwnerOnly` gets no owner and no mode; that is
/// worth saying when either was asked for.
fn owner_unverified(policy: &AttrPolicy) -> PaxResult<()> {
    if !policy.preserve_owner && !policy.preserve_perms {
        return Ok(());
    }
    Err(PaxError::Io(std::io::Error::other(
        "not preserving owner and permissions: its owner could not be verified",
    )))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn member(path: &str) -> MemberPath {
        MemberPath::parse(Path::new(path)).unwrap().unwrap()
    }

    /// The inode a descriptor refers to.
    fn ino_of(fd: &OwnedFd) -> u64 {
        lstat_at(fd.as_raw_fd(), c".").unwrap().st_ino
    }

    fn ino_at(path: &Path) -> u64 {
        use std::os::unix::fs::MetadataExt;
        std::fs::symlink_metadata(path).unwrap().ino()
    }

    /// A terminal swapped in for a source file is refused -- and must not
    /// become the controlling terminal of a pax that has none (cron, CI, a
    /// daemon) in the open before the refusal.
    #[cfg(target_os = "linux")]
    #[test]
    fn test_open_source_file_never_adopts_a_terminal() {
        use crate::modes::race_hook;
        if !race_hook::in_new_session() {
            race_hook::rerun_in_new_session(
                "modes::anchored::tests::test_open_source_file_never_adopts_a_terminal",
            );
            return;
        }
        let (_master, pts, slave) = race_hook::open_pty();
        assert!(!race_hook::has_controlling_tty());
        assert!(open_source_file(pts.as_raw_fd(), &slave, false, (0, 0)).is_err());
        assert!(
            !race_hook::has_controlling_tty(),
            "opening the source made it the controlling terminal"
        );
    }

    /// A symbolic link, or a FIFO made by someone else, found where a FIFO was
    /// just made is not taken for it: nothing is applied through it.
    #[test]
    fn test_made_node_attrs_refuse_what_was_not_made() {
        use std::os::unix::fs::PermissionsExt;
        let tmp = plib::tmp::TempDir::new().unwrap();
        let target = tmp.path().join("target");
        std::fs::write(&target, "").unwrap();
        std::fs::set_permissions(&target, std::fs::Permissions::from_mode(0o644)).unwrap();
        std::os::unix::fs::symlink(&target, tmp.path().join("link")).unwrap();
        let dir = File::open(tmp.path()).unwrap();
        let p = policy(false, true);

        let r = set_made_node_attrs(dir.as_fd(), c"link", libc::S_IFIFO, &attrs(0o4777), &p);
        assert!(r.is_err());
        let mode = std::fs::metadata(&target).unwrap().permissions().mode();
        assert_eq!(mode & 0o7777, 0o644);

        // What it is for: a FIFO, which cannot be opened without blocking.
        let fifo = tmp.path().join("fifo");
        let fifo_c = CString::new(fifo.as_os_str().as_bytes()).unwrap();
        assert_eq!(unsafe { libc::mkfifo(fifo_c.as_ptr(), 0o644) }, 0);
        set_made_node_attrs(dir.as_fd(), c"fifo", libc::S_IFIFO, &attrs(0o604), &p).unwrap();
        let mode = std::fs::symlink_metadata(&fifo)
            .unwrap()
            .permissions()
            .mode();
        assert_eq!(mode & 0o7777, 0o604);

        // Asked for as a device, the FIFO is not one.
        let r = set_made_node_attrs(dir.as_fd(), c"fifo", libc::S_IFCHR, &attrs(0o600), &p);
        assert!(r.is_err());
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

    /// An inode number the registry holds can come back: the directory made
    /// for `p/` removed while still empty, and the next mkdir -- at `q` --
    /// given its number. Recording the new directory forgets everything the
    /// number stood for, so `q` renamed to `p` is not taken for the directory
    /// made for `p/`.
    #[test]
    fn test_a_reused_inode_number_forgets_what_it_stood_for() {
        let dir = plib::tmp::TempDir::new().unwrap();
        let tree = DirTree::open_path(dir.path()).unwrap();
        let id = (1, 4242);
        let (p, q) = (member("p").key(), member("q").key());
        let fresh = |trust| FreshDir {
            id,
            trust,
            left_as: LEFT,
        };
        let st = status(id, LEFT);

        tree.record_made(fresh(MadeTrust::Full), &p, false);
        assert!(tree.standing(&st, &p) == Standing::Made);
        tree.record_made(fresh(MadeTrust::Full), &q, true);
        assert!(tree.standing(&st, &q) == Standing::Implicit);
        assert!(
            tree.standing(&st, &p) == Standing::Ordinary,
            "the number still stood for the directory made for p/"
        );

        // Nor does an earlier refusal or doubt about the number survive a
        // directory proven new.
        tree.replaced.borrow_mut().insert(id);
        tree.record_made(fresh(MadeTrust::Full), &p, false);
        assert!(tree.standing(&st, &p) == Standing::Made);
        tree.record_made(fresh(MadeTrust::ParentOwnerOnly), &q, true);
        assert!(tree.standing(&st, &q) == Standing::Unverified);
        tree.record_made(fresh(MadeTrust::Full), &p, false);
        assert!(tree.standing(&st, &p) == Standing::Made);
    }

    /// What `record_made` is told a directory was left as, in the tests.
    const LEFT: LeftAs = LeftAs {
        uid: 0,
        gid: 0,
        mode: 0o700,
    };

    /// An `fstat` answer for the directory `id`, as `left` says.
    fn status(id: (u64, u64), left: LeftAs) -> libc::stat {
        let mut st: libc::stat = unsafe { std::mem::zeroed() };
        // Casts needed: the field types differ between platforms.
        #[allow(clippy::unnecessary_cast)]
        {
            st.st_dev = id.0 as _;
            st.st_ino = id.1 as _;
            st.st_mode = (libc::S_IFDIR as u32 | left.mode) as _;
        }
        st.st_uid = left.uid;
        st.st_gid = left.gid;
        st
    }

    /// A directory this run made, removed by someone who can write its parent
    /// and made again there by them, can be given the same inode number --
    /// ext4 hands a freed one straight back. Met at its member's name before
    /// its attributes are applied, it is not the directory made: it is found
    /// existing there (`Standing::Ordinary`), and judged as one, unless it is
    /// still owned, grouped and moded as this run left it.
    #[test]
    fn test_a_directory_made_again_at_a_reused_number_is_not_the_one_made() {
        let dir = plib::tmp::TempDir::new().unwrap();
        let tree = DirTree::open_path(dir.path()).unwrap();
        let id = (1, 4242);
        let (p, q) = (member("p").key(), member("q").key());
        let fresh = FreshDir {
            id,
            trust: MadeTrust::Full,
            left_as: LEFT,
        };
        tree.record_made(fresh, &p, false);
        tree.record_made(
            FreshDir {
                id: (1, 4243),
                ..fresh
            },
            &q,
            true,
        );
        let bobs = LeftAs { uid: 1000, ..LEFT };
        let regrouped = LeftAs { gid: 1000, ..LEFT };
        let opened = LeftAs {
            mode: 0o755,
            ..LEFT
        };
        for other in [bobs, regrouped, opened] {
            assert!(tree.standing(&status(id, other), &p) == Standing::Ordinary);
            assert!(tree.standing(&status((1, 4243), other), &q) == Standing::Ordinary);
        }
        assert!(tree.standing(&status(id, LEFT), &p) == Standing::Made);
        assert!(tree.standing(&status((1, 4243), LEFT), &q) == Standing::Implicit);

        // What this run gives it itself is noted, and it stays the one made.
        tree.note_left_as(&status(id, opened));
        assert!(tree.standing(&status(id, opened), &p) == Standing::Made);
    }

    /// A directory this run made and then, under -p o as root, gave to
    /// someone else is theirs from then on: they can remove it and make
    /// another at its name, which ext4 can give the same number, and set its
    /// mode to what this run left. A second pending apply for that name -- a
    /// member named twice, or two sources -s maps onto one name -- must then
    /// judge it as found existing (`Standing::Ordinary`), never as the one
    /// made, or root gives the planted directory the second member's owner
    /// and mode without the existing-directory rule.
    #[test]
    fn test_a_made_directory_given_to_someone_else_is_no_longer_the_one_made() {
        let dir = plib::tmp::TempDir::new().unwrap();
        let tree = DirTree::open_path(dir.path()).unwrap();
        let euid = unsafe { libc::geteuid() };
        let ours = LeftAs { uid: euid, ..LEFT };
        let given = LeftAs {
            uid: euid.wrapping_add(1),
            mode: 0o755,
            ..LEFT
        };
        let (p, q) = (member("p").key(), member("q").key());
        for (id, key, implicit) in [((1, 4242), &p, false), ((1, 4243), &q, true)] {
            let fresh = FreshDir {
                id,
                trust: MadeTrust::Full,
                left_as: ours,
            };
            tree.record_made(fresh, key, implicit);
            // pax gave it the member's owner and mode, and noted that.
            tree.note_left_as(&status(id, given));
            assert!(
                tree.standing(&status(id, given), key) == Standing::Ordinary,
                "implicit={implicit}: a directory given away still counted as made"
            );
        }

        // Left with its own owner -- the user's, or the one a filesystem that
        // stores no owners reports for everything -- it stays the one made.
        let mount_owner = LeftAs {
            uid: euid.wrapping_add(2),
            ..LEFT
        };
        for left in [ours, mount_owner] {
            let id = (1, 4244);
            let fresh = FreshDir {
                id,
                trust: MadeTrust::Full,
                left_as: left,
            };
            tree.record_made(fresh, &p, false);
            let stamped = LeftAs {
                mode: 0o750,
                ..left
            };
            tree.note_left_as(&status(id, stamped));
            assert!(tree.standing(&status(id, stamped), &p) == Standing::Made);
        }
    }

    /// Whether the destination already is the source decides whether a link
    /// is made at all: that file is left in place, and the member counted as
    /// linked to itself. It was decided by resolving the source's name again,
    /// which someone who can rename in the source directory can point at the
    /// destination's file once the source is pinned: the member was then
    /// skipped, its name left holding another file. The pinned inode decides.
    #[cfg(target_os = "linux")]
    #[test]
    fn test_link_replacing_judges_itself_by_the_pinned_source() {
        use crate::modes::race_hook::{with_hook, Point};
        let dir = plib::tmp::TempDir::new().unwrap();
        std::fs::write(dir.path().join("a"), "source\n").unwrap();
        // Another name keeps the source linkable once `a` is taken from it.
        std::fs::hard_link(dir.path().join("a"), dir.path().join("a2")).unwrap();
        std::fs::write(dir.path().join("d"), "other\n").unwrap();
        let tree = DirTree::open_path(dir.path()).unwrap();
        let root = tree.root().as_raw_fd();
        let source = file_id(&lstat_at(tree.root().as_raw_fd(), c"a").unwrap());

        let path = dir.path().to_path_buf();
        let swap = move |point, _: libc::c_int, _: &CStr| {
            if point == Point::LinkExists {
                // `a` now names the destination's file.
                std::fs::remove_file(path.join("a")).unwrap();
                std::fs::hard_link(path.join("d"), path.join("a")).unwrap();
            }
        };
        let linked = with_hook(swap, || {
            link_replacing_with(
                root,
                c"a",
                None,
                Some(Expected::id(source)),
                tree.root(),
                c"d",
                false,
            )
        });
        assert!(!linked.unwrap(), "skipped as already linked to itself");
        let d = lstat_at(tree.root().as_raw_fd(), c"d").unwrap();
        assert_eq!(file_id(&d), source, "d does not hold the source");
    }

    /// Under -H/-L the symbolic link itself counts as the source too, so a
    /// destination that already is that link is left alone. It was recognised
    /// by resolving the link's name again, which someone who can rename in the
    /// source directory can point at the destination's file once the link's
    /// target is pinned: the member was then skipped. The link is the one the
    /// walk examined, by its identity.
    #[cfg(target_os = "linux")]
    #[test]
    fn test_link_replacing_judges_a_followed_link_by_the_walks_identity() {
        use crate::modes::race_hook::{with_hook, Point};
        let dir = plib::tmp::TempDir::new().unwrap();
        std::fs::write(dir.path().join("t"), "target\n").unwrap();
        std::os::unix::fs::symlink("t", dir.path().join("l")).unwrap();
        std::fs::write(dir.path().join("d"), "other\n").unwrap();
        let tree = DirTree::open_path(dir.path()).unwrap();
        let root = tree.root().as_raw_fd();
        let id = |name: &CStr| file_id(&lstat_at(root, name).unwrap());
        let (link, target) = (id(c"l"), id(c"t"));

        let path = dir.path().to_path_buf();
        let swap = move |point, _: libc::c_int, _: &CStr| {
            if point == Point::LinkExists {
                // `l` now names the destination's file.
                std::fs::remove_file(path.join("l")).unwrap();
                std::fs::hard_link(path.join("d"), path.join("l")).unwrap();
            }
        };
        let linked = with_hook(swap, || {
            link_replacing_with(
                root,
                c"l",
                Some(link),
                Some(Expected::id(target)),
                tree.root(),
                c"d",
                false,
            )
        });
        assert!(!linked.unwrap(), "skipped as already the link itself");
        assert_eq!(id(c"d"), target, "d does not hold the link's target");
    }

    /// -k leaves an existing directory entirely alone, and says nothing: one
    /// this run made with an owner it could not verify (`Standing::Unverified`,
    /// NFS or FUSE) too. Its attributes were withheld and diagnosed even under
    /// -k; without -k they still are.
    #[test]
    fn test_no_clobber_keeps_an_unverified_directory_silently() {
        let dir = plib::tmp::TempDir::new().unwrap();
        let tree = DirTree::open_path(dir.path()).unwrap();
        std::fs::create_dir(dir.path().join("u")).unwrap();
        let u = file_id(&lstat_at(tree.root().as_raw_fd(), c"u").unwrap());
        tree.unverified.borrow_mut().insert(u);

        let kept = make_dir_at(&tree, tree.root(), &member("u"), 0o755, true).unwrap();
        assert_eq!(kept, DirAttrs::Keep);
        let decided = make_dir_at(&tree, tree.root(), &member("u"), 0o755, false).unwrap();
        assert_eq!(decided, DirAttrs::Withheld(u));
        let st = lstat_at(tree.root().as_raw_fd(), c"u").unwrap();
        assert!(tree.standing(&st, &member("u").key()) == Standing::Unverified);
    }

    /// A directory recorded as found in place of one made, or as made with
    /// an owner that could not be verified, keeps that standing whatever
    /// reaches it -- the walk, copy mode's entry, a member naming it -- and
    /// only a verified fresh mkdir (`record_made`) can change it.
    #[test]
    fn test_a_refused_or_unverified_directory_is_never_downgraded() {
        let dir = plib::tmp::TempDir::new().unwrap();
        let tree = DirTree::open_path(dir.path()).unwrap();
        for name in ["r", "u"] {
            std::fs::create_dir(dir.path().join(name)).unwrap();
        }
        let id = |name: &CStr| file_id(&lstat_at(tree.root().as_raw_fd(), name).unwrap());
        let (r, u) = (id(c"r"), id(c"u"));
        let (r_key, u_key) = (member("r").key(), member("u").key());
        assert!(matches!(tree.refuse_replaced(r), PaxError::Io(_)));
        tree.unverified.borrow_mut().insert(u);

        // Everything that can reach them, in turn.
        assert!(tree.open_dir(tree.root(), c"r", false).is_err());
        assert!(tree.parent_of(&member("r/x"), true).is_err());
        assert!(make_dir_at(&tree, tree.root(), &member("r"), 0o755, false).is_err());
        assert!(tree.open_dir(tree.root(), c"u", false).is_ok());
        assert!(tree.parent_of(&member("u/x"), true).is_ok());
        let decided = make_dir_at(&tree, tree.root(), &member("u"), 0o755, false).unwrap();
        assert_eq!(decided, DirAttrs::Withheld(u));
        assert!(!tree.claim_implicit(
            &lstat_at(tree.root().as_raw_fd(), c"u").unwrap(),
            &member("u")
        ));

        let st = |name: &CStr| lstat_at(tree.root().as_raw_fd(), name).unwrap();
        assert!(tree.standing(&st(c"r"), &r_key) == Standing::Replaced);
        assert!(tree.standing(&st(c"u"), &u_key) == Standing::Unverified);
    }

    /// Every site that enters, merges into or stamps a directory consults the
    /// same registry: one recorded as found in place of a directory this run
    /// made is refused by `open_dir` (copy mode's entry) as by the walk, and
    /// neither it nor one whose owner could not be verified is stamped.
    #[test]
    fn test_every_directory_site_consults_the_standing() {
        let dir = plib::tmp::TempDir::new().unwrap();
        let tree = DirTree::open_path(dir.path()).unwrap();
        for name in ["r", "u"] {
            std::fs::create_dir(dir.path().join(name)).unwrap();
        }
        let id = |name: &CStr| file_id(&lstat_at(tree.root().as_raw_fd(), name).unwrap());
        tree.replaced.borrow_mut().insert(id(c"r"));
        tree.unverified.borrow_mut().insert(id(c"u"));

        assert!(tree.open_dir(tree.root(), c"r", false).is_err());
        assert!(tree.parent_of(&member("r/x"), false).is_err());
        assert!(tree.open_dir(tree.root(), c"u", false).is_ok());

        let mut pending = PendingDirs::default();
        let mut stamp = |name: &str, id| {
            let mut attrs = attrs(0o751);
            attrs.mtime = 12345;
            pending.push(&member(name), id, attrs);
        };
        stamp("r", id(c"r"));
        stamp("u", id(c"u"));
        let mut p = policy(false, true);
        p.preserve_mtime = true;
        pending.apply(&tree, &p);
        for name in ["r", "u"] {
            let md = std::fs::metadata(dir.path().join(name)).unwrap();
            use std::os::unix::fs::MetadataExt;
            assert_ne!(md.mtime(), 12345, "{name} was stamped");
        }
    }

    /// A directory the walk holds a descriptor for can later be found renamed
    /// over one this run made: moved away, and back over the new directory
    /// of its old name. Once refused, the walk's cached descriptor for it
    /// must not take a member there either.
    #[test]
    fn test_a_cached_directory_found_replaced_is_not_reused() {
        use crate::modes::race_hook::{self, Point};
        use std::os::unix::fs::PermissionsExt;
        let dir = plib::tmp::TempDir::new().unwrap();
        // Others may rename entries in it, so a made directory is verified.
        std::fs::set_permissions(dir.path(), std::fs::Permissions::from_mode(0o777)).unwrap();
        let tree = DirTree::open_path(dir.path()).unwrap();

        // The walk makes `a` and keeps a descriptor for it.
        tree.parent_of(&member("a/x"), true).unwrap();
        std::fs::write(dir.path().join("a/x"), "").unwrap();
        // It is moved away; a member naming `a` makes a new one, and the old
        // one is renamed back over it.
        let away = dir.path().join("away");
        std::fs::rename(dir.path().join("a"), &away).unwrap();
        let away_c = CString::new(away.as_os_str().as_bytes()).unwrap();
        let hook = move |point, dirfd, name: &CStr| {
            if point == Point::MadeDir && name == c"a" {
                race_hook::swap_for_directory(dirfd, name, &away_c);
            }
        };
        let made = race_hook::with_hook(hook, || {
            make_dir_at(&tree, tree.root(), &member("a"), 0o755, false)
        });
        assert!(made.is_err(), "the directory renamed over it was taken");

        assert!(
            tree.parent_of(&member("a/y"), true).is_err(),
            "a cached descriptor reached the refused directory"
        );
        assert!(!dir.path().join("a/y").exists());
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

    /// Whether a group is private is looked up only when the answer is used: when -p asks for
    /// mode or owner of a directory that was already there. Without it, a group-writable
    /// destination with such a directory in it is walked and stamped without a lookup; with
    /// `-p p` one is made.
    #[test]
    fn test_private_groups_are_looked_up_only_under_p() {
        use std::os::unix::fs::{MetadataExt, PermissionsExt};
        let tmp = plib::tmp::TempDir::new().unwrap();
        let dest = tmp.path().join("dest");
        std::fs::create_dir_all(dest.join("d/x")).unwrap();
        std::fs::set_permissions(dest.join("d"), std::fs::Permissions::from_mode(0o775)).unwrap();
        std::fs::set_permissions(&dest, std::fs::Permissions::from_mode(0o775)).unwrap();
        let x = std::fs::metadata(dest.join("d/x")).unwrap();
        let apply = |policy: &AttrPolicy| {
            let tree = DirTree::open_path(&dest).unwrap();
            let mut pending = PendingDirs::default();
            pending.push(&member("d/x"), (x.dev(), x.ino()), attrs(0o755));
            pending.apply(&tree, policy);
        };

        let before = plib::madefs::private_group_queries();
        apply(&policy(false, false));
        assert_eq!(
            plib::madefs::private_group_queries(),
            before,
            "looked up without -p"
        );
        apply(&policy(false, true));
        assert!(
            plib::madefs::private_group_queries() > before,
            "-p p did not"
        );
        std::fs::set_permissions(&dest, std::fs::Permissions::from_mode(0o755)).unwrap();
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

        let same = link_replacing_with(
            dir.root().as_raw_fd(),
            &f,
            None,
            None,
            dir.root(),
            &f,
            false,
        )
        .unwrap();
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

        let same = link_replacing_with(
            dir.root().as_raw_fd(),
            &f,
            None,
            None,
            dir.root(),
            &g,
            false,
        )
        .unwrap();
        assert!(!same);
        assert_eq!(
            std::fs::read_to_string(temp.path().join("g")).unwrap(),
            "DATA\n"
        );
    }
}
