//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::pinned::{CopiedSources, PinnedEntry, SourceState};
use super::{error_string, quote, Verbose};
use ftw::{self, traverse_directory};
use gettextrs::gettext;
pub use plib::madefs::ChainTrust;
pub use plib::madefs::MadeTrust;
#[cfg(target_os = "linux")]
use plib::madefs::{chmod_pinned, proc_fd_name, procfs_dir, utimens_link_if_still};
use plib::madefs::{
    fs_owners, made_by_us, FoundDir, FsOwners, MadeObject, NamedAnchor, Preserve, SEARCH_ONLY,
};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    ffi::{CStr, CString, OsStr},
    fs, io,
    os::{
        fd::{AsRawFd, FromRawFd, OwnedFd},
        unix::{ffi::OsStrExt, fs::MetadataExt},
    },
    path::{Path, PathBuf},
    rc::Rc,
};

/// Where each already-copied inode landed, so a later name for the same file can be hard-linked
/// to it instead of copied again.
///
/// The descriptor is shared rather than duplicated: `dup`ing one per hard-linked inode grew the
/// process's descriptor use without bound over a large move, and made the walk's own descriptor
/// budget meaningless.
pub type InodeMap = HashMap<(u64, u64), FirstCopy>;

/// Where the first copy of a hard-linked source file was made, and what was made.
pub struct FirstCopy {
    /// The directory it was made in, held open since.
    dir: Rc<ftw::FileDescriptor>,
    name: CString,
    /// `(st_dev, st_ino)` of the copy, from the descriptor it was written through (or, for a
    /// symbolic link or special file, from the `lstat` that followed its creation).
    made: (u64, u64),
    /// The source file as the first copy duplicated it.
    source: SourceState,
}

/// Whether `link_to_first_copy` made the new name a link to the first copy.
enum Linked {
    ToFirstCopy,
    /// The source file is no longer what the first copy duplicated (written to since, or its
    /// inode number now another file's): nothing was linked.
    SourceChanged,
    /// The first copy's name now holds another file, and the link to it was undone.
    NotTheFirstCopy,
}

/// Make `name` in `dirfd` a hard link to `first`, for another name of the source file, now in
/// state `source`.
///
/// Only a source file still as the first copy duplicated it is linked to that copy; one written
/// to since (or an inode number freed and given to a new file) is not (`SourceChanged`), and the
/// caller copies it afresh, so the destination gets what the file holds now.
///
/// The link is made by name, and anyone who can write the directory the first copy was made in
/// can have renamed a file of their own over that name since. So the new link must turn out to
/// be the very file the first copy made; a link to anything else is unlinked again
/// (`NotTheFirstCopy`), and the caller copies the file afresh.
fn link_to_first_copy(
    first: &FirstCopy,
    source: &SourceState,
    dirfd: libc::c_int,
    name: &CStr,
) -> io::Result<Linked> {
    if *source != first.source {
        return Ok(Linked::SourceChanged);
    }
    let ret = unsafe {
        libc::linkat(
            first.dir.as_raw_fd(),
            first.name.as_ptr(),
            dirfd,
            name.as_ptr(),
            0, // Don't dereference the first copy if it is a symlink
        )
    };
    if ret != 0 {
        return Err(io::Error::last_os_error());
    }
    let linked = ftw::Metadata::new(dirfd, name, false)?;
    if (linked.dev(), linked.ino()) == first.made {
        return Ok(Linked::ToFirstCopy);
    }
    if unsafe { libc::unlinkat(dirfd, name.as_ptr(), 0) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(Linked::NotTheFirstCopy)
}

/// The identity of a symbolic link or special file this copy just made by name (`symlinkat`,
/// `mknodat`), for hard-linking later names to it: `None` unless the name holds what a fresh one
/// is (`is_fresh_node`). A node that fails that is simply not linked to: each later name of the
/// source file is copied afresh.
fn made_node_identity(
    dirfd: libc::c_int,
    name: *const libc::c_char,
    made_type: ftw::FileType,
) -> Option<(u64, u64)> {
    let md = ftw::Metadata::new(dirfd, unsafe { CStr::from_ptr(name) }, false).ok()?;
    let euid = unsafe { libc::geteuid() };
    is_fresh_node(md.file_type(), md.nlink(), md.uid(), made_type, euid)
        .then(|| (md.dev(), md.ino()))
}

/// Whether a node of type `found` with `nlink` links, owned by `uid`, can be the `made` node cp
/// (running as `euid`) has just made: the same type, a single link, and cp's own. On filesystems
/// that map owners, cp's own nodes may not show as its own; those are then not hard-linked to.
fn is_fresh_node(
    found: ftw::FileType,
    nlink: u64,
    uid: u32,
    made: ftw::FileType,
    euid: u32,
) -> bool {
    found == made && nlink == 1 && uid == euid
}

/// Which symbolic links are acted on by what they refer to, rather than as links
/// (POSIX cp 90609-90623).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DerefMode {
    /// `-P`: act on the link itself, whether it is an operand or was found during the walk.
    Never,
    /// `-H`: act on the referent of a link named as an operand, and on nothing else.
    CommandLineOnly,
    /// `-L`, and the default when `-R` was not given (90610-90612).
    Always,
}

impl DerefMode {
    fn follow_symlinks_on_args(self) -> bool {
        self != DerefMode::Never
    }

    fn follow_symlinks(self) -> bool {
        self == DerefMode::Always
    }

    /// Whether *this* entry's link is to be followed.
    ///
    /// This lines up with what the traversal was told to do, which is what makes the entry's
    /// metadata the right one to act on: under `CommandLineOnly` the walk dereferences exactly
    /// the operand, which is exactly where `at_top_level` is true.
    fn deref_entry(self, at_top_level: bool) -> bool {
        match self {
            DerefMode::Always => true,
            DerefMode::CommandLineOnly => at_top_level,
            DerefMode::Never => false,
        }
    }
}

pub struct CopyConfig {
    pub force: bool,
    pub deref: DerefMode,
    pub interactive: bool,
    pub preserve: bool,
    pub recursive: bool,
    /// GNU `-n`: a non-directory whose destination already exists is skipped silently.
    pub no_clobber: bool,
    /// GNU `-l`: each non-directory is hard-linked to the source instead of copied.
    pub link: bool,
    /// Diagnostic prefix (`"cp"` or `"mv"`) for messages emitted directly by the copy engine.
    pub prog: &'static str,
    /// When `true` (cp), a per-file failure is reported and the walk continues with same-level and
    /// ancestor entries (POSIX cp CONSEQUENCES OF ERRORS, 90829-90832). When `false` (mv), the
    /// first structural error stops the duplication so the source is not removed.
    pub continue_on_error: bool,
    /// What the copy may find at the destination operand itself.
    pub destination: Destination,
    /// GNU `-v`: write each step done to standard output, in this wording.
    pub verbose: Option<Verbose>,
}

/// With `-v`, write to standard output what a copy step just did: `made_dir` when it made the
/// directory `target`, otherwise when it copied or linked the non-directory `source` there.
fn report_copied(cfg: &CopyConfig, source: &Path, target: &Path, made_dir: bool) {
    let line = match (cfg.verbose, made_dir) {
        (None, _) => return,
        (Some(Verbose::Copy), _) => gettext!("{} -> {}", quote(source), quote(target)),
        (Some(Verbose::Move), true) => gettext!("created directory {}", quote(target)),
        (Some(Verbose::Move), false) => {
            gettext!("copied {} -> {}", quote(source), quote(target))
        }
    };
    super::report_verbose(&line);
}

/// What a copy may find at its destination operand (not below it).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Destination {
    /// cp: an existing destination is examined, and then replaced, written into or (for a
    /// directory) copied into, as POSIX cp prescribes.
    MayExist,
    /// mv across filesystems, after its step 5 removed the destination or found none: the copy
    /// must create the destination itself (`O_CREAT | O_EXCL`, `mkdirat`, `symlinkat`,
    /// `mknodat`). Whatever is at its name by then appeared during the move and is never
    /// written into, filled or removed: the create fails with EEXIST, and that is an error.
    MustCreate,
}

/// Where the destination directory of a `CopyingDirectory` came from, so that the descriptor
/// later opened for it can be checked to be that directory.
enum DirOrigin {
    /// This copy made it with `mkdirat`.
    Made,
    /// It already existed, with the identity the decision's `lstat` saw.
    Found { dev: u64, ino: u64 },
}

/// What a copy's destination operand is handed by the directory it is made in, should it be
/// found existing there: whether, as a directory, it may take the source's mode and owner under
/// -p (`ChainTrust`). The anchor the trust is carried down from is the destination directory
/// the user named.
#[derive(Clone, Debug)]
pub enum OperandTrust {
    /// The operand is that directory itself, copied into (`cp -R src/. dest`): the anchor,
    /// which takes what -p asks wherever it is, as GNU cp gives it.
    Named,
    /// The directory the operand is made in is the anchor: `dir` for `cp -R src dir` finding
    /// `dir/src`, and the directory `new` was to be made in for `cp -R src new` finding `new`
    /// there after all. It is located only once the operand is found (`parent_anchor`).
    Parent,
    /// Handed down the directories walked from the anchor to the one the operand is made in
    /// (`cp --parents`).
    Chain(ChainTrust),
}

/// Every destination directory a cp run has made and verified in full (`MadeTrust::Full`), by
/// `(st_dev, st_ino)`, with the destination path it was made at.
///
/// Found again at that path -- by a later operand, or by `--parents` on the way to one -- it is
/// still the run's own: it takes what -p asks, and hands the directories found in it the trust
/// of a directory made (`ChainTrust::made`), wherever it is. Found at any other path it is one
/// found existing there, as pax counts it: whoever renamed it there could have renamed it to
/// the name of a source directory it does not stand for, and it would take that one's mode.
///
/// Nor is it the run's own once anything but cp has changed it since cp last did: its
/// status-change time, owner, group and mode must be the ones cp left it with. An inode number
/// is reused -- ext4 hands a freed one straight back -- so whoever could remove the directory
/// (an empty one, from a parent they can write that is not sticky) could make one of their own
/// at its path under its number. That one's status-change time is its making, after cp's last
/// change; but the time has the grain of the clock tick the kernel stamps it with (a whole
/// second on ext4 with 128-byte inodes, HFS+ and NFSv3), so within that tick it is the owner
/// that tells them apart: someone else's `mkdir` gives the directory their own uid, which cp
/// did not leave there -- unless cp gave the one it made that very owner (-p, as root, from a
/// source of theirs), and then the directory was theirs already, and passing one of theirs for
/// it hands them nothing they did not have. That is the residual, with root, who can make a
/// directory with any owner. cp notes its own changes from the descriptor it has held
/// throughout -- a directory held open keeps its number from being reused -- once it has filled
/// and finished one (`refresh`), so an honest later operand still finds it unchanged. Residual
/// of that: once -p as root has given a made directory away, its new owner can change it while
/// cp still fills it, and the refresh records their change as cp's -- passing for the run's own
/// a directory that is theirs anyway. Recording a directory replaces whatever its number stood
/// for.
#[derive(Default)]
pub struct MadeDirs(HashMap<(u64, u64), MadeAt>);

/// Where a directory this run made is, and its status as cp last left it.
struct MadeAt {
    path: PathBuf,
    left: LeftAs,
}

/// What of a directory's status cp checks it left unchanged: its status-change time, owner,
/// group and mode.
#[derive(Clone, Copy, PartialEq, Eq)]
struct LeftAs {
    ctime: (i64, i64),
    uid: u32,
    gid: u32,
    mode: u32,
}

/// `(st_dev, st_ino)` of `md`, and what `LeftAs` keeps of it.
fn id_and_status(md: &impl MetadataExt) -> ((u64, u64), LeftAs) {
    let left = LeftAs {
        ctime: (md.ctime(), md.ctime_nsec()),
        uid: md.uid(),
        gid: md.gid(),
        mode: md.mode(),
    };
    ((md.dev(), md.ino()), left)
}

impl MadeDirs {
    /// Record the directory `md` describes, just made at `path` and verified in full.
    pub fn record(&mut self, md: &impl MetadataExt, path: &Path) {
        let (id, left) = id_and_status(md);
        let path = path.to_path_buf();
        self.0.insert(id, MadeAt { path, left });
    }

    /// Whether the directory `md` describes, found at `path`, is one this run made there.
    pub fn made_at(&self, md: &impl MetadataExt, path: &Path) -> bool {
        let (id, left) = id_and_status(md);
        self.0
            .get(&id)
            .is_some_and(|at| at.path == path && at.left == left)
    }

    /// Note what cp has just changed on the directory `md` describes, which this run made at
    /// `path` and has held since it found it to be so (`made_at`).
    pub fn refresh(&mut self, md: &impl MetadataExt, path: &Path) {
        let (id, left) = id_and_status(md);
        if let Some(at) = self.0.get_mut(&id).filter(|at| at.path == path) {
            at.left = left;
        }
    }
}

#[cfg(test)]
thread_local! {
    /// Called with the descriptor of each directory of the run's own (`MadeDirs`) just before
    /// cp finishes it: lets a test change it the way a slow copy would.
    static BEFORE_FINISHING_OWN: std::cell::Cell<Option<fn(libc::c_int)>> =
        const { std::cell::Cell::new(None) };
}

/// What a cp run carries from one operand to the next.
#[derive(Default)]
pub struct CopyRun {
    /// Destinations already written, so a later source cannot clobber one.
    pub created_files: HashSet<PathBuf>,
    /// Destination directories made (`MadeDirs`).
    pub made_dirs: MadeDirs,
}

/// What a destination directory the copy entered is given once its contents are copied.
#[derive(Clone, Copy, Debug)]
enum DirFinish {
    /// This copy made it, and `verify_made_dir` trusts it this far.
    Made(MadeTrust),
    /// It was there already, or was made earlier in the run (`MadeDirs`).
    Found(FoundDir),
}

/// A destination directory the walk is in: the one the operand is made in at the bottom of the
/// stack, then one per source directory entered.
struct DestDir {
    fd: Rc<ftw::FileDescriptor>,
    /// What it hands the directories found in it.
    hands: OperandTrust,
    /// What it is given once its contents are copied; `None` for the operand's own directory,
    /// which the copy only makes the operand in.
    finish: Option<DirFinish>,
    /// Whether this run made it, here (`MadeDirs`): what cp changes on it is then noted.
    own: bool,
}

/// The trust the anchor of `OperandTrust::Parent` hands the operand `target`, found existing as
/// the directory with identity `id`: the directory `target` names it in, when it is there under
/// its own name.
///
/// That directory is `parent`, held, or, when `parent` is the working directory the operand is
/// resolved from, `target` less its last component, opened for search only. The operand's own
/// name must then be the very directory found -- read with `AT_SYMLINK_NOFOLLOW`, and a
/// directory has no other name. It is not when the operand ends in a slash and its name is a
/// symbolic link: the copy follows it, as GNU cp's does, but the directory reached is anywhere
/// the link's owner chose, and judging it by the directory the link is in, or by its own `..`,
/// would hand it trust that directory never gave. Anything not located hands none
/// (`ChainTrust::unlocated`).
///
/// The directory a path names is itself the directory the user named, and anchors as one
/// (`ChainTrust::named`): reached through a symbolic link in a directory others can write, it
/// trusts nothing.
///
/// The directories opened here come back with the trust, for the caller to hold while it asks
/// for that trust: the trust holds them weakly.
fn parent_anchor(
    parent: &Rc<ftw::FileDescriptor>,
    id: (u64, u64),
    target: &Path,
) -> io::Result<(ChainTrust, HeldAnchor)> {
    let unlocated = || Ok((ChainTrust::unlocated(), None));
    let Some(name) = target.file_name() else {
        return unlocated();
    };
    let Ok(name) = CString::new(name.as_bytes()) else {
        return unlocated();
    };
    let path = match target.parent() {
        Some(path) if !path.as_os_str().is_empty() => path,
        _ => Path::new("."),
    };
    let opened = if parent.as_raw_fd() == libc::AT_FDCWD {
        let Ok(path) = CString::new(path.as_os_str().as_bytes()) else {
            return unlocated();
        };
        let flags = SEARCH_ONLY | libc::O_DIRECTORY | libc::O_CLOEXEC;
        match open_fd_at(libc::AT_FDCWD, &path, flags) {
            Ok(fd) => Some(Rc::new(fd)),
            Err(_) => return unlocated(),
        }
    } else {
        None
    };
    let dir = opened
        .as_ref()
        .map_or(parent.as_raw_fd(), |fd| fd.as_raw_fd());
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    let flags = libc::AT_SYMLINK_NOFOLLOW;
    if unsafe { libc::fstatat(dir, name.as_ptr(), &mut st, flags) } != 0 {
        return unlocated();
    }
    // Cast needed: `dev_t` is i32 on macOS and u64 on Linux.
    #[allow(clippy::unnecessary_cast)]
    if (st.st_dev as u64, st.st_ino as u64) != id {
        return unlocated();
    }
    match opened {
        Some(opened) => {
            let named = ChainTrust::named(path, &opened)?;
            Ok((named.hands.clone(), Some((opened, named))))
        }
        None => Ok((ChainTrust::anchor(parent)?, None)),
    }
}

/// What `parent_anchor` opened, held while its trust may be asked.
type HeldAnchor = Option<(Rc<OwnedFd>, NamedAnchor)>;

/// For a directory found existing at the destination path `target`, open on `fd` with identity
/// `id`, in the directory `parent` handing it `hands`: what it is given once its contents are
/// copied, and what it hands the directories found in it.
///
/// It takes what -p asks only where nobody else could have created its name, in its parent or
/// any directory above it up to the anchor; elsewhere it is left as it is, times included, and
/// that is reported (`ChainTrust::found_dir`, which pax follows too). (One this run made at that
/// path is its own, and not judged at all: `own_dir_trust`.)
fn found_dir_trust(
    hands: &OperandTrust,
    parent: &Rc<ftw::FileDescriptor>,
    fd: &Rc<ftw::FileDescriptor>,
    id: (u64, u64),
    target: &Path,
    requested: Preserve,
) -> io::Result<(DirFinish, ChainTrust)> {
    let (chain, _held) = match hands {
        // Without a mode or owner asked for, the named directory is given nothing, and its
        // name need not be read.
        OperandTrust::Named if !requested.mode && !requested.owner => {
            return Ok((
                DirFinish::Found(FoundDir::TimesOnly),
                ChainTrust::anchor(fd)?,
            ));
        }
        OperandTrust::Named => {
            let named = ChainTrust::named(target, fd)?;
            let finish = DirFinish::Found(named.named_dir(requested));
            return Ok((finish, named.hands.clone()));
        }
        OperandTrust::Parent => parent_anchor(parent, id, target)?,
        OperandTrust::Chain(chain) => (chain.clone(), None),
    };
    // Asked while the anchor `parent_anchor` opened is still held.
    let finish = DirFinish::Found(chain.found_dir(requested));
    Ok((finish, chain.found(fd)?))
}

/// For a directory found existing, open on `fd`, that this run made at the same path
/// (`MadeDirs::made_at`): what it is given once its contents are copied -- what -p asks, as
/// for any directory the run made -- and the trust it hands, afresh.
fn own_dir_trust(
    fd: &Rc<ftw::FileDescriptor>,
    requested: Preserve,
) -> io::Result<(DirFinish, ChainTrust)> {
    let finish = DirFinish::Found(requested.where_trusted());
    Ok((finish, ChainTrust::made(fd)?))
}

/// The -p failure reported for a directory found existing where others could have created its
/// name (`FoundDir::LeaveAlone`): nothing of it is changed, times included.
fn found_dir_left_alone(target: &Path) -> io::Error {
    io::Error::other(gettext!(
        "not preserving the owner, permissions or times of '{}': the directory was already \
         there, and others can create entries beside it",
        target.display()
    ))
}

/// `fstat` of a descriptor the caller keeps open (and goes on owning).
fn fd_metadata(fd: libc::c_int) -> io::Result<fs::Metadata> {
    std::mem::ManuallyDrop::new(unsafe { fs::File::from_raw_fd(fd) }).metadata()
}

/// The diagnostic for `open_made_dir` failing on the directory cp made at `path`.
///
/// Something other than a directory swapped in for it (ELOOP, ENOTDIR) is reported as one found
/// there would be ("exists but is not a directory"), any other system error as cp reports a
/// directory it cannot open; a diagnostic of cp's own, which already names the path (the
/// directory "was replaced after it was made"), is kept as it is.
pub fn made_dir_open_error(path: &Path, e: io::Error) -> io::Error {
    match e.raw_os_error() {
        Some(libc::ELOOP) | Some(libc::ENOTDIR) => io::Error::other(gettext!(
            "'{}' exists but is not a directory: {}",
            path.display(),
            error_string(&e)
        )),
        Some(_) => io::Error::other(gettext!(
            "cannot open directory '{}': {}",
            path.display(),
            error_string(&e)
        )),
        None => e,
    }
}

/// `openat(dirfd, name, flags)`.
fn open_fd_at(dirfd: libc::c_int, name: &CStr, flags: libc::c_int) -> io::Result<OwnedFd> {
    let fd = unsafe { libc::openat(dirfd, name.as_ptr(), flags) };
    if fd < 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(unsafe { OwnedFd::from_raw_fd(fd) })
}

/// Open the directory this copy has just made with `mkdirat` at `name` in `parent_fd`, for
/// reading, checked to be the one made (`verify_made_dir`), and with its owner's read, write and
/// search permission.
///
/// POSIX cp 2.e makes the directory with the source's permission bits "modified by the file
/// creation mask of the user ... OR'ed with S_IRWXU", but `mkdirat` applies the umask to the
/// S_IRWXU too: under a umask of 0400 the directory came out 0300, and cp could neither open it
/// to copy into nor read it to see that it was the one made. So it is pinned search-only first,
/// verified through the pin, given S_IRWXU through the pin when it is cp's own and lacks it, and
/// only then opened for reading -- through the pin's `self/fd/N` under a verified procfs, or,
/// without one, by name with its identity checked against the pin.
pub fn open_made_dir(
    parent_fd: libc::c_int,
    name: &CStr,
    target: &Path,
) -> io::Result<(OwnedFd, MadeTrust)> {
    let flags = SEARCH_ONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC;
    let pin = open_fd_at(parent_fd, name, flags)?;
    let trust = verify_made_dir(parent_fd, pin.as_raw_fd(), target)?;
    let md = fd_metadata(pin.as_raw_fd())?;
    // S_IRWXU is 0o700 (fixed by POSIX).
    if md.uid() == unsafe { libc::geteuid() } && md.mode() & 0o700 != 0o700 {
        let mode = ((md.mode() & 0o7777) | 0o700) as libc::mode_t;
        plib::madefs::chmod_fd(pin.as_raw_fd(), mode)?;
    }
    if let Some(opened) = reopen_through_procfs(&pin) {
        return Ok((opened?, trust));
    }
    // Without procfs, by name, refused unless it is still the pinned directory.
    let flags = libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC;
    let opened = open_fd_at(parent_fd, name, flags)?;
    let now = fd_metadata(opened.as_raw_fd())?;
    if (now.dev(), now.ino()) != (md.dev(), md.ino()) {
        return Err(io::Error::other(gettext!(
            "'{}' was replaced after it was made",
            target.display()
        )));
    }
    Ok((opened, trust))
}

/// The directory `pin` holds, opened again for reading through its `self/fd/N` under a
/// verified procfs, which names exactly the pinned inode; `None` without one.
#[cfg(target_os = "linux")]
fn reopen_through_procfs(pin: &OwnedFd) -> Option<io::Result<OwnedFd>> {
    let proc_dir = procfs_dir().ok()?;
    let path = proc_fd_name(pin.as_raw_fd());
    let flags = libc::O_RDONLY | libc::O_DIRECTORY | libc::O_CLOEXEC;
    Some(open_fd_at(proc_dir.as_raw_fd(), &path, flags))
}

/// Elsewhere there is no procfs to reopen through.
#[cfg(not(target_os = "linux"))]
fn reopen_through_procfs(_pin: &OwnedFd) -> Option<io::Result<OwnedFd>> {
    None
}

/// Check a directory cp has just made with `mkdirat` in `parent_fd` and then opened as `dir_fd`
/// (`O_DIRECTORY | O_NOFOLLOW`): between the two, anyone else who can rename entries in the
/// parent could have swapped in a directory of their own, and cp would copy into it (and,
/// under -p, give it the source's owner and mode).
///
/// Only the parent's owner, and anyone with group or other write permission on it when it is
/// not sticky, can do that; when that is nobody but cp's own user, there is nothing to check.
/// Otherwise the directory must be what a fresh `mkdirat` yields: empty, with the owner and
/// link count `made_by_us` accepts. (Write permission a POSIX ACL grants shows in the group
/// bits; one a macOS or NFSv4 ACL grants is not seen -- the residual documented at
/// `plib::madefs::others_can_rename`.)
///
/// An operand resolved from the working directory has no parent descriptor; its parent is then
/// read as the opened directory's own `..`, which names wherever that directory actually is.
///
/// Returns how far the directory is trusted: `ParentOwnerOnly` means -p must not give it an
/// owner or a mode (see `made_by_us`).
pub fn verify_made_dir(
    parent_fd: libc::c_int,
    dir_fd: libc::c_int,
    dir: &Path,
) -> io::Result<MadeTrust> {
    // The rule, shared with pax: `plib::madefs::verify_made_dir`. Every fact about the made
    // directory comes from the descriptor cp goes on to use; reading it to see that it is
    // empty borrows its owner's read permission when a umask withheld it.
    plib::madefs::verify_made_dir(parent_fd, dir_fd)?.ok_or_else(|| {
        io::Error::other(gettext!(
            "'{}' was replaced after it was made",
            dir.display()
        ))
    })
}

/// The -p failure reported for an object `made_by_us` trusted only as `ParentOwnerOnly`: its
/// mode cannot be duplicated (POSIX requires a diagnostic), and neither its owner nor its mode
/// is touched.
fn owner_unverified_error(target: &Path) -> io::Error {
    io::Error::other(gettext!(
        "not preserving the owner and permissions of '{}': its owner could not be verified",
        target.display()
    ))
}

enum CopyResult {
    CopyingDirectory(DirOrigin),
    /// A non-directory was copied.
    CopiedFile(CopiedFile),
    Skipped,
}

/// A non-directory this copy made.
struct CopiedFile {
    /// `(st_dev, st_ino)` of what was made, when known for certain (`FirstCopy::made`).
    made: Option<(u64, u64)>,
    /// The source as it was duplicated: for a regular file, from the `fstat` of the descriptor
    /// read, taken before the read; otherwise as the walk saw it.
    source: SourceState,
    /// Any failure to duplicate its characteristics (-p), which is reported but never undoes
    /// the copy.
    preserve_error: Option<io::Error>,
}

/// S_ISUID | S_ISGID, whose values POSIX fixes.
const ID_BITS: u32 = 0o6000;

/// The mode -p gives the copy: the source's twelve permission bits, less set-user-ID and
/// set-group-ID when the owner could not be duplicated (POSIX cp 90720-90721, mv 108104-108105).
fn preserved_mode(source_md: &impl MetadataExt, chown_ok: bool) -> libc::mode_t {
    let mut mode = source_md.mode() & 0o7777;
    if !chown_ok {
        mode &= !ID_BITS;
    }
    mode as libc::mode_t
}

/// The source's [access, modification] times, as `futimens`/`utimensat` take them.
fn source_times(source_md: &impl MetadataExt) -> [libc::timespec; 2] {
    [
        libc::timespec {
            tv_sec: source_md.atime(),
            tv_nsec: source_md.atime_nsec(),
        },
        libc::timespec {
            tv_sec: source_md.mtime(),
            tv_nsec: source_md.mtime_nsec(),
        },
    ]
}

fn preserve_times_error(target: &Path, e: &io::Error) -> io::Error {
    io::Error::other(gettext!(
        "failed to preserve times for '{}': {}",
        target.display(),
        error_string(e)
    ))
}

fn preserve_mode_error(target: &Path, e: &io::Error) -> io::Error {
    io::Error::other(gettext!(
        "failed to preserve permissions for '{}': {}",
        target.display(),
        error_string(e)
    ))
}

/// -p through a descriptor cp holds for the destination it created or opened: times, then
/// owner, then mode. Nothing is resolved by name, so a file renamed over the destination after
/// cp opened it is never touched. The mode (set-user-ID included) is applied last, only to a
/// file whose owner is already final.
///
/// For an object trusted only as `ParentOwnerOnly` the times are applied and the owner and
/// mode are not: that is reported (`owner_unverified_error`).
pub fn preserve_through_fd(
    fd: libc::c_int,
    source_md: &impl MetadataExt,
    target: &Path,
    trust: MadeTrust,
) -> io::Result<()> {
    let times = source_times(source_md);
    if unsafe { libc::futimens(fd, times.as_ptr()) } != 0 {
        return Err(preserve_times_error(target, &io::Error::last_os_error()));
    }
    if trust == MadeTrust::ParentOwnerOnly {
        return Err(owner_unverified_error(target));
    }
    // A failure to duplicate the owner is not itself reported (POSIX leaves it unspecified);
    // its consequence is the mode below.
    let chown_ok = unsafe { libc::fchown(fd, source_md.uid(), source_md.gid()) } == 0;
    if unsafe { libc::fchmod(fd, preserved_mode(source_md, chown_ok)) } != 0 {
        return Err(preserve_mode_error(target, &io::Error::last_os_error()));
    }
    Ok(())
}

/// The mode a directory cp made without -p ends with (POSIX cp 2.g): the source's nine file
/// permission bits less the umask, in place of the S_IRWXU-widened ones 2.e created it with.
/// Every bit above those nine stays as `made` has it -- the sticky bit `mkdirat` applied, a
/// set-group-ID bit inherited from its parent -- as GNU cp leaves them.
fn made_dir_mode(made: u32, source: u32, umask: u32) -> u32 {
    (made & 0o7000) | (source & 0o777 & !umask)
}

/// POSIX cp 2.g without -p, for a directory cp made, once its contents are copied: set its
/// mode (`made_dir_mode`) through the descriptor cp holds for it, never by name. Nothing is
/// changed when the mode is already right.
pub fn finish_made_dir_mode(
    fd: libc::c_int,
    source_md: &impl MetadataExt,
    umask: u32,
    target: &Path,
) -> io::Result<()> {
    let set_mode_error = |e: &io::Error| {
        io::Error::other(gettext!(
            "cannot set permissions for '{}': {}",
            target.display(),
            error_string(e)
        ))
    };
    let made = fd_metadata(fd).map_err(|e| set_mode_error(&e))?.mode() & 0o7777;
    let wanted = made_dir_mode(made, source_md.mode(), umask);
    if wanted != made && unsafe { libc::fchmod(fd, wanted as libc::mode_t) } != 0 {
        return Err(set_mode_error(&io::Error::last_os_error()));
    }
    Ok(())
}

/// A destination directory's attributes, once its contents are copied so nothing written into
/// it moves its times afterwards: under -p the source's owner, mode and times on one this copy
/// made, and on one it found only where nobody else could have created its name
/// (`found_dir_trust`); without -p the final mode of one this copy made
/// (`finish_made_dir_mode`), and nothing on one it found. Applied through `fd`, the descriptor
/// the copy has held for the directory since it entered it, never by name. The source's
/// metadata is what the walk recorded when it stat'ed the directory, before reading it: the
/// read moved its access time, and GNU keeps the original.
fn finish_dir(
    fd: libc::c_int,
    source: &ftw::Entry<'_>,
    finish: DirFinish,
    preserve: bool,
    umask: u32,
    target: &Path,
) -> io::Result<()> {
    let trust = match finish {
        DirFinish::Made(trust) => trust,
        // cp's -p asks for the times along with the mode and owner, and without it a
        // directory found is given nothing.
        DirFinish::Found(FoundDir::TimesOnly) => return Ok(()),
        DirFinish::Found(FoundDir::LeaveAlone) => return Err(found_dir_left_alone(target)),
        DirFinish::Found(FoundDir::AsRequested) if preserve => MadeTrust::Full,
        DirFinish::Found(FoundDir::AsRequested) => return Ok(()),
    };
    let source_md = source
        .metadata()
        .ok_or_else(|| io::Error::other(gettext!("cannot stat '{}'", source.path())))?;
    if preserve {
        // A directory made but trusted only as owned like its parent gets no owner and no
        // mode.
        preserve_through_fd(fd, source_md, target, trust)
    } else {
        finish_made_dir_mode(fd, source_md, umask, target)
    }
}

/// The source's metadata as it is now, through the directory descriptor the walk used, and
/// required to be the very file the walk recorded: an entry swapped since must not lend its
/// owner and mode to the copy. Used for symbolic links and special files, whose data cp does
/// not read (a regular file's times come from its descriptor before the read, a directory's
/// from the walk's stat before the walk read it).
fn fresh_source_md(source: &ftw::Entry) -> io::Result<ftw::Metadata> {
    let changed = || io::Error::other(gettext!("'{}' changed during the copy", source.path()));
    let recorded = source.metadata().ok_or_else(changed)?;
    // The walk recorded a followed link's referent; stat the same thing.
    let follow = source.is_symlink() == Some(true) && !recorded.is_symlink();
    let md = ftw::Metadata::new(source.dir_fd(), source.file_name(), follow)?;
    if md.dev() != recorded.dev() || md.ino() != recorded.ino() {
        return Err(changed());
    }
    Ok(md)
}

/// -p for a symbolic link or special file cp just made with `symlinkat`/`mknodat`, which return
/// no descriptor.
///
/// What the name holds must be what cp made: the same file type, with the owner and link count
/// `made_by_us` accepts. On Linux the node is pinned first with an `O_PATH | O_NOFOLLOW`
/// descriptor, every check reads that descriptor's `fstat`/`fstatfs`, and every change goes
/// through it: owner and times with `AT_EMPTY_PATH`, the mode of a special file through
/// `/proc/self/fd`, which names exactly that inode. Elsewhere the node is checked by `lstat`
/// and changed by name with `AT_SYMLINK_NOFOLLOW`, each change after re-checking the identity --
/// the residual is a replacement between that check and the call. A symbolic link's own mode
/// is never set: Linux has none to set, and no access check reads it anywhere.
///
/// The parent's owner is read from `dirfd` itself (`.` relative to it resolves no name). An
/// operand resolved from the working directory has no parent descriptor, so only an object
/// owned by cp's effective user is accepted there.
fn preserve_made_node(
    dirfd: libc::c_int,
    name: &CStr,
    made_type: ftw::FileType,
    source_md: &ftw::Metadata,
    target: &Path,
) -> io::Result<()> {
    let parent_uid = if dirfd == libc::AT_FDCWD {
        None
    } else {
        Some(ftw::Metadata::new(dirfd, c".", false)?.uid())
    };
    let check = |uid: u32, nlink: u64, type_ok: bool, owners: FsOwners| {
        let made = MadeObject {
            uid,
            nlink,
            is_dir: false,
            owners,
        };
        match made_by_us(made, parent_uid, unsafe { libc::geteuid() }) {
            Some(trust) if type_ok => Ok(trust),
            _ => Err(io::Error::other(gettext!(
                "'{}' was replaced during the copy",
                target.display()
            ))),
        }
    };
    preserve_node_attributes(dirfd, name, made_type, check, source_md, target)
}

#[cfg(target_os = "linux")]
fn preserve_node_attributes(
    dirfd: libc::c_int,
    name: &CStr,
    made_type: ftw::FileType,
    check: impl Fn(u32, u64, bool, FsOwners) -> io::Result<MadeTrust>,
    source_md: &ftw::Metadata,
    target: &Path,
) -> io::Result<()> {
    let fd = unsafe {
        libc::openat(
            dirfd,
            name.as_ptr(),
            libc::O_PATH | libc::O_NOFOLLOW | libc::O_CLOEXEC,
        )
    };
    if fd < 0 {
        return Err(io::Error::last_os_error());
    }
    let fd = unsafe { std::os::fd::OwnedFd::from_raw_fd(fd) };
    let pinned = fs::File::from(fd);
    let pinned_md = pinned.metadata()?;
    let trust = check(
        pinned_md.uid(),
        pinned_md.nlink(),
        same_file_type(pinned_md.file_type(), made_type),
        fs_owners(pinned.as_raw_fd()),
    )?;
    let fd = pinned.as_raw_fd();
    let empty = c"";

    let times = source_times(source_md);
    let symlink = made_type == ftw::FileType::SymbolicLink;
    match utimens_pinned(dirfd, name, &pinned, symlink, &times) {
        Ok(true) => {}
        Ok(false) => {
            return Err(io::Error::other(gettext!(
                "'{}' was replaced during the copy",
                target.display()
            )))
        }
        Err(e) => return Err(preserve_times_error(target, &e)),
    }
    if trust == MadeTrust::ParentOwnerOnly {
        return Err(owner_unverified_error(target));
    }
    let chown_ok = unsafe {
        libc::fchownat(
            fd,
            empty.as_ptr(),
            source_md.uid(),
            source_md.gid(),
            libc::AT_EMPTY_PATH,
        )
    } == 0;
    if made_type != ftw::FileType::SymbolicLink {
        chmod_pinned(fd, preserved_mode(source_md, chown_ok))
            .map_err(|e| preserve_mode_error(target, &e))?;
    }
    Ok(())
}

/// Set the times of the node `pinned` holds -- made at `name` in `dirfd` -- through the pin.
///
/// `utimensat(AT_EMPTY_PATH)` (Linux 5.8 and later). Before that it fails with EINVAL, and then
/// a special file goes through `self/fd/N` in a `/proc` verified to be procfs, which names
/// exactly the pinned inode. A symbolic link has no such route -- that path is followed to the
/// link and then through it -- so its times go by name with `AT_SYMLINK_NOFOLLOW`, only once a
/// fresh `lstat` shows the name still holds the pinned link (`utimens_link_if_still`, whose
/// residual -- a wrong mtime, at worst -- is documented there). `false` when the name no longer
/// does, for the caller to report against the full target path.
#[cfg(target_os = "linux")]
fn utimens_pinned(
    dirfd: libc::c_int,
    name: &CStr,
    pinned: &fs::File,
    symlink: bool,
    times: &[libc::timespec; 2],
) -> io::Result<bool> {
    let fd = pinned.as_raw_fd();
    if unsafe { libc::utimensat(fd, c"".as_ptr(), times.as_ptr(), libc::AT_EMPTY_PATH) } == 0 {
        return Ok(true);
    }
    let e = io::Error::last_os_error();
    if e.raw_os_error() != Some(libc::EINVAL) {
        return Err(e);
    }
    if symlink {
        let md = pinned.metadata()?;
        return utimens_link_if_still(dirfd, name, (md.dev(), md.ino()), times);
    }
    let proc_dir = procfs_dir()?;
    let path = proc_fd_name(fd);
    if unsafe { libc::utimensat(proc_dir.as_raw_fd(), path.as_ptr(), times.as_ptr(), 0) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(true)
}

#[cfg(not(target_os = "linux"))]
fn preserve_node_attributes(
    dirfd: libc::c_int,
    name: &CStr,
    made_type: ftw::FileType,
    check: impl Fn(u32, u64, bool, FsOwners) -> io::Result<MadeTrust>,
    source_md: &ftw::Metadata,
    target: &Path,
) -> io::Result<()> {
    let made = ftw::Metadata::new(dirfd, name, false)?;
    // Without the filesystem type, owners are taken to be stored: only cp's own are trusted.
    let trust = check(
        made.uid(),
        made.nlink(),
        made.file_type() == made_type,
        FsOwners::Stored,
    )?;
    if trust == MadeTrust::ParentOwnerOnly {
        return Err(owner_unverified_error(target));
    }
    let still_made = || -> io::Result<()> {
        let md = ftw::Metadata::new(dirfd, name, false)?;
        if md.dev() != made.dev() || md.ino() != made.ino() || md.file_type() != made_type {
            return Err(io::Error::other(gettext!(
                "'{}' was replaced during the copy",
                target.display()
            )));
        }
        Ok(())
    };

    still_made()?;
    let times = source_times(source_md);
    if unsafe {
        libc::utimensat(
            dirfd,
            name.as_ptr(),
            times.as_ptr(),
            libc::AT_SYMLINK_NOFOLLOW,
        )
    } != 0
    {
        return Err(preserve_times_error(target, &io::Error::last_os_error()));
    }
    still_made()?;
    let chown_ok = unsafe {
        libc::fchownat(
            dirfd,
            name.as_ptr(),
            source_md.uid(),
            source_md.gid(),
            libc::AT_SYMLINK_NOFOLLOW,
        )
    } == 0;
    if made_type != ftw::FileType::SymbolicLink {
        still_made()?;
        let mode = preserved_mode(source_md, chown_ok);
        if unsafe { libc::fchmodat(dirfd, name.as_ptr(), mode, libc::AT_SYMLINK_NOFOLLOW) } != 0 {
            return Err(preserve_mode_error(target, &io::Error::last_os_error()));
        }
    }
    Ok(())
}

// Implements the algorithm for `cp`:
//
// https://pubs.opengroup.org/onlinepubs/9699919799/utilities/cp.html
/// State carried across every entry of one `copy_file` walk.
struct CopyState<'a> {
    /// Destinations this copy has already written, so a later source cannot clobber one.
    created_files: &'a mut HashSet<PathBuf>,
    /// Identity of every destination directory this copy created or entered.
    dest_dir_ids: &'a RefCell<HashSet<(u64, u64)>>,
    /// The operands as the user wrote them, for the diagnostics that must name them rather than
    /// the entry the failure was noticed on.
    operands: (&'a Path, &'a Path),
    /// Whether the entry is the source operand itself, copied to the target operand.
    at_top_level: bool,
}

/// The pathname stored in the symbolic link `source`.
///
/// `ftw` fills in `Entry::read_link` for every symbolic link, but reading it here keeps this
/// correct regardless of how the walk was configured -- the alternative was an `unwrap` that
/// turned a missing value into a process abort.
fn read_source_link(source: &ftw::Entry) -> io::Result<CString> {
    if let Some(link) = source.read_link() {
        return Ok(link.to_owned());
    }

    // Deliberately no errno: the `readlinkat` that failed ran inside the traversal, which
    // reported it, and several other syscalls have overwritten `errno` since.
    Err(io::Error::other(gettext!(
        "cannot read symbolic link '{}'",
        source.path()
    )))
}

/// Renders a mode as the pair used in the overwrite prompt: four octal digits, and the nine
/// `rwx` characters.
fn format_mode(mode: u32) -> (String, String) {
    let mut mode_str = String::new();
    let bit_loc = 0o400;
    for i in 0..9 {
        let mask = bit_loc >> i;
        if mode & mask != 0 {
            match i % 3 {
                0 => mode_str.push('r'),
                1 => mode_str.push('w'),
                2 => mode_str.push('x'),
                _ => (),
            }
        } else {
            mode_str.push('-');
        }
    }

    // `gettext!` takes no format spec, so the octal has to be rendered separately.
    (format!("{:04o}", mode & 0o7777), mode_str)
}

fn copy_file_impl<F>(
    cfg: &CopyConfig,
    source: &ftw::Entry,
    target: &Path,
    target_dirfd: libc::c_int,
    target_filename: *const libc::c_char,
    state: &mut CopyState<'_>,
    prompt_fn: F,
) -> io::Result<CopyResult>
where
    F: Fn(&str) -> bool,
{
    let source_md = source.metadata().unwrap();
    let at_top_level = state.at_top_level;
    let deref_this_entry = cfg.deref.deref_entry(at_top_level);
    // Act on the link itself only when the options say so (POSIX 90689).
    let act_on_link_itself = source.is_symlink().unwrap_or(false) && !deref_this_entry;
    let source_file_type = source_md.file_type();
    let source_is_dir = source_file_type == ftw::FileType::Directory;

    let source_is_special_file = matches!(
        source_file_type,
        ftw::FileType::BlockDevice
            | ftw::FileType::CharacterDevice
            | ftw::FileType::Fifo
            | ftw::FileType::Socket
    );
    let source_deref_md = unsafe { ftw::Metadata::new(source.dir_fd(), source.file_name(), true) };

    // A link we were told to act through, whose referent does not exist, is an error -- there is
    // nothing to copy. Without this the failure surfaced later as "cannot open ... for reading".
    // (Under -P the link itself is the subject, and a dangling one is reproduced as-is.)
    if deref_this_entry && source.is_symlink().unwrap_or(false) {
        if let Err(e) = &source_deref_md {
            return Err(io::Error::other(gettext!(
                "cannot stat '{}': {}",
                source.path(),
                error_string(e)
            )));
        }
    }

    // A destination the copy must create is not examined at all: it is taken to be absent, so
    // every path below creates it exclusively, and anything at its name makes that fail.
    let must_create = at_top_level && cfg.destination == Destination::MustCreate;
    let examine_target = |follow: bool| {
        if must_create {
            Err(io::Error::from_raw_os_error(libc::ENOENT))
        } else {
            ftw::Metadata::new(
                target_dirfd,
                unsafe { CStr::from_ptr(target_filename) },
                follow,
            )
        }
    };
    let target_symlink_md = examine_target(false);
    let target_deref_md = examine_target(true);
    let target_is_dangling_symlink = target_symlink_md.is_ok() && target_deref_md.is_err();

    let target_symlink_md = match target_symlink_md {
        Ok(md) => Some(md),
        Err(e) => {
            if e.kind() == io::ErrorKind::NotFound {
                None
            } else {
                let err_str =
                    gettext!("cannot access '{}': {}", target.display(), error_string(&e));
                return Err(io::Error::other(err_str));
            }
        }
    };

    let target_is_dir = match &target_symlink_md {
        Some(md) => md.file_type() == ftw::FileType::Directory,
        None => false,
    };

    let target_exists = target_symlink_md.is_some();

    // -l: a destination that already is the link asked for -- the very file `link_source` would
    // link, a symbolic link itself unless it is followed -- is left as it is (GNU succeeds too).
    // Any other destination is for the -l branch below to refuse or replace.
    if cfg.link {
        let linked = if deref_this_entry {
            source_deref_md.as_ref().ok()
        } else {
            Some(source_md)
        };
        if let (Some(smd), Some(tmd)) = (linked, &target_symlink_md) {
            if smd.dev() == tmd.dev() && smd.ino() == tmd.ino() {
                return Ok(CopyResult::Skipped);
            }
        }
    }
    // 1. If source_file references the same file as dest_file
    else if let (Ok(smd), Ok(tmd)) = (&source_deref_md, &target_deref_md) {
        if smd.dev() == tmd.dev() && smd.ino() == tmd.ino() {
            let err_str = gettext!(
                "'{}' and '{}' are the same file",
                source.path(),
                target.display()
            );
            return Err(io::Error::other(err_str));
        }
    }

    // 2. If source_file is of type directory
    if source_is_dir {
        // 2.a
        if !cfg.recursive {
            let err_str = gettext!("-r not specified; omitting directory '{}'", source.path());
            return Err(io::Error::other(err_str));
        }

        // 2.b `fs::read_dir` skips `.` and `..`. Any occurence means it comes
        // from the input to `cp`.

        // 2.d
        if target_exists && !target_is_dir {
            let err_str = gettext!(
                "cannot overwrite non-directory '{}' with directory '{}'",
                target.display(),
                source.path()
            );
            return Err(io::Error::other(err_str));
        }

        // Refuse to descend into a directory that is one of this copy's own destinations, which
        // is what a copy into itself looks like from the inside. The previous test compared path
        // text, so "./a" and "a" named the same directory without matching, and `cp -R ./a a/b`
        // recursed until the filesystem filled. Identity cannot be spelled two ways.
        //
        // The message names the operands, not this entry: the recursion is only visible several
        // levels down, but what the user got wrong is the pair they typed.
        // Borrowed only for the test: the caller mutates this set while handling the result.
        let copying_into_self = state
            .dest_dir_ids
            .borrow()
            .contains(&(source_md.dev(), source_md.ino()));
        if copying_into_self {
            let (source_arg, target_arg) = state.operands;
            let err_str = gettext!(
                "cannot copy a directory, '{}', into itself, '{}'",
                source_arg.display(),
                target_arg.display()
            );
            return Err(io::Error::other(err_str));
        }

        // 2.e
        if !target_exists {
            unsafe {
                // Creates the target directory with the same file permission bits as the source,
                // modified by the umask of the process and OR'ed with S_IRWXU. Its final mode
                // (2.g; under -p, the source's without the umask) is set by `finish_dir` once
                // its contents are copied. Under -p it is made owner-only: until `finish_dir`
                // duplicates the owner, the directory belongs to whoever ran cp, and group or
                // other write permission would let others plant entries in it.
                let mode = if cfg.preserve {
                    libc::S_IRWXU
                } else {
                    // OR'ed with S_IRWXU according to the spec
                    source_md.mode() as libc::mode_t | libc::S_IRWXU
                };
                let ret = libc::mkdirat(target_dirfd, target_filename, mode);

                if ret != 0 {
                    let e = io::Error::last_os_error();
                    let err_str = gettext!(
                        "cannot create directory '{}': {}",
                        target.display(),
                        error_string(&e)
                    );
                    return Err(io::Error::other(err_str));
                }
            }
        }

        Ok(CopyResult::CopyingDirectory(match &target_symlink_md {
            Some(md) => DirOrigin::Found {
                dev: md.dev(),
                ino: md.ino(),
            },
            None => DirOrigin::Made,
        }))
    } else {
        // 3. If source_file is of type regular file

        // When the options say not to follow this entry's links, refuse to follow one that
        // appeared between the traversal's `lstat` and this open. GNU guards the same way.
        //
        // Every open of a source or destination here carries `O_NOCTTY`: a terminal device
        // copied from or to (`cp /dev/tty x`, a pty swapped in) must never become cp's
        // controlling terminal.
        let source_open_flags = libc::O_RDONLY
            | libc::O_NOCTTY
            | if deref_this_entry {
                0
            } else {
                libc::O_NOFOLLOW
            };

        // A destination found absent (or just unlinked by -f) is created exclusively: one that
        // appears after the check, a symbolic link included, is never written through or into.
        // Under -n that EEXIST is the skip; otherwise it is reported. The one non-exclusive
        // create is POSIX's write through a dangling symbolic link that is the operand itself
        // (see below). Returns whether the copy was made.
        //
        // That write-through also truncates: a file that appears at the link's target between
        // the check and the open is what the link now names, so it is replaced as a resolving
        // operand link's target is, never written into with its old tail left behind. There is
        // no earlier identity to compare it with -- the referent did not exist when checked --
        // and the link followed is the operand itself, resolved in the directory cp holds.
        let write_through_dangling = target_is_dangling_symlink && !cfg.no_clobber;
        let create_flags = libc::O_WRONLY
            | libc::O_NOCTTY
            | libc::O_CREAT
            | if write_through_dangling {
                libc::O_TRUNC
            } else {
                libc::O_EXCL
            };
        // Returns the source's metadata from before the read, and the new destination, still
        // open; or `None` for a -n skip.
        let create_target_then_copy = || -> io::Result<Option<(fs::Metadata, fs::File)>> {
            let (mut source_file, source_before_read) =
                open_source(source, source_md, source_open_flags)?;

            // 3.b. POSIX 90670-90671 asks for source_file's permission bits. The set-user-ID and
            // set-group-ID bits are masked off: the copy belongs to whoever ran cp, so carrying
            // them over would hand that user's privileges to anyone who can run it. Under -p the
            // file is created owner-only, because until the copy is complete and its owner
            // duplicated it holds another user's data under the invoker's ownership; the final
            // mode, set-user-ID included, is applied through this same descriptor afterwards
            // (`preserve_through_fd`), only once the owner is right. GNU creates it the same way.
            let create_mode = if cfg.preserve {
                0o600
            } else {
                source_md.mode() & 0o777
            };
            let target_fd = unsafe {
                libc::openat(
                    target_dirfd,
                    target_filename,
                    create_flags | libc::O_CLOEXEC,
                    create_mode,
                )
            };
            if target_fd == -1 {
                let e = io::Error::last_os_error();
                if cfg.no_clobber && e.raw_os_error() == Some(libc::EEXIST) {
                    return Ok(None);
                }

                // `ErrorKind::IsADirectory` is unstable:
                // https://github.com/rust-lang/rust/issues/86442
                let err_msg = if let Some(libc::EISDIR) = e.raw_os_error() {
                    // EISDIR -> ENOTDIR is to match the diagnostic from
                    // coreutils/tests/cp/trailing-slash.sh
                    error_string(&io::Error::from_raw_os_error(libc::ENOTDIR))
                } else {
                    error_string(&e)
                };
                let err_str = gettext!(
                    "cannot create regular file '{}': {}",
                    target.display(),
                    err_msg
                );
                return Err(io::Error::other(err_str));
            }
            let mut target_file = unsafe { fs::File::from_raw_fd(target_fd) };

            // 3.d
            io::copy(&mut source_file, &mut target_file)?;

            Ok(Some((source_before_read, target_file)))
        };

        // -n: any existing destination, a dangling link included, is left alone.
        if cfg.no_clobber && target_exists {
            return Ok(CopyResult::Skipped);
        }

        // POSIX creates the file a dangling destination link names, which GNU does only under
        // POSIXLY_CORRECT. Inside a recursive copy that link is whatever the destination tree
        // holds, and following it can write anywhere, so it is done only for the operand the
        // user named; below it the link is refused in GNU's words. A link that resolves is
        // refused below the operand for the same reason (GNU writes through it): the tree's
        // owner chose where it points, not the user who ran cp. (A link to be reproduced as a
        // link, a special file, or a hard link under -l, which writes nothing, replaces the link
        // instead, or is refused like any existing destination.)
        let target_is_symlink = target_symlink_md
            .as_ref()
            .is_some_and(|md| md.file_type() == ftw::FileType::SymbolicLink);
        if target_is_symlink
            && !state.at_top_level
            && !act_on_link_itself
            && !(source_is_special_file && cfg.recursive)
            && !cfg.link
        {
            return Err(io::Error::other(if target_is_dangling_symlink {
                gettext!(
                    "not writing through dangling symlink '{}'",
                    target.display()
                )
            } else {
                gettext!("not writing through symlink '{}'", target.display())
            }));
        }

        // 3.a
        if target_exists && !target_is_dangling_symlink {
            if state.created_files.contains(target) {
                let err_str = gettext!(
                    "will not overwrite just-created '{}' with '{}'",
                    target.display(),
                    source.path(),
                );
                return Err(io::Error::other(err_str));
            }

            // 3.a.i. The prompt belongs to -i alone (POSIX 90703-90705). -f is only step
            // 3.a.iii -- "if the descriptor cannot be obtained, unlink and proceed" (90699-90700)
            // -- so it must never prompt: with no terminal the prompt read EOF, took it for a
            // refusal, and exited 0 having copied nothing. An unwritable destination only
            // changes the wording, as it does in GNU cp.
            if cfg.interactive {
                let target_is_writable =
                    ftw::is_writable_at(target_dirfd, unsafe { CStr::from_ptr(target_filename) });

                let is_affirm = if target_is_writable {
                    prompt_fn(&gettext!("overwrite '{}'?", target.display()))
                } else {
                    let (mode_octal, mode_str) =
                        format_mode(target_symlink_md.as_ref().unwrap().mode());
                    prompt_fn(&gettext!(
                        "replace '{}', overriding mode {} ({})?",
                        target.display(),
                        mode_octal,
                        mode_str
                    ))
                };
                if !is_affirm {
                    return Ok(CopyResult::Skipped);
                }
            }
        }

        // A destination that exists and is not a dangling link has now been checked against the
        // just-created set and prompted for. Everything below replaces it.
        let replacing_existing = target_exists && !target_is_dangling_symlink;

        if cfg.link {
            // GNU replaces an existing destination under -f, or once -i was answered yes.
            if target_is_dir {
                return Err(io::Error::other(gettext!(
                    "cannot overwrite directory '{}' with non-directory '{}'",
                    target.display(),
                    source.path()
                )));
            }
            let replace = target_exists && (cfg.force || (cfg.interactive && replacing_existing));
            if target_exists && !replace {
                let e = io::Error::from_raw_os_error(libc::EEXIST);
                return Err(hard_link_error(target, source, &e));
            }
            let linked = link_source(
                source,
                source_md,
                deref_this_entry,
                target,
                target_dirfd,
                target_filename,
                replace,
            )?;
            state.created_files.insert(target.to_path_buf());
            return Ok(linked);
        }

        // 4. -R is required for a FIFO, device or socket; without it the contents are read like
        // any other file, which is what makes `cp /dev/null x` work.
        if source_is_special_file && cfg.recursive {
            if target_is_dir {
                let err_str = gettext!(
                    "cannot overwrite directory '{}' with non-directory '{}'",
                    target.display(),
                    source.path()
                );
                return Err(io::Error::other(err_str));
            }
            // `mknodat` creates exclusively; under -n a destination that appeared since the
            // check above is left alone.
            return match copy_special_file(
                source_md,
                source_file_type,
                target,
                target_dirfd,
                target_filename,
                target_exists,
                state.created_files,
            ) {
                Ok(()) => Ok(copied_node(
                    cfg,
                    source,
                    source_md,
                    target_dirfd,
                    target_filename,
                    source_file_type,
                    target,
                )),
                Err(e) if cfg.no_clobber && e.kind() == io::ErrorKind::AlreadyExists => {
                    Ok(CopyResult::Skipped)
                }
                Err(e) => Err(e),
            };
        }

        // 4.c
        if act_on_link_itself {
            let link_target = read_source_link(source)?;

            if target_exists {
                if target_is_dir {
                    let err_str = gettext!(
                        "cannot overwrite directory '{}' with non-directory '{}'",
                        target.display(),
                        source.path()
                    );
                    return Err(io::Error::other(err_str));
                }
                // Also covers a dangling destination link, which `symlinkat` would otherwise
                // refuse with EEXIST.
                let ret = unsafe { libc::unlinkat(target_dirfd, target_filename, 0) };
                if ret != 0 {
                    let e = io::Error::last_os_error();
                    return Err(io::Error::other(gettext!(
                        "cannot remove '{}': {}",
                        target.display(),
                        error_string(&e)
                    )));
                }
            }

            let ret =
                unsafe { libc::symlinkat(link_target.as_ptr(), target_dirfd, target_filename) };
            if ret != 0 {
                let e = io::Error::last_os_error();
                // -n: a destination that appeared since the existence check is left alone.
                if cfg.no_clobber && e.raw_os_error() == Some(libc::EEXIST) {
                    return Ok(CopyResult::Skipped);
                }
                return Err(io::Error::other(gettext!(
                    "cannot create symbolic link '{}': {}",
                    target.display(),
                    error_string(&e)
                )));
            }
            state.created_files.insert(target.to_path_buf());
            return Ok(copied_node(
                cfg,
                source,
                source_md,
                target_dirfd,
                target_filename,
                ftw::FileType::SymbolicLink,
                target,
            ));
        }

        let (source_before_read, target_file) = if replacing_existing {
            if target_is_dir {
                let err_str = gettext!(
                    "cannot overwrite directory '{}' with non-directory '{}'",
                    target.display(),
                    source.path()
                );
                return Err(io::Error::other(err_str));
            }

            // 3.a.ii. Open the source first: truncating the destination before knowing the
            // source can be read destroyed its contents and then reported a failure.
            let (mut source_file, source_before_read) =
                open_source(source, source_md, source_open_flags)?;

            // Open what was checked above, and nothing else. A link is followed only when the
            // check saw one, which is now only the operand itself (POSIX writes through it);
            // otherwise `O_NOFOLLOW` refuses a link swapped in since. Either way the descriptor
            // must be the file the check examined -- the link's referent, or the file itself --
            // and only then is it truncated: `O_TRUNC` at the open would have emptied a file
            // swapped in before its identity could be checked.
            let (open_flags, expected_md) = if target_is_symlink {
                (
                    libc::O_WRONLY | libc::O_NOCTTY | libc::O_CLOEXEC,
                    target_deref_md.as_ref().ok(),
                )
            } else {
                (
                    libc::O_WRONLY | libc::O_NOCTTY | libc::O_CLOEXEC | libc::O_NOFOLLOW,
                    target_symlink_md.as_ref(),
                )
            };
            // A regular file is opened `O_NONBLOCK`, so a FIFO swapped in for it fails the open
            // (ENXIO, no reader) instead of waiting for a reader; the flag is cleared once the
            // descriptor is known to be the checked file (`openat_guarded`, which also waits out
            // a lease). A destination that was checked as a FIFO or device (`cp x /dev/null`)
            // is opened as before, blocking.
            let expect_regular =
                expected_md.is_some_and(|md| md.file_type() == ftw::FileType::RegularFile);
            let target_fd =
                openat_guarded(target_dirfd, target_filename, open_flags, expect_regular);
            if target_fd != -1 {
                let mut target_file = unsafe { fs::File::from_raw_fd(target_fd) };
                let opened_md = target_file.metadata()?;
                // The type too: a file unlinked and replaced can hand its inode number on.
                let is_checked_file = expected_md.is_some_and(|md| {
                    md.dev() == opened_md.dev()
                        && md.ino() == opened_md.ino()
                        && same_file_type(opened_md.file_type(), md.file_type())
                });
                if !is_checked_file {
                    return Err(io::Error::other(gettext!(
                        "will not write to '{}': it changed after it was checked",
                        target.display()
                    )));
                }
                if expect_regular {
                    clear_nonblock(target_fd)?;
                    // Truncated only now, and only a regular file: `O_TRUNC` is ignored for a
                    // FIFO or terminal, and ftruncate would refuse one.
                    target_file.set_len(0).map_err(|e| {
                        io::Error::other(gettext!(
                            "cannot truncate '{}': {}",
                            target.display(),
                            error_string(&e)
                        ))
                    })?;
                }

                io::copy(&mut source_file, &mut target_file)?;
                (source_before_read, target_file)
            } else {
                // 3.a.iii
                if cfg.force {
                    // Plain `unlinkat`: a directory destination was refused above, and removing
                    // one to put a file in its place is not something -f asks for.
                    let ret = unsafe { libc::unlinkat(target_dirfd, target_filename, 0) };
                    if ret != 0 {
                        let e = io::Error::last_os_error();
                        return Err(io::Error::other(gettext!(
                            "cannot remove '{}': {}",
                            target.display(),
                            error_string(&e)
                        )));
                    }

                    // 3.b
                    match create_target_then_copy()? {
                        Some(pair) => pair,
                        None => return Ok(CopyResult::Skipped),
                    }
                } else {
                    // The open that failed was for writing, and without -f there is no
                    // second attempt. Same wording as GNU cp.
                    let e = io::Error::last_os_error();
                    let err_str = gettext!(
                        "cannot create regular file '{}': {}",
                        target.display(),
                        error_string(&e)
                    );
                    return Err(io::Error::other(err_str));
                }
            }
        } else {
            // 3.b
            match create_target_then_copy()? {
                Some(pair) => pair,
                None => return Ok(CopyResult::Skipped),
            }
        };

        state.created_files.insert(target.to_path_buf());

        // -p through the descriptor just written, before it is closed; the source's metadata
        // from the fstat of the descriptor that was then read, taken before the read moved its
        // access time (GNU keeps the original access time too).
        let preserve_error = if cfg.preserve {
            // The file is cp's own: created with O_EXCL, or the very file the decision checked.
            preserve_through_fd(
                target_file.as_raw_fd(),
                &source_before_read,
                target,
                MadeTrust::Full,
            )
            .err()
        } else {
            None
        };
        // What was made is the file open on the descriptor written through.
        let made = target_file.metadata().ok().map(|md| (md.dev(), md.ino()));
        Ok(CopyResult::CopiedFile(CopiedFile {
            made,
            source: SourceState::of(&source_before_read),
            preserve_error,
        }))
    }
}

fn hard_link_error(target: &Path, source: &ftw::Entry, e: &io::Error) -> io::Error {
    io::Error::other(gettext!(
        "cannot create hard link '{}' to '{}': {}",
        target.display(),
        source.path(),
        error_string(e)
    ))
}

/// GNU `-l`: make `target` another name for the source, through the directories the walk and
/// the copy hold open, never by path. `follow` says whether the source entry is acted on through
/// its symbolic link (`DerefMode::deref_entry`); otherwise a link is itself given the new name.
///
/// `linkat` resolves the source by name, so the new name must turn out to be the very file the
/// walk examined (`source_md`, which is the referent's metadata when `follow`); anything else,
/// swapped in since, is unlinked again and reported. Nothing is done to the file's attributes:
/// it is the source itself, so -p has nothing to duplicate.
fn link_source(
    source: &ftw::Entry,
    source_md: &ftw::Metadata,
    follow: bool,
    target: &Path,
    target_dirfd: libc::c_int,
    target_filename: *const libc::c_char,
    replace: bool,
) -> io::Result<CopyResult> {
    let flags = if follow { libc::AT_SYMLINK_FOLLOW } else { 0 };
    let link_as = |name: &CStr| {
        let ret = unsafe {
            libc::linkat(
                source.dir_fd(),
                source.file_name().as_ptr(),
                target_dirfd,
                name.as_ptr(),
                flags,
            )
        };
        if ret == 0 {
            Ok(())
        } else {
            Err(io::Error::last_os_error())
        }
    };
    let expected = (source_md.dev(), source_md.ino());
    let is_source = |name: &CStr| {
        ftw::Metadata::new(target_dirfd, name, false)
            .is_ok_and(|md| (md.dev(), md.ino()) == expected)
    };
    let check = |name: &CStr| {
        if is_source(name) {
            return Ok(());
        }
        unsafe { libc::unlinkat(target_dirfd, name.as_ptr(), 0) };
        Err(io::Error::other(gettext!(
            "'{}' changed before it could be linked",
            source.path()
        )))
    };
    let target_name = unsafe { CStr::from_ptr(target_filename) };
    if !replace {
        link_as(target_name).map_err(|e| hard_link_error(target, source, &e))?;
        check(target_name)?;
    } else {
        // An existing destination is replaced as GNU replaces it: the link is made under a fresh
        // name beside it and renamed over it. A link that cannot be made at all (another
        // filesystem, a source the kernel will not link) leaves the destination as it was, and
        // there is no moment without one.
        let temp = link_beside(&link_as).map_err(|e| hard_link_error(target, source, &e))?;
        check(&temp)?;
        let ret =
            unsafe { libc::renameat(target_dirfd, temp.as_ptr(), target_dirfd, target_filename) };
        // A rename onto another name of the same file does nothing, leaving the fresh name.
        if ret != 0 || is_source(&temp) {
            let e = io::Error::last_os_error();
            unsafe { libc::unlinkat(target_dirfd, temp.as_ptr(), 0) };
            if ret != 0 {
                return Err(hard_link_error(target, source, &e));
            }
        }
    }
    Ok(CopyResult::CopiedFile(CopiedFile {
        made: Some(expected),
        source: SourceState::of(source_md),
        preserve_error: None,
    }))
}

/// Make a link with `link_as` under a name not yet taken in the destination directory, and
/// return that name.
fn link_beside(link_as: &impl Fn(&CStr) -> io::Result<()>) -> io::Result<CString> {
    for n in 0..1000 {
        let name = CString::new(format!(".cp-link.{}.{n}", std::process::id())).unwrap();
        match link_as(&name) {
            Ok(()) => return Ok(name),
            Err(e) if e.raw_os_error() == Some(libc::EEXIST) => continue,
            Err(e) => return Err(e),
        }
    }
    Err(io::Error::from_raw_os_error(libc::EEXIST))
}

/// The result for a symbolic link or special file just made by name: its identity, read before
/// anything else is done to it, and -p.
fn copied_node(
    cfg: &CopyConfig,
    source: &ftw::Entry,
    source_md: &ftw::Metadata,
    target_dirfd: libc::c_int,
    target_filename: *const libc::c_char,
    made_type: ftw::FileType,
    target: &Path,
) -> CopyResult {
    let made = made_node_identity(target_dirfd, target_filename, made_type);
    let preserve_error = preserve_made(
        cfg,
        source,
        target_dirfd,
        target_filename,
        made_type,
        target,
    );
    CopyResult::CopiedFile(CopiedFile {
        made,
        // A symbolic link's target was read, and a special file's type and device taken, from
        // what the walk saw.
        source: SourceState::of(source_md),
        preserve_error,
    })
}

/// Whether a descriptor's file type is the one the walk recorded.
fn same_file_type(opened: fs::FileType, walked: ftw::FileType) -> bool {
    use std::os::unix::fs::FileTypeExt;
    match walked {
        ftw::FileType::RegularFile => opened.is_file(),
        ftw::FileType::Directory => opened.is_dir(),
        ftw::FileType::SymbolicLink => opened.is_symlink(),
        ftw::FileType::Fifo => opened.is_fifo(),
        ftw::FileType::CharacterDevice => opened.is_char_device(),
        ftw::FileType::BlockDevice => opened.is_block_device(),
        ftw::FileType::Socket => opened.is_socket(),
        ftw::FileType::Unknown => false,
    }
}

/// Open the source file the walk stat'ed (`walked`), for reading.
///
/// The walk's `lstat` (or `stat`, for a link it follows) came first, so the descriptor must be
/// that same file, or nothing is read from it. When the walk saw a regular file the open also
/// carries `O_NONBLOCK`, so a FIFO swapped in since cannot hold the open waiting for a writer
/// before the identity check refuses it (`openat_guarded`, which also waits out a lease); the
/// flag is then cleared for the copy. A FIFO or device
/// the walk saw (copied as data without -R) is opened as before, blocking.
///
/// Also returns that descriptor's `fstat`, taken before anything is read from it: -p copies the
/// access time it shows, not the one the read is about to set.
fn open_source(
    source: &ftw::Entry,
    walked: &ftw::Metadata,
    flags: libc::c_int,
) -> io::Result<(fs::File, fs::Metadata)> {
    let cannot_open = |e: &io::Error| {
        io::Error::other(gettext!(
            "cannot open '{}' for reading: {}",
            source.path(),
            error_string(e)
        ))
    };
    let regular = walked.file_type() == ftw::FileType::RegularFile;
    let fd = openat_guarded(
        source.dir_fd(),
        source.file_name().as_ptr(),
        flags | libc::O_CLOEXEC,
        regular,
    );
    if fd == -1 {
        return Err(cannot_open(&io::Error::last_os_error()));
    }
    let file = unsafe { fs::File::from_raw_fd(fd) };
    let opened = file.metadata().map_err(|e| cannot_open(&e))?;
    // The type too: a file unlinked and replaced can hand its inode number straight on.
    if opened.dev() != walked.dev()
        || opened.ino() != walked.ino()
        || !same_file_type(opened.file_type(), walked.file_type())
    {
        return Err(io::Error::other(gettext!(
            "will not read '{}': it changed after it was checked",
            source.path()
        )));
    }
    if regular {
        clear_nonblock(fd).map_err(|e| cannot_open(&e))?;
    }
    Ok((file, opened))
}

/// `openat(dirfd, name, flags)`, with `O_NONBLOCK` added when `guard` is set so that a FIFO
/// swapped in for the expected regular file cannot hold the open.
///
/// A regular file under a lease (a Samba oplock, a knfsd delegation) fails a non-blocking open
/// with EAGAIN/EWOULDBLOCK instead of waiting for the lease to break, so that open is retried
/// blocking (`reopen_regular_blocking`) -- without letting the retry reach a FIFO swapped in
/// after the first attempt. The caller's identity and type check follows either open.
/// Returns -1 with `errno` set on failure.
fn openat_guarded(
    dirfd: libc::c_int,
    name: *const libc::c_char,
    flags: libc::c_int,
    guard: bool,
) -> libc::c_int {
    if !guard {
        return unsafe { libc::openat(dirfd, name, flags) };
    }
    let fd = unsafe { libc::openat(dirfd, name, flags | libc::O_NONBLOCK) };
    let errno = io::Error::last_os_error().raw_os_error();
    if fd != -1 || !matches!(errno, Some(e) if e == libc::EAGAIN || e == libc::EWOULDBLOCK) {
        return fd;
    }
    reopen_regular_blocking(dirfd, name, flags)
}

/// The blocking retry of `openat_guarded`, which may wait for a lease to break and so must
/// only ever open a regular file.
///
/// On Linux the name is pinned with `O_PATH` (plus the caller's `O_NOFOLLOW`), the pinned
/// inode must be a regular file, and the blocking open is of that inode itself, through
/// `self/fd/N` in a `/proc` verified to be procfs: no name is resolved again, so nothing
/// swapped in after the pin is reached, and a regular file's open cannot block on a missing
/// FIFO peer. Without procfs (and on other systems) the name is `fstatat`'ed -- following a
/// symbolic link exactly when the caller's flags do -- and must be a regular file just before
/// the blocking open; the residual is a FIFO swapped in between those two calls. A non-regular
/// file fails with ENXIO, the error
/// a non-blocking open of a FIFO would have given.
fn reopen_regular_blocking(
    dirfd: libc::c_int,
    name: *const libc::c_char,
    flags: libc::c_int,
) -> libc::c_int {
    let not_regular = || {
        errno::set_errno(errno::Errno(libc::ENXIO));
        -1
    };
    #[cfg(target_os = "linux")]
    if let Ok(proc_dir) = procfs_dir() {
        let pin = unsafe {
            libc::openat(
                dirfd,
                name,
                libc::O_PATH | libc::O_CLOEXEC | (flags & libc::O_NOFOLLOW),
            )
        };
        if pin == -1 {
            return -1;
        }
        let pin = unsafe { fs::File::from_raw_fd(pin) };
        match pin.metadata() {
            Ok(md) if md.is_file() => {}
            Ok(_) => return not_regular(),
            Err(_) => return -1,
        }
        // The magic link is followed to the pinned inode: `O_NOFOLLOW` would refuse it.
        let pinned = proc_fd_name(pin.as_raw_fd());
        return unsafe {
            libc::openat(
                proc_dir.as_raw_fd(),
                pinned.as_ptr(),
                flags & !libc::O_NOFOLLOW,
            )
        };
    }
    // The check resolves the name as the open below will: it follows a symbolic link exactly
    // when the caller's flags do (-L, or the operand link POSIX writes through), so a copy
    // through a link to a leased file waits for the lease like any other. Residual, accepted:
    // without procfs (and off Linux) nothing pins the inode between this check and the
    // blocking open, so a FIFO swapped in for the name (or for the link's target) in that
    // window is opened and can block it.
    let name_cstr = unsafe { CStr::from_ptr(name) };
    let follow = flags & libc::O_NOFOLLOW == 0;
    match ftw::Metadata::new(dirfd, name_cstr, follow) {
        Ok(md) if md.file_type() == ftw::FileType::RegularFile => {}
        Ok(_) => return not_regular(),
        Err(_) => return -1,
    }
    unsafe { libc::openat(dirfd, name, flags) }
}

/// Clear `O_NONBLOCK` on a descriptor opened with it only to keep a swapped-in FIFO from
/// blocking the open.
fn clear_nonblock(fd: libc::c_int) -> io::Result<()> {
    let status = unsafe { libc::fcntl(fd, libc::F_GETFL) };
    if status == -1 || unsafe { libc::fcntl(fd, libc::F_SETFL, status & !libc::O_NONBLOCK) } == -1 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

/// -p for a symbolic link or special file this copy just made (no descriptor to act through).
fn preserve_made(
    cfg: &CopyConfig,
    source: &ftw::Entry,
    target_dirfd: libc::c_int,
    target_filename: *const libc::c_char,
    made_type: ftw::FileType,
    target: &Path,
) -> Option<io::Error> {
    if !cfg.preserve {
        return None;
    }
    let name = unsafe { CStr::from_ptr(target_filename) };
    fresh_source_md(source)
        .and_then(|source_md| preserve_made_node(target_dirfd, name, made_type, &source_md, target))
        .err()
}

/// Copy `source_arg` to `target_arg`, resolved from the working directory. `trust` is what the
/// directory the operand is made in hands it should it be found existing (`OperandTrust`);
/// `run` carries what the run has done so far (`CopyRun`).
pub fn copy_file<F>(
    cfg: &CopyConfig,
    source_arg: &Path,
    target_arg: &Path,
    trust: OperandTrust,
    run: &mut CopyRun,
    inode_map: Option<&mut InodeMap>,
    prompt_fn: F,
) -> io::Result<()>
where
    F: Copy + Fn(&str) -> bool,
{
    copy_tree(
        cfg,
        SourceRoot::Path(source_arg),
        TargetRoot::in_cwd(target_arg, trust),
        &mut run.created_files,
        &mut run.made_dirs,
        inode_map,
        prompt_fn,
    )
}

/// `copy_file`, with the destination's directory given as a descriptor, with the trust it
/// hands the directories found in it, carried down from the anchor: the copy is made as
/// `target_arg`'s last component inside that directory and `target_arg` is only named in
/// diagnostics, so no part of it is resolved by path.
pub fn copy_file_at<F>(
    cfg: &CopyConfig,
    source_arg: &Path,
    target_arg: &Path,
    target_dir: (ftw::FileDescriptor, ChainTrust),
    run: &mut CopyRun,
    inode_map: Option<&mut InodeMap>,
    prompt_fn: F,
) -> io::Result<()>
where
    F: Copy + Fn(&str) -> bool,
{
    let (dir, trust) = target_dir;
    copy_tree(
        cfg,
        SourceRoot::Path(source_arg),
        TargetRoot::in_dir(target_arg, dir, trust)?,
        &mut run.created_files,
        &mut run.made_dirs,
        inode_map,
        prompt_fn,
    )
}

/// Where a copy's destination operand is made.
struct TargetRoot<'a> {
    /// The directory it is made in.
    dir: Rc<ftw::FileDescriptor>,
    /// What `dir` hands it, should it be found existing.
    trust: OperandTrust,
    /// Its name in `dir`.
    name: &'a OsStr,
    /// What precedes `name` in the operand, for diagnostics.
    display_parent: PathBuf,
    /// The operand as given, for the diagnostics that name the operands.
    operand: &'a Path,
}

impl<'a> TargetRoot<'a> {
    /// cp: the operand resolved from the working directory.
    fn in_cwd(operand: &'a Path, trust: OperandTrust) -> Self {
        TargetRoot {
            dir: Rc::new(ftw::FileDescriptor::cwd()),
            trust,
            name: operand.as_os_str(),
            display_parent: PathBuf::new(),
            operand,
        }
    }

    /// cp --parents: the operand's last component in `dir`. An operand with none (`..`, `/`)
    /// is refused: it names no entry of `dir`, and resolving it some other way would leave
    /// both the directory and the trust it hands behind.
    fn in_dir(operand: &'a Path, dir: ftw::FileDescriptor, trust: ChainTrust) -> io::Result<Self> {
        let name = operand.file_name().ok_or_else(|| {
            io::Error::other(gettext!(
                "'{}' names no entry of a directory",
                operand.display()
            ))
        })?;
        Ok(TargetRoot {
            dir: Rc::new(dir),
            trust: OperandTrust::Chain(trust),
            name,
            display_parent: operand.parent().unwrap_or(Path::new("")).to_path_buf(),
            operand,
        })
    }

    /// mv: the destination operand pinned in the directory it was found in.
    fn pinned(entry: &'a PinnedEntry) -> Self {
        TargetRoot {
            dir: entry.dir_rc(),
            // The move's copy must create its operand (`Destination::MustCreate`), so it is
            // never found; the directory it was pinned in is the anchor.
            trust: OperandTrust::Parent,
            name: OsStr::from_bytes(entry.name().to_bytes()),
            display_parent: entry.display_parent().to_path_buf(),
            operand: entry.path(),
        }
    }
}

/// The source of a `mv` across filesystems: the operand, pinned, and the file it must be.
pub struct MoveSource<'a> {
    pub entry: &'a PinnedEntry,
    /// `(st_dev, st_ino)` of the operand itself (not followed) when the move examined it: the
    /// copy refuses to start from any other file.
    pub identity: (u64, u64),
    /// Filled in with every source file the copy duplicated, which is what the move then
    /// removes.
    pub copied: &'a mut CopiedSources,
}

/// The duplication step of `mv` across filesystems (POSIX mv step 6): `copy_file_at`, walking
/// the source from the directory it was pinned in, recording what it copied, and making the
/// destination in the directory it was pinned in.
///
/// Every directory it enters it made itself, so the directories one move made need not be
/// known to the next (`MadeDirs`).
pub fn copy_moved_file(
    cfg: &CopyConfig,
    source: MoveSource<'_>,
    target: &PinnedEntry,
    created_files: &mut HashSet<PathBuf>,
    inode_map: &mut InodeMap,
) -> io::Result<()> {
    copy_tree(
        cfg,
        SourceRoot::Pinned(source),
        TargetRoot::pinned(target),
        created_files,
        &mut MadeDirs::default(),
        Some(inode_map),
        // mv never asks: it already asked its own question.
        |_| false,
    )
}

/// Where a copy's walk starts.
enum SourceRoot<'a> {
    /// cp: the operand, resolved as written.
    Path(&'a Path),
    /// mv: see `MoveSource`.
    Pinned(MoveSource<'a>),
}

/// The error for a moved operand that is no longer the file the move examined.
fn changed_before_copy(source: &ftw::Entry) -> io::Error {
    io::Error::other(gettext!(
        "'{}' changed before it could be copied",
        source.path()
    ))
}

fn copy_tree<F>(
    cfg: &CopyConfig,
    source_root: SourceRoot<'_>,
    target_root: TargetRoot<'_>,
    created_files: &mut HashSet<PathBuf>,
    made_by_run: &mut MadeDirs,
    mut inode_map: Option<&mut InodeMap>,
    prompt_fn: F,
) -> io::Result<()>
where
    F: Copy + Fn(&str) -> bool,
{
    let (source_arg, pinned_source, expected_root, mut copied) = match source_root {
        SourceRoot::Path(path) => (path, None, None, None),
        SourceRoot::Pinned(MoveSource {
            entry,
            identity,
            copied,
        }) => (entry.path(), Some(entry), Some(identity), Some(copied)),
    };
    // The operand's own directory and name.
    let TargetRoot {
        dir: top_dir,
        trust: top_trust,
        name: top_name,
        display_parent: top_dir_path,
        operand: target_arg,
    } = target_root;
    // Without -p a directory found is given nothing wherever it is, so the trust is never
    // asked for, and `OperandTrust::Parent` need not read the operand's `..`.
    let top_trust = if cfg.preserve {
        top_trust
    } else {
        OperandTrust::Named
    };
    // What -p asks of a directory: cp's preserves mode and owner together.
    let requested = Preserve {
        mode: cfg.preserve,
        owner: cfg.preserve,
    };
    // `RefCell` to allow sharing these between closures. The bottom entry is the operand's
    // directory, so a stack of one means the entry is the operand itself.
    let target_dirfd_stack = RefCell::new(vec![DestDir {
        fd: top_dir,
        hands: top_trust,
        finish: None,
        own: false,
    }]);
    // (st_dev, st_ino) of every destination directory this copy creates or enters. A source
    // directory found in here is one we are copying *into*.
    let dest_dir_ids = RefCell::new(HashSet::<(u64, u64)>::new());
    let made_by_run = RefCell::new(made_by_run);
    // Read once: each read is a pair of umask(2) calls.
    let umask = plib::modestr::umask();
    let target_dir_path = RefCell::new(top_dir_path);
    let terminate = RefCell::new(false);
    let last_error = RefCell::new(None);
    // In `continue_on_error` (cp) mode each diagnostic is emitted immediately and this flag records
    // that the final exit status must be non-zero (without a returned message to re-print).
    let had_error = RefCell::new(false);

    let file_handler = |source: ftw::Entry<'_>| -> Result<bool, ()> {
        let mut terminate_borrowed = terminate.borrow_mut();
        let mut target_dirfd_stack_borrowed = target_dirfd_stack.borrow_mut();
        let mut target_dir_path_borrowed = target_dir_path.borrow_mut();

        if *terminate_borrowed {
            return Ok(false);
        }

        let at_top_level = target_dirfd_stack_borrowed.len() == 1;
        let target_level = target_dirfd_stack_borrowed.last().unwrap();
        let target_dirfd = &target_level.fd;
        let hands = &target_level.hands;

        let target_filename = if at_top_level {
            top_name
        } else {
            OsStr::from_bytes(source.file_name().to_bytes())
        };

        let target = target_dir_path_borrowed.join(target_filename);
        let target_filename_cstr = CString::new(target_filename.as_bytes()).unwrap();

        let source_md = source.metadata().unwrap();
        let identifier = (source_md.dev(), source_md.ino());

        // A moved operand must be the file the move examined; nothing else is copied (and
        // so nothing else will be removed).
        if at_top_level && expected_root.is_some_and(|expected| expected != identifier) {
            *last_error.borrow_mut() = Some(changed_before_copy(&source));
            *terminate_borrowed = true;
            return Ok(false);
        }

        // Hard-link preserving behavior of `mv`. `cp` does not maintain the hard-link structure
        // of the hierarchy according to the standard
        if let Some(inode_map) = inode_map.as_deref_mut() {
            // Preserve hard links like coreutils mv. Creating a copy is also
            // allowed by the standard.
            if let Some(first) = inode_map.get(&identifier) {
                let source_state = SourceState::of(source_md);
                match link_to_first_copy(
                    first,
                    &source_state,
                    target_dirfd.as_raw_fd(),
                    &target_filename_cstr,
                ) {
                    Ok(Linked::ToFirstCopy) => {
                        // GNU cp lists the link it makes; GNU mv does not.
                        if cfg.verbose == Some(Verbose::Copy) {
                            report_copied(cfg, source.path().as_inner(), &target, false);
                        }
                        // Skip since this file/directory is handled by hard-linking
                        if let Some(copied) = copied.as_deref_mut() {
                            copied.record(source_state);
                        }
                        return Ok(false);
                    }
                    // Copied afresh below. The first copy is no longer one to link to; the
                    // fresh copy takes its place if the file still has other names.
                    Ok(Linked::SourceChanged) | Ok(Linked::NotTheFirstCopy) => {
                        inode_map.remove(&identifier);
                    }
                    Err(e) => {
                        // Under -n an existing destination is kept, linked or not.
                        if cfg.no_clobber && e.raw_os_error() == Some(libc::EEXIST) {
                            return Ok(false);
                        }
                        if cfg.continue_on_error {
                            eprintln!("{}: {}", cfg.prog, error_string(&e));
                            *had_error.borrow_mut() = true;
                        } else {
                            *last_error.borrow_mut() = Some(e);
                            *terminate_borrowed = true;
                        }
                        return Ok(false);
                    }
                }
            }
        }

        let continue_processing = match copy_file_impl(
            cfg,
            &source,
            &target,
            target_dirfd.as_raw_fd(),
            target_filename_cstr.as_ptr(),
            &mut CopyState {
                created_files,
                dest_dir_ids: &dest_dir_ids,
                operands: (source_arg, target_arg),
                at_top_level,
            },
            prompt_fn,
        ) {
            Ok(copy_result) => {
                // A directory this copy made is reported once it is opened and checked to be
                // the one made, below; one copied into was not made by this copy.
                if let CopyResult::CopiedFile(_) = &copy_result {
                    report_copied(cfg, source.path().as_inner(), &target, false);
                }
                // Record where this inode landed only if a file was actually created there.
                // Recording a skipped copy pointed a later hard link at a target that does
                // not exist, and every directory reports nlink > 1, so directories were
                // recorded too.
                if let CopyResult::CopiedFile(CopiedFile { made, source, .. }) = &copy_result {
                    if let Some(copied) = copied.as_deref_mut() {
                        copied.record(*source);
                    }
                    // Only files that have hard links are worth tracking, and only a copy whose
                    // identity is known: a later link is checked against it.
                    if let (Some(inode_map), Some(made), true) =
                        (inode_map.as_deref_mut(), made, source_md.nlink() > 1)
                    {
                        inode_map.insert(
                            identifier,
                            FirstCopy {
                                dir: Rc::clone(target_dirfd),
                                name: target_filename_cstr.clone(),
                                made: *made,
                                source: *source,
                            },
                        );
                    }
                }

                match copy_result {
                    CopyResult::CopyingDirectory(origin) => {
                        // mkdir/mkdirat doesn't return a file descriptor so a new one must be
                        // opened here. Using O_CREAT | O_DIRECTORY in a call to open/openat would
                        // not allow atomically creating a directory then opening it:
                        //
                        // https://stackoverflow.com/questions/45818628/whats-the-expected-behavior-of-openname-o-creato-directory-mode/48693137#48693137
                        //
                        // `copy_file_impl` accepted the destination as a directory from its
                        // `lstat` (or made it), so it is never a symbolic link to follow:
                        // `O_NOFOLLOW` refuses one swapped in since, which would otherwise
                        // redirect everything copied below it. (A trailing slash on the
                        // operand still resolves, for the open as for the `lstat`.)
                        //
                        // The descriptor must then be the directory decided on: the one the
                        // `lstat` saw, or for one this copy made, what a fresh `mkdirat`
                        // yields (`verify_made_dir`). Its identity is read from the
                        // descriptor, never by name.
                        //
                        // From the same descriptor comes what it is given once filled, and the
                        // trust it hands the directories found in it: one this copy made and
                        // verified in full is trusted afresh (`ChainTrust::made`); any other is
                        // judged by what its parent hands it (`found_dir_trust`).
                        let cannot_open = |e: io::Error| {
                            io::Error::other(gettext!(
                                "cannot open directory '{}': {}",
                                target.display(),
                                error_string(&e)
                            ))
                        };
                        let opened = match origin {
                            DirOrigin::Found { dev, ino } => ftw::FileDescriptor::open_at(
                                target_dirfd,
                                &target_filename_cstr,
                                libc::O_RDONLY
                                    | libc::O_DIRECTORY
                                    | libc::O_NOFOLLOW
                                    | libc::O_CLOEXEC,
                            )
                            .map_err(cannot_open)
                            .and_then(|fd| {
                                let fd = Rc::new(fd);
                                let md = fd_metadata(fd.as_raw_fd())?;
                                if dev != md.dev() || ino != md.ino() {
                                    return Err(io::Error::other(gettext!(
                                        "'{}' was replaced after it was checked",
                                        target.display()
                                    )));
                                }
                                let own = made_by_run.borrow().made_at(&md, &target);
                                let (finish, trust) = if own {
                                    own_dir_trust(&fd, requested)?
                                } else {
                                    found_dir_trust(
                                        hands,
                                        target_dirfd,
                                        &fd,
                                        (dev, ino),
                                        &target,
                                        requested,
                                    )?
                                };
                                Ok((fd, md, finish, trust, own))
                            }),
                            DirOrigin::Made => open_made_dir(
                                target_dirfd.as_raw_fd(),
                                &target_filename_cstr,
                                &target,
                            )
                            .map_err(|e| made_dir_open_error(&target, e))
                            .and_then(|(fd, made)| {
                                let fd = Rc::new(ftw::FileDescriptor::from(fd));
                                let md = fd_metadata(fd.as_raw_fd())?;
                                let own = made == MadeTrust::Full;
                                let trust = if own {
                                    made_by_run.borrow_mut().record(&md, &target);
                                    ChainTrust::made(&fd)?
                                } else {
                                    // Owned like its parent only, it may be someone else's: it
                                    // hands on what a directory found would.
                                    let (_, trust) = found_dir_trust(
                                        hands,
                                        target_dirfd,
                                        &fd,
                                        (md.dev(), md.ino()),
                                        &target,
                                        requested,
                                    )?;
                                    trust
                                };
                                report_copied(cfg, source.path().as_inner(), &target, true);
                                Ok((fd, md, DirFinish::Made(made), trust, own))
                            }),
                        };
                        let (new_target_dirfd, new_target_md, finish, trust, own) = match opened {
                            Ok(pair) => pair,
                            Err(e) => {
                                if cfg.continue_on_error {
                                    eprintln!("{}: {}", cfg.prog, error_string(&e));
                                    *had_error.borrow_mut() = true;
                                } else {
                                    *last_error.borrow_mut() = Some(e);
                                    *terminate_borrowed = true;
                                }
                                return Ok(false);
                            }
                        };

                        // Record what this destination directory *is*, from the descriptor
                        // already in hand rather than by name. Recording on entry, not on
                        // creation, so a destination that existed beforehand counts too.
                        dest_dir_ids
                            .borrow_mut()
                            .insert((new_target_md.dev(), new_target_md.ino()));
                        if let Some(copied) = copied.as_deref_mut() {
                            copied.record(SourceState::of(source_md));
                        }

                        target_dirfd_stack_borrowed.push(DestDir {
                            fd: new_target_dirfd,
                            hands: OperandTrust::Chain(trust),
                            finish: Some(finish),
                            own,
                        });
                        target_dir_path_borrowed.push(target_filename);

                        true
                    }
                    CopyResult::CopiedFile(CopiedFile { preserve_error, .. }) => {
                        // `copy_file_impl` already applied -p to the file; directories are
                        // handled in the `postprocess_dir` closure below.
                        if let Some(e) = preserve_error {
                            // A characteristics-duplication failure is never fatal: cp
                            // reports it and sets a non-zero exit status; mv reports it but
                            // must NOT modify its exit status (108114-108115) and still
                            // completes the move.
                            eprintln!("{}: {}", cfg.prog, error_string(&e));
                            if cfg.continue_on_error {
                                *had_error.borrow_mut() = true;
                            }
                        }
                        true
                    }
                    CopyResult::Skipped => false,
                }
            }
            Err(e) => {
                if cfg.continue_on_error {
                    eprintln!("{}: {}", cfg.prog, error_string(&e));
                    *had_error.borrow_mut() = true;
                } else {
                    *last_error.borrow_mut() = Some(e);
                    *terminate_borrowed = true;
                }
                false
            }
        };

        Ok(continue_processing)
    };
    // Pops unconditionally. `ftw` calls this for every directory whose handler returned
    // `true`, including ones it then could not descend into; leaving the push in place there
    // would silently redirect every later file into the wrong destination directory.
    // The target directory exists either way, so `finish_dir` still applies to it.
    let postprocess_dir = |source: ftw::Entry<'_>, _exit| -> Result<(), ()> {
        let mut target_dirfd_stack_borrowed = target_dirfd_stack.borrow_mut();
        let mut target_dir_path_borrowed = target_dir_path.borrow_mut();

        let dir_path = target_dir_path_borrowed.clone();
        target_dir_path_borrowed.pop();
        let target_dir = target_dirfd_stack_borrowed.pop();

        if let Some(DestDir {
            fd,
            finish: Some(finish),
            own,
            ..
        }) = target_dir
        {
            #[cfg(test)]
            if own {
                BEFORE_FINISHING_OWN.with(|hook| hook.get().map(|hook| hook(fd.as_raw_fd())));
            }
            let finished = finish_dir(
                fd.as_raw_fd(),
                &source,
                finish,
                cfg.preserve,
                umask,
                &dir_path,
            );
            if let Err(e) = finished {
                // Same policy as the file case: never fatal, exit-status only for cp.
                eprintln!("{}: {}", cfg.prog, error_string(&e));
                if cfg.continue_on_error {
                    *had_error.borrow_mut() = true;
                }
            }
            // Filled and finished through the descriptor held since it was found to be the
            // run's own, it still is: what that changed is noted, so a later operand finding
            // it unchanged since knows it again.
            if own {
                if let Ok(md) = fd_metadata(fd.as_raw_fd()) {
                    made_by_run.borrow_mut().refresh(&md, &dir_path);
                }
            }
        }

        Ok(())
    };
    let err_reporter = |entry: ftw::Entry<'_>, error: ftw::Error| {
        // `ftw::Error` carries no filename; the entry it failed on does.
        let err_str = gettext!(
            "cannot access '{}': {}",
            entry.path(),
            error_string(&error.inner())
        );
        if cfg.continue_on_error {
            eprintln!("{}: {}", cfg.prog, err_str);
            *had_error.borrow_mut() = true;
        } else {
            *last_error.borrow_mut() = Some(io::Error::other(err_str));
            *terminate.borrow_mut() = true;
        }
    };
    let opts = ftw::TraverseDirectoryOpts {
        follow_symlinks_on_args: cfg.deref.follow_symlinks_on_args(),
        follow_symlinks: cfg.deref.follow_symlinks(),
        // One target-directory descriptor is held per level in `target_dirfd_stack`, so the
        // traversal must count those too when deciding to conserve descriptors.
        caller_fds_per_level: 1,
        ..Default::default()
    };
    let _ = match pinned_source {
        Some(entry) => ftw::traverse_directory_at(
            entry.dir(),
            entry.name(),
            entry.display_parent(),
            file_handler,
            postprocess_dir,
            err_reporter,
            opts,
        ),
        None => traverse_directory(
            source_arg,
            file_handler,
            postprocess_dir,
            err_reporter,
            opts,
        ),
    };

    match last_error.into_inner() {
        Some(e) => Err(e),
        // In `continue_on_error` mode every diagnostic was already written to stderr; signal
        // failure with an empty-message error so the caller sets a non-zero status without
        // re-printing.
        None if had_error.into_inner() => Err(io::Error::other(String::new())),
        None => Ok(()),
    }
}

pub fn copy_files<F>(
    cfg: &CopyConfig,
    sources: &[PathBuf],
    target: &Path,
    mut inode_map: Option<&mut InodeMap>,
    prompt_fn: F,
) -> Option<()>
where
    F: Copy + Fn(&str) -> bool,
{
    let mut result = Some(());

    let mut run = CopyRun::default();

    // loop through sources, moving each to target
    for source in sources {
        // This doesn't seem to be compliant with POSIX
        let ends_with_slash_dot = |p: &Path| -> bool {
            let bytes = p.as_os_str().as_bytes();
            if bytes.len() >= 2 {
                let end = &bytes[(bytes.len() - 2)..];
                return end == b"/.";
            }
            false
        };

        // `target` is the destination directory the user named, and the anchor: the operand is
        // either an entry of it or, for `src/.`, the directory itself.
        let (new_target, trust) = if source.is_dir() && ends_with_slash_dot(source) {
            // This causes the contents of `source` to be copied instead of
            // `source` itself
            (target.to_path_buf(), OperandTrust::Named)
        } else {
            match source.file_name() {
                Some(file_name) => (target.join(file_name), OperandTrust::Parent),
                None => {
                    let err_str = gettext!("invalid filename: {}", source.display());
                    eprintln!("{}: {}", cfg.prog, err_str);
                    result = None;
                    continue;
                }
            }
        };

        match copy_file(
            cfg,
            source,
            &new_target,
            trust,
            &mut run,
            inode_map.as_deref_mut(),
            prompt_fn,
        ) {
            Ok(_) => (),
            Err(e) => {
                // `copy_file` emits its own per-file diagnostics in continue-on-error mode and then
                // returns an empty-message marker; only print here if a message is actually carried.
                let s = error_string(&e);
                if !s.is_empty() {
                    eprintln!("{}: {}", cfg.prog, s);
                }
                result = None;
            }
        }
    }

    result
}

/// POSIX cp step 4: reproduce a FIFO, device or socket at the destination.
///
/// The caller has already applied steps 1 to 3.a, so an existing destination has been prompted
/// for and is known not to be a directory.
fn copy_special_file(
    source_md: &ftw::Metadata,
    source_file_type: ftw::FileType,

    // Should only be used for keeping track of created files and for displaying error messages
    target: &Path,

    target_dirfd: libc::c_int,
    target_filename: *const libc::c_char,
    target_exists: bool,
    created_files: &mut HashSet<PathBuf>,
) -> io::Result<()> {
    let is_fifo = source_file_type == ftw::FileType::Fifo;

    // 4.b. A FIFO takes the source's permission bits (POSIX 90683-90685), all twelve of them:
    // unlike a regular file a set-user-ID FIFO is inert, and GNU reproduces the bit too. For the
    // other special types the permissions are implementation-defined, so keep only the ordinary
    // nine. `mknodat` applies the umask, and `-p` sets the exact bits afterwards through
    // `preserve_made`, which checks and pins the node first (`preserve_made_node`) and clears
    // set-user-ID and set-group-ID if the owner could not be duplicated.
    let perm = source_md.mode() & if is_fifo { 0o7777 } else { 0o777 };

    // 4.a: "The dest_file shall be created with the same file type as source_file." Passing no
    // type bits to `mknod` asks for a *regular file*, which is how copying a character device
    // used to report success having written an empty regular file.
    #[allow(clippy::unnecessary_cast)]
    let mode = (source_md.mode() & libc::S_IFMT as u32) | perm;

    if target_exists {
        // Never `AT_REMOVEDIR`: removing a directory to plant a device node is destruction POSIX
        // never asks for. The caller rejects a directory destination before getting here.
        let ret = unsafe { libc::unlinkat(target_dirfd, target_filename, 0) };
        if ret != 0 {
            let e = io::Error::last_os_error();
            return Err(io::Error::other(gettext!(
                "cannot remove '{}': {}",
                target.display(),
                error_string(&e)
            )));
        }
    }

    let ret = unsafe {
        libc::mknodat(
            target_dirfd,
            target_filename,
            mode as libc::mode_t,
            source_md.rdev() as libc::dev_t,
        )
    };
    if ret == 0 {
        created_files.insert(target.to_path_buf());
        Ok(())
    } else {
        let e = io::Error::last_os_error();
        let err_str = if is_fifo {
            gettext!(
                "cannot create fifo '{}': {}",
                target.display(),
                error_string(&e)
            )
        } else {
            gettext!(
                "cannot create special file '{}': {}",
                target.display(),
                error_string(&e)
            )
        };
        // The kind is kept so that -n can tell a destination that appeared meanwhile.
        Err(io::Error::new(e.kind(), err_str))
    }
}

#[cfg(test)]
mod tests {
    // made_by_us and the by-name link times are tested with them, in plib::madefs.
    use super::MadeTrust;

    /// A directory cp made that cannot be opened is reported against its path: as one that is
    /// no longer a directory when something else was swapped in for it, and otherwise as cp
    /// reports any directory it cannot open.
    #[test]
    fn a_made_directory_that_cannot_be_opened_is_named() {
        use super::made_dir_open_error;
        use std::io;
        use std::path::Path;

        let path = Path::new("t/dir");
        let swapped = made_dir_open_error(path, io::Error::from_raw_os_error(libc::ELOOP));
        assert!(
            swapped
                .to_string()
                .starts_with("'t/dir' exists but is not a directory: "),
            "{swapped}"
        );
        let other = made_dir_open_error(path, io::Error::from_raw_os_error(libc::EACCES));
        assert!(
            other
                .to_string()
                .starts_with("cannot open directory 't/dir': "),
            "{other}"
        );
        // A diagnostic that already names the path is kept as it is.
        let named = io::Error::other("'t/dir' was replaced after it was made");
        let kept = made_dir_open_error(path, named);
        assert_eq!(kept.to_string(), "'t/dir' was replaced after it was made");
    }

    /// -p on an object trusted only as owned like its parent: the times are applied, the mode
    /// (and owner) are not, and that is reported.
    #[test]
    fn withholds_owner_and_mode_from_a_parent_owner_only_object() {
        use super::preserve_through_fd;
        use std::fs;
        use std::os::fd::AsRawFd;
        use std::os::unix::fs::{MetadataExt, PermissionsExt};

        let tmp = plib::tmp::tempdir().unwrap();
        let dir = tmp.path();
        let source = dir.join("source");
        let target = dir.join("target");
        fs::write(&source, b"s").unwrap();
        fs::set_permissions(&source, fs::Permissions::from_mode(0o755)).unwrap();
        let old = std::time::UNIX_EPOCH + std::time::Duration::from_secs(978_307_200);
        fs::File::options()
            .write(true)
            .open(&source)
            .unwrap()
            .set_modified(old)
            .unwrap();
        fs::write(&target, b"t").unwrap();
        fs::set_permissions(&target, fs::Permissions::from_mode(0o600)).unwrap();

        let source_md = fs::metadata(&source).unwrap();
        let target_file = fs::File::open(&target).unwrap();
        let result = preserve_through_fd(
            target_file.as_raw_fd(),
            &source_md,
            &target,
            MadeTrust::ParentOwnerOnly,
        );
        let after = fs::metadata(&target).unwrap();

        assert!(
            result.is_err(),
            "withholding owner and mode must be reported"
        );
        assert_eq!(after.mode() & 0o7777, 0o600, "the mode was applied");
        assert_eq!(after.mtime(), 978_307_200, "the times were not applied");
    }

    /// Move the status-change time of the directory open on `fd` into a later clock tick, as
    /// a copy taking longer than a tick does: `fchmod` to the mode it has, until the time moves.
    fn tick_ctime(fd: libc::c_int) {
        let ctime = |fd| {
            let mut st: libc::stat = unsafe { std::mem::zeroed() };
            assert_eq!(unsafe { libc::fstat(fd, &mut st) }, 0);
            (st.st_ctime, st.st_ctime_nsec, st.st_mode)
        };
        let (sec, nsec, mode) = ctime(fd);
        loop {
            std::thread::sleep(std::time::Duration::from_millis(2));
            assert_eq!(unsafe { libc::fchmod(fd, mode & 0o7777) }, 0);
            let (now_sec, now_nsec, _) = ctime(fd);
            if (now_sec, now_nsec) != (sec, nsec) {
                return;
            }
        }
    }

    /// Two operands merging into one directory of a destination others can write: the first
    /// makes it, the second finds it the run's own and stamps it. Between the two, cp itself
    /// changed it -- filled and finished it -- past a tick of the clock that stamps its ctime;
    /// cp notes what it changed (`MadeDirs::refresh`), or the second operand would take it for
    /// someone else's.
    #[test]
    fn a_made_directory_changed_by_cp_itself_is_still_the_runs_own() {
        use std::fs;
        use std::os::unix::fs::{MetadataExt, PermissionsExt};

        let tmp = plib::tmp::tempdir().unwrap();
        let dir = tmp.path();
        for (source, mode, file) in [("s1/x", 0o700, "f"), ("s2/x", 0o750, "g")] {
            fs::create_dir_all(dir.join(source)).unwrap();
            fs::write(dir.join(source).join(file), b"data").unwrap();
            fs::set_permissions(dir.join(source), fs::Permissions::from_mode(mode)).unwrap();
        }
        let dest = dir.join("dest");
        fs::create_dir(&dest).unwrap();
        fs::set_permissions(&dest, fs::Permissions::from_mode(0o777)).unwrap();

        super::BEFORE_FINISHING_OWN.with(|hook| hook.set(Some(tick_ctime)));
        let mut run = super::CopyRun::default();
        for source in ["s1/x", "s2/x"] {
            let copied = super::copy_file(
                &cp_pr_config(),
                &dir.join(source),
                &dest.join("x"),
                super::OperandTrust::Parent,
                &mut run,
                None,
                |_| false,
            );
            assert!(copied.is_ok(), "{source}: {copied:?}");
        }
        super::BEFORE_FINISHING_OWN.with(|hook| hook.set(None));
        fs::set_permissions(&dest, fs::Permissions::from_mode(0o755)).unwrap();
        let x = fs::metadata(dest.join("x")).unwrap();
        assert_eq!(x.mode() & 0o7777, 0o750);
        assert!(dest.join("x/f").exists() && dest.join("x/g").exists());
    }

    /// The anchor of a found operand is the directory its name is in only while that name is
    /// the very directory found: another directory under the name -- the found one renamed
    /// away and another put there -- leaves the operand in no directory it can judge.
    #[test]
    fn parent_anchor_requires_the_name_to_be_the_directory_found() {
        use std::fs;
        use std::os::unix::fs::MetadataExt;
        use std::rc::Rc;

        let tmp = plib::tmp::tempdir().unwrap();
        let dir = tmp.path();
        fs::create_dir(dir.join("x")).unwrap();
        let parent_c = std::ffi::CString::new(dir.as_os_str().as_encoded_bytes()).unwrap();
        let parent = Rc::new(
            ftw::FileDescriptor::open_at(
                &ftw::FileDescriptor::cwd(),
                &parent_c,
                libc::O_RDONLY | libc::O_DIRECTORY,
            )
            .unwrap(),
        );
        let target = std::path::Path::new("x");
        let mode = plib::madefs::Preserve {
            mode: true,
            owner: true,
        };
        let x = fs::metadata(dir.join("x")).unwrap();
        let (trust, _) = super::parent_anchor(&parent, (x.dev(), x.ino()), target).unwrap();
        assert_eq!(trust.found_dir(mode), super::FoundDir::AsRequested);

        let other = fs::metadata(dir).unwrap();
        let (trust, _) = super::parent_anchor(&parent, (other.dev(), other.ino()), target).unwrap();
        assert_eq!(trust.found_dir(mode), super::FoundDir::LeaveAlone);
    }

    /// What `fstat` might report of a directory: only what `MadeDirs` reads is set.
    #[derive(Clone, Copy)]
    struct Status {
        ino: u64,
        uid: u32,
        gid: u32,
        mode: u32,
        ctime: i64,
    }

    impl std::os::unix::fs::MetadataExt for Status {
        fn dev(&self) -> u64 {
            1
        }
        fn ino(&self) -> u64 {
            self.ino
        }
        fn mode(&self) -> u32 {
            self.mode
        }
        fn nlink(&self) -> u64 {
            2
        }
        fn uid(&self) -> u32 {
            self.uid
        }
        fn gid(&self) -> u32 {
            self.gid
        }
        fn rdev(&self) -> u64 {
            0
        }
        fn size(&self) -> u64 {
            0
        }
        fn atime(&self) -> i64 {
            0
        }
        fn atime_nsec(&self) -> i64 {
            0
        }
        fn mtime(&self) -> i64 {
            0
        }
        fn mtime_nsec(&self) -> i64 {
            0
        }
        fn ctime(&self) -> i64 {
            self.ctime
        }
        fn ctime_nsec(&self) -> i64 {
            0
        }
        fn blksize(&self) -> u64 {
            4096
        }
        fn blocks(&self) -> u64 {
            0
        }
    }

    /// Within one tick of the clock that stamps ctime, a directory removed and made again at
    /// its path under the same inode number has the made one's ctime. It still does not pass
    /// for the made one unless it also has the owner, group and mode cp left that one with --
    /// and someone else's `mkdir` gives it their own uid.
    #[test]
    fn a_made_directory_is_known_by_its_owner_group_and_mode_too() {
        use super::MadeDirs;
        // S_IFDIR, whose type is u16 on macOS and u32 on Linux.
        const DIR: u32 = 0o040000;
        let path = std::path::Path::new("dest/x");
        let made = Status {
            ino: 7,
            uid: 1000,
            gid: 1000,
            mode: DIR | 0o755,
            ctime: 1_000_000_000,
        };
        let mut dirs = MadeDirs::default();
        dirs.record(&made, path);
        assert!(dirs.made_at(&made, path));
        for other in [
            Status { uid: 1001, ..made },
            Status { gid: 1001, ..made },
            Status {
                mode: DIR | 0o777,
                ..made
            },
        ] {
            assert!(!dirs.made_at(&other, path), "passed for the made one");
        }
        // What cp itself changes, it notes.
        let chowned = Status { uid: 0, ..made };
        dirs.refresh(&chowned, path);
        assert!(dirs.made_at(&chowned, path));
        assert!(!dirs.made_at(&made, path));
    }

    /// A directory this run made stops counting as made once anything but cp changes it --
    /// above all once it is removed and another made at its path, which may take its inode
    /// number. What cp changes itself, it notes (`MadeDirs::refresh`).
    #[test]
    fn a_made_directory_changed_since_is_no_longer_made() {
        use super::MadeDirs;
        use std::fs;
        use std::os::unix::fs::{MetadataExt, PermissionsExt};

        let tmp = plib::tmp::tempdir().unwrap();
        let path = tmp.path().join("x");
        let ctime_moves = |path: &std::path::Path, before: &fs::Metadata| {
            // ctime has the clock's tick for its grain; wait the change into a later one.
            loop {
                let mode = fs::metadata(path).unwrap().mode() ^ 0o001;
                fs::set_permissions(path, fs::Permissions::from_mode(mode)).unwrap();
                let after = fs::metadata(path).unwrap();
                if (after.ctime(), after.ctime_nsec()) != (before.ctime(), before.ctime_nsec()) {
                    return after;
                }
            }
        };

        fs::create_dir(&path).unwrap();
        let mut made = MadeDirs::default();
        let md = fs::metadata(&path).unwrap();
        made.record(&md, &path);
        assert!(made.made_at(&md, &path));
        assert!(
            !made.made_at(&md, &tmp.path().join("y")),
            "found at another path"
        );

        // Changed by someone else.
        let changed = ctime_moves(&path, &md);
        assert!(!made.made_at(&changed, &path), "changed since it was made");
        // Changed by cp, which notes it.
        made.refresh(&changed, &path);
        assert!(made.made_at(&changed, &path));

        // Removed, and another made at its path: when it takes the same inode number (ext4
        // often hands it straight back), it still is not the one made.
        let mut reused = false;
        for _ in 0..64 {
            let before = fs::metadata(&path).unwrap();
            made.record(&before, &path);
            fs::remove_dir(&path).unwrap();
            fs::create_dir(&path).unwrap();
            let other = fs::metadata(&path).unwrap();
            if other.ino() == before.ino() {
                reused = true;
                let other = if (other.ctime(), other.ctime_nsec())
                    == (before.ctime(), before.ctime_nsec())
                {
                    // Same tick: the residual `MadeDirs` documents. Move it on.
                    ctime_moves(&path, &before)
                } else {
                    other
                };
                assert!(
                    !made.made_at(&other, &path),
                    "a new directory passed for the made one"
                );
                break;
            }
        }
        if !reused {
            eprintln!("note: this filesystem did not reuse an inode number; reuse not exercised");
        }
    }

    /// `cp -pR`'s configuration.
    fn cp_pr_config() -> super::CopyConfig {
        super::CopyConfig {
            force: false,
            deref: super::DerefMode::Never,
            interactive: false,
            preserve: true,
            recursive: true,
            no_clobber: false,
            link: false,
            prog: "cp",
            continue_on_error: true,
            destination: super::Destination::MayExist,
            verbose: None,
        }
    }

    /// Whether a group is private is looked up only when the answer is used: when -p asks for
    /// mode and owner and a directory found existing is met. A plain `cp -R` into a
    /// group-writable destination, onto a directory already there, looks nothing up; `cp -pR`
    /// does.
    #[test]
    fn private_groups_are_looked_up_only_under_p() {
        use std::fs;
        use std::os::unix::fs::PermissionsExt;

        let tmp = plib::tmp::tempdir().unwrap();
        let dir = tmp.path();
        let source = dir.join("src");
        fs::create_dir_all(source.join("sub")).unwrap();
        fs::write(source.join("f"), b"f").unwrap();
        // Every directory group-writable, as under a umask of 002, and found again below.
        let dest = dir.join("dest");
        fs::create_dir_all(dest.join("src/sub")).unwrap();
        for found in ["src/sub", "src", ""] {
            fs::set_permissions(dest.join(found), fs::Permissions::from_mode(0o775)).unwrap();
        }
        let copy = |cfg: &super::CopyConfig, name: &str| {
            let _ = super::copy_file(
                cfg,
                &source,
                &dest.join(name),
                super::OperandTrust::Parent,
                &mut super::CopyRun::default(),
                None,
                |_| false,
            );
        };

        let before = plib::madefs::private_group_queries();
        let plain = super::CopyConfig {
            preserve: false,
            ..cp_pr_config()
        };
        copy(&plain, "src");
        assert_eq!(
            plib::madefs::private_group_queries(),
            before,
            "cp -R looked up"
        );
        assert_eq!(fs::read(dest.join("src/f")).unwrap(), b"f");
        // Under -p, but meeting no directory that was already there.
        copy(&cp_pr_config(), "new");
        assert_eq!(
            plib::madefs::private_group_queries(),
            before,
            "nothing found, yet looked up"
        );
        assert_eq!(fs::read(dest.join("new/f")).unwrap(), b"f");
        copy(&cp_pr_config(), "src");
        assert!(
            plib::madefs::private_group_queries() > before,
            "cp -pR did not"
        );
        fs::set_permissions(&dest, fs::Permissions::from_mode(0o755)).unwrap();
    }

    /// `cp -pR src open/new/` where, after cp found no `open/new`, someone who can write `open`
    /// planted `new` as a symbolic link to the user's private directory in a parent only the
    /// user can write. The trailing slash follows the link, as GNU cp's copy does; but the
    /// directory found is not `open/new`, so it is judged as one found in `open` -- or rather
    /// not judged at all, being somewhere else: it keeps its mode, and that is reported.
    #[test]
    fn a_directory_reached_through_a_trailing_slash_link_is_left_alone() {
        use std::fs;
        use std::os::unix::fs::{symlink, PermissionsExt};

        let tmp = plib::tmp::tempdir().unwrap();
        let dir = tmp.path();
        let source = dir.join("src");
        fs::create_dir(&source).unwrap();
        fs::write(source.join("f"), b"f").unwrap();
        fs::set_permissions(&source, fs::Permissions::from_mode(0o755)).unwrap();
        let home = dir.join("home");
        let private = home.join("private");
        fs::create_dir_all(&private).unwrap();
        fs::set_permissions(&private, fs::Permissions::from_mode(0o700)).unwrap();
        fs::set_permissions(&home, fs::Permissions::from_mode(0o755)).unwrap();
        let open = dir.join("open");
        fs::create_dir(&open).unwrap();
        symlink(&private, open.join("new")).unwrap();
        fs::set_permissions(&open, fs::Permissions::from_mode(0o777)).unwrap();

        let mut target = open.join("new").into_os_string();
        target.push("/");
        let result = super::copy_file(
            &cp_pr_config(),
            &source,
            std::path::Path::new(&target),
            super::OperandTrust::Parent,
            &mut super::CopyRun::default(),
            None,
            |_| false,
        );
        fs::set_permissions(&open, fs::Permissions::from_mode(0o755)).unwrap();
        let mode = fs::metadata(&private).unwrap().permissions().mode() & 0o7777;
        assert_eq!(mode, 0o700, "the private directory was opened up");
        assert!(result.is_err(), "leaving it alone must be reported");
        assert!(
            private.join("f").exists(),
            "the copy follows the link, as GNU's does"
        );
    }

    /// The copy `mv` makes after its step 5, which must create the destination operand.
    fn must_create_config() -> super::CopyConfig {
        super::CopyConfig {
            force: true,
            deref: super::DerefMode::Never,
            interactive: false,
            preserve: true,
            recursive: true,
            no_clobber: false,
            link: false,
            prog: "mv",
            continue_on_error: false,
            destination: super::Destination::MustCreate,
            verbose: None,
        }
    }

    /// A fresh directory for one test, removed when dropped.
    fn must_create_scratch() -> plib::tmp::TempDir {
        plib::tmp::tempdir().unwrap()
    }

    /// Copy `source` to `target` in `MustCreate` mode; the error must be the destination's
    /// EEXIST, whatever kind of file the source is.
    fn copy_must_create(source: &std::path::Path, target: &std::path::Path) {
        let result = super::copy_file(
            &must_create_config(),
            source,
            target,
            super::OperandTrust::Parent,
            &mut super::CopyRun::default(),
            None,
            |_| false,
        );
        let exists = super::error_string(&std::io::Error::from_raw_os_error(libc::EEXIST));
        match result {
            Ok(()) => panic!("copied onto a destination it did not create"),
            Err(e) => assert!(e.to_string().ends_with(&exists), "{e}"),
        }
    }

    /// A hard link planted at the destination's name is never written into (as root it would
    /// also take the source's owner and mode).
    #[test]
    fn must_create_refuses_a_file_found_at_the_destination() {
        use std::fs;

        let tmp = must_create_scratch();
        let dir = tmp.path();
        fs::write(dir.join("source"), b"moved").unwrap();
        fs::write(dir.join("victim"), b"victim").unwrap();
        fs::hard_link(dir.join("victim"), dir.join("target")).unwrap();

        copy_must_create(&dir.join("source"), &dir.join("target"));
        let victim = fs::read(dir.join("victim")).unwrap();
        assert_eq!(victim, b"victim");
    }

    /// A directory found at the destination's name is not filled.
    #[test]
    fn must_create_refuses_a_directory_found_at_the_destination() {
        use std::fs;

        let tmp = must_create_scratch();
        let dir = tmp.path();
        fs::create_dir(dir.join("source")).unwrap();
        fs::write(dir.join("source/f"), b"moved").unwrap();
        fs::create_dir(dir.join("target")).unwrap();

        copy_must_create(&dir.join("source"), &dir.join("target"));
        let entries = fs::read_dir(dir.join("target")).unwrap().count();
        assert_eq!(entries, 0);
    }

    /// A symbolic link or special file found under the name just made is taken for the one made
    /// only if it is of that type, has a single link, and belongs to cp's effective user: a
    /// node someone else swapped in (their own, or a link to another) is not.
    #[test]
    fn a_made_node_is_of_its_type_with_one_link_and_ours() {
        use super::is_fresh_node;
        use ftw::FileType;
        const ME: u32 = 1000;

        assert!(is_fresh_node(
            FileType::SymbolicLink,
            1,
            ME,
            FileType::SymbolicLink,
            ME
        ));
        assert!(!is_fresh_node(
            FileType::SymbolicLink,
            1,
            2000,
            FileType::SymbolicLink,
            ME
        ));
        assert!(!is_fresh_node(
            FileType::SymbolicLink,
            2,
            ME,
            FileType::SymbolicLink,
            ME
        ));
        assert!(!is_fresh_node(
            FileType::RegularFile,
            1,
            ME,
            FileType::Fifo,
            ME
        ));
    }

    /// Nor is anything found there unlinked to make room for a symbolic link.
    #[test]
    fn must_create_refuses_to_replace_a_file_with_a_symlink() {
        use std::fs;

        let tmp = must_create_scratch();
        let dir = tmp.path();
        std::os::unix::fs::symlink("anywhere", dir.join("source")).unwrap();
        fs::write(dir.join("target"), b"kept").unwrap();

        copy_must_create(&dir.join("source"), &dir.join("target"));
        let kept = fs::read(dir.join("target")).unwrap();
        assert_eq!(kept, b"kept");
    }
}
