//
// Copyright (c) 2024-2025 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use super::error_string;
use ftw::{self, traverse_directory};
use gettextrs::gettext;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    ffi::{CStr, CString, OsStr},
    fs, io,
    os::{
        fd::{AsRawFd, FromRawFd},
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
pub type InodeMap = HashMap<(u64, u64), (Rc<ftw::FileDescriptor>, CString)>;

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
    /// Diagnostic prefix (`"cp"` or `"mv"`) for messages emitted directly by the copy engine.
    pub prog: &'static str,
    /// When `true` (cp), a per-file failure is reported and the walk continues with same-level and
    /// ancestor entries (POSIX cp CONSEQUENCES OF ERRORS, 90829-90832). When `false` (mv), the
    /// first structural error stops the duplication so the source is not removed.
    pub continue_on_error: bool,
}

/// Where the destination directory of a `CopyingDirectory` came from, so that the descriptor
/// later opened for it can be checked to be that directory.
enum DirOrigin {
    /// This copy made it with `mkdirat`.
    Made,
    /// It already existed, with the identity the decision's `lstat` saw.
    Found { dev: u64, ino: u64 },
}

/// `fstat` of a descriptor the caller keeps open (and goes on owning).
fn fd_metadata(fd: libc::c_int) -> io::Result<fs::Metadata> {
    std::mem::ManuallyDrop::new(unsafe { fs::File::from_raw_fd(fd) }).metadata()
}

/// Check a directory cp has just made with `mkdirat` in `parent_fd` and then opened as `dir_fd`
/// (`O_DIRECTORY | O_NOFOLLOW`): between the two, anyone else who can rename entries in the
/// parent could have swapped in a directory of their own, and cp would copy into it (and,
/// under -p, give it the source's owner and mode).
///
/// Only the parent's owner, and anyone with group or other write permission on it when it is
/// not sticky, can do that; when that is nobody but cp's own user, there is nothing to check.
/// Otherwise the directory must be what a fresh `mkdirat` yields: empty, with the owner and
/// link count `made_by_us` accepts. (Group or other write permission granted by an ACL shows in
/// the group bits.)
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
    let euid = unsafe { libc::geteuid() };
    // `.` relative to a directory descriptor is that directory: no name is resolved.
    let parent = if parent_fd == libc::AT_FDCWD {
        ftw::Metadata::new(dir_fd, c"..", false)?
    } else {
        ftw::Metadata::new(parent_fd, c".", false)?
    };
    // S_ISVTX is 0o1000 (fixed by POSIX).
    let others_can_rename =
        parent.uid() != euid || (parent.mode() & 0o022 != 0 && parent.mode() & 0o1000 == 0);
    if !others_can_rename {
        return Ok(MadeTrust::Full);
    }
    // Every fact about the made directory comes from the descriptor cp goes on to use.
    let opened = fd_metadata(dir_fd)?;
    let made = MadeObject {
        uid: opened.uid(),
        nlink: opened.nlink(),
        is_dir: true,
        owners: fs_owners(dir_fd),
    };
    let replaced = || {
        io::Error::other(gettext!(
            "'{}' was replaced after it was made",
            dir.display()
        ))
    };
    let trust = made_by_us(made, Some(parent.uid()), euid).ok_or_else(replaced)?;
    if !ftw::is_empty_dir_fd(dir_fd)? {
        return Err(replaced());
    }
    Ok(trust)
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

/// What `fstat` (on a descriptor cp holds for it) reports about an object cp has just made.
#[derive(Clone, Copy)]
struct MadeObject {
    uid: u32,
    nlink: u64,
    is_dir: bool,
    /// How the object's filesystem keeps owners (`fs_owners`).
    owners: FsOwners,
}

/// How a filesystem keeps file owners, as far as its type says.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum FsOwners {
    /// Each object's owner is stored and reported as it is (ext4, xfs, btrfs, tmpfs, ...), and
    /// the answer wherever the type cannot be read.
    Stored,
    /// Owners may be mapped -- reported from a mount option, an id map or a squash rule rather
    /// than from who made the object -- or may be stored for real, depending on the mount:
    /// NFS (root_squash or not), FUSE (sshfs with or without idmap, mergerfs, ceph-fuse, ...),
    /// cifs/smb (`uid=` or unix extensions).
    MayBeMapped,
    /// No owner is stored at all; every object reports the mount's owner: msdos/vfat, exfat,
    /// ntfs and ntfs3.
    None,
}

/// How the filesystem holding `fd` keeps owners, from its `fstatfs` type.
#[cfg(target_os = "linux")]
fn fs_owners(fd: libc::c_int) -> FsOwners {
    const OWNERLESS_FS: [u64; 4] = [
        0x4d44,      // MSDOS_SUPER_MAGIC (msdos, vfat)
        0x2011_bab0, // EXFAT_SUPER_MAGIC
        0x5346_544e, // NTFS_SB_MAGIC
        0x7366_746e, // ntfs3
    ];
    const MAYBE_MAPPED_FS: [u64; 5] = [
        0xff53_4d42, // CIFS_SUPER_MAGIC
        0xfe53_4d42, // SMB2_SUPER_MAGIC
        0x517b,      // SMB_SUPER_MAGIC
        0x6969,      // NFS_SUPER_MAGIC
        0x6573_5546, // FUSE_SUPER_MAGIC (fuse and fuseblk)
    ];
    let mut st = std::mem::MaybeUninit::<libc::statfs>::uninit();
    if unsafe { libc::fstatfs(fd, st.as_mut_ptr()) } != 0 {
        return FsOwners::Stored;
    }
    let st = unsafe { st.assume_init() };
    // `f_type` is a signed word whose width varies by architecture; the magic numbers are its
    // low 32 bits.
    let f_type = u64::from(st.f_type as u32);
    if OWNERLESS_FS.contains(&f_type) {
        FsOwners::None
    } else if MAYBE_MAPPED_FS.contains(&f_type) {
        FsOwners::MayBeMapped
    } else {
        FsOwners::Stored
    }
}

#[cfg(not(target_os = "linux"))]
fn fs_owners(_fd: libc::c_int) -> FsOwners {
    FsOwners::Stored
}

/// How far cp trusts an object `made_by_us` accepted.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum MadeTrust {
    /// Owned by cp's effective user, or on a filesystem that stores no owners: -p applies in full.
    Full,
    /// Accepted only because it is owned like its parent, on a filesystem that may store owners
    /// for real: cp copies into it, but -p must not chown or chmod it.
    ParentOwnerOnly,
}

/// Whether `made`, found where cp has just made an object in a directory owned by `parent_uid`
/// (`None` when cp holds no descriptor for that directory), can be the object cp made rather
/// than one swapped in by someone else -- and if so, how far it is trusted.
///
/// Owned by cp's effective user: accepted, in full. Otherwise only in a parent cp's user does
/// not own, and only when the object is owned like that parent, on a filesystem whose type says
/// owners may not be what each creator was:
/// - msdos/vfat, exfat, ntfs, ntfs3 store no owner at all, so every object reports the mount's
///   owner and nothing about ownership can be learned or conferred (a -p chown there fails and
///   drops set-user-ID): accepted in full.
/// - NFS, FUSE and cifs/smb may map owners (root_squash, sshfs without idmap, `uid=`) -- or may
///   store them for real (NFS without squashing, sshfs with idmap, mergerfs, ceph-fuse). In the
///   second case someone who can write the parent but does not own it can rename in an object
///   of the parent owner's (an empty directory, a symbolic link, a FIFO or device node with one
///   link), which cp cannot tell from its own. Such an object is still accepted, so that copying
///   onto these filesystems works, but only as `ParentOwnerOnly`: cp gives it no owner and no
///   mode, so an object it did not make is never chowned or chmod'ed.
///
/// On every other filesystem only cp's effective user is accepted: no one can give a file
/// away, a directory cannot be hard-linked, and a hard link to one of cp's user's nodes fails
/// the link count. The parent's owner, who could plant an object of their own anywhere here,
/// controls every entry of that directory already, cp's included.
///
/// Link count: a fresh symbolic link or special file has exactly one; a fresh directory has
/// two (itself and its `.`), or one on filesystems that do not count directory links (btrfs,
/// some FUSE).
fn made_by_us(made: MadeObject, parent_uid: Option<u32>, euid: u32) -> Option<MadeTrust> {
    let nlink_ok = if made.is_dir {
        made.nlink <= 2
    } else {
        made.nlink == 1
    };
    if !nlink_ok {
        return None;
    }
    if made.uid == euid {
        return Some(MadeTrust::Full);
    }
    let owned_like_parent =
        parent_uid.is_some_and(|parent_uid| parent_uid != euid && made.uid == parent_uid);
    match (made.owners, owned_like_parent) {
        (FsOwners::None, true) => Some(MadeTrust::Full),
        (FsOwners::MayBeMapped, true) => Some(MadeTrust::ParentOwnerOnly),
        _ => None,
    }
}

enum CopyResult {
    CopyingDirectory(DirOrigin),
    /// A non-directory was copied. Carries any failure to duplicate its characteristics (-p),
    /// which is reported but never undoes the copy.
    CopiedFile(Option<io::Error>),
    Skipped,
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
    if unsafe { libc::utimensat(fd, empty.as_ptr(), times.as_ptr(), libc::AT_EMPTY_PATH) } != 0 {
        return Err(preserve_times_error(target, &io::Error::last_os_error()));
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

/// The `fchmodat2` system call number, where it is the generic one (Linux 6.6 and later).
#[cfg(all(
    target_os = "linux",
    any(
        target_arch = "x86_64",
        target_arch = "x86",
        target_arch = "aarch64",
        target_arch = "arm",
        target_arch = "riscv64",
        target_arch = "loongarch64",
        target_arch = "powerpc64",
        target_arch = "s390x"
    )
))]
const SYS_FCHMODAT2: Option<libc::c_long> = Some(452);
#[cfg(all(
    target_os = "linux",
    not(any(
        target_arch = "x86_64",
        target_arch = "x86",
        target_arch = "aarch64",
        target_arch = "arm",
        target_arch = "riscv64",
        target_arch = "loongarch64",
        target_arch = "powerpc64",
        target_arch = "s390x"
    ))
))]
const SYS_FCHMODAT2: Option<libc::c_long> = None;

/// Set the mode of the inode an `O_PATH` descriptor pins, which `fchmod` refuses (EBADF).
///
/// First `fchmodat2(fd, "", mode, AT_EMPTY_PATH)` (Linux 6.6 and later), which acts on that
/// inode directly. Where it does not exist (ENOSYS) or does not take `AT_EMPTY_PATH` (EINVAL),
/// `fchmodat` on `self/fd/N` relative to a `/proc` descriptor verified to be procfs, which
/// names the same inode. With neither, the mode is not set and the failure is reported: a
/// by-name fallback could act on whatever the name holds by then.
#[cfg(target_os = "linux")]
fn chmod_pinned(fd: libc::c_int, mode: libc::mode_t) -> io::Result<()> {
    if let Some(sys_fchmodat2) = SYS_FCHMODAT2 {
        let ret = unsafe {
            libc::syscall(
                sys_fchmodat2,
                fd,
                c"".as_ptr(),
                libc::c_uint::from(mode),
                libc::AT_EMPTY_PATH,
            )
        };
        if ret == 0 {
            return Ok(());
        }
        let e = io::Error::last_os_error();
        if !matches!(e.raw_os_error(), Some(libc::ENOSYS) | Some(libc::EINVAL)) {
            return Err(e);
        }
    }
    let proc_dir = procfs_dir()?;
    let pinned = proc_fd_name(fd);
    if unsafe { libc::fchmodat(proc_dir.as_raw_fd(), pinned.as_ptr(), mode, 0) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
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

    let target_symlink_md = ftw::Metadata::new(
        target_dirfd,
        unsafe { CStr::from_ptr(target_filename) },
        false,
    );
    let target_deref_md = ftw::Metadata::new(
        target_dirfd,
        unsafe { CStr::from_ptr(target_filename) },
        true,
    );
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

    // 1. If source_file references the same file as dest_file
    if let (Ok(smd), Ok(tmd)) = (&source_deref_md, &target_deref_md) {
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
                // modified by the umask of the process. Copying the permission bits without the
                // umask is postponed to the `postprocess_dir` closure on the call to
                // `traverse_directory` inside `copy_file`. Under -p it is made owner-only: until
                // that closure duplicates the owner, the directory belongs to whoever ran cp, and
                // group or other write permission would let others plant entries in it.
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
        // link, or a special file, replaces the link instead.)
        let target_is_symlink = target_symlink_md
            .as_ref()
            .is_some_and(|md| md.file_type() == ftw::FileType::SymbolicLink);
        if target_is_symlink
            && !state.at_top_level
            && !act_on_link_itself
            && !(source_is_special_file && cfg.recursive)
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
                Ok(()) => Ok(CopyResult::CopiedFile(preserve_made(
                    cfg,
                    source,
                    target_dirfd,
                    target_filename,
                    source_file_type,
                    target,
                ))),
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
            return Ok(CopyResult::CopiedFile(preserve_made(
                cfg,
                source,
                target_dirfd,
                target_filename,
                ftw::FileType::SymbolicLink,
                target,
            )));
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
        Ok(CopyResult::CopiedFile(preserve_error))
    }
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
/// FIFO peer. Without procfs (and on other systems) the name is `fstatat`'ed with
/// `AT_SYMLINK_NOFOLLOW` and must be a regular file just before the blocking open; the residual
/// is a FIFO swapped in between those two calls. A non-regular file fails with ENXIO, the error
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
    let name_cstr = unsafe { CStr::from_ptr(name) };
    match ftw::Metadata::new(dirfd, name_cstr, false) {
        Ok(md) if md.file_type() == ftw::FileType::RegularFile => {}
        Ok(_) => return not_regular(),
        Err(_) => return -1,
    }
    unsafe { libc::openat(dirfd, name, flags) }
}

/// `/proc`, opened and verified to be procfs (`PROC_SUPER_MAGIC`), so that `self/fd/N` names
/// exactly the inode open on descriptor N rather than whatever else is mounted or planted there.
#[cfg(target_os = "linux")]
fn procfs_dir() -> io::Result<fs::File> {
    const PROC_SUPER_MAGIC: u32 = 0x9fa0;
    let fd = unsafe {
        libc::open(
            c"/proc".as_ptr(),
            libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC,
        )
    };
    if fd == -1 {
        return Err(io::Error::last_os_error());
    }
    let dir = unsafe { fs::File::from_raw_fd(fd) };
    let mut st = std::mem::MaybeUninit::<libc::statfs>::uninit();
    if unsafe { libc::fstatfs(dir.as_raw_fd(), st.as_mut_ptr()) } != 0 {
        return Err(io::Error::last_os_error());
    }
    // `f_type` is a signed word whose width varies by architecture; the magic is its low bits.
    if unsafe { st.assume_init() }.f_type as u32 != PROC_SUPER_MAGIC {
        return Err(io::Error::other(gettext("/proc is not a procfs mount")));
    }
    Ok(dir)
}

/// `self/fd/N`, relative to `procfs_dir`.
#[cfg(target_os = "linux")]
fn proc_fd_name(fd: libc::c_int) -> CString {
    CString::new(format!("self/fd/{fd}")).unwrap()
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

pub fn copy_file<F>(
    cfg: &CopyConfig,
    source_arg: &Path,
    target_arg: &Path,
    created_files: &mut HashSet<PathBuf>,
    inode_map: Option<&mut InodeMap>,
    prompt_fn: F,
) -> io::Result<()>
where
    F: Copy + Fn(&str) -> bool,
{
    copy_file_at(
        cfg,
        source_arg,
        target_arg,
        None,
        created_files,
        inode_map,
        prompt_fn,
    )
}

/// `copy_file`, with the destination's directory optionally given as a descriptor: with
/// `Some(dir)`, the copy is made as `target_arg`'s last component inside `dir` and `target_arg`
/// is only named in diagnostics, so no part of it is resolved by path.
pub fn copy_file_at<F>(
    cfg: &CopyConfig,
    source_arg: &Path,
    target_arg: &Path,
    target_dir: Option<ftw::FileDescriptor>,
    created_files: &mut HashSet<PathBuf>,
    mut inode_map: Option<&mut InodeMap>,
    prompt_fn: F,
) -> io::Result<()>
where
    F: Copy + Fn(&str) -> bool,
{
    // The operand's own directory and name: the current directory and the whole operand, or the
    // given directory and the operand's last component.
    let (top_dir, top_name, top_dir_path) = match (target_dir, target_arg.file_name()) {
        (Some(dir), Some(name)) => (
            dir,
            name,
            target_arg.parent().unwrap_or(Path::new("")).to_path_buf(),
        ),
        _ => (
            ftw::FileDescriptor::cwd(),
            target_arg.as_os_str(),
            PathBuf::new(),
        ),
    };
    // `RefCell` to allow sharing these between closures. The bottom entry is the operand's
    // directory, so a stack of one means the entry is the operand itself.
    let target_dirfd_stack = RefCell::new(vec![Rc::new(top_dir)]);
    // (st_dev, st_ino) of every destination directory this copy creates or enters. A source
    // directory found in here is one we are copying *into*.
    let dest_dir_ids = RefCell::new(HashSet::<(u64, u64)>::new());
    // (st_dev, st_ino) of the destination directories this copy made but trusts only as
    // `MadeTrust::ParentOwnerOnly`: -p gives them no owner and no mode.
    let parent_owner_only_dirs = RefCell::new(HashSet::<(u64, u64)>::new());
    let target_dir_path = RefCell::new(top_dir_path);
    let terminate = RefCell::new(false);
    let last_error = RefCell::new(None);
    // In `continue_on_error` (cp) mode each diagnostic is emitted immediately and this flag records
    // that the final exit status must be non-zero (without a returned message to re-print).
    let had_error = RefCell::new(false);

    let _ = traverse_directory(
        source_arg,
        |source| {
            let mut terminate_borrowed = terminate.borrow_mut();
            let mut target_dirfd_stack_borrowed = target_dirfd_stack.borrow_mut();
            let mut target_dir_path_borrowed = target_dir_path.borrow_mut();

            if *terminate_borrowed {
                return Ok(false);
            }

            let at_top_level = target_dirfd_stack_borrowed.len() == 1;
            let target_dirfd = target_dirfd_stack_borrowed.last().unwrap();

            let target_filename = if at_top_level {
                top_name
            } else {
                OsStr::from_bytes(source.file_name().to_bytes())
            };

            let target = target_dir_path_borrowed.join(target_filename);
            let target_filename_cstr = CString::new(target_filename.as_bytes()).unwrap();

            let source_md = source.metadata().unwrap();
            let identifier = (source_md.dev(), source_md.ino());

            // Hard-link preserving behavior of `mv`. `cp` does not maintain the hard-link structure
            // of the hierarchy according to the standard
            if let Some(inode_map) = inode_map.as_deref_mut() {
                // Preserve hard links like coreutils mv. Creating a copy is also
                // allowed by the standard.
                if let Some((prev_dirfd, prev_filename)) = inode_map.get(&identifier) {
                    let ret = unsafe {
                        libc::linkat(
                            prev_dirfd.as_raw_fd(),
                            prev_filename.as_ptr(),
                            target_dirfd.as_raw_fd(),
                            target_filename_cstr.as_ptr(),
                            0, // Don't dereference prev if it's a symlink
                        )
                    };
                    // If success
                    if ret == 0 {
                        // Skip since this file/directory is handled by hard-linking
                        return Ok(false);
                    }
                    // else failed
                    else {
                        let e = io::Error::last_os_error();
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
                    // Record where this inode landed only if a file was actually created there.
                    // Recording a skipped copy pointed a later hard link at a target that does
                    // not exist, and every directory reports nlink > 1, so directories were
                    // recorded too.
                    if matches!(copy_result, CopyResult::CopiedFile(_)) {
                        if let Some(inode_map) = inode_map.as_deref_mut() {
                            // Only files that have hard links are worth tracking.
                            if source_md.nlink() > 1 {
                                inode_map.insert(
                                    identifier,
                                    (Rc::clone(target_dirfd), target_filename_cstr.clone()),
                                );
                            }
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
                            let opened = ftw::FileDescriptor::open_at(
                                target_dirfd,
                                &target_filename_cstr,
                                libc::O_RDONLY
                                    | libc::O_DIRECTORY
                                    | libc::O_NOFOLLOW
                                    | libc::O_CLOEXEC,
                            )
                            .map_err(|e| {
                                io::Error::other(gettext!(
                                    "cannot open directory '{}': {}",
                                    target.display(),
                                    error_string(&e)
                                ))
                            })
                            .and_then(|fd| {
                                let md = fd_metadata(fd.as_raw_fd())?;
                                match origin {
                                    DirOrigin::Found { dev, ino }
                                        if dev != md.dev() || ino != md.ino() =>
                                    {
                                        Err(io::Error::other(gettext!(
                                            "'{}' was replaced after it was checked",
                                            target.display()
                                        )))
                                    }
                                    DirOrigin::Found { .. } => Ok((fd, md)),
                                    DirOrigin::Made => {
                                        let trust = verify_made_dir(
                                            target_dirfd.as_raw_fd(),
                                            fd.as_raw_fd(),
                                            &target,
                                        )?;
                                        if trust == MadeTrust::ParentOwnerOnly {
                                            parent_owner_only_dirs
                                                .borrow_mut()
                                                .insert((md.dev(), md.ino()));
                                        }
                                        Ok((fd, md))
                                    }
                                }
                            });
                            let (new_target_dirfd, new_target_md) = match opened {
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

                            target_dirfd_stack_borrowed.push(Rc::new(new_target_dirfd));
                            target_dir_path_borrowed.push(target_filename);

                            true
                        }
                        CopyResult::CopiedFile(preserve_error) => {
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
        },
        // Pops unconditionally. `ftw` calls this for every directory whose handler returned
        // `true`, including ones it then could not descend into; leaving the push in place there
        // would silently redirect every later file into the wrong destination directory.
        // The target directory exists either way, so `-p` still applies to it.
        |source, _exit| {
            let mut target_dirfd_stack_borrowed = target_dirfd_stack.borrow_mut();
            let mut target_dir_path_borrowed = target_dir_path.borrow_mut();

            let dir_path = target_dir_path_borrowed.clone();
            target_dir_path_borrowed.pop();
            let target_dir = target_dirfd_stack_borrowed.pop();

            // Preserve metadata for directories, after their contents so nothing written into
            // them moves their times afterwards. Applied through the descriptor this copy has
            // held for the directory since it entered it, never by name. The source's metadata
            // is what the walk recorded when it stat'ed the directory, before reading it: the
            // read moved its access time, and GNU keeps the original too.
            if let (true, Some(target_dir)) = (cfg.preserve, target_dir) {
                let recorded = source
                    .metadata()
                    .ok_or_else(|| io::Error::other(gettext!("cannot stat '{}'", source.path())));
                // A directory made but trusted only as owned like its parent gets no owner and
                // no mode; its identity comes from the descriptor held for it.
                let trust = fd_metadata(target_dir.as_raw_fd()).map(|md| {
                    if parent_owner_only_dirs
                        .borrow()
                        .contains(&(md.dev(), md.ino()))
                    {
                        MadeTrust::ParentOwnerOnly
                    } else {
                        MadeTrust::Full
                    }
                });
                if let Err(e) = recorded.and_then(|source_md| {
                    preserve_through_fd(target_dir.as_raw_fd(), source_md, &dir_path, trust?)
                }) {
                    // Same policy as the file case: never fatal, exit-status only for cp.
                    eprintln!("{}: {}", cfg.prog, error_string(&e));
                    if cfg.continue_on_error {
                        *had_error.borrow_mut() = true;
                    }
                }
            }

            Ok(())
        },
        |entry, error| {
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
        },
        ftw::TraverseDirectoryOpts {
            follow_symlinks_on_args: cfg.deref.follow_symlinks_on_args(),
            follow_symlinks: cfg.deref.follow_symlinks(),
            // One target-directory descriptor is held per level in `target_dirfd_stack`, so the
            // traversal must count those too when deciding to conserve descriptors.
            caller_fds_per_level: 1,
            ..Default::default()
        },
    );

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

    let mut created_files = HashSet::new();

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

        let new_target = if source.is_dir() && ends_with_slash_dot(source) {
            // This causes the contents of `source` to be copied instead of
            // `source` itself
            target.to_path_buf()
        } else {
            match source.file_name() {
                Some(file_name) => target.join(file_name),
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
            &mut created_files,
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
    use super::{made_by_us, FsOwners, MadeObject, MadeTrust};

    const CP: u32 = 1000;
    const OTHER: u32 = 2000;
    const FULL: Option<MadeTrust> = Some(MadeTrust::Full);
    const PARENT_OWNER_ONLY: Option<MadeTrust> = Some(MadeTrust::ParentOwnerOnly);

    fn dir(uid: u32, nlink: u64) -> MadeObject {
        MadeObject {
            uid,
            nlink,
            is_dir: true,
            owners: FsOwners::Stored,
        }
    }

    fn node(uid: u32, nlink: u64) -> MadeObject {
        MadeObject {
            uid,
            nlink,
            is_dir: false,
            owners: FsOwners::Stored,
        }
    }

    /// On NFS, FUSE or cifs/smb, which may map owners or store them.
    fn maybe_mapped(made: MadeObject) -> MadeObject {
        MadeObject {
            owners: FsOwners::MayBeMapped,
            ..made
        }
    }

    /// On msdos/vfat, exfat or ntfs, which store no owner.
    fn ownerless(made: MadeObject) -> MadeObject {
        MadeObject {
            owners: FsOwners::None,
            ..made
        }
    }

    /// The ordinary case: cp's own object in cp's own or anyone's directory, on any filesystem.
    #[test]
    fn trusts_what_cp_made() {
        assert_eq!(made_by_us(dir(CP, 2), Some(CP), CP), FULL);
        assert_eq!(made_by_us(dir(CP, 2), Some(OTHER), CP), FULL);
        assert_eq!(made_by_us(node(CP, 1), Some(OTHER), CP), FULL);
        assert_eq!(made_by_us(node(CP, 1), None, CP), FULL);
        assert_eq!(made_by_us(maybe_mapped(dir(CP, 2)), Some(OTHER), CP), FULL);
    }

    /// A filesystem with no owners reports the mount's owner for everything: what cp makes is
    /// owned like its parent, and there is no owner an attacker's object could carry instead.
    #[test]
    fn trusts_an_owner_reported_by_an_ownerless_filesystem() {
        assert_eq!(made_by_us(ownerless(dir(4242, 2)), Some(4242), CP), FULL);
        assert_eq!(made_by_us(ownerless(node(4242, 1)), Some(4242), CP), FULL);
    }

    /// NFS, FUSE or cifs/smb owned like the parent: accepted, so the copy works where owners are
    /// mapped (root_squash, sshfs without idmap), but -p withholds owner and mode, because where
    /// owners are stored (NFS without squashing, sshfs with idmap) the parent's owner's object
    /// may have been renamed in by someone else.
    #[test]
    fn accepts_but_does_not_trust_an_owner_that_may_be_stored() {
        assert_eq!(
            made_by_us(maybe_mapped(dir(4242, 2)), Some(4242), CP),
            PARENT_OWNER_ONLY
        );
        assert_eq!(
            made_by_us(maybe_mapped(node(4242, 1)), Some(4242), CP),
            PARENT_OWNER_ONLY
        );
        assert_eq!(
            made_by_us(maybe_mapped(dir(65534, 2)), Some(65534), 0),
            PARENT_OWNER_ONLY
        );
    }

    /// Where owners are stored, an object owned by the parent's owner (who is not cp's user)
    /// was made by that owner, not by cp.
    #[test]
    fn refuses_a_parent_owners_object_where_owners_are_stored() {
        assert_eq!(made_by_us(dir(OTHER, 2), Some(OTHER), CP), None);
        assert_eq!(made_by_us(node(OTHER, 1), Some(OTHER), CP), None);
    }

    /// The parent-owner arm needs the parent's owner; with no parent descriptor only cp's own
    /// user is accepted.
    #[test]
    fn refuses_a_foreign_owner_without_a_parent() {
        assert_eq!(made_by_us(maybe_mapped(dir(4242, 2)), None, CP), None);
        assert_eq!(made_by_us(ownerless(dir(4242, 2)), None, CP), None);
    }

    /// Filesystems that report 1 for every directory's link count (btrfs, some FUSE).
    #[test]
    fn accepts_a_directory_link_count_of_one() {
        assert_eq!(made_by_us(dir(CP, 1), Some(CP), CP), FULL);
    }

    /// Someone else's object in a directory cp owns: an attacker in a shared directory.
    #[test]
    fn refuses_another_users_object() {
        assert_eq!(made_by_us(dir(OTHER, 2), Some(CP), CP), None);
        assert_eq!(made_by_us(node(OTHER, 1), Some(CP), CP), None);
        assert_eq!(made_by_us(maybe_mapped(dir(OTHER, 2)), Some(CP), CP), None);
        assert_eq!(made_by_us(ownerless(dir(OTHER, 2)), Some(CP), CP), None);
        // In someone else's directory, an object owned by a third user.
        assert_eq!(made_by_us(dir(3000, 2), Some(OTHER), CP), None);
        assert_eq!(
            made_by_us(maybe_mapped(dir(3000, 2)), Some(OTHER), CP),
            None
        );
    }

    /// A hard link to an existing node (cp's own, or a mapped owner's) is not a fresh one, and
    /// a directory with subdirectories is not a fresh one.
    #[test]
    fn refuses_link_counts_a_fresh_object_cannot_have() {
        assert_eq!(made_by_us(node(CP, 2), Some(CP), CP), None);
        assert_eq!(
            made_by_us(maybe_mapped(node(4242, 2)), Some(4242), CP),
            None
        );
        assert_eq!(made_by_us(dir(CP, 3), Some(CP), CP), None);
    }

    /// -p on an object trusted only as owned like its parent: the times are applied, the mode
    /// (and owner) are not, and that is reported.
    #[test]
    fn withholds_owner_and_mode_from_a_parent_owner_only_object() {
        use super::preserve_through_fd;
        use std::fs;
        use std::os::fd::AsRawFd;
        use std::os::unix::fs::{MetadataExt, PermissionsExt};

        let dir = std::env::temp_dir().join(format!("cp_parent_owner_only_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir(&dir).unwrap();
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
        let _ = fs::remove_dir_all(&dir);

        assert!(
            result.is_err(),
            "withholding owner and mode must be reported"
        );
        assert_eq!(after.mode() & 0o7777, 0o600, "the mode was applied");
        assert_eq!(after.mtime(), 978_307_200, "the times were not applied");
    }
}
