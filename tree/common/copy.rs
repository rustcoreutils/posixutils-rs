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
    mem::MaybeUninit,
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

enum CopyResult {
    CopyingDirectory,
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
fn preserve_through_fd(
    fd: libc::c_int,
    source_md: &impl MetadataExt,
    target: &Path,
) -> io::Result<()> {
    let times = source_times(source_md);
    if unsafe { libc::futimens(fd, times.as_ptr()) } != 0 {
        return Err(preserve_times_error(target, &io::Error::last_os_error()));
    }
    // A failure to duplicate the owner is not itself reported (POSIX leaves it unspecified);
    // its consequence is the mode below.
    let chown_ok = unsafe { libc::fchown(fd, source_md.uid(), source_md.gid()) } == 0;
    if unsafe { libc::fchmod(fd, preserved_mode(source_md, chown_ok)) } != 0 {
        return Err(preserve_mode_error(target, &io::Error::last_os_error()));
    }
    Ok(())
}

/// The source's metadata as it is now (reading it may have moved its access time), through the
/// directory descriptor the walk used, and required to be the very file the walk recorded: an
/// entry swapped since must not lend its owner and mode to the copy.
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
/// What the name holds must be what cp made: the same file type, owned by cp's effective user.
/// On Linux the node is then pinned with an `O_PATH | O_NOFOLLOW` descriptor whose `(dev, ino)`
/// must match that check, and every change goes through it: owner and times with
/// `AT_EMPTY_PATH`, the mode of a special file through `/proc/self/fd`, which names exactly
/// that inode. Elsewhere the changes are made by name with `AT_SYMLINK_NOFOLLOW`, each after
/// re-checking the identity -- the residual is a replacement between that check and the call.
/// A symbolic link's own mode is never set: Linux has none to set, and no access check reads
/// it anywhere.
fn preserve_made_node(
    dirfd: libc::c_int,
    name: &CStr,
    made_type: ftw::FileType,
    source_md: &ftw::Metadata,
    target: &Path,
) -> io::Result<()> {
    let md = ftw::Metadata::new(dirfd, name, false)?;
    if md.file_type() != made_type || md.uid() != unsafe { libc::geteuid() } {
        return Err(io::Error::other(gettext!(
            "'{}' was replaced during the copy",
            target.display()
        )));
    }
    preserve_node_attributes(dirfd, name, &md, made_type, source_md, target)
}

#[cfg(target_os = "linux")]
fn preserve_node_attributes(
    dirfd: libc::c_int,
    name: &CStr,
    made: &ftw::Metadata,
    made_type: ftw::FileType,
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
    if pinned_md.dev() != made.dev() || pinned_md.ino() != made.ino() {
        return Err(io::Error::other(gettext!(
            "'{}' was replaced during the copy",
            target.display()
        )));
    }
    let fd = pinned.as_raw_fd();
    let empty = c"";

    let times = source_times(source_md);
    if unsafe { libc::utimensat(fd, empty.as_ptr(), times.as_ptr(), libc::AT_EMPTY_PATH) } != 0 {
        return Err(preserve_times_error(target, &io::Error::last_os_error()));
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
        let proc_path = CString::new(format!("/proc/self/fd/{fd}")).unwrap();
        let mode = preserved_mode(source_md, chown_ok);
        if unsafe { libc::fchmodat(libc::AT_FDCWD, proc_path.as_ptr(), mode, 0) } != 0 {
            return Err(preserve_mode_error(target, &io::Error::last_os_error()));
        }
    }
    Ok(())
}

#[cfg(not(target_os = "linux"))]
fn preserve_node_attributes(
    dirfd: libc::c_int,
    name: &CStr,
    made: &ftw::Metadata,
    made_type: ftw::FileType,
    source_md: &ftw::Metadata,
    target: &Path,
) -> io::Result<()> {
    let still_made = || -> io::Result<()> {
        let md = ftw::Metadata::new(dirfd, name, false)?;
        if md.dev() != made.dev() || md.ino() != made.ino() {
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

        Ok(CopyResult::CopyingDirectory)
    } else {
        // 3. If source_file is of type regular file

        // When the options say not to follow this entry's links, refuse to follow one that
        // appeared between the traversal's `lstat` and this open. GNU guards the same way.
        let source_open_flags = libc::O_RDONLY
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
        let write_through_dangling = target_is_dangling_symlink && !cfg.no_clobber;
        let create_flags = libc::O_WRONLY
            | libc::O_CREAT
            | if write_through_dangling {
                0
            } else {
                libc::O_EXCL
            };
        // Returns the source and the new destination, both still open, or `None` for a -n skip.
        let create_target_then_copy = || -> io::Result<Option<(fs::File, fs::File)>> {
            let source_fd = unsafe {
                libc::openat(
                    source.dir_fd(),
                    source.file_name().as_ptr(),
                    source_open_flags,
                )
            };
            if source_fd == -1 {
                let e = io::Error::last_os_error();
                let err_str = gettext!(
                    "cannot open '{}' for reading: {}",
                    source.path(),
                    error_string(&e)
                );
                return Err(io::Error::other(err_str));
            }
            let mut source_file = unsafe { fs::File::from_raw_fd(source_fd) };

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

            Ok(Some((source_file, target_file)))
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

        let (source_file, target_file) = if replacing_existing {
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
            let source_fd = unsafe {
                libc::openat(
                    source.dir_fd(),
                    source.file_name().as_ptr(),
                    source_open_flags,
                )
            };
            if source_fd == -1 {
                let e = io::Error::last_os_error();
                let err_str = gettext!(
                    "cannot open '{}' for reading: {}",
                    source.path(),
                    error_string(&e)
                );
                return Err(io::Error::other(err_str));
            }
            let mut source_file = unsafe { fs::File::from_raw_fd(source_fd) };

            // Open what was checked above, and nothing else. A link is followed only when the
            // check saw one, which is now only the operand itself (POSIX writes through it);
            // otherwise `O_NOFOLLOW` refuses a link swapped in since. Either way the descriptor
            // must be the file the check examined -- the link's referent, or the file itself --
            // and only then is it truncated: `O_TRUNC` at the open would have emptied a file
            // swapped in before its identity could be checked.
            let (open_flags, expected_md) = if target_is_symlink {
                (
                    libc::O_WRONLY | libc::O_CLOEXEC,
                    target_deref_md.as_ref().ok(),
                )
            } else {
                (
                    libc::O_WRONLY | libc::O_CLOEXEC | libc::O_NOFOLLOW,
                    target_symlink_md.as_ref(),
                )
            };
            let target_fd = unsafe { libc::openat(target_dirfd, target_filename, open_flags) };
            if target_fd != -1 {
                let mut target_file = unsafe { fs::File::from_raw_fd(target_fd) };
                let opened_md = target_file.metadata()?;
                let is_checked_file = expected_md
                    .is_some_and(|md| md.dev() == opened_md.dev() && md.ino() == opened_md.ino());
                if !is_checked_file {
                    return Err(io::Error::other(gettext!(
                        "will not write to '{}': it changed after it was checked",
                        target.display()
                    )));
                }
                target_file.set_len(0).map_err(|e| {
                    io::Error::other(gettext!(
                        "cannot truncate '{}': {}",
                        target.display(),
                        error_string(&e)
                    ))
                })?;

                io::copy(&mut source_file, &mut target_file)?;
                (source_file, target_file)
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
        // from its own descriptor, as it is after the read.
        let preserve_error = if cfg.preserve {
            source_file
                .metadata()
                .and_then(|source_md| {
                    preserve_through_fd(target_file.as_raw_fd(), &source_md, target)
                })
                .err()
        } else {
            None
        };
        Ok(CopyResult::CopiedFile(preserve_error))
    }
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
                        CopyResult::CopyingDirectory => {
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
                            let new_target_dirfd = match unsafe {
                                ftw::FileDescriptor::open_at(
                                    target_dirfd,
                                    &target_filename_cstr,
                                    libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW,
                                )
                            } {
                                Ok(fd) => fd,
                                Err(e) => {
                                    let err_str = gettext!(
                                        "cannot open directory '{}': {}",
                                        target.display(),
                                        error_string(&e)
                                    );
                                    let e = io::Error::other(err_str);
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
                            let mut st = MaybeUninit::<libc::stat>::uninit();
                            if unsafe { libc::fstat(new_target_dirfd.as_raw_fd(), st.as_mut_ptr()) }
                                == 0
                            {
                                let st = unsafe { st.assume_init() };
                                // Casts needed: `dev_t`/`ino_t` are not u64 on every platform.
                                #[allow(clippy::unnecessary_cast)]
                                dest_dir_ids
                                    .borrow_mut()
                                    .insert((st.st_dev as u64, st.st_ino as u64));
                            }

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

            // Preserve metadata for directories. Must do this inside this closure to ensure no
            // further last access time changes to the source will be made. Applied through the
            // descriptor this copy has held for the directory since it entered it, never by
            // name.
            if let (true, Some(target_dir)) = (cfg.preserve, target_dir) {
                if let Err(e) = fresh_source_md(&source).and_then(|source_md| {
                    preserve_through_fd(target_dir.as_raw_fd(), &source_md, &dir_path)
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
    // nine. `mknodat` applies the umask, and `-p` restores the exact bits afterwards through
    // `copy_characteristics`.
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
