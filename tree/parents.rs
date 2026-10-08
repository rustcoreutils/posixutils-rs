//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `cp --parents`: the destination of `dir/sub/file` is `target/dir/sub/file`, and the
//! directories on the way are made from the source's. Forced by debhelper (dh_install,
//! dh_installdocs, dh_installexamples), which copies a filtered tree one path at a time.
//!
//! The directories are walked with `openat`/`mkdirat` from descriptors, so each step acts on
//! the directory the previous step opened, and `-p` applies attributes through those same
//! descriptors.

use crate::common::{
    copy_file, copy_file_at, error_string, finish_made_dir_mode, made_dir_open_error,
    open_made_dir, preserve_through_fd, report_verbose_bytes, ChainTrust, CopyConfig, CopyRun,
    InodeMap, MadeDirs, MadeTrust, OperandTrust,
};
use gettextrs::gettext;
use std::ffi::CString;
use std::fs::File;
use std::io;
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::MetadataExt;
use std::path::{Component, Path, PathBuf};

/// A destination directory `--parents` created, with the source directory it stands for.
struct MadeDir {
    dest: File,
    source: std::fs::Metadata,
    /// Where it is, for diagnostics only.
    path: PathBuf,
    /// How far `verify_made_dir` trusts it: -p gives it an owner and mode only in full.
    trust: MadeTrust,
}

/// Open directory `name` in `dirfd`. `extra_flags` is OR'ed in (`O_NOFOLLOW`).
fn open_dir_at(dirfd: libc::c_int, name: &CString, extra_flags: libc::c_int) -> io::Result<File> {
    let fd = unsafe {
        libc::openat(
            dirfd,
            name.as_ptr(),
            libc::O_RDONLY | libc::O_DIRECTORY | libc::O_CLOEXEC | extra_flags,
        )
    };
    if fd < 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(File::from(unsafe { OwnedFd::from_raw_fd(fd) }))
}

fn cstring(bytes: &[u8]) -> io::Result<CString> {
    CString::new(bytes).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))
}

/// Make every directory in `source`'s parent path under `target`, returning the ones made and
/// a descriptor for the last one, where the copy itself goes.
///
/// The target operand and the source path are resolved as the user wrote them. Below the target,
/// every destination component is opened from the held parent descriptor with
/// `O_DIRECTORY | O_NOFOLLOW`, whether this call made it or found it: a symbolic link there is
/// refused, so neither the copy nor `-p`'s owner, mode and times can be redirected through it.
/// GNU cp follows a symbolic link it finds, but a link planted between a failed lookup and the
/// `mkdirat` is indistinguishable from one that was there before, so none is followed. Nothing
/// is stat'ed before its open; the identity used afterwards is the opened descriptor's own.
///
/// The target is the anchor, and the trust it hands is carried down the walk with the
/// descriptors (`ChainTrust`): a directory found hands on what it was handed and its own, one
/// this run made and verified in full -- now, or earlier at the same path (`MadeDirs`) --
/// starts afresh. The last one's is returned, for the directories the copy finds in it.
fn make_parents(
    source: &Path,
    target: &Path,
    preserve: bool,
    verbose: bool,
    made_dirs: &mut MadeDirs,
) -> io::Result<Walked> {
    let mut made = Vec::new();
    let mut own = Vec::new();
    let mut dest_dir = open_dir_at(libc::AT_FDCWD, &cstring(target.as_os_str().as_bytes())?, 0)?;
    let mut trust = ChainTrust::anchor(dest_dir.as_raw_fd())?;
    let Some(parent) = source.parent() else {
        return Ok(Walked {
            made,
            own,
            dest: dest_dir,
            trust,
        });
    };
    let start = if source.is_absolute() { "/" } else { "." };
    let mut src_dir = open_dir_at(libc::AT_FDCWD, &cstring(start.as_bytes())?, 0)?;
    let mut dest_path = target.to_path_buf();
    // The source's components so far, for -v.
    let mut src_path = PathBuf::from(if source.is_absolute() { "/" } else { "" });

    for comp in parent.components() {
        // `..` is followed as written, as GNU cp does.
        let name = match comp {
            Component::Normal(_) | Component::ParentDir => cstring(comp.as_os_str().as_bytes())?,
            Component::RootDir | Component::CurDir | Component::Prefix(_) => continue,
        };
        dest_path.push(comp);
        src_path.push(comp);

        let next_src = open_dir_at(src_dir.as_raw_fd(), &name, 0).map_err(|e| {
            io::Error::other(gettext!(
                "cannot stat '{}': {}",
                source.display(),
                error_string(&e)
            ))
        })?;
        let src_md = next_src.metadata()?;

        // Owner search and write are needed to fill the directory; without -p the umask
        // applies as it does to cp -R, and `finish_dir` takes back the owner bits the source
        // lacks. Under -p it is made owner-only, and `finish_dir` sets
        // the exact mode through its descriptor once the owner is duplicated: until then it
        // belongs to whoever ran cp, and must not let others plant entries in it.
        let mode = if preserve {
            libc::S_IRWXU
        } else {
            (src_md.mode() & 0o7777) as libc::mode_t | libc::S_IRWXU
        };
        let created = unsafe { libc::mkdirat(dest_dir.as_raw_fd(), name.as_ptr(), mode) } == 0;
        if !created {
            let e = io::Error::last_os_error();
            if e.raw_os_error() != Some(libc::EEXIST) {
                return Err(io::Error::other(gettext!(
                    "cannot make directory '{}': {}",
                    dest_path.display(),
                    error_string(&e)
                )));
            }
        }
        let next_dest = if created {
            // Between the `mkdirat` and the open, anyone else who can rename entries in the
            // parent could have swapped in a directory of their own; and the umask may have
            // withheld the owner permission the directory needs to be filled
            // (`open_made_dir`).
            let (opened, made_trust) = open_made_dir(dest_dir.as_raw_fd(), &name, &dest_path)
                .map_err(|e| made_dir_open_error(&dest_path, e))?;
            let next_dest = File::from(opened);
            trust = if made_trust == MadeTrust::Full {
                made_dirs.record(&next_dest.metadata()?, &dest_path);
                own.push((next_dest.try_clone()?, dest_path.clone()));
                ChainTrust::made(next_dest.as_raw_fd())?
            } else {
                // Owned like its parent only, it may be someone else's: it hands on what a
                // directory found would.
                trust.found(next_dest.as_raw_fd())?
            };
            made.push(MadeDir {
                dest: next_dest.try_clone()?,
                source: src_md,
                path: dest_path.clone(),
                trust: made_trust,
            });
            if verbose {
                report_made_dir(&src_path, &dest_path);
            }
            next_dest
        } else {
            let found =
                open_dir_at(dest_dir.as_raw_fd(), &name, libc::O_NOFOLLOW).map_err(|e| {
                    io::Error::other(gettext!(
                        "'{}' exists but is not a directory: {}",
                        dest_path.display(),
                        error_string(&e)
                    ))
                })?;
            trust = if made_dirs.made_at(&found.metadata()?, &dest_path) {
                own.push((found.try_clone()?, dest_path.clone()));
                ChainTrust::made(found.as_raw_fd())?
            } else {
                trust.found(found.as_raw_fd())?
            };
            found
        };
        src_dir = next_src;
        dest_dir = next_dest;
    }
    Ok(Walked {
        made,
        own,
        dest: dest_dir,
        trust,
    })
}

/// What `make_parents` walked to.
struct Walked {
    /// The directories it made.
    made: Vec<MadeDir>,
    /// The directories on the way this run made, now or earlier (`MadeDirs`), and where: the
    /// copy below them changes them, and that is noted once it is done (`MadeDirs::refresh`).
    own: Vec<(File, PathBuf)>,
    /// The last directory, where the copy itself goes.
    dest: File,
    /// The trust that directory hands the directories the copy finds in it.
    trust: ChainTrust,
}

/// GNU `cp -v --parents` for a directory made on the way: `source -> dest`, unquoted, unlike
/// every other line `-v` writes.
fn report_made_dir(source: &Path, dest: &Path) {
    let mut line = source.as_os_str().as_bytes().to_vec();
    line.extend_from_slice(b" -> ");
    line.extend_from_slice(dest.as_os_str().as_bytes());
    report_verbose_bytes(&line);
}

/// The final attributes of a directory `--parents` made, set once the copy below it is done.
///
/// Under -p: owner, mode and times of its source directory.
/// The same code as every other -p through a held descriptor (`preserve_through_fd`): set-user-ID
/// and set-group-ID are dropped when the owner cannot be copied, and a directory trusted only
/// as owned like its parent gets times but no owner or mode.
///
/// Without -p it gets its source's permission bits less the umask (`finish_made_dir_mode`),
/// taking back the S_IRWXU `make_parents` added. POSIX has no --parents; this is the mode GNU
/// cp gives these directories, and the one POSIX cp 2.g gives a directory `cp -R` makes.
fn finish_dir(dir: &MadeDir, preserve: bool, umask: u32) -> io::Result<()> {
    let fd = dir.dest.as_raw_fd();
    if preserve {
        preserve_through_fd(fd, &dir.source, &dir.path, dir.trust)
    } else {
        finish_made_dir_mode(fd, &dir.source, umask, &dir.path)
    }
}

/// Copy each source to `target` joined with the source's own path. Returns false if anything
/// failed; every failure has been reported.
pub fn copy_with_parents<F>(
    cfg: &CopyConfig,
    sources: &[PathBuf],
    target: &Path,
    mut inode_map: Option<&mut InodeMap>,
    prompt_fn: F,
) -> bool
where
    F: Copy + Fn(&str) -> bool,
{
    let mut ok = true;
    let mut run = CopyRun::default();
    // Read once: each read is a pair of umask(2) calls.
    let umask = plib::modestr::umask();
    for source in sources {
        match source.components().next_back() {
            Some(Component::Normal(_)) => {}
            // A source ending in `..` has a destination ending in `..`: no name inside the
            // target, but whatever directory that reaches -- the target's parent, for `..`
            // itself -- which no chain from the target describes. GNU cp copies onto it.
            Some(Component::ParentDir) => {
                eprintln!(
                    "cp: {}",
                    gettext!(
                        "with --parents, '{}' would be copied to '{}', which is not a name \
                         inside '{}'",
                        source.display(),
                        target.join(source).display(),
                        target.display()
                    )
                );
                ok = false;
                continue;
            }
            // `.` (or `/`) is the directory every source path starts from, whose copy is the
            // target itself: its contents are copied there, as for `cp -R src/. target`. Its
            // last component taken for a name, `target/.` was copied into `target/target`.
            _ => {
                let copied = copy_file(
                    cfg,
                    source,
                    target,
                    OperandTrust::Named,
                    &mut run,
                    inode_map.as_deref_mut(),
                    prompt_fn,
                );
                if let Err(e) = copied {
                    let s = error_string(&e);
                    if !s.is_empty() {
                        eprintln!("cp: {s}");
                    }
                    ok = false;
                }
                continue;
            }
        }
        let walked = make_parents(
            source,
            target,
            cfg.preserve,
            cfg.verbose.is_some(),
            &mut run.made_dirs,
        );
        let Walked {
            made,
            own,
            dest: dest_dir,
            trust,
        } = match walked {
            Ok(walked) => walked,
            Err(e) => {
                eprintln!("cp: {}", error_string(&e));
                ok = false;
                continue;
            }
        };
        let relative: PathBuf = source
            .components()
            .filter(|c| !matches!(c, Component::RootDir | Component::Prefix(_)))
            .collect();
        let dest = target.join(relative);
        // The copy is made in the directory `make_parents` holds open, never by re-resolving
        // `dest`, whose components could have been swapped since.
        if let Err(e) = copy_file_at(
            cfg,
            source,
            &dest,
            (OwnedFd::from(dest_dir).into(), trust),
            &mut run,
            inode_map.as_deref_mut(),
            prompt_fn,
        ) {
            let s = error_string(&e);
            if !s.is_empty() {
                eprintln!("cp: {s}");
            }
            ok = false;
        }
        for dir in &made {
            if let Err(e) = finish_dir(dir, cfg.preserve, umask) {
                eprintln!("cp: {}", error_string(&e));
                ok = false;
            }
        }
        // Held since they were found to be the run's own, they still are.
        for (dir, path) in &own {
            if let Ok(md) = dir.metadata() {
                run.made_dirs.refresh(&md, path);
            }
        }
    }
    ok
}
