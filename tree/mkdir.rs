//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::gettext;
use modestr::{ChmodActionOp, ChmodClause, ChmodMode};
use plib::madefs::{chmod_fd, fstat, lstat_at, MadeTrust, SEARCH_ONLY};
use plib::modestr;
use std::ffi::{CStr, CString};
use std::io;
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};

/// mkdir - make directories
#[derive(Parser)]
#[command(version, about = gettext("mkdir - make directories"))]
struct Args {
    #[arg(short, long, help = gettext("Create any missing intermediate pathname components"))]
    parents: bool,

    #[arg(short, long, allow_hyphen_values = true, help = gettext("Set the file permission bits of the newly-created directory to the specified mode value"))]
    mode: Option<String>,

    #[arg(help = gettext("A pathname of a directory to be created"))]
    dirs: Vec<String>,
}

/// How a directory `make_dir` makes gets its final mode.
#[derive(Clone, Copy)]
enum Finish {
    /// As `mkdir` leaves it.
    AsMade,
    /// The mode `-m` gives, where `mkdir` left any of the bits it names (the mask;
    /// `named_bits`) otherwise: `mkdir` takes no set-user-ID or set-group-ID bit, and under a
    /// default ACL its mode only masks the ACL the directory inherits.
    Named(libc::mode_t, u32),
    /// Owner write and search added where missing, as POSIX asks of a `-p` intermediate.
    OwnerWriteSearch,
}

/// Create the directory `path` with the given mode, then finish its mode (`Finish`). When
/// `bypass_umask` is set, the umask is temporarily cleared so the directory is made with
/// exactly `mode` (used for an explicit `-m`). Returns `Ok(false)` if the path already exists
/// as a directory (so `-p` can skip it), `Ok(true)` if newly created.
fn make_dir(
    path: &Path,
    mode: libc::mode_t,
    bypass_umask: bool,
    finish: Finish,
) -> io::Result<bool> {
    let bytes = path.as_os_str().as_bytes();
    if bytes.contains(&b'\n') {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            gettext("pathname contains a <newline> character"),
        ));
    }
    let c_path = CString::new(bytes).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;

    let saved = if bypass_umask {
        Some(unsafe { libc::umask(0) })
    } else {
        None
    };
    let ret = unsafe { libc::mkdir(c_path.as_ptr(), mode) };
    let err = io::Error::last_os_error();
    if let Some(prev) = saved {
        unsafe { libc::umask(prev) };
    }

    if ret == 0 {
        finish_mode(&c_path, finish)?;
        Ok(true)
    } else if err.raw_os_error() == Some(libc::EEXIST) && path.is_dir() {
        Ok(false)
    } else {
        Err(err)
    }
}

/// Give the directory `path`, just made, its final mode (`Finish`), as GNU mkdir does, through
/// a descriptor checked to be that directory: one renamed over it in the meantime is not
/// touched. Nothing is changed, and nothing opened, where the mode is already right.
fn finish_mode(path: &CStr, finish: Finish) -> io::Result<()> {
    // Cast needed: `mode_t` is u16 on macOS and u32 on Linux.
    #[allow(clippy::unnecessary_cast)]
    let wanted = |made: u32| match finish {
        Finish::AsMade => made,
        // GNU's `dirchownmod`: where a named bit differs, the whole mode is given, with the
        // bits it does not name as made.
        Finish::Named(mode, bits) if (made ^ mode as u32) & bits != 0 => {
            mode as u32 | (made & !bits)
        }
        Finish::Named(..) => made,
        Finish::OwnerWriteSearch => made | 0o300,
    };
    #[allow(clippy::unnecessary_cast)]
    let made = lstat_at(libc::AT_FDCWD, path)?.st_mode as u32 & 0o7777;
    if wanted(made) == made {
        return Ok(());
    }
    let pin = pin_made_dir(path)?;
    #[allow(clippy::unnecessary_cast)]
    let made = fstat(pin.as_raw_fd())?.st_mode as u32 & 0o7777;
    chmod_fd(pin.as_raw_fd(), wanted(made) as libc::mode_t)
}

/// The directory `path` names, opened without following a symbolic link and for search only,
/// and checked to be the one just made (`plib::madefs::verify_made_dir`): a directory whose
/// owner cannot be verified is refused.
fn pin_made_dir(path: &CStr) -> io::Result<OwnedFd> {
    let flags = SEARCH_ONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC;
    let fd = unsafe { libc::open(path.as_ptr(), flags) };
    if fd < 0 {
        return Err(io::Error::last_os_error());
    }
    let pin = unsafe { OwnedFd::from_raw_fd(fd) };
    match plib::madefs::verify_made_dir(libc::AT_FDCWD, pin.as_raw_fd())? {
        Some(MadeTrust::Full) => Ok(pin),
        Some(MadeTrust::ParentOwnerOnly) => Err(io::Error::other(gettext(
            "not setting its permissions: its owner could not be verified",
        ))),
        None => Err(io::Error::other(gettext(
            "it was replaced after it was made",
        ))),
    }
}

/// The permission bits the mode `-m` gives names, the rest staying as `mkdir` leaves them
/// (gnulib's `mode_adjust`, whose result GNU mkdir applies). An octal mode names every bit but
/// the set-user-ID and set-group-ID bits it leaves clear, unless it has five digits -- which a
/// directory inherits from its parent are kept. A symbolic mode names what each of its
/// clauses changes: `=` every bit of the classes it is for, `+` and `-` the bits they add or
/// remove.
fn named_bits(mode: &ChmodMode, umask: u32) -> u32 {
    match mode {
        ChmodMode::Absolute(mode, digits) if *digits < 5 => (mode & 0o6000) | 0o1777,
        ChmodMode::Absolute(..) => 0o7777,
        ChmodMode::Symbolic(sym) => sym
            .clauses
            .iter()
            .fold(0, |bits, clause| bits | clause_bits(clause, umask)),
    }
}

/// The bits one clause of a symbolic mode names (`named_bits`).
fn clause_bits(clause: &ChmodClause, umask: u32) -> u32 {
    let class = |given, bits| if given { bits } else { 0 };
    let who =
        class(clause.user, 0o4700) | class(clause.group, 0o2070) | class(clause.others, 0o1007);
    // No class given: all of them, the permission bits less the umask.
    let (who, mask) = if who == 0 {
        (0o7777, !umask)
    } else {
        (who, !0)
    };
    let mut bits = 0;
    for action in &clause.actions {
        let copies = action.copy_user || action.copy_group || action.copy_others;
        let perm = if copies {
            7
        } else {
            class(action.read, 4)
                | class(action.write, 2)
                | class(action.execute || action.execute_dir, 1)
        };
        let rwx = (perm << 6) | (perm << 3) | perm;
        bits |= match action.op {
            ChmodActionOp::Set => who & !class(!action.setuid, 0o6000),
            ChmodActionOp::Add | ChmodActionOp::Remove => {
                (rwx & who & 0o777 & mask)
                    | class(action.setuid, who & 0o6000)
                    | class(action.sticky, 0o1000)
            }
        };
    }
    bits
}

fn do_mkdir(
    dirname: &str,
    mode: &ChmodMode,
    parents: bool,
    explicit_mode: bool,
    umask: u32,
) -> io::Result<()> {
    // Cast for macOS, where libc mode constants are u16.
    #[allow(clippy::unnecessary_cast)]
    let leaf_mode = (match mode {
        ChmodMode::Absolute(mode, _) => *mode,
        ChmodMode::Symbolic(sym) => modestr::mutate(0o777, true, sym),
    }) as libc::mode_t;
    // GNU mkdir sets the bits `-m` names once the directory is made.
    let leaf_finish = if explicit_mode {
        Finish::Named(leaf_mode, named_bits(mode, umask))
    } else {
        Finish::AsMade
    };

    if parents {
        // POSIX: intermediate components get the default mode modified by umask, plus write and
        // search permission for the owner, so the descendants can always be created. They are
        // made as a plain `mkdir` makes one, so a default ACL applies to them as it does there.

        let parts: Vec<&str> = dirname.split('/').filter(|p| !p.is_empty()).collect();
        let last = parts.len().saturating_sub(1);
        let mut path = if dirname.starts_with('/') {
            PathBuf::from("/")
        } else {
            PathBuf::new()
        };
        for (i, part) in parts.iter().enumerate() {
            path.push(part);
            if i == last {
                make_dir(&path, leaf_mode, explicit_mode, leaf_finish)?;
            } else {
                make_dir(&path, 0o777, false, Finish::OwnerWriteSearch)?;
            }
        }
        Ok(())
    } else if make_dir(Path::new(dirname), leaf_mode, explicit_mode, leaf_finish)? {
        Ok(())
    } else {
        // Without `-p`, an existing directory is an error.
        Err(io::Error::from_raw_os_error(libc::EEXIST))
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("mkdir");

    let args = plib::optarg::parse::<Args>();

    let mut exit_code = 0;

    // parse the mode string
    let explicit_mode = args.mode.is_some();
    let mode = match args.mode {
        Some(mode) => modestr::parse(&mode)?,
        None => ChmodMode::Absolute(0o777, 3),
    };

    // Read once: each read is a pair of umask(2) calls.
    let umask = modestr::umask();

    // apply the mode to each file
    for dirname in &args.dirs {
        if let Err(e) = do_mkdir(dirname, &mode, args.parents, explicit_mode, umask) {
            exit_code = 1;
            eprintln!("mkdir: {}: {}", dirname, plib::diag::io_error_text(&e));
        }
    }

    std::process::exit(exit_code)
}
