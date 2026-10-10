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
use modestr::ChmodMode;
use plib::madefs::{chmod_fd, fs_owners, fstat, lstat_at, made_by_us, MadeObject, MadeTrust};
use plib::modestr;
use std::ffi::{CStr, CString};
use std::io;
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};

/// mkfifo - make FIFO special files
#[derive(Parser)]
#[command(version, about = gettext("mkfifo - make FIFO special files"))]
struct Args {
    #[arg(short, long, allow_hyphen_values = true, help = gettext("Set the file permission bits of the newly-created FIFO to the specified mode value"))]
    mode: Option<String>,

    #[arg(help = gettext("A pathname of the FIFO special file to be created"))]
    files: Vec<String>,
}

fn do_mkfifo(filename: &str, mode: &ChmodMode, explicit_mode: bool) -> io::Result<()> {
    let mode_val = match mode {
        ChmodMode::Absolute(mode, _) => *mode,
        ChmodMode::Symbolic(sym) => modestr::mutate(0o666, false, sym),
    };

    // Reject a <newline> in the name (FUTURE DIRECTIONS); build a NUL-terminated path for libc
    // (a Rust &str is not NUL-terminated, so passing `as_ptr()` directly was undefined behavior).
    if filename.as_bytes().contains(&b'\n') {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            gettext("pathname contains a <newline> character"),
        ));
    }
    let c_path =
        CString::new(filename).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;

    // When mode is explicitly specified with -m, bypass umask per POSIX spec
    // When no mode is specified, let umask apply normally
    let old_umask = if explicit_mode {
        Some(unsafe { libc::umask(0) })
    } else {
        None
    };

    let res = unsafe { libc::mkfifo(c_path.as_ptr(), mode_val as libc::mode_t) };

    // Restore the original umask if we changed it
    if let Some(umask) = old_umask {
        unsafe { libc::umask(umask) };
    }

    if res < 0 {
        return Err(io::Error::last_os_error());
    }

    // GNU mkfifo then sets the mode `-m` gives: `mkfifo` may take no set-user-ID or
    // set-group-ID bit, and under a default ACL its mode only masks the ACL the FIFO inherits.
    if explicit_mode {
        // Cast for macOS, where `mode_t` is u16.
        #[allow(clippy::unnecessary_cast)]
        set_made_mode(&c_path, mode_val as libc::mode_t)?;
    }
    Ok(())
}

/// Give the FIFO `path`, just made, the mode `mode`, through a descriptor checked to be that
/// FIFO: a file put in its place meanwhile is not touched. Nothing is opened where the mode is
/// already right.
fn set_made_mode(path: &CStr, mode: libc::mode_t) -> io::Result<()> {
    let st = lstat_at(libc::AT_FDCWD, path)?;
    if st.st_mode & 0o7777 == mode & 0o7777 {
        return Ok(());
    }
    let pin = pin_made_fifo(path, &st)?;
    chmod_fd(pin.as_raw_fd(), mode)
}

/// The FIFO `path` names, opened without following a symbolic link and without waiting for a
/// writer (`O_PATH` on Linux, `O_NONBLOCK` for reading elsewhere), and checked to be the one
/// `st` saw, a FIFO this user owns with one link: one just made (`made_by_us`).
fn pin_made_fifo(path: &CStr, st: &libc::stat) -> io::Result<OwnedFd> {
    let replaced = || io::Error::other(gettext("it was replaced after it was made"));
    // Off Linux the open is a real one: nothing but a FIFO is opened -- not a device put in
    // its place, whose open could act on it -- and never as a controlling terminal.
    if st.st_mode & libc::S_IFMT != libc::S_IFIFO {
        return Err(replaced());
    }
    #[cfg(target_os = "linux")]
    let flags = libc::O_PATH | libc::O_NOFOLLOW | libc::O_CLOEXEC;
    #[cfg(not(target_os = "linux"))]
    let flags =
        libc::O_RDONLY | libc::O_NONBLOCK | libc::O_NOFOLLOW | libc::O_NOCTTY | libc::O_CLOEXEC;
    let fd = unsafe { libc::open(path.as_ptr(), flags) };
    if fd < 0 {
        return Err(io::Error::last_os_error());
    }
    let pin = unsafe { OwnedFd::from_raw_fd(fd) };
    let held = fstat(pin.as_raw_fd())?;
    let made = MadeObject {
        uid: held.st_uid,
        // Cast needed: `nlink_t` is u16 on macOS and u64 on Linux.
        #[allow(clippy::unnecessary_cast)]
        nlink: held.st_nlink as u64,
        is_dir: false,
        owners: fs_owners(pin.as_raw_fd()),
    };
    let same = (held.st_dev, held.st_ino) == (st.st_dev, st.st_ino)
        && held.st_mode & libc::S_IFMT == libc::S_IFIFO;
    match made_by_us(made, None, unsafe { libc::geteuid() }) {
        Some(MadeTrust::Full) if same => Ok(pin),
        _ => Err(replaced()),
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    plib::diag::init_locale("mkfifo");

    let args = plib::optarg::parse::<Args>();

    let mut exit_code = 0;

    // parse the mode string
    let explicit_mode = args.mode.is_some();
    let mode = match args.mode {
        Some(mode) => modestr::parse(&mode)?,
        None => ChmodMode::Absolute(0o666, 3),
    };

    // apply the mode to each file
    for filename in &args.files {
        if let Err(e) = do_mkfifo(filename, &mode, explicit_mode) {
            exit_code = 1;
            eprintln!("mkfifo: {}: {}", filename, plib::diag::io_error_text(&e));
        }
    }

    std::process::exit(exit_code)
}
