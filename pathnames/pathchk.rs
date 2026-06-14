//
// Copyright (c) 2024-2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::{CStr, CString, OsString};
use std::io;
use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};

use clap::Parser;
use errno::{errno, set_errno, Errno};
use gettextrs::gettext;
use plib::diag;

// Minimum values from XBD <limits.h> (Minimum Values).
const POSIX_PATH_MAX: usize = 256;
const POSIX_NAME_MAX: usize = 14;

#[derive(Parser)]
#[command(version, about = gettext("pathchk - check pathnames"))]
struct Args {
    #[arg(
        short,
        long,
        help = gettext(
            "Instead of performing checks based on the underlying file system, \
             perform portable, POSIX-compliant checks"
        )
    )]
    portable: bool,

    #[arg(
        short = 'P',
        help = gettext(
            "Write a diagnostic for each pathname that contains a component \
             beginning with '-' or is empty"
        )
    )]
    basic: bool,

    #[arg(help = gettext("The pathnames to be checked"))]
    pathnames: Vec<OsString>,
}

/// Iterate the non-empty components of a pathname (operating on raw bytes,
/// so that "." / ".." and encoding are preserved verbatim).
fn components(pathname: &[u8]) -> impl Iterator<Item = &[u8]> {
    pathname.split(|&b| b == b'/').filter(|s| !s.is_empty())
}

/// A byte is in the POSIX portable filename character set if it is an ASCII
/// alphanumeric or one of period, underscore, or hyphen-minus.
fn is_portable_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || matches!(b, b'.' | b'_' | b'-')
}

/// `-P` checks: empty pathname, or any component whose first character is a
/// <hyphen-minus>.
fn check_path_basic(pathname: &[u8]) -> Result<(), String> {
    if pathname.is_empty() {
        return Err(gettext("empty pathname"));
    }

    for comp in components(pathname) {
        if comp.first() == Some(&b'-') {
            return Err(gettext("a component begins with '-'"));
        }
    }

    Ok(())
}

/// `None` for a limit means the filesystem reports it as indeterminate
/// (`pathconf` returned -1 with errno unchanged); that length check is skipped.
fn check_path_limits(
    pathname: &[u8],
    max_path: Option<usize>,
    max_name: Option<usize>,
    portable_charset: bool,
) -> Result<(), String> {
    if let Some(max_path) = max_path {
        if pathname.len() > max_path {
            return Err(gettext("pathname too long"));
        }
    }

    for comp in components(pathname) {
        if let Some(max_name) = max_name {
            if comp.len() > max_name {
                return Err(gettext("component too long"));
            }
        }
        if portable_charset && !comp.iter().all(|&b| is_portable_byte(b)) {
            return Err(gettext("component contains non-portable characters"));
        }
    }

    Ok(())
}

/// Query a `pathconf` limit. Returns `Ok(Some(n))` for a defined limit,
/// `Ok(None)` when the limit is indeterminate (the call returns -1 but leaves
/// errno unchanged), and `Err` on a genuine error (-1 with errno set).
fn query_pathconf(c: &CStr, name: libc::c_int) -> Result<Option<usize>, String> {
    set_errno(Errno(0));
    // SAFETY: `c` is a valid NUL-terminated C string for the lifetime of the call.
    let value = unsafe { libc::pathconf(c.as_ptr(), name) };
    if value >= 0 {
        return Ok(Some(value as usize));
    }
    let err = errno();
    if err.0 == 0 {
        Ok(None) // indeterminate / no limit
    } else {
        Err(format!(
            "{}: {}",
            gettext("cannot query path limits"),
            io::Error::from(err)
        ))
    }
}

/// Find the deepest existing ancestor of `pathname` (the path itself if it
/// exists). Never returns an empty path: an empty parent resolves to the
/// current directory.
fn find_fshandle(pathname: &[u8]) -> PathBuf {
    let mut cur: &Path = Path::new(std::ffi::OsStr::from_bytes(pathname));

    loop {
        if cur.exists() {
            return if cur.as_os_str().is_empty() {
                PathBuf::from(".")
            } else {
                cur.to_path_buf()
            };
        }
        match cur.parent() {
            Some(parent) if !parent.as_os_str().is_empty() => cur = parent,
            _ => return PathBuf::from("."),
        }
    }
}

/// Verify that an existing directory has search (execute) permission for the
/// calling process. Best-effort: only the deepest existing directory in the
/// path is checked.
fn check_searchable(fsh: &Path) -> Result<(), String> {
    if !fsh.is_dir() {
        return Ok(());
    }

    let c = CString::new(fsh.as_os_str().as_bytes())
        .map_err(|_| gettext("pathname contains a NUL byte"))?;

    // SAFETY: `c` is a valid NUL-terminated C string for the lifetime of the call.
    let rc = unsafe { libc::access(c.as_ptr(), libc::X_OK) };
    if rc != 0 && io::Error::last_os_error().raw_os_error() == Some(libc::EACCES) {
        return Err(gettext("directory is not searchable"));
    }

    Ok(())
}

fn check_path_posix(pathname: &[u8]) -> Result<(), String> {
    check_path_limits(pathname, Some(POSIX_PATH_MAX), Some(POSIX_NAME_MAX), true)
}

fn check_path_fs(pathname: &[u8]) -> Result<(), String> {
    let fsh = find_fshandle(pathname);
    let c = CString::new(fsh.as_os_str().as_bytes())
        .map_err(|_| gettext("pathname contains a NUL byte"))?;

    let path_max = query_pathconf(&c, libc::_PC_PATH_MAX)?;
    let name_max = query_pathconf(&c, libc::_PC_NAME_MAX)?;

    check_path_limits(pathname, path_max, name_max, false)?;
    check_searchable(&fsh)
}

fn check_path(args: &Args, pathname: &[u8]) -> Result<(), String> {
    // -p replaces the file-system checks with portable checks; otherwise the
    // default file-system checks apply.
    if args.portable {
        check_path_posix(pathname)?;
    } else {
        check_path_fs(pathname)?;
    }

    // -P is additive: it applies on top of whichever check ran above.
    if args.basic {
        check_path_basic(pathname)?;
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    diag::init_locale("pathchk");

    let args = Args::parse();

    for pathname in &args.pathnames {
        if let Err(msg) = check_path(&args, pathname.as_bytes()) {
            diag::error(&format!("{}: {}", pathname.to_string_lossy(), msg));
        }
    }

    std::process::exit(diag::exit_status())
}
