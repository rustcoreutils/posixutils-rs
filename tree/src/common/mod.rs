// This module is shared between `cp`, `mv` and `rm` but is considered as three
// separated modules due to the project structure. The `#![allow(unused)]` is
// to remove warnings when, say, `rm` doesn't use all the the functions in this
// module (but is used in `cp` or `mv`).
#![allow(unused)]

use gettextrs::gettext;
use std::ffi::{CStr, CString};
use std::os::unix::{ffi::OsStrExt, fs::MetadataExt};
use std::path::Path;
use std::{fs, io};

/// Return the error message.
///
/// This is for compatibility with coreutils mv. `format!("{e}")` will append
/// the error code after the error message which we do not want.
pub fn error_string(e: &io::Error) -> String {
    let s = match e.raw_os_error() {
        // Like `format!("{e}")` except without the error code.
        //
        // `std` doesn't expose `sys::os::error_string` so this was copied from:
        // https://github.com/rust-lang/rust/blob/72f616273cbbacc06918ef50470d052d39d9b514/library/std/src/sys/pal/unix/os.rs#L124-L149
        Some(errno) => {
            let mut buf = [0; 128];

            unsafe {
                if libc::strerror_r(errno as _, buf.as_mut_ptr(), buf.len()) == 0 {
                    String::from_utf8_lossy(CStr::from_ptr(buf.as_ptr()).to_bytes()).to_string()
                } else {
                    // `std` just panics here
                    String::from("Unknown error")
                }
            }
        }
        None => format!("{e}"),
    };

    // Translate the error string
    gettext(s)
}

/// Copy the metadata in `source_md` to `target`.
///
/// This copies the last access time, last modification time, user ownership,
/// group ownership, and permissions.
pub fn copy_characteristics(source_md: &fs::Metadata, target: &Path) -> io::Result<()> {
    let target_cstr = CString::new(target.as_os_str().as_bytes())?;

    // [last_access_time, last_modified_time]
    let times = [
        libc::timespec {
            tv_sec: source_md.atime(),
            tv_nsec: source_md.atime_nsec(),
        },
        libc::timespec {
            tv_sec: source_md.mtime(),
            tv_nsec: source_md.mtime_nsec(),
        },
    ];

    unsafe {
        // Copy last access and last modified times
        let ret = libc::utimensat(
            libc::AT_FDCWD, // Relative to the cwd of the process
            target_cstr.as_ptr(),
            times.as_ptr(),
            libc::AT_SYMLINK_NOFOLLOW, // Update the file itself if a symlink
        );
        if ret != 0 {
            return Err(io::Error::last_os_error());
        }

        // Copy user and group
        let ret = libc::chown(target_cstr.as_ptr(), source_md.uid(), source_md.gid());
        if ret != 0 {
            return Err(io::Error::last_os_error());
        }

        // Copy permissions
        let ret = libc::chmod(target_cstr.as_ptr(), source_md.mode() as libc::mode_t);
        if ret != 0 {
            return Err(io::Error::last_os_error());
        }
    }
    Ok(())
}

/// Check if the file is writable for the current process.
pub fn is_file_writable(md: Option<&fs::Metadata>) -> bool {
    match md {
        Some(md) => {
            // These are "effective" IDs and not "real" to allow for things like
            // sudo
            let uid = unsafe { libc::geteuid() };
            let gid = unsafe { libc::getegid() };

            let same_user = md.uid() == uid;
            let same_group = md.gid() == gid;

            // `libc::mode_t` is not the same for all platforms while
            // `unix::fs::MetadataExt::mode` is always a `u32`.
            let mode = md.mode() as libc::mode_t;

            if same_user && (mode & libc::S_IWUSR != 0) {
                true
            } else if same_group && (mode & libc::S_IWGRP != 0) {
                true
            } else {
                mode & libc::S_IWOTH != 0
            }
        }
        None => false,
    }
}
