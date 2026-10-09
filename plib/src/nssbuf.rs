//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The buffer protocol shared by the reentrant database lookups
//! `getpwuid_r`, `getpwnam_r`, `getgrgid_r` and `getgrnam_r`.
//!
//! The plain forms (`getpwuid` and friends) return a pointer into one static
//! buffer per process, so a lookup on one thread is overwritten by a lookup on
//! another before the first caller has read it. The test harness runs tests on
//! parallel threads, which is how `pax`'s round-trip test came to read uid 10
//! out of a lookup for uid 1001. The `_r` forms write into a buffer the caller
//! owns; this module owns that buffer, grows it when the entry does not fit,
//! and hands the caller a reference to a filled-in entry to copy out of.

use std::ffi::{c_char, c_int, CStr, OsString};
use std::io;
use std::mem::MaybeUninit;
use std::os::unix::ffi::OsStringExt;
use std::ptr;

/// The size to start at when `sysconf` gives no suggestion.
const DEFAULT_LEN: usize = 1024;

/// The largest buffer worth trying. A group entry holds its whole member
/// list, and a directory-service group can list tens of thousands of members,
/// so this is generous; past it the lookup reports `ERANGE`.
const MAX_LEN: usize = 16 * 1024 * 1024;

/// Run one `get*_r` lookup.
///
/// `size_hint` is the `sysconf` name for the suggested buffer size
/// (`_SC_GETPW_R_SIZE_MAX` or `_SC_GETGR_R_SIZE_MAX`). `call` receives the
/// entry to fill, the buffer and the result pointer, and returns the
/// function's error number. `convert` copies what the caller needs out of the
/// entry, which points into the buffer and dies with it.
///
/// Returns `Ok(None)` when the database has no such entry, which the `_r`
/// functions report as success with a null result. Every other failure -- an
/// unreachable directory service, a corrupt file -- is an `Err`, so a caller
/// that must fail closed can tell it from a clean "no such entry".
pub(crate) fn lookup<E, T>(
    size_hint: c_int,
    mut call: impl FnMut(*mut E, &mut [c_char], *mut *mut E) -> c_int,
    convert: impl FnOnce(&E) -> T,
) -> io::Result<Option<T>> {
    let mut len = initial_len(size_hint);
    loop {
        let mut buf = vec![0 as c_char; len];
        let mut entry = MaybeUninit::<E>::uninit();
        let mut result: *mut E = ptr::null_mut();
        match call(entry.as_mut_ptr(), &mut buf, &mut result) {
            0 if result.is_null() => return Ok(None),
            // SAFETY: on success `result` points at `entry`, which the call
            // filled in, and every pointer inside it points into `buf`; both
            // outlive `convert`.
            0 => return Ok(Some(convert(unsafe { &*result }))),
            libc::EINTR => continue,
            libc::ERANGE if len < MAX_LEN => len = (len * 2).min(MAX_LEN),
            errno => return Err(io::Error::from_raw_os_error(errno)),
        }
    }
}

fn initial_len(size_hint: c_int) -> usize {
    // SAFETY: sysconf has no preconditions.
    let suggested = unsafe { libc::sysconf(size_hint) };
    usize::try_from(suggested)
        .ok()
        .filter(|&n| n > 0)
        .unwrap_or(DEFAULT_LEN)
        .min(MAX_LEN)
}

/// Copy a C string out of a database entry, byte for byte. A null pointer,
/// which some backends leave in fields they do not fill, reads as empty.
///
/// # Safety
/// `p` must be null or point to a NUL-terminated string.
pub(crate) unsafe fn os_string(p: *const c_char) -> OsString {
    if p.is_null() {
        return OsString::new();
    }
    OsString::from_vec(CStr::from_ptr(p).to_bytes().to_vec())
}
