//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use crate::platform::{self, endutxent, getutxent, setutxent, utmpxname};
use std::ffi::{CStr, CString};
use std::io;
use std::path::Path;

pub struct Utmpx {
    pub user: String,
    pub id: String,
    pub line: String,
    pub pid: libc::pid_t,
    pub typ: libc::c_short,
    pub timestamp: libc::time_t,
    pub host: String,
}

pub fn ut_type_str(typ: libc::c_short) -> &'static str {
    match typ {
        platform::BOOT_TIME => "BOOT_TIME",
        platform::DEAD_PROCESS => "DEAD_PROCESS",
        platform::EMPTY => "EMPTY",
        platform::INIT_PROCESS => "INIT_PROCESS",
        platform::LOGIN_PROCESS => "LOGIN_PROCESS",
        platform::NEW_TIME => "NEW_TIME",
        platform::OLD_TIME => "OLD_TIME",
        platform::RUN_LVL => "RUN_LVL",
        platform::USER_PROCESS => "USER_PROCESS",

        _ => "(unknown)",
    }
}

// Internal function to load entries after utmpx stream is initialized
fn load_entries() -> Vec<Utmpx> {
    let mut entries = Vec::new();

    unsafe {
        let mut utxent = getutxent(); // Get the first utx entry

        // Loop through all utx entries
        while !utxent.is_null() {
            let utx = &*utxent;

            let user = CStr::from_ptr(utx.ut_user.as_ptr())
                .to_string_lossy()
                .into_owned();
            let id = CStr::from_ptr(utx.ut_id.as_ptr())
                .to_string_lossy()
                .into_owned();
            let line = CStr::from_ptr(utx.ut_line.as_ptr())
                .to_string_lossy()
                .into_owned();
            let pid = utx.ut_pid;
            let typ = utx.ut_type;
            let timestamp = utx.ut_tv.tv_sec;
            let host = CStr::from_ptr(utx.ut_host.as_ptr())
                .to_string_lossy()
                .into_owned();

            // Add the utx to the returned list of entries
            entries.push(Utmpx {
                user,
                id,
                line,
                pid,
                typ,
                timestamp: timestamp as libc::time_t,
                host,
            });

            utxent = getutxent(); // Move to the next utx entry
        }

        endutxent(); // Close the utx entry stream
    }

    entries
}

/// Load utmpx entries from the system default utmpx database.
pub fn load() -> Vec<Utmpx> {
    unsafe {
        setutxent(); // Initialize the utx entry stream
    }
    load_entries()
}

/// Load utmpx entries from a specified file.
///
/// This sets the utmpx database file to the given path before reading entries.
/// Returns an error if the path cannot be converted to a C string.
pub fn load_from_file(path: &Path) -> io::Result<Vec<Utmpx>> {
    let path_str = path
        .to_str()
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "invalid path"))?;
    let c_path =
        CString::new(path_str).map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;

    unsafe {
        // Set the utmpx database file
        // Note: Return value semantics differ between platforms (macOS returns 1 on success,
        // glibc returns 0), so we don't check it. File validity is checked by getutxent.
        utmpxname(c_path.as_ptr());
        setutxent(); // Initialize the utx entry stream for the new file
    }

    Ok(load_entries())
}
