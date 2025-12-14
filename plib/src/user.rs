//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::{CStr, CString};

/// User account information from the system password database.
pub struct User {
    pub name: String,
    pub uid: libc::uid_t,
    pub gid: libc::gid_t,
}

impl User {
    /// Returns the user's UID.
    pub fn uid(&self) -> u32 {
        self.uid
    }

    /// Returns the user's primary GID.
    pub fn gid(&self) -> u32 {
        self.gid
    }
}

/// Look up a user by name.
pub fn get_by_name(name: &str) -> Option<User> {
    let name_cstr = CString::new(name).ok()?;

    unsafe {
        let passwd = libc::getpwnam(name_cstr.as_ptr());
        if passwd.is_null() {
            return None;
        }

        let passwd_ref = &*passwd;
        let user_name = CStr::from_ptr(passwd_ref.pw_name)
            .to_string_lossy()
            .to_string();

        Some(User {
            name: user_name,
            uid: passwd_ref.pw_uid,
            gid: passwd_ref.pw_gid,
        })
    }
}

/// Look up a user by UID.
pub fn get_by_uid(uid: u32) -> Option<User> {
    unsafe {
        let passwd = libc::getpwuid(uid);
        if passwd.is_null() {
            return None;
        }

        let passwd_ref = &*passwd;
        let user_name = CStr::from_ptr(passwd_ref.pw_name)
            .to_string_lossy()
            .to_string();

        Some(User {
            name: user_name,
            uid: passwd_ref.pw_uid,
            gid: passwd_ref.pw_gid,
        })
    }
}
