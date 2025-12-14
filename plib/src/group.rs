//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use libc::{endgrent, getgrent, setgrent};
use std::ffi::{CStr, CString};
use std::ptr;

pub struct Group {
    pub name: String,
    pub passwd: String,
    pub gid: libc::gid_t,
    pub members: Vec<String>,
}

impl Group {
    /// Returns the group's GID.
    pub fn gid(&self) -> u32 {
        self.gid
    }

    /// Construct a Group from a raw libc::group pointer.
    ///
    /// # Safety
    /// The pointer must be non-null and point to a valid libc::group struct.
    unsafe fn from_raw(group: *const libc::group) -> Self {
        let group_ref = &*group;
        let name = CStr::from_ptr(group_ref.gr_name)
            .to_string_lossy()
            .to_string();
        let passwd = CStr::from_ptr(group_ref.gr_passwd)
            .to_string_lossy()
            .to_string();

        // Copy group members from null-terminated array of C strings.
        // read_unaligned is necessary on macOS to avoid alignment issues.
        let mut members = Vec::new();
        if !group_ref.gr_mem.is_null() {
            let mut member_ptr_arr = group_ref.gr_mem;
            while !ptr::read_unaligned(member_ptr_arr).is_null() {
                let member_ptr = ptr::read_unaligned(member_ptr_arr);
                let member = CStr::from_ptr(member_ptr).to_string_lossy().to_string();
                members.push(member);
                member_ptr_arr = member_ptr_arr.add(1);
            }
        }

        Group {
            name,
            passwd,
            gid: group_ref.gr_gid,
            members,
        }
    }
}

pub fn load() -> Vec<Group> {
    let mut groups = Vec::new();

    unsafe {
        setgrent();
        let mut groupent = getgrent();

        while !groupent.is_null() {
            groups.push(Group::from_raw(groupent));
            groupent = getgrent();
        }

        endgrent();
    }

    groups
}

/// Look up a group by name.
pub fn get_by_name(name: &str) -> Option<Group> {
    let name_cstr = CString::new(name).ok()?;

    unsafe {
        let group = libc::getgrnam(name_cstr.as_ptr());
        if group.is_null() {
            return None;
        }
        Some(Group::from_raw(group))
    }
}

/// Look up a group by GID.
pub fn get_by_gid(gid: u32) -> Option<Group> {
    unsafe {
        let group = libc::getgrgid(gid);
        if group.is_null() {
            return None;
        }
        Some(Group::from_raw(group))
    }
}
