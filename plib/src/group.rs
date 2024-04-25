//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate libc;
use libc::{endgrent, getgrent, setgrent};
use std::ffi::CStr;
use std::ptr;

pub struct Group {
    pub name: String,
    pub passwd: String,
    pub gid: libc::gid_t,
    pub members: Vec<String>,
}

pub fn load() -> Vec<Group> {
    let mut groups = Vec::new();

    unsafe {
        setgrent(); // Initialize the group entry stream
        let mut groupent = getgrent(); // Get the first group entry

        // Loop through all group entries
        while !groupent.is_null() {
            let group = &*groupent;

            // copy basic group fields: name, passwd, gid
            let name = CStr::from_ptr(group.gr_name).to_string_lossy().to_string();
            let passwd = CStr::from_ptr(group.gr_passwd)
                .to_string_lossy()
                .to_string();
            let gid = group.gr_gid;

            // copy group members, a null-terminated array of C strings
            let mut members = Vec::new();
            if !group.gr_mem.is_null() {
                let mut member_ptr_arr = group.gr_mem;

                // Loop through all group members
                // read_unaligned is necessary on MacOS, possibly
                // other platforms, to avoid alignment issues
                while !ptr::read_unaligned(member_ptr_arr).is_null() {
                    let member_ptr = ptr::read_unaligned(member_ptr_arr);
                    let member = CStr::from_ptr(member_ptr).to_string_lossy().to_string();
                    members.push(member);
                    member_ptr_arr = member_ptr_arr.add(1);
                }
            }

            // Add the group to the returned list of groups
            groups.push(Group {
                name,
                passwd,
                gid,
                members,
            });

            groupent = getgrent(); // Move to the next group entry
        }

        endgrent(); // Close the group entry stream
    }

    groups
}
