//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use libc::{c_int, c_void, getsid, pid_t, proc_listallpids, proc_pidinfo, proc_pidpath};
use std::ffi::CStr;
use std::fs;
use std::io::Error;
use std::os::unix::fs::MetadataExt;

const PROC_PIDPATHINFO_MAXSIZE: usize = 4096;

pub struct ProcessInfo {
    pub pid: pid_t,
    pub ppid: pid_t,
    pub uid: u32,
    pub gid: u32,
    pub path: String,
    pub tty: Option<String>, // Add TTY field for -a option
    pub sid: pid_t,          // Session ID (SID) for -d option
}

pub fn list_processes() -> Result<Vec<ProcessInfo>, Error> {
    let mut pids: Vec<pid_t> = vec![0; 1024];
    let num_pids = unsafe {
        proc_listallpids(
            pids.as_mut_ptr() as *mut c_void,
            (pids.len() * std::mem::size_of::<pid_t>()) as c_int,
        )
    };

    if num_pids < 0 {
        return Err(Error::last_os_error());
    }

    let mut processes = Vec::new();

    for &pid in &pids[0..num_pids as usize] {
        if pid == 0 {
            continue;
        }
        if let Some(info) = get_process_info(pid) {
            processes.push(info);
        }
    }

    Ok(processes)
}

fn get_process_info(pid: pid_t) -> Option<ProcessInfo> {
    let mut proc_info = std::mem::MaybeUninit::<libc::proc_bsdinfo>::uninit();
    let proc_info_size = std::mem::size_of::<libc::proc_bsdinfo>();
    let res = unsafe {
        proc_pidinfo(
            pid,
            libc::PROC_PIDTBSDINFO,
            0,
            proc_info.as_mut_ptr() as *mut c_void,
            proc_info_size as c_int,
        )
    };

    if res <= 0 {
        return None;
    }

    let proc_info = unsafe { proc_info.assume_init() };

    let mut path_buf = vec![0u8; PROC_PIDPATHINFO_MAXSIZE];
    let path_len = unsafe {
        proc_pidpath(
            pid,
            path_buf.as_mut_ptr() as *mut c_void,
            PROC_PIDPATHINFO_MAXSIZE as u32,
        )
    };

    let path = if path_len > 0 {
        unsafe { CStr::from_ptr(path_buf.as_ptr() as *const i8) }
            .to_string_lossy()
            .into_owned()
    } else {
        String::new()
    };

    // Retrieve the terminal device ID (TTY)
    let tty_dev = proc_info.e_tdev;

    // Map the terminal device ID to the TTY name
    let tty = get_tty_name(tty_dev);

    Some(ProcessInfo {
        pid: proc_info.pbi_pid as pid_t,
        ppid: proc_info.pbi_ppid as pid_t,
        uid: proc_info.pbi_uid,
        gid: proc_info.pbi_gid,
        path,
        tty,                         // Add the terminal (TTY) name
        sid: unsafe { getsid(pid) }, // Add session ID (SID)
    })
}

// Function to map a device ID to a TTY name
fn get_tty_name(tty_dev: u32) -> Option<String> {
    let dev_dir = "/dev/";
    if let Ok(entries) = fs::read_dir(dev_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if let Ok(metadata) = fs::metadata(&path) {
                if metadata.rdev() == tty_dev as u64 {
                    return Some(path.file_name()?.to_string_lossy().into_owned());
                }
            }
        }
    }
    None
}
