//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use libc::{c_int, c_void, getpgid, getsid, pid_t, proc_listallpids, proc_pidinfo, proc_pidpath};
use std::ffi::CStr;
use std::fs;
use std::io::Error;
use std::os::unix::fs::MetadataExt;

const PROC_PIDPATHINFO_MAXSIZE: usize = 4096;

pub struct ProcessInfo {
    pub pid: pid_t,
    pub ppid: pid_t,
    pub uid: u32,            // effective UID
    pub gid: u32,            // effective GID
    pub ruid: u32,           // real UID
    pub rgid: u32,           // real GID
    pub pgid: pid_t,         // process group ID
    pub tty: Option<String>, // controlling terminal
    pub sid: pid_t,          // session ID
    pub nice: i32,           // nice value
    pub vsz: u64,            // virtual memory size in KB
    pub time: u64,           // CPU time in clock ticks
    pub start_time: u64,     // start time (seconds since epoch)
    pub state: char,         // process state
    pub priority: i32,       // priority
    pub flags: u32,          // process flags
    pub comm: String,        // command name (basename)
    pub args: String,        // full command line
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
    // Get BSD info for basic process information
    let mut bsd_info = std::mem::MaybeUninit::<libc::proc_bsdinfo>::uninit();
    let bsd_info_size = std::mem::size_of::<libc::proc_bsdinfo>();
    let res = unsafe {
        proc_pidinfo(
            pid,
            libc::PROC_PIDTBSDINFO,
            0,
            bsd_info.as_mut_ptr() as *mut c_void,
            bsd_info_size as c_int,
        )
    };

    if res <= 0 {
        return None;
    }

    let bsd_info = unsafe { bsd_info.assume_init() };

    // Get task info for CPU time and virtual memory
    let mut task_info = std::mem::MaybeUninit::<libc::proc_taskinfo>::uninit();
    let task_info_size = std::mem::size_of::<libc::proc_taskinfo>();
    let task_res = unsafe {
        proc_pidinfo(
            pid,
            libc::PROC_PIDTASKINFO,
            0,
            task_info.as_mut_ptr() as *mut c_void,
            task_info_size as c_int,
        )
    };

    let (vsz, time) = if task_res > 0 {
        let task_info = unsafe { task_info.assume_init() };
        let vsz_kb = task_info.pti_virtual_size / 1024;
        // Total CPU time in nanoseconds, convert to clock ticks (assume 100 ticks/sec)
        let total_time = task_info.pti_total_user + task_info.pti_total_system;
        let time_ticks = total_time / 10_000_000; // nanoseconds to centiseconds
        (vsz_kb, time_ticks)
    } else {
        (0, 0)
    };

    // Get process path
    let mut path_buf = vec![0u8; PROC_PIDPATHINFO_MAXSIZE];
    let path_len = unsafe {
        proc_pidpath(
            pid,
            path_buf.as_mut_ptr() as *mut c_void,
            PROC_PIDPATHINFO_MAXSIZE as u32,
        )
    };

    let full_path = if path_len > 0 {
        unsafe { CStr::from_ptr(path_buf.as_ptr() as *const i8) }
            .to_string_lossy()
            .into_owned()
    } else {
        String::new()
    };

    // Extract command name from path
    let comm = full_path
        .rsplit('/')
        .next()
        .unwrap_or(&full_path)
        .to_string();

    // Retrieve the terminal device ID (TTY)
    let tty_dev = bsd_info.e_tdev;
    let tty = get_tty_name(tty_dev);

    // Get process state character
    let state = match bsd_info.pbi_status {
        2 => 'R', // SRUN
        1 => 'S', // SIDL
        3 => 'S', // SSLEEP
        4 => 'T', // SSTOP
        5 => 'Z', // SZOMB
        _ => 'S', // Default to sleeping
    };

    // Get process group ID and session ID
    let pgid = unsafe { getpgid(pid) };
    let sid = unsafe { getsid(pid) };

    Some(ProcessInfo {
        pid: bsd_info.pbi_pid as pid_t,
        ppid: bsd_info.pbi_ppid as pid_t,
        uid: bsd_info.pbi_uid,
        gid: bsd_info.pbi_gid,
        ruid: bsd_info.pbi_ruid,
        rgid: bsd_info.pbi_rgid,
        pgid,
        tty,
        sid,
        nice: bsd_info.pbi_nice,
        vsz,
        time,
        start_time: bsd_info.pbi_start_tvsec,
        state,
        priority: 0, // Not easily available on macOS, use 0
        flags: bsd_info.pbi_flags,
        comm,
        args: full_path, // Use path as args for now (full args harder to get on macOS)
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
