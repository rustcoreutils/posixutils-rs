//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use libc::{c_int, c_void, getpgid, getsid, pid_t, proc_listallpids, proc_pidinfo, proc_pidpath};
use std::collections::HashMap;
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
    pub time: u64,           // cumulative CPU time in whole seconds
    pub start_time: u64,     // start time in seconds since the Unix epoch
    pub state: char,         // process state
    pub priority: i32,       // priority
    pub flags: u32,          // process flags
    pub comm: String,        // command name (basename)
    pub args: String,        // full command line
}

pub fn list_processes() -> Result<Vec<ProcessInfo>, Error> {
    // Query the count first (NULL buffer), then allocate — a fixed-size buffer
    // silently truncates the process list on a busy system (audit #P2).
    let count = unsafe { proc_listallpids(std::ptr::null_mut(), 0) };
    if count < 0 {
        return Err(Error::last_os_error());
    }
    // Add slack for processes created between the two calls.
    let cap = count as usize + 64;
    let mut pids: Vec<pid_t> = vec![0; cap];
    let num_pids = unsafe {
        proc_listallpids(
            pids.as_mut_ptr() as *mut c_void,
            (pids.len() * std::mem::size_of::<pid_t>()) as c_int,
        )
    };
    if num_pids < 0 {
        return Err(Error::last_os_error());
    }

    // Build the /dev device-number -> name map once, rather than rescanning
    // /dev for every process (audit #P13).
    let dev_map = build_dev_name_map();

    let mut processes = Vec::new();
    let n = (num_pids as usize).min(pids.len());
    for &pid in &pids[0..n] {
        if pid == 0 {
            continue;
        }
        if let Some(info) = get_process_info(pid, &dev_map) {
            processes.push(info);
        }
    }

    Ok(processes)
}

/// Map device numbers (`rdev`) to their /dev entry names, built once per run.
fn build_dev_name_map() -> HashMap<u64, String> {
    let mut map = HashMap::new();
    if let Ok(entries) = fs::read_dir("/dev") {
        for entry in entries.flatten() {
            if let Ok(metadata) = fs::metadata(entry.path()) {
                map.entry(metadata.rdev())
                    .or_insert_with(|| entry.file_name().to_string_lossy().into_owned());
            }
        }
    }
    map
}

/// Reconstruct a process's full argument vector via `KERN_PROCARGS2`, matching
/// what was passed at exec (audit #P8). The buffer layout is:
///   [argc: i32][exec_path\0][padding \0...][argv0\0 argv1\0 ...][env...]
/// Returns None if the arguments cannot be read (e.g. permission denied).
fn get_process_args(pid: pid_t) -> Option<String> {
    // Size the buffer from KERN_ARGMAX.
    let mut argmax: c_int = 0;
    let mut size = std::mem::size_of::<c_int>() as libc::size_t;
    let mut mib = [libc::CTL_KERN, libc::KERN_ARGMAX];
    let rc = unsafe {
        libc::sysctl(
            mib.as_mut_ptr(),
            mib.len() as libc::c_uint,
            &mut argmax as *mut c_int as *mut c_void,
            &mut size,
            std::ptr::null_mut(),
            0,
        )
    };
    if rc != 0 || argmax <= 0 {
        return None;
    }

    // Fetch the argument area for this pid.
    let mut buf = vec![0u8; argmax as usize];
    let mut size = argmax as libc::size_t;
    let mut mib = [libc::CTL_KERN, libc::KERN_PROCARGS2, pid as c_int];
    let rc = unsafe {
        libc::sysctl(
            mib.as_mut_ptr(),
            mib.len() as libc::c_uint,
            buf.as_mut_ptr() as *mut c_void,
            &mut size,
            std::ptr::null_mut(),
            0,
        )
    };
    if rc != 0 || size < 4 {
        return None;
    }
    buf.truncate(size);

    let argc = i32::from_ne_bytes([buf[0], buf[1], buf[2], buf[3]]);
    if argc <= 0 {
        return None;
    }

    let mut pos = 4;
    // Skip the executable path string and the NUL padding that follows it.
    while pos < buf.len() && buf[pos] != 0 {
        pos += 1;
    }
    while pos < buf.len() && buf[pos] == 0 {
        pos += 1;
    }

    // Collect argc NUL-terminated argument strings.
    let mut args: Vec<String> = Vec::new();
    for _ in 0..argc {
        if pos >= buf.len() {
            break;
        }
        let start = pos;
        while pos < buf.len() && buf[pos] != 0 {
            pos += 1;
        }
        args.push(String::from_utf8_lossy(&buf[start..pos]).into_owned());
        pos += 1; // step over the NUL terminator
    }

    if args.is_empty() {
        None
    } else {
        Some(args.join(" "))
    }
}

fn get_process_info(pid: pid_t, dev_map: &HashMap<u64, String>) -> Option<ProcessInfo> {
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
        // Total CPU time is in nanoseconds; normalize to whole seconds to match
        // the shared formatter's contract (audit #P9).
        let total_time = task_info.pti_total_user + task_info.pti_total_system;
        let time_secs = total_time / 1_000_000_000;
        (vsz_kb, time_secs)
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
    let tty = get_tty_name(tty_dev, dev_map);

    // Reconstruct the full argument vector; fall back to "[comm]" in brackets
    // (matching the Linux backend and POSIX 112547-112549) when unavailable.
    let args = get_process_args(pid).unwrap_or_else(|| {
        if comm.is_empty() {
            String::new()
        } else {
            format!("[{}]", comm)
        }
    });

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
        args,
    })
}

/// Resolve a controlling-terminal device number to a /dev name via the
/// prebuilt map. `NODEV` (no controlling terminal, `e_tdev` == 0 or -1) yields
/// None instead of scanning (audit #P13).
fn get_tty_name(tty_dev: u32, dev_map: &HashMap<u64, String>) -> Option<String> {
    if tty_dev == 0 || tty_dev == u32::MAX {
        return None;
    }
    dev_map.get(&(tty_dev as u64)).cloned()
}
