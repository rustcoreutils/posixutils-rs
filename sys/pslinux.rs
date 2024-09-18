//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs;
use std::fs::read_to_string;
use std::io::Error;
use std::path::{Path, PathBuf};

pub struct ProcessInfo {
    pub pid: i32,
    pub ppid: i32,
    pub uid: u32,
    pub gid: u32,
    pub path: String,
    pub tty: Option<String>, // Add TTY field for -a option
    pub sid: i32,            // Add session ID (SID) for -d option
}

pub fn list_processes() -> Result<Vec<ProcessInfo>, Error> {
    let mut processes = Vec::new();
    for entry in fs::read_dir("/proc")? {
        let entry = entry?;
        let path = entry.path();
        if let Ok(pid) = entry.file_name().to_str().unwrap_or("").parse::<i32>() {
            if pid > 0 {
                if let Some(info) = get_process_info(pid, &path) {
                    processes.push(info);
                }
            }
        }
    }
    Ok(processes)
}

fn get_process_info(pid: i32, proc_path: &Path) -> Option<ProcessInfo> {
    let status_path = proc_path.join("status");
    let cmdline_path = proc_path.join("cmdline");
    let stat_path = proc_path.join("stat");
    let exe_path = proc_path.join("exe");

    let status = read_to_string(status_path).ok()?;
    let cmdline = read_to_string(cmdline_path).unwrap_or_default();
    let exe = fs::read_link(exe_path).unwrap_or_else(|_| PathBuf::from("[Permission denied]"));
    let comm = String::new();

    // Read from /proc/<pid>/stat to get the session ID and TTY number
    let stat = read_to_string(stat_path).ok()?;
    let stat_fields: Vec<&str> = stat.split_whitespace().collect();
    let sid = stat_fields[5].parse().unwrap_or(0); // Session ID (SID)
    let tty_nr = stat_fields[6].parse::<i32>().unwrap_or(0);

    let tty = if tty_nr > 0 {
        Some(format!("tty{}", tty_nr)) // Simplified TTY representation
    } else {
        None
    };

    let mut ppid = 0;
    let mut uid = 0;
    let mut gid = 0;

    for line in status.lines() {
        if line.starts_with("PPid:") {
            if let Some(val) = line.split_whitespace().nth(1) {
                ppid = val.parse().unwrap_or(0);
            }
        } else if line.starts_with("Uid:") {
            if let Some(val) = line.split_whitespace().nth(1) {
                uid = val.parse().unwrap_or(0);
            }
        } else if line.starts_with("Gid:") {
            if let Some(val) = line.split_whitespace().nth(1) {
                gid = val.parse().unwrap_or(0);
            }
        }
    }

    let path = if !cmdline.is_empty() {
        cmdline.replace('\0', " ")
    } else if !comm.is_empty() {
        format!("[{}]", comm)
    } else {
        exe.to_string_lossy().to_string()
    };

    Some(ProcessInfo {
        pid,
        ppid,
        uid,
        gid,
        path,
        tty,
        sid, // Return the session ID (SID)
    })
}
