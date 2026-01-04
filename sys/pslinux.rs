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
    pub uid: u32,            // effective UID
    pub gid: u32,            // effective GID
    pub ruid: u32,           // real UID
    pub rgid: u32,           // real GID
    pub pgid: i32,           // process group ID
    pub tty: Option<String>, // controlling terminal
    pub sid: i32,            // session ID
    pub nice: i32,           // nice value
    pub vsz: u64,            // virtual memory size in KB
    pub time: u64,           // CPU time in clock ticks
    pub start_time: u64,     // start time (clock ticks since boot)
    pub state: char,         // process state (R, S, D, Z, T, etc.)
    pub priority: i32,       // priority
    pub flags: u32,          // process flags
    pub comm: String,        // command name (basename)
    pub args: String,        // full command line
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
    let statm_path = proc_path.join("statm");

    let status = read_to_string(&status_path).ok()?;
    let cmdline = read_to_string(&cmdline_path).unwrap_or_default();
    let stat = read_to_string(&stat_path).ok()?;
    let statm = read_to_string(&statm_path).unwrap_or_default();

    // Parse /proc/[pid]/stat
    // Format: pid (comm) state ppid pgrp session tty_nr tpgid flags minflt cminflt majflt cmajflt
    //         utime stime cutime cstime priority nice num_threads itrealvalue starttime vsize rss ...
    // The comm field can contain spaces and parentheses, so we need to parse carefully
    let stat_after_comm = stat.rfind(')').map(|i| &stat[i + 2..])?;
    let stat_fields: Vec<&str> = stat_after_comm.split_whitespace().collect();

    if stat_fields.len() < 20 {
        return None;
    }

    let state = stat_fields[0].chars().next().unwrap_or('?');
    let ppid: i32 = stat_fields[1].parse().unwrap_or(0);
    let pgid: i32 = stat_fields[2].parse().unwrap_or(0);
    let sid: i32 = stat_fields[3].parse().unwrap_or(0);
    let tty_nr: i32 = stat_fields[4].parse().unwrap_or(0);
    let flags: u32 = stat_fields[6].parse().unwrap_or(0);
    let utime: u64 = stat_fields[11].parse().unwrap_or(0);
    let stime: u64 = stat_fields[12].parse().unwrap_or(0);
    let priority: i32 = stat_fields[15].parse().unwrap_or(0);
    let nice: i32 = stat_fields[16].parse().unwrap_or(0);
    let start_time: u64 = stat_fields[19].parse().unwrap_or(0);

    let time = utime + stime; // Total CPU time in clock ticks

    // Extract comm from stat (between parentheses)
    let comm_start = stat.find('(').map(|i| i + 1)?;
    let comm_end = stat.rfind(')')?;
    let comm = stat[comm_start..comm_end].to_string();

    // Parse /proc/[pid]/statm for virtual memory size
    // Format: size resident shared text lib data dt
    // size is in pages, we convert to KB
    let vsz: u64 = statm
        .split_whitespace()
        .next()
        .and_then(|s| s.parse::<u64>().ok())
        .map(|pages| pages * 4) // Assume 4KB pages, convert to KB
        .unwrap_or(0);

    // Parse TTY device number
    let tty = if tty_nr > 0 {
        // Major/minor device number encoding
        let major = (tty_nr >> 8) & 0xff;
        let minor = tty_nr & 0xff;
        match major {
            4 => Some(format!("tty{}", minor)),          // Virtual console
            136..=143 => Some(format!("pts/{}", minor)), // Pseudo-terminal
            _ => Some(format!("tty{}", tty_nr)),
        }
    } else {
        None
    };

    // Parse /proc/[pid]/status for UID/GID info
    let mut uid: u32 = 0;
    let mut gid: u32 = 0;
    let mut ruid: u32 = 0;
    let mut rgid: u32 = 0;

    for line in status.lines() {
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() >= 2 {
            match parts[0] {
                "Uid:" => {
                    // Format: Uid: real effective saved fs
                    ruid = parts.get(1).and_then(|s| s.parse().ok()).unwrap_or(0);
                    uid = parts.get(2).and_then(|s| s.parse().ok()).unwrap_or(ruid);
                }
                "Gid:" => {
                    // Format: Gid: real effective saved fs
                    rgid = parts.get(1).and_then(|s| s.parse().ok()).unwrap_or(0);
                    gid = parts.get(2).and_then(|s| s.parse().ok()).unwrap_or(rgid);
                }
                _ => {}
            }
        }
    }

    // Build args from cmdline (null-separated arguments)
    let args = if !cmdline.is_empty() {
        cmdline.trim_end_matches('\0').replace('\0', " ")
    } else {
        format!("[{}]", comm)
    };

    Some(ProcessInfo {
        pid,
        ppid,
        uid,
        gid,
        ruid,
        rgid,
        pgid,
        tty,
        sid,
        nice,
        vsz,
        time,
        start_time,
        state,
        priority,
        flags,
        comm,
        args,
    })
}
