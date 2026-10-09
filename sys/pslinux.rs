//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs;
use std::fs::read_to_string;
use std::io::Error;
use std::path::Path;

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
    pub rss: u64,            // resident set size in KB
    pub time: u64,           // cumulative CPU time in whole seconds
    pub cpu_ms: u64,         // cumulative CPU time in milliseconds
    pub tpgid: i32,          // foreground process group of its terminal
    pub threads: u32,        // number of threads
    pub locked: bool,        // has pages locked into memory
    pub start_time: u64,     // start time in seconds since the Unix epoch
    pub state: char,         // process state (R, S, D, Z, T, etc.)
    pub priority: i32,       // priority
    pub flags: u32,          // process flags
    pub comm: String,        // command name (basename)
    pub args: String,        // full command line
}

/// Clock ticks per second (`_SC_CLK_TCK`); used to convert /proc clock-tick
/// fields to seconds. Defaults to 100 if the query fails.
fn clock_ticks_per_sec() -> u64 {
    let t = unsafe { libc::sysconf(libc::_SC_CLK_TCK) };
    if t > 0 {
        t as u64
    } else {
        100
    }
}

/// The page size in KB (`_SC_PAGESIZE`); /proc/[pid]/statm counts pages.
fn page_size_kb() -> u64 {
    let size = unsafe { libc::sysconf(libc::_SC_PAGESIZE) };
    if size > 0 {
        size as u64 / 1024
    } else {
        4
    }
}

/// Total physical memory in KB, from the `MemTotal` line of /proc/meminfo;
/// 0 if unavailable.
pub fn total_memory_kb() -> u64 {
    read_to_string("/proc/meminfo")
        .ok()
        .and_then(|info| {
            info.lines()
                .find_map(|line| line.strip_prefix("MemTotal:"))
                .and_then(|rest| rest.split_whitespace().next()?.parse().ok())
        })
        .unwrap_or(0)
}

/// System boot time as seconds since the Unix epoch, read from the `btime`
/// line of /proc/stat. Returns 0 if unavailable (etime then clamps to 0).
fn boot_time_epoch() -> u64 {
    if let Ok(stat) = read_to_string("/proc/stat") {
        for line in stat.lines() {
            if let Some(rest) = line.strip_prefix("btime ") {
                if let Ok(v) = rest.trim().parse::<u64>() {
                    return v;
                }
            }
        }
    }
    0
}

pub fn list_processes() -> Result<Vec<ProcessInfo>, Error> {
    // These are host-wide constants; read them once, not per-process.
    let clk_tck = clock_ticks_per_sec();
    let boot_epoch = boot_time_epoch();
    let page_kb = page_size_kb();

    let mut processes = Vec::new();
    for entry in fs::read_dir("/proc")? {
        let entry = entry?;
        let path = entry.path();
        if let Ok(pid) = entry.file_name().to_str().unwrap_or("").parse::<i32>() {
            if pid > 0 {
                if let Some(info) = get_process_info(pid, &path, clk_tck, boot_epoch, page_kb) {
                    processes.push(info);
                }
            }
        }
    }
    Ok(processes)
}

fn get_process_info(
    pid: i32,
    proc_path: &Path,
    clk_tck: u64,
    boot_epoch: u64,
    page_kb: u64,
) -> Option<ProcessInfo> {
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
    let tpgid: i32 = stat_fields[5].parse().unwrap_or(-1);
    let flags: u32 = stat_fields[6].parse().unwrap_or(0);
    let utime: u64 = stat_fields[11].parse().unwrap_or(0);
    let stime: u64 = stat_fields[12].parse().unwrap_or(0);
    let priority: i32 = stat_fields[15].parse().unwrap_or(0);
    let nice: i32 = stat_fields[16].parse().unwrap_or(0);
    let threads: u32 = stat_fields[17].parse().unwrap_or(1);
    let start_ticks: u64 = stat_fields[19].parse().unwrap_or(0);

    // Normalize to seconds so the shared formatters are unit-agnostic.
    // Total CPU time (utime+stime) is in clock ticks; starttime (field 22) is
    // clock ticks since boot, so add the boot epoch to get an absolute time.
    let time = (utime + stime) / clk_tck;
    let cpu_ms = (utime + stime) * 1000 / clk_tck;
    let start_time = if boot_epoch > 0 {
        boot_epoch + start_ticks / clk_tck
    } else {
        0
    };

    // Extract comm from stat (between parentheses)
    let comm_start = stat.find('(').map(|i| i + 1)?;
    let comm_end = stat.rfind(')')?;
    let comm = stat[comm_start..comm_end].to_string();

    // Parse /proc/[pid]/statm for the virtual and resident sizes
    // Format: size resident shared text lib data dt
    // in pages, which we convert to KB
    let mut statm_pages = statm
        .split_whitespace()
        .map(|s| s.parse::<u64>().unwrap_or(0) * page_kb);
    let vsz = statm_pages.next().unwrap_or(0);
    let rss = statm_pages.next().unwrap_or(0);

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
    let mut locked = false;

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
                // Format: VmLck: <size> kB
                "VmLck:" => locked = parts[1] != "0",
                _ => {}
            }
        }
    }

    // Build args from cmdline (null-separated arguments); an empty one, as
    // a kernel thread or a process part-way through exec or exit has, shows
    // the command name in brackets.
    let args = cmdline.trim_end_matches('\0').replace('\0', " ");
    let args = if args.is_empty() {
        format!("[{}]", comm)
    } else {
        args
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
        rss,
        time,
        cpu_ms,
        tpgid,
        threads,
        locked,
        start_time,
        state,
        priority,
        flags,
        comm,
        args,
    })
}
