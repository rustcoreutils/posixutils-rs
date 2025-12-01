//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use chrono::{Local, TimeZone};
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use std::ffi::CStr;

#[cfg(target_os = "linux")]
use std::io::{self, BufRead};

#[derive(Parser)]
#[command(
    version,
    about = gettext("ipcs - report XSI interprocess communication facilities status")
)]
struct Args {
    #[arg(short = 'q', long = "queues", help = gettext("Write information about active message queues"))]
    message_queues: bool,

    #[arg(short = 'm', long = "shmems", help = gettext("Write information about active shared memory segments"))]
    shared_memory: bool,

    #[arg(short, long, help = gettext("Write information about active semaphore sets"))]
    semaphores: bool,

    #[arg(short, long, help = gettext("Use all print options (-b, -c, -o, -p, -t)"))]
    all: bool,

    #[arg(short = 'b', long = "max-size", help = gettext("Write information on maximum allowable size"))]
    max_size: bool,

    #[arg(short, long, help = gettext("Write creator's user name and group name"))]
    creator: bool,

    #[arg(short, long, help = gettext("Write information on outstanding usage"))]
    outstanding: bool,

    #[arg(short, long, help = gettext("Write process number information"))]
    pid: bool,

    #[arg(short, long, help = gettext("Write time information"))]
    time: bool,
}

fn enable_all_options(args: &mut Args) {
    args.max_size = true;
    args.creator = true;
    args.outstanding = true;
    args.pid = true;
    args.time = true;
}

fn enable_all_facilities(args: &mut Args) {
    args.message_queues = true;
    args.shared_memory = true;
    args.semaphores = true;
}

/// Get username from UID, returning UID as string if not found
fn get_username(uid: u32) -> String {
    unsafe {
        let pw = libc::getpwuid(uid);
        if pw.is_null() {
            uid.to_string()
        } else {
            CStr::from_ptr((*pw).pw_name)
                .to_str()
                .unwrap_or(&uid.to_string())
                .to_string()
        }
    }
}

/// Get group name from GID, returning GID as string if not found
fn get_groupname(gid: u32) -> String {
    unsafe {
        let gr = libc::getgrgid(gid);
        if gr.is_null() {
            gid.to_string()
        } else {
            CStr::from_ptr((*gr).gr_name)
                .to_str()
                .unwrap_or(&gid.to_string())
                .to_string()
        }
    }
}

/// Truncate a string to at most `max_chars` characters (not bytes).
/// This is safe for UTF-8 strings with multi-byte characters.
fn truncate_to_chars(s: &str, max_chars: usize) -> &str {
    match s.char_indices().nth(max_chars) {
        Some((idx, _)) => &s[..idx],
        None => s,
    }
}

/// Format permission mode as 11-character string per POSIX
/// Format: [S-][RC-][rwx][rwx][rwx][ +]
/// - First char: S if process waiting on msgsnd, else -
/// - Second char: R if process waiting on msgrcv, C if shm cleared on attach, else -
/// - Chars 3-11: owner/group/other permissions (r/w/a for read/write/alter)
/// - Char 12: space or + for additional access control
fn format_mode(mode: u16, facility: char, _waiting_send: bool, _waiting_recv: bool) -> String {
    // First two special characters
    let c1 = '-'; // We don't have waiting info in current implementation
    let c2 = match facility {
        'm' => '-', // Could be 'C' for SHM_DEST
        _ => '-',
    };

    // Permission bits
    let owner_r = if mode & 0o400 != 0 { 'r' } else { '-' };
    let owner_w = if mode & 0o200 != 0 { 'w' } else { '-' };
    let owner_a = if mode & 0o100 != 0 { 'a' } else { '-' };

    let group_r = if mode & 0o040 != 0 { 'r' } else { '-' };
    let group_w = if mode & 0o020 != 0 { 'w' } else { '-' };
    let group_a = if mode & 0o010 != 0 { 'a' } else { '-' };

    let other_r = if mode & 0o004 != 0 { 'r' } else { '-' };
    let other_w = if mode & 0o002 != 0 { 'w' } else { '-' };
    let other_a = if mode & 0o001 != 0 { 'a' } else { '-' };

    format!(
        "{}{}{}{}{}{}{}{}{}{}{}",
        c1, c2, owner_r, owner_w, owner_a, group_r, group_w, group_a, other_r, other_w, other_a
    )
}

/// Format time as HH:MM:SS or " no-entry" per POSIX
fn format_time(timestamp: i64) -> String {
    if timestamp == 0 {
        " no-entry".to_string()
    } else {
        match Local.timestamp_opt(timestamp, 0).single() {
            Some(dt) => dt.format("%H:%M:%S").to_string(),
            None => " no-entry".to_string(), // Invalid or ambiguous timestamp
        }
    }
}

/// Get current date in POSIX locale format (matching `date` command)
fn get_current_date() -> String {
    let now = Local::now();
    now.format("%a %b %e %H:%M:%S %Z %Y").to_string()
}

// ============================================================================
// Linux-specific IPC enumeration via /proc/sysvipc
// ============================================================================

#[cfg(target_os = "linux")]
fn read_proc_msg() -> io::Result<Vec<MsgQueueInfo>> {
    let mut entries = Vec::new();
    let file = std::fs::File::open("/proc/sysvipc/msg")?;
    let reader = io::BufReader::new(file);

    for (i, line) in reader.lines().enumerate() {
        if i == 0 {
            continue;
        } // Skip header
        let line = line?;
        let fields: Vec<&str> = line.split_whitespace().collect();
        if fields.len() >= 14 {
            entries.push(MsgQueueInfo {
                key: i32::from_str_radix(fields[0], 10).unwrap_or(0),
                msqid: fields[1].parse().unwrap_or(0),
                perms: fields[2].parse().unwrap_or(0),
                cbytes: fields[3].parse().unwrap_or(0),
                qnum: fields[4].parse().unwrap_or(0),
                lspid: fields[5].parse().unwrap_or(0),
                lrpid: fields[6].parse().unwrap_or(0),
                uid: fields[7].parse().unwrap_or(0),
                gid: fields[8].parse().unwrap_or(0),
                cuid: fields[9].parse().unwrap_or(0),
                cgid: fields[10].parse().unwrap_or(0),
                stime: fields[11].parse().unwrap_or(0),
                rtime: fields[12].parse().unwrap_or(0),
                ctime: fields[13].parse().unwrap_or(0),
            });
        }
    }
    Ok(entries)
}

#[cfg(target_os = "linux")]
fn read_proc_shm() -> io::Result<Vec<ShmInfo>> {
    let mut entries = Vec::new();
    let file = std::fs::File::open("/proc/sysvipc/shm")?;
    let reader = io::BufReader::new(file);

    for (i, line) in reader.lines().enumerate() {
        if i == 0 {
            continue;
        } // Skip header
        let line = line?;
        let fields: Vec<&str> = line.split_whitespace().collect();
        if fields.len() >= 14 {
            entries.push(ShmInfo {
                key: i32::from_str_radix(fields[0], 10).unwrap_or(0),
                shmid: fields[1].parse().unwrap_or(0),
                perms: fields[2].parse().unwrap_or(0),
                size: fields[3].parse().unwrap_or(0),
                cpid: fields[4].parse().unwrap_or(0),
                lpid: fields[5].parse().unwrap_or(0),
                nattch: fields[6].parse().unwrap_or(0),
                uid: fields[7].parse().unwrap_or(0),
                gid: fields[8].parse().unwrap_or(0),
                cuid: fields[9].parse().unwrap_or(0),
                cgid: fields[10].parse().unwrap_or(0),
                atime: fields[11].parse().unwrap_or(0),
                dtime: fields[12].parse().unwrap_or(0),
                ctime: fields[13].parse().unwrap_or(0),
            });
        }
    }
    Ok(entries)
}

#[cfg(target_os = "linux")]
fn read_proc_sem() -> io::Result<Vec<SemInfo>> {
    let mut entries = Vec::new();
    let file = std::fs::File::open("/proc/sysvipc/sem")?;
    let reader = io::BufReader::new(file);

    for (i, line) in reader.lines().enumerate() {
        if i == 0 {
            continue;
        } // Skip header
        let line = line?;
        let fields: Vec<&str> = line.split_whitespace().collect();
        if fields.len() >= 10 {
            entries.push(SemInfo {
                key: i32::from_str_radix(fields[0], 10).unwrap_or(0),
                semid: fields[1].parse().unwrap_or(0),
                perms: fields[2].parse().unwrap_or(0),
                nsems: fields[3].parse().unwrap_or(0),
                uid: fields[4].parse().unwrap_or(0),
                gid: fields[5].parse().unwrap_or(0),
                cuid: fields[6].parse().unwrap_or(0),
                cgid: fields[7].parse().unwrap_or(0),
                otime: fields[8].parse().unwrap_or(0),
                ctime: fields[9].parse().unwrap_or(0),
            });
        }
    }
    Ok(entries)
}

// ============================================================================
// macOS-specific IPC enumeration
// ============================================================================

// Maximum IPC ID to iterate through on macOS.
// macOS doesn't provide /proc/sysvipc like Linux, so we must iterate.
// 32768 is a reasonable upper bound that balances coverage vs performance.
#[cfg(target_os = "macos")]
const MACOS_MAX_IPC_ID: i32 = 32768;

#[cfg(target_os = "macos")]
fn read_macos_shm() -> Vec<ShmInfo> {
    use libc::{shmctl, shmid_ds, IPC_STAT};

    let mut entries = Vec::new();
    let mut shmbuf: shmid_ds = unsafe { std::mem::zeroed() };

    for shmid in 0..MACOS_MAX_IPC_ID {
        if unsafe { shmctl(shmid, IPC_STAT, &mut shmbuf) } == 0 {
            entries.push(ShmInfo {
                key: shmbuf.shm_perm._key,
                shmid,
                perms: shmbuf.shm_perm.mode,
                size: shmbuf.shm_segsz as u64,
                cpid: shmbuf.shm_cpid,
                lpid: shmbuf.shm_lpid,
                nattch: shmbuf.shm_nattch as u32,
                uid: shmbuf.shm_perm.uid,
                gid: shmbuf.shm_perm.gid,
                cuid: shmbuf.shm_perm.cuid,
                cgid: shmbuf.shm_perm.cgid,
                atime: shmbuf.shm_atime,
                dtime: shmbuf.shm_dtime,
                ctime: shmbuf.shm_ctime,
            });
        }
    }
    entries
}

#[cfg(target_os = "macos")]
fn read_macos_sem() -> Vec<SemInfo> {
    use libc::{semctl, semid_ds, IPC_STAT};

    let mut entries = Vec::new();
    let mut sembuf: semid_ds = unsafe { std::mem::zeroed() };

    for semid in 0..MACOS_MAX_IPC_ID {
        if unsafe { semctl(semid, 0, IPC_STAT, &mut sembuf) } == 0 {
            entries.push(SemInfo {
                key: sembuf.sem_perm._key,
                semid,
                perms: sembuf.sem_perm.mode,
                nsems: sembuf.sem_nsems as u32,
                uid: sembuf.sem_perm.uid,
                gid: sembuf.sem_perm.gid,
                cuid: sembuf.sem_perm.cuid,
                cgid: sembuf.sem_perm.cgid,
                otime: sembuf.sem_otime,
                ctime: sembuf.sem_ctime,
            });
        }
    }
    entries
}

// ============================================================================
// IPC info structures
// ============================================================================

#[cfg(target_os = "linux")]
#[derive(Debug)]
struct MsgQueueInfo {
    key: i32,
    msqid: i32,
    perms: u16,
    cbytes: u64,
    qnum: u64,
    lspid: i32,
    lrpid: i32,
    uid: u32,
    gid: u32,
    cuid: u32,
    cgid: u32,
    stime: i64,
    rtime: i64,
    ctime: i64,
}

#[derive(Debug)]
struct ShmInfo {
    key: i32,
    shmid: i32,
    perms: u16,
    size: u64,
    cpid: i32,
    lpid: i32,
    nattch: u32,
    uid: u32,
    gid: u32,
    cuid: u32,
    cgid: u32,
    atime: i64,
    dtime: i64,
    ctime: i64,
}

#[derive(Debug)]
struct SemInfo {
    key: i32,
    semid: i32,
    perms: u16,
    nsems: u32,
    uid: u32,
    gid: u32,
    cuid: u32,
    cgid: u32,
    otime: i64,
    ctime: i64,
}

// ============================================================================
// Display functions
// ============================================================================

fn display_message_queues(_args: &Args) {
    println!();

    #[cfg(target_os = "linux")]
    let args = _args;

    #[cfg(target_os = "linux")]
    {
        match read_proc_msg() {
            Ok(entries) if !entries.is_empty() => {
                // Build header
                let mut header = String::from("T     ID     KEY        MODE        OWNER    GROUP");
                if args.creator {
                    header.push_str("    CREATOR  CGROUP");
                }
                if args.outstanding {
                    header.push_str("   CBYTES    QNUM");
                }
                if args.max_size {
                    header.push_str("   QBYTES");
                }
                if args.pid {
                    header.push_str("    LSPID    LRPID");
                }
                if args.time {
                    header.push_str("      STIME      RTIME      CTIME");
                }
                println!("{}", header);
                println!("{}:", gettext("Message Queues"));

                for entry in entries {
                    let mode_str = format_mode(entry.perms, 'q', false, false);
                    let owner = get_username(entry.uid);
                    let group = get_groupname(entry.gid);

                    let mut line = format!(
                        "q {:>6} 0x{:08x} {:11} {:>8} {:>8}",
                        entry.msqid,
                        entry.key as u32,
                        mode_str,
                        truncate_to_chars(&owner, 8),
                        truncate_to_chars(&group, 8)
                    );

                    if args.creator {
                        let creator = get_username(entry.cuid);
                        let cgroup = get_groupname(entry.cgid);
                        line.push_str(&format!(
                            " {:>8} {:>8}",
                            truncate_to_chars(&creator, 8),
                            truncate_to_chars(&cgroup, 8)
                        ));
                    }
                    if args.outstanding {
                        line.push_str(&format!(" {:>8} {:>7}", entry.cbytes, entry.qnum));
                    }
                    if args.max_size {
                        // Note: qbytes is not in /proc, would need sysctl
                        line.push_str(&format!(" {:>8}", "-"));
                    }
                    if args.pid {
                        line.push_str(&format!(" {:>8} {:>8}", entry.lspid, entry.lrpid));
                    }
                    if args.time {
                        line.push_str(&format!(
                            " {:>9} {:>9} {:>9}",
                            format_time(entry.stime),
                            format_time(entry.rtime),
                            format_time(entry.ctime)
                        ));
                    }

                    println!("{}", line);
                }
            }
            _ => {
                println!("{}", gettext("Message Queue facility not in system."));
            }
        }
    }

    #[cfg(target_os = "macos")]
    {
        println!("{}", gettext("Message Queue facility not in system."));
    }
}

fn display_shared_memory(args: &Args) {
    println!();

    #[cfg(target_os = "linux")]
    let entries = read_proc_shm().unwrap_or_default();

    #[cfg(target_os = "macos")]
    let entries = read_macos_shm();

    if entries.is_empty() {
        println!("{}", gettext("Shared Memory facility not in system."));
        return;
    }

    // Build header
    let mut header = String::from("T     ID     KEY        MODE        OWNER    GROUP");
    if args.creator {
        header.push_str("    CREATOR  CGROUP");
    }
    if args.outstanding {
        header.push_str("   NATTCH");
    }
    if args.max_size {
        header.push_str("      SEGSZ");
    }
    if args.pid {
        header.push_str("     CPID     LPID");
    }
    if args.time {
        header.push_str("      ATIME      DTIME      CTIME");
    }
    println!("{}", header);
    println!("{}:", gettext("Shared Memory"));

    for entry in entries {
        let mode_str = format_mode(entry.perms, 'm', false, false);
        let owner = get_username(entry.uid);
        let group = get_groupname(entry.gid);

        let mut line = format!(
            "m {:>6} 0x{:08x} {:11} {:>8} {:>8}",
            entry.shmid,
            entry.key as u32,
            mode_str,
            truncate_to_chars(&owner, 8),
            truncate_to_chars(&group, 8)
        );

        if args.creator {
            let creator = get_username(entry.cuid);
            let cgroup = get_groupname(entry.cgid);
            line.push_str(&format!(
                " {:>8} {:>8}",
                truncate_to_chars(&creator, 8),
                truncate_to_chars(&cgroup, 8)
            ));
        }
        if args.outstanding {
            line.push_str(&format!(" {:>8}", entry.nattch));
        }
        if args.max_size {
            line.push_str(&format!(" {:>10}", entry.size));
        }
        if args.pid {
            line.push_str(&format!(" {:>8} {:>8}", entry.cpid, entry.lpid));
        }
        if args.time {
            line.push_str(&format!(
                " {:>9} {:>9} {:>9}",
                format_time(entry.atime),
                format_time(entry.dtime),
                format_time(entry.ctime)
            ));
        }

        println!("{}", line);
    }
}

fn display_semaphores(args: &Args) {
    println!();

    #[cfg(target_os = "linux")]
    let entries = read_proc_sem().unwrap_or_default();

    #[cfg(target_os = "macos")]
    let entries = read_macos_sem();

    if entries.is_empty() {
        println!("{}", gettext("Semaphore facility not in system."));
        return;
    }

    // Build header
    let mut header = String::from("T     ID     KEY        MODE        OWNER    GROUP");
    if args.creator {
        header.push_str("    CREATOR  CGROUP");
    }
    if args.max_size {
        header.push_str("    NSEMS");
    }
    if args.time {
        header.push_str("      OTIME      CTIME");
    }
    println!("{}", header);
    println!("{}:", gettext("Semaphores"));

    for entry in entries {
        let mode_str = format_mode(entry.perms, 's', false, false);
        let owner = get_username(entry.uid);
        let group = get_groupname(entry.gid);

        let mut line = format!(
            "s {:>6} 0x{:08x} {:11} {:>8} {:>8}",
            entry.semid,
            entry.key as u32,
            mode_str,
            truncate_to_chars(&owner, 8),
            truncate_to_chars(&group, 8)
        );

        if args.creator {
            let creator = get_username(entry.cuid);
            let cgroup = get_groupname(entry.cgid);
            line.push_str(&format!(
                " {:>8} {:>8}",
                truncate_to_chars(&creator, 8),
                truncate_to_chars(&cgroup, 8)
            ));
        }
        if args.max_size {
            line.push_str(&format!(" {:>8}", entry.nsems));
        }
        if args.time {
            line.push_str(&format!(
                " {:>9} {:>9}",
                format_time(entry.otime),
                format_time(entry.ctime)
            ));
        }

        println!("{}", line);
    }
}

fn display_ipc_status(args: &Args) {
    // POSIX requires: "IPC status from %s as of %s\n", <source>, <date>
    #[cfg(target_os = "linux")]
    let source = "/proc/sysvipc";
    #[cfg(target_os = "macos")]
    let source = "kernel";
    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    let source = "system";

    println!("IPC status from {} as of {}", source, get_current_date());

    if args.message_queues {
        display_message_queues(args);
    }

    if args.shared_memory {
        display_shared_memory(args);
    }

    if args.semaphores {
        display_semaphores(args);
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let mut args = Args::parse();

    // -a enables all print options
    if args.all {
        enable_all_options(&mut args);
    }

    // Default to showing all facilities if none of -q, -m, or -s are specified
    if !(args.message_queues || args.shared_memory || args.semaphores) {
        enable_all_facilities(&mut args);
    }

    // Display IPC status
    display_ipc_status(&args);

    Ok(())
}
