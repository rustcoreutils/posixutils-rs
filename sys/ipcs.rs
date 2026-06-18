//
// Copyright (c) 2024-2026 Jeff Garzik
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
use std::io;

#[cfg(target_os = "linux")]
use std::io::BufRead;

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

/// SHM_DEST: shared memory segment marked to be destroyed when the last
/// process detaches. Stored in the upper bits of the IPC mode field on both
/// Linux (visible via /proc/sysvipc) and macOS (shm_perm.mode).
const SHM_DEST: u16 = 0o1000;

/// Format the access mode and flags as the 11-character string POSIX mandates
/// (IEEE Std 1003.1-2024, ipcs STDOUT, MODE column):
///   `[C-]` `rwa rwa rwa` `[ ]`
/// - Char 1: `C` if this is a shared memory segment marked for clearing
///   (SHM_DEST), otherwise `-`.
/// - Chars 2-10: three sets of three bits (owner, group, others); within each
///   set: read (`r`), write/alter (`a`), nothing (`-`).
/// - Char 11: alternate/additional access-control indicator; a single <space>
///   when none is associated with the facility.
///
/// IPC permission bits map to file permission bits (matching the rendering
/// below): the first character of each triad is read, the second write, the
/// third alter (IPC objects have no execute permission):
/// - read (r):  0400/040/004
/// - write (w): 0200/020/002
/// - alter (a): 0100/010/001
fn format_mode(mode: u16, facility: char) -> String {
    // First character: C for a shared memory segment pending destruction.
    let clear = if facility == 'm' && (mode & SHM_DEST) != 0 {
        'C'
    } else {
        '-'
    };

    // Permission bits: read (r), write/alter (a). The first bit of each triad
    // is read; the second is write/alter.
    let owner_r = if mode & 0o400 != 0 { 'r' } else { '-' };
    let owner_w = if mode & 0o200 != 0 { 'w' } else { '-' };
    let owner_a = if mode & 0o100 != 0 { 'a' } else { '-' };

    let group_r = if mode & 0o040 != 0 { 'r' } else { '-' };
    let group_w = if mode & 0o020 != 0 { 'w' } else { '-' };
    let group_a = if mode & 0o010 != 0 { 'a' } else { '-' };

    let other_r = if mode & 0o004 != 0 { 'r' } else { '-' };
    let other_w = if mode & 0o002 != 0 { 'w' } else { '-' };
    let other_a = if mode & 0o001 != 0 { 'a' } else { '-' };

    // 11th character: space means no alternate access control method.
    let acl = ' ';

    format!(
        "{}{}{}{}{}{}{}{}{}{}{}",
        clear, owner_r, owner_w, owner_a, group_r, group_w, group_a, other_r, other_w, other_a, acl
    )
}

/// Format a timestamp as the POSIX `%d:%2.2d:%2.2d` time-of-day (unpadded hour,
/// zero-padded minute and second) or " no-entry" when no event has occurred.
fn format_time(timestamp: i64) -> String {
    if timestamp == 0 {
        " no-entry".to_string()
    } else {
        match Local.timestamp_opt(timestamp, 0).single() {
            // `%-H` suppresses the leading zero on the hour, matching the
            // spec's `%d` (e.g. "9:30:00", not "09:30:00").
            Some(dt) => dt.format("%-H:%M:%S").to_string(),
            None => " no-entry".to_string(), // Invalid or ambiguous timestamp
        }
    }
}

/// Get current date in POSIX locale format (matching `date` command)
/// Uses libc strftime to get proper timezone abbreviation (e.g., "EST" not "-05:00")
fn get_current_date() -> String {
    use std::mem::MaybeUninit;

    let mut buf = [0u8; 64];
    let now = unsafe { libc::time(std::ptr::null_mut()) };
    let mut tm = MaybeUninit::<libc::tm>::uninit();

    unsafe {
        let tm_ptr = libc::localtime_r(&now, tm.as_mut_ptr());
        if tm_ptr.is_null() {
            // Fallback to chrono if localtime_r fails
            chrono::Local::now()
                .format("%a %b %e %H:%M:%S %Z %Y")
                .to_string()
        } else {
            let tm = tm.assume_init();
            let format = std::ffi::CString::new("%a %b %e %H:%M:%S %Z %Y").unwrap();
            let len = libc::strftime(
                buf.as_mut_ptr() as *mut libc::c_char,
                buf.len(),
                format.as_ptr(),
                &tm,
            );
            if len > 0 {
                String::from_utf8_lossy(&buf[..len]).to_string()
            } else {
                // Fallback to chrono if strftime fails
                chrono::Local::now()
                    .format("%a %b %e %H:%M:%S %z %Y")
                    .to_string()
            }
        }
    }
}

// ============================================================================
// Linux-specific IPC enumeration via /proc/sysvipc
// ============================================================================

/// Fetch the maximum-bytes (`msg_qbytes`) limit of a message queue.
///
/// This value is not exposed in /proc/sysvipc/msg, so query it directly via
/// `msgctl(IPC_STAT)`. Returns `None` if the call fails (e.g. the queue was
/// removed between enumeration and the stat, or we lack read permission), in
/// which case the column is rendered as "-".
#[cfg(target_os = "linux")]
fn get_msg_qbytes(msqid: i32) -> Option<u64> {
    let mut buf: libc::msqid_ds = unsafe { std::mem::zeroed() };
    if unsafe { libc::msgctl(msqid, libc::IPC_STAT, &mut buf) } == 0 {
        Some(buf.msg_qbytes as u64)
    } else {
        None
    }
}

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
            let msqid: i32 = fields[1].parse().unwrap_or(0);
            entries.push(MsgQueueInfo {
                key: fields[0].parse::<i32>().unwrap_or(0),
                msqid,
                // The kernel prints the mode field in /proc/sysvipc in octal
                // (see Documentation: it uses "%4o"), so parse base 8.
                perms: u16::from_str_radix(fields[2], 8).unwrap_or(0),
                // qbytes is not in /proc; fetch it via msgctl(IPC_STAT).
                qbytes: get_msg_qbytes(msqid),
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
                key: fields[0].parse::<i32>().unwrap_or(0),
                shmid: fields[1].parse().unwrap_or(0),
                // Octal mode field (see read_proc_msg); includes SHM_DEST when set.
                perms: u16::from_str_radix(fields[2], 8).unwrap_or(0),
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
                key: fields[0].parse::<i32>().unwrap_or(0),
                semid: fields[1].parse().unwrap_or(0),
                // Octal mode field (see read_proc_msg).
                perms: u16::from_str_radix(fields[2], 8).unwrap_or(0),
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

/// Default value for kern.sysv.semmni on macOS (max semaphore identifiers)
#[cfg(target_os = "macos")]
const MACOS_DEFAULT_SEMMNI: i32 = 87381;

/// macOS has no /proc and no IPC-enumeration API, so we probe identifiers by
/// the (slot, sequence) encoding. These bound the probe: the *slot* count is
/// the number of identifier slots a live object can occupy (`shmmni`/`semmni`),
/// and the *sequence* count is how many reuse generations we probe per slot.
/// They are a best-effort ceiling — objects in very high slots/sequences on an
/// extremely busy host may be missed.
#[cfg(target_os = "macos")]
const MACOS_MAX_SLOTS: i32 = 2048;

#[cfg(target_os = "macos")]
const MACOS_SEQ_PROBE: i32 = 256;

/// Query a sysctl integer value by name
#[cfg(target_os = "macos")]
fn get_sysctl_int(name: &str) -> Option<i32> {
    use std::ffi::CString;
    use std::mem::size_of;

    let cname = CString::new(name).ok()?;
    let mut value: i32 = 0;
    let mut size = size_of::<i32>();

    let result = unsafe {
        libc::sysctlbyname(
            cname.as_ptr(),
            &mut value as *mut i32 as *mut libc::c_void,
            &mut size,
            std::ptr::null_mut(),
            0,
        )
    };

    if result == 0 {
        Some(value)
    } else {
        None
    }
}

/// Iterate through macOS IPC IDs efficiently.
/// macOS IPC IDs follow the pattern: slot_index * 65536 + sequence.
/// We iterate through each slot and a range of sequences within each slot.
/// This is much faster than iterating through all possible IDs sequentially.
#[cfg(target_os = "macos")]
fn iter_macos_ipc_ids(max_slots: i32, seqs_per_slot: i32) -> impl Iterator<Item = i32> {
    let max_slots = max_slots.min(MACOS_MAX_SLOTS); // Hard ceiling for safety
    let seqs = seqs_per_slot.min(MACOS_SEQ_PROBE);

    (0..max_slots).flat_map(move |slot| {
        let base = slot * 65536;
        (0..seqs).map(move |seq| base + seq)
    })
}

#[cfg(target_os = "macos")]
fn read_macos_shm() -> Vec<ShmInfo> {
    use libc::{shmctl, shmid_ds, IPC_STAT};

    let mut entries = Vec::new();
    let mut shmbuf: shmid_ds = unsafe { std::mem::zeroed() };

    // kern.sysv.shmmni is the max number of shared memory identifiers (typically 32)
    let max_slots = get_sysctl_int("kern.sysv.shmmni").unwrap_or(32);
    let seqs_per_slot = MACOS_SEQ_PROBE;

    for shmid in iter_macos_ipc_ids(max_slots, seqs_per_slot) {
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

    // kern.sysv.semmni is the max number of semaphore identifiers. It can be
    // very large, so the slot count is bounded by MACOS_MAX_SLOTS inside
    // iter_macos_ipc_ids (no longer artificially capped at the sequence-probe
    // budget, which previously dropped any identifier in slot >= 256).
    let max_slots = get_sysctl_int("kern.sysv.semmni").unwrap_or(MACOS_DEFAULT_SEMMNI);
    let seqs_per_slot = MACOS_SEQ_PROBE;

    for semid in iter_macos_ipc_ids(max_slots, seqs_per_slot) {
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
    qbytes: Option<u64>,
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

fn display_message_queues(_args: &Args) -> io::Result<()> {
    println!();

    #[cfg(target_os = "linux")]
    {
        let args = _args;
        let entries = match read_proc_msg() {
            Ok(e) => e,
            // Facility compiled out of the kernel: report "not in system".
            Err(e) if e.kind() == io::ErrorKind::NotFound => {
                println!("{}", gettext("Message Queue facility not in system."));
                return Ok(());
            }
            // Any other error (e.g. permission denied) is a real failure.
            Err(e) => return Err(e),
        };

        // POSIX: a facility not installed or not used since the last reboot is
        // reported as "not in system" rather than an empty table.
        if entries.is_empty() {
            println!("{}", gettext("Message Queue facility not in system."));
            return Ok(());
        }

        // Column headings (facility has been used since the last reboot).
        let mut header = String::from("T     ID     KEY        MODE        OWNER    GROUP");
        if args.creator {
            header.push_str("  CREATOR   CGROUP");
        }
        if args.outstanding {
            header.push_str(" CBYTES  QNUM");
        }
        if args.max_size {
            header.push_str(" QBYTES");
        }
        if args.pid {
            header.push_str("  LSPID  LRPID");
        }
        if args.time {
            header.push_str("   STIME    RTIME    CTIME");
        }
        println!("{}", header);
        println!("{}:", gettext("Message Queues"));

        for entry in entries {
            let mode_str = format_mode(entry.perms, 'q');
            let owner = get_username(entry.uid);
            let group = get_groupname(entry.gid);

            let mut line = format!(
                "q {:>6} 0x{:08x} {:12} {:>8} {:>8}",
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
                match entry.qbytes {
                    Some(q) => line.push_str(&format!(" {:>8}", q)),
                    None => line.push_str(&format!(" {:>8}", "-")),
                }
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

    // macOS doesn't support SysV message queues
    #[cfg(target_os = "macos")]
    {
        let _ = _args;
        println!("{}", gettext("Message Queue facility not in system."));
    }

    Ok(())
}

fn display_shared_memory(args: &Args) -> io::Result<()> {
    println!();

    #[cfg(target_os = "linux")]
    let entries = match read_proc_shm() {
        Ok(e) => e,
        Err(e) if e.kind() == io::ErrorKind::NotFound => {
            println!("{}", gettext("Shared Memory facility not in system."));
            return Ok(());
        }
        Err(e) => return Err(e),
    };

    #[cfg(target_os = "macos")]
    let entries = read_macos_shm();

    // POSIX: not installed or unused since last reboot -> "not in system".
    if entries.is_empty() {
        println!("{}", gettext("Shared Memory facility not in system."));
        return Ok(());
    }

    // Column headings (facility has been used since the last reboot).
    let mut header = String::from("T     ID     KEY        MODE        OWNER    GROUP");
    if args.creator {
        header.push_str("  CREATOR   CGROUP");
    }
    if args.outstanding {
        header.push_str(" NATTCH");
    }
    if args.max_size {
        header.push_str("  SEGSZ");
    }
    if args.pid {
        header.push_str("  CPID  LPID");
    }
    if args.time {
        header.push_str("   ATIME    DTIME    CTIME");
    }
    println!("{}", header);
    println!("{}:", gettext("Shared Memory"));

    for entry in entries {
        let mode_str = format_mode(entry.perms, 'm');
        let owner = get_username(entry.uid);
        let group = get_groupname(entry.gid);

        let mut line = format!(
            "m {:>6} 0x{:08x} {:12} {:>8} {:>8}",
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

    Ok(())
}

fn display_semaphores(args: &Args) -> io::Result<()> {
    println!();

    #[cfg(target_os = "linux")]
    let entries = match read_proc_sem() {
        Ok(e) => e,
        Err(e) if e.kind() == io::ErrorKind::NotFound => {
            println!("{}", gettext("Semaphore facility not in system."));
            return Ok(());
        }
        Err(e) => return Err(e),
    };

    #[cfg(target_os = "macos")]
    let entries = read_macos_sem();

    // POSIX: not installed or unused since last reboot -> "not in system".
    if entries.is_empty() {
        println!("{}", gettext("Semaphore facility not in system."));
        return Ok(());
    }

    // Column headings (facility has been used since the last reboot).
    let mut header = String::from("T     ID     KEY        MODE        OWNER    GROUP");
    if args.creator {
        header.push_str("  CREATOR   CGROUP");
    }
    if args.max_size {
        header.push_str(" NSEMS");
    }
    if args.time {
        header.push_str("   OTIME    CTIME");
    }
    println!("{}", header);
    println!("{}:", gettext("Semaphores"));

    for entry in entries {
        let mode_str = format_mode(entry.perms, 's');
        let owner = get_username(entry.uid);
        let group = get_groupname(entry.gid);

        let mut line = format!(
            "s {:>6} 0x{:08x} {:12} {:>8} {:>8}",
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

    Ok(())
}

fn display_ipc_status(args: &Args) -> io::Result<()> {
    // POSIX requires: "IPC status from %s as of %s\n", <source>, <date>
    #[cfg(target_os = "linux")]
    let source = "/proc/sysvipc";
    #[cfg(target_os = "macos")]
    let source = "kernel";
    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    let source = "system";

    println!("IPC status from {} as of {}", source, get_current_date());

    if args.message_queues {
        display_message_queues(args)?;
    }

    if args.shared_memory {
        display_shared_memory(args)?;
    }

    if args.semaphores {
        display_semaphores(args)?;
    }

    Ok(())
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
    display_ipc_status(&args)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    // The /proc/sysvipc mode field is printed in octal by the kernel; ipcs must
    // parse it base 8, not base 10 (regression for audit item #IS1).
    #[test]
    fn proc_mode_field_is_octal() {
        // "640" is octal 0o640 == 416, NOT decimal 640.
        assert_eq!(u16::from_str_radix("640", 8).unwrap(), 0o640);
        assert_ne!(u16::from_str_radix("640", 8).unwrap(), 640);
        // With SHM_DEST set the kernel prints e.g. "1640".
        assert_eq!(u16::from_str_radix("1640", 8).unwrap(), 0o1640);
    }

    // The MODE column is exactly 11 characters: [C-] + 9 perm chars + ACL space
    // (audit item #IS3 + the 11-vs-12 char POSIX conformance correction).
    #[test]
    fn mode_string_is_eleven_chars_and_decodes_perms() {
        // 0o640 = rw-r----- ; not a shm dest, so leading flag is '-'.
        let s = format_mode(0o640, 'q');
        assert_eq!(s.chars().count(), 11, "MODE must be 11 chars, got {s:?}");
        assert_eq!(s, "-rw-r----- ");

        // Full perms 0o777 -> rwarwarwa.
        assert_eq!(format_mode(0o777, 's'), "-rwarwarwa ");
        // No perms: leading flag '-', nine '-' perm chars, ACL space.
        assert_eq!(format_mode(0o000, 'm'), "---------- ");
    }

    // The time columns use " no-entry" when no event has occurred (timestamp 0),
    // and otherwise the unpadded-hour POSIX form `%d:%2.2d:%2.2d` (#IS4).
    #[test]
    fn time_no_entry_and_format() {
        assert_eq!(format_time(0), " no-entry");
        // A real timestamp renders as H:MM:SS with exactly two ':' separators
        // and zero-padded minute/second; the hour carries no leading zero.
        let s = format_time(1_000_000_000);
        assert_eq!(s.matches(':').count(), 2, "expected H:MM:SS, got {s:?}");
        let hour = s.split(':').next().unwrap();
        assert!(
            hour.len() == 1 || !hour.starts_with('0'),
            "hour must be unpadded (no leading zero): {s:?}"
        );
    }

    // The 'C' clear-on-detach flag is set only for shared memory with SHM_DEST.
    #[test]
    fn shm_dest_sets_c_flag() {
        // SHM_DEST set on a shared memory segment -> leading 'C'.
        assert_eq!(format_mode(0o1640, 'm'), "Crw-r----- ");
        // SHM_DEST bit set but not a shared memory facility -> still '-'.
        assert_eq!(format_mode(0o1640, 'q'), "-rw-r----- ");
        // Shared memory without SHM_DEST -> '-'.
        assert_eq!(format_mode(0o640, 'm'), "-rw-r----- ");
    }
}
