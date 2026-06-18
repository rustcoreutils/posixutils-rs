//
// Copyright (c) 2024-2025 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::{c_int, c_ushort};
use std::io::{self, Error, ErrorKind};
use std::ptr;

use clap::{ArgMatches, CommandFactory, FromArgMatches, Parser};
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
#[cfg(not(target_os = "macos"))]
use libc::{msgctl, msgget, msqid_ds};
use libc::{semctl, semget, shmctl, shmget, shmid_ds};

/// Parse an IPC key value that may be decimal or hexadecimal (0x prefix)
fn parse_ipc_key(s: &str) -> Result<i32, String> {
    if let Some(hex) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
        // Reject if hex part starts with a sign
        if hex.starts_with('+') || hex.starts_with('-') {
            return Err("invalid hex key: unexpected sign after 0x prefix".to_string());
        }
        u32::from_str_radix(hex, 16)
            .map(|v| v as i32)
            .map_err(|e| format!("invalid hex key: {}", e))
    } else {
        // Accept the full 32-bit key_t range: signed decimals directly, and
        // unsigned decimals above i32::MAX cast to i32 like the hex path (#IR2).
        s.parse::<i32>()
            .or_else(|_| s.parse::<u32>().map(|v| v as i32))
            .map_err(|e| format!("invalid key: {}", e))
    }
}

#[derive(Parser)]
#[command(
    version,
    about = gettext("ipcrm - remove an XSI message queue, semaphore set, or shared memory segment identifier")
)]
struct Args {
    #[arg(
        short = 's',
        action = clap::ArgAction::Append,
        help = gettext("Remove the semaphore identifier semid from the system")
    )]
    semid: Vec<i32>,

    #[arg(
        short = 'S',
        action = clap::ArgAction::Append,
        value_parser = parse_ipc_key,
        help = gettext("Remove the semaphore identifier, created with key semkey, from the system")
    )]
    semkey: Vec<i32>,

    #[arg(
        short = 'm',
        action = clap::ArgAction::Append,
        help = gettext("Remove the shared memory identifier shmid from the system")
    )]
    shmid: Vec<i32>,

    #[arg(
        short = 'M',
        action = clap::ArgAction::Append,
        value_parser = parse_ipc_key,
        help = gettext("Remove the shared memory identifier, created with key shmkey, from the system")
    )]
    shmkey: Vec<i32>,

    #[arg(
        short = 'q',
        action = clap::ArgAction::Append,
        help = gettext("Remove the message queue identifier msgid from the system")
    )]
    msgid: Vec<i32>,

    #[arg(
        short = 'Q',
        action = clap::ArgAction::Append,
        value_parser = parse_ipc_key,
        help = gettext("Remove the message queue identifier, created with key msgkey, from the system")
    )]
    msgkey: Vec<i32>,
}

#[cfg(not(target_os = "macos"))]
fn msg_key_lookup(msgkey: i32) -> io::Result<i32> {
    if msgkey == libc::IPC_PRIVATE {
        return Err(Error::new(
            ErrorKind::InvalidInput,
            gettext("invalid key: IPC_PRIVATE"),
        ));
    }
    let res: i32 = unsafe { msgget(msgkey, 0) };

    if res < 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(res)
    }
}

#[cfg(not(target_os = "macos"))]
fn msg_rm(msgid: i32) -> io::Result<()> {
    let res: i32 = unsafe {
        msgctl(
            msgid,
            libc::IPC_RMID,
            ptr::null::<msqid_ds>() as *mut msqid_ds,
        )
    };

    if res < 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(())
    }
}

fn shm_key_lookup(shmkey: i32) -> io::Result<i32> {
    if shmkey == libc::IPC_PRIVATE {
        return Err(Error::new(
            ErrorKind::InvalidInput,
            gettext("invalid key: IPC_PRIVATE"),
        ));
    }
    let res: i32 = unsafe { shmget(shmkey, 0, 0) };

    if res < 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(res)
    }
}

fn shm_rm(shmid: i32) -> io::Result<()> {
    let res: i32 = unsafe {
        shmctl(
            shmid,
            libc::IPC_RMID,
            ptr::null::<shmid_ds>() as *mut shmid_ds,
        )
    };

    if res < 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(())
    }
}

fn sem_key_lookup(semkey: i32) -> io::Result<i32> {
    if semkey == libc::IPC_PRIVATE {
        return Err(Error::new(
            ErrorKind::InvalidInput,
            gettext("invalid key: IPC_PRIVATE"),
        ));
    }
    let res: i32 = unsafe { semget(semkey, 0, 0) };

    if res < 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(res)
    }
}

// Define the union semun as per POSIX requirements
#[repr(C)]
union semun {
    val: c_int,               // for SETVAL
    buf: *mut libc::semid_ds, // for IPC_STAT and IPC_SET
    array: *mut c_ushort,     // for GETALL and SETALL
}

fn sem_rm(semid: i32) -> io::Result<()> {
    let arg = semun { val: 0 };

    let res: i32 = unsafe { semctl(semid, 0, libc::IPC_RMID, arg) };

    if res < 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(())
    }
}

/// A single removal request, in the order it appeared on the command line.
enum Op {
    SemKey(i32),
    SemId(i32),
    ShmKey(i32),
    ShmId(i32),
    MsgKey(i32),
    MsgId(i32),
}

// Each remover prints its own diagnostic and returns true on failure.

fn rm_sem_key(semkey: i32) -> bool {
    match sem_key_lookup(semkey) {
        Ok(semid) => sem_rm(semid)
            .map_err(|e| eprintln!("ipcrm: {}: {}", gettext("semaphore key"), e))
            .is_err(),
        Err(e) => {
            eprintln!("ipcrm: {}: 0x{:x}: {}", gettext("semaphore key"), semkey, e);
            true
        }
    }
}

fn rm_sem_id(semid: i32) -> bool {
    sem_rm(semid)
        .map_err(|e| eprintln!("ipcrm: {}: {}: {}", gettext("semaphore id"), semid, e))
        .is_err()
}

fn rm_shm_key(shmkey: i32) -> bool {
    match shm_key_lookup(shmkey) {
        Ok(shmid) => shm_rm(shmid)
            .map_err(|e| eprintln!("ipcrm: {}: {}", gettext("shared memory key"), e))
            .is_err(),
        Err(e) => {
            eprintln!(
                "ipcrm: {}: 0x{:x}: {}",
                gettext("shared memory key"),
                shmkey,
                e
            );
            true
        }
    }
}

fn rm_shm_id(shmid: i32) -> bool {
    shm_rm(shmid)
        .map_err(|e| eprintln!("ipcrm: {}: {}: {}", gettext("shared memory id"), shmid, e))
        .is_err()
}

#[cfg(not(target_os = "macos"))]
fn rm_msg_key(msgkey: i32) -> bool {
    match msg_key_lookup(msgkey) {
        Ok(msgid) => msg_rm(msgid)
            .map_err(|e| eprintln!("ipcrm: {}: {}", gettext("message queue key"), e))
            .is_err(),
        Err(e) => {
            eprintln!(
                "ipcrm: {}: 0x{:x}: {}",
                gettext("message queue key"),
                msgkey,
                e
            );
            true
        }
    }
}

#[cfg(not(target_os = "macos"))]
fn rm_msg_id(msgid: i32) -> bool {
    msg_rm(msgid)
        .map_err(|e| eprintln!("ipcrm: {}: {}: {}", gettext("message queue id"), msgid, e))
        .is_err()
}

// macOS does not support SysV message queues: report and fail.
#[cfg(target_os = "macos")]
fn rm_msg_key(msgkey: i32) -> bool {
    eprintln!(
        "ipcrm: {}: 0x{:x}: {}",
        gettext("message queue key"),
        msgkey,
        gettext("message queues not supported on this system")
    );
    true
}

#[cfg(target_os = "macos")]
fn rm_msg_id(msgid: i32) -> bool {
    eprintln!(
        "ipcrm: {}: {}: {}",
        gettext("message queue id"),
        msgid,
        gettext("message queues not supported on this system")
    );
    true
}

fn remove_ipcs(ops: &[Op]) -> i32 {
    let mut exit_code = 0;
    for op in ops {
        let failed = match op {
            Op::SemKey(k) => rm_sem_key(*k),
            Op::SemId(id) => rm_sem_id(*id),
            Op::ShmKey(k) => rm_shm_key(*k),
            Op::ShmId(id) => rm_shm_id(*id),
            Op::MsgKey(k) => rm_msg_key(*k),
            Op::MsgId(id) => rm_msg_id(*id),
        };
        if failed {
            exit_code = 1;
        }
    }
    exit_code
}

/// Append each parsed value paired with its command-line value index, so the
/// operations can later be sorted back into argv order (Guideline 11, #IR1).
fn collect(
    out: &mut Vec<(usize, Op)>,
    m: &ArgMatches,
    id: &str,
    values: &[i32],
    make: fn(i32) -> Op,
) {
    if let Some(indices) = m.indices_of(id) {
        for (val, idx) in values.iter().zip(indices) {
            out.push((idx, make(*val)));
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    // Parse via clap (for --help/--version/validation) and keep the matches so
    // we can recover the command-line order of the repeated options.
    let matches = Args::command().get_matches();
    let args = Args::from_arg_matches(&matches)?;

    let mut ops: Vec<(usize, Op)> = Vec::new();
    collect(&mut ops, &matches, "semkey", &args.semkey, Op::SemKey);
    collect(&mut ops, &matches, "semid", &args.semid, Op::SemId);
    collect(&mut ops, &matches, "shmkey", &args.shmkey, Op::ShmKey);
    collect(&mut ops, &matches, "shmid", &args.shmid, Op::ShmId);
    collect(&mut ops, &matches, "msgkey", &args.msgkey, Op::MsgKey);
    collect(&mut ops, &matches, "msgid", &args.msgid, Op::MsgId);
    ops.sort_by_key(|(idx, _)| *idx);
    let ordered: Vec<Op> = ops.into_iter().map(|(_, op)| op).collect();

    let exit_code = remove_ipcs(&ordered);

    std::process::exit(exit_code)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_keys() {
        assert_eq!(parse_ipc_key("0x10"), Ok(0x10));
        assert_eq!(parse_ipc_key("16"), Ok(16));
        assert_eq!(parse_ipc_key("-1"), Ok(-1));
        // Decimal above i32::MAX is accepted and wraps like the hex path (#IR2).
        assert_eq!(parse_ipc_key("3000000000"), Ok(0xb2d0_5e00u32 as i32));
        assert_eq!(parse_ipc_key("0xB2D05E00"), Ok(0xb2d0_5e00u32 as i32));
        assert!(parse_ipc_key("0xZZ").is_err());
        assert!(parse_ipc_key("notanumber").is_err());
    }
}
