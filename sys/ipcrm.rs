//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::{c_int, c_ushort};
use std::io::{self, Error, ErrorKind};
use std::ptr;

use clap::Parser;
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
        s.parse::<i32>().map_err(|e| format!("invalid key: {}", e))
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

fn remove_ipcs(args: &Args) -> i32 {
    let mut exit_code = 0;

    // Remove semaphores by key
    for semkey in &args.semkey {
        match sem_key_lookup(*semkey) {
            Ok(semid) => {
                if let Err(e) = sem_rm(semid) {
                    eprintln!("ipcrm: {}: {}", gettext("semaphore key"), e);
                    exit_code = 1;
                }
            }
            Err(e) => {
                eprintln!(
                    "ipcrm: {}: 0x{:x}: {}",
                    gettext("semaphore key"),
                    *semkey,
                    e
                );
                exit_code = 1;
            }
        }
    }

    // Remove semaphores by ID
    for semid in &args.semid {
        if let Err(e) = sem_rm(*semid) {
            eprintln!("ipcrm: {}: {}: {}", gettext("semaphore id"), semid, e);
            exit_code = 1;
        }
    }

    // Remove shared memory segments by key
    for shmkey in &args.shmkey {
        match shm_key_lookup(*shmkey) {
            Ok(shmid) => {
                if let Err(e) = shm_rm(shmid) {
                    eprintln!("ipcrm: {}: {}", gettext("shared memory key"), e);
                    exit_code = 1;
                }
            }
            Err(e) => {
                eprintln!(
                    "ipcrm: {}: 0x{:x}: {}",
                    gettext("shared memory key"),
                    *shmkey,
                    e
                );
                exit_code = 1;
            }
        }
    }

    // Remove shared memory segments by ID
    for shmid in &args.shmid {
        if let Err(e) = shm_rm(*shmid) {
            eprintln!("ipcrm: {}: {}: {}", gettext("shared memory id"), shmid, e);
            exit_code = 1;
        }
    }

    // Remove message queues (Linux only - macOS doesn't support SysV message queues)
    #[cfg(not(target_os = "macos"))]
    {
        // Remove message queues by key
        for msgkey in &args.msgkey {
            match msg_key_lookup(*msgkey) {
                Ok(msgid) => {
                    if let Err(e) = msg_rm(msgid) {
                        eprintln!("ipcrm: {}: {}", gettext("message queue key"), e);
                        exit_code = 1;
                    }
                }
                Err(e) => {
                    eprintln!(
                        "ipcrm: {}: 0x{:x}: {}",
                        gettext("message queue key"),
                        *msgkey,
                        e
                    );
                    exit_code = 1;
                }
            }
        }

        // Remove message queues by ID
        for msgid in &args.msgid {
            if let Err(e) = msg_rm(*msgid) {
                eprintln!("ipcrm: {}: {}: {}", gettext("message queue id"), msgid, e);
                exit_code = 1;
            }
        }
    }

    // macOS doesn't support SysV message queues - report error if options used
    #[cfg(target_os = "macos")]
    {
        for msgkey in &args.msgkey {
            eprintln!(
                "ipcrm: {}: 0x{:x}: {}",
                gettext("message queue key"),
                *msgkey,
                gettext("message queues not supported on this system")
            );
            exit_code = 1;
        }
        for msgid in &args.msgid {
            eprintln!(
                "ipcrm: {}: {}: {}",
                gettext("message queue id"),
                msgid,
                gettext("message queues not supported on this system")
            );
            exit_code = 1;
        }
    }

    exit_code
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let exit_code = remove_ipcs(&args);

    std::process::exit(exit_code)
}
