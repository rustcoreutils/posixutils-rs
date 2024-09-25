//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
#[cfg(not(target_os = "macos"))]
use libc::{msgctl, msgget, msqid_ds};
use libc::{semctl, semget, shmctl, shmget, shmid_ds};
use plib::PROJECT_NAME;
use std::ffi::{c_int, c_ushort};
use std::io::{self, Error, ErrorKind};
use std::ptr;

/// ipcrm - remove an XSI message queue, semaphore set, or shared memory segment identifier
#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// Remove the shared memory identifier semid from the system.
    #[arg(short = 's', long)]
    semid: Option<i32>,

    /// Remove the shared memory identifier, created with key semkey, from the system.
    #[arg(short = 'S', long)]
    semkey: Option<i32>,

    /// Remove the shared memory identifier shmid from the system.
    #[arg(short = 'm', long)]
    shmid: Option<i32>,

    /// Remove the shared memory identifier, created with key shmkey, from the system.
    #[arg(short = 'M', long)]
    shmkey: Option<i32>,

    /// Remove the message queue identifier msgid from the system.
    #[cfg(not(target_os = "macos"))]
    #[arg(short = 'q', long)]
    msgid: Option<i32>,

    /// Remove the message queue identifier, created with key msgkey, from the system.
    #[cfg(not(target_os = "macos"))]
    #[arg(short = 'Q', long)]
    msgkey: Option<i32>,
}

#[cfg(not(target_os = "macos"))]
fn msg_key_lookup(msgkey: i32) -> io::Result<i32> {
    if msgkey == libc::IPC_PRIVATE {
        return Err(Error::new(ErrorKind::Other, "Invalid key"));
    }
    let res: i32 = unsafe { msgget(msgkey, 0) };

    if res < 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(res)
    }
}

#[cfg(not(target_os = "macos"))]
fn msg_rm(msgid: i32) -> io::Result<i32> {
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
        Ok(res)
    }
}

fn shm_key_lookup(shmkey: i32) -> io::Result<i32> {
    if shmkey == libc::IPC_PRIVATE {
        return Err(Error::new(ErrorKind::Other, "Invalid key"));
    }
    let res: i32 = unsafe { shmget(shmkey, 0, 0) };

    if res < 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(res)
    }
}

fn shm_rm(shmid: i32) -> io::Result<i32> {
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
        Ok(res)
    }
}

fn sem_key_lookup(semkey: i32) -> io::Result<i32> {
    if semkey == libc::IPC_PRIVATE {
        return Err(Error::new(ErrorKind::Other, "Invalid key"));
    }
    let res: i32 = unsafe { semget(semkey, 0, 0) };

    if res < 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(res)
    }
}

// Define the union semun as per your requirements
#[repr(C)]
union semun {
    val: c_int,               // for SETVAL
    buf: *mut libc::semid_ds, // for IPC_STAT and IPC_SET
    array: *mut c_ushort,     // for GETALL and SETALL
                              // Depending on your platform, you might need to add other fields as well
}

fn sem_rm(semid: i32) -> io::Result<i32> {
    let arg = semun { val: 0 };

    let res: i32 = unsafe { semctl(semid, 0, libc::IPC_RMID, arg) };

    if res < 0 {
        Err(io::Error::last_os_error())
    } else {
        Ok(res)
    }
}

fn remove_ipcs(args: &Args) -> io::Result<()> {
    // remove semaphores
    if let Some(semkey) = args.semkey {
        let semid = sem_key_lookup(semkey)?;
        sem_rm(semid)?;
    }
    if let Some(semid) = args.semid {
        sem_rm(semid)?;
    }

    // remove shared memory segments
    if let Some(shmkey) = args.shmkey {
        let shmid = shm_key_lookup(shmkey)?;
        shm_rm(shmid)?;
    }
    if let Some(shmid) = args.shmid {
        shm_rm(shmid)?;
    }

    // remove message queues
    #[cfg(not(target_os = "macos"))]
    {
        if let Some(msgkey) = args.msgkey {
            let msgid = msg_key_lookup(msgkey)?;
            msg_rm(msgid)?;
        }
        if let Some(msgid) = args.msgid {
            msg_rm(msgid)?;
        }
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut exit_code = 0;

    if let Err(e) = remove_ipcs(&args) {
        exit_code = 1;
        eprintln!("{}", e);
    }

    std::process::exit(exit_code)
}
