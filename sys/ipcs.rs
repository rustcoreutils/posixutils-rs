//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

extern crate clap;
extern crate gettextrs;
extern crate plib;

use chrono::Local;
use clap::Parser;
use gettextrs::{bind_textdomain_codeset, gettext, setlocale, textdomain, LocaleCategory};
use plib::PROJECT_NAME;

/// ipcs - report XSI interprocess communication facilities status
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Write information about active message queues.
    #[arg(short = 'q', long = "queues")]
    message_queues: bool,

    /// Write information about active shared memory segments.
    #[arg(short = 'm', long = "shmems")]
    shared_memory: bool,

    /// Write information about active semaphore sets.
    #[arg(short, long)]
    semaphores: bool,

    /// Use all print options.
    #[arg(short, long)]
    all: bool,

    /// Write information on maximum allowable size.
    #[arg(short = 'b', long = "max-size")]
    max_size: bool,

    /// Write creator's user name and group name.
    #[arg(short, long)]
    creator: bool,

    /// Write information on outstanding usage.
    #[arg(short, long)]
    outstanding: bool,

    /// Write process number information.
    #[arg(short, long)]
    pid: bool,

    /// Write time information.
    #[arg(short, long)]
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

fn display_message_queues(_args: &Args) {
    #[cfg(not(target_os = "macos"))]
    use std::ffi::CStr;

    println!("Message Queues:");
    println!("T     ID     KEY        MODE       OWNER    GROUP");

    #[cfg(not(target_os = "macos"))]
    {
        use libc::{msgctl, msqid_ds, IPC_STAT};

        let mut msqid: i32 = 0;
        let mut msg_ds: msqid_ds = unsafe { std::mem::zeroed() };

        loop {
            if unsafe { msgctl(msqid, IPC_STAT, &mut msg_ds) } == -1 {
                break;
            }

            let key = {
                #[cfg(not(target_env = "musl"))]
                {
                    msg_ds.msg_perm.__key // Ensure the correct field name for your system
                }

                // TODO: What placeholder value should go here?
                #[cfg(target_env = "musl")]
                {
                    0_i32
                }
            };
            let mode = msg_ds.msg_perm.mode;
            let uid = msg_ds.msg_perm.uid;
            let gid = msg_ds.msg_perm.gid;

            let owner = unsafe {
                CStr::from_ptr(libc::getpwuid(uid).as_ref().unwrap().pw_name)
                    .to_str()
                    .unwrap()
            };
            let group = unsafe {
                CStr::from_ptr(libc::getgrgid(gid).as_ref().unwrap().gr_name)
                    .to_str()
                    .unwrap()
            };

            let mode_str = format!(
                "{}{}{}",
                if mode & 0o400 != 0 { "r" } else { "-" },
                if mode & 0o200 != 0 { "w" } else { "-" },
                if mode & 0o100 != 0 { "a" } else { "-" }
            );

            println!(
                "q     {:<5}  0x{:08x}  {:<10}  {:<8}  {:<8}",
                msqid, key, mode_str, owner, group
            );

            msqid += 1;
        }
    }

    #[cfg(target_os = "macos")]
    {
        println!("{}", gettext("Message Queue facility not in system."));
    }
}

fn display_shared_memory(_args: &Args) {
    use libc::{shmctl, shmid_ds, IPC_STAT};
    use std::ffi::CStr;

    #[cfg(target_os = "macos")]
    const SHM_INFO: libc::c_int = 14; // SHM_INFO is typically 14 in Linux but this is not standard

    #[cfg(not(target_os = "macos"))]
    const SHM_INFO: libc::c_int = 14;

    let mut shmbuf: shmid_ds = unsafe { std::mem::zeroed() };

    let maxid = unsafe { shmctl(0, SHM_INFO, &mut shmbuf) };
    if maxid < 0 {
        println!("{}", gettext("Shared Memory facility not in system."));
        return;
    }

    println!("Shared Memory:");
    println!("T     ID     KEY        MODE       OWNER    GROUP");

    for shmid in 0..=maxid {
        if unsafe { shmctl(shmid, IPC_STAT, &mut shmbuf) } == -1 {
            continue;
        }

        // Prevent accidental shadowing by using a block
        let key = {
            #[cfg(target_os = "macos")]
            {
                shmbuf.shm_perm._key // Check for the correct field name on your system
            }

            #[cfg(all(not(target_os = "macos"), not(target_env = "musl")))]
            {
                shmbuf.shm_perm.__key // Check for the correct field name on your system
            }

            // TODO: What placeholder value should go here?
            #[cfg(all(not(target_os = "macos"), target_env = "musl"))]
            {
                0_i32
            }
        };
        let mode = shmbuf.shm_perm.mode;
        let uid = shmbuf.shm_perm.uid;
        let gid = shmbuf.shm_perm.gid;

        let owner = unsafe {
            CStr::from_ptr(libc::getpwuid(uid).as_ref().unwrap().pw_name)
                .to_str()
                .unwrap()
        };
        let group = unsafe {
            CStr::from_ptr(libc::getgrgid(gid).as_ref().unwrap().gr_name)
                .to_str()
                .unwrap()
        };

        let mode_str = format!(
            "{}{}{}",
            if mode & 0o400 != 0 { "r" } else { "-" },
            if mode & 0o200 != 0 { "w" } else { "-" },
            if mode & 0o100 != 0 { "a" } else { "-" }
        );

        println!(
            "m     {:<5}  0x{:08x}  {:<10}  {:<8}  {:<8}",
            shmid, key, mode_str, owner, group
        );
    }
}

#[cfg(not(target_env = "musl"))]
fn display_semaphores(_args: &Args) {
    use libc::{semctl, semid_ds, IPC_STAT};
    use std::ffi::CStr;

    let mut semid: i32 = 0;
    let mut sem_ds: semid_ds = unsafe { std::mem::zeroed() };

    println!("Semaphores:");
    println!("T     ID     KEY        MODE       OWNER    GROUP    NSEMS");

    loop {
        if unsafe { semctl(semid, 0, IPC_STAT, &mut sem_ds) } == -1 {
            break;
        }

        #[cfg(not(target_os = "macos"))]
        let key = sem_ds.sem_perm.__key; // Check for the correct field name on your system
        #[cfg(target_os = "macos")]
        let key = sem_ds.sem_perm._key; // Check for the correct field name on your system

        let mode = sem_ds.sem_perm.mode;
        let uid = sem_ds.sem_perm.uid;
        let gid = sem_ds.sem_perm.gid;

        let owner = unsafe {
            CStr::from_ptr(libc::getpwuid(uid).as_ref().unwrap().pw_name)
                .to_str()
                .unwrap()
        };
        let group = unsafe {
            CStr::from_ptr(libc::getgrgid(gid).as_ref().unwrap().gr_name)
                .to_str()
                .unwrap()
        };

        let mode_str = format!(
            "{}{}{}",
            if mode & 0o400 != 0 { "r" } else { "-" },
            if mode & 0o200 != 0 { "w" } else { "-" },
            if mode & 0o100 != 0 { "a" } else { "-" }
        );

        println!(
            "s     {:<5}  0x{:08x}  {:<10}  {:<8}  {:<8}  {:<5}",
            semid, key, mode_str, owner, group, sem_ds.sem_nsems
        );

        semid += 1;
    }
}

#[cfg(target_env = "musl")]
fn display_semaphores(_args: &Args) {
    // TODO
    unimplemented!();
}

fn get_current_date() -> String {
    // Retrieve the current date and time in a human-readable format
    let now = Local::now();
    now.format("%Y-%m-%d %H:%M:%S").to_string()
}

fn display_ipc_status(args: &Args) {
    println!("IPC status from {} as of {}", "source", get_current_date());

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
    // Parse command line arguments
    let mut args = Args::parse();

    // Set locale and text domain for localization
    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    // Validate arguments and determine what to display
    if args.all {
        // Enable all options
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
