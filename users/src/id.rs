//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// TODO:
// - bug: only one group is returned, in group list (MacOS-only?)
//

extern crate clap;
extern crate libc;
extern crate plib;

use clap::Parser;
use gettextrs::{bind_textdomain_codeset, textdomain};
use plib::PROJECT_NAME;
use std::collections::HashMap;
use std::io::Error;

/// id - return user identity
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Output all different group IDs (effective, real, and supplementary) only, using a numeric format
    #[arg(short = 'G', long, group = "output")]
    groups: bool,

    /// Output only the effective group ID, using the numeric format
    #[arg(short, long, group = "output")]
    group: bool,

    /// Output only the effective user ID, using the numeric format
    #[arg(short = 'u', long = "user", group = "output")]
    e_user: bool,

    /// Output the real ID instead of the effective ID.
    #[arg(short, long)]
    real: bool,

    /// Output the name in string format, instead of the numeric
    #[arg(short, long)]
    name: bool,

    /// The login name for which information is to be written.
    user: Option<String>,
}

struct UserInfo {
    uid: libc::uid_t,
    gid: libc::gid_t,
    euid: libc::uid_t,
    egid: libc::gid_t,
    groups: Vec<libc::gid_t>,

    username: String,
    group_names: HashMap<libc::gid_t, String>,
}

fn userinfo_process(userinfo: &mut UserInfo) -> Result<(), Box<dyn std::error::Error>> {
    userinfo.uid = unsafe { libc::getuid() };
    userinfo.gid = unsafe { libc::getgid() };
    userinfo.euid = unsafe { libc::geteuid() };
    userinfo.egid = unsafe { libc::getegid() };

    let passwd = unsafe { libc::getpwuid(userinfo.uid) };
    if passwd.is_null() {
        let err = Error::last_os_error();
        eprintln!("getpwuid: {}", err);
        return Err(Box::new(err));
    }

    userinfo.username = unsafe {
        std::ffi::CStr::from_ptr((*passwd).pw_name)
            .to_string_lossy()
            .to_string()
    };

    Ok(())
}

fn userinfo_name(userinfo: &mut UserInfo, user: &str) -> Result<(), Box<dyn std::error::Error>> {
    let user_str = std::ffi::CString::new(user).unwrap();
    let passwd = unsafe { libc::getpwnam(user_str.as_ptr()) };
    if passwd.is_null() {
        let err = Error::last_os_error();
        eprintln!("getpwnam: {}", err);
        return Err(Box::new(err));
    }

    unsafe {
        userinfo.uid = (*passwd).pw_uid;
        userinfo.gid = (*passwd).pw_gid;
    }
    userinfo.euid = userinfo.uid;
    userinfo.egid = userinfo.gid;
    userinfo.username = unsafe {
        std::ffi::CStr::from_ptr((*passwd).pw_name)
            .to_string_lossy()
            .to_string()
    };

    Ok(())
}

fn get_user_info(args: &Args) -> Result<UserInfo, Box<dyn std::error::Error>> {
    let mut userinfo = UserInfo {
        uid: 0,
        gid: 0,
        euid: 0,
        egid: 0,
        groups: Vec::new(),
        username: String::new(),
        group_names: HashMap::new(),
    };

    // fill in uid, gid
    match args.user {
        None => userinfo_process(&mut userinfo)?,
        Some(ref user) => userinfo_name(&mut userinfo, user)?,
    }

    Ok(userinfo)
}

fn get_group_info(userinfo: &mut UserInfo) -> Result<(), Box<dyn std::error::Error>> {
    let mut group_names: HashMap<libc::gid_t, String> = HashMap::new();

    let mut group = unsafe { libc::getgrgid(userinfo.gid) };
    if group.is_null() {
        let err = Error::last_os_error();
        eprintln!("getgrgid({}): {}", userinfo.gid, err);
        return Err(Box::new(err));
    }
    let group_name = unsafe {
        std::ffi::CStr::from_ptr((*group).gr_name)
            .to_string_lossy()
            .to_string()
    };
    group_names.insert(userinfo.gid, group_name);
    userinfo.groups.push(userinfo.gid);

    group = unsafe { libc::getgrgid(userinfo.egid) };
    if group.is_null() {
        let err = Error::last_os_error();
        eprintln!("getgrgid: {}", err);
        return Err(Box::new(err));
    }
    let group_name = unsafe {
        std::ffi::CStr::from_ptr((*group).gr_name)
            .to_string_lossy()
            .to_string()
    };
    if userinfo.egid != userinfo.gid {
        userinfo.groups.push(userinfo.egid);
    }
    group_names.insert(userinfo.egid, group_name);

    for gid in &userinfo.groups {
        group = unsafe { libc::getgrgid(*gid) };
        if group.is_null() {
            let err = Error::last_os_error();
            eprintln!("getgrgid: {}", err);
            return Err(Box::new(err));
        }
        let group_name = unsafe {
            std::ffi::CStr::from_ptr((*group).gr_name)
                .to_string_lossy()
                .to_string()
        };
        group_names.insert(*gid, group_name);
    }

    userinfo.groups.sort();
    userinfo.group_names = group_names;

    Ok(())
}

fn display_user_info(args: &Args, userinfo: &UserInfo) {
    if args.e_user {
        println!("{}", userinfo.euid);
        return;
    }

    if args.group {
        println!("{}", userinfo.egid);
        return;
    }

    if args.groups {
        for gid in &userinfo.groups {
            print!("{} ", gid);
        }
        println!();
        return;
    }

    if args.name {
        let group_name = {
            match userinfo.group_names.get(&userinfo.egid) {
                None => "unknown",
                Some(name) => name,
            }
        };
        println!(
            "uid={}({}) gid={}({}) groups={}",
            userinfo.uid, userinfo.username, userinfo.gid, group_name, userinfo.egid
        );
        for gid in &userinfo.groups {
            print!("{},", userinfo.group_names[gid]);
        }
        println!();
        return;
    }

    println!(
        "uid={} gid={} groups={}",
        userinfo.uid, userinfo.gid, userinfo.egid
    );
    for gid in &userinfo.groups {
        print!("{},", gid);
    }
    println!();
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // parse command line arguments
    let args = Args::parse();

    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;

    let mut userinfo = get_user_info(&args)?;
    get_group_info(&mut userinfo)?;

    display_user_info(&args, &userinfo);

    Ok(())
}
