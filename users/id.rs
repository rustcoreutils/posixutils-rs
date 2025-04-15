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

use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, setlocale, textdomain};
use plib::group;
use std::collections::HashMap;
use std::io::Error;

/// id - return user identity
#[derive(Parser)]
#[command(version, about)]
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
    let groups = group::load();

    for group in &groups {
        // skip groups that the user is not a member of
        let mut found = false;
        for member in &group.members {
            if *member == userinfo.username {
                found = true;
                break;
            }
        }
        if !found && group.gid != userinfo.gid {
            continue;
        }

        // add group to user's group list
        userinfo.groups.push(group.gid);
        userinfo.group_names.insert(group.gid, group.name.clone());
    }

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
            print!("{}({}),", gid, userinfo.group_names[gid]);
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
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs")?;
    bind_textdomain_codeset("posixutils-rs", "UTF-8")?;

    let args = Args::parse();

    let mut userinfo = get_user_info(&args)?;
    get_group_info(&mut userinfo)?;

    display_user_info(&args, &userinfo);

    Ok(())
}
