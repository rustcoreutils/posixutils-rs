//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::{LocaleCategory, bind_textdomain_codeset, gettext, setlocale, textdomain};
use plib::group;
use std::collections::{HashMap, HashSet};
use std::ffi::CStr;
use std::io::{self, Write};
use std::process::ExitCode;

/// id - return user identity
#[derive(Parser)]
#[command(version, about = gettext("id - return user identity"))]
struct Args {
    #[arg(short = 'G', long, group = "output", help = gettext("Output all different group IDs (effective, real, and supplementary) only"))]
    groups: bool,

    #[arg(short, long, group = "output", help = gettext("Output only the effective group ID"))]
    group: bool,

    #[arg(short = 'u', long = "user", group = "output", help = gettext("Output only the effective user ID"))]
    e_user: bool,

    #[arg(short, long, help = gettext("Output the real ID instead of the effective ID"))]
    real: bool,

    #[arg(short, long, help = gettext("Output the name in string format, instead of the numeric"))]
    name: bool,

    #[arg(help = gettext("The login name for which information is to be written"))]
    user: Option<String>,
}

struct UserInfo {
    uid: libc::uid_t,
    gid: libc::gid_t,
    euid: libc::uid_t,
    egid: libc::gid_t,
    groups: Vec<libc::gid_t>,

    username: String,
    eusername: String,
    group_names: HashMap<libc::gid_t, String>,
}

/// Get supplementary groups for the current process using getgroups() syscall.
fn get_process_groups() -> Vec<libc::gid_t> {
    // First call with 0 to get the number of groups
    let ngroups = unsafe { libc::getgroups(0, std::ptr::null_mut()) };
    if ngroups <= 0 {
        return Vec::new();
    }

    let mut groups = vec![0 as libc::gid_t; ngroups as usize];
    let n = unsafe { libc::getgroups(ngroups, groups.as_mut_ptr()) };
    if n > 0 {
        groups.truncate(n as usize);
        groups
    } else {
        Vec::new()
    }
}

/// Look up username for a given uid.
fn get_username(uid: libc::uid_t) -> Option<String> {
    let passwd = unsafe { libc::getpwuid(uid) };
    if passwd.is_null() {
        return None;
    }
    Some(unsafe {
        CStr::from_ptr((*passwd).pw_name)
            .to_string_lossy()
            .to_string()
    })
}

/// Look up group name for a given gid.
fn get_groupname(gid: libc::gid_t) -> Option<String> {
    let grp = unsafe { libc::getgrgid(gid) };
    if grp.is_null() {
        return None;
    }
    Some(unsafe { CStr::from_ptr((*grp).gr_name).to_string_lossy().to_string() })
}

fn userinfo_process(userinfo: &mut UserInfo) -> Result<(), String> {
    userinfo.uid = unsafe { libc::getuid() };
    userinfo.gid = unsafe { libc::getgid() };
    userinfo.euid = unsafe { libc::geteuid() };
    userinfo.egid = unsafe { libc::getegid() };

    // Get username for real uid
    userinfo.username = get_username(userinfo.uid)
        .ok_or_else(|| format!("id: cannot find name for user ID {}", userinfo.uid))?;

    // Get username for effective uid (may be same as real)
    userinfo.eusername = if userinfo.euid == userinfo.uid {
        userinfo.username.clone()
    } else {
        get_username(userinfo.euid).unwrap_or_default()
    };

    // Get supplementary groups using getgroups() syscall
    userinfo.groups = get_process_groups();

    Ok(())
}

fn userinfo_name(userinfo: &mut UserInfo, user: &str) -> Result<(), String> {
    let user_str = std::ffi::CString::new(user).map_err(|_| "invalid username".to_string())?;
    let passwd = unsafe { libc::getpwnam(user_str.as_ptr()) };
    if passwd.is_null() {
        return Err(format!("id: {}: no such user", user));
    }

    unsafe {
        userinfo.uid = (*passwd).pw_uid;
        userinfo.gid = (*passwd).pw_gid;
    }
    // For a named user, effective IDs are same as real IDs
    userinfo.euid = userinfo.uid;
    userinfo.egid = userinfo.gid;

    userinfo.username = unsafe {
        CStr::from_ptr((*passwd).pw_name)
            .to_string_lossy()
            .to_string()
    };
    userinfo.eusername = userinfo.username.clone();

    Ok(())
}

fn get_user_info(args: &Args) -> Result<UserInfo, String> {
    let mut userinfo = UserInfo {
        uid: 0,
        gid: 0,
        euid: 0,
        egid: 0,
        groups: Vec::new(),
        username: String::new(),
        eusername: String::new(),
        group_names: HashMap::new(),
    };

    match args.user {
        None => userinfo_process(&mut userinfo)?,
        Some(ref user) => userinfo_name(&mut userinfo, user)?,
    }

    Ok(userinfo)
}

fn get_group_info(userinfo: &mut UserInfo, is_named_user: bool) {
    // Build group name lookup map for all groups we might need
    let mut seen_gids = HashSet::new();

    // Always include primary group
    if let Some(name) = get_groupname(userinfo.gid) {
        userinfo.group_names.insert(userinfo.gid, name);
    }
    seen_gids.insert(userinfo.gid);

    // Include effective group if different
    if userinfo.egid != userinfo.gid {
        if let Some(name) = get_groupname(userinfo.egid) {
            userinfo.group_names.insert(userinfo.egid, name);
        }
        seen_gids.insert(userinfo.egid);
    }

    if is_named_user {
        // For named user lookup, scan /etc/group for membership
        let groups = group::load();
        let mut user_groups = Vec::new();

        // Add primary group first
        user_groups.push(userinfo.gid);

        for grp in &groups {
            // Skip if already added
            if seen_gids.contains(&grp.gid) {
                continue;
            }

            // Check if user is a member of this group
            if grp.members.iter().any(|m| m == &userinfo.username) {
                user_groups.push(grp.gid);
                userinfo.group_names.insert(grp.gid, grp.name.clone());
                seen_gids.insert(grp.gid);
            }
        }

        userinfo.groups = user_groups;
    } else {
        // For current process, groups are already set from getgroups()
        // Just need to look up names and deduplicate
        let mut unique_groups = Vec::new();

        // Ensure primary group is first
        unique_groups.push(userinfo.gid);

        for &gid in &userinfo.groups {
            if !seen_gids.contains(&gid) {
                unique_groups.push(gid);
                if let Some(name) = get_groupname(gid) {
                    userinfo.group_names.insert(gid, name);
                }
                seen_gids.insert(gid);
            }
        }

        userinfo.groups = unique_groups;
    }
}

fn display_user_info(args: &Args, userinfo: &UserInfo) -> io::Result<()> {
    let stdout = io::stdout();
    let mut out = stdout.lock();

    // -u: Output only effective user ID (or real if -r)
    if args.e_user {
        let uid = if args.real {
            userinfo.uid
        } else {
            userinfo.euid
        };
        if args.name {
            let name = if args.real {
                &userinfo.username
            } else {
                &userinfo.eusername
            };
            if name.is_empty() {
                // If name not found, output numeric per POSIX
                writeln!(out, "{}", uid)?;
            } else {
                writeln!(out, "{}", name)?;
            }
        } else {
            writeln!(out, "{}", uid)?;
        }
        return Ok(());
    }

    // -g: Output only effective group ID (or real if -r)
    if args.group {
        let gid = if args.real {
            userinfo.gid
        } else {
            userinfo.egid
        };
        if args.name {
            if let Some(name) = userinfo.group_names.get(&gid) {
                writeln!(out, "{}", name)?;
            } else if let Some(name) = get_groupname(gid) {
                writeln!(out, "{}", name)?;
            } else {
                // If name not found, output numeric per POSIX
                writeln!(out, "{}", gid)?;
            }
        } else {
            writeln!(out, "{}", gid)?;
        }
        return Ok(());
    }

    // -G: Output all different group IDs
    if args.groups {
        let mut first = true;
        for gid in &userinfo.groups {
            if !first {
                write!(out, " ")?;
            }
            first = false;

            if args.name {
                if let Some(name) = userinfo.group_names.get(gid) {
                    write!(out, "{}", name)?;
                } else if let Some(name) = get_groupname(*gid) {
                    write!(out, "{}", name)?;
                } else {
                    // If name not found, output numeric per POSIX
                    write!(out, "{}", gid)?;
                }
            } else {
                write!(out, "{}", gid)?;
            }
        }
        writeln!(out)?;
        return Ok(());
    }

    // Default output format: uid=UID(username) gid=GID(groupname) [euid=...] [egid=...] groups=...

    // uid=UID(username)
    write!(out, "uid={}", userinfo.uid)?;
    if !userinfo.username.is_empty() {
        write!(out, "({})", userinfo.username)?;
    }

    // gid=GID(groupname)
    write!(out, " gid={}", userinfo.gid)?;
    if let Some(name) = userinfo.group_names.get(&userinfo.gid) {
        write!(out, "({})", name)?;
    }

    // euid=EUID(eusername) - only if different from uid
    if userinfo.euid != userinfo.uid {
        write!(out, " euid={}", userinfo.euid)?;
        if !userinfo.eusername.is_empty() {
            write!(out, "({})", userinfo.eusername)?;
        }
    }

    // egid=EGID(egroupname) - only if different from gid
    if userinfo.egid != userinfo.gid {
        write!(out, " egid={}", userinfo.egid)?;
        if let Some(name) = userinfo.group_names.get(&userinfo.egid) {
            write!(out, "({})", name)?;
        }
    }

    // groups=GID(name),GID(name),...
    if !userinfo.groups.is_empty() {
        write!(out, " groups=")?;
        let mut first = true;
        for gid in &userinfo.groups {
            if !first {
                write!(out, ",")?;
            }
            first = false;

            write!(out, "{}", gid)?;
            if let Some(name) = userinfo.group_names.get(gid) {
                write!(out, "({})", name)?;
            }
        }
    }

    writeln!(out)?;
    Ok(())
}

fn main() -> ExitCode {
    setlocale(LocaleCategory::LcAll, "");
    textdomain("posixutils-rs").ok();
    bind_textdomain_codeset("posixutils-rs", "UTF-8").ok();

    let args = Args::parse();

    let mut userinfo = match get_user_info(&args) {
        Ok(info) => info,
        Err(e) => {
            eprintln!("{}", e);
            return ExitCode::from(1);
        }
    };

    get_group_info(&mut userinfo, args.user.is_some());

    if let Err(e) = display_user_info(&args, &userinfo) {
        eprintln!("id: write error: {}", e);
        return ExitCode::from(1);
    }

    ExitCode::SUCCESS
}
