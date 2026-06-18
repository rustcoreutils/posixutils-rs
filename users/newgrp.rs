//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::Parser;
use gettextrs::gettext;
use libc::{getgid, getgroups, getlogin, getpwnam, getpwuid, getuid, gid_t, passwd};

#[cfg(target_os = "linux")]
use libc::{ECHO, ECHONL, TCSANOW};
#[cfg(target_os = "linux")]
use libcrypt_rs::Crypt;
use plib::group::Group;

use std::ffi::{CStr, CString};
use std::io;
use std::os::unix::process::CommandExt;
use std::path::Path;
use std::process::{self, Command};

#[cfg(target_os = "linux")]
use std::{
    fs::File,
    io::{BufRead, BufReader},
    os::unix::io::AsRawFd,
};

#[cfg(target_os = "linux")]
const GROUPSHADOW_PATH: &str = "/etc/gshadow";

#[cfg(target_os = "linux")]
const MAX_GROUPS: usize = libc::KERN_NGROUPS_MAX as usize;

#[cfg(target_os = "macos")]
const MAX_GROUPS: usize = 16;

#[derive(Parser)]
#[command(version, about = gettext("newgrp — change to a new group"))]
struct Args {
    #[arg(
        short = 'l',
        help = gettext("Change the environment to what would be expected if the user actually logged in again (letter 'l').")
    )]
    login: bool,

    #[arg(
        value_name = "GROUP",
        help = gettext("Group name or non-negative numeric group ID. With no operand, restore the user's login groups.")
    )]
    group: Option<String>,
}

/// Top-level flow.
///
/// newgrp creates a new shell execution environment with a new group identity.
/// Per POSIX, a failure to assign the new group identification (for security or
/// password reasons) shall NOT prevent the shell from being created — so a
/// group-change failure is reported as a diagnostic and we still exec the
/// shell. The function never returns on success (it execs the shell).
fn run(args: &Args) -> ! {
    let pwd = match get_password() {
        Ok(p) => p,
        Err(_) => {
            plib::diag::error(&gettext("could not retrieve current user information"));
            process::exit(1);
        }
    };

    // Attempt the group change; a failure is non-fatal (invoke the shell anyway).
    if let Err(msg) = change_group(args, &pwd) {
        plib::diag::error(&msg);
    }

    // SECURITY: drop any set-user-ID privilege before exec'ing the shell, so an
    // installed-setuid-root newgrp never hands the user a root shell. This must
    // run after the (privileged) group change. (The audit's suggestion to
    // remove this call was incorrect — it is the privilege drop, not dead code.)
    if unsafe { libc::setuid(getuid()) } != 0 {
        plib::diag::error(&gettext(
            "failed to drop privileges; refusing to exec shell",
        ));
        process::exit(1);
    }

    exec_shell(args.login, &pwd);
}

/// Change the process group identity per the operand, or restore the user's
/// login groups when no operand is given.
fn change_group(args: &Args, pwd: &passwd) -> Result<(), String> {
    match args.group.as_deref() {
        None => restore_login_groups(pwd),
        Some(id) => switch_to_group(id.trim(), pwd),
    }
}

/// With no operand, restore the effective/real group to the user's primary
/// group and the supplementary list to the user's group-database entries.
fn restore_login_groups(pwd: &passwd) -> Result<(), String> {
    // initgroups() needs to run while privileged; it sets the supplementary
    // list (including the base gid) from the group database.
    if unsafe { libc::initgroups(pwd.pw_name, pwd.pw_gid as _) } != 0 {
        return Err(format!(
            "{}: {}",
            gettext("cannot restore supplementary groups"),
            io::Error::last_os_error()
        ));
    }
    if unsafe { libc::setgid(pwd.pw_gid) } != 0 {
        return Err(format!(
            "{}: {}",
            gettext("cannot restore group ID"),
            io::Error::last_os_error()
        ));
    }
    Ok(())
}

/// Switch to the named/numeric group: resolve it, check permission (possibly
/// prompting for the group password), then apply the change.
fn switch_to_group(identifier: &str, pwd: &passwd) -> Result<(), String> {
    let groups = plib::group::load();
    let group = find_matching_group(identifier, &groups)
        .ok_or_else(|| format!("{}: {}", gettext("no such group"), identifier))?;

    check_perms(&group, pwd).map_err(|e| e.to_string())?;

    apply_group_change(group.gid)
}

/// Apply the new group id and adjust the supplementary group list per
/// POSIX.1-2024 newgrp (the four supplementary-group cases). The real and
/// effective group IDs are set to `new_gid`; supplementary-group membership is
/// adjusted so the previous identity is preserved where the spec requires.
fn apply_group_change(new_gid: gid_t) -> Result<(), String> {
    let current_gid = unsafe { getgid() };
    let mut supp = get_supplementary_gids()
        .map_err(|e| format!("{}: {}", gettext("cannot read supplementary groups"), e))?;

    let old_in_supp = supp.contains(&current_gid);
    let new_in_supp = supp.contains(&new_gid);

    if old_in_supp {
        // Old effective gid is in the supplementary list.
        if !new_in_supp && supp.len() < MAX_GROUPS {
            supp.push(new_gid);
        }
    } else {
        // Old effective gid is NOT in the supplementary list.
        if new_in_supp {
            supp.retain(|&g| g != new_gid);
        } else if supp.len() < MAX_GROUPS {
            supp.push(current_gid);
        }
    }

    // Apply the supplementary list first (while still privileged), checking the
    // result (the old code discarded setgroups failures).
    set_supplementary_gids(&supp)
        .map_err(|e| format!("{}: {}", gettext("cannot set supplementary groups"), e))?;

    // Set real+effective (and, when privileged, saved) group ID.
    if unsafe { libc::setgid(new_gid) } != 0 {
        return Err(format!(
            "{}: {}",
            gettext("cannot set group ID"),
            io::Error::last_os_error()
        ));
    }

    Ok(())
}

/// Exec the shell, replacing the current process. Never returns on success.
///
/// `-l` re-initializes the environment as if logging in (login shell argv0
/// `-name`, HOME/SHELL/USER/LOGNAME set, working directory = HOME). The default
/// (non-login) form retains the current environment and working directory and
/// honors $SHELL.
fn exec_shell(login: bool, pwd: &passwd) -> ! {
    let pw_shell = unsafe { CStr::from_ptr(pwd.pw_shell) }
        .to_str()
        .unwrap_or("");

    let shell = if login {
        if pw_shell.is_empty() {
            "/bin/sh".to_string()
        } else {
            pw_shell.to_string()
        }
    } else {
        std::env::var("SHELL")
            .ok()
            .filter(|s| !s.is_empty())
            .unwrap_or_else(|| {
                if pw_shell.is_empty() {
                    "/bin/sh".to_string()
                } else {
                    pw_shell.to_string()
                }
            })
    };

    let mut cmd = Command::new(&shell);

    if login {
        let base = Path::new(&shell)
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or(shell.as_str());
        // Login shells receive argv0 prefixed with '-'.
        cmd.arg0(format!("-{}", base));

        let home = unsafe { CStr::from_ptr(pwd.pw_dir) }.to_str().unwrap_or("");
        let user = unsafe { CStr::from_ptr(pwd.pw_name) }
            .to_str()
            .unwrap_or("");
        cmd.env("HOME", home)
            .env("SHELL", &shell)
            .env("USER", user)
            .env("LOGNAME", user);
        if !home.is_empty() {
            cmd.current_dir(home);
        }
    }

    // exec() only returns on failure.
    let err = cmd.exec();
    plib::exec::exec_error_exit(&shell, err);
}

/// Retrieves the current supplementary group IDs for the calling process.
fn get_supplementary_gids() -> Result<Vec<gid_t>, io::Error> {
    let mut supplementary_gids: [gid_t; MAX_GROUPS] = [0; MAX_GROUPS];
    let num_groups = unsafe {
        getgroups(
            supplementary_gids.len() as i32,
            supplementary_gids.as_mut_ptr(),
        )
    };

    if num_groups < 0 {
        return Err(io::Error::last_os_error());
    }

    Ok(supplementary_gids[..num_groups as usize].to_vec())
}

/// Sets the supplementary group IDs for the calling process.
fn set_supplementary_gids(gids: &[gid_t]) -> Result<(), io::Error> {
    #[cfg(target_os = "macos")]
    let gids_len = gids.len() as i32;
    #[cfg(target_os = "linux")]
    let gids_len = gids.len();

    if unsafe { libc::setgroups(gids_len, gids.as_ptr()) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

/// Retrieves the password entry for the current user based on the login name
/// or user ID (UID).
fn get_password() -> Result<passwd, io::Error> {
    unsafe {
        let login_ptr = getlogin();
        let ruid = getuid();

        if !login_ptr.is_null() {
            if let Ok(login_name) = CStr::from_ptr(login_ptr).to_str() {
                if !login_name.is_empty() {
                    if let Ok(c_login_name) = CString::new(login_name) {
                        let pw = getpwnam(c_login_name.as_ptr());
                        // Only trust getlogin() if its uid matches the real uid.
                        if !pw.is_null() && (*pw).pw_uid == ruid {
                            return Ok(*pw);
                        }
                    }
                }
            }
        }

        // Fall back to the real UID's password entry.
        let pw_by_uid = getpwuid(ruid);
        if !pw_by_uid.is_null() {
            return Ok(*pw_by_uid);
        }

        Err(io::Error::new(io::ErrorKind::NotFound, "no password entry"))
    }
}

/// Finds a matching group by numeric GID or by name.
///
/// Per POSIX, if the identifier is a non-negative numeric string that also
/// exists as a group *name*, the named group's GID is preferred.
fn find_matching_group(group_identifier: &str, groups: &[Group]) -> Option<Group> {
    // Spec preference: a numeric operand that is also a group *name* resolves
    // by name first.
    if let Some(group) = groups.iter().find(|g| g.name == group_identifier) {
        return Some(group.clone());
    }

    if let Ok(gid) = group_identifier.parse::<u32>() {
        if let Some(group) = groups.iter().find(|g| g.gid == gid) {
            return Some(group.clone());
        }
    }

    None
}

/// Checks permission to change to `group`. If a password is required and the
/// user is not a member, prompts for the group password and verifies it.
/// Returns `Ok` if permitted, `Err(PermissionDenied)` otherwise.
fn check_perms(group: &Group, password: &passwd) -> Result<(), io::Error> {
    let pw_name = unsafe {
        CStr::from_ptr(password.pw_name)
            .to_string_lossy()
            .into_owned()
    };

    // A member of the group (by primary gid or membership) needs no password.
    let is_member =
        group.gid == password.pw_gid || group.members.iter().any(|member| member == &pw_name);

    if is_member {
        return Ok(());
    }

    // Not a member: a password is required if the group has one. If the group
    // has no password, whether non-members may join is implementation-defined;
    // we deny (fail closed).
    if group.passwd.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            gettext("permission denied"),
        ));
    }

    // Root may always change group.
    if unsafe { getuid() } == 0 {
        return Ok(());
    }

    verify_group_password(group)
}

#[cfg(target_os = "linux")]
fn verify_group_password(group: &Group) -> Result<(), io::Error> {
    let password_input = read_password()?;

    // Prefer the shadow group password; fall back to the group-database field.
    let stored = match get_shadow_password(&group.name)? {
        Some(p) if !p.is_empty() => p,
        _ => group.passwd.clone(),
    };

    let hashed_input = pw_encrypt(&password_input, &stored)?;

    if constant_time_eq(hashed_input.as_bytes(), stored.as_bytes()) {
        Ok(())
    } else {
        Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            gettext("incorrect password"),
        ))
    }
}

#[cfg(target_os = "macos")]
fn verify_group_password(_group: &Group) -> Result<(), io::Error> {
    // macOS group-password verification is not implemented; non-root non-members
    // are denied. (Documented limitation; Linux performs full verification.)
    Err(io::Error::new(
        io::ErrorKind::PermissionDenied,
        gettext("group password verification is not supported on this platform"),
    ))
}

/// Constant-time byte comparison, to avoid leaking match length via timing.
#[cfg(target_os = "linux")]
fn constant_time_eq(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    let mut diff = 0u8;
    for (x, y) in a.iter().zip(b.iter()) {
        diff |= x ^ y;
    }
    diff == 0
}

/// Retrieves the shadow group password for `group_name` from /etc/gshadow.
#[cfg(target_os = "linux")]
fn get_shadow_password(group_name: &str) -> Result<Option<String>, io::Error> {
    let file = match File::open(GROUPSHADOW_PATH) {
        Ok(f) => f,
        // No gshadow (or not readable): fall back to the group-database field.
        Err(_) => return Ok(None),
    };
    let reader = BufReader::new(file);

    for line in reader.lines() {
        let line = line?;
        let mut fields = line.splitn(3, ':');
        if let (Some(name), Some(passwd)) = (fields.next(), fields.next()) {
            if name == group_name {
                return Ok(Some(passwd.to_string()));
            }
        }
    }

    Ok(None)
}

/// Extracts the `$id$params$salt` prefix from a crypt(3) hash.
#[cfg(target_os = "linux")]
fn extract_salt(full_hash: &str) -> Option<String> {
    let parts: Vec<&str> = full_hash.split('$').collect();
    if parts.len() >= 4 {
        Some(format!("${}${}${}", parts[1], parts[2], parts[3]))
    } else {
        None
    }
}

/// Encrypts `clear` using the salt extracted from `stored`. Fails closed if the
/// stored hash is a locked/blocked entry or the salt cannot be extracted.
#[cfg(target_os = "linux")]
fn pw_encrypt(clear: &str, stored: &str) -> Result<String, io::Error> {
    // Locked/disabled accounts (`!`, `*`, `!*`, empty) never match a password.
    if stored.is_empty() || stored.starts_with('!') || stored.starts_with('*') {
        return Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            gettext("group password is locked"),
        ));
    }

    let salt = extract_salt(stored).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            gettext("malformed group password"),
        )
    })?;

    let mut engine = Crypt::new();
    if engine.set_salt(salt).is_err() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            gettext("salt generation failed"),
        ));
    }
    if engine.encrypt(clear.to_string()).is_err() {
        return Err(io::Error::other(gettext("encryption failed")));
    }
    Ok(engine.encrypted)
}

/// Reads a single line from /dev/tty with echo disabled, for the password prompt.
#[cfg(target_os = "linux")]
fn read_password() -> io::Result<String> {
    let tty = File::open("/dev/tty")?;
    let fd = tty.as_raw_fd();
    let mut reader = BufReader::new(tty);

    eprint!("{}", gettext("Password: "));

    let mut term_orig = std::mem::MaybeUninit::uninit();
    let term_orig = unsafe {
        libc::tcgetattr(fd, term_orig.as_mut_ptr());
        term_orig.assume_init()
    };

    let mut term_modified = term_orig;
    term_modified.c_lflag &= !ECHO;
    term_modified.c_lflag |= ECHONL;

    if unsafe { libc::tcsetattr(fd, TCSANOW, &term_modified) } != 0 {
        return Err(io::Error::last_os_error());
    }

    let mut password = String::new();
    let read_res = reader.read_line(&mut password);

    // Always restore the terminal, even if the read failed.
    let restore = unsafe { libc::tcsetattr(fd, TCSANOW, &term_orig) };
    read_res?;
    if restore != 0 {
        return Err(io::Error::last_os_error());
    }

    Ok(password.trim_end().to_string())
}

fn main() {
    plib::diag::init_locale("newgrp");
    let args = Args::parse();
    run(&args);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn grp(name: &str, gid: u32, members: &[&str], passwd: &str) -> Group {
        Group {
            name: name.to_string(),
            gid,
            members: members.iter().map(|s| s.to_string()).collect(),
            passwd: passwd.to_string(),
        }
    }

    #[test]
    fn find_group_by_name() {
        let groups = vec![grp("staff", 50, &[], ""), grp("wheel", 0, &[], "")];
        assert_eq!(find_matching_group("staff", &groups).unwrap().gid, 50);
    }

    #[test]
    fn find_group_by_numeric_gid() {
        let groups = vec![grp("staff", 50, &[], "")];
        assert_eq!(find_matching_group("50", &groups).unwrap().name, "staff");
    }

    #[test]
    fn find_group_numeric_string_that_is_also_a_name_prefers_name() {
        // A group literally named "50" with gid 99 must win over gid 50.
        let groups = vec![grp("50", 99, &[], ""), grp("staff", 50, &[], "")];
        let found = find_matching_group("50", &groups).unwrap();
        assert_eq!(
            found.gid, 99,
            "numeric operand that is also a name resolves by name"
        );
    }

    #[test]
    fn find_group_missing() {
        let groups = vec![grp("staff", 50, &[], "")];
        assert!(find_matching_group("nope", &groups).is_none());
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn extract_salt_sha512() {
        // A 4-field crypt hash ($id$salt$hash) yields the $id$salt$hash prefix.
        let salt = extract_salt("$6$abcdefgh$rest.of.the.hash").unwrap();
        assert!(salt.starts_with("$6$abcdefgh$"));
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn extract_salt_too_few_parts() {
        assert!(extract_salt("notahash").is_none());
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn constant_time_eq_matches() {
        assert!(constant_time_eq(b"abc", b"abc"));
        assert!(!constant_time_eq(b"abc", b"abd"));
        assert!(!constant_time_eq(b"abc", b"ab"));
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn pw_encrypt_rejects_locked() {
        // Locked/blocked entries must never authenticate.
        assert!(pw_encrypt("x", "").is_err());
        assert!(pw_encrypt("x", "!").is_err());
        assert!(pw_encrypt("x", "*").is_err());
        assert!(pw_encrypt("x", "!*").is_err());
    }
}
