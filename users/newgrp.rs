//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use clap::{error::ErrorKind, Parser};
use gettextrs::{bind_textdomain_codeset, setlocale, textdomain, LocaleCategory};
use libc::{
    getgid, getgrnam, getgroups, getlogin, getpwnam, getpwuid, getuid, gid_t, passwd, setegid,
    setgid, setgroups, setuid, uid_t,
};

#[cfg(target_os = "linux")]
use libc::{ECHO, ECHONL, TCSANOW};
#[cfg(target_os = "linux")]
use libcrypt_rs::Crypt;
use plib::{group::Group, PROJECT_NAME};

use std::{
    env,
    ffi::{CStr, CString},
    io,
    process::{self, Command},
};

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

/// newgrp — change to a new group
#[derive(Parser, Debug)]
#[command(author, version, about, long_about)]
struct Args {
    /// Change the environment to what would be expected if the user actually logged in again (letter `l`).
    #[arg(short = 'l')]
    login: bool,

    /// Specifies the group ID or group name. This is a positional argument that must be provided.
    #[arg(value_name = "GROUP", required = true)]
    group: String,
}

/// Changes the effective group ID and updates the supplementary group list based on the specified group.
///
/// This function first retrieves the current user's password entry to get the user's supplementary groups.
/// It then checks the specified group against these groups and performs the necessary updates to the effective
/// group ID and supplementary group list.
///
/// # Parameters
///
/// * `args`: The command-line arguments containing the target group information.
///
/// # Returns
/// Returns `Ok(())` if the operation is successful, or an error message if it fails.
fn newgrp(args: Args) -> Result<(), io::Error> {
    let groups = plib::group::load();

    // Retrieve current user information
    let pwd = get_password().or_else(|_| {
        Err(io::Error::new(
            io::ErrorKind::NotFound,
            "Could not retrieve current user information.",
        ))
    })?;
    let user_name = unsafe { CStr::from_ptr(pwd.pw_name) }
        .to_str()
        .unwrap_or_else(|_| "???");

    if args.login {
        set_login_environment(&user_name)?;
    }

    let group_identifier = args.group.trim();

    // Find the matching group
    let group = find_matching_group(group_identifier, &groups).ok_or_else(|| {
        eprintln!("newgrp: GROUP '{}' does not exist.", group_identifier);
        io::Error::new(io::ErrorKind::NotFound, "Group not found.")
    })?;

    let current_gid = unsafe { getgid() };
    let current_group_gids: Vec<u32> = groups.iter().map(|g| g.gid).collect();

    // Check if the user is already a member of the target group
    if group.gid == current_gid {
        eprintln!(
            "newgrp: You are already in group '{}'. Trying to change GID",
            group_identifier
        );
        if unsafe { getuid() } == 0 {
            change_gid_and_uid(group.gid, group_identifier)?;
        }
        return Ok(());
    }

    // Check permissions for the user to join the specified group
    check_perms(&group, pwd)?;

    // Check if the effective GID is in the supplementary list
    let effective_gid_in_supplementary = current_group_gids.contains(&current_gid);
    let new_gid_in_supplementary = current_group_gids.contains(&group.gid);

    // Logic for changing the effective GID based on the requirements
    if effective_gid_in_supplementary {
        // The effective GID is in the supplementary list
        if new_gid_in_supplementary {
            // New GID is also in the supplementary list; change the effective GID
            change_effective_gid_and_uid(group.gid, group_identifier)?;
            logger(&user_name, group.gid);
        } else {
            // New GID is not in the supplementary list; add it if possible
            add_gid_to_groups(group.gid);
            change_effective_gid_and_uid(group.gid, group_identifier)?;
            logger(&user_name, group.gid);
        }
    } else {
        // The effective GID is not in the supplementary list
        if new_gid_in_supplementary {
            // New GID is in the supplementary list; delete it
            remove_gid_from_groups(group.gid);
        } else {
            // Neither the old nor new GID is in the supplementary list; add the old GID
            add_gid_to_groups(current_gid);
        }
        change_gid_and_uid(group.gid, group_identifier)?;
        logger(&user_name, group.gid);
    }

    Ok(())
}

/// Adds a GID to the supplementary group list of the process if there is room.
///
/// This function modifies the supplementary group list to include the specified GID.
/// If there is no room, it prints an error message.
///
/// # Parameters
///
/// * `gid`: The group ID to add to the supplementary group list.
fn add_gid_to_groups(gid: gid_t) {
    let mut supplementary_groups = get_current_supplementary_groups();
    if supplementary_groups.len() < MAX_GROUPS {
        supplementary_groups.push(gid);

        #[cfg(target_os = "macos")]
        let supplementary_groups_len = supplementary_groups.len() as i32;
        #[cfg(target_os = "linux")]
        let supplementary_groups_len = supplementary_groups.len();

        unsafe {
            setgroups(supplementary_groups_len, supplementary_groups.as_ptr());
        }
    } else {
        eprintln!("Error: No room to add GID {}", gid);
    }
}

/// Removes the current GID from the supplementary group list.
///
/// This function modifies the supplementary group list to remove the specified GID.
///
/// # Parameters
///
/// * `gid`: The group ID to remove from the supplementary group list.
fn remove_gid_from_groups(gid: gid_t) {
    let mut supplementary_groups = get_current_supplementary_groups();
    if let Some(pos) = supplementary_groups.iter().position(|&x| x == gid) {
        supplementary_groups.remove(pos);

        #[cfg(target_os = "macos")]
        let supplementary_groups_len = supplementary_groups.len() as i32;
        #[cfg(target_os = "linux")]
        let supplementary_groups_len = supplementary_groups.len();

        unsafe {
            setgroups(supplementary_groups_len, supplementary_groups.as_ptr());
        }
    }
}

/// Retrieves the current supplementary groups for the process.
///
/// This function obtains the current list of supplementary groups for the calling process.
///
/// # Returns
/// Returns a vector of GIDs representing the current supplementary groups.
fn get_current_supplementary_groups() -> Vec<gid_t> {
    let mut groups = vec![0; MAX_GROUPS];

    #[cfg(target_os = "macos")]
    let max_groups: i32 = 16;
    #[cfg(target_os = "linux")]
    let max_groups = libc::KERN_NGROUPS_MAX;

    let num_groups = unsafe { libc::getgroups(max_groups, groups.as_mut_ptr()) };
    groups.truncate(num_groups as usize);
    groups
}

/// Changes the effective user ID to the specified UID.
///
/// # Arguments
///
/// * `uid` - The new user ID to set.
///
/// # Returns
///
/// Returns `Ok(())` if the UID was successfully changed. If an error occurs, it returns
/// an `Err` containing the last OS error.
///
fn change_uid(uid: uid_t) -> Result<(), io::Error> {
    if unsafe { setuid(uid) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

/// Retrieves the current supplementary group IDs for the calling process.
///
/// # Returns
///
/// Returns a vector of group IDs (`Vec<gid_t>`) if successful. If an error occurs,
/// it returns an `Err` containing the last OS error.
fn get_current_supplementary_gids() -> Result<Vec<gid_t>, io::Error> {
    let mut supplementary_gids: [gid_t; 32] = [0; 32];
    let num_groups = unsafe {
        getgroups(
            supplementary_gids.len() as i32,
            supplementary_gids.as_mut_ptr(),
        )
    };

    // Check if getgroups was successful
    if num_groups < 0 {
        return Err(io::Error::last_os_error());
    }

    Ok(supplementary_gids[..num_groups as usize].to_vec())
}

/// Sets the supplementary group IDs for the calling process.
///
/// # Arguments
///
/// * `gids` - A slice of group IDs to set as supplementary groups.
///
/// # Returns
///
/// Returns `Ok(())` if the supplementary GIDs were successfully set. If an error occurs,
/// it returns an `Err` containing the last OS error.
fn set_supplementary_gids(gids: &[gid_t]) -> Result<(), io::Error> {
    #[cfg(target_os = "macos")]
    let gids_len = gids.len() as i32;

    #[cfg(target_os = "linux")]
    let gids_len = gids.len() as usize;

    if unsafe { setgroups(gids_len, gids.as_ptr()) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

/// Changes the effective group ID and user ID of the calling process to the specified group
/// and current user.
///
/// # Arguments
///
/// * `gid` - The group ID to set as the new effective group ID.
/// * `group_name` - The name of the group to be set (for logging or error messages).
///
/// # Returns
///
/// Returns `Ok(())` if the group and user IDs were successfully changed. If an error occurs,
/// it returns an `Err` containing the last OS error.
fn change_gid_and_uid(gid: gid_t, group_name: &str) -> Result<(), io::Error> {
    // Create a C string for the group name
    let c_group_name = CString::new(group_name)
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "CString::new failed"))?;

    // Attempt to retrieve the group entry
    let gr_entry = unsafe { getgrnam(c_group_name.as_ptr()) };
    if gr_entry.is_null() {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!("Group '{}' does not exist.", group_name),
        ));
    }

    // Attempt to set the GID
    if unsafe { setgid(gid) } != 0 {
        return Err(io::Error::last_os_error());
    }

    // Retrieve the current user UID
    let current_uid: uid_t = unsafe { getuid() };

    // Get the current supplementary GIDs
    let mut supplementary_gids = get_current_supplementary_gids()?;

    // Add the new GID if it is not already present
    if !supplementary_gids.contains(&gid) {
        supplementary_gids.push(gid);
    }

    // Set the supplementary groups
    set_supplementary_gids(&supplementary_gids)?;

    // Attempt to set the UID to the current UID
    change_uid(current_uid)?;

    Ok(())
}

/// Changes the effective group ID and user ID of the calling process to the specified group
/// while maintaining the current supplementary group memberships.
///
/// # Arguments
///
/// * `gid` - The group ID to set as the new effective group ID.
/// * `group_name` - The name of the group to be set (for logging or error messages).
///
/// # Returns
///
/// Returns `Ok(())` if the group and user IDs were successfully changed. If an error occurs,
/// it returns an `Err` containing the last OS error.
fn change_effective_gid_and_uid(gid: gid_t, group_name: &str) -> Result<(), io::Error> {
    // Create a C string for the group name
    let c_group_name = CString::new(group_name)
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "CString::new failed"))?;

    // Attempt to retrieve the group entry
    let gr_entry = unsafe { getgrnam(c_group_name.as_ptr()) };
    if gr_entry.is_null() {
        return Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!("Group '{}' does not exist.", group_name),
        ));
    }

    // Attempt to set the GID
    if unsafe { setegid(gid) } != 0 {
        return Err(io::Error::last_os_error());
    }

    // Retrieve the current user UID
    let current_uid: uid_t = unsafe { getuid() };

    // Get the current supplementary GIDs
    let current_supplementary_gids = get_current_supplementary_gids()?;

    // Set the supplementary groups (no need to add new GID)
    set_supplementary_gids(&current_supplementary_gids)?;

    // Attempt to set the UID to the current UID
    change_uid(current_uid)?;

    Ok(())
}

/// Retrieves the password entry for the current user based on the login name
/// or user ID (UID).
///
/// This function attempts to obtain the login name of the current user using
/// the `getlogin` system call. If successful, it checks for a password entry
/// corresponding to that login name. If the login name is empty or if no
/// matching password entry is found, the function falls back to using the
/// UID to retrieve the password entry.
///
/// # Returns
/// Returns an `Option<passwd>`, where:
/// - `Some(passwd)`: Represents the password entry if found.
/// - `None`: If the login name cannot be retrieved, if there is no matching
///   password entry for the login name or UID, or if an error occurs.
///
fn get_password() -> Result<passwd, io::Error> {
    unsafe {
        // Get the login name and handle potential null pointer
        let login_ptr = getlogin();
        if login_ptr.is_null() {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                "Login name not found.",
            ));
        }

        let login_name = match CStr::from_ptr(login_ptr).to_str() {
            Ok(name) => name,
            Err(_) => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Failed to convert login name to valid UTF-8.",
                ));
            }
        };

        let ruid = getuid();

        // Attempt to get the password entry by login name
        if !login_name.is_empty() {
            if let Ok(c_login_name) = CString::new(login_name) {
                let pw = getpwnam(c_login_name.as_ptr());

                // Check if pw is not null and the UID matches
                if !pw.is_null() && (*pw).pw_uid == ruid {
                    return Ok(*pw);
                }
            }
        }

        // Fall back to getting the password entry by UID
        let pw_by_uid = getpwuid(ruid);
        if !pw_by_uid.is_null() {
            return Ok(*pw_by_uid);
        }

        // If no password entry is found, return an error
        Err(io::Error::new(
            io::ErrorKind::NotFound,
            format!(
                "Unable to retrieve password entry for login '{}' or UID '{}'.",
                login_name, ruid
            ),
        ))
    }
}

/// Finds a matching group in a vector of groups by name or GID.
///
/// This function takes a string identifier that can represent either a group name
/// or a group ID (GID). It searches through the provided vector of `Group` instances
/// to find a match. If a match is found, a clone of the matching `Group` is returned.
///
/// # Parameters
///
/// * `group_identifier`: A string slice that can be a group name or a numeric GID.
/// * `groups`: A vector of `Group` instances to search through.
///
/// # Returns
///
/// This function returns an `Option<Group>`. If a matching group is found,
/// it returns `Some(Group)`, otherwise it returns `None`.
fn find_matching_group(group_identifier: &str, groups: &Vec<Group>) -> Option<Group> {
    // Helper closure to clone and return the group
    let clone_group = |group: &Group| {
        Some(Group {
            gid: group.gid,
            name: group.name.clone(),
            members: group.members.clone(),
            passwd: group.passwd.clone(),
        })
    };

    // Check if the identifier is a number (GID)
    if let Ok(gid) = group_identifier.parse::<u32>() {
        // Find the matching group by GID
        if let Some(group) = groups.iter().find(|group| group.gid == gid) {
            return clone_group(group);
        }
    }

    // Otherwise, treat it as a group name and find the matching group
    if let Some(group) = groups.iter().find(|group| group.name == group_identifier) {
        return clone_group(group);
    }

    None
}

/// Logs a message indicating that a user has switched to a different group.
///
/// This function retrieves the current login name and terminal device name (tty)
/// and logs a message to standard error, formatted to include the user's name,
/// their login name, the tty they are using, and the group ID they switched to.
///
/// # Parameters
///
/// * `name`: A string slice representing the name of the user who is switching groups.
/// * `gid`: An unsigned 32-bit integer representing the group ID the user is switching to.
///
fn logger(name: &str, gid: u32) {
    // Get the current login name
    let loginname = get_current_login().unwrap_or("???".to_string());

    // Get the current tty device name
    let tty = get_current_tty().unwrap_or("???".to_string());

    // Log the message
    eprintln!(
        "user '{}' (login '{}' on {}) switched to group with id '{}'",
        name, loginname, tty, gid
    );
}

/// Retrieves the current login name.
fn get_current_login() -> Option<String> {
    unsafe {
        let login_ptr = libc::getlogin();
        if !login_ptr.is_null() {
            CStr::from_ptr(login_ptr)
                .to_str()
                .ok()
                .map(|s| s.to_string())
        } else {
            None
        }
    }
}

/// Retrieves the current tty name.
fn get_current_tty() -> Option<String> {
    unsafe {
        let tty_ptr = libc::ttyname(0);
        if !tty_ptr.is_null() {
            CStr::from_ptr(tty_ptr).to_str().ok().map(|s| {
                if s.starts_with("/dev/") {
                    s[5..].to_string()
                } else {
                    s.to_string()
                }
            })
        } else {
            None
        }
    }
}

/// Checks permissions for accessing a specified group based on the provided user credentials.
///
/// This function determines whether the user associated with the given password structure
/// has permission to access the specified group. If a password is required, it prompts the
/// user for the password and verifies it against the group's password.
///
/// # Arguments
///
/// * `group` - A reference to the `Group` struct representing the group whose permissions are being checked.
/// * `password` - A `passwd` struct containing the user's password information.
///
/// # Returns
///
/// Returns `Ok(group.gid)` if the user has the necessary permissions to access the group. If a password is required
/// and does not match, it returns an `Err` with `io::ErrorKind::PermissionDenied`.

fn check_perms(group: &Group, password: passwd) -> Result<u32, io::Error> {
    let pw_name = unsafe {
        CStr::from_ptr(password.pw_name)
            .to_string_lossy()
            .into_owned()
    };

    // Determine if a password is needed based on group membership and GID
    let mut need_password =
        group.gid != password.pw_gid && group.members.iter().all(|member| member != &pw_name);

    // Convert C-style strings (char pointers) to Rust &CStr and check for empty passwords
    unsafe {
        let user_password = CStr::from_ptr(password.pw_passwd).to_bytes();

        if user_password.is_empty() && !group.passwd.is_empty() {
            need_password = true;
        }
    }

    // Check for permissions if necessary
    unsafe {
        if getuid() != 0 {
            if need_password {
                #[cfg(target_os = "linux")]
                {
                    let password_input = read_password().unwrap_or_default();
                    let shadow_password = get_shadow_password(&group.name)?;
                    let hashed_input = pw_encrypt(&password_input, Some(&shadow_password))?;

                    if hashed_input == shadow_password {
                        // Return GID if password matches
                        return Ok(group.gid);
                    } else {
                        eprintln!("Error: Incorrect password for group '{}'.", group.name);
                        return Err(io::Error::new(
                            io::ErrorKind::PermissionDenied,
                            "Incorrect password for group.",
                        ));
                    }
                }
            }
            #[cfg(target_os = "macos")]
            {
                return Err(io::Error::new(
                    io::ErrorKind::PermissionDenied,
                    "Try to use root.",
                ));
            }
        }
    }

    // Return the group GID if no password check is required or if it passed
    Ok(group.gid)
}

/// Retrieves the shadow password for a specified group from the group shadow file.
///
/// This function searches through the group shadow file defined by `GROUPSHADOW_PATH`
/// to find the entry corresponding to the provided `group_name`. If found, it extracts
/// and returns the shadow password associated with that group.
///
/// # Parameters
///
/// - `group_name`: A string slice that holds the name of the group whose shadow password
///   is to be retrieved.
///
/// # Returns
///
/// - `Result<String, io::Error>`: On success, returns the shadow password as a `String`.
///   If the group is not found or if an I/O error occurs, it returns an `io::Error`.
///
#[cfg(target_os = "linux")]
fn get_shadow_password(group_name: &str) -> Result<String, io::Error> {
    let file = File::open(GROUPSHADOW_PATH)?;
    let reader = BufReader::new(file);

    for line in reader.lines() {
        let line_content = line?;

        if let Some(colon_pos) = line_content.find(':') {
            let group = &line_content[..colon_pos];
            if group == group_name {
                let remaining = &line_content[colon_pos + 1..];
                if let Some(next_colon_pos) = remaining.find(':') {
                    let password = &remaining[..next_colon_pos];
                    return Ok(password.to_string());
                }
            }
        }
    }

    Ok(String::new())
}

/// Extracts the salt from a given full hash string in the format used by cryptographic hash functions.
///
/// # Parameters
/// - `full_hash`: A string slice representing the full hash, which typically includes the algorithm identifier,
///   the salt, and the hashed password.
///
/// # Returns
/// - `Some(String)`: The extracted salt if the hash string has the expected format with at least four parts.
/// - `None`: If the hash string does not have the correct format or does not contain enough parts to extract the salt.
#[cfg(target_os = "linux")]
fn extract_salt(full_hash: &str) -> Option<String> {
    let parts: Vec<&str> = full_hash.split('$').collect();

    if parts.len() >= 4 {
        Some(format!("${}${}${}", parts[1], parts[2], parts[3]))
    } else {
        None
    }
}

/// Encrypts a clear text password using the salt extracted from a shadow password entry.
///
/// # Parameters
/// - `clear`: A string slice representing the clear text password to be encrypted.
/// - `shadow_password`: An optional string slice containing the full shadow password hash
///   from which the salt will be extracted.
///
/// # Returns
/// - `Some(String)`: The encrypted password if the encryption process is successful.
/// - `None`: If the salt extraction or encryption fails.
#[cfg(target_os = "linux")]
fn pw_encrypt(clear: &str, shadow_password: Option<&str>) -> Result<String, io::Error> {
    let mut engine = Crypt::new();

    // Extract the salt from the shadow password, returning an error if extraction fails
    let salt = match shadow_password.and_then(extract_salt) {
        Some(salt) if !salt.contains("!*") => salt, // Ensure the salt is not blocked
        _ => {
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "Shadow password is blocked or salt extraction failed.",
            ));
        }
    };

    // Attempt to set the salt in the engine
    if engine.set_salt(salt).is_err() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Salt generation failed.",
        ));
    }

    // Attempt to encrypt the clear text password
    if engine.encrypt(clear.to_string()).is_err() {
        return Err(io::Error::new(io::ErrorKind::Other, "Encryption failed."));
    }

    Ok(engine.encrypted)
}
/// Reads a password from the terminal without echoing the input.
///
/// This function opens the terminal (`/dev/tty`), modifies the terminal settings to hide
/// the user's input while typing (except for newlines), prompts the user for a password,
/// reads the input, and restores the original terminal settings before returning the password.
///
/// # Returns
///
/// On success, returns the user's password as a `String`. The password is trimmed to remove
/// any trailing newline characters. On failure, returns an `io::Result` with the error encountered.
///
/// # Errors
///
/// This function may return an `io::Error` in the following cases:
/// - Failure to open `/dev/tty` for reading from the terminal.
/// - Failure to read the current terminal attributes.
/// - Failure to set the modified terminal attributes (i.e., disabling input echo).
/// - Failure to read the password input.
/// - Failure to restore the original terminal attributes after reading the password.
#[cfg(target_os = "linux")]
fn read_password() -> io::Result<String> {
    // Open the terminal (tty) and get its file descriptor
    let tty = File::open("/dev/tty")?;
    let fd = tty.as_raw_fd();
    let mut reader = BufReader::new(tty);

    // Print password prompt without a newline
    eprint!("Password: ");

    // Get the current terminal settings
    let mut term_orig = std::mem::MaybeUninit::uninit();
    let term_orig = unsafe {
        libc::tcgetattr(fd, term_orig.as_mut_ptr());
        term_orig.assume_init()
    };

    // Modify terminal settings to hide user input (except newline)
    let mut term_modified = term_orig;
    term_modified.c_lflag &= !ECHO; // Disable echo
    term_modified.c_lflag |= ECHONL; // Keep newline

    // Apply the modified terminal settings
    let set_result = unsafe { libc::tcsetattr(fd, TCSANOW, &term_modified) };
    if set_result != 0 {
        return Err(io::Error::last_os_error());
    }

    // Read the password
    let mut password = String::new();
    reader.read_line(&mut password)?;

    // Restore the original terminal settings
    let restore_result = unsafe { libc::tcsetattr(fd, TCSANOW, &term_orig) };
    if restore_result != 0 {
        return Err(io::Error::last_os_error());
    }

    Ok(password.trim_end().to_string())
}

/// Set the environment variables as if the user has logged in and execute their shell.
///
/// # Arguments
///
/// * `user` - A string slice that holds the username whose environment needs to be set.
///
/// # Returns
///
/// * `Result<(), io::Error>` - Returns `Ok(())` on success, or an `io::Error` if something goes wrong.
///
fn set_login_environment(user: &str) -> Result<(), io::Error> {
    // Get the user's shell from the password entry
    let pwd = get_password().or_else(|_| {
        Err(io::Error::new(
            io::ErrorKind::NotFound,
            "Could not retrieve user information.",
        ))
    })?;

    let user_shell = unsafe { CStr::from_ptr(pwd.pw_shell) }
        .to_str()
        .unwrap_or("/bin/sh");

    // Set the necessary environment variables
    env::set_var("USER", user);
    env::set_var("HOME", unsafe {
        CStr::from_ptr(pwd.pw_dir).to_str().unwrap_or("")
    });
    env::set_var("SHELL", user_shell);

    let status = Command::new(user_shell)
        .env("USER", user)
        .env("HOME", unsafe {
            CStr::from_ptr(pwd.pw_dir).to_str().unwrap_or("")
        })
        .env("SHELL", user_shell)
        .status()?;

    if !status.success() {
        eprintln!("Failed to start shell: {}", user_shell);
        return Err(io::Error::new(
            io::ErrorKind::Other,
            "Shell execution failed",
        ));
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::try_parse().unwrap_or_else(|err| {
        if err.kind() == ErrorKind::DisplayHelp || err.kind() == ErrorKind::DisplayVersion {
            // Print help or version message
            eprintln!("{}", err);
        } else {
            // Print custom error message
            eprintln!("Error parsing arguments: {}", err);
        }

        // Exit with a non-zero status code
        std::process::exit(1);
    });

    setlocale(LocaleCategory::LcAll, "");
    textdomain(PROJECT_NAME)?;
    bind_textdomain_codeset(PROJECT_NAME, "UTF-8")?;
    let mut exit_code = 0;

    if let Err(err) = newgrp(args) {
        exit_code = 1;
        eprint!("{}", err);
    }

    process::exit(exit_code)
}
