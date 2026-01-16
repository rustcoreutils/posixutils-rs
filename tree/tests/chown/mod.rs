//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};
use std::{
    collections::HashSet,
    fs, io,
    os::unix::{self, fs::MetadataExt},
    sync::OnceLock,
};

fn chown_test(args: &[&str], expected_output: &str, expected_error: &str, expected_exit_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("chown"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

fn is_root() -> bool {
    unsafe { libc::geteuid() == 0 }
}

// Returns all the groups of the current user
fn current_user_group_ids() -> &'static Vec<libc::gid_t> {
    static GROUP_IDS: OnceLock<Vec<libc::gid_t>> = OnceLock::new();
    GROUP_IDS.get_or_init(|| {
        let num_groups = unsafe { libc::getgroups(0, std::ptr::null_mut()) };
        if num_groups < 0 {
            panic!(
                "unable to determine number of secondary groups {}",
                io::Error::last_os_error()
            );
        }

        let mut groups_list: Vec<libc::gid_t> = vec![0; num_groups as usize];
        let actual_num_groups = unsafe { libc::getgroups(num_groups, groups_list.as_mut_ptr()) };
        assert_eq!(num_groups, actual_num_groups);

        groups_list
    })
}

// Tests parsing the `owner:group` argument. Requires root because this modifies the owner.
#[test]
#[cfg_attr(
    not(all(feature = "posixutils_test_all", feature = "requires_root")),
    ignore
)]
fn test_chown_ownergroup_spec() {
    let test_dir = &format!("{}/test_chown_ownergroup_spec", env!("CARGO_TARGET_TMPDIR"));
    let f = &format!("{test_dir}/f");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();

    // chown owner[:group] f

    chown_test(&["0", f], "", "", 0);
    chown_test(&["0:0", f], "", "", 0);

    // Empty `owner` is allowed and is equivalent to chgrp
    chown_test(&[":0", f], "", "", 0);

    // `group` cannot be empty
    chown_test(&["0:", f], "", "chown: invalid spec: '0:'\n", 1);

    fs::remove_dir_all(test_dir).unwrap();
}

// Changing the owner always requires root. Requires root because this modifies the owner.
#[test]
#[cfg_attr(
    not(all(feature = "posixutils_test_all", feature = "requires_root")),
    ignore
)]
fn test_chown_change_owner() {
    let test_dir = &format!("{}/test_chown_change_owner", env!("CARGO_TARGET_TMPDIR"));
    let f = &format!("{test_dir}/f");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();

    // Test `chown owner f`
    for owner in [0, 1, 2] {
        chown_test(&[owner.to_string().as_str(), f], "", "", 0);
        let md = fs::metadata(f).unwrap();
        assert_eq!(owner, md.uid());
    }

    // `chown owner: f` should fail and not modify the owner of the file
    chown_test(&["1", f], "", "", 0);
    let original_uid = fs::metadata(f).unwrap().uid();
    chown_test(&["0:", f], "", "chown: invalid spec: '0:'\n", 1);
    assert_eq!(original_uid, fs::metadata(f).unwrap().uid());

    fs::remove_dir_all(test_dir).unwrap();
}

// Tests the error when the owner is changed by a non-root user
#[test]
fn test_chown_change_owner_without_root() {
    if is_root() {
        eprintln!("Skipping test: must not be root to test the failure case");
        return;
    }

    let test_dir = &format!("{}/test_chown_change_owner", env!("CARGO_TARGET_TMPDIR"));
    let f = &format!("{test_dir}/f");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();

    chown_test(
        &["0", f],
        "",
        &format!("chown: changing ownership of '{f}': Operation not permitted\n"),
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Tests changing both the owner and the group simultaneously. Requires root because this modifies
// the owner.
#[test]
#[cfg_attr(
    not(all(feature = "posixutils_test_all", feature = "requires_root")),
    ignore
)]
fn test_chown_change_owner_and_group() {
    let test_dir = &format!(
        "{}/test_chown_change_owner_and_group",
        env!("CARGO_TARGET_TMPDIR")
    );
    let f = &format!("{test_dir}/f");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();

    // Test `chown owner:group f`
    for owner in [0, 1, 2] {
        for group in [0, 1, 2] {
            chown_test(&[&format!("{owner}:{group}"), f], "", "", 0);
            let md = fs::metadata(f).unwrap();
            assert_eq!(owner, md.uid());
            assert_eq!(group, md.gid());
        }
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Changing only the group does not require root privileges
#[test]
fn test_chown_change_group() {
    let test_dir = &format!("{}/test_chown_change_group", env!("CARGO_TARGET_TMPDIR"));
    let f = &format!("{test_dir}/f");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();

    // Test `chown :group f`
    let owner = fs::metadata(f).unwrap().uid();
    for &group in current_user_group_ids() {
        chown_test(&[&format!(":{group}"), f], "", "", 0);
        let md = fs::metadata(f).unwrap();
        assert_eq!(owner, md.uid());
        assert_eq!(group, md.gid());
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Verifies that changing a file's group to one the user doesn't belong to fails with the expected
// error message
#[test]
fn test_chown_change_to_non_member_group() {
    if is_root() {
        eprintln!("Skipping test: must not be root to test the failure case");
        return;
    }

    // Get the GID of a group that the test runner doesn't belong to
    fn get_non_member_group() -> Option<u32> {
        let user_groups: HashSet<_> = current_user_group_ids().iter().collect();
        let mut non_member_group: Option<u32> = None; // Group that the current user does not belong to

        // Start reading the group database
        unsafe { libc::setgrent() };

        loop {
            let group = unsafe { libc::getgrent() };
            if group.is_null() {
                break;
            }
            let gid = unsafe { (&*group).gr_gid };

            if !user_groups.contains(&gid) {
                non_member_group = Some(gid);
                break;
            }
        }

        // End reading the group database
        unsafe { libc::endgrent() };

        non_member_group
    }

    let test_dir = &format!(
        "{}/test_chown_change_to_non_member_group",
        env!("CARGO_TARGET_TMPDIR")
    );
    let f = &format!("{test_dir}/f");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();

    if let Some(non_member_group) = get_non_member_group() {
        chown_test(
            &[&format!(":{non_member_group}"), f],
            "",
            // Message should be "changing group of" instead of "changing ownership of"
            &format!("chown: changing group of '{f}': Operation not permitted\n"),
            1,
        );
    } else {
        panic!("Unable to test: the user belongs to all groups");
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Tests error messages for nonexistent file/user/group
#[test]
fn test_chown_nonexistent() {
    let test_dir = &format!("{}/test_chown_nonexistent", env!("CARGO_TARGET_TMPDIR"));
    let f = &format!("{test_dir}/f");

    fs::create_dir(test_dir).unwrap();

    // Nonexistent file
    chown_test(
        &[":1", f],
        "",
        &format!("chown: cannot access '{f}': No such file or directory\n"),
        1,
    );
    fs::File::create(f).unwrap();

    // Nonexistent user
    chown_test(
        &["nonexistent_user", f],
        "",
        "chown: invalid user: 'nonexistent_user'\n",
        1,
    );

    // Nonexistent group
    chown_test(
        &[":nonexistent_group", f],
        "",
        "chown: invalid group: 'nonexistent_group'\n",
        1,
    );
    fs::remove_dir_all(test_dir).unwrap();
}

// Returns a UID and GID that are different from the default UID/GID that are set when creating a
// file. GID is chosen from the current user's groups while the UID is either `None` when not
// running as root or any other UID.
fn select_target_ownergroup() -> (Option<u32>, u32) {
    let original_gid = unsafe { libc::getegid() };

    let target_owner = if is_root() {
        // Choose any UID that is not the owner of `f` and `symlink`
        let mut target_owner = None;
        for owner in 0.. {
            if owner != original_gid {
                target_owner = Some(owner);
                break;
            }
        }
        Some(target_owner.unwrap())
    } else {
        // Don't change the owner when this test is not run as root
        None
    };

    let target_group = if is_root() {
        // Choose any GID that is not the group of `f` and `symlink`
        let mut target_group = None;
        for group in 0.. {
            if group != original_gid {
                target_group = Some(group);
                break;
            }
        }
        target_group.unwrap()
    } else {
        // Choose a group from the current user's group
        let mut target_group = None;
        for &group in current_user_group_ids() {
            if group != original_gid {
                target_group = Some(group);
                break;
            }
        }
        target_group.unwrap()
    };

    (target_owner, target_group)
}

// Tests the -h flag. This test can be run with or without root.
#[test]
fn test_chown_symlink() {
    let test_dir = &format!("{}/test_chown_symlink", env!("CARGO_TARGET_TMPDIR"));
    let f = &format!("{test_dir}/f");
    let symlink = &format!("{test_dir}/symlink");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();
    unix::fs::symlink(f, symlink).unwrap();

    let (original_uid, original_gid) = fs::metadata(f).map(|md| (md.uid(), md.gid())).unwrap();
    let (symlink_uid, symlink_gid) = fs::symlink_metadata(symlink)
        .map(|md| (md.uid(), md.gid()))
        .unwrap();

    // Because they are created by the same process
    assert_eq!(original_uid, symlink_uid);
    assert_eq!(original_gid, symlink_gid);

    let (target_owner, target_group) = select_target_ownergroup();
    let target_owner_str = match target_owner {
        Some(uid) => uid.to_string(),
        None => String::from(""),
    };

    // `chown -h owner:group symlink` should only change the owner/group of the symlink itself
    chown_test(
        &["-h", &format!("{target_owner_str}:{target_group}"), symlink],
        "",
        "",
        0,
    );
    assert_eq!(fs::symlink_metadata(f).unwrap().gid(), original_gid); // `f` should be unaffected
    assert_eq!(fs::symlink_metadata(symlink).unwrap().gid(), target_group);
    if is_root() {
        assert_eq!(
            fs::symlink_metadata(symlink).unwrap().uid().to_string(),
            target_owner_str
        );
    }

    // Reset `symlink`
    {
        let owner_group_arg = if is_root() {
            format!("{original_uid}:{original_gid}")
        } else {
            format!(":{original_gid}")
        };
        chown_test(&["-h", &owner_group_arg, symlink], "", "", 0);
    }

    // `chown owner:group symlink` should modify `f` through `symlink`
    chown_test(
        &[&format!("{target_owner_str}:{target_group}"), symlink],
        "",
        "",
        0,
    );
    assert_eq!(fs::symlink_metadata(symlink).unwrap().gid(), original_gid); // `symlink` should be unaffected
    assert_eq!(fs::symlink_metadata(f).unwrap().gid(), target_group);
    if is_root() {
        assert_eq!(
            fs::symlink_metadata(f).unwrap().uid().to_string(),
            target_owner_str
        );
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Tests the -R flag
#[test]
fn test_chown_recursive_basic() {
    let test_dir = &format!("{}/test_chown_recursive_basic", env!("CARGO_TARGET_TMPDIR"));
    let f = &format!("{test_dir}/f");
    let f_g = &format!("{test_dir}/f/g");
    let f_g_h = &format!("{test_dir}/f/g/h");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir_all(f_g).unwrap();
    fs::File::create(f_g_h).unwrap();

    let (target_owner, target_group) = select_target_ownergroup();
    let target_owner_str = match target_owner {
        Some(uid) => uid.to_string(),
        None => String::from(""),
    };

    chown_test(
        &["-R", &format!("{target_owner_str}:{target_group}"), f],
        "",
        "",
        0,
    );

    // Recursively change the owner and group of f, f/g and f/g/h
    assert_eq!(target_group, fs::metadata(f).unwrap().gid());
    assert_eq!(target_group, fs::metadata(f_g).unwrap().gid());
    assert_eq!(target_group, fs::metadata(f_g_h).unwrap().gid());
    if is_root() {
        let target_owner = target_owner.unwrap();
        assert_eq!(target_owner, fs::metadata(f).unwrap().uid());
        assert_eq!(target_owner, fs::metadata(f_g).unwrap().uid());
        assert_eq!(target_owner, fs::metadata(f_g_h).unwrap().uid());
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Tests the -P flag
#[test]
fn test_chown_recursive_follow_none() {
    let test_dir = &format!(
        "{}/test_chown_recursive_follow_none",
        env!("CARGO_TARGET_TMPDIR")
    );
    let f = &format!("{test_dir}/f");
    let f_g = &format!("{test_dir}/f/g");
    let f_g_h = &format!("{test_dir}/f/g/h");
    let g = &format!("{test_dir}/g");
    let g_symlink = &format!("{test_dir}/g/symlink");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir_all(f_g).unwrap();
    fs::File::create(f_g_h).unwrap();
    fs::create_dir_all(g).unwrap();
    unix::fs::symlink(f, g_symlink).unwrap();

    let original_uid = unsafe { libc::geteuid() };
    let original_gid = unsafe { libc::getegid() };

    let (target_owner, target_group) = select_target_ownergroup();
    let target_owner_str = match target_owner {
        Some(uid) => uid.to_string(),
        None => String::from(""),
    };

    // chown -RP owner:group g
    // Only g and g/symlink should be changed. The symlink to f should not be followed.
    chown_test(
        &["-RP", &format!("{target_owner_str}:{target_group}"), g],
        "",
        "",
        0,
    );

    // f, f/g and f/g/h must be unchanged
    assert_eq!(original_gid, fs::metadata(f).unwrap().gid());
    assert_eq!(original_gid, fs::metadata(f_g).unwrap().gid());
    assert_eq!(original_gid, fs::metadata(f_g_h).unwrap().gid());
    if is_root() {
        assert_eq!(original_uid, fs::metadata(f).unwrap().uid());
        assert_eq!(original_uid, fs::metadata(f_g).unwrap().uid());
        assert_eq!(original_uid, fs::metadata(f_g_h).unwrap().uid());
    }

    // g and g/symlink should be changed
    assert_eq!(target_group, fs::metadata(g).unwrap().gid());
    assert_eq!(target_group, fs::symlink_metadata(g_symlink).unwrap().gid());
    if is_root() {
        let target_owner = target_owner.unwrap();
        assert_eq!(target_owner, fs::metadata(g).unwrap().uid());
        assert_eq!(target_owner, fs::symlink_metadata(g_symlink).unwrap().uid());
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Tests the -L flag
#[test]
fn test_chown_recursive_follow_symlinks() {
    let test_dir = &format!(
        "{}/test_chown_recursive_follow_symlinks",
        env!("CARGO_TARGET_TMPDIR")
    );
    let f = &format!("{test_dir}/f");
    let f_g = &format!("{test_dir}/f/g");
    let f_g_h = &format!("{test_dir}/f/g/h");
    let g = &format!("{test_dir}/g");
    let g_symlink = &format!("{test_dir}/g/symlink");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir_all(f_g).unwrap();
    fs::File::create(f_g_h).unwrap();
    fs::create_dir_all(g).unwrap();
    unix::fs::symlink(f, g_symlink).unwrap();

    let original_uid = unsafe { libc::geteuid() };
    let original_gid = unsafe { libc::getegid() };

    let (target_owner, target_group) = select_target_ownergroup();
    let target_owner_str = match target_owner {
        Some(uid) => uid.to_string(),
        None => String::from(""),
    };

    // chown -RL owner:group g
    // Follow the symlink to f and change everything. `g/symlink` remains unchanged because f was
    // changed in its place.
    chown_test(
        &["-RL", &format!("{target_owner_str}:{target_group}"), g],
        "",
        "",
        0,
    );

    // g/symlink should be unchanged
    assert_eq!(original_gid, fs::symlink_metadata(g_symlink).unwrap().gid());
    if is_root() {
        assert_eq!(original_uid, fs::symlink_metadata(g_symlink).unwrap().uid());
    }

    // f, g, f/g and f/g/h should be changed
    assert_eq!(target_group, fs::metadata(f).unwrap().gid());
    assert_eq!(target_group, fs::metadata(f_g).unwrap().gid());
    assert_eq!(target_group, fs::metadata(f_g_h).unwrap().gid());
    assert_eq!(target_group, fs::metadata(g).unwrap().gid());
    if is_root() {
        let target_owner = target_owner.unwrap();
        assert_eq!(target_owner, fs::metadata(f).unwrap().uid());
        assert_eq!(target_owner, fs::metadata(f_g).unwrap().uid());
        assert_eq!(target_owner, fs::metadata(f_g_h).unwrap().uid());
        assert_eq!(target_owner, fs::metadata(g).unwrap().uid());
    }

    // Recreate the files
    fs::remove_dir_all(test_dir).unwrap();
    fs::create_dir(test_dir).unwrap();
    fs::create_dir_all(f_g).unwrap();
    fs::File::create(f_g_h).unwrap();
    fs::create_dir_all(g).unwrap();
    unix::fs::symlink(f, g_symlink).unwrap();

    // chown -RLh owner:group g
    // Follow the symlink to f and change everything. `f` is the one unchanged when -h is used
    chown_test(
        &["-RLh", &format!("{target_owner_str}:{target_group}"), g],
        "",
        "",
        0,
    );

    // f should be unchanged
    assert_eq!(original_gid, fs::metadata(f).unwrap().gid());
    if is_root() {
        assert_eq!(original_uid, fs::metadata(f).unwrap().uid());
    }

    // f, g, f/g and f/g/h should be changed
    assert_eq!(target_group, fs::symlink_metadata(g_symlink).unwrap().gid());
    assert_eq!(target_group, fs::metadata(f_g).unwrap().gid());
    assert_eq!(target_group, fs::metadata(f_g_h).unwrap().gid());
    assert_eq!(target_group, fs::metadata(g).unwrap().gid());
    if is_root() {
        let target_owner = target_owner.unwrap();
        assert_eq!(target_owner, fs::symlink_metadata(g_symlink).unwrap().uid());
        assert_eq!(target_owner, fs::metadata(f_g).unwrap().uid());
        assert_eq!(target_owner, fs::metadata(f_g_h).unwrap().uid());
        assert_eq!(target_owner, fs::metadata(g).unwrap().uid());
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Changing ownership should propagate through hard links
#[test]
fn test_chown_hardlink() {
    let test_dir = &format!("{}/test_chown_hardlink", env!("CARGO_TARGET_TMPDIR"));
    let f = &format!("{test_dir}/f");
    let hardlink = &format!("{test_dir}/hardlink");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();
    fs::hard_link(f, hardlink).unwrap();

    let (target_owner, target_group) = select_target_ownergroup();
    let target_owner_str = match target_owner {
        Some(uid) => uid.to_string(),
        None => String::from(""),
    };

    chown_test(
        &[&format!("{target_owner_str}:{target_group}"), f],
        "",
        "",
        0,
    );

    // `hardlink` should be changed
    assert_eq!(target_group, fs::metadata(hardlink).unwrap().gid());
    if is_root() {
        assert_eq!(target_owner.unwrap(), fs::metadata(hardlink).unwrap().uid());
    }

    // Recreate f and the hard link
    fs::remove_file(f).unwrap();
    fs::remove_file(hardlink).unwrap();
    fs::File::create(f).unwrap();
    fs::hard_link(f, hardlink).unwrap();

    // Test changing ownership using the hard link
    chown_test(
        &[&format!("{target_owner_str}:{target_group}"), hardlink],
        "",
        "",
        0,
    );

    // `f` should be changed
    assert_eq!(target_group, fs::metadata(f).unwrap().gid());
    if is_root() {
        assert_eq!(target_owner.unwrap(), fs::metadata(f).unwrap().uid());
    }

    fs::remove_dir_all(test_dir).unwrap();
}
