//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

// Casts are necessary for cross-platform compatibility (libc types differ by platform)
#![allow(clippy::unnecessary_cast)]

use plib::testing::{run_test, TestPlan};
use std::{
    fs, io,
    os::unix::{
        self,
        fs::{MetadataExt, PermissionsExt},
    },
    sync::{Once, RwLock},
    thread,
    time::Duration,
};

fn chgrp_test(args: &[&str], expected_output: &str, expected_error: &str, expected_exit_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("chgrp"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

#[derive(Clone)]
struct Groups {
    primary_group: String,
    secondary_group: String,
    gid1: u32,
    gid2: u32,
}

static INIT_GROUPS: Once = Once::new();
static GROUPS: RwLock<Groups> = RwLock::new(Groups {
    primary_group: String::new(),
    secondary_group: String::new(),
    gid1: 0,
    gid2: 0,
});

fn get_group_id(name: &str) -> u32 {
    match plib::group::get_by_name(name) {
        Some(group) => group.gid,
        None => panic!("Group name not found"),
    }
}

/// The name of `gid`, which the test needs as text.
fn group_name(gid: u32) -> String {
    let group = plib::group::get_by_gid(gid).expect("Unable to get group entry");
    group.name.into_string().unwrap()
}

// Return two groups that the current user belongs to.
fn get_groups() -> ((String, u32), (String, u32)) {
    // Guard the writes to the GROUPS with a `Once`
    INIT_GROUPS.call_once(|| {
        let mut groups = GROUPS.write().unwrap();
        let Groups {
            primary_group,
            secondary_group,
            gid1,
            gid2,
        } = &mut *groups;

        // Initialize group strings
        // Linux - (primary group of current user, a group in the supplemental group list that
        //          is not the primary group)
        // macOS - ("staff", "admin")
        if cfg!(target_os = "linux") {
            unsafe {
                let uid = libc::getuid();
                let primary_gid = plib::user::get_by_uid(uid)
                    .expect("the test user has a passwd entry")
                    .gid;
                *primary_group = group_name(primary_gid);

                let mut count = libc::getgroups(0, std::ptr::null_mut());
                if count < 0 {
                    panic!(
                        "unable to determine number of secondary groups {}",
                        io::Error::last_os_error()
                    );
                }

                let mut groups_list: Vec<libc::gid_t> = vec![0; count as usize];

                count = libc::getgroups(count, groups_list.as_mut_ptr());
                match count {
                    _ if count < 2 => panic!("user must be a member of at least two groups"),
                    -1 => panic!(
                        "unable to get secondary groups: {}",
                        io::Error::last_os_error()
                    ),
                    _ => {}
                }

                for second_gid in groups_list {
                    // Skip over the primary_gid
                    if second_gid == primary_gid {
                        continue;
                    } else {
                        *secondary_group = group_name(second_gid);
                        break;
                    }
                }

                if secondary_group.is_empty() {
                    panic!("unable to find suitable secondary group");
                }
            }
        } else if cfg!(target_os = "macos") {
            *primary_group = String::from("staff");
            *secondary_group = String::from("admin");
        } else {
            panic!("Unsupported OS");
        }

        // Initialize the group IDs corresponding to the group strings
        *gid1 = get_group_id(primary_group);
        *gid2 = get_group_id(secondary_group);
    });

    // The reads to GROUPS should not have conflicts with the writes because:
    // 1) `Once` will block until the initialization is finished.
    // 2) The writes are only done inside `Once::call_once`.
    let groups = GROUPS.read().unwrap();
    let groups_ref = &*groups;
    let Groups {
        primary_group,
        secondary_group,
        gid1,
        gid2,
    } = groups_ref.clone();

    // Must be initialized
    assert_ne!(gid1, 0);
    assert_ne!(gid2, 0);

    // Must be different groups
    assert_ne!(gid1, gid2);

    ((primary_group, gid1), (secondary_group, gid2))
}

fn file_gid(path: &str) -> io::Result<u32> {
    // Not `fs::metadata` because we want the metadata of the file itself
    fs::symlink_metadata(path).map(|md| md.gid())
}

// Partial port of coreutils/tests/chgrp/basic.sh
// --reference is not part of the POSIX standard for chgrp
#[test]
fn test_chgrp_basic() {
    let test_dir = &format!("{}/test_chgrp_basic", env!("CARGO_TARGET_TMPDIR"));
    let d = &format!("{test_dir}/d");
    let f = &format!("{test_dir}/f");
    let g = &format!("{test_dir}/g");
    let f2 = &format!("{test_dir}/f2");
    let d_f3 = &format!("{test_dir}/d/f3");
    let symlink = &format!("{test_dir}/symlink");
    let d_files = [d, d_f3];

    let ((g1, gid1), (g2, gid2)) = get_groups();
    let g1 = &g1;
    let g2 = &g2;

    fs::create_dir(test_dir).unwrap();

    fs::create_dir(d).unwrap();
    for file in [f, f2, d_f3] {
        fs::File::create(file).unwrap();
    }

    chgrp_test(&[g1, f], "", "", 0);
    chgrp_test(&[g2, f], "", "", 0);
    chgrp_test(&[g2, f2], "", "", 0);
    chgrp_test(&["-R", g1, d], "", "", 0);

    chgrp_test(&[g1, f], "", "", 0);
    assert_eq!(file_gid(f).unwrap(), gid1);

    // Intenionally done twice
    chgrp_test(&[g2, f], "", "", 0);
    assert_eq!(file_gid(f).unwrap(), gid2);
    chgrp_test(&[g2, f], "", "", 0);
    assert_eq!(file_gid(f).unwrap(), gid2);

    // An empty group operand is now rejected (#CG3); the file's group is unchanged.
    chgrp_test(&["", f], "", "chgrp: invalid group: ''\n", 1);
    assert_eq!(file_gid(f).unwrap(), gid2);

    // Also done twice
    chgrp_test(&[g1, f], "", "", 0);
    assert_eq!(file_gid(f).unwrap(), gid1);
    chgrp_test(&[g1, f], "", "", 0);
    assert_eq!(file_gid(f).unwrap(), gid1);

    chgrp_test(&["-R", g2, d], "", "", 0);
    for file in d_files {
        assert_eq!(file_gid(file).unwrap(), gid2);
    }

    chgrp_test(&["-R", g1, d], "", "", 0);
    for file in d_files {
        assert_eq!(file_gid(file).unwrap(), gid1);
    }

    // Repeat the previous two
    {
        chgrp_test(&["-R", g2, d], "", "", 0);
        for file in d_files {
            assert_eq!(file_gid(file).unwrap(), gid2);
        }

        chgrp_test(&["-R", g1, d], "", "", 0);
        for file in d_files {
            assert_eq!(file_gid(file).unwrap(), gid1);
        }
    }

    // No -R here so d/f3 should still belong to g1
    chgrp_test(&[g2, d], "", "", 0);
    for (file, gid) in d_files.iter().zip([gid2, gid1]) {
        assert_eq!(file_gid(file).unwrap(), gid);
    }

    fs::remove_file(f).unwrap();
    fs::File::create(f).unwrap();
    unix::fs::symlink(f, symlink).unwrap();
    chgrp_test(&[g1, f], "", "", 0);
    assert_eq!(file_gid(f).unwrap(), gid1);

    chgrp_test(&["-h", g2, symlink], "", "", 0);
    assert_eq!(file_gid(f).unwrap(), gid1);

    assert_eq!(file_gid(symlink).unwrap(), gid2);

    let chown_from = |path: &str, from: u32, to: u32| {
        assert_eq!(file_gid(path).unwrap(), from);
        unix::fs::chown(path, None, Some(to)).unwrap();
    };

    chown_from(f, gid1, gid2);

    chgrp_test(&[g1, symlink], "", "", 0);
    assert_eq!(file_gid(f).unwrap(), gid1); // group was affected through `symlink`
    chown_from(f, gid1, gid2);

    chgrp_test(&["-h", g1, f, symlink], "", "", 0);
    assert_eq!(file_gid(symlink).unwrap(), gid1);
    chgrp_test(&["-R", g2, symlink], "", "", 0); // -R by itself should enable -h
    assert_eq!(file_gid(symlink).unwrap(), gid2);
    chown_from(f, gid1, gid2);

    // Remove read permissions from all
    {
        let mask = !(libc::S_IRUSR | libc::S_IRGRP | libc::S_IROTH) as u32;
        let new_mode = fs::metadata(f).unwrap().mode() & mask;
        fs::set_permissions(f, fs::Permissions::from_mode(new_mode)).unwrap();
    }
    chown_from(f, gid2, gid1);
    fs::set_permissions(f, fs::Permissions::from_mode(0o0)).unwrap();
    chown_from(f, gid1, gid2);

    {
        fs::remove_file(f).unwrap();
        for file in [f, g] {
            fs::File::create(file).unwrap();
        }

        chgrp_test(&[g1, f, g], "", "", 0);
        let f_ctime_1 = fs::metadata(f).unwrap().ctime();
        chgrp_test(&[g2, g], "", "", 0);
        thread::sleep(Duration::from_secs(1));
        chgrp_test(&[g1, f], "", "", 0);
        let f_ctime_2 = fs::metadata(f).unwrap().ctime();

        // Added check to see if the last chgrp was not optimized away
        assert!(f_ctime_2 > f_ctime_1);

        // A chgrp to the (already-current) group still updates the ctime.
        chgrp_test(&[g1, f], "", "", 0);
        let f_ctime_3 = fs::metadata(f).unwrap().ctime();
        let g_ctime = fs::metadata(g).unwrap().ctime();
        assert!(f_ctime_3 > g_ctime);
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/chgrp/default-no-deref.sh
#[test]
fn test_chgrp_default_no_deref() {
    let test_dir = &format!(
        "{}/test_chgrp_default_no_deref",
        env!("CARGO_TARGET_TMPDIR")
    );
    let d = &format!("{test_dir}/d");
    let f = &format!("{test_dir}/f");
    let d_s = &format!("{test_dir}/d/s");

    let (_, (g2, gid2)) = get_groups();
    let g2 = &g2;

    fs::create_dir(test_dir).unwrap();

    fs::create_dir(d).unwrap();
    fs::File::create(f).unwrap();
    unix::fs::symlink("../f", d_s).unwrap(); // `..f` is relative to d/s

    let init_group = file_gid(f).unwrap();

    // Should chgrp to a different group
    assert_ne!(init_group, gid2);

    chgrp_test(&["-R", g2, d], "", "", 0);

    // The group of `f` should not change
    assert_eq!(init_group, file_gid(f).unwrap());

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/chgrp/deref.sh
// --dereference flag is not part of the POSIX standard for chgrp
#[test]
fn test_chgrp_deref() {
    let test_dir = &format!("{}/test_chgrp_deref", env!("CARGO_TARGET_TMPDIR"));
    let f = &format!("{test_dir}/f");
    let symlink = &format!("{test_dir}/symlink");

    let ((g1, gid1), (g2, gid2)) = get_groups();
    let g1 = &g1;
    let g2 = &g2;

    fs::create_dir(test_dir).unwrap();

    fs::File::create(f).unwrap();
    unix::fs::symlink(f, symlink).unwrap();

    chgrp_test(&["-h", g2, symlink], "", "", 0);
    assert_eq!(file_gid(symlink).unwrap(), gid2);

    chgrp_test(&[g1, f], "", "", 0);
    assert_eq!(file_gid(f).unwrap(), gid1);

    chgrp_test(&["-h", g2, symlink], "", "", 0);
    assert_eq!(file_gid(f).unwrap(), gid1);
    assert_eq!(file_gid(symlink).unwrap(), gid2);

    chgrp_test(&["-h", g2, symlink], "", "", 0);
    assert_eq!(file_gid(f).unwrap(), gid1);
    assert_eq!(file_gid(symlink).unwrap(), gid2);

    chgrp_test(&[g2, f], "", "", 0);
    assert_eq!(file_gid(f).unwrap(), gid2);

    chgrp_test(&[g1, symlink], "", "", 0);
    assert_eq!(file_gid(f).unwrap(), gid1);
    assert_eq!(file_gid(symlink).unwrap(), gid2);

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/chgrp/deref.sh
#[test]
fn test_chgrp_no_x() {
    let test_dir = &format!("{}/test_chgrp_no_x", env!("CARGO_TARGET_TMPDIR"));
    let d = &format!("{test_dir}/d");
    let d_no_x = &format!("{test_dir}/d/no-x");
    let d_no_x_y = &format!("{test_dir}/d/no-x/y");

    let (_, (g2, _)) = get_groups();
    let g2 = &g2;

    fs::create_dir(test_dir).unwrap();

    fs::create_dir_all(d_no_x_y).unwrap();
    let perm_usr_rw_only = {
        let mut mode = fs::metadata(d_no_x).unwrap().permissions().mode();
        mode &= !(libc::S_IXUSR as u32); // Remove execute permission
        mode |= (libc::S_IRUSR | libc::S_IWUSR) as u32; // Add read and write permissions
        fs::Permissions::from_mode(mode)
    };
    fs::set_permissions(d_no_x, perm_usr_rw_only).unwrap();

    // `d/no-x` is readable but not searchable, so its entries can be listed but not stat'ed;
    // the diagnostic names the entry that could not be reached, as GNU chgrp does.
    chgrp_test(
        &["-R", g2, d],
        "",
        &format!("chgrp: cannot access '{d_no_x_y}': Permission denied\n"),
        1,
    );

    // Reset permissions to allow deletion of `test_dir`
    fs::set_permissions(d_no_x, fs::Permissions::from_mode(0o777)).unwrap();
    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/chgrp/posix-H.sh
// --preserve-root flag is omitted because it is not part of the POSIX standard for chgrp.
#[test]
#[allow(non_snake_case)]
fn test_chgrp_posix_h() {
    let test_dir = &format!("{}/test_chgrp_posix_h", env!("CARGO_TARGET_TMPDIR"));
    let dir1 = &format!("{test_dir}/1");
    let dir1_1f = &format!("{test_dir}/1/1F");
    let dir1s = &format!("{test_dir}/1s");
    let dir2 = &format!("{test_dir}/2");
    let dir2_2f = &format!("{test_dir}/2/2F");
    let dir2_2s = &format!("{test_dir}/2/2s");
    let dir3 = &format!("{test_dir}/3");
    let dir3_3f = &format!("{test_dir}/3/3F");

    let ((g1, gid1), (g2, gid2)) = get_groups();
    let g1 = &g1;
    let g2 = &g2;

    fs::create_dir(test_dir).unwrap();

    for dir in [dir1, dir2, dir3] {
        fs::create_dir(dir).unwrap();
    }
    for file in [dir1_1f, dir2_2f, dir3_3f] {
        fs::File::create(file).unwrap();
    }
    unix::fs::symlink(dir1, dir1s).unwrap();
    unix::fs::symlink("../3", dir2_2s).unwrap();
    chgrp_test(&["-R", g1, dir1, dir2, dir3], "", "", 0);

    chgrp_test(&["-H", "-R", g2, dir1s, dir2], "", "", 0);

    // Unlike GNU, the symlink 2/2s met inside the tree is changed itself, not
    // the directory 3 it points to: POSIX leaves it unspecified under -H, and
    // following it would change a file anywhere.
    for file in [dir1, dir1_1f, dir2, dir2_2f, dir2_2s] {
        assert_eq!(file_gid(file).unwrap(), gid2);
    }

    for file in [dir1s, dir3, dir3_3f] {
        assert_eq!(file_gid(file).unwrap(), gid1);
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/chgrp/recurse.sh
#[test]
fn test_chgrp_recurse() {
    let test_dir = &format!("{}/test_chgrp_recurse", env!("CARGO_TARGET_TMPDIR"));
    let d = &format!("{test_dir}/d");
    let d_dd = &format!("{test_dir}/d/dd");
    let d_s = &format!("{test_dir}/d/s");
    let e = &format!("{test_dir}/e");
    let e_ee = &format!("{test_dir}/e/ee");
    let link = &format!("{test_dir}/link");

    let ((g1, gid1), (g2, gid2)) = get_groups();
    let g1 = &g1;
    let g2 = &g2;

    fs::create_dir(test_dir).unwrap();

    fs::create_dir(d).unwrap();
    fs::create_dir(e).unwrap();
    fs::File::create(d_dd).unwrap();
    fs::File::create(e_ee).unwrap();

    unix::fs::symlink("../e", d_s).unwrap(); // ../e is relative to d/s

    chgrp_test(&["-R", g1, e_ee], "", "", 0);

    chgrp_test(&["-R", g2, d], "", "", 0);
    assert_eq!(file_gid(e_ee).unwrap(), gid1);

    chgrp_test(&["-L", "-R", g2, d], "", "", 0);
    assert_eq!(file_gid(e_ee).unwrap(), gid2);

    chgrp_test(&["-H", "-R", g1, d], "", "", 0);
    assert_eq!(file_gid(e_ee).unwrap(), gid2);

    unix::fs::symlink(d, link).unwrap();

    chgrp_test(&["-H", "-R", g1, link], "", "", 0);
    assert_eq!(file_gid(e_ee).unwrap(), gid2);
    assert_eq!(file_gid(d_dd).unwrap(), gid1);

    fs::remove_dir_all(test_dir).unwrap();
}

/// With -R -H only the operands are followed: a symlink met inside the tree
/// is changed itself, never the file it points to (which may be anywhere,
/// /etc/shadow included).
#[test]
fn test_chgrp_rh_does_not_follow_symlinks_inside_the_tree() {
    let test_dir = &format!("{}/test_chgrp_rh_inner_link", env!("CARGO_TARGET_TMPDIR"));
    let (d, outside, link) = (
        &format!("{test_dir}/d"),
        &format!("{test_dir}/outside"),
        &format!("{test_dir}/d/link"),
    );
    let (_, (g2, gid2)) = get_groups();

    let _ = fs::remove_dir_all(test_dir);
    fs::create_dir_all(d).unwrap();
    fs::File::create(outside).unwrap();
    unix::fs::symlink(outside, link).unwrap();
    let outside_gid = file_gid(outside).unwrap();
    assert_ne!(outside_gid, gid2);

    chgrp_test(&["-R", "-H", &g2, d], "", "", 0);
    let (outside_after, link_after) = (file_gid(outside).unwrap(), file_gid(link).unwrap());
    fs::remove_dir_all(test_dir).unwrap();

    assert_eq!(outside_after, outside_gid, "the link's target was changed");
    assert_eq!(link_after, gid2, "the link itself was not changed");
}

/// chgrp changes only the group: it must pass no owner to chown, not the
/// owner of whatever the walk saw. Through a symlink, the walk sees the
/// link and chown acts on its target, and as root a stale owner hands the
/// file to someone else. Linux's `/dev/stdin` is a root-owned symlink to
/// this process's file descriptor 0, here a file this user owns: chgrp must
/// change its group, not try to give it to root.
#[cfg(target_os = "linux")]
#[test]
fn test_chgrp_through_a_symlink_owned_by_another_user() {
    let test_dir = &format!(
        "{}/test_chgrp_symlink_other_owner",
        env!("CARGO_TARGET_TMPDIR")
    );
    let f = &format!("{test_dir}/f");
    let (_, (g2, gid2)) = get_groups();

    let _ = fs::remove_dir_all(test_dir);
    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();
    assert_ne!(file_gid(f).unwrap(), gid2);

    let output = std::process::Command::new(plib::testing::get_binary_path("chgrp"))
        .args([g2.as_str(), "/dev/stdin"])
        .stdin(fs::File::open(f).unwrap())
        .output()
        .unwrap();

    let gid = file_gid(f).unwrap();
    fs::remove_dir_all(test_dir).unwrap();
    assert_eq!(output.status.code(), Some(0), "{output:?}");
    assert_eq!(gid, gid2);
}

/// A trailing slash on a symlink operand follows the link, -h or not, and names a directory
/// (POSIX pathname resolution): `fl/` naming a link to a file is "Not a directory", and `dl/`
/// naming a link to a directory changes the directory, and with -R what is in it, never the link.
#[test]
fn test_chgrp_trailing_slash_follows_symlink() {
    let test_dir = &format!("{}/test_chgrp_trailing_slash", env!("CARGO_TARGET_TMPDIR"));
    let (d, d_f, file, dl, fl) = (
        &format!("{test_dir}/d"),
        &format!("{test_dir}/d/f"),
        &format!("{test_dir}/file"),
        &format!("{test_dir}/dl"),
        &format!("{test_dir}/fl"),
    );
    let ((g1, gid1), (g2, gid2)) = get_groups();
    let _ = fs::remove_dir_all(test_dir);
    fs::create_dir_all(d).unwrap();
    fs::File::create(d_f).unwrap();
    fs::File::create(file).unwrap();
    unix::fs::symlink("d", dl).unwrap();
    unix::fs::symlink("file", fl).unwrap();
    chgrp_test(&["-hR", &g1, d, d_f, file, dl, fl], "", "", 0);
    let gids = || [d, d_f, file, dl, fl].map(|p| file_gid(p).unwrap());
    assert_eq!(gids(), [gid1; 5]);

    for opt in ["-h", "-R"] {
        chgrp_test(
            &[opt, &g2, &format!("{fl}/")],
            "",
            &format!("chgrp: cannot access '{fl}/': Not a directory\n"),
            1,
        );
    }
    assert_eq!(gids(), [gid1; 5]);

    chgrp_test(&["-h", &g2, &format!("{dl}/")], "", "", 0);
    assert_eq!(gids(), [gid2, gid1, gid1, gid1, gid1]);

    chgrp_test(&["-hR", &g2, &format!("{dl}//")], "", "", 0);
    assert_eq!(gids(), [gid2, gid2, gid1, gid1, gid1]);

    fs::remove_dir_all(test_dir).unwrap();
}
