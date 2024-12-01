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
    ffi::{CStr, CString},
    fs, io,
    os::unix::{
        self,
        fs::{MetadataExt, PermissionsExt},
    },
    sync::Once,
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

static mut PRIMARY_GROUP: String = String::new();
static INIT_PRIMARY_GROUP: Once = Once::new();

static mut GID1: u32 = 0;
static mut GID2: u32 = 0;
static INIT_GID: Once = Once::new();

fn get_group_id(name: &CStr) -> u32 {
    unsafe {
        let grp = libc::getgrnam(name.as_ptr());
        if grp.is_null() {
            panic!("Group name not found: {}", name.to_string_lossy());
        } else {
            (&*grp).gr_gid
        }
    }
}

// Return two groups that the current user belongs to.
fn get_groups() -> ((String, u32), (String, u32)) {
    // Linux - (primary group of current user, "adm")
    // macOS - ("staff", "admin")
    let (g1, g2) = if cfg!(target_os = "linux") {
        unsafe {
            INIT_PRIMARY_GROUP.call_once(|| {
                let uid = libc::getuid();
                let pw = libc::getpwuid(uid);
                if pw.is_null() {
                    panic!("{}", io::Error::last_os_error());
                }

                let primary_gid = (&*pw).pw_gid;
                let gr = libc::getgrgid(primary_gid);
                if gr.is_null() {
                    panic!("{}", io::Error::last_os_error());
                }

                let gr_name = CStr::from_ptr((&*gr).gr_name).to_owned();
                PRIMARY_GROUP = gr_name.to_str().unwrap().to_owned();
            });
            (PRIMARY_GROUP.clone(), "adm".to_owned())
        }
    } else if cfg!(target_os = "macos") {
        ("staff".to_owned(), "admin".to_owned())
    } else {
        panic!("Unsupported OS")
    };

    unsafe {
        INIT_GID.call_once(|| {
            let g1_cstr = CString::new(g1.as_str()).unwrap();
            let g2_cstr = CString::new(g2.as_str()).unwrap();

            GID1 = get_group_id(&g1_cstr);
            GID2 = get_group_id(&g2_cstr);
        });

        // Must be initialized
        assert_ne!(GID1, 0);
        assert_ne!(GID2, 0);

        // Must be different groups
        assert_ne!(GID1, GID2);

        ((g1, GID1), (g2, GID2))
    }
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

    chgrp_test(&["", f], "", "", 0); // Empty group names are accepted
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
    fs::set_permissions(f, fs::Permissions::from_mode(0)).unwrap();
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

        // `chgrp '' f` is supposed to update the ctime
        chgrp_test(&["", f], "", "", 0);
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

    // Other acceptable error messages:
    // chgrp: '{d_no_x}': Permission denied
    // chgrp: cannot access '{d_no_x_y}': Permission denied
    chgrp_test(
        &["-R", g2, d],
        "",
        &format!("chgrp: cannot read directory '{d_no_x}': Permission denied\n"),
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
    let _1 = &format!("{test_dir}/1");
    let _1_1F = &format!("{test_dir}/1/1F");
    let _1s = &format!("{test_dir}/1s");
    let _2 = &format!("{test_dir}/2");
    let _2_2F = &format!("{test_dir}/2/2F");
    let _2_2s = &format!("{test_dir}/2/2s");
    let _3 = &format!("{test_dir}/3");
    let _3_3F = &format!("{test_dir}/3/3F");

    let ((g1, gid1), (g2, gid2)) = get_groups();
    let g1 = &g1;
    let g2 = &g2;

    fs::create_dir(test_dir).unwrap();

    for dir in [_1, _2, _3] {
        fs::create_dir(dir).unwrap();
    }
    for file in [_1_1F, _2_2F, _3_3F] {
        fs::File::create(file).unwrap();
    }
    unix::fs::symlink(_1, _1s).unwrap();
    unix::fs::symlink("../3", _2_2s).unwrap();
    chgrp_test(&["-R", g1, _1, _2, _3], "", "", 0);

    chgrp_test(&["-H", "-R", g2, _1s, _2], "", "", 0);

    for file in [_1, _1_1F, _2, _2_2F, _3] {
        assert_eq!(file_gid(file).unwrap(), gid2);
    }

    for file in [_1s, _2_2s, _3_3F] {
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
