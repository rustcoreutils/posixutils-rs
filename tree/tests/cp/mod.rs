//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::{run_test, TestPlan};
use std::ffi::CString;
use std::io::{Read, Write};
use std::os::unix::fs::{DirBuilderExt, FileTypeExt};
use std::os::unix::{
    self,
    fs::{MetadataExt, PermissionsExt},
};
use std::path::Path;
use std::process::{Command, Stdio};
use std::{fs, io};

fn cp_test(args: &[&str], expected_output: &str, expected_error: &str, expected_exit_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("cp"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

fn cp_test_with_stdin(
    args: &[&str],
    stdin_data: &str,
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("cp"),
        args: str_args,
        stdin_data: String::from(stdin_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

// Partial port of coreutils/tests/mv/childproof.sh
// Only includes the test for `cp` excluding the non-standard
// `--backup=numbered` option
#[test]
fn test_cp_childproof() {
    let test_dir = &format!("{}/test_cp_childproof", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");
    let c = &format!("{test_dir}/c");
    let a_f = &format!("{test_dir}/a/f");
    let b_f = &format!("{test_dir}/b/f");
    let c_f = &format!("{test_dir}/c/f");

    fs::create_dir(test_dir).unwrap();
    for dir in [a, b, c] {
        fs::create_dir(dir).unwrap();
    }

    {
        let mut file = fs::File::create(a_f).unwrap();
        file.write_all(b"a\n").unwrap();
    }
    {
        let mut file = fs::File::create(b_f).unwrap();
        file.write_all(b"b\n").unwrap();
    }

    cp_test(
        &[a_f, b_f, c],
        "",
        &format!("cp: will not overwrite just-created '{c_f}' with '{b_f}'\n"),
        1,
    );
    assert!(Path::new(a_f).exists());
    assert!(Path::new(b_f).exists());
    assert!(Path::new(c_f).exists());

    {
        let mut file = fs::File::open(c_f).unwrap();
        let mut buf = String::new();
        file.read_to_string(&mut buf).unwrap();
        assert_eq!(buf, "a\n"); // b/f should not overwrite file created by a/f
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/cp/cp-deref.sh
#[test]
fn test_cp_deref() {
    let test_dir = &format!("{}/test_cp_deref", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");
    let c = &format!("{test_dir}/c");
    let d = &format!("{test_dir}/d");
    let a_c = &format!("{test_dir}/a/c");
    let b_c = &format!("{test_dir}/b/c");

    fs::create_dir(test_dir).unwrap();
    for dir in [a, b, c, d] {
        fs::create_dir(dir).unwrap();
    }

    unix::fs::symlink("../c", a_c).unwrap();
    unix::fs::symlink("../c", b_c).unwrap();

    cp_test(&["-RL", a, b, d], "", "", 0);

    let a_c = Path::new(a_c);
    let b_c = Path::new(b_c);
    assert!(a_c.exists());
    assert!(a_c.is_dir());
    assert!(b_c.exists());
    assert!(b_c.is_dir());

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/cp/cp-HL.sh
#[test]
fn test_cp_hl() {
    let test_dir = &format!("{}/test_cp_hl", env!("CARGO_TARGET_TMPDIR"));
    let src_dir = &format!("{test_dir}/src-dir");
    let dest_dir = &format!("{test_dir}/dest-dir");
    let f = &format!("{test_dir}/f");
    let slink = &format!("{test_dir}/slink");
    let no_such_file = &format!("{test_dir}/no-such-file");
    let src_dir_slink = &format!("{test_dir}/src-dir/slink");
    let dest_dir_src_dir = &format!("{test_dir}/dest-dir/src-dir");
    let dest_dir_slink = &format!("{test_dir}/dest-dir/slink");
    let dest_dir_src_dir_slink = &format!("{test_dir}/dest-dir/src-dir/slink");
    let contents = "f\n";

    fs::create_dir(test_dir).unwrap();

    for dir in [src_dir, dest_dir] {
        fs::create_dir(dir).unwrap();
    }

    {
        let mut file = fs::File::create(f).unwrap();
        file.write_all(contents.as_bytes()).unwrap();
    }

    unix::fs::symlink(f, slink).unwrap();
    unix::fs::symlink(no_such_file, src_dir_slink).unwrap();

    cp_test(&["-H", "-R", slink, src_dir, dest_dir], "", "", 0);
    assert!(Path::new(src_dir).exists());
    assert!(Path::new(dest_dir_src_dir).exists());

    // Not a symlink
    {
        let mut file = fs::File::open(dest_dir_slink).unwrap();
        let mut buf = String::new();
        file.read_to_string(&mut buf).unwrap();
        assert_eq!(buf, contents);
    }

    // Dangling link
    assert!(fs::symlink_metadata(dest_dir_src_dir_slink).is_ok());
    assert!(fs::metadata(dest_dir_src_dir_slink).is_err());

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/cp/cp-i.sh
// Not including the -v, -n, -b and --update.
// Replicated the non-standard -v option by manually checking the contents.
#[test]
fn test_cp_i() {
    let test_dir = &format!("{}/test_cp_i", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");
    let c = &format!("{test_dir}/c");
    let d = &format!("{test_dir}/d");
    let b_a_c = &format!("{test_dir}/b/a/c");
    let a_c = &format!("{test_dir}/a/c");

    fs::create_dir(test_dir).unwrap();

    for dir in [a, b_a_c] {
        fs::create_dir_all(dir).unwrap();
    }

    fs::File::create(a_c).unwrap();

    cp_test_with_stdin(
        &["-iR", a, b],
        "n\n",
        "",
        &format!("cp: overwrite '{b_a_c}'? "),
        0,
    );

    for (filename, content) in [c, d].iter().zip(["c\n", "d\n"]) {
        let _ = fs::remove_file(filename);
        let mut file = fs::File::create(filename).unwrap();
        file.write_all(content.as_bytes()).unwrap();
    }

    cp_test_with_stdin(
        &["-i", c, d],
        "n\n",
        "",
        &format!("cp: overwrite '{d}'? "),
        0,
    );
    {
        let mut file = fs::File::open(d).unwrap();
        let mut buf = String::new();
        file.read_to_string(&mut buf).unwrap();
        assert_eq!(buf, "d\n");
    }

    cp_test_with_stdin(
        &["-i", c, d],
        "y\n",
        "",
        &format!("cp: overwrite '{d}'? "),
        0,
    );
    {
        let mut file = fs::File::open(d).unwrap();
        let mut buf = String::new();
        file.read_to_string(&mut buf).unwrap();
        assert_eq!(buf, "c\n"); // Overwritten because answered with "y"
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/cp/dir-slash.sh
#[test]
fn test_cp_dir_slash() {
    let test_dir = &format!("{}/test_cp_dir_slash", env!("CARGO_TARGET_TMPDIR"));
    let dir1 = &format!("{test_dir}/dir1");
    let dir2 = &format!("{test_dir}/dir2");
    let dir1_file = &format!("{test_dir}/dir1/file");
    let dir2_file = &format!("{test_dir}/dir2/file");
    let dir2_dir1_file = &format!("{test_dir}/dir2/dir1/file");

    fs::create_dir(test_dir).unwrap();
    for dir in [dir1, dir2] {
        fs::create_dir(dir).unwrap();
    }
    fs::File::create(dir1_file).unwrap();

    // Argument adds a slash to dir1
    cp_test(&["-R", &format!("{dir1}/"), dir2], "", "", 0);

    assert!(!Path::new(dir2_file).exists());

    assert!(Path::new(dir2_dir1_file).exists());
    assert!(Path::new(dir1_file).exists());

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/cp/dir-vs-file.sh
#[test]
fn test_cp_dir_vs_file() {
    let test_dir = &format!("{}/test_cp_dir_vs_file", env!("CARGO_TARGET_TMPDIR"));
    let dir = &format!("{test_dir}/dir");
    let file = &format!("{test_dir}/file");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(dir).unwrap();
    fs::File::create(file).unwrap();

    cp_test(
        &["-R", dir, file],
        "",
        &format!("cp: cannot overwrite non-directory '{file}' with directory '{dir}'\n"),
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/cp/existing-perm-dir.sh
#[test]
fn test_cp_existing_perm_dir() {
    let test_dir = &format!("{}/test_cp_existing_perm_dir", env!("CARGO_TARGET_TMPDIR"));
    let src = &format!("{test_dir}/src");
    let dst = &format!("{test_dir}/dst");
    let src_dir = &format!("{test_dir}/src/dir");
    let dst_dir = &format!("{test_dir}/dst/dir");

    fs::create_dir(test_dir).unwrap();

    unsafe {
        // Always succeeds
        libc::umask(0o002);
    }

    fs::DirBuilder::new()
        .mode(0o775)
        .recursive(true)
        .create(src_dir)
        .unwrap();
    fs::DirBuilder::new()
        .mode(0o700)
        .recursive(true)
        .create(dst_dir)
        .unwrap();

    cp_test(&["-R", &format!("{src}/."), &format!("{dst}/")], "", "", 0);

    let mode = fs::metadata(dst_dir).unwrap().mode();
    assert_eq!(mode & 0o777, 0o700); // Should be drwx-----

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/cp/fail-perm.sh
// Not including the tests for the --no-target-directory option.
#[test]
fn test_cp_fail_perm() {
    let test_dir = &format!("{}/test_cp_fail_perm", env!("CARGO_TARGET_TMPDIR"));
    let d = &format!("{test_dir}/D");
    let dd = &format!("{test_dir}/DD");
    let d_d = &format!("{test_dir}/D/D");
    let d_a = &format!("{test_dir}/D/a");
    let symlink = &format!("{test_dir}/symlink");
    let f = &format!("{test_dir}/f");

    let setgid = libc::S_ISGID as u32;
    let setuid = libc::S_ISUID as u32;

    fs::create_dir(test_dir).unwrap();

    // chmod g-s .
    let mode = fs::symlink_metadata(test_dir).unwrap().mode();
    fs::set_permissions(test_dir, fs::Permissions::from_mode(mode & !setgid)).unwrap();

    fs::create_dir(d).unwrap();
    fs::create_dir(d_d).unwrap();
    fs::File::create(d_a).unwrap();

    // chmod 0 D/a
    fs::set_permissions(d_a, fs::Permissions::from_mode(0)).unwrap();

    // chmod u=rx,go=,-st D
    let nonperm_bits = fs::symlink_metadata(d).unwrap().mode() & !(setgid | setuid) & !0o777;
    fs::set_permissions(d, fs::Permissions::from_mode(0o500 | nonperm_bits)).unwrap();

    cp_test(
        &["-pR", d, dd],
        "",
        &format!("cp: cannot open '{d_a}' for reading: Permission denied\n"),
        1,
    );

    // chmod 0 D
    fs::set_permissions(d, fs::Permissions::from_mode(0)).unwrap();
    unix::fs::symlink(d_d, symlink).unwrap();
    fs::File::create(f).unwrap();

    cp_test(
        &[f, symlink],
        "",
        &format!("cp: cannot stat '{symlink}': Permission denied\n"),
        1,
    );

    for f in [test_dir, d, d_a] {
        fs::set_permissions(f, fs::Permissions::from_mode(0o777)).unwrap();
    }
    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/cp/into-self.sh
// Not including the tests for the -l option.
#[test]
fn test_cp_into_self() {
    let test_dir = &format!("{}/test_cp_into_self", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let dir = &format!("{test_dir}/dir");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(a).unwrap();
    fs::create_dir(dir).unwrap();

    cp_test(
        &["-R", dir, dir],
        "",
        &format!("cp: cannot copy a directory, '{dir}', into itself, '{dir}/dir'\n"),
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/mv/i-2.sh
// Does not include the test for `mv`.
#[test]
fn test_cp_i_2() {
    let test_dir = &format!("{}/test_cp_i_2", env!("CARGO_TARGET_TMPDIR"));
    fs::create_dir(test_dir).unwrap();

    let e = &format!("{test_dir}/e");
    let f = &format!("{test_dir}/f");
    let g = &format!("{test_dir}/g");
    let h = &format!("{test_dir}/h");

    for (var, byte_string) in [(e, b"e\n"), (f, b"f\n"), (g, b"g\n"), (h, b"h\n")] {
        let mut file = fs::File::create(var).unwrap();
        file.write_all(byte_string).unwrap();

        // flush before `fs::set_permissions` below
        std::mem::drop(file);

        if var == f || var == h {
            // chmod 0
            let perm = fs::Permissions::from_mode(0);
            fs::set_permissions(var, perm).unwrap();
        }
    }

    cp_test_with_stdin(
        &["-if", e, f],
        "y\n",
        "",
        &format!("cp: replace '{f}', overriding mode 0000 (---------)? "),
        0,
    );
    assert!(Path::new(e).exists());
    assert!(Path::new(f).exists());

    let mut f_contents = String::new();
    let mut file = fs::File::open(f).unwrap();
    file.read_to_string(&mut f_contents).unwrap();
    assert_eq!(f_contents, "e\n");

    cp_test_with_stdin(
        &["-fi", g, h],
        "y\n",
        "",
        &format!("cp: replace '{h}', overriding mode 0000 (---------)? "),
        0,
    );
    assert!(Path::new(g).exists());
    assert!(Path::new(h).exists());

    let mut h_contents = String::new();
    let mut file = fs::File::open(h).unwrap();
    file.read_to_string(&mut h_contents).unwrap();
    assert_eq!(h_contents, "g\n");

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/cp/perm.sh
// Not including the `mv` tests.
#[test]
fn test_cp_perm() {
    let test_dir = &format!("{}/test_cp_perm", env!("CARGO_TARGET_TMPDIR"));
    let src = &format!("{test_dir}/src");
    let dest = &format!("{test_dir}/dest");

    fs::create_dir(test_dir).unwrap();

    let args_combination: Vec<Vec<&str>> = vec![
        vec![src, dest],
        vec!["-p", src, dest],
        vec!["-f", src, dest],
        vec!["-p", "-f", src, dest],
    ];

    for mask in [0o31, 0o37, 0o2] {
        unsafe {
            libc::umask(mask);
        }
        for args in &args_combination {
            for existing_dest in [true, false] {
                for g_perm in 1..=7 {
                    for o_perm in 1..=7 {
                        // u=r,g=rx,o
                        let src_mode = 0o450;
                        // u=rw,g=$g_perm,o=$o_perm
                        let dest_mode = 0o600 | g_perm << 3 | o_perm;

                        fs::File::create(src).unwrap();
                        fs::set_permissions(src, fs::Permissions::from_mode(src_mode)).unwrap();

                        if existing_dest {
                            fs::File::create(dest).unwrap();
                            fs::set_permissions(dest, fs::Permissions::from_mode(dest_mode))
                                .unwrap();
                        }

                        cp_test(args, "", "", 0);
                        assert!(Path::new(src).exists());

                        if !args.contains(&"-p") {
                            let mode = fs::metadata(dest).unwrap().mode();
                            if existing_dest {
                                assert_eq!(mode & 0o777, dest_mode);
                            } else {
                                // Directly inspect the mode bits instead of
                                // going through `stat` and `sed` like in the
                                // original
                                if mask == 0o37 {
                                    // sed 's/.....$/-----/'
                                    // Converts `rwxrwxrwx` to `rwxr-----`
                                    assert_eq!(mode & 0o777, src_mode & 0b111100000);
                                } else if mask == 0o31 {
                                    // sed 's/..\(..\).$/--\1-/'
                                    // Converts `rwxrwxrwx` to `rwxr--rw-`
                                    assert_eq!(mode & 0o777, src_mode & 0b111100110);
                                }
                            }
                        }

                        // Cleanup for the next loop
                        fs::set_permissions(src, fs::Permissions::from_mode(0o777)).unwrap();
                        fs::remove_file(src).unwrap();
                        fs::set_permissions(dest, fs::Permissions::from_mode(0o777)).unwrap();
                        fs::remove_file(dest).unwrap();
                    }
                }
            }
        }
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/cp/preserve-slink-time.sh
#[test]
fn test_cp_preserve_slink_time() {
    let test_dir = &format!(
        "{}/test_cp_preserve_slink_time",
        env!("CARGO_TARGET_TMPDIR")
    );
    let no_such = &format!("{test_dir}/no-such");
    let dangle = &format!("{test_dir}/dangle");
    let d2 = &format!("{test_dir}/d2");

    fs::create_dir(test_dir).unwrap();

    unix::fs::symlink(no_such, dangle).unwrap();

    std::thread::sleep(std::time::Duration::from_secs(2));

    cp_test(&["-Pp", dangle, d2], "", "", 0);

    unsafe {
        let dangle_cstr = CString::new(dangle.as_bytes()).unwrap();
        let mut dangle_stat = std::mem::MaybeUninit::zeroed().assume_init();
        let ret = libc::lstat(dangle_cstr.as_ptr(), &mut dangle_stat);
        if ret != 0 {
            panic!("{}", io::Error::last_os_error());
        }

        let d2_cstr = CString::new(d2.as_bytes()).unwrap();
        let mut d2_stat = std::mem::MaybeUninit::zeroed().assume_init();
        let ret = libc::lstat(d2_cstr.as_ptr(), &mut d2_stat);
        if ret != 0 {
            panic!("{}", io::Error::last_os_error());
        }

        assert_eq!(dangle_stat.st_atime, d2_stat.st_atime);
        assert_eq!(dangle_stat.st_atime_nsec, d2_stat.st_atime_nsec);
        assert_eq!(dangle_stat.st_mtime, d2_stat.st_mtime);
        assert_eq!(dangle_stat.st_mtime_nsec, d2_stat.st_mtime_nsec);
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/cp/r-vs-symlink.sh
#[test]
fn test_cp_r_vs_symlink() {
    let test_dir = &format!("{}/test_cp_r_vs_symlink", env!("CARGO_TARGET_TMPDIR"));
    let foo = &format!("{test_dir}/foo");
    let bar = &format!("{test_dir}/bar");
    let slink = &format!("{test_dir}/slink");
    let junk = &format!("{test_dir}/junk");
    let no_such_file = &format!("{test_dir}/no-such-file");
    let no_file = &format!("{test_dir}/no-file");

    fs::create_dir(test_dir).unwrap();

    {
        let mut file = fs::File::create(foo).unwrap();
        file.write_all(b"abc\n").unwrap();
    }
    unix::fs::symlink(foo, slink).unwrap();
    unix::fs::symlink(no_such_file, no_file).unwrap();

    cp_test(&["-r", no_file, junk], "", "", 0);
    cp_test(&["-r", slink, bar], "", "", 0);

    assert!(Path::new(bar).is_symlink());

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/cp/same-file.sh
// Only the -f flag is tested
#[test]
fn test_cp_same_file() {
    let test_dir = &format!("{}/test_cp_same_file", env!("CARGO_TARGET_TMPDIR"));
    let foo = &format!("{test_dir}/foo");
    let symlink = &format!("{test_dir}/symlink");
    let hardlink = &format!("{test_dir}/hardlink");
    let sl1 = &format!("{test_dir}/sl1");
    let sl2 = &format!("{test_dir}/sl2");
    let hlsl = &format!("{test_dir}/hlsl");
    let contents = "XYZ\n";

    let combination: &[(&[&str], &str, i32)] = &[
        (
            &[foo, symlink],
            &format!("cp: '{foo}' and '{symlink}' are the same file\n"),
            1,
        ),
        (
            &["-f", foo, symlink],
            &format!("cp: '{foo}' and '{symlink}' are the same file\n"),
            1,
        ),
        (
            &[symlink, foo],
            &format!("cp: '{symlink}' and '{foo}' are the same file\n"),
            1,
        ),
        (
            &["-f", symlink, foo],
            &format!("cp: '{symlink}' and '{foo}' are the same file\n"),
            1,
        ),
        (
            &[foo, foo],
            &format!("cp: '{foo}' and '{foo}' are the same file\n"),
            1,
        ),
        (
            &["-f", foo, foo],
            &format!("cp: '{foo}' and '{foo}' are the same file\n"),
            1,
        ),
        (
            &[sl1, sl2],
            &format!("cp: '{sl1}' and '{sl2}' are the same file\n"),
            1,
        ),
        (
            &["-f", sl1, sl2],
            &format!("cp: '{sl1}' and '{sl2}' are the same file\n"),
            1,
        ),
        (
            &[foo, hardlink],
            &format!("cp: '{foo}' and '{hardlink}' are the same file\n"),
            1,
        ),
        (
            &["-f", foo, hardlink],
            &format!("cp: '{foo}' and '{hardlink}' are the same file\n"),
            1,
        ),
        (
            &[hlsl, sl2],
            &format!("cp: '{hlsl}' and '{sl2}' are the same file\n"),
            1,
        ),
        (
            &["-f", hlsl, sl2],
            &format!("cp: '{hlsl}' and '{sl2}' are the same file\n"),
            1,
        ),
    ];

    for (args, err_msg, exit_code) in combination {
        fs::create_dir(test_dir).unwrap();
        {
            let mut file = fs::File::create(foo).unwrap();
            file.write_all(contents.as_bytes()).unwrap();
        }
        if args.contains(&symlink.as_str()) {
            unix::fs::symlink(foo, symlink).unwrap();
        }
        if args.contains(&hardlink.as_str()) {
            fs::hard_link(foo, hardlink).unwrap();
        }
        if args.contains(&sl1.as_str()) {
            unix::fs::symlink(foo, sl1).unwrap();
        }
        if args.contains(&sl2.as_str()) {
            unix::fs::symlink(foo, sl2).unwrap();
        }
        if args.contains(&hlsl.as_str()) {
            fs::hard_link(sl2, hlsl).unwrap();
        }

        cp_test(args, "", err_msg, *exit_code);

        // Only the last 2 items in the slice
        for filename in args.iter().rev().take(2) {
            let mut file = fs::File::open(filename).unwrap();
            let mut buf = String::new();
            file.read_to_string(&mut buf).unwrap();
            assert_eq!(buf, contents);
        }

        fs::remove_dir_all(test_dir).unwrap();
    }
}

// Port of coreutils/tests/cp/special-f.sh
#[test]
fn test_cp_special_f() {
    let test_dir = &format!("{}/test_cp_special_f", env!("CARGO_TARGET_TMPDIR"));
    let fifo = &format!("{test_dir}/fifo");
    let e = &format!("{test_dir}/e");

    fs::create_dir(test_dir).unwrap();

    unsafe {
        let fifo_cstr = CString::new(fifo.as_bytes()).unwrap();
        let ret = libc::mkfifo(fifo_cstr.as_ptr(), 0o644);
        if ret != 0 {
            panic!("{}", io::Error::last_os_error());
        }
    }
    fs::File::create(e).unwrap();

    let arg_combinations: [&[&str]; 2] = [&["-R", fifo, e], &["-R", "-f", fifo, e]];
    for args in arg_combinations {
        cp_test(args, "", "", 0);
        let md = fs::metadata(fifo).unwrap();
        let file_type = md.file_type();
        assert!(file_type.is_fifo());
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/cp/thru-dangling.sh
// Only includes the test with POSIXLY_CORRECT=1
#[test]
fn test_cp_thru_dangling() {
    let test_dir = &format!("{}/test_cp_thru_dangling", env!("CARGO_TARGET_TMPDIR"));
    let f = &format!("{test_dir}/f");
    let no_such = &format!("{test_dir}/no-such");
    let dangle = &format!("{test_dir}/dangle");
    let contents = "hi\n";

    fs::create_dir(test_dir).unwrap();

    unix::fs::symlink(no_such, dangle).unwrap();
    {
        let mut file = fs::File::create(f).unwrap();
        file.write_all(contents.as_bytes()).unwrap();
    }

    cp_test(&[f, dangle], "", "", 0);

    {
        let mut file = fs::File::open(no_such).unwrap();
        let mut buf = String::new();
        file.read_to_string(&mut buf).unwrap();
        assert_eq!(buf, contents);
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/cp/trailing-slash.sh
// Not including the tests for mv and the -T and -u options.
#[test]
fn test_cp_trailing_slash() {
    let test_dir = &format!("{}/test_cp_trailing_slash", env!("CARGO_TARGET_TMPDIR"));
    let d = &format!("{test_dir}/d");
    let e = &format!("{test_dir}/e");
    let b = &format!("{test_dir}/b");
    let no_such = &format!("{test_dir}/no-such");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(d).unwrap();

    cp_test(&["-r", d, &format!("{e}/")], "", "", 0);

    assert!(Path::new(d).exists());
    assert!(Path::new(e).exists());

    fs::File::create(b).unwrap();

    #[cfg(target_os = "linux")]
    let err_msg = "Not a directory";

    #[cfg(target_os = "macos")]
    let err_msg = "No such file or directory";

    cp_test(
        &[b, &format!("{no_such}/")],
        "",
        &format!("cp: cannot create regular file '{no_such}/': {err_msg}\n"),
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/mv/part-symlink.sh
// Not including --rem, -d, -b and the test for `mv`
#[test]
#[cfg_attr(not(target_os = "linux"), ignore)]
fn test_cp_part_symlink() {
    fn cd_and_cp_test(
        test_dir: &str,
        args: &[&str],
        expected_error: &str,
        expected_exit_code: i32,
    ) {
        let program = env!("CARGO_BIN_EXE_cp");
        let mut command = Command::new(program);
        let child = command
            .current_dir(test_dir)
            .args(args)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap();

        let output = child.wait_with_output().unwrap();

        let stderr = String::from_utf8_lossy(&output.stderr);
        assert_eq!(stderr, expected_error);

        assert_eq!(output.status.code(), Some(expected_exit_code));
    }

    let test_name = "test_cp_part_symlink";
    let test_dir = &format!("{}/{test_name}", env!("CARGO_TARGET_TMPDIR"));

    let other_dir = &format!(
        "{}/{test_name}",
        option_env!("OTHER_PARTITION_TMPDIR").unwrap_or("/dev/shm")
    );

    let contents = "XYZ\n";
    let loc_reg = "loc_reg";
    let loc_sl = "loc_sl";
    let dir = &format!("{test_dir}/dir");
    let rem_reg = &format!("{other_dir}/rem_reg");
    let rem_sl = &format!("{other_dir}/rem_sl");

    for (reg_abs, slink, args, err_str, err_code) in [
        // cp loc_reg rem_sl
        (
            format!("{dir}/{loc_reg}"),
            rem_sl.to_string(),
            [loc_reg, rem_sl.as_str()],
            format!("cp: '{loc_reg}' and '{rem_sl}' are the same file\n"),
            1,
        ),
        // cp rem_sl loc_reg
        (
            format!("{dir}/{loc_reg}"),
            rem_sl.to_string(),
            [rem_sl.as_str(), loc_reg],
            format!("cp: '{rem_sl}' and '{loc_reg}' are the same file\n"),
            1,
        ),
        // cp loc_sl rem_reg
        (
            rem_reg.to_string(),
            format!("{dir}/{loc_sl}"),
            [loc_sl, rem_reg.as_str()],
            format!("cp: '{loc_sl}' and '{rem_reg}' are the same file\n"),
            1,
        ),
        // cp rem_reg loc_sl
        (
            rem_reg.to_string(),
            format!("{dir}/{loc_sl}"),
            [rem_reg.as_str(), loc_sl],
            format!("cp: '{rem_reg}' and '{loc_sl}' are the same file\n"),
            1,
        ),
    ] {
        fs::create_dir(test_dir).unwrap();
        fs::create_dir(other_dir).unwrap();
        fs::create_dir(dir).unwrap();

        let mut f = fs::File::create(&reg_abs).unwrap();
        f.write_all(contents.as_bytes()).unwrap();

        unix::fs::symlink(&reg_abs, &slink).unwrap();

        cd_and_cp_test(&dir, &args, &err_str, err_code);

        fs::remove_dir_all(test_dir).unwrap();
        fs::remove_dir_all(other_dir).unwrap();
    }
}

// Port of coreutils/tests/cp/proc-short-read.sh
#[test]
#[cfg_attr(not(target_os = "linux"), ignore)]
fn test_cp_proc_short_read() {
    let test_dir = &format!("{}/test_cp_proc_short_read", env!("CARGO_TARGET_TMPDIR"));
    let cpuinfo = "/proc/cpuinfo";
    let out = &format!("{test_dir}/1");

    fs::create_dir(test_dir).unwrap();

    cp_test(&[cpuinfo, out], "", "", 0);

    let mut original = String::new();
    let mut file = fs::File::open(cpuinfo).unwrap();
    file.read_to_string(&mut original).unwrap();

    let mut copy = String::new();
    let mut file = fs::File::open(out).unwrap();
    file.read_to_string(&mut copy).unwrap();

    let lines_original: Vec<_> = original.lines().collect();
    let lines_copy: Vec<_> = copy.lines().collect();

    assert_eq!(lines_original.len(), lines_copy.len());
    for (line_original, line_copy) in lines_original.iter().zip(lines_copy.iter()) {
        if line_original.contains("MHz") || line_original.to_lowercase().contains("bogomips") {
            continue;
        }
        assert_eq!(line_original, line_copy);
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/cp/special-bits.sh
//
// This test needs root access and a non-root username passed in the
// `NON_ROOT_USERNAME` env var.
#[test]
#[cfg_attr(
    not(all(
        target_os = "linux",
        feature = "posixutils_test_all",
        feature = "requires_root"
    )),
    ignore
)]
fn test_cp_special_bits() {
    let test_dir = &format!("{}/test_cp_special_bits", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");
    let c = &format!("{test_dir}/c");
    let a2 = &format!("{test_dir}/a2");
    let b2 = &format!("{test_dir}/b2");
    let c2 = &format!("{test_dir}/c2");

    fs::create_dir(test_dir).unwrap();

    for filename in [a, b, c] {
        fs::File::create(filename).unwrap();
    }

    // chmod u+sx,go= a
    let a_mode = fs::metadata(a).unwrap().mode();
    fs::set_permissions(
        a,
        fs::Permissions::from_mode(a_mode | libc::S_IXUSR as u32 | libc::S_ISUID as u32),
    )
    .unwrap();

    // chmod u=rwx,g=sx,o= b
    fs::set_permissions(b, fs::Permissions::from_mode(0o710 | libc::S_ISGID as u32)).unwrap();

    // chmod a=r,ug+sx c
    fs::set_permissions(
        c,
        fs::Permissions::from_mode(0o554 | libc::S_ISUID as u32 | libc::S_ISGID as u32),
    )
    .unwrap();

    let non_root = option_env!("NON_ROOT_USERNAME").expect(
        "`test_cp_special_bits` requires the \
        `NON_ROOT_USERNAME` environment variable",
    );

    unsafe {
        let non_root_cstr = CString::new(non_root).unwrap();
        let passwd = libc::getpwnam(non_root_cstr.as_ptr());
        if passwd.is_null() {
            panic!("{}", io::Error::last_os_error());
        }
        let uid = (&*passwd).pw_uid;

        // chown "$NON_ROOT_USERNAME" .
        let md = fs::metadata(test_dir).unwrap();
        let test_dir_cstr = CString::new(test_dir.as_bytes()).unwrap();
        let ret = libc::chown(test_dir_cstr.as_ptr(), uid, md.gid());
        if ret != 0 {
            panic!("{}", io::Error::last_os_error());
        }
    }

    // chmod u=rwx,g=rx,o=rx .
    fs::set_permissions(c, fs::Permissions::from_mode(0o755)).unwrap();

    cp_test(&["-p", a, a2], "", "", 0);
    assert_eq!(
        fs::metadata(a).unwrap().mode(),
        fs::metadata(a2).unwrap().mode()
    );

    cp_test(&["-p", b, b2], "", "", 0);
    assert_eq!(
        fs::metadata(b).unwrap().mode(),
        fs::metadata(b2).unwrap().mode()
    );

    let mut child = Command::new("chroot")
        .args([
            "--skip-chdir",
            &format!("--user={non_root}"),
            "/",
            env!("CARGO_BIN_EXE_cp"),
            "-p",
            c,
            c2,
        ])
        .spawn()
        .unwrap();
    assert!(child.wait().unwrap().success());
    assert_eq!(
        fs::metadata(c).unwrap().mode(),
        fs::metadata(c2).unwrap().mode()
    );

    fs::remove_dir_all(test_dir).unwrap();
}
