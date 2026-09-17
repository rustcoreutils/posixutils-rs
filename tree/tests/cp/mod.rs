//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};
use std::ffi::CString;
use std::io::{Read, Write};
use std::os::unix::fs::FileTypeExt;
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

    // Cast needed: libc mode constants are u16 on macOS, u32 on Linux
    #[allow(clippy::unnecessary_cast)]
    let setgid = libc::S_ISGID as u32;
    #[allow(clippy::unnecessary_cast)]
    let setuid = libc::S_ISUID as u32;

    fs::create_dir(test_dir).unwrap();

    // chmod g-s .
    let mode = fs::symlink_metadata(test_dir).unwrap().mode();
    fs::set_permissions(test_dir, fs::Permissions::from_mode(mode & !setgid)).unwrap();

    fs::create_dir(d).unwrap();
    fs::create_dir(d_d).unwrap();
    fs::File::create(d_a).unwrap();

    // chmod 0 D/a
    fs::set_permissions(d_a, fs::Permissions::from_mode(0o0)).unwrap();

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
    fs::set_permissions(d, fs::Permissions::from_mode(0o0)).unwrap();
    unix::fs::symlink(d_d, symlink).unwrap();
    fs::File::create(f).unwrap();

    cp_test(
        &[f, symlink],
        "",
        &format!("cp: cannot stat '{symlink}': Permission denied\n"),
        1,
    );

    for f in [test_dir, d, d_a, dd] {
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
            let perm = fs::Permissions::from_mode(0o0);
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

    fs::create_dir(test_dir)
        .unwrap_or_else(|error| panic!("Error creating directory {test_dir:?}: {error:?}"));

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

        cd_and_cp_test(dir, &args, &err_str, err_code);

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
    let Some(non_root) = option_env!("NON_ROOT_USERNAME") else {
        eprintln!("Skipping: NON_ROOT_USERNAME not set");
        return;
    };

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
    // Cast needed: libc mode constants are u16 on macOS, u32 on Linux
    let a_mode = fs::metadata(a).unwrap().mode();
    #[allow(clippy::unnecessary_cast)]
    let mode_a = a_mode | libc::S_IXUSR as u32 | libc::S_ISUID as u32;
    fs::set_permissions(a, fs::Permissions::from_mode(mode_a)).unwrap();

    // chmod u=rwx,g=sx,o= b
    #[allow(clippy::unnecessary_cast)]
    let mode_b = 0o710 | libc::S_ISGID as u32;
    fs::set_permissions(b, fs::Permissions::from_mode(mode_b)).unwrap();

    // chmod a=r,ug+sx c
    #[allow(clippy::unnecessary_cast)]
    let mode_c = 0o554 | libc::S_ISUID as u32 | libc::S_ISGID as u32;
    fs::set_permissions(c, fs::Permissions::from_mode(mode_c)).unwrap();

    unsafe {
        let non_root_cstr = CString::new(non_root).unwrap();
        let passwd = libc::getpwnam(non_root_cstr.as_ptr());
        if passwd.is_null() {
            panic!("{}", io::Error::last_os_error());
        }
        let uid = (*passwd).pw_uid;

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

// `cp -p` must preserve S_ISUID/S_ISGID when ownership can be duplicated.
// Copying our own file reproduces the chown-succeeds path (same owner), so the bits stay.
#[test]
fn test_cp_preserve_keeps_setuid_same_owner() {
    let test_dir = &format!(
        "{}/test_cp_preserve_keeps_setuid_same_owner",
        env!("CARGO_TARGET_TMPDIR")
    );
    let s = &format!("{test_dir}/s");
    let s2 = &format!("{test_dir}/s2");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(s).unwrap();

    // Cast needed: libc mode constants are u16 on macOS, u32 on Linux
    #[allow(clippy::unnecessary_cast)]
    let setuid = libc::S_ISUID as u32;
    fs::set_permissions(s, fs::Permissions::from_mode(0o755 | setuid)).unwrap();

    cp_test(&["-p", s, s2], "", "", 0);

    // chown to our own uid succeeds, so the mode (including setuid) is preserved verbatim.
    assert_eq!(
        fs::metadata(s).unwrap().mode(),
        fs::metadata(s2).unwrap().mode()
    );
    assert_ne!(fs::metadata(s2).unwrap().mode() & setuid, 0);

    fs::remove_dir_all(test_dir).unwrap();
}

// `cp -p` must CLEAR S_ISUID/S_ISGID when the user/group ID cannot be duplicated
// (POSIX cp 90720-90721). A non-root user copying a root-owned setuid file cannot chown the copy
// back to root, so the privileged bits must be dropped. Needs root + `NON_ROOT_USERNAME`.
#[test]
#[cfg_attr(
    not(all(
        target_os = "linux",
        feature = "posixutils_test_all",
        feature = "requires_root"
    )),
    ignore
)]
fn test_cp_preserve_clears_setuid_on_chown_fail() {
    let Some(non_root) = option_env!("NON_ROOT_USERNAME") else {
        eprintln!("Skipping: NON_ROOT_USERNAME not set");
        return;
    };

    let test_dir = &format!(
        "{}/test_cp_preserve_clears_setuid_on_chown_fail",
        env!("CARGO_TARGET_TMPDIR")
    );
    let s = &format!("{test_dir}/s");
    let s2 = &format!("{test_dir}/s2");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(s).unwrap(); // owned by root

    // chmod 4755 (setuid + world rx so the non-root user can read it)
    fs::set_permissions(s, fs::Permissions::from_mode(0o4755)).unwrap();

    // Hand the directory to the non-root user so it can create the copy there.
    unsafe {
        let non_root_cstr = CString::new(non_root).unwrap();
        let passwd = libc::getpwnam(non_root_cstr.as_ptr());
        if passwd.is_null() {
            panic!("{}", io::Error::last_os_error());
        }
        let uid = (*passwd).pw_uid;
        let md = fs::metadata(test_dir).unwrap();
        let test_dir_cstr = CString::new(test_dir.as_bytes()).unwrap();
        if libc::chown(test_dir_cstr.as_ptr(), uid, md.gid()) != 0 {
            panic!("{}", io::Error::last_os_error());
        }
    }

    let mut child = Command::new("chroot")
        .args([
            "--skip-chdir",
            &format!("--user={non_root}"),
            "/",
            env!("CARGO_BIN_EXE_cp"),
            "-p",
            s,
            s2,
        ])
        .spawn()
        .unwrap();
    assert!(child.wait().unwrap().success());

    // Cast needed: libc mode constants are u16 on macOS, u32 on Linux
    #[allow(clippy::unnecessary_cast)]
    let id_bits = (libc::S_ISUID | libc::S_ISGID) as u32;
    let s2_mode = fs::metadata(s2).unwrap().mode();
    assert_eq!(
        s2_mode & id_bits,
        0,
        "setuid/setgid must be cleared when ownership cannot be duplicated (got mode {s2_mode:o})"
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Replicates failure to copy D/D on `test_cp_fail_perm` due to not OR'ing with S_IRWXU:
// https://github.com/rustcoreutils/posixutils-rs/issues/199
#[test]
fn test_cp_issue199() {
    let test_dir = &format!("{}/test_cp_issue199", env!("CARGO_TARGET_TMPDIR"));
    let d = &format!("{test_dir}/D");
    let dd = &format!("{test_dir}/DD");
    let d_d = &format!("{test_dir}/D/D");

    // Cast needed: libc mode constants are u16 on macOS, u32 on Linux
    #[allow(clippy::unnecessary_cast)]
    let setgid = libc::S_ISGID as u32;
    #[allow(clippy::unnecessary_cast)]
    let setuid = libc::S_ISUID as u32;

    fs::create_dir(test_dir).unwrap();

    let mode = fs::symlink_metadata(test_dir).unwrap().mode();
    fs::set_permissions(test_dir, fs::Permissions::from_mode(mode & !setgid)).unwrap();

    fs::create_dir(d).unwrap();
    fs::create_dir(d_d).unwrap();

    let nonperm_bits = fs::symlink_metadata(d).unwrap().mode() & !(setgid | setuid) & !0o777;
    fs::set_permissions(d, fs::Permissions::from_mode(0o500 | nonperm_bits)).unwrap();

    cp_test(&["-pR", d, dd], "", "", 0);

    fs::set_permissions(d, fs::Permissions::from_mode(0o777)).unwrap();
    fs::set_permissions(dd, fs::Permissions::from_mode(0o777)).unwrap();

    fs::remove_dir_all(test_dir).unwrap();
}

// More than one source with a target that is not an existing directory must be an
// error (POSIX cp 90605-90606), not a silent copy of only the first source.
#[test]
fn test_cp_multi_source_nondir_target() {
    let test_dir = &format!(
        "{}/test_cp_multi_source_nondir_target",
        env!("CARGO_TARGET_TMPDIR")
    );
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");
    let c = &format!("{test_dir}/c"); // does not exist

    fs::create_dir(test_dir).unwrap();
    fs::write(a, b"aaa").unwrap();
    fs::write(b, b"bbb").unwrap();

    cp_test(
        &[a, b, c],
        "",
        &format!("cp: target '{c}' is not a directory\n"),
        1,
    );
    assert!(!Path::new(c).exists());

    fs::remove_dir_all(test_dir).unwrap();
}

// A per-file failure during `cp -R` must not abort the whole copy — same-level and
// ancestor entries are still copied (POSIX cp 90829-90832). One unreadable file among several
// readable siblings: all readable siblings must still be copied (regardless of readdir order),
// and the exit status is non-zero.
#[test]
fn test_cp_recursive_continue_on_error() {
    let test_dir = &format!(
        "{}/test_cp_recursive_continue_on_error",
        env!("CARGO_TARGET_TMPDIR")
    );
    let src = &format!("{test_dir}/src");
    let dst = &format!("{test_dir}/dst");
    let u = &format!("{src}/u"); // unreadable

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(src).unwrap();
    for name in ["a", "b", "c"] {
        fs::write(format!("{src}/{name}"), name.as_bytes()).unwrap();
    }
    fs::File::create(u).unwrap();
    fs::set_permissions(u, fs::Permissions::from_mode(0o0)).unwrap();

    cp_test(
        &["-R", src, dst],
        "",
        &format!("cp: cannot open '{u}' for reading: Permission denied\n"),
        1,
    );

    // The readable siblings were copied despite the failure.
    for name in ["a", "b", "c"] {
        assert!(
            Path::new(&format!("{dst}/{name}")).exists(),
            "{name} should have been copied"
        );
    }

    fs::set_permissions(u, fs::Permissions::from_mode(0o644)).unwrap();
    fs::remove_dir_all(test_dir).unwrap();
}

fn is_root() -> bool {
    unsafe { libc::geteuid() == 0 }
}

// A subdirectory the walk cannot descend into must not shift every later file one level deeper.
// `ftw` told the copy engine to descend, which pushed a target directory descriptor, and then
// refused; without the matching unwind the stack stayed too deep and the remaining siblings were
// written inside the failed directory instead of beside it.
#[test]
fn test_cp_unreadable_subdir_does_not_misplace_siblings() {
    if is_root() {
        eprintln!("Skipping test: root can descend into a mode-0 directory");
        return;
    }

    let test_dir = &format!(
        "{}/test_cp_unreadable_subdir_does_not_misplace_siblings",
        env!("CARGO_TARGET_TMPDIR")
    );
    let src = &format!("{test_dir}/src");
    let dst = &format!("{test_dir}/dst");
    // Sorts before the files, so an unbalanced stack would capture all of them.
    let locked = &format!("{src}/aaa_locked");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(src).unwrap();
    fs::create_dir(locked).unwrap();
    for name in ["zzz_b", "zzz_c", "zzz_d"] {
        fs::write(format!("{src}/{name}.txt"), b"x").unwrap();
    }
    fs::set_permissions(locked, fs::Permissions::from_mode(0o000)).unwrap();

    cp_test(
        &["-R", src, dst],
        "",
        &format!("cp: cannot access '{locked}': Permission denied\n"),
        1,
    );

    fs::set_permissions(locked, fs::Permissions::from_mode(0o755)).unwrap();

    for name in ["zzz_b", "zzz_c", "zzz_d"] {
        assert!(
            Path::new(&format!("{dst}/{name}.txt")).exists(),
            "{name}.txt was not copied beside the failed directory"
        );
        assert!(
            !Path::new(&format!("{dst}/aaa_locked/{name}.txt")).exists(),
            "{name}.txt was written inside the directory that could not be read"
        );
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// POSIX 90699-90700 and step 3.a.iii: -f means "if the destination cannot be opened, unlink it
// and try again". It is not a prompt. Prompting made `cp -f` read EOF from a script's stdin,
// treat that as "no", and exit 0 with the destination never written.
#[test]
fn test_cp_f_does_not_prompt_without_tty() {
    if is_root() {
        eprintln!("Skipping test: root can write a mode-0444 file without unlinking it");
        return;
    }

    let test_dir = &format!(
        "{}/test_cp_f_does_not_prompt_without_tty",
        env!("CARGO_TARGET_TMPDIR")
    );
    let src = &format!("{test_dir}/src");
    let dst = &format!("{test_dir}/dst");

    fs::create_dir(test_dir).unwrap();
    fs::write(src, b"NEWDATA\n").unwrap();
    fs::write(dst, b"OLDDATA\n").unwrap();
    fs::set_permissions(dst, fs::Permissions::from_mode(0o444)).unwrap();

    cp_test(&["-f", src, dst], "", "", 0);

    let mut contents = String::new();
    fs::File::open(dst)
        .unwrap()
        .read_to_string(&mut contents)
        .unwrap();
    assert_eq!(contents, "NEWDATA\n", "-f left the destination unchanged");

    fs::remove_dir_all(test_dir).unwrap();
}

// The prompt is a -i behavior (POSIX 90703-90705). An unwritable destination only changes its
// wording, as in GNU cp, and does not make -f prompt.
#[test]
fn test_cp_i_unwritable_prompt_wording() {
    if is_root() {
        eprintln!("Skipping test: root is not blocked by the mode bits under test");
        return;
    }

    let test_dir = &format!(
        "{}/test_cp_i_unwritable_prompt_wording",
        env!("CARGO_TARGET_TMPDIR")
    );
    let src = &format!("{test_dir}/src");
    let dst = &format!("{test_dir}/dst");

    fs::create_dir(test_dir).unwrap();
    fs::write(src, b"NEWDATA\n").unwrap();
    fs::write(dst, b"OLDDATA\n").unwrap();
    fs::set_permissions(dst, fs::Permissions::from_mode(0o444)).unwrap();

    cp_test_with_stdin(
        &["-i", src, dst],
        "n\n",
        "",
        &format!("cp: replace '{dst}', overriding mode 0444 (r--r--r--)? "),
        0,
    );

    let mut contents = String::new();
    fs::File::open(dst)
        .unwrap()
        .read_to_string(&mut contents)
        .unwrap();
    assert_eq!(contents, "OLDDATA\n", "a declined prompt still copied");

    fs::remove_dir_all(test_dir).unwrap();
}

// -P must act on the link itself, both for an operand and for links found during the walk
// (POSIX 90621-90623). While -P was inert, `cp -RP` followed a symlinked directory and copied
// the real files it pointed at, which is exactly what -P is passed to prevent.
#[test]
fn test_cp_rp_does_not_escape_source_tree() {
    let test_dir = &format!(
        "{}/test_cp_rp_does_not_escape_source_tree",
        env!("CARGO_TARGET_TMPDIR")
    );
    let outside = &format!("{test_dir}/outside");
    let src = &format!("{test_dir}/src");
    let escape = &format!("{src}/escape");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(outside).unwrap();
    fs::write(format!("{outside}/secret.txt"), b"secret\n").unwrap();
    fs::create_dir(src).unwrap();
    // Absolute, so the copy cannot resolve it by accident from its new location.
    unix::fs::symlink(outside, escape).unwrap();

    for (flags, dst_name) in [(&["-R", "-P"][..], "dst_p"), (&["-R"][..], "dst_default")] {
        let dst = &format!("{test_dir}/{dst_name}");
        let mut args = flags.to_vec();
        args.push(src);
        args.push(dst);
        cp_test(&args, "", "", 0);

        let copied = &format!("{dst}/escape");
        assert!(
            fs::symlink_metadata(copied).unwrap().is_symlink(),
            "{dst_name}: the symlink was resolved instead of recreated"
        );
        assert_eq!(
            fs::read_link(copied).unwrap(),
            Path::new(outside),
            "{dst_name}: the link was not reproduced verbatim"
        );
        // Following the recreated link reaches the original, which is expected; what must not
        // happen is a real copy of it appearing inside the destination.
        assert!(
            !fs::symlink_metadata(format!("{dst}/escape/"))
                .map(|md| md.is_dir())
                .unwrap_or(false)
                || fs::symlink_metadata(copied).unwrap().is_symlink(),
            "{dst_name}: the referenced directory was duplicated into the destination"
        );
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// -L acts on what the link refers to, for operands and for links met during the walk
// (POSIX 90706-90708).
#[test]
fn test_cp_rl_dereferences_symlink_in_hierarchy() {
    let test_dir = &format!(
        "{}/test_cp_rl_dereferences_symlink_in_hierarchy",
        env!("CARGO_TARGET_TMPDIR")
    );
    let src = &format!("{test_dir}/src");
    let dst = &format!("{test_dir}/dst");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(src).unwrap();
    fs::write(format!("{src}/real"), b"REAL\n").unwrap();
    unix::fs::symlink("real", format!("{src}/link")).unwrap();

    cp_test(&["-R", "-L", src, dst], "", "", 0);

    let link_copy = &format!("{dst}/link");
    assert!(
        !fs::symlink_metadata(link_copy).unwrap().is_symlink(),
        "-L recreated the link instead of copying what it refers to"
    );
    let mut contents = String::new();
    fs::File::open(link_copy)
        .unwrap()
        .read_to_string(&mut contents)
        .unwrap();
    assert_eq!(contents, "REAL\n");

    fs::remove_dir_all(test_dir).unwrap();
}

// POSIX 90727-90728: more than one of -H, -L, -P is not an error, and the last one wins.
#[test]
fn test_cp_deref_flags_last_wins() {
    let test_dir = &format!(
        "{}/test_cp_deref_flags_last_wins",
        env!("CARGO_TARGET_TMPDIR")
    );
    let src = &format!("{test_dir}/src");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(src).unwrap();
    fs::write(format!("{src}/real"), b"REAL\n").unwrap();
    unix::fs::symlink("real", format!("{src}/link")).unwrap();

    // (flags, destination, whether the copied entry should still be a symlink)
    let cases: [(&[&str], &str, bool); 3] = [
        (&["-R", "-L", "-P"], "a", true),
        (&["-R", "-P", "-L"], "b", false),
        (&["-R", "-H", "-P"], "c", true),
    ];
    for (flags, dst_name, expect_symlink) in cases {
        let dst = &format!("{test_dir}/{dst_name}");
        let mut args = flags.to_vec();
        args.push(src);
        args.push(dst);
        cp_test(&args, "", "", 0);

        assert_eq!(
            fs::symlink_metadata(format!("{dst}/link"))
                .unwrap()
                .is_symlink(),
            expect_symlink,
            "{flags:?} did not take the last-specified option"
        );
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// The cp synopsis (POSIX 90580) allows -P without -R; only -H and -L require it.
#[test]
fn test_cp_p_without_r_is_accepted() {
    let test_dir = &format!(
        "{}/test_cp_p_without_r_is_accepted",
        env!("CARGO_TARGET_TMPDIR")
    );
    let real = &format!("{test_dir}/real");
    let link = &format!("{test_dir}/link");
    let dst = &format!("{test_dir}/dst");

    fs::create_dir(test_dir).unwrap();
    fs::write(real, b"REAL\n").unwrap();
    unix::fs::symlink("real", link).unwrap();

    cp_test(&["-P", link, dst], "", "", 0);
    assert!(fs::symlink_metadata(dst).unwrap().is_symlink());

    fs::remove_dir_all(test_dir).unwrap();
}

// Without -R and without -P, cp acts on what the link refers to (POSIX 90610-90612), so a
// dangling link is an error rather than something to recreate.
#[test]
fn test_cp_dangling_source_is_an_error() {
    let test_dir = &format!(
        "{}/test_cp_dangling_source_is_an_error",
        env!("CARGO_TARGET_TMPDIR")
    );
    let dangle = &format!("{test_dir}/dangle");
    let dst = &format!("{test_dir}/dst");

    fs::create_dir(test_dir).unwrap();
    unix::fs::symlink("no-such-file", dangle).unwrap();

    cp_test(
        &[dangle, dst],
        "",
        &format!("cp: cannot stat '{dangle}': No such file or directory\n"),
        1,
    );
    assert!(fs::symlink_metadata(dst).is_err());

    fs::remove_dir_all(test_dir).unwrap();
}

fn mkfifo_at(path: &str, mode: libc::mode_t) {
    let c = CString::new(path.as_bytes()).unwrap();
    let ret = unsafe { libc::mkfifo(c.as_ptr(), mode) };
    if ret != 0 {
        panic!("{}", io::Error::last_os_error());
    }
}

// The special-file path ran before step 1 and before all of the -i and -f handling, and unlinked
// whatever was at the destination unconditionally, so `cp -R -i` destroyed an existing file
// without ever asking.
#[test]
fn test_cp_special_i_does_not_clobber() {
    let test_dir = &format!(
        "{}/test_cp_special_i_does_not_clobber",
        env!("CARGO_TARGET_TMPDIR")
    );
    let fifo = &format!("{test_dir}/fifo");
    let target = &format!("{test_dir}/target");

    fs::create_dir(test_dir).unwrap();
    mkfifo_at(fifo, 0o644);
    fs::write(target, b"IMPORTANT\n").unwrap();

    cp_test_with_stdin(
        &["-R", "-i", fifo, target],
        "n\n",
        "",
        &format!("cp: overwrite '{target}'? "),
        0,
    );

    assert!(fs::metadata(target).unwrap().file_type().is_file());
    let mut contents = String::new();
    fs::File::open(target)
        .unwrap()
        .read_to_string(&mut contents)
        .unwrap();
    assert_eq!(
        contents, "IMPORTANT\n",
        "the declined copy went ahead anyway"
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Step 1 applies to special files too: copying one onto itself unlinked and recreated the source.
#[test]
fn test_cp_special_same_file() {
    let test_dir = &format!("{}/test_cp_special_same_file", env!("CARGO_TARGET_TMPDIR"));
    let fifo = &format!("{test_dir}/fifo");

    fs::create_dir(test_dir).unwrap();
    mkfifo_at(fifo, 0o644);

    cp_test(
        &["-R", fifo, fifo],
        "",
        &format!("cp: '{fifo}' and '{fifo}' are the same file\n"),
        1,
    );
    assert!(fs::metadata(fifo).unwrap().file_type().is_fifo());

    fs::remove_dir_all(test_dir).unwrap();
}

// POSIX 90682 step 4.a: the destination is created with the same file type as the source. mknod
// with no type bits creates a regular file, so an unprivileged `cp -R /dev/zero t` used to
// "succeed" with an empty regular file instead of failing.
#[test]
fn test_cp_special_device_keeps_its_type() {
    if is_root() {
        eprintln!("Skipping test: root may create device nodes");
        return;
    }

    let test_dir = &format!(
        "{}/test_cp_special_device_keeps_its_type",
        env!("CARGO_TARGET_TMPDIR")
    );
    let target = &format!("{test_dir}/target");

    fs::create_dir(test_dir).unwrap();

    // /dev/null rather than /dev/zero deliberately: if this path ever regresses to reading the
    // device as an ordinary file, an empty read ends immediately, where /dev/zero would write
    // until the filesystem filled up.
    cp_test(
        &["-R", "/dev/null", target],
        "",
        &format!("cp: cannot create special file '{target}': Operation not permitted\n"),
        1,
    );
    assert!(
        fs::symlink_metadata(target).is_err(),
        "a file was created where the device node could not be"
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// POSIX 90683-90685: a FIFO gets the source's permission bits, not a hard-coded 0644.
#[test]
fn test_cp_special_fifo_mode_from_source() {
    let test_dir = &format!(
        "{}/test_cp_special_fifo_mode_from_source",
        env!("CARGO_TARGET_TMPDIR")
    );
    let fifo = &format!("{test_dir}/fifo");
    let copy = &format!("{test_dir}/copy");

    fs::create_dir(test_dir).unwrap();
    mkfifo_at(fifo, 0o644);
    fs::set_permissions(fifo, fs::Permissions::from_mode(0o600)).unwrap();

    cp_test(&["-R", fifo, copy], "", "", 0);

    let md = fs::metadata(copy).unwrap();
    assert!(md.file_type().is_fifo());
    assert_eq!(
        md.permissions().mode() & 0o077,
        0,
        "group and other bits came from somewhere other than the source"
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Without -p, cp must not hand the copy a set-user-ID bit: the new file belongs to whoever ran
// cp, so preserving it would grant that user's privileges to anyone who can execute it.
#[test]
fn test_cp_setuid_not_preserved_without_p() {
    let test_dir = &format!(
        "{}/test_cp_setuid_not_preserved_without_p",
        env!("CARGO_TARGET_TMPDIR")
    );
    let src = &format!("{test_dir}/src");
    let dst = &format!("{test_dir}/dst");

    fs::create_dir(test_dir).unwrap();
    fs::write(src, b"x").unwrap();
    fs::set_permissions(src, fs::Permissions::from_mode(0o4755)).unwrap();

    cp_test(&[src, dst], "", "", 0);

    #[allow(clippy::unnecessary_cast)]
    let setid = (libc::S_ISUID as u32) | (libc::S_ISGID as u32);
    assert_eq!(
        fs::metadata(dst).unwrap().permissions().mode() & setid,
        0,
        "the copy carries a set-user-ID or set-group-ID bit"
    );

    fs::remove_dir_all(test_dir).unwrap();
}

/// Runs `cp` directly with a wall-clock limit, killing it if it overruns.
///
/// The copy-into-itself tests describe a runaway recursion, so a regression would otherwise run
/// until the filesystem filled up rather than failing the test.
fn cp_test_bounded(cwd: &str, args: &[&str], expected_error: &str, expected_exit_code: i32) {
    use std::time::{Duration, Instant};

    let mut child = Command::new(env!("CARGO_BIN_EXE_cp"))
        .args(args)
        .current_dir(cwd)
        .env("LC_ALL", "C")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    let deadline = Instant::now() + Duration::from_secs(20);
    loop {
        match child.try_wait().unwrap() {
            Some(_) => break,
            None if Instant::now() >= deadline => {
                let _ = child.kill();
                let _ = child.wait();
                panic!("cp did not terminate: {args:?}");
            }
            None => std::thread::sleep(Duration::from_millis(50)),
        }
    }

    let output = child.wait_with_output().unwrap();
    assert_eq!(String::from_utf8_lossy(&output.stderr), expected_error);
    assert_eq!(output.status.code(), Some(expected_exit_code));
}

// The guard against copying a directory into itself compared path text, so any other spelling of
// the same directory defeated it and the copy recursed without bound.
#[test]
fn test_cp_into_self_alt_spelling() {
    let test_dir = &format!(
        "{}/test_cp_into_self_alt_spelling",
        env!("CARGO_TARGET_TMPDIR")
    );
    let a = &format!("{test_dir}/a");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(a).unwrap();
    fs::write(format!("{a}/f"), b"x").unwrap();

    // Relative and spelled with a `.`, as a user would type it. Comparing path text could be
    // defeated by any spelling that names the same directory differently.
    cp_test_bounded(
        test_dir,
        &["-R", "./a", "a/b"],
        "cp: cannot copy a directory, './a', into itself, 'a/b'\n",
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// The guard was skipped entirely when the destination already existed.
#[test]
fn test_cp_into_self_existing_target() {
    let test_dir = &format!(
        "{}/test_cp_into_self_existing_target",
        env!("CARGO_TARGET_TMPDIR")
    );
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/a/b");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(a).unwrap();
    fs::create_dir(b).unwrap();
    fs::write(format!("{a}/f"), b"x").unwrap();

    cp_test_bounded(
        test_dir,
        &["-R", "a", "a/b"],
        "cp: cannot copy a directory, 'a', into itself, 'a/b/a'\n",
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// A destination that is an existing symbolic link to a regular file is written *through*, as
// POSIX 90658-90661 and GNU both do. Hardening the creating open must not change that.
#[test]
fn test_cp_overwrites_symlink_target() {
    let test_dir = &format!(
        "{}/test_cp_overwrites_symlink_target",
        env!("CARGO_TARGET_TMPDIR")
    );
    let real = &format!("{test_dir}/real");
    let link = &format!("{test_dir}/link");
    let src = &format!("{test_dir}/src");

    fs::create_dir(test_dir).unwrap();
    fs::write(real, b"OLD\n").unwrap();
    unix::fs::symlink("real", link).unwrap();
    fs::write(src, b"NEW\n").unwrap();

    cp_test(&[src, link], "", "", 0);

    assert!(
        fs::symlink_metadata(link).unwrap().is_symlink(),
        "the link was replaced instead of written through"
    );
    let mut contents = String::new();
    fs::File::open(real)
        .unwrap()
        .read_to_string(&mut contents)
        .unwrap();
    assert_eq!(contents, "NEW\n");

    fs::remove_dir_all(test_dir).unwrap();
}
