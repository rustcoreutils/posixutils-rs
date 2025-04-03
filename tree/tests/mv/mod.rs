//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};
use std::ffi::CString;
use std::fs::{self, Permissions};
use std::io::{self, Read, Write};
use std::os::unix::fs::{FileTypeExt, MetadataExt, PermissionsExt};
use std::os::unix::{self};
use std::path::Path;
use std::process::{Command, Stdio};

fn mv_test(args: &[&str], expected_output: &str, expected_error: &str, expected_exit_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("mv"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

fn mv_test_with_stdin(
    args: &[&str],
    stdin_data: &str,
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("mv"),
        args: str_args,
        stdin_data: String::from(stdin_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

// Port of coreutils/tests/mv/atomic.sh
#[test]
fn test_mv_atomic() {
    let test_dir = &format!("{}/test_mv_atomic", env!("CARGO_TARGET_TMPDIR"));
    let t1 = &format!("{test_dir}/t1");
    let s1 = &format!("{test_dir}/s1");
    let t2 = &format!("{test_dir}/t2");
    let s2 = &format!("{test_dir}/s2");

    fs::create_dir(test_dir).unwrap();

    // Dangling symlinks
    unix::fs::symlink(t1, s1).unwrap();
    unix::fs::symlink(t2, s2).unwrap();

    // The original command is:
    // strace -qe unlink mv -T s1 s2
    //
    // The mv implementation here doesn't call `unlink`.
    // -T doesn't matter here since s2 is not a directory.
    mv_test(&[s1, s2], "", "", 0);

    assert!(!Path::new(s1).exists());
    assert_eq!(fs::read_link(s2).unwrap(), Path::new(t1));

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/mv/atomic2.sh
#[test]
fn test_mv_atomic2() {
    let test_dir = &format!("{}/test_mv_atomic2", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");
    let b2 = &format!("{test_dir}/b2");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(a).unwrap();
    fs::File::create(b).unwrap();
    unix::fs::symlink(b, b2).unwrap();

    // The original command is:
    // strace -qe unlink mv a b
    //
    // As in `test_mv_atomic`, the check for `unlink` is not applicable to this
    // implementation.
    mv_test(&[a, b], "", "", 0);

    assert!(!Path::new(a).exists());

    let metadata = Path::new(b).metadata().unwrap();
    assert_eq!(metadata.nlink(), 1);

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/mv/childproof.sh
// This only includes the tests for `mv` and not `cp`.
#[test]
fn test_mv_childproof() {
    let test_dir = &format!("{}/test_mv_childproof", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");
    let c = &format!("{test_dir}/c");
    let a_f = &format!("{test_dir}/a/f");
    let b_f = &format!("{test_dir}/b/f");
    let c_f = &format!("{test_dir}/c/f");
    let b_g = &format!("{test_dir}/b/g");
    let c_g = &format!("{test_dir}/c/g");

    fs::create_dir(test_dir).unwrap();
    for d in [a, b, c] {
        fs::create_dir(d).unwrap();
    }

    for (f, bytes) in [(a_f, b"a\n"), (b_f, b"b\n")] {
        let mut file = fs::File::create(f).unwrap();
        file.write_all(bytes).unwrap();
    }

    mv_test(
        &[a_f, b_f, c],
        "",
        &format!("mv: will not overwrite just-created '{c_f}' with '{b_f}'\n"),
        1,
    );
    assert!(!Path::new(a_f).exists());
    assert!(Path::new(b_f).exists());
    assert!(Path::new(c_f).exists());

    // c/f should contain the string "a"
    {
        let mut buf = Vec::new();
        let mut file = fs::File::open(c_f).unwrap();
        file.read_to_end(&mut buf).unwrap();
        let s = String::from_utf8(buf).unwrap();
        assert_eq!(s, "a\n");
    }

    fs::File::create(a_f).unwrap();
    fs::hard_link(a_f, b_g).unwrap();
    mv_test(&[a_f, b_g, c], "", "", 0);
    assert!(!Path::new(a_f).exists());
    assert!(!Path::new(b_g).exists());
    assert!(Path::new(c_f).exists());
    assert!(Path::new(c_g).exists());

    for f in [a_f, b_f, b_g] {
        fs::File::create(f).unwrap();
    }
    mv_test(
        &[a_f, b_f, b_g, c],
        "",
        &format!("mv: will not overwrite just-created '{c_f}' with '{b_f}'\n"),
        1,
    );
    assert!(!Path::new(a_f).exists());
    assert!(Path::new(b_f).exists());
    assert!(!Path::new(b_g).exists());
    assert!(Path::new(c_f).exists());
    assert!(Path::new(c_g).exists());

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/mv/dir-file.sh
#[test]
fn test_mv_dir_file() {
    let test_dir = &format!("{}/test_mv_dir_file", env!("CARGO_TARGET_TMPDIR"));
    let dir = &format!("{test_dir}/dir");
    let dir_file = &format!("{test_dir}/dir/file");
    let file = &format!("{test_dir}/file");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(dir).unwrap();
    fs::create_dir(dir_file).unwrap();
    fs::File::create(file).unwrap();

    mv_test(
        &[dir, file],
        "",
        &format!("mv: cannot overwrite non-directory '{file}' with directory '{dir}'\n"),
        1,
    );
    mv_test(
        &[file, dir],
        "",
        &format!("mv: cannot overwrite directory '{dir_file}' with non-directory '{file}'\n"),
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/mv/dir2dir.sh
#[test]
fn test_mv_dir2dir() {
    let test_dir = &format!("{}/test_mv_dir2dir", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");
    let a_t = &format!("{test_dir}/a/t");
    let b_t = &format!("{test_dir}/b/t");
    let a_t_f = &format!("{test_dir}/a/t/f");

    fs::create_dir(test_dir).unwrap();
    for d in [a, b, a_t, b_t] {
        fs::create_dir(d).unwrap();
    }
    fs::File::create(a_t_f).unwrap();

    mv_test(
        &[b_t, a],
        "",
        &format!("mv: cannot move '{b_t}' to '{a_t}': Directory not empty\n"),
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/mv/dup-source.sh
#[test]
fn test_mv_dup_source() {
    let test_dir = &format!("{}/test_mv_dup_source", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");
    let d = &format!("{test_dir}/d");

    fs::create_dir(test_dir).unwrap();

    let reset_files = || {
        let _ = fs::remove_file(a);
        let _ = fs::remove_dir_all(b);
        let _ = fs::remove_dir_all(d);

        fs::create_dir(b).unwrap();
        fs::create_dir(d).unwrap();
        fs::File::create(a).unwrap();
    };

    // mv a a d/
    reset_files();
    mv_test(
        &[a, a, d],
        "",
        &format!("mv: cannot move '{a}' to '{d}/a': No such file or directory\n"),
        1,
    );
    assert!(!Path::new(a).exists());

    // Skipping `mv ./a a d/`
    // It's redundant with the previous test and would require chaiging the
    // current directory of the `TestPlan` subprocess

    // mv ./b b d/
    reset_files();
    mv_test(
        &[b, b, d],
        "",
        &format!("mv: cannot move '{b}' to '{d}/b': No such file or directory\n"),
        1,
    );
    assert!(!Path::new(b).exists());

    // Skipping `mv --verbose ./b b d/`
    // --verbose is not supported

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/mv/force.sh
#[test]
fn test_mv_force() {
    let test_dir = &format!("{}/test_mv_force", env!("CARGO_TARGET_TMPDIR"));
    let ff = &format!("{test_dir}/mvforce");
    let ff2 = &format!("{test_dir}/mvforce2");

    fs::create_dir(test_dir).unwrap();

    let mut file = fs::File::create(ff).unwrap();
    file.write_all(b"force-contents\n").unwrap();

    fs::hard_link(ff, ff2).unwrap();

    mv_test(
        &[ff, ff],
        "",
        &format!("mv: '{ff}' and '{ff}' are the same file\n"),
        1,
    );
    mv_test(
        &[ff, ff2],
        "",
        &format!("mv: '{ff}' and '{ff2}' are the same file\n"),
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/mv/hard-2.sh
// This only includes the tests for `mv` and not `cp`.
#[test]
fn test_mv_hard_2() {
    let test_dir = &format!("{}/test_mv_hard_2", env!("CARGO_TARGET_TMPDIR"));
    let dst = &format!("{test_dir}/dst");
    let dst_a = &format!("{test_dir}/dst/a");
    let dst_b = &format!("{test_dir}/dst/b");
    let dst_c = &format!("{test_dir}/dst/c");
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");
    let c = &format!("{test_dir}/c");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(dst).unwrap();
    for f in [dst_a, dst_b, dst_c] {
        fs::File::create(f).unwrap();
    }
    fs::File::create(a).unwrap();

    fs::hard_link(a, b).unwrap();
    fs::hard_link(a, c).unwrap();

    mv_test(&[a, b, c, dst], "", "", 0);

    assert!(!Path::new(a).exists());
    assert!(!Path::new(b).exists());
    assert!(!Path::new(c).exists());

    assert!(Path::new(dst_a).exists());
    assert!(Path::new(dst_b).exists());
    assert!(Path::new(dst_c).exists());

    let inode_a = Path::new(dst_a).metadata().unwrap().ino();
    let inode_b = Path::new(dst_b).metadata().unwrap().ino();
    let inode_c = Path::new(dst_c).metadata().unwrap().ino();
    assert_eq!(inode_a, inode_b);
    assert_eq!(inode_a, inode_c);

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/mv/hard-4.sh
// --backup in coreutils is not supported
#[test]
fn test_mv_hard_4() {
    let test_dir = &format!("{}/test_mv_hard_4", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(a).unwrap();
    fs::hard_link(a, b).unwrap();

    mv_test(
        &[a, b],
        "",
        &format!("mv: '{a}' and '{b}' are the same file\n"),
        1,
    );
    assert!(Path::new(a).exists());
    assert!(Path::new(b).exists());

    // Skipping `mv --backup=simple a b`

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/mv/i-1.pl
#[test]
fn test_mv_i_1() {
    let test_dir = &format!("{}/test_mv_i_1", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(a).unwrap();
    fs::File::create(b).unwrap();

    mv_test_with_stdin(
        &["-i", a, b],
        "n\n", // prompt response
        "",
        &format!("mv: overwrite '{b}'? "),
        0,
    );
    // `a` wasn't moved
    assert!(Path::new(a).exists());

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/mv/i-2.sh
// This only includes the tests for `mv` and not `cp`. This also does not
// consider the writability of the destination when generating the prompt.
#[test]
fn test_mv_i_2() {
    let test_dir = &format!("{}/test_mv_i_2", env!("CARGO_TARGET_TMPDIR"));
    fs::create_dir(test_dir).unwrap();

    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");
    let c = &format!("{test_dir}/c");
    let d = &format!("{test_dir}/d");

    for (var, byte_string) in [(a, b"a\n"), (b, b"b\n"), (c, b"c\n"), (d, b"d\n")] {
        let mut file = fs::File::create(var).unwrap();
        file.write_all(byte_string).unwrap();

        // flush before `fs::set_permissions` below
        std::mem::drop(file);

        if var == b || var == d {
            // chmod 0
            let perm = fs::Permissions::from_mode(0);
            fs::set_permissions(var, perm).unwrap();
        }
    }

    mv_test_with_stdin(&["-if", a, b], "", "", "", 0);
    assert!(!Path::new(a).exists());

    // coreutils mv changes the prompt depending on whether the destination is
    // writeable. For this case it's:
    // "mv: replace 'd', overriding mode 0000 (---------)?"
    mv_test_with_stdin(
        &["-fi", c, d],
        "y\n",
        "",
        &format!("mv: overwrite '{d}'? "),
        0,
    );
    assert!(!Path::new(c).exists());

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/mv/i-4.sh
#[test]
fn test_mv_i_4() {
    let test_dir = &format!("{}/test_mv_i_4", env!("CARGO_TARGET_TMPDIR"));
    fs::create_dir(test_dir).unwrap();

    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");

    for (var, byte_string) in [(a, b"a\n"), (b, b"b\n")] {
        let mut file = fs::File::create(var).unwrap();
        file.write_all(byte_string).unwrap();
    }

    mv_test_with_stdin(
        &["-i", a, b],
        "y\n",
        "",
        &format!("mv: overwrite '{b}'? "),
        0,
    );

    // b should now contain "a"
    {
        let mut buf = Vec::new();
        let mut file = fs::File::open(b).unwrap();
        file.read_to_end(&mut buf).unwrap();
        let s = String::from_utf8(buf).unwrap();
        assert_eq!(s, "a\n");
    }

    fs::remove_file(b).unwrap();

    // echo a > a
    let mut file = fs::File::create(a).unwrap();
    file.write_all(b"a\n").unwrap();

    fs::hard_link(a, b).unwrap();

    mv_test_with_stdin(
        &["-i", a, b],
        "y\n",
        "",
        &format!(
            "mv: overwrite '{b}'? \
             mv: '{a}' and '{b}' are the same file\n"
        ),
        1,
    );
    // a was not moved
    assert!(Path::new(a).exists());
    // b should not be deleted
    assert!(Path::new(b).exists());

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/mv/i-5.sh
#[test]
fn test_mv_i_5() {
    let test_dir = &format!("{}/test_mv_i_5", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(a).unwrap();
    fs::File::create(b).unwrap();

    mv_test_with_stdin(
        &["-i", a, b],
        "y\n",
        "",
        // Prompt before failing.
        &format!(
            "mv: overwrite '{b}'? \
             mv: cannot overwrite non-directory '{b}' with directory '{a}'\n"
        ),
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/mv/into-self.sh
#[test]
fn test_mv_into_self() {
    let test_dir = &format!("{}/test_mv_into_self", env!("CARGO_TARGET_TMPDIR"));
    let dir = &format!("{test_dir}/toself-dir");
    let dir_a = &format!("{test_dir}/toself-dir/a");
    let dir_a_b = &format!("{test_dir}/toself-dir/a/b");
    let file = &format!("{test_dir}/toself-file");

    let _ = fs::remove_dir_all(dir);
    let _ = fs::remove_file(file);

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(dir).unwrap();
    fs::create_dir(dir_a).unwrap();
    fs::create_dir(dir_a_b).unwrap();
    fs::File::create(file).unwrap();

    mv_test(
        &[dir, file, dir],
        "",
        &format!("mv: cannot move '{dir}' to a subdirectory of itself, '{dir}/toself-dir'\n"),
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/mv/into-self-3.sh
#[test]
fn test_mv_into_self_3() {
    let test_dir = &format!("{}/test_mv_into_self_3", env!("CARGO_TARGET_TMPDIR"));
    let dir1 = &format!("{test_dir}/is3-dir1");
    let dir2 = &format!("{test_dir}/is3-dir2");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(dir1).unwrap();
    fs::create_dir(dir2).unwrap();

    mv_test(
        &[dir1, dir2, dir2],
        "",
        &format!("mv: cannot move '{dir2}' to a subdirectory of itself, '{dir2}/is3-dir2'\n"),
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/mv/into-self-4.sh
#[test]
fn test_mv_into_self_4() {
    let test_dir = &format!("{}/test_mv_into_self_4", env!("CARGO_TARGET_TMPDIR"));
    let file = &format!("{test_dir}/file");
    let s = &format!("{test_dir}/s");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(file).unwrap();
    unix::fs::symlink(file, s).unwrap();

    mv_test(
        &[s, s],
        "",
        &format!("mv: '{s}' and '{s}' are the same file\n"),
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/mv/perm-1.sh
#[test]
fn test_mv_perm_1() {
    let test_dir = &format!("{}/test_mv_perm_1", env!("CARGO_TARGET_TMPDIR"));
    let no_write = &format!("{test_dir}/no-write");
    let no_write_dir = &format!("{test_dir}/no-write/dir");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(no_write).unwrap();
    fs::create_dir(no_write_dir).unwrap();

    // chmod ug-w no-write
    let ug_write = (libc::S_IWUSR | libc::S_IWGRP) as u32;
    let mode = Path::new(no_write).metadata().unwrap().mode();
    fs::set_permissions(no_write, Permissions::from_mode(mode & !ug_write as u32)).unwrap();

    mv_test(
        &[no_write_dir, test_dir],
        "",
        &format!("mv: cannot move '{no_write_dir}' to '{test_dir}/dir': Permission denied\n"),
        1,
    );

    // Allow write permission again for cleanup
    fs::set_permissions(no_write, Permissions::from_mode(mode | ug_write)).unwrap();

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/mv/symlink-onto-hardlink-to-self.sh
// Does not include the test for cp and the --backup option.
#[test]
fn test_mv_symlink_onto_hardlink_to_self() {
    let test_dir = &format!(
        "{}/test_mv_symlink_onto_hardlink_to_self",
        env!("CARGO_TARGET_TMPDIR")
    );
    let f = &format!("{test_dir}/f");
    let s1 = &format!("{test_dir}/s1");
    let s2 = &format!("{test_dir}/s2");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();
    unix::fs::symlink(f, s2).unwrap();
    fs::hard_link(s2, s1).unwrap();

    mv_test(
        &[s1, s2],
        "",
        &format!("mv: '{s1}' and '{s2}' are the same file\n"),
        1,
    );
    assert!(Path::new(s1).exists());

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/mv/symlink-onto-hardlink.sh
#[test]
fn test_mv_symlink_onto_hardlink() {
    let test_dir = &format!(
        "{}/test_mv_symlink_onto_hardlink",
        env!("CARGO_TARGET_TMPDIR")
    );
    let f = &format!("{test_dir}/f");
    let h = &format!("{test_dir}/h");
    let s = &format!("{test_dir}/s");
    let l = &format!("{test_dir}/l");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();
    fs::hard_link(f, h).unwrap();
    unix::fs::symlink(f, s).unwrap();

    mv_test(
        &[s, f],
        "",
        &format!("mv: '{s}' and '{f}' are the same file\n"),
        1,
    );
    mv_test(&[s, l], "", "", 0);

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/mv/trailing-slash.sh
// -T and -u are not supported.
#[test]
fn test_mv_trailing_slash() {
    let test_dir = &format!("{}/test_mv_trailing_slash", env!("CARGO_TARGET_TMPDIR"));
    let foo = &format!("{test_dir}/foo");
    let foo_slash = &format!("{test_dir}/foo/");
    let bar = &format!("{test_dir}/bar");
    let d = &format!("{test_dir}/d");
    let e_slash = &format!("{test_dir}/e/");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(foo).unwrap();

    mv_test(&[foo_slash, bar], "", "", 0);
    // `foo` itself should be moved even if `foo/` was specified
    assert!(!Path::new(foo).exists());
    assert!(Path::new(bar).exists());
    fs::remove_dir(bar).unwrap();

    fs::create_dir(d).unwrap();

    mv_test(&[d, e_slash], "", "", 0);
    // `d` was moved to `e/`
    assert!(!Path::new(d).exists());
    assert!(Path::new(e_slash).exists());

    fs::remove_dir_all(test_dir).unwrap();
}

// The next tests require the `/dev/shm` or a directory in another filesystem
// supplied through the `OTHER_PARTITION_TMPDIR` environment variable.

// Port of coreutils/tests/mv/hard-link-1.sh
#[test]
#[cfg_attr(not(target_os = "linux"), ignore)]
fn test_mv_hard_link_1() {
    let test_name = "test_mv_hard_link_1";
    let test_dir = &format!("{}/{test_name}", env!("CARGO_TARGET_TMPDIR"));
    let dir = &format!("{test_dir}/hlink");
    let dir_a = &format!("{test_dir}/hlink/a");
    let dir_b = &format!("{test_dir}/hlink/b");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(dir).unwrap();
    fs::File::create(dir_a).unwrap();

    fs::hard_link(dir_a, dir_b).unwrap();

    let other_dir = &format!(
        "{}/{test_name}",
        option_env!("OTHER_PARTITION_TMPDIR").unwrap_or("/dev/shm")
    );
    let other_dir_dir_a = &format!("{other_dir}/hlink/a");
    let other_dir_dir_b = &format!("{other_dir}/hlink/b");

    fs::create_dir(other_dir).unwrap();

    mv_test(&[dir, other_dir], "", "", 0);

    let md_a = Path::new(other_dir_dir_a).metadata().unwrap();
    let md_b = Path::new(other_dir_dir_b).metadata().unwrap();

    assert_eq!(md_a.ino(), md_b.ino());

    fs::remove_dir_all(test_dir).unwrap();
    fs::remove_dir_all(other_dir).unwrap();
}

// Port of coreutils/tests/mv/into-self-2.sh
#[test]
#[cfg_attr(not(target_os = "linux"), ignore)]
fn test_mv_into_self_2() {
    let test_name = "test_mv_into_self_2";
    let test_dir = &format!("{}/{test_name}", env!("CARGO_TARGET_TMPDIR"));
    let symlink = &format!("{test_dir}/symlink");
    fs::create_dir(test_dir).unwrap();

    let other_dir = &format!(
        "{}/{test_name}",
        option_env!("OTHER_PARTITION_TMPDIR").unwrap_or("/dev/shm")
    );
    let file = &format!("{other_dir}/file");
    fs::create_dir(other_dir).unwrap();

    let mut f = fs::File::create(file).unwrap();
    f.write_all(b"whatever\n").unwrap();

    unix::fs::symlink(file, symlink).unwrap();

    mv_test(
        &[symlink, file],
        "",
        &format!("mv: '{symlink}' and '{file}' are the same file\n"),
        1,
    );

    mv_test(&[file, symlink], "", "", 0);

    fs::remove_dir_all(test_dir).unwrap();
    fs::remove_dir_all(other_dir).unwrap();
}

// Port of coreutils/tests/mv/leak-fd.sh
#[test]
#[cfg_attr(not(target_os = "linux"), ignore)]
fn test_mv_leak_fd() {
    let test_name = "test_mv_leak_fd";
    let test_dir = &format!("{}/{test_name}", env!("CARGO_TARGET_TMPDIR"));
    fs::create_dir(test_dir).unwrap();

    let other_dir = &format!(
        "{}/{test_name}",
        option_env!("OTHER_PARTITION_TMPDIR").unwrap_or("/dev/shm")
    );
    fs::create_dir(other_dir).unwrap();

    let letters = [
        "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f", "g", "h",
        "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
        "_A", "_B", "_C", "_D", "_E", "_F", "_G", "_H", "_I", "_J", "_K", "_L", "_M", "_N", "_O",
        "_P", "_Q", "_R", "_S", "_T", "_U", "_V", "_W", "_X", "_Y", "_Z",
    ];

    let mut dirs = Vec::new();
    for i in letters {
        dirs.push(format!("{test_dir}/{}", i.to_string()));
        for j in letters {
            dirs.push(format!("{test_dir}/{i}{j}"));
        }
    }

    for dir in &dirs {
        fs::create_dir(dir).unwrap();

        let file = &format!("{dir}/f");
        fs::File::create(file).unwrap();
    }

    let last_file = &format!("{}/f", dirs.last().unwrap());
    assert!(Path::new(last_file).exists());

    let mut args: Vec<_> = dirs.iter().map(|s| s.as_str()).collect();
    args.push(other_dir.as_str());

    mv_test(&args, "", "", 0);

    // Everything was moved
    assert!(!Path::new(last_file).exists());
    for dir in &dirs {
        assert!(!Path::new(dir).exists());
    }

    fs::remove_dir_all(test_dir).unwrap();
    fs::remove_dir_all(other_dir).unwrap();
}

// Port of coreutils/tests/mv/part-fail.sh
#[test]
#[cfg_attr(not(target_os = "linux"), ignore)]
fn test_mv_part_fail() {
    let test_name = "test_mv_part_fail";
    let test_dir = &format!("{}/{test_name}", env!("CARGO_TARGET_TMPDIR"));
    let k = &format!("{test_dir}/k");
    fs::create_dir(test_dir).unwrap();
    fs::File::create(k).unwrap();

    let other_dir = &format!(
        "{}/{test_name}",
        option_env!("OTHER_PARTITION_TMPDIR").unwrap_or("/dev/shm")
    );
    let other_dir_k = &format!("{other_dir}/k");
    fs::create_dir(other_dir).unwrap();
    fs::File::create(other_dir_k).unwrap();

    // chmod u-w
    let u_write = libc::S_IWUSR as u32;
    let mode = Path::new(other_dir).metadata().unwrap().mode();
    fs::set_permissions(other_dir, Permissions::from_mode(mode & !u_write)).unwrap();

    mv_test(
        &["-f", k, other_dir],
        "",
        &format!(
            "mv: inter-device move failed: '{k}' to '{other_dir_k}': \
            unable to remove target: Permission denied\n"
        ),
        1,
    );

    // Reset write permission to allow deletion
    fs::set_permissions(other_dir, Permissions::from_mode(mode | u_write)).unwrap();
    fs::remove_dir_all(test_dir).unwrap();
    fs::remove_dir_all(other_dir).unwrap();
}

// Port of coreutils/tests/mv/part-hardlink.sh
#[test]
#[cfg_attr(not(target_os = "linux"), ignore)]
fn test_mv_part_hardlink() {
    let test_name = "test_mv_part_hardlink";
    let test_dir = &format!("{}/{test_name}", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");
    let f = &format!("{test_dir}/f");
    let g = &format!("{test_dir}/g");
    let a_1 = &format!("{test_dir}/a/1");
    let b_1 = &format!("{test_dir}/b/1");
    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();
    fs::hard_link(f, g).unwrap();
    fs::create_dir(a).unwrap();
    fs::create_dir(b).unwrap();
    fs::File::create(a_1).unwrap();
    fs::hard_link(a_1, b_1).unwrap();

    let other_dir = &format!(
        "{}/{test_name}",
        option_env!("OTHER_PARTITION_TMPDIR").unwrap_or("/dev/shm")
    );
    let other_f = &format!("{other_dir}/f");
    let other_g = &format!("{other_dir}/g");
    let other_a_1 = &format!("{other_dir}/a/1");
    let other_b_1 = &format!("{other_dir}/b/1");
    fs::create_dir(other_dir).unwrap();

    mv_test(&[f, g, other_dir], "", "", 0);
    mv_test(&[a, b, other_dir], "", "", 0);

    assert_eq!(
        Path::new(other_f).metadata().unwrap().ino(),
        Path::new(other_g).metadata().unwrap().ino(),
    );
    assert_eq!(
        Path::new(other_a_1).metadata().unwrap().ino(),
        Path::new(other_b_1).metadata().unwrap().ino(),
    );

    fs::remove_dir_all(test_dir).unwrap();
    fs::remove_dir_all(other_dir).unwrap();
}

// Port of coreutils/tests/mv/part-rename.sh
#[test]
#[cfg_attr(not(target_os = "linux"), ignore)]
fn test_mv_part_rename() {
    let test_name = "test_mv_part_rename";
    let test_dir = &format!("{}/{test_name}", env!("CARGO_TARGET_TMPDIR"));
    fs::create_dir(test_dir).unwrap();

    let other_dir = &format!(
        "{}/{test_name}",
        option_env!("OTHER_PARTITION_TMPDIR").unwrap_or("/dev/shm")
    );
    fs::create_dir(other_dir).unwrap();

    let foo_slash = &format!("{test_dir}/foo/");
    let other_bar = &format!("{other_dir}/bar");
    fs::create_dir(foo_slash).unwrap();
    mv_test(&[foo_slash, other_bar], "", "", 0);

    // Don't overwrite other_dir/bar from the previous step
    let bar = &format!("{test_dir}/bar");
    fs::File::create(bar).unwrap();
    mv_test(
        &[bar, other_dir],
        "",
        &format!("mv: cannot overwrite directory '{other_bar}' with non-directory '{bar}'\n"),
        1,
    );

    let bar2 = &format!("{test_dir}/bar2");
    fs::create_dir(bar2).unwrap();
    let other_bar2 = &format!("{other_dir}/bar2");
    fs::File::create(other_bar2).unwrap();
    mv_test(
        &[bar2, other_dir],
        "",
        &format!("mv: cannot overwrite non-directory '{other_bar2}' with directory '{bar2}'\n"),
        1,
    );

    let bar3 = &format!("{test_dir}/bar3");
    fs::create_dir(bar3).unwrap();
    let bar3_file = &format!("{test_dir}/bar3/file");
    fs::File::create(bar3_file).unwrap();
    let other_bar3 = &format!("{other_dir}/bar3");
    fs::create_dir(other_bar3).unwrap();
    mv_test(&[bar3, other_dir], "", "", 0);
    let other_bar3_file = &format!("{other_dir}/bar3/file");
    assert!(Path::new(other_bar3_file).exists());

    fs::create_dir(bar3).unwrap();
    let bar3_file2 = &format!("{test_dir}/bar3/file2");
    fs::File::create(bar3_file2).unwrap();
    mv_test(
        &[bar3, other_dir],
        "",
        &format!(
            "mv: inter-device move failed: '{bar3}' to '{other_dir}/bar3': \
            unable to remove target: Directory not empty\n"
        ),
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
    fs::remove_dir_all(other_dir).unwrap();
}

// Partial port of coreutils/tests/mv/part-symlink.sh
// Not including --rem, -d, -b and the test for `cp`
#[test]
#[cfg_attr(not(target_os = "linux"), ignore)]
fn test_mv_part_symlink() {
    fn cd_and_mv_test(
        test_dir: &str,
        args: &[&str],
        expected_error: &str,
        expected_exit_code: i32,
    ) {
        let program = env!("CARGO_BIN_EXE_mv");
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

    let test_name = "test_mv_part_symlink";
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
        // mv loc_reg rem_sl
        (
            format!("{dir}/{loc_reg}"),
            rem_sl.to_string(),
            [loc_reg, rem_sl.as_str()],
            format!(""),
            0,
        ),
        // mv rem_sl loc_reg
        (
            format!("{dir}/{loc_reg}"),
            rem_sl.to_string(),
            [rem_sl.as_str(), loc_reg],
            format!("mv: '{rem_sl}' and '{loc_reg}' are the same file\n"),
            1,
        ),
        // mv loc_sl rem_reg
        (
            rem_reg.to_string(),
            format!("{dir}/{loc_sl}"),
            [loc_sl, rem_reg.as_str()],
            format!("mv: '{loc_sl}' and '{rem_reg}' are the same file\n"),
            1,
        ),
        // mv rem_reg loc_sl
        (
            rem_reg.to_string(),
            format!("{dir}/{loc_sl}"),
            [rem_reg.as_str(), loc_sl],
            format!(""),
            0,
        ),
    ] {
        fs::create_dir(test_dir).unwrap();
        fs::create_dir(other_dir).unwrap();
        fs::create_dir(dir).unwrap();

        let mut f = fs::File::create(&reg_abs).unwrap();
        f.write_all(contents.as_bytes()).unwrap();

        unix::fs::symlink(&reg_abs, &slink).unwrap();

        cd_and_mv_test(dir, &args, &err_str, err_code);

        fs::remove_dir_all(test_dir).unwrap();
        fs::remove_dir_all(other_dir).unwrap();
    }
}

// Port of coreutils/tests/mv/partition-perm.sh
#[test]
#[cfg_attr(not(target_os = "linux"), ignore)]
fn test_mv_partition_perm() {
    let test_name = "test_mv_partition_perm";
    let test_dir = &format!("{}/{test_name}", env!("CARGO_TARGET_TMPDIR"));
    let file = &format!("{test_dir}/file");
    fs::create_dir(test_dir).unwrap();
    fs::File::create(file).unwrap();

    let other_dir = &format!(
        "{}/{test_name}",
        option_env!("OTHER_PARTITION_TMPDIR").unwrap_or("/dev/shm")
    );
    let other_file = &format!("{other_dir}/file");
    fs::create_dir(other_dir).unwrap();

    // chmod a=rwx
    let write_all = (libc::S_IRWXU | libc::S_IRWXG | libc::S_IRWXO) as u32;
    fs::set_permissions(file, Permissions::from_mode(write_all)).unwrap();
    let mode = Path::new(file).metadata().unwrap().mode();

    mv_test(&[file, other_dir], "", "", 0);
    assert!(!Path::new(file).exists());
    assert!(Path::new(other_file).exists());

    let other_mode = Path::new(other_file).metadata().unwrap().mode();
    assert_eq!(mode, other_mode);

    fs::remove_dir_all(test_dir).unwrap();
    fs::remove_dir_all(other_dir).unwrap();
}

// Port of coreutils/tests/mv/mv-special.sh
#[test]
#[cfg_attr(not(target_os = "linux"), ignore)]
fn test_mv_special() {
    let test_name = "test_mv_special";
    let test_dir = &format!("{}/{test_name}", env!("CARGO_TARGET_TMPDIR"));
    let null = &format!("{test_dir}/mv-null");
    let dir = &format!("{test_dir}/mv-dir");
    let dir_a_b_c = &format!("{dir}/a/b/c");
    let dir_d_e_f = &format!("{dir}/d/e/f");
    let dir_a_b_c_file1 = &format!("{dir}/a/b/c/file1");
    let dir_d_e_f_file2 = &format!("{dir}/d/e/f/file2");

    fs::create_dir(test_dir).unwrap();

    unsafe {
        let null_cstr = CString::new(null.to_string()).unwrap();

        // mknod mv-null p
        let ret = libc::mknod(null_cstr.as_ptr(), libc::S_IFIFO | 0o644, 0);
        if ret != 0 {
            panic!("{}", io::Error::last_os_error());
        }

        assert!(Path::new(null).metadata().unwrap().file_type().is_fifo());
    }

    fs::create_dir_all(dir_a_b_c).unwrap();
    fs::create_dir_all(dir_d_e_f).unwrap();
    fs::File::create(dir_a_b_c_file1).unwrap();
    fs::File::create(dir_d_e_f_file2).unwrap();

    let other_dir = &format!(
        "{}/{test_name}",
        option_env!("OTHER_PARTITION_TMPDIR").unwrap_or("/dev/shm")
    );
    let other_dir_null = &format!("{other_dir}/mv-null");
    let other_dir_dir = &format!("{other_dir}/mv-dir");
    let other_dir_dir_a_b_c = &format!("{other_dir_dir}/a/b/c");
    let other_dir_dir_d_e_f = &format!("{other_dir_dir}/d/e/f");
    let other_dir_dir_a_b_c_file1 = &format!("{other_dir_dir}/a/b/c/file1");
    let other_dir_dir_d_e_f_file2 = &format!("{other_dir_dir}/d/e/f/file2");
    fs::create_dir(other_dir).unwrap();

    mv_test(&[null, dir, other_dir], "", "", 0);

    for s in [
        null,
        dir,
        dir_a_b_c,
        dir_d_e_f,
        dir_a_b_c_file1,
        dir_d_e_f_file2,
    ] {
        assert!(!Path::new(s).exists());
    }

    for s in [
        other_dir_null,
        other_dir_dir,
        other_dir_dir_a_b_c,
        other_dir_dir_d_e_f,
        other_dir_dir_a_b_c_file1,
        other_dir_dir_d_e_f_file2,
    ] {
        assert!(Path::new(s).exists());
    }

    fs::remove_dir_all(test_dir).unwrap();
    fs::remove_dir_all(other_dir).unwrap();
}

// Port of coreutils/tests/mv/to-symlink.sh
#[test]
#[cfg_attr(not(target_os = "linux"), ignore)]
fn test_mv_to_symlink() {
    let test_name = "test_mv_to_symlink";
    let test_dir = &format!("{}/{test_name}", env!("CARGO_TARGET_TMPDIR"));
    let file = &format!("{test_dir}/to-sym");
    fs::create_dir(test_dir).unwrap();

    let other_dir = &format!(
        "{}/{test_name}",
        option_env!("OTHER_PARTITION_TMPDIR").unwrap_or("/dev/shm")
    );
    let rem_file = &format!("{other_dir}/file");
    let rem_symlink = &format!("{other_dir}/symlink");
    fs::create_dir(other_dir).unwrap();

    let mut f = fs::File::create(file).unwrap();
    f.write_all(b"local\n").unwrap();

    let mut f = fs::File::create(rem_file).unwrap();
    f.write_all(b"local\n").unwrap();

    unix::fs::symlink(rem_file, rem_symlink).unwrap();

    mv_test(&[file, rem_symlink], "", "", 0);

    fs::remove_dir_all(test_dir).unwrap();
    fs::remove_dir_all(other_dir).unwrap();
}

// The following tests require potentially resource intensive setup.
// Run with `cargo test --features posixutils_test_all test_mv_`.

// Port of coreutils/tests/mv/hardlink-case.sh
//
// This test requires the case-insensitive filesystem to be manually supplied
// through a directory in the CASE_INSENSITIVE_TMPDIR environment variable.
//
// This passes on a NTFS drive when using WSL2.
#[test]
#[cfg_attr(not(all(target_os = "linux", feature = "posixutils_test_all")), ignore)]
fn test_mv_hardlink_case() {
    let tmpdir = option_env!("CASE_INSENSITIVE_TMPDIR").expect(
        "`test_mv_hardlink_case` requires the \
        `CASE_INSENSITIVE_TMPDIR` environment variable",
    );
    let mnt = &format!("{tmpdir}/test_mv_hardlink_case");
    fs::create_dir(mnt).unwrap();

    let foo = &format!("{mnt}/foo");
    let foo_caps = &format!("{mnt}/FOO");
    let whatever = &format!("{mnt}/whatever");

    fs::File::create(foo).unwrap();
    fs::hard_link(foo, whatever).unwrap();

    // foo and FOO are the same if `tmpdir` is in a case-insensitive filesystem
    mv_test(
        &[foo, foo_caps],
        "",
        &format!("mv: '{foo}' and '{foo_caps}' are the same file\n"),
        1,
    );

    assert!(Path::new(foo).exists());

    fs::remove_dir_all(mnt).unwrap();
}

// Port of coreutils/tests/mv/i-link-no.sh
//
// Requires the use of the `script` command to fake a terminal as input:
//
// https://www.man7.org/linux/man-pages/man1/script.1.html
#[test]
#[cfg_attr(not(all(target_os = "linux", feature = "posixutils_test_all")), ignore)]
fn test_mv_i_link_no() {
    // Tricks `io::stdin().is_terminal()` to allow showing the overwrite
    // prompt without -i
    fn mv_test_fake_tty(
        args: &[&str],
        stdin_data: &str,
        script_output: &str,
        expected_exit_code: i32,
    ) {
        // This is a special compile-time variable like CARGO_TARGET_TMPDIR
        let mv = env!("CARGO_BIN_EXE_mv");

        let mv_args = args.join(" ");
        let mv_cmd = format!("{mv} {mv_args}");

        let script_args = vec!["-q", "-c", &mv_cmd, script_output];

        let mut command = Command::new("script");
        let mut child = command
            .args(&script_args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();

        let stdin = child.stdin.as_mut().unwrap();
        stdin.write_all(stdin_data.as_bytes()).unwrap();

        assert_eq!(child.wait().unwrap().code(), Some(expected_exit_code));
    }

    let test_dir = &format!("{}/test_mv_i_link_no", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");
    let a_foo = &format!("{test_dir}/a/foo");
    let a_bar = &format!("{test_dir}/a/bar");
    let b_foo = &format!("{test_dir}/b/foo");
    let b_bar = &format!("{test_dir}/b/bar");
    let b_fubar = &format!("{test_dir}/b/FUBAR");
    let script_output = &format!("{test_dir}/output");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(a).unwrap();
    fs::create_dir(b).unwrap();

    let mut file = fs::File::create(a_foo).unwrap();
    file.write_all(b"foo\n").unwrap();

    fs::hard_link(a_foo, a_bar).unwrap();

    let mut file = fs::File::create(b_fubar).unwrap();
    file.write_all(b"FUBAR\n").unwrap();

    fs::hard_link(b_fubar, b_bar).unwrap();

    // chmod a-w b/bar
    let write_all = (libc::S_IWUSR | libc::S_IWGRP | libc::S_IWOTH) as u32;
    let mode = Path::new(b_bar).metadata().unwrap().mode();
    // Zeros the write permission bits
    fs::set_permissions(b_bar, Permissions::from_mode(mode & !write_all)).unwrap();

    // Not using -i so have to use a fake tty to enable the overwrite prompt
    mv_test_fake_tty(&[a_bar, a_foo, b], "n\n", script_output, 0);

    {
        let mut buf = Vec::new();
        let mut file = fs::File::open(b_foo).unwrap();
        file.read_to_end(&mut buf).unwrap();
        let s = String::from_utf8(buf).unwrap();
        assert_eq!(s, "foo\n");
    }

    // a/foo was moved to b/foo
    assert!(!Path::new(a_foo).exists());
    // a/bar was not because `n` was passed to the prompt
    assert!(Path::new(a_bar).exists());

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/mv/sticky-to-xpart.sh
//
// This test needs root access and a non-root username passed in the
// `NON_ROOT_USERNAME` env var. *MUST BE RUN ALONE* because running as root will
// allow overwriting/deleting files which is otherwise protected by permissions
// thus breaking tests that expect the command to fail.
#[test]
#[cfg_attr(
    not(all(
        target_os = "linux",
        feature = "posixutils_test_all",
        feature = "requires_root"
    )),
    ignore
)]
fn test_mv_sticky_to_xpart() {
    let test_name = "test_mv_sticky_to_xpart";
    let test_dir = &format!("{}/{test_name}", env!("CARGO_TARGET_TMPDIR"));
    let t = &format!("{test_dir}/t");
    let t_root_owned = &format!("{test_dir}/t/root-owned");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(t).unwrap();

    unsafe {
        let t_cstr = CString::new(t.as_bytes()).unwrap();
        // chmod a=rwx,o+t t
        let ret = libc::chmod(t_cstr.as_ptr(), 0o777 | libc::S_ISVTX);
        if ret != 0 {
            panic!("{}", io::Error::last_os_error());
        }
    }

    let file_t_root_owned = fs::File::create(t_root_owned).unwrap();
    let md = file_t_root_owned.metadata().unwrap();
    // chmod a+r t/root-owned
    fs::set_permissions(t_root_owned, Permissions::from_mode(md.mode() | 0o444)).unwrap();

    let other_dir = &format!(
        "{}/{test_name}",
        option_env!("OTHER_PARTITION_TMPDIR").unwrap_or("/dev/shm")
    );
    fs::create_dir(other_dir).unwrap();

    let non_root = option_env!("NON_ROOT_USERNAME").expect(
        "`test_mv_sticky_to_xpart` requires the \
        `NON_ROOT_USERNAME` environment variable",
    );

    unsafe {
        let non_root_cstr = CString::new(non_root).unwrap();
        let passwd = libc::getpwnam(non_root_cstr.as_ptr());
        if passwd.is_null() {
            panic!("{}", io::Error::last_os_error());
        }
        let uid = (*passwd).pw_uid;

        // chown "$NON_ROOT_USERNAME" "$other_partition_tmpdir"
        let md = fs::metadata(other_dir).unwrap();
        let other_dir_cstr = CString::new(other_dir.as_bytes()).unwrap();
        let ret = libc::chown(other_dir_cstr.as_ptr(), uid, md.gid());
        if ret != 0 {
            panic!("{}", io::Error::last_os_error());
        }
    }

    let md = fs::metadata(test_dir).unwrap();
    // chmod go+x .
    fs::set_permissions(test_dir, Permissions::from_mode(md.mode() | 0o011)).unwrap();

    let child = Command::new("chroot")
        .args([
            "--skip-chdir",
            &format!("--user={non_root}"),
            "/",
            env!("CARGO_BIN_EXE_mv"),
            t_root_owned,
            other_dir,
        ])
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    let output = child.wait_with_output().unwrap();

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(
        stderr,
        format!("mv: cannot remove '{t_root_owned}': Operation not permitted\n")
    );

    assert_eq!(output.status.code(), Some(1));

    fs::remove_dir_all(test_dir).unwrap();
    fs::remove_dir_all(other_dir).unwrap();
}
