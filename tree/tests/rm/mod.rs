//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::ffi::CString;
use std::fs;
use std::io::{self, Write};
use std::os::unix::fs::{DirBuilderExt, MetadataExt, PermissionsExt};
use std::os::unix::{self};
use std::path::Path;
use std::process::{Command, Stdio};

use plib::testing::{run_test, run_test_with_checker, TestPlan};

fn rm_test(args: &[&str], expected_output: &str, expected_error: &str, expected_exit_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("rm"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

fn rm_test_with_stdin(
    args: &[&str],
    stdin_data: &str,
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("rm"),
        args: str_args,
        stdin_data: String::from(stdin_data),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

fn rm_test_fake_tty(args: &[&str], stdin_data: &str, expected_exit_code: i32) {
    let bin = env!("CARGO_BIN_EXE_rm");

    #[cfg(target_os = "linux")]
    let cmd = {
        let args = args.join(" ");
        format!("{bin} {args}")
    };
    #[cfg(target_os = "linux")]
    let script_args = vec!["-q", "-c", &cmd, "/dev/null"];

    #[cfg(target_os = "macos")]
    let script_args = {
        let mut v = vec!["-q", "/dev/null", bin];
        v.extend(args.iter());
        v
    };

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

fn rm_test_with_checker<F: FnMut(&TestPlan, &std::process::Output)>(
    args: &[&str],
    stdin_data: &str,
    checker: F,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    let test_plan = TestPlan {
        cmd: String::from("rm"),
        args: str_args,
        stdin_data: String::from(stdin_data),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };

    run_test_with_checker(test_plan, checker);
}

// Port of coreutils/tests/rm/cycle.sh
#[test]
fn test_rm_cycle() {
    let test_dir = &format!("{}/test_rm_cycle", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let a_b = &format!("{test_dir}/a/b");
    let a_b_file = &format!("{test_dir}/a/b/file");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir_all(a_b).unwrap();
    fs::File::create(a_b_file).unwrap();

    let ug_w = (libc::S_IWUSR | libc::S_IWGRP) as u32;
    let mode = fs::metadata(a_b).unwrap().mode();
    fs::set_permissions(a_b, fs::Permissions::from_mode(mode & !ug_w)).unwrap();

    rm_test(
        &["-rf", a, a],
        "",
        &format!(
            "rm: cannot remove '{a_b_file}': Permission denied\n\
             rm: cannot remove '{a_b_file}': Permission denied\n",
        ),
        1,
    );

    fs::set_permissions(a_b, fs::Permissions::from_mode(0o777)).unwrap();
    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/dangling-symlink.sh
#[test]
#[cfg_attr(not(target_os = "linux"), ignore)]
fn test_rm_dangling_symlink() {
    let test_dir = &format!("{}/test_rm_dangling_symlink", env!("CARGO_TARGET_TMPDIR"));
    let no_file = &format!("{test_dir}/no-file");
    let dangle = &format!("{test_dir}/dangle");
    let symlink = &format!("{test_dir}/symlinks");

    fs::create_dir(test_dir).unwrap();
    unix::fs::symlink(no_file, dangle).unwrap();
    unix::fs::symlink("/", symlink).unwrap();

    rm_test_fake_tty(&[dangle, symlink], "", 0);

    assert!(!Path::new(dangle).exists());
    assert!(!Path::new(symlink).exists());

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/deep-2.sh
#[test]
fn test_rm_deep_2() {
    let test_dir = &format!("{}/test_rm_deep_2", env!("CARGO_TARGET_TMPDIR"));
    let x = &format!("{test_dir}/x");
    let x200: &str = &["x"; 200].join("");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(&x).unwrap();

    // The final filename is too long to use for `fs::create_dir_all`. It also
    // cannot be created via `std::env::set_current_dir` because changing the
    // current directory of the process would also affect other tests.
    // The only option would be to do it one directory at a time in a child
    // process.

    let mut command = String::new();
    let single = format!("mkdir {x200} && chmod 700 {x200} && cd {x200}");

    let levels = 52;
    for i in 0..levels {
        command.push_str(&single);

        // All except the last
        if i != levels - 1 {
            command.push_str(" && ");
        }
    }
    let mut child = Command::new("/bin/bash")
        .arg("-c")
        .arg(&command)
        .current_dir(x)
        .spawn()
        .unwrap();
    let status = child.wait().unwrap();
    assert!(status.success());

    rm_test_fake_tty(&["-r", x], "", 0);

    assert!(!Path::new(x).exists());

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/dir-no-w.sh
#[test]
fn test_rm_dir_no_w() {
    let test_dir = &format!("{}/test_rm_dir_no_w", env!("CARGO_TARGET_TMPDIR"));
    let unwritable_dir = &format!("{test_dir}/unwritable-dir");

    fs::create_dir(test_dir).unwrap();

    fs::DirBuilder::new()
        .mode(0o500)
        .create(unwritable_dir)
        .unwrap();

    // The error message is always the same with or without root unlike what the
    // original code implies
    rm_test(
        &[unwritable_dir],
        "",
        &format!("rm: cannot remove '{unwritable_dir}': Is a directory\n"),
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/dir-nonrecur.sh
#[test]
fn test_rm_dir_nonrecur() {
    let test_dir = &format!("{}/test_rm_dir_nonrecur", env!("CARGO_TARGET_TMPDIR"));
    let d = &format!("{test_dir}/d");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(d).unwrap();

    rm_test(
        &[d],
        "",
        &format!("rm: cannot remove '{d}': Is a directory\n"),
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/dot-rel.sh
#[test]
fn test_rm_dot_rel() {
    let test_dir = &format!("{}/test_rm_dot_rel", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");
    let a_f = &format!("{test_dir}/a/f");
    let b_f = &format!("{test_dir}/b/f");

    fs::create_dir(test_dir).unwrap();

    fs::create_dir(a).unwrap();
    fs::create_dir(b).unwrap();

    fs::File::create(a_f).unwrap();
    fs::File::create(b_f).unwrap();

    rm_test(&["-r", a, b], "", "", 0);

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/rm/empty-inacc.sh
// Not including the non-standard -d option
#[test]
fn test_rm_empty_inacc() {
    let test_dir = &format!("{}/test_rm_empty_inacc", env!("CARGO_TARGET_TMPDIR"));
    let inacc = &format!("{test_dir}/inacc");
    let a = &format!("{test_dir}/a");
    let a_unreadable = &format!("{test_dir}/a/unreadable");

    fs::create_dir(test_dir).unwrap();

    fs::DirBuilder::new().mode(0).create(inacc).unwrap();
    rm_test(&["-rf", inacc], "", "", 0);
    assert!(!Path::new(inacc).exists());

    // mkdir -m a-r -p a/unreadable
    fs::create_dir(a).unwrap();
    fs::DirBuilder::new()
        .mode(0o311)
        .create(a_unreadable)
        .unwrap();
    rm_test(&["-rf", a], "", "", 0);
    assert!(!Path::new(a).exists());

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/empty-name.pl
#[test]
fn test_rm_empty_name() {
    let test_dir = &format!("{}/test_rm_empty_name", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");

    fs::create_dir(test_dir).unwrap();

    // `clap` would normally refuse to parse empty strings so a custom parser
    // for `PathBuf` had to be used in the `rm` code. `""` has to be relative
    // and not inside `test_dir` to hit this code path.

    rm_test(
        &[""],
        "",
        "rm: cannot remove '': No such file or directory\n",
        1,
    );

    fs::File::create(a).unwrap();
    fs::File::create(b).unwrap();

    rm_test(
        &[a, "", b],
        "",
        "rm: cannot remove '': No such file or directory\n",
        1,
    );
    assert!(!Path::new(a).exists());
    assert!(!Path::new(b).exists());

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/f-1.sh
#[test]
fn test_rm_f_1() {
    let test_dir = &format!("{}/test_rm_f_1", env!("CARGO_TARGET_TMPDIR"));
    let d = &format!("{test_dir}/d");
    let d_no_such_file = &format!("{test_dir}/d/no-such-file");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(d).unwrap();

    rm_test(&["-f", d_no_such_file], "", "", 0);

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/fail-eacces.sh
#[test]
fn test_rm_fail_eacces() {
    let test_dir = &format!("{}/test_rm_fail_eacces", env!("CARGO_TARGET_TMPDIR"));
    let d = &format!("{test_dir}/d");
    let d_f = &format!("{test_dir}/d/f");
    let d_slink = &format!("{test_dir}/d/slink");
    let e = &format!("{test_dir}/e");
    let e_slink = &format!("{test_dir}/e/slink");
    let f = &format!("{test_dir}/f");

    fs::create_dir(test_dir).unwrap();

    fs::create_dir(d).unwrap();
    fs::File::create(d_f).unwrap();
    unix::fs::symlink(f, d_slink).unwrap();
    // chmod a-w d
    fs::set_permissions(d, fs::Permissions::from_mode(0o555)).unwrap();

    fs::create_dir(e).unwrap();
    unix::fs::symlink(f, e_slink).unwrap();
    // chmod a-w e
    fs::set_permissions(e, fs::Permissions::from_mode(0o555)).unwrap();

    rm_test(
        &["-rf", d_f],
        "",
        &format!("rm: cannot remove '{d_f}': Permission denied\n"),
        1,
    );

    rm_test(
        &["-rf", e],
        "",
        &format!("rm: cannot remove '{e_slink}': Permission denied\n"),
        1,
    );

    fs::set_permissions(d, fs::Permissions::from_mode(0o777)).unwrap();
    fs::set_permissions(e, fs::Permissions::from_mode(0o777)).unwrap();
    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/hash.sh
#[test]
fn test_rm_hash() {
    let test_dir = &format!("{}/test_rm_hash", env!("CARGO_TARGET_TMPDIR"));
    let t = &format!("{test_dir}/t");
    let y = ["y"; 150].join("/");

    fs::create_dir(test_dir).unwrap();

    for i in "123".chars() {
        for j in "abcdefghijklmnopqrstuvwxyz".chars() {
            let path = format!("{t}/{i}/{j}/{y}");
            fs::create_dir_all(path).unwrap();
        }
    }

    rm_test(&["-r", t], "", "", 0);

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/i-1.sh
#[test]
fn test_rm_i_1() {
    let test_dir = &format!("{}/test_rm_i_1", env!("CARGO_TARGET_TMPDIR"));
    let t = &format!("{test_dir}/t");
    let t_a = &format!("{test_dir}/t/a");

    fs::create_dir(test_dir).unwrap();

    fs::create_dir(t).unwrap();
    fs::File::create(t_a).unwrap();
    assert!(Path::new(t_a).exists());

    rm_test_with_stdin(
        &["-i", t_a],
        "n\n",
        "",
        &format!("rm: remove regular empty file '{t_a}'? "),
        0,
    );
    assert!(Path::new(t_a).exists());

    rm_test_with_stdin(
        &["-i", t_a],
        "y\n",
        "",
        &format!("rm: remove regular empty file '{t_a}'? "),
        0,
    );
    assert!(!Path::new(t_a).exists());

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/isatty.sh
#[test]
fn test_rm_isatty() {
    let test_dir = &format!("{}/test_rm_isatty", env!("CARGO_TARGET_TMPDIR"));
    let f = &format!("{test_dir}/f");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();
    fs::set_permissions(f, fs::Permissions::from_mode(0)).unwrap();

    let bin = env!("CARGO_BIN_EXE_rm");

    #[cfg(target_os = "linux")]
    let cmd = format!("{bin} {f}");
    #[cfg(target_os = "linux")]
    let script_args = vec!["-q", "-c", &cmd, "/dev/null"];

    #[cfg(target_os = "macos")]
    let script_args = vec!["-q", "/dev/null", bin, f];

    // Pretend to be a terminal using `script`
    let mut command = Command::new("script");
    let child = command
        .args(&script_args)
        .stdin(Stdio::piped()) // Prevents mangling of terminal output
        .stdout(Stdio::piped()) // script writes the prompt to stdout
        .stderr(Stdio::piped()) // Hides SIGTERM message
        .spawn()
        .unwrap();

    std::thread::sleep(std::time::Duration::from_secs(1));

    assert!(Path::new(f).exists());

    // Terminate without giving an answer to the prompt and then inspect the
    // prompt
    unsafe {
        // SIGTERM should allow `script` to properly shutdown. The SIGKILL in
        // `Child::kill` occasionally causes a "broken pipe" error on other
        // tests.
        let ret = libc::kill(child.id() as libc::pid_t, libc::SIGTERM);
        if ret != 0 {
            panic!("{}", io::Error::last_os_error());
        }
    }

    let output = child.wait_with_output().unwrap();
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();

    let macos_script_prefix = "^D\u{8}\u{8}";
    let out = if stdout.starts_with(macos_script_prefix) {
        stdout.strip_prefix(macos_script_prefix).unwrap()
    } else {
        &stdout
    };

    assert_eq!(
        out,
        format!("rm: remove write-protected regular empty file '{f}'? ")
    );

    fs::set_permissions(f, fs::Permissions::from_mode(0o777)).unwrap();
    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/i-no-r.sh
#[test]
fn test_rm_i_no_r() {
    let test_dir = &format!("{}/test_rm_i_no_r", env!("CARGO_TARGET_TMPDIR"));
    let dir = &format!("{test_dir}/dir");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(dir).unwrap();

    rm_test_with_stdin(
        &["-i", dir],
        "y\n",
        "",
        &format!("rm: cannot remove '{dir}': Is a directory\n"),
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/ignorable.sh
#[test]
fn test_rm_ignorable() {
    let test_dir = &format!("{}/test_rm_ignorable", env!("CARGO_TARGET_TMPDIR"));
    let dir = &format!("{test_dir}/dir");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(dir).unwrap();

    rm_test_with_stdin(
        &["-i", dir],
        "y\n",
        "",
        &format!("rm: cannot remove '{dir}': Is a directory\n"),
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/inaccessible.sh
#[test]
fn test_rm_inaccessible() {
    let test_dir = &format!("{}/test_rm_inaccessible", env!("CARGO_TARGET_TMPDIR"));
    let abs1 = &format!("{test_dir}/abs1");
    let abs2 = &format!("{test_dir}/abs2");
    let no_access = &format!("{test_dir}/no-access");

    fs::create_dir(test_dir).unwrap();

    fs::create_dir(abs1).unwrap();
    fs::create_dir(abs2).unwrap();
    fs::create_dir(no_access).unwrap();

    let command = format!(
        "chmod 0 . && {} -r {abs1} rel {abs2}",
        env!("CARGO_BIN_EXE_rm")
    );

    let child = Command::new("/bin/bash")
        .arg("-c")
        .arg(&command)
        .current_dir(no_access)
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    let output = child.wait_with_output().unwrap();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    assert_eq!("rm: cannot remove 'rel': Permission denied\n", stderr);
    assert_eq!(output.status.code(), Some(1));

    fs::set_permissions(no_access, fs::Permissions::from_mode(0o777)).unwrap();
    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/ir-1.sh
#[test]
fn test_rm_ir_1() {
    let test_dir = &format!("{}/test_rm_ir_1", env!("CARGO_TARGET_TMPDIR"));
    let t = &format!("{test_dir}/t");
    let t_a = &format!("{test_dir}/t/a");
    let t_b = &format!("{test_dir}/t/b");
    let t_c = &format!("{test_dir}/t/c");
    let t_a_a = &format!("{test_dir}/t/a/a");
    let t_b_bb = &format!("{test_dir}/t/b/bb");
    let t_c_cc = &format!("{test_dir}/t/c/cc");

    fs::create_dir(test_dir).unwrap();
    for d in [t_a, t_b, t_c] {
        fs::create_dir_all(d).unwrap();
    }
    for f in [t_a_a, t_b_bb, t_c_cc] {
        fs::File::create(f).unwrap();
    }

    rm_test_with_checker(
        &["-i", "-r", t],
        "y\ny\ny\ny\ny\ny\ny\ny\nn\nn\nn\n",
        |_, output| {
            // Only check the exit code. The prompts have no definite order.
            assert_eq!(output.status.code(), Some(0));
        },
    );

    assert!(Path::new(t).exists());

    let num_subdirs = fs::read_dir(t).unwrap().count();
    assert_eq!(num_subdirs, 1);

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/r-3.sh
#[test]
fn test_rm_r_3() {
    let test_dir = &format!("{}/test_rm_r_3", env!("CARGO_TARGET_TMPDIR"));
    let t = &format!("{test_dir}/t");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(t).unwrap();

    for i in "0123456789abcdefghij".chars() {
        for j in "abcdefghijklmnopqrstuvwxy".chars() {
            let f = &format!("{t}/{i}{j}");
            fs::File::create(f).unwrap();
        }
    }

    assert!(Path::new(&format!("{t}/0a")).exists());
    assert!(Path::new(&format!("{t}/by")).exists());

    rm_test(&["-rf", t], "", "", 0);
    assert!(!Path::new(t).exists());

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/r-4.sh
#[test]
fn test_rm_r_4() {
    let test_dir = &format!("{}/test_rm_r_4", env!("CARGO_TARGET_TMPDIR"));
    let d = &format!("{test_dir}/d");
    let d_a = &format!("{test_dir}/d/a");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(d).unwrap();
    fs::File::create(d_a).unwrap();

    for (dir, dir_cleaned) in [
        (format!("{d}/."), format!("{d}/.")),
        (format!("{d}/./"), format!("{d}/./")),
        // The trailing slashes should be reduced to 1
        (format!("{d}/.////"), format!("{d}/./")),
        (format!("{d}/.."), format!("{d}/..")),
        (format!("{d}/../"), format!("{d}/../")),
    ] {
        rm_test(
            &["-fr", &dir],
            "",
            &format!("rm: refusing to remove '.' or '..' directory: skipping '{dir_cleaned}'\n"),
            1,
        );
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Simplified port of coreutils/tests/rm/r-root.sh
//
// --no-preserve-root is non-POSIX so this only tests if `rm` will give a proper
// diagnostic if the user attempts to remove `/`
#[test]
fn test_rm_r_root() {
    let test_dir = &format!("{}/test_rm_r_root", env!("CARGO_TARGET_TMPDIR"));
    let rootlink = &format!("{test_dir}/rootlink");
    let rootlink2 = &format!("{test_dir}/rootlink2");
    let rootlink3 = &format!("{test_dir}/rootlink3");

    fs::create_dir(test_dir).unwrap();
    unix::fs::symlink("/", rootlink).unwrap();

    // ln -s rootlink rootlink2
    unix::fs::symlink(rootlink, rootlink2).unwrap();

    let relpath_to_root = Path::new(test_dir)
        .components()
        .filter_map(|c| match c {
            std::path::Component::RootDir => None,
            _ => Some(".."),
        })
        .collect::<Vec<&str>>()
        .join("/");
    // ln -sr / rootlink3
    unix::fs::symlink(relpath_to_root, rootlink3).unwrap();

    for arg in [
        String::from("/"),
        String::from("//"),
        String::from("///"),
        String::from("////"),
        format!("{rootlink}/"),
        #[cfg(target_os = "linux")]
        format!("{rootlink2}/"), // macOS treats this as a symlink and not a dir
        format!("{rootlink3}/"),
    ] {
        let err = if arg == "/" {
            String::from("rm: it is dangerous to operate recursively on '/'\n")
        } else {
            format!("rm: it is dangerous to operate recursively on '{arg}' (same as '/')\n")
        };
        rm_test(&["-r", &arg], "", &err, 1);
    }

    for arg in [
        String::from("//."),
        String::from("/./"),
        String::from("/.//"),
        String::from("/../"),
        String::from("/.././"),
        format!("{rootlink}/.."),
        format!("{rootlink2}/."),
        format!("{rootlink3}/./"),
    ] {
        let cleaned = if arg.ends_with("//") {
            arg.strip_suffix("/").unwrap()
        } else {
            &arg
        };
        rm_test(
            &["-r", &arg],
            "",
            &format!("rm: refusing to remove '.' or '..' directory: skipping '{cleaned}'\n"),
            1,
        );
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/readdir-bug.sh
#[test]
fn test_rm_readdir_bug() {
    let test_dir = &format!("{}/test_rm_readdir_bug", env!("CARGO_TARGET_TMPDIR"));
    let b = &format!("{test_dir}/b");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(b).unwrap();

    for i in 1..=250 {
        let f = format!("{b}/{:040}", i);
        fs::File::create(f).unwrap();
    }

    rm_test(&["-rf", b], "", "", 0);
    assert!(!Path::new(b).exists());

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/rm1.sh
#[test]
fn test_rm_rm1() {
    let test_dir = &format!("{}/test_rm_rm1", env!("CARGO_TARGET_TMPDIR"));
    let b = &format!("{test_dir}/b");
    let b_a = &format!("{test_dir}/b/a");
    let b_a_p = &format!("{test_dir}/b/a/p");
    let b_c = &format!("{test_dir}/b/c");
    let b_d = &format!("{test_dir}/b/d");

    fs::create_dir(test_dir).unwrap();
    for dir in [b_a_p, b_c, b_d] {
        fs::create_dir_all(dir).unwrap();
    }
    // chmod ug-w b/a, assuming default dir permission of 0o755
    fs::set_permissions(b_a, fs::Permissions::from_mode(0o555)).unwrap();

    rm_test(
        &["-rf", b],
        "",
        &format!("rm: cannot remove directory '{b_a_p}': Permission denied\n"),
        1,
    );

    assert!(Path::new(b_a_p).exists());
    assert!(!Path::new(b_c).exists());
    assert!(!Path::new(b_d).exists());

    fs::set_permissions(b_a, fs::Permissions::from_mode(0o777)).unwrap();
    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/rm2.sh
#[test]
fn test_rm_rm2() {
    let test_dir = &format!("{}/test_rm_rm2", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let a_0 = &format!("{test_dir}/a/0");
    let a_1 = &format!("{test_dir}/a/1");
    let a_1_2 = &format!("{test_dir}/a/1/2");
    let a_2 = &format!("{test_dir}/a/2");
    let a_3 = &format!("{test_dir}/a/3");
    let b = &format!("{test_dir}/b");
    let b_3 = &format!("{test_dir}/b/3");

    fs::create_dir(test_dir).unwrap();
    for dir in [a_0, a_1_2, b_3, a_2, a_3] {
        fs::create_dir_all(dir).unwrap();
    }
    for dir in [a_1, b] {
        // chmod u-x a/1 b, assuming default dir permission of 0o755
        fs::set_permissions(dir, fs::Permissions::from_mode(0o655)).unwrap();
    }

    rm_test(
        &["-rf", a, b],
        "",
        &format!(
            "rm: cannot remove '{a_1}': Permission denied\n\
            rm: cannot remove '{b}': Permission denied\n"
        ),
        1,
    );

    for dir in [a_1, b] {
        fs::set_permissions(dir, fs::Permissions::from_mode(0o777)).unwrap();
    }

    assert!(!Path::new(a_0).exists());
    assert!(!Path::new(a_2).exists());
    assert!(!Path::new(a_3).exists());

    assert!(Path::new(a_1).exists());
    assert!(Path::new(b_3).exists());

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/rm3.sh
#[test]
fn test_rm_rm3() {
    let test_dir = &format!("{}/test_rm_rm3", env!("CARGO_TARGET_TMPDIR"));
    let z = &format!("{test_dir}/z");
    let empty = &format!("{test_dir}/z/empty");
    let empty_u = &format!("{test_dir}/z/empty-u");
    let fu = &format!("{test_dir}/z/fu");
    let slink = &format!("{test_dir}/z/slink");
    let slinkdot = &format!("{test_dir}/z/slinkdot");
    let d = &format!("{test_dir}/z/d");
    let du = &format!("{test_dir}/z/du");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(z).unwrap();
    for file in [empty, empty_u] {
        fs::File::create(file).unwrap();
    }
    {
        let mut file = fs::File::create(fu).unwrap();
        file.write_all(b"not-empty\n").unwrap();
    }
    unix::fs::symlink("empty-f", slink).unwrap();
    unix::fs::symlink(".", slinkdot).unwrap();
    for dir in [d, du] {
        fs::create_dir(dir).unwrap();
    }
    for path in [fu, du, empty_u] {
        let mode = fs::metadata(path).unwrap().mode();
        let write = libc::S_IWUSR as u32;
        fs::set_permissions(path, fs::Permissions::from_mode(mode & !write)).unwrap();
    }

    rm_test_with_checker(&["-ir", z], "y\ny\ny\ny\ny\ny\ny\ny\ny\n", |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let expected_stderr = format!(
            "rm: descend into directory '{z}'? \
            rm: remove regular empty file '{empty}'? \
            rm: remove write-protected regular file '{fu}'? \
            rm: remove write-protected regular empty file '{empty_u}'? \
            rm: remove symbolic link '{slink}'? \
            rm: remove symbolic link '{slinkdot}'? \
            rm: remove directory '{d}'? \
            rm: remove write-protected directory '{du}'? \
            rm: remove directory '{z}'? "
        );

        // Prompts need to be sorted because the order that they appear depends
        // on the underlying filesystem
        let mut stderr_lines: Vec<_> = stderr.split("? ").collect();
        let mut expected_stderr_lines: Vec<_> = expected_stderr.split("? ").collect();
        stderr_lines.sort();
        expected_stderr_lines.sort();

        assert_eq!(stderr_lines, expected_stderr_lines);
        assert_eq!(output.status.code(), Some(0));
    });

    assert!(!Path::new(z).exists());
    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/rm4.sh
#[test]
fn test_rm_rm4() {
    let test_dir = &format!("{}/test_rm_rm4", env!("CARGO_TARGET_TMPDIR"));
    let dir = &format!("{test_dir}/dir");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(dir).unwrap();

    rm_test(
        &[dir],
        "",
        &format!("rm: cannot remove '{dir}': Is a directory\n"),
        1,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/rm5.sh
#[test]
fn test_rm_rm5() {
    let test_dir = &format!("{}/test_rm_rm5", env!("CARGO_TARGET_TMPDIR"));
    let d = &format!("{test_dir}/d");
    let d_e = &format!("{test_dir}/d/e");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir_all(d_e).unwrap();

    rm_test_with_checker(&["-ir", d], "y\ny\ny\n", |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let expected_stderr = format!(
            "rm: descend into directory '{d}'? \
            rm: remove directory '{d_e}'? \
            rm: remove directory '{d}'? "
        );

        let mut stderr_lines: Vec<_> = stderr.split("? ").collect();
        let mut expected_stderr_lines: Vec<_> = expected_stderr.split("? ").collect();
        stderr_lines.sort();
        expected_stderr_lines.sort();

        assert_eq!(stderr_lines, expected_stderr_lines);
        assert_eq!(output.status.code(), Some(0));
    });

    assert!(!Path::new(d).exists());
    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/sunos-1.sh
#[test]
fn test_rm_sunos_1() {
    rm_test(
        &[""],
        "",
        &format!("rm: cannot remove '': No such file or directory\n"),
        1,
    );
}

// Port of coreutils/tests/rm/unread2.sh
#[test]
fn test_rm_unread2() {
    let test_dir = &format!("{}/test_rm_unread2", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let a_b = &format!("{test_dir}/a/b");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir_all(a_b).unwrap();
    // chmod u-r a, assuming default dir permission of 0o644
    fs::set_permissions(a, fs::Permissions::from_mode(0o244)).unwrap();

    rm_test(
        &["-rf", a],
        "",
        &format!("rm: cannot remove '{a}': Permission denied\n"),
        1,
    );

    fs::set_permissions(a, fs::Permissions::from_mode(0o777)).unwrap();
    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/unread3.sh
#[test]
fn test_rm_unread3() {
    fn cd_and_rm_test(test_dir: &str, args: &[&str], expected_exit_code: i32) {
        let program = env!("CARGO_BIN_EXE_rm");
        let mut command = Command::new(program);
        let child = command
            .current_dir(test_dir)
            .args(args)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap();

        let output = child.wait_with_output().unwrap();
        assert!(output.stdout.is_empty());
        assert!(output.stderr.is_empty());
        assert_eq!(output.status.code(), Some(expected_exit_code));
    }

    let test_dir = &format!("{}/test_rm_unread3", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let a_1 = &format!("{test_dir}/a/1");
    let b = &format!("{test_dir}/b");
    let c = &format!("{test_dir}/c");
    let d = &format!("{test_dir}/d");
    let d_2 = &format!("{test_dir}/d/2");
    let e = &format!("{test_dir}/e");
    let e_3 = &format!("{test_dir}/e/3");

    fs::create_dir(test_dir).unwrap();
    for dir in [a_1, b, c, d_2, e_3] {
        fs::create_dir_all(dir).unwrap();
    }
    fs::set_permissions(c, fs::Permissions::from_mode(0o100)).unwrap();

    cd_and_rm_test(c, &["-r", a, b], 0);
    cd_and_rm_test(c, &["-r", d, e], 0);

    fs::set_permissions(c, fs::Permissions::from_mode(0o777)).unwrap();
    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/unreadable.pl
#[test]
fn test_rm_unreadable() {
    let test_dir = &format!("{}/test_rm_unreadable", env!("CARGO_TARGET_TMPDIR"));
    let d = &format!("{test_dir}/d");
    let d_x = &format!("{test_dir}/d/x");

    fs::create_dir(test_dir).unwrap();

    fs::DirBuilder::new().mode(0o100).create(d).unwrap();
    rm_test(&["-rf", d], "", "", 0);

    fs::create_dir(d).unwrap();
    fs::DirBuilder::new().mode(0o700).create(d_x).unwrap();
    fs::set_permissions(d, fs::Permissions::from_mode(0o100)).unwrap();
    rm_test(
        &["-rf", d],
        "",
        &format!("rm: cannot remove '{d}': Permission denied\n"),
        1,
    );

    fs::set_permissions(d, fs::Permissions::from_mode(0o777)).unwrap();
    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/fail-2eperm.sh
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
fn test_rm_fail_2eperm() {
    let test_dir = &format!("{}/test_rm_fail_2eperm", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let a_b = &format!("{test_dir}/a/b");

    fs::create_dir(test_dir).unwrap();

    let non_root = option_env!("NON_ROOT_USERNAME").expect(
        "`test_rm_fail_2eperm` requires the \
        `NON_ROOT_USERNAME` environment variable",
    );

    // chown $NON_ROOT_USERNAME $test_dir
    unsafe {
        let non_root_cstr = CString::new(non_root).unwrap();
        let passwd = libc::getpwnam(non_root_cstr.as_ptr());
        if passwd.is_null() {
            panic!("{}", io::Error::last_os_error());
        }
        let uid = (*passwd).pw_uid;

        let test_dir_cstr = CString::new(test_dir.as_bytes()).unwrap();

        libc::chown(
            test_dir_cstr.as_ptr(),
            uid,
            fs::metadata(test_dir).unwrap().gid(),
        );
    }

    fs::DirBuilder::new().mode(0o1777).create(a).unwrap();
    fs::File::create(a_b).unwrap();

    let child = Command::new("chroot")
        .args([
            "--skip-chdir",
            &format!("--user={non_root}"),
            "/",
            env!("CARGO_BIN_EXE_rm"),
            "-rf",
            a,
        ])
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    let output = child.wait_with_output().unwrap();

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(
        stderr,
        format!("rm: cannot remove '{a_b}': Permission denied\n")
    );

    assert_eq!(output.status.code(), Some(1));

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/rm/no-give-up.sh
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
fn test_rm_no_give_up() {
    let test_dir = &format!("{}/test_rm_no_give_up", env!("CARGO_TARGET_TMPDIR"));
    let d = &format!("{test_dir}/d");
    let d_f = &format!("{test_dir}/d/f");

    fs::create_dir(test_dir).unwrap();

    let non_root = option_env!("NON_ROOT_USERNAME").expect(
        "`test_rm_no_give_up` requires the \
        `NON_ROOT_USERNAME` environment variable",
    );

    fs::create_dir(d).unwrap();
    fs::File::create(d_f).unwrap();

    unsafe {
        let non_root_cstr = CString::new(non_root).unwrap();
        let passwd = libc::getpwnam(non_root_cstr.as_ptr());
        if passwd.is_null() {
            panic!("{}", io::Error::last_os_error());
        }
        let uid = (*passwd).pw_uid;

        // The two calls below to `libc::chown` is equivalent to:
        // chown -R $NON_ROOT_USERNAME d

        // chown $NON_ROOT_USERNAME d
        let d_cstr = CString::new(d.as_bytes()).unwrap();
        libc::chown(d_cstr.as_ptr(), uid, fs::metadata(d).unwrap().gid());

        // chown $NON_ROOT_USERNAME d/f
        let d_f_cstr = CString::new(d_f.as_bytes()).unwrap();
        libc::chown(d_f_cstr.as_ptr(), uid, fs::metadata(d_f).unwrap().gid());
    }

    // chmod go=x $test_dir
    fs::set_permissions(test_dir, fs::Permissions::from_mode(0o611)).unwrap();

    let child = Command::new("chroot")
        .args([
            "--skip-chdir",
            &format!("--user={non_root}"),
            "/",
            env!("CARGO_BIN_EXE_rm"),
            "-rf",
            d,
        ])
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();

    let output = child.wait_with_output().unwrap();

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(
        stderr,
        format!("rm: cannot remove '{d}': Permission denied\n")
    );

    assert!(Path::new(d).exists());
    assert!(!Path::new(d_f).exists());

    fs::remove_dir_all(test_dir).unwrap();
}
