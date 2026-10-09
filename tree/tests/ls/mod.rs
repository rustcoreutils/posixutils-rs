//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use regex::Regex;
use std::ffi::CString;
use std::fs;
use std::io::{self, Write};
use std::os::unix::fs::MetadataExt;
use std::path::Path;
use std::thread;
use std::time::Duration;

fn get_errno() -> i32 {
    io::Error::last_os_error().raw_os_error().unwrap()
}

fn canonical_symlink(original: &str, link: &str) -> io::Result<()> {
    // `fs::canonicalize` is important here. If it's left out then the
    // symbolic link cannot be followed with -L.
    std::os::unix::fs::symlink(fs::canonicalize(original).unwrap(), link)
}

enum TimeToChange<'a> {
    Accessed(&'a str),
    Modified(&'a str),
    Both(&'a str),
}

fn change_file_time(path: &str, time: TimeToChange) {
    let s = match &time {
        TimeToChange::Accessed(s) => s,
        TimeToChange::Modified(s) => s,
        TimeToChange::Both(s) => s,
    };
    let dt =
        chrono::DateTime::parse_from_str(&format!("{s} +0000"), "%Y-%m-%d %H:%M:%S %z").unwrap();
    let tv_sec = dt.timestamp() as libc::time_t;
    let tv_usec = dt.timestamp_subsec_micros() as libc::suseconds_t;

    unsafe {
        let filename = CString::new(path).unwrap();
        let mut times = [libc::timeval { tv_sec, tv_usec }; 2];
        match &time {
            TimeToChange::Both(_) => (),
            other => {
                let metadata = Path::new(path).metadata().unwrap();

                match other {
                    TimeToChange::Accessed(_) => {
                        let modified_dt: chrono::DateTime<chrono::Utc> =
                            metadata.modified().unwrap().into();
                        let tv_sec = modified_dt.timestamp() as libc::time_t;
                        let tv_usec = modified_dt.timestamp_subsec_micros() as libc::suseconds_t;
                        times[1] = libc::timeval { tv_sec, tv_usec }
                    }
                    TimeToChange::Modified(_) => {
                        let accessed_dt: chrono::DateTime<chrono::Utc> =
                            metadata.accessed().unwrap().into();
                        let tv_sec = accessed_dt.timestamp() as libc::time_t;
                        let tv_usec = accessed_dt.timestamp_subsec_micros() as libc::suseconds_t;
                        times[0] = libc::timeval { tv_sec, tv_usec }
                    }
                    _ => unreachable!(),
                }
            }
        }
        let ret = libc::utimes(filename.as_ptr(), times.as_ptr());
        if ret != 0 {
            panic!("{}", io::Error::last_os_error());
        }
    }
}

fn ls_test(args: &[&str], expected_output: &str, expected_error: &str, expected_exit_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("ls"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

fn ls_test_with_checker<F: FnMut(&TestPlan, &std::process::Output)>(args: &[&str], checker: F) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    let test_plan = TestPlan {
        cmd: String::from("ls"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    };

    run_test_with_checker(test_plan, checker);
}

// `ls_test` but sets the working directory for the child process
fn cd_and_ls_test(test_dir: &str, args: &[&str], expected_out: &str) {
    let mut command = std::process::Command::new(env!("CARGO_BIN_EXE_ls"));
    let child = command
        .current_dir(test_dir)
        .args(args)
        .stdout(std::process::Stdio::piped())
        .spawn()
        .unwrap();

    let output = child.wait_with_output().unwrap();

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert_eq!(stdout, expected_out);

    assert_eq!(output.status.code(), Some(0));
}

// Port of coreutils/tests/ls/a-option.sh
#[test]
fn test_ls_empty_directory() {
    let test_dir = &format!("{}/test_ls_empty_directory", env!("CARGO_TARGET_TMPDIR"));
    fs::create_dir(test_dir).unwrap();
    ls_test(&["-aA", test_dir], "", "", 0);
    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/ls/dangle.sh
// Not including substituting missing metadata with "?" which is non-standard
#[test]
fn test_ls_dangle() {
    let test_dir = &format!("{}/test_ls_dangle", env!("CARGO_TARGET_TMPDIR"));
    let dangle = &format!("{test_dir}/dangle");
    let dir = &format!("{test_dir}/dir");
    let dir_sub = &format!("{test_dir}/dir/sub");
    let slink_to_dir = &format!("{test_dir}/slink-to-dir");

    // Removed first: the test creates with `unwrap()` and only cleans up on
    // success, so one failure left the directory behind and every later run
    // died on "File exists" instead of reporting the real problem.
    let _ = fs::remove_dir_all(test_dir);
    fs::create_dir(test_dir).unwrap();
    fs::create_dir(dir).unwrap();
    fs::create_dir(dir_sub).unwrap();

    if let Err(e) = std::os::unix::fs::symlink("no-such-file", dangle) {
        if e.kind() != io::ErrorKind::AlreadyExists {
            panic!("{}", e);
        }
    }

    canonical_symlink(dir, slink_to_dir).unwrap();

    // Must fail to dereference the symlink
    ls_test(
        &["-L", dangle],
        "",
        &format!("ls: cannot access '{dangle}': No such file or directory\n"),
        1,
    );
    ls_test(
        &["-H", dangle],
        "",
        &format!("ls: cannot access '{dangle}': No such file or directory\n"),
        1,
    );

    // Not using -H or -L should cause it to succeed
    ls_test(&[dangle], &format!("{dangle}\n"), "", 0);

    // slink_to_dir is a proper symlink so these three should all succeed
    ls_test(&[slink_to_dir], "sub\n", "", 0);
    ls_test(&["-H", slink_to_dir], "sub\n", "", 0);
    ls_test(&["-L", slink_to_dir], "sub\n", "", 0);

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/ls/file-type.sh
// --indicator-style and --color non-standard so are not included.
//
// This test will skip the block/character devices if not run with sudo:
// `sudo -E cargo test`
#[test]
fn test_ls_file_type() {
    let test_dir = &format!("{}/test_ls_file_type", env!("CARGO_TARGET_TMPDIR"));
    let sub = &format!("{test_dir}/sub");
    let dir = &format!("{test_dir}/sub/dir");
    let regular = &format!("{test_dir}/sub/regular");
    let executable = &format!("{test_dir}/sub/executable");
    let slink_reg = &format!("{test_dir}/sub/slink-reg");
    let slink_dir = &format!("{test_dir}/sub/slink-dir");
    let slink_dangle = &format!("{test_dir}/sub/slink-dangle");
    let block = &format!("{test_dir}/sub/block");
    let char = &format!("{test_dir}/sub/char");
    let fifo = &format!("{test_dir}/sub/fifo");
    let block_cstr = CString::new(block.as_bytes()).unwrap();
    let char_cstr = CString::new(char.as_bytes()).unwrap();
    let fifo_cstr = CString::new(fifo.as_bytes()).unwrap();

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(sub).unwrap();
    fs::create_dir(dir).unwrap();

    fs::File::create(regular).unwrap();
    fs::File::create(executable).unwrap();

    unsafe {
        let executable_cstr = CString::new(executable.as_bytes()).unwrap();

        // Executable for all
        let mode = libc::S_IXUSR | libc::S_IXGRP | libc::S_IXOTH;

        let ret = libc::chmod(executable_cstr.as_ptr(), mode);
        if ret != 0 {
            panic!("{}", io::Error::last_os_error());
        }
    }

    canonical_symlink(regular, slink_reg).unwrap();
    canonical_symlink(dir, slink_dir).unwrap();

    if let Err(e) = std::os::unix::fs::symlink("nowhere", slink_dangle) {
        if e.kind() != io::ErrorKind::AlreadyExists {
            panic!("{}", e);
        }
    }

    let mut skip_device_files = false;

    unsafe {
        // Creating files with S_IFBLK or S_IFCHR requires superuser.
        let ret = libc::mknod(block_cstr.as_ptr(), libc::S_IFBLK, libc::makedev(20, 20));
        if ret != 0 {
            match get_errno() {
                libc::EEXIST => (),
                libc::EPERM => skip_device_files = true,
                _ => panic!("{}", io::Error::last_os_error()),
            }
        }
        let ret = libc::mknod(char_cstr.as_ptr(), libc::S_IFCHR, libc::makedev(10, 10));
        if ret != 0 {
            match get_errno() {
                libc::EEXIST => (),
                libc::EPERM => skip_device_files = true,
                _ => panic!("{}", io::Error::last_os_error()),
            }
        }

        // rw-r--r--
        let mode = libc::S_IRUSR | libc::S_IWUSR | libc::S_IRGRP | libc::S_IROTH;
        let ret = libc::mkfifo(fifo_cstr.as_ptr(), mode);
        if ret != 0 && get_errno() != libc::EEXIST {
            panic!("{}", io::Error::last_os_error());
        }
    }

    let ls_f_result = "dir/\nexecutable*\nfifo|\nregular\nslink-dangle@\nslink-dir@\nslink-reg@\n";
    if !skip_device_files {
        ls_test(&["-F", sub], &format!("block\nchar\n{ls_f_result}"), "", 0);
    } else {
        ls_test(&["-F", sub], ls_f_result, "", 0);
    }

    let ls_p_result = "dir/\nexecutable\nfifo\nregular\nslink-dangle\nslink-dir\nslink-reg\n";
    if !skip_device_files {
        ls_test(&["-p", sub], &format!("block\nchar\n{ls_p_result}"), "", 0);
    } else {
        ls_test(&["-p", sub], ls_p_result, "", 0);
    }

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/ls/infloop.sh
#[test]
fn test_ls_infloop() {
    let test_dir = &format!("{}/test_ls_infloop", env!("CARGO_TARGET_TMPDIR"));
    let loop_dir = &format!("{test_dir}/loop");
    let loop_sub = &format!("{test_dir}/loop/sub");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(loop_dir).unwrap();
    canonical_symlink(loop_dir, loop_sub).unwrap();

    // The diagnostic names the entry that would close the cycle, as GNU does,
    // not the directory being listed when it was found.
    ls_test(
        &["-RL", loop_sub],
        &format!("{loop_sub}:\nsub\n"),
        &format!("ls: {loop_sub}/sub: not listing already-listed directory\n"),
        2,
    );

    fs::remove_dir_all(test_dir).unwrap();
}

/// A tree like the one `cp -a` leaves: hard links (one pair inside a single
/// directory, one pair across directories), a symlink to a directory, and empty
/// directories.
fn make_hard_link_tree(root: &Path) {
    let d2 = root.join("d1/d2");
    fs::create_dir_all(d2.join("sub")).unwrap();
    fs::create_dir_all(root.join("d1/empty")).unwrap();
    fs::write(d2.join("f1"), "a").unwrap();
    fs::write(d2.join("f2"), "b").unwrap();
    fs::hard_link(d2.join("f1"), root.join("d1/hl")).unwrap();
    fs::hard_link(d2.join("f2"), d2.join("f2same")).unwrap();
    std::os::unix::fs::symlink("d1/d2", root.join("lnk")).unwrap();
}

/// Two names for one file inside a directory are not a directory cycle: a
/// directory operand is listed once, in full, with no diagnostic, whatever the
/// output format.
#[test]
fn test_ls_hard_links_are_not_an_already_listed_directory() {
    let dir = plib::tmp::tempdir().unwrap();
    make_hard_link_tree(dir.path());
    let d2 = dir.path().join("d1/d2");
    let d2s = d2.to_str().unwrap();

    ls_test(&[d2s], "f1\nf2\nf2same\nsub\n", "", 0);

    let ino = |name: &str| fs::symlink_metadata(d2.join(name)).unwrap().ino();
    let expected = format!(
        "{} f1\n{} f2\n{} f2same\n{} sub\n",
        ino("f1"),
        ino("f2"),
        ino("f2same"),
        ino("sub")
    );
    ls_test(&["-i", "-1", d2s], &expected, "", 0);

    for args in [&["-l", d2s][..], &["-li", d2s][..], &["-R", d2s][..]] {
        ls_test_with_checker(args, |_, output| {
            assert_eq!(String::from_utf8_lossy(&output.stderr), "", "ls {args:?}");
            assert_eq!(output.status.code(), Some(0), "ls {args:?}");
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(stdout.contains("f2same"), "ls {args:?}: {stdout:?}");
        });
    }
}

/// The same directory named twice is listed twice, and a directory reached
/// through a symlink operand is listed even when it is also named directly.
#[test]
fn test_ls_directory_operand_listed_each_time_it_is_named() {
    let dir = plib::tmp::tempdir().unwrap();
    make_hard_link_tree(dir.path());
    let d1 = dir.path().join("d1");
    let d1s = d1.to_str().unwrap();
    let lnk = dir.path().join("lnk");
    let lnks = lnk.to_str().unwrap();

    ls_test(
        &[d1s, d1s],
        &format!("{d1s}:\nd2\nempty\nhl\n\n{d1s}:\nd2\nempty\nhl\n"),
        "",
        0,
    );
    ls_test(&[lnks], "f1\nf2\nf2same\nsub\n", "", 0);
    ls_test(
        &["-R", d1s],
        &format!(
            "{d1s}:\nd2\nempty\nhl\n\n{d1s}/d2:\nf1\nf2\nf2same\nsub\n\n\
             {d1s}/d2/sub:\n\n{d1s}/empty:\n"
        ),
        "",
        0,
    );
}

/// POSIX: directory operands are sorted like any other names (and by -r/-t/-S),
/// not listed in command-line order.
#[test]
fn test_ls_directory_operands_are_sorted() {
    let dir = plib::tmp::tempdir().unwrap();
    make_hard_link_tree(dir.path());
    let d1 = dir.path().join("d1");
    let d1s = d1.to_str().unwrap();
    let d2 = d1.join("d2");
    let d2s = d2.to_str().unwrap();
    let d1_listing = format!("{d1s}:\nd2\nempty\nhl\n");
    let d2_listing = format!("{d2s}:\nf1\nf2\nf2same\nsub\n");

    ls_test(&[d2s, d1s], &format!("{d1_listing}\n{d2_listing}"), "", 0);
    ls_test(
        &["-r", d1s, d2s],
        &format!("{d2s}:\nsub\nf2same\nf2\nf1\n\n{d1s}:\nhl\nempty\nd2\n"),
        "",
        0,
    );
}

/// -d lists a directory operand as itself, not its contents.
#[test]
fn test_ls_d_lists_directory_operands_as_files() {
    let dir = plib::tmp::tempdir().unwrap();
    make_hard_link_tree(dir.path());
    let d1 = dir.path().join("d1");
    let d1s = d1.to_str().unwrap();
    let hl = d1.join("hl");
    let hls = hl.to_str().unwrap();
    let lnk = dir.path().join("lnk");
    let lnks = lnk.to_str().unwrap();

    ls_test(&["-d", hls, d1s], &format!("{d1s}\n{hls}\n"), "", 0);
    ls_test(&["-d", lnks], &format!("{lnks}\n"), "", 0);
    ls_test_with_checker(&["-ld", d1s], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.starts_with('d'), "{stdout:?}");
        assert!(stdout.ends_with(&format!(" {d1s}\n")), "{stdout:?}");
        assert_eq!(stdout.lines().count(), 1, "{stdout:?}");
    });
}

/// POSIX: with -d, -F or -l and neither -H nor -L, a symbolic link to a
/// directory named as an operand is written as the link itself.
#[test]
fn test_ls_symlink_operand_not_followed_under_d_f_l() {
    let dir = plib::tmp::tempdir().unwrap();
    make_hard_link_tree(dir.path());
    let lnk = dir.path().join("lnk");
    let lnks = lnk.to_str().unwrap();

    ls_test(&["-F", lnks], &format!("{lnks}@\n"), "", 0);
    ls_test_with_checker(&["-l", lnks], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.starts_with('l'), "{stdout:?}");
        assert!(
            stdout.ends_with(&format!(" {lnks} -> d1/d2\n")),
            "{stdout:?}"
        );
    });
    // -H follows it again, so -l lists the directory's contents.
    ls_test_with_checker(&["-lH", lnks], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.starts_with("total "), "{stdout:?}");
        assert!(stdout.contains(" f2same\n"), "{stdout:?}");
    });
    ls_test(&["-FL", lnks], "f1\nf2\nf2same\nsub/\n", "", 0);
}

/// POSIX: under -l or -s each list of files within a directory is preceded by
/// its total, and an empty list is still a list: `total 0`.
#[test]
fn test_ls_empty_directory_has_a_total_line() {
    let dir = plib::tmp::tempdir().unwrap();
    let top = dir.path().join("top");
    fs::create_dir_all(top.join("empty")).unwrap();
    let tops = top.to_str().unwrap();
    let empty = top.join("empty");
    let emptys = empty.to_str().unwrap();

    ls_test(&["-l", emptys], "total 0\n", "", 0);
    ls_test(&["-s", emptys], "total 0\n", "", 0);
    ls_test(&[emptys], "", "", 0);
    // A directory's own block count depends on the file system.
    ls_test_with_checker(&["-sR", tops], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.starts_with(&format!("{tops}:\ntotal ")),
            "{stdout:?}"
        );
        assert!(
            stdout.ends_with(&format!(" empty\n\n{emptys}:\ntotal 0\n")),
            "{stdout:?}"
        );
        assert_eq!(output.status.code(), Some(0));
    });
}

/// A `-R` cycle refuses only the entry that closes it; the rest of the tree is
/// still listed, as GNU does.
#[test]
fn test_ls_recursive_cycle_skips_only_the_looping_entry() {
    let dir = plib::tmp::tempdir().unwrap();
    let d1 = dir.path().join("d1");
    fs::create_dir_all(d1.join("a/sub")).unwrap();
    fs::create_dir_all(d1.join("z")).unwrap();
    fs::write(d1.join("z/last"), "").unwrap();
    std::os::unix::fs::symlink("../..", d1.join("a/sub/up")).unwrap();
    let d1s = d1.to_str().unwrap();

    ls_test(
        &["-RL", d1s],
        &format!("{d1s}:\na\nz\n\n{d1s}/a:\nsub\n\n{d1s}/a/sub:\nup\n\n{d1s}/z:\nlast\n"),
        &format!("ls: {d1s}/a/sub/up: not listing already-listed directory\n"),
        2,
    );
}

// Port of coreutils/tests/ls/inode.sh
#[test]
fn test_ls_inode() {
    let test_dir = &format!("{}/test_ls_inode", env!("CARGO_TARGET_TMPDIR"));
    let original = &format!("{test_dir}/f");
    let link = &format!("{test_dir}/slink");

    let re = Regex::new(r"(\d+) .*f\s+(\d+) .*slink").unwrap();

    fs::create_dir(test_dir).unwrap();
    fs::File::create(original).unwrap();
    canonical_symlink(original, link).unwrap();

    // Args passed in command line
    // Different inode numbers without -H or -L
    ls_test_with_checker(&["-Ci", original, link], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let captures = re.captures(&stdout).unwrap();
        let inode_f: u64 = captures.get(1).unwrap().as_str().parse().unwrap();
        let inode_slink: u64 = captures.get(2).unwrap().as_str().parse().unwrap();
        assert_ne!(inode_f, inode_slink);
        assert_eq!(output.status.code(), Some(0));
    });

    // Args passed in command line
    // Same inode numbers with -L
    ls_test_with_checker(&["-CLi", original, link], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let captures = re.captures(&stdout).unwrap();
        let inode_f: u64 = captures.get(1).unwrap().as_str().parse().unwrap();
        let inode_slink: u64 = captures.get(2).unwrap().as_str().parse().unwrap();
        assert_eq!(inode_f, inode_slink);
        assert_eq!(output.status.code(), Some(0));
    });

    // Args passed in command line
    // Same inode numbers with -H
    ls_test_with_checker(&["-CHi", original, link], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let captures = re.captures(&stdout).unwrap();
        let inode_f: u64 = captures.get(1).unwrap().as_str().parse().unwrap();
        let inode_slink: u64 = captures.get(2).unwrap().as_str().parse().unwrap();
        assert_eq!(inode_f, inode_slink);
        assert_eq!(output.status.code(), Some(0));
    });

    // Files in directory
    // Different inode numbers without -L
    ls_test_with_checker(&["-Ci", test_dir], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let captures = re.captures(&stdout).unwrap();
        let inode_f: u64 = captures.get(1).unwrap().as_str().parse().unwrap();
        let inode_slink: u64 = captures.get(2).unwrap().as_str().parse().unwrap();
        assert_ne!(inode_f, inode_slink);
        assert_eq!(output.status.code(), Some(0));
    });

    // Files in directory
    // Same inode numbers with -L
    ls_test_with_checker(&["-CLi", test_dir], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let captures = re.captures(&stdout).unwrap();
        let inode_f: u64 = captures.get(1).unwrap().as_str().parse().unwrap();
        let inode_slink: u64 = captures.get(2).unwrap().as_str().parse().unwrap();
        assert_eq!(inode_f, inode_slink);
        assert_eq!(output.status.code(), Some(0));
    });

    // Files in directory
    // Different inode numbers even with -H
    ls_test_with_checker(&["-CHi", test_dir], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let captures = re.captures(&stdout).unwrap();
        let inode_f: u64 = captures.get(1).unwrap().as_str().parse().unwrap();
        let inode_slink: u64 = captures.get(2).unwrap().as_str().parse().unwrap();
        assert_ne!(inode_f, inode_slink);
        assert_eq!(output.status.code(), Some(0));
    });

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/ls/m-option.sh
// The -w argument is non-POSIX.
#[test]
fn test_ls_m_option() {
    let test_dir = &format!("{}/test_ls_m_option", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(a).unwrap();
    {
        let mut file = fs::File::create(b).unwrap();

        for i in 1..=2000 {
            let s = format!("{i}\n");
            file.write_all(s.as_bytes()).unwrap();
        }
    }

    // Use `cd_and_ls_test` to avoid forcing the output into a single column
    // because of the operands `a` and `b` being too long if passed as absolute
    // paths.

    // Original test is using -w2 here
    cd_and_ls_test(test_dir, &["-m", "a", "b"], "a, b\n");

    // -k for 1024-byte block sizes which is the default for coreutils. Default
    // for this implementation is 512-byte blocks as mentioned in the STDOUT
    // section of:
    // https://pubs.opengroup.org/onlinepubs/9699919799/utilities/ls.html
    cd_and_ls_test(test_dir, &["-smk", "a", "b"], "0 a, 12 b\n");

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/ls/no-arg.sh
#[test]
fn test_ls_no_arg() {
    let test_dir = &format!("{}/test_ls_no_arg", env!("CARGO_TARGET_TMPDIR"));
    let dir = &format!("{test_dir}/dir");
    let subdir = &format!("{test_dir}/dir/subdir");
    let file2 = &format!("{test_dir}/dir/subdir/file2");
    let symlink = &format!("{test_dir}/symlink");
    let out = &format!("{test_dir}/out");
    let exp = &format!("{test_dir}/exp");

    fs::create_dir(test_dir).unwrap();
    fs::create_dir(dir).unwrap();
    fs::create_dir(subdir).unwrap();
    fs::File::create(file2).unwrap();

    if let Err(e) = std::os::unix::fs::symlink("f", symlink) {
        if e.kind() != io::ErrorKind::AlreadyExists {
            panic!("{}", e);
        }
    }

    // Not really using this to write the output unlike in the original test
    fs::File::create(out).unwrap();

    let exp_str = "dir\n\
                   exp\n\
                   out\n\
                   symlink\n";

    {
        let mut file = fs::File::create(exp).unwrap();
        file.write_all(exp_str.as_bytes()).unwrap();
    }

    cd_and_ls_test(test_dir, &["-1"], exp_str);

    let exp_str = ".:\n\
                   dir\n\
                   exp\n\
                   out\n\
                   symlink\n\
                   \n\
                   ./dir:\n\
                   subdir\n\
                   \n\
                   ./dir/subdir:\n\
                   file2\n";
    cd_and_ls_test(test_dir, &["-R1"], exp_str);

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/ls/recursive.sh
#[test]
fn test_ls_recursive() {
    let test_dir = &format!("{}/test_ls_recursive", env!("CARGO_TARGET_TMPDIR"));
    let x = &format!("{test_dir}/x");
    let y = &format!("{test_dir}/y");
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");
    let c = &format!("{test_dir}/c");
    let a1 = &format!("{test_dir}/a/1");
    let a2 = &format!("{test_dir}/a/2");
    let a3 = &format!("{test_dir}/a/3");

    let f = &format!("{test_dir}/f");
    let a1i = &format!("{test_dir}/a/1/I");
    let a1ii = &format!("{test_dir}/a/1/II");

    fs::create_dir(test_dir).unwrap();
    for dir in [x, y, a, b, c] {
        fs::create_dir(dir).unwrap();
    }
    for dir in [a1, a2, a3] {
        fs::create_dir(dir).unwrap();
    }
    for file in [f, a1i, a1ii] {
        fs::File::create(file).unwrap();
    }

    let result = format!(
        "{a}:\n\
        1\n\
        2\n\
        3\n\
        \n\
        {a1}:\n\
        I\n\
        II\n\
        \n\
        {a2}:\n\
        \n\
        {a3}:\n\
        \n\
        {b}:\n\
        \n\
        {c}:\n"
    );
    ls_test(&["-R1", a, b, c], &result, "", 0);

    let result = format!(
        "{f}\n\
        \n\
        {x}:\n\
        \n\
        {y}:\n"
    );
    ls_test(&["-R1", x, y, f], &result, "", 0);

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/ls/rt-1.sh
#[test]
fn test_ls_rt_1() {
    let test_dir = &format!("{}/test_ls_rt_1", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");
    let c = &format!("{test_dir}/c");
    let date = "1998-01-15 00:00:00";

    fs::create_dir(test_dir).unwrap();
    fs::File::create(a).unwrap();
    fs::File::create(b).unwrap();
    fs::File::create(c).unwrap();
    change_file_time(a, TimeToChange::Both(date));
    change_file_time(b, TimeToChange::Both(date));
    change_file_time(c, TimeToChange::Both(date));

    ls_test(&["-1t", a, b, c], &format!("{a}\n{b}\n{c}\n"), "", 0);
    ls_test(&["-1rt", a, b, c], &format!("{c}\n{b}\n{a}\n"), "", 0);

    fs::remove_dir_all(test_dir).unwrap();
}

// Port of coreutils/tests/ls/size-align.sh
#[test]
fn test_ls_size_align() {
    let test_dir = &format!("{}/test_ls_size_align", env!("CARGO_TARGET_TMPDIR"));
    let small = &format!("{test_dir}/small");
    let alloc = &format!("{test_dir}/alloc");
    let large = &format!("{test_dir}/large");

    fs::create_dir(test_dir).unwrap();
    fs::File::create(small).unwrap();
    {
        let mut file = fs::File::create(alloc).unwrap();
        file.write_all(b"\n").unwrap();
    }
    {
        let mut file = fs::File::create(large).unwrap();
        let data = vec![b'\n'; 123456];
        file.write_all(&data).unwrap();
    }

    // The rows should all have the same length
    ls_test_with_checker(&["-s", "-l", small, alloc, large], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let lengths: Vec<_> = stdout.lines().map(|x| x.len()).collect();
        let length = lengths[0];
        assert!(lengths.iter().all(|l| *l == length));
        assert_eq!(output.status.code(), Some(0));
    });

    fs::remove_dir_all(test_dir).unwrap();
}

// Partial port of coreutils/tests/ls/ls-time.sh
// --full is non-POSIX so we can only check for coarser grained timestamps
#[test]
fn test_ls_time() {
    // This gets inherited by child processes
    std::env::set_var("TZ", "UTC0");

    let test_dir = &format!("{}/test_ls_time", env!("CARGO_TARGET_TMPDIR"));
    let a = &format!("{test_dir}/a");
    let b = &format!("{test_dir}/b");
    let c = &format!("{test_dir}/c");

    let t1 = "1998-01-15 21:00:00";
    let t2 = "1998-01-15 22:00:00";
    let t3 = "1998-01-15 23:00:00";

    let u1 = "1998-01-14 11:00:00";
    let u2 = "1998-01-14 12:00:00";
    let u3 = "1998-01-14 13:00:00";

    fs::create_dir(test_dir).unwrap();
    fs::File::create(a).unwrap();
    fs::File::create(b).unwrap();
    fs::File::create(c).unwrap();

    change_file_time(a, TimeToChange::Modified(t3));
    change_file_time(b, TimeToChange::Modified(t2));
    change_file_time(c, TimeToChange::Modified(t1));

    change_file_time(c, TimeToChange::Accessed(u3));
    change_file_time(b, TimeToChange::Accessed(u2));

    // Change a's ctime to be after c's
    loop {
        thread::sleep(Duration::from_millis(100));
        change_file_time(a, TimeToChange::Accessed(u1));
        let metadata_a = Path::new(a).metadata().unwrap();
        let metadata_c = Path::new(c).metadata().unwrap();
        if metadata_a.ctime() > metadata_c.ctime() {
            break;
        }
    }

    // a's ctime is newer than c's
    ls_test(&["-c", a, c], &format!("{a}\n{c}\n"), "", 0);

    // Change c's ctime to be after a's.
    // The original is using `ln` for this but `std::os::unix::fs::symlink`
    // doesn't seem to modify the ctime so we're changing the file time instead.
    loop {
        thread::sleep(Duration::from_millis(100));
        change_file_time(c, TimeToChange::Accessed(u3));
        let metadata_a = Path::new(a).metadata().unwrap();
        let metadata_c = Path::new(c).metadata().unwrap();
        if metadata_c.ctime() > metadata_a.ctime() {
            break;
        }
    }

    ls_test_with_checker(&["-l", a], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.find("Jan 15  1998").is_some());
        assert_eq!(output.status.code(), Some(0));
    });

    ls_test_with_checker(&["-lu", a], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.find("Jan 14  1998").is_some());
        assert_eq!(output.status.code(), Some(0));
    });

    ls_test(&["-ut", a, b, c], &format!("{c}\n{b}\n{a}\n"), "", 0);
    ls_test(&["-t", a, b, c], &format!("{a}\n{b}\n{c}\n"), "", 0);

    // c's ctime is newer than a's
    ls_test(&["-ct", a, c], &format!("{c}\n{a}\n"), "", 0);

    fs::remove_dir_all(test_dir).unwrap();
}

// `ls -l` on a symbolic link — named directly on the command line, and present
// inside a listed directory — must not panic; it renders `name -> target`.
#[test]
fn test_ls_l_symlink_no_panic() {
    let test_dir = &format!("{}/test_ls_l_symlink_no_panic", env!("CARGO_TARGET_TMPDIR"));
    let sub = &format!("{test_dir}/sub");
    let target = &format!("{test_dir}/sub/target");
    let link = &format!("{test_dir}/sub/link");

    fs::create_dir_all(sub).unwrap();
    fs::File::create(target).unwrap();
    std::os::unix::fs::symlink("target", link).unwrap();

    // #LS2: symlink discovered while listing the directory.
    ls_test_with_checker(&["-l", sub], |_, output| {
        assert_eq!(output.status.code(), Some(0));
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("link -> target"), "stdout: {stdout}");
    });

    // #LS1: symlink named directly on the command line.
    ls_test_with_checker(&["-l", link], |_, output| {
        assert_eq!(output.status.code(), Some(0));
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("link -> target"), "stdout: {stdout}");
    });

    fs::remove_dir_all(test_dir).unwrap();
}

// A socket gets type char `s` (long format) and indicator `=` (-F).
#[test]
fn test_ls_socket_classification() {
    // A socket path must fit in sun_path (104 bytes on macOS, 108 on Linux), which a target
    // directory deep in a checkout can exceed; the system temporary directory is short.
    let test_dir = plib::tmp::tempdir().unwrap();
    let sock = &test_dir.path().join("sock").to_str().unwrap().to_string();
    let _listener = std::os::unix::net::UnixListener::bind(sock).unwrap();

    ls_test_with_checker(&["-F", sock], |_, output| {
        assert_eq!(output.status.code(), Some(0));
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("sock="), "stdout: {stdout}");
    });
    ls_test_with_checker(&["-l", sock], |_, output| {
        assert_eq!(output.status.code(), Some(0));
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.starts_with('s'),
            "mode line should start with 's': {stdout}"
        );
    });
}

// The long-format day is blank-padded (`%e`), not zero-padded (`Jun  5`, not `Jun 05`).
#[test]
fn test_ls_date_blank_padded_day() {
    let test_dir = &format!(
        "{}/test_ls_date_blank_padded_day",
        env!("CARGO_TARGET_TMPDIR")
    );
    let f = &format!("{test_dir}/f");
    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();
    // An old (year-format) date on a single-digit day; noon UTC keeps the day stable across TZs.
    change_file_time(f, TimeToChange::Both("2020-06-05 12:00:00"));

    ls_test_with_checker(&["-l", f], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("2020"), "expected year format: {stdout}");
        assert!(
            (stdout.contains("Jun  5") || stdout.contains("Jun  6")),
            "day should be blank-padded: {stdout}"
        );
        assert!(
            !stdout.contains("Jun 05") && !stdout.contains("Jun 06"),
            "day should not be zero-padded: {stdout}"
        );
    });
    fs::remove_dir_all(test_dir).unwrap();
}

// The recent-vs-old date format tracks the DISPLAYED timestamp — under `-u`, a file
// with an old mtime but a recent atime uses the recent (HH:MM) format, not the year format.
#[test]
fn test_ls_u_recency_displayed_time() {
    let test_dir = &format!(
        "{}/test_ls_u_recency_displayed_time",
        env!("CARGO_TARGET_TMPDIR")
    );
    let f = &format!("{test_dir}/f");
    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();

    let recent = (chrono::Local::now() - chrono::Duration::days(2))
        .format("%Y-%m-%d %H:%M:%S")
        .to_string();
    change_file_time(f, TimeToChange::Modified("2020-01-05 12:00:00")); // old mtime
    change_file_time(f, TimeToChange::Accessed(&recent)); // recent atime (mtime preserved)

    // -l uses mtime (old) → year format.
    ls_test_with_checker(&["-l", f], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("2020"),
            "-l should show the old year: {stdout}"
        );
    });
    // -lu uses atime (recent) → time-of-day format, no year.
    ls_test_with_checker(&["-lu", f], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            Regex::new(r"\d\d:\d\d").unwrap().is_match(&stdout),
            "-lu should show HH:MM for a recent atime: {stdout}"
        );
        assert!(
            !stdout.contains("2020"),
            "-lu recent atime should not show a year: {stdout}"
        );
    });
    fs::remove_dir_all(test_dir).unwrap();
}

// A file carrying a POSIX ACL gets the trailing `+` on its mode string. Gated on
// `setfacl` being available and the filesystem supporting ACLs.
#[test]
fn test_ls_acl_plus_flag() {
    let test_dir = &format!("{}/test_ls_acl_plus_flag", env!("CARGO_TARGET_TMPDIR"));
    let f = &format!("{test_dir}/f");
    fs::create_dir(test_dir).unwrap();
    fs::File::create(f).unwrap();

    let ok = std::process::Command::new("setfacl")
        .args(["-m", "u:0:r", f])
        .status()
        .map(|s| s.success())
        .unwrap_or(false);
    if !ok {
        eprintln!("Skipping: setfacl unavailable or filesystem lacks ACL support");
        fs::remove_dir_all(test_dir).unwrap();
        return;
    }

    ls_test_with_checker(&["-l", f], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let mode = stdout.split_whitespace().next().unwrap_or("");
        assert!(mode.ends_with('+'), "mode should carry ACL '+': {stdout}");
    });
    fs::remove_dir_all(test_dir).unwrap();
}

// Explicit `-q` replaces non-printable filename characters with `?`.
#[test]
fn test_ls_q_non_printable() {
    let test_dir = &format!("{}/test_ls_q_non_printable", env!("CARGO_TARGET_TMPDIR"));
    fs::create_dir(test_dir).unwrap();
    let weird = &format!("{test_dir}/a\tb");
    fs::File::create(weird).unwrap();

    ls_test_with_checker(&["-q", test_dir], |_, output| {
        assert_eq!(output.status.code(), Some(0));
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("a?b"), "tab should become '?': {stdout:?}");
        assert!(!stdout.contains('\t'), "no raw tab: {stdout:?}");
    });
    fs::remove_dir_all(test_dir).unwrap();
}

// `-s` reports blocks in 1024-byte units by default (matches coreutils).
#[test]
fn test_ls_s_kib_default() {
    let test_dir = &format!("{}/test_ls_s_kib_default", env!("CARGO_TARGET_TMPDIR"));
    let f = &format!("{test_dir}/f");
    fs::create_dir(test_dir).unwrap();
    // 8 KiB of data → ~8 1024-byte blocks (allocation may add a little).
    fs::write(f, vec![0u8; 8192]).unwrap();

    let blocks_512 = fs::metadata(f).unwrap().blocks();
    let expected_kib = blocks_512 / 2;
    ls_test_with_checker(&["-s", f], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let first: u64 = stdout.split_whitespace().next().unwrap().parse().unwrap();
        assert_eq!(first, expected_kib, "expected 1024-byte units: {stdout}");
    });
    fs::remove_dir_all(test_dir).unwrap();
}

// A diagnostic must name the file it is about. `main` printed `ls: {e}` on an
// error that never carried the operand, so with several operands the user
// could not tell which one failed.
#[test]
fn ls_names_the_file_it_could_not_read() {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_ls"))
        .arg("/nonexistent_ls_probe")
        .output()
        .expect("run ls");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_ne!(out.status.code(), Some(0));
    assert!(stderr.starts_with("ls: "), "must name ls: {stderr:?}");
    assert!(
        stderr.contains("/nonexistent_ls_probe"),
        "must name the file: {stderr:?}"
    );
}

#[test]
fn ls_reports_a_bad_operand_and_still_lists_the_good_one() {
    let dir = plib::tmp::TempDir::new().unwrap();
    std::fs::write(dir.path().join("present.txt"), b"x").unwrap();
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_ls"))
        .arg("/nonexistent_ls_probe")
        .arg(dir.path())
        .output()
        .expect("run ls");
    let stderr = String::from_utf8_lossy(&out.stderr);
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stderr.contains("/nonexistent_ls_probe"),
        "the failing operand must be named: {stderr:?}"
    );
    assert!(
        stdout.contains("present.txt"),
        "the good operand must still be listed: {stdout:?}"
    );
}

/// `ls dir | head` must die by SIGPIPE, not panic with exit 101.
///
/// Rust ignores SIGPIPE before `main`, so every utility in the tree had this
/// gap until `plib::diag::init_locale` started restoring the default. `ls` is
/// the one a user hits first.
///
/// The listing is a fixture, not a system directory: `ls -R /usr` writes only
/// about 29 KB on some hosts -- less than a pipe holds, so nothing races -- and
/// on this one it also exits 2 with "not listing already-listed directory" on
/// stderr, which is the opposite of what the helper asserts.
#[test]
fn test_ls_dies_by_sigpipe_on_a_closed_pipe() {
    let dir = plib::tmp::tempdir().unwrap();
    // Long names, so the output exceeds one pipe buffer without needing tens of
    // thousands of files.
    for n in 0..4_000 {
        std::fs::write(
            dir.path().join(format!("entry-with-a-long-name-{n:06}")),
            "",
        )
        .unwrap();
    }

    plib::testing::assert_dies_by_sigpipe("ls", &["-1", dir.path().to_str().unwrap()]);
}

/// The `-l` mode string shows the set-user-ID, set-group-ID and restricted
/// deletion bits in the owner, group and others execute positions (XCU ls,
/// STDOUT: `s`/`S`, `t`/`T`). The group position only ever showed `x` or
/// `-`, so a set-group-ID file or directory was indistinguishable from one
/// without the bit.
#[test]
fn test_ls_l_mode_string_special_bits() {
    use std::os::unix::fs::PermissionsExt;

    let dir = plib::tmp::tempdir().unwrap();
    let cases: &[(&str, bool, u32, &str)] = &[
        ("sgid_x", false, 0o2754, "-rwxr-sr--"),
        ("sgid_nox", false, 0o2744, "-rwxr-Sr--"),
        ("suid_x", false, 0o4755, "-rwsr-xr-x"),
        ("suid_nox", false, 0o4644, "-rwSr--r--"),
        ("all_x", false, 0o6755, "-rwsr-sr-x"),
        ("dir_sgid", true, 0o2775, "drwxrwsr-x"),
        ("dir_sgid_nox", true, 0o2705, "drwx--Sr-x"),
        ("dir_sticky", true, 0o1777, "drwxrwxrwt"),
        ("dir_sticky_nox", true, 0o1770, "drwxrwx--T"),
    ];
    for &(name, is_dir, mode, _) in cases {
        let path = dir.path().join(name);
        if is_dir {
            fs::create_dir(&path).unwrap();
        } else {
            fs::File::create(&path).unwrap();
        }
        fs::set_permissions(&path, fs::Permissions::from_mode(mode)).unwrap();
    }

    for &(name, _, mode, expected) in cases {
        let path = dir.path().join(name);
        // chmod may drop set-group-ID when the file's group is not one of
        // ours; compare against what the file actually got.
        let actual_mode = fs::symlink_metadata(&path).unwrap().mode() & 0o7777;
        if actual_mode != mode {
            eprintln!("Skipping {name}: mode {actual_mode:o}, wanted {mode:o}");
            continue;
        }
        ls_test_with_checker(&["-ld", path.to_str().unwrap()], |_, output| {
            assert_eq!(output.status.code(), Some(0));
            let stdout = String::from_utf8_lossy(&output.stdout);
            let mode = stdout.split_whitespace().next().unwrap_or("");
            assert_eq!(mode.trim_end_matches('+'), expected, "{name}: {stdout}");
        });
    }
}

/// The `+` alternate-access flag describes the file the line is about. For a
/// symbolic link listed as itself that is the link, which carries no ACL on
/// Linux; the probe followed the link and reported the target's ACL. When
/// `-L` (or `-H` for an operand) makes the line describe the target, the
/// target's `+` is the right answer. Gated like `test_ls_acl_plus_flag`.
#[test]
fn test_ls_acl_plus_flag_describes_the_link_itself() {
    let dir = plib::tmp::tempdir().unwrap();
    let target = dir.path().join("target");
    let link = dir.path().join("link");
    fs::File::create(&target).unwrap();
    std::os::unix::fs::symlink("target", &link).unwrap();
    let (target, link) = (target.to_str().unwrap(), link.to_str().unwrap());

    let ok = std::process::Command::new("setfacl")
        .args(["-m", "u:0:r", target])
        .status()
        .map(|s| s.success())
        .unwrap_or(false);
    if !ok {
        eprintln!("Skipping: setfacl unavailable or filesystem lacks ACL support");
        return;
    }

    let mode_of = |output: &std::process::Output| -> String {
        assert_eq!(output.status.code(), Some(0));
        let stdout = String::from_utf8_lossy(&output.stdout);
        stdout.split_whitespace().next().unwrap_or("").to_string()
    };

    // The link itself: operand, and an entry found inside a directory.
    ls_test_with_checker(&["-l", link], |_, output| {
        assert_eq!(mode_of(output), "lrwxrwxrwx");
    });
    ls_test_with_checker(&["-l", dir.path().to_str().unwrap()], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let line = stdout.lines().find(|l| l.contains("link -> ")).unwrap();
        assert!(line.starts_with("lrwxrwxrwx "), "got {stdout:?}");
    });

    // Followed: the line describes the target, so it carries the target's `+`.
    for args in [["-lL", link], ["-lH", link]] {
        ls_test_with_checker(&args, |_, output| {
            let mode = mode_of(output);
            assert!(
                mode.starts_with('-') && mode.ends_with('+'),
                "{args:?}: {mode}"
            );
        });
    }
    ls_test_with_checker(&["-lL", dir.path().to_str().unwrap()], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        for line in stdout.lines().skip(1) {
            let mode = line.split_whitespace().next().unwrap();
            assert!(mode.starts_with('-') && mode.ends_with('+'), "{stdout:?}");
        }
    });
}

/// Started with SIGPIPE ignored, `ls` into a closed pipe reports the write
/// error and exits nonzero, rather than dying by the ignored signal or
/// panicking in `println!`.
/// See `plib::testing::assert_epipe_when_sigpipe_ignored`.
#[test]
fn test_ls_reports_epipe_when_sigpipe_is_ignored() {
    let dir = plib::tmp::tempdir().unwrap();
    fs::File::create(dir.path().join("f")).unwrap();

    plib::testing::assert_epipe_when_sigpipe_ignored("ls", &[dir.path().to_str().unwrap()], 1);
}
