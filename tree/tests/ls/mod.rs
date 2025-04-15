//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{TestPlan, run_test, run_test_with_checker};
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
        expected_exit_code: expected_exit_code,
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
        "ls: No such file or directory (os error 2)\n",
        1,
    );
    ls_test(
        &["-H", dangle],
        "",
        "ls: No such file or directory (os error 2)\n",
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
        if ret != 0 {
            if get_errno() != libc::EEXIST {
                panic!("{}", io::Error::last_os_error());
            }
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

    ls_test(
        &["-RL", loop_sub],
        &format!("{loop_sub}:\nsub\n"),
        &format!("ls: {loop_sub}: not listing already-listed directory\n"),
        2,
    );

    fs::remove_dir_all(test_dir).unwrap();
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
    cd_and_ls_test(test_dir, &["-m", "a", "b"], &format!("a, b\n"));

    // -k for 1024-byte block sizes which is the default for coreutils. Default
    // for this implementation is 512-byte blocks as mentioned in the STDOUT
    // section of:
    // https://pubs.opengroup.org/onlinepubs/9699919799/utilities/ls.html
    cd_and_ls_test(test_dir, &["-smk", "a", "b"], &format!("0 a, 12 b\n"));

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
    // TODO: Audit that the environment access only happens in single-threaded code.
    unsafe { std::env::set_var("TZ", "UTC0") };

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
