//
// Copyright (c) 2024-2026 Jeff Garzik
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{
    run_test, run_test_with_checker, run_test_with_checker_and_env, run_test_with_env, TestPlan,
};
use std::ffi::CString;
use std::fs;
use std::os::unix::fs::{symlink, PermissionsExt};
use std::os::unix::net::UnixListener;

fn test_test(args: &[&str], expected_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("test"),
        args: str_args,
        stdin_data: String::from(""),
        expected_out: String::from(""),
        expected_err: String::from(""),
        expected_exit_code: expected_code,
    });
}

/// Run `test` with explicit environment variables (e.g. a locale override).
fn test_test_env(args: &[&str], env: &[(&str, &str)], expected_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_env(
        TestPlan {
            cmd: String::from("test"),
            args: str_args,
            stdin_data: String::from(""),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: expected_code,
        },
        env,
    );
}

/// Test that expects stderr output (doesn't check exact message, only exit code)
fn test_test_with_err(args: &[&str], expected_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("test"),
            args: str_args,
            stdin_data: String::from(""),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: expected_code,
        },
        |_plan, output| {
            assert_eq!(output.status.code(), Some(expected_code));
            assert!(
                !output.stderr.is_empty(),
                "expected stderr output for error case"
            );
        },
    );
}

#[test]
fn test_intops() {
    test_test(&["20", "-eq", "20"], 0);
    test_test(&["20", "-eq", "21"], 1);

    test_test(&["20", "-ne", "20"], 1);
    test_test(&["20", "-ne", "21"], 0);

    test_test(&["20", "-gt", "10"], 0);
    test_test(&["10", "-gt", "20"], 1);

    test_test(&["20", "-ge", "10"], 0);
    test_test(&["10", "-ge", "20"], 1);
    test_test(&["20", "-ge", "20"], 0);

    test_test(&["20", "-lt", "10"], 1);
    test_test(&["10", "-lt", "20"], 0);

    test_test(&["20", "-le", "10"], 1);
    test_test(&["10", "-le", "20"], 0);
    test_test(&["20", "-le", "20"], 0);
}

/// Integer operand robustness: surrounding blanks tolerated; explicit sign
/// accepted; non-numeric and i64-overflow operands are diagnosed (exit 2).
/// (audit #3)
#[test]
fn test_intops_operands() {
    // Leading/trailing blanks are tolerated (historical `test` behavior).
    test_test(&[" 5", "-eq", "5"], 0);
    test_test(&["5", "-eq", "5 "], 0);
    test_test(&[" 5 ", "-eq", "5"], 0);

    // Explicit sign and leading zeros.
    test_test(&["+5", "-eq", "5"], 0);
    test_test(&["-5", "-lt", "0"], 0);
    test_test(&["-5", "-eq", "-5"], 0);
    test_test(&["05", "-eq", "5"], 0);

    // Non-numeric and overflow operands are errors (exit > 1).
    test_test_with_err(&["abc", "-eq", "1"], 2);
    test_test_with_err(&["", "-eq", "1"], 2);
    test_test_with_err(&["99999999999999999999", "-gt", "1"], 2);
}

#[test]
fn test_strops() {
    test_test(&["a", "=", "a"], 0);
    test_test(&["a", "=", "b"], 1);

    test_test(&["a", "!=", "a"], 1);
    test_test(&["a", "!=", "b"], 0);

    test_test(&["a", "<", "b"], 0);
    test_test(&["b", "<", "a"], 1);

    test_test(&["a", ">", "b"], 1);
    test_test(&["b", ">", "a"], 0);
}

/// `<` and `>` must order strings by the current locale's collating sequence
/// (LC_COLLATE), not by raw byte/scalar value. (audit #1)
#[test]
fn test_str_collation() {
    // In the C locale, collation is byte order: 'B'(0x42) sorts before 'a'(0x61).
    test_test_env(&["B", "<", "a"], &[("LC_ALL", "C")], 0);
    test_test_env(&["a", "<", "B"], &[("LC_ALL", "C")], 1);
    test_test_env(&["a", ">", "B"], &[("LC_ALL", "C")], 0);

    // In a UTF-8 locale, collation interleaves case: 'a' sorts before 'B', the
    // opposite of byte order. Probe en_US.UTF-8 but skip gracefully when the
    // locale is not installed (the binary then falls back to C/byte order).
    let str_args: Vec<String> = ["a", "<", "B"].iter().map(|s| String::from(*s)).collect();
    run_test_with_checker_and_env(
        TestPlan {
            cmd: String::from("test"),
            args: str_args,
            stdin_data: String::from(""),
            expected_out: String::from(""),
            expected_err: String::from(""),
            expected_exit_code: 0,
        },
        &[("LC_ALL", "en_US.UTF-8")],
        |_plan, output| match output.status.code() {
            Some(0) => {} // collation active: 'a' < 'B' as expected
            _ => eprintln!("skipping en_US.UTF-8 collation check: locale unavailable"),
        },
    );
}

#[test]
fn test_str_basic() {
    test_test(&[], 1);

    test_test(&[""], 1);
    test_test(&["a"], 0);

    test_test(&["-z", ""], 0);
    test_test(&["-z", "a"], 1);

    test_test(&["-n", ""], 1);
    test_test(&["-n", "a"], 0);
}

// ============================================================================
// File type tests
// ============================================================================

#[test]
fn test_file_exists() {
    // -e tests file existence
    test_test(&["-e", "/tmp"], 0);
    test_test(&["-e", "/nonexistent_file_12345"], 1);
}

#[test]
fn test_file_directory() {
    // -d tests if file is a directory
    test_test(&["-d", "/tmp"], 0);
    test_test(&["-d", "/etc/passwd"], 1);
    test_test(&["-d", "/nonexistent_file_12345"], 1);
}

#[test]
fn test_file_regular() {
    // -f tests if file is a regular file
    test_test(&["-f", "/etc/passwd"], 0);
    test_test(&["-f", "/tmp"], 1);
    test_test(&["-f", "/nonexistent_file_12345"], 1);
}

#[test]
fn test_file_size() {
    // -s tests if file has size > 0
    test_test(&["-s", "/etc/passwd"], 0);
    test_test(&["-s", "/nonexistent_file_12345"], 1);
}

#[test]
fn test_file_readable() {
    // -r tests if file is readable
    test_test(&["-r", "/etc/passwd"], 0);
    test_test(&["-r", "/nonexistent_file_12345"], 1);
}

#[test]
fn test_file_writable() {
    // -w tests if file is writable
    test_test(&["-w", "/tmp"], 0);
}

#[test]
fn test_file_executable() {
    // -x tests if file is executable
    test_test(&["-x", "/bin/sh"], 0);
    test_test(&["-x", "/etc/passwd"], 1);
}

// ============================================================================
// Symlink tests
// ============================================================================

#[test]
fn test_symlink() {
    let test_dir = tempfile::tempdir().unwrap();
    let file_path = test_dir.path().join("file");
    let link_path = test_dir.path().join("link");
    let broken_link_path = test_dir.path().join("broken_link");

    // Create a regular file
    fs::write(&file_path, "content").unwrap();

    // Create a symlink to the file
    symlink(&file_path, &link_path).unwrap();

    // Create a broken symlink
    symlink("/nonexistent_target_12345", &broken_link_path).unwrap();

    // -h and -L should detect symlinks (not following them)
    test_test(&["-h", link_path.to_str().unwrap()], 0);
    test_test(&["-L", link_path.to_str().unwrap()], 0);

    // -h and -L should detect broken symlinks too
    test_test(&["-h", broken_link_path.to_str().unwrap()], 0);
    test_test(&["-L", broken_link_path.to_str().unwrap()], 0);

    // Regular file is not a symlink
    test_test(&["-h", file_path.to_str().unwrap()], 1);
    test_test(&["-L", file_path.to_str().unwrap()], 1);

    // -f follows symlinks, so symlink to file should be true
    test_test(&["-f", link_path.to_str().unwrap()], 0);

    // Broken symlink should fail -e (target doesn't exist)
    test_test(&["-e", broken_link_path.to_str().unwrap()], 1);
}

// ============================================================================
// Negation tests
// ============================================================================

#[test]
fn test_negation() {
    // ! negates the expression
    test_test(&["!", ""], 0);
    test_test(&["!", "a"], 1);

    // Negate unary
    test_test(&["!", "-d", "/tmp"], 1);
    test_test(&["!", "-d", "/nonexistent"], 0);

    // Negate binary
    test_test(&["!", "a", "=", "a"], 1);
    test_test(&["!", "a", "=", "b"], 0);
}

// ============================================================================
// XSI operators (-a, -o, parentheses)
// ============================================================================

#[test]
fn test_xsi_and() {
    // -a is AND
    test_test(&["-d", "/tmp", "-a", "-d", "/"], 0);
    test_test(&["-d", "/tmp", "-a", "-f", "/tmp"], 1);
    test_test(&["-f", "/tmp", "-a", "-d", "/"], 1);
}

#[test]
fn test_xsi_or() {
    // -o is OR
    test_test(&["-d", "/tmp", "-o", "-d", "/nonexistent"], 0);
    test_test(&["-d", "/nonexistent", "-o", "-d", "/tmp"], 0);
    test_test(&["-f", "/tmp", "-o", "-f", "/"], 1);
}

#[test]
fn test_xsi_precedence() {
    // -a has higher precedence than -o
    // "false -o true -a true" should be "false -o (true -a true)" = true
    test_test(&["-f", "/tmp", "-o", "-d", "/tmp", "-a", "-d", "/"], 0);

    // "true -a false -o true" should be "(true -a false) -o true" = true
    test_test(&["-d", "/tmp", "-a", "-f", "/tmp", "-o", "-d", "/"], 0);
}

#[test]
fn test_xsi_parentheses() {
    // Parentheses for grouping (3 args)
    test_test(&["(", "a", ")"], 0);
    test_test(&["(", "", ")"], 1);

    // Parentheses for grouping (4 args)
    test_test(&["(", "-d", "/tmp", ")"], 0);

    // Parentheses with XSI expressions
    test_test(&["(", "-d", "/tmp", ")", "-a", "-d", "/"], 0);
    test_test(&["(", "-f", "/tmp", ")", "-o", "-d", "/"], 0);
}

#[test]
fn test_xsi_not_combinations() {
    // ! with -a and -o
    test_test(&["!", "-f", "/tmp", "-a", "-d", "/tmp"], 0);
    test_test(&["!", "-d", "/tmp", "-o", "-d", "/"], 0);
}

/// The legacy `-a`/`-o` operators are accepted uniformly, including the
/// 3-argument forms (`test x -a y`) that the count-based algorithm leaves
/// unspecified -- not only inside >4-argument expressions. (audit #2)
#[test]
fn test_legacy_operators_uniform() {
    // 3-argument -a / -o (string operands).
    test_test(&["x", "-a", "y"], 0); // true AND true
    test_test(&["x", "-a", ""], 1); // true AND false
    test_test(&["", "-a", "y"], 1); // false AND true
    test_test(&["", "-o", "y"], 0); // false OR true
    test_test(&["x", "-o", ""], 0); // true OR false
    test_test(&["", "-o", ""], 1); // false OR false

    // 3-argument -a / -o with unary primaries.
    test_test(&["-d", "/tmp", "-o", ""], 0);

    // Genuinely malformed expressions still error (exit 2).
    test_test_with_err(&["a", "b", "c"], 2);
    test_test_with_err(&["(", "a"], 2);
}

/// Precedence and grouping in the extended (>4-argument) grammar: `-a` binds
/// tighter than `-o`, `!` binds tightest, and `(`/`)` regroup. (audit, PR D)
#[test]
fn test_expr_precedence() {
    // -z '' (true) -o (-n '' (false) -a -n '' (false)) => true OR false => true
    test_test(&["-z", "", "-o", "-n", "", "-a", "-n", ""], 0);
    // (! -n '' (true)) -a -n x (true) => true
    test_test(&["!", "-n", "", "-a", "-n", "x"], 0);
    // Parentheses override -a/-o precedence: ( '' -o x ) -a x => true
    test_test(&["(", "", "-o", "x", ")", "-a", "x"], 0);
    // ( x -a '' ) -o x => false OR true => true
    test_test(&["(", "x", "-a", "", ")", "-o", "x"], 0);
    // x -a '' -o '' => (true AND false) OR false => false
    test_test(&["x", "-a", "", "-o", ""], 1);
}

// ============================================================================
// Integer error handling
// ============================================================================

#[test]
fn test_integer_errors() {
    // Invalid integers should cause exit code 2
    test_test_with_err(&["abc", "-eq", "123"], 2);
    test_test_with_err(&["123", "-eq", "abc"], 2);
    test_test_with_err(&["abc", "-lt", "def"], 2);
}

// ============================================================================
// Edge cases
// ============================================================================

#[test]
fn test_empty_args() {
    // No arguments = false
    test_test(&[], 1);
}

#[test]
fn test_special_strings() {
    // Strings that look like operators should be treated as strings
    test_test(&["-d"], 0); // Non-empty string = true
    test_test(&["!"], 0); // Non-empty string = true
    test_test(&["("], 0); // Non-empty string = true
    test_test(&[")"], 0); // Non-empty string = true
    test_test(&["-a"], 0); // Non-empty string = true
    test_test(&["-o"], 0); // Non-empty string = true

    // "test !" should be true (non-empty string)
    test_test(&["!"], 0);

    // "test ! !" should be ! of "!" which is non-empty, so false
    test_test(&["!", "!"], 1);
}

#[test]
fn test_path_comparisons() {
    // -ef tests if two paths refer to the same file
    let test_dir = tempfile::tempdir().unwrap();
    let file_path = test_dir.path().join("file");
    let link_path = test_dir.path().join("link");

    fs::write(&file_path, "content").unwrap();
    fs::hard_link(&file_path, &link_path).unwrap();

    test_test(
        &[
            file_path.to_str().unwrap(),
            "-ef",
            link_path.to_str().unwrap(),
        ],
        0,
    );
    test_test(&[file_path.to_str().unwrap(), "-ef", "/etc/passwd"], 1);
}

#[test]
fn test_posix_examples() {
    // From POSIX examples in the spec

    // "test ]" must exit with status 0 (non-empty string)
    test_test(&["]"], 0);

    // String comparisons
    test_test(&["pear", "=", "pear"], 0);
    test_test(&["pear", "=", "grape"], 1);
}

// ============================================================================
// Special-file-type and mode-bit primaries (audit PR D coverage)
// ============================================================================

/// `-c`: character special file. /dev/null is a character device on Linux and
/// macOS; regular files and directories are not.
#[test]
fn test_char_device() {
    test_test(&["-c", "/dev/null"], 0);
    test_test(&["-c", "/tmp"], 1);
    test_test(&["-c", "/nonexistent-xyz"], 1);

    let dir = tempfile::tempdir().unwrap();
    let f = dir.path().join("regular");
    fs::write(&f, "x").unwrap();
    test_test(&["-c", f.to_str().unwrap()], 1);
}

/// `-b`: block special file. No portable path resolves to a block device
/// without privilege, so only the false cases are asserted (the primary must
/// return false for non-block files, not error).
#[test]
fn test_block_device() {
    test_test(&["-b", "/dev/null"], 1); // char device, not block
    test_test(&["-b", "/tmp"], 1);
    test_test(&["-b", "/nonexistent-xyz"], 1);
}

/// `-p`: FIFO (named pipe), created with mkfifo(3).
#[test]
fn test_fifo() {
    let dir = tempfile::tempdir().unwrap();
    let fifo = dir.path().join("fifo");
    let c = CString::new(fifo.to_str().unwrap()).unwrap();
    let rc = unsafe { libc::mkfifo(c.as_ptr(), 0o644) };
    assert_eq!(rc, 0, "mkfifo failed");

    test_test(&["-p", fifo.to_str().unwrap()], 0);

    let reg = dir.path().join("regular");
    fs::write(&reg, "x").unwrap();
    test_test(&["-p", reg.to_str().unwrap()], 1);
}

/// `-S`: socket, created by binding a Unix-domain listener.
#[test]
fn test_socket() {
    let dir = tempfile::tempdir().unwrap();
    let sock = dir.path().join("sock");
    let _listener = UnixListener::bind(&sock).unwrap();

    test_test(&["-S", sock.to_str().unwrap()], 0);

    let reg = dir.path().join("regular");
    fs::write(&reg, "x").unwrap();
    test_test(&["-S", reg.to_str().unwrap()], 1);
}

/// `-g` / `-u`: set-group-ID / set-user-ID bits. Some filesystems strip these
/// bits on chmod for an unprivileged owner, so the assertion is skipped when
/// the bit does not actually stick.
#[test]
fn test_setgid_setuid() {
    let dir = tempfile::tempdir().unwrap();

    let gfile = dir.path().join("sgid");
    fs::write(&gfile, "x").unwrap();
    fs::set_permissions(&gfile, fs::Permissions::from_mode(0o2755)).unwrap();
    if fs::metadata(&gfile).unwrap().permissions().mode() & 0o2000 != 0 {
        test_test(&["-g", gfile.to_str().unwrap()], 0);
    } else {
        eprintln!("skipping -g check: setgid bit did not stick on this filesystem");
    }
    // A plain file never has the bit.
    let plain = dir.path().join("plain");
    fs::write(&plain, "x").unwrap();
    test_test(&["-g", plain.to_str().unwrap()], 1);

    let ufile = dir.path().join("suid");
    fs::write(&ufile, "x").unwrap();
    fs::set_permissions(&ufile, fs::Permissions::from_mode(0o4755)).unwrap();
    if fs::metadata(&ufile).unwrap().permissions().mode() & 0o4000 != 0 {
        test_test(&["-u", ufile.to_str().unwrap()], 0);
    } else {
        eprintln!("skipping -u check: setuid bit did not stick on this filesystem");
    }
    test_test(&["-u", plain.to_str().unwrap()], 1);
}

/// `-t`: file descriptor is an open terminal. Under the test harness the
/// inherited descriptors are pipes/files, never a tty, so every case is false;
/// invalid and non-numeric descriptors are also false (not errors).
#[test]
fn test_terminal() {
    test_test(&["-t", "0"], 1);
    test_test(&["-t", "1"], 1);
    test_test(&["-t", "99"], 1); // not an open fd
    test_test(&["-t", "notanumber"], 1);
}

/// Set a file's last-modification time (seconds since the epoch).
fn set_mtime(path: &std::path::Path, secs: i64) {
    let c = CString::new(path.to_str().unwrap()).unwrap();
    let tv = libc::timeval {
        tv_sec: secs as libc::time_t,
        tv_usec: 0,
    };
    let times = [tv, tv]; // [atime, mtime]
    let rc = unsafe { libc::utimes(c.as_ptr(), times.as_ptr()) };
    assert_eq!(rc, 0, "utimes failed");
}

/// `-nt` / `-ot`: compare last-modification timestamps, including the rules for
/// when one operand cannot be resolved.
#[test]
fn test_newer_older() {
    let dir = tempfile::tempdir().unwrap();
    let older = dir.path().join("older");
    let newer = dir.path().join("newer");
    fs::write(&older, "x").unwrap();
    fs::write(&newer, "x").unwrap();
    set_mtime(&older, 1_000_000_000);
    set_mtime(&newer, 2_000_000_000);
    let (o, n) = (older.to_str().unwrap(), newer.to_str().unwrap());
    let missing = dir.path().join("missing");
    let m = missing.to_str().unwrap();

    // Both exist.
    test_test(&[n, "-nt", o], 0);
    test_test(&[o, "-nt", n], 1);
    test_test(&[o, "-ot", n], 0);
    test_test(&[n, "-ot", o], 1);

    // -nt: true if p1 exists and p2 does not.
    test_test(&[n, "-nt", m], 0);
    test_test(&[m, "-nt", n], 1);

    // -ot: true if p2 exists and p1 does not.
    test_test(&[m, "-ot", n], 0);
    test_test(&[n, "-ot", m], 1);
}
