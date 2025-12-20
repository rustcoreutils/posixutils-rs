//
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, run_test_with_checker, TestPlan};
use std::{fs, io::Write, os::unix::fs::MetadataExt, process::Output};

fn du_test(args: &[&str], expected_output: &str, expected_error: &str, expected_exit_code: i32) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("du"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

fn du_test_with_checker(args: &[&str], checker: impl FnMut(&TestPlan, &Output)) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("du"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        checker,
    );
}

fn du_test_with_checker_and_exit(
    args: &[&str],
    expected_exit_code: i32,
    checker: impl FnMut(&TestPlan, &Output),
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test_with_checker(
        TestPlan {
            cmd: String::from("du"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code,
        },
        checker,
    );
}

// Helper to sort output lines for comparison (order may vary)
fn sort_lines(s: &str) -> Vec<String> {
    let mut lines: Vec<String> = s.lines().map(|s| s.to_string()).collect();
    lines.sort();
    lines
}

// Partial port of coreutils/tests/du/basic.sh
// Omits the nonstandard --block-size and -S options
#[test]
fn test_du_basic() {
    let test_dir = &format!("{}/test_du_basic", env!("CARGO_TARGET_TMPDIR"));

    let _ = fs::remove_dir_all(test_dir);

    let a = &format!("{test_dir}/a");
    let a_b = &format!("{test_dir}/a/b");
    let d = &format!("{test_dir}/d");
    let d_sub = &format!("{test_dir}/d/sub");

    let a_b_f = &format!("{a_b}/F");
    let d_1 = &format!("{d}/1");
    let d_sub_2 = &format!("{d_sub}/2");

    fs::create_dir(test_dir).unwrap();
    for dir in [a_b, d, d_sub] {
        fs::create_dir_all(dir).unwrap();
    }

    {
        // Create a > 257 bytes file
        let mut file1 = fs::File::create(a_b_f).unwrap();
        write!(file1, "{:>257}", "x").unwrap();

        // Create a 4 KiB file
        let mut file2 = fs::File::create(d_1).unwrap();
        write!(file2, "{:>4096}", "x").unwrap();

        fs::copy(d_1, d_sub_2).unwrap();
    }

    // Manually calculate the block sizes for directory a
    let [size_abf, mut size_ab, mut size_a] =
        [a_b_f, a_b, a].map(|filename| fs::metadata(filename).unwrap().blocks());
    size_ab += size_abf;
    size_a += size_ab;

    // Test -a: should print files AND directories
    du_test(
        &["-a", a],
        &format!(
            "{size_abf}\t{a_b_f}\
            \n{size_ab}\t{a_b}\
            \n{size_a}\t{a}\n"
        ),
        "",
        0,
    );

    // Test -s: should only print the total
    du_test(&["-s", a], &format!("{size_a}\t{a}\n"), "", 0);

    // Manually calculate the block sizes for directory d
    let [size_dsub2, mut size_dsub, size_d1, mut size_d] =
        [d_sub_2, d_sub, d_1, d].map(|filename| fs::metadata(filename).unwrap().blocks());
    size_dsub += size_dsub2;
    size_d += size_d1 + size_dsub;

    du_test_with_checker(&["-a", d], |_, output| {
        // Order of d/1 vs d/sub is not guaranteed, so compare sorted lines
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        let expected = format!(
            "{size_dsub2}\t{d_sub_2}\
            \n{size_dsub}\t{d_sub}\
            \n{size_d1}\t{d_1}\
            \n{size_d}\t{d}\n"
        );
        assert_eq!(sort_lines(&stdout), sort_lines(&expected));
    });

    fs::remove_dir_all(test_dir).unwrap();
}

// Test default behavior (no -a): should only print directories
#[test]
fn test_du_default_no_files() {
    let test_dir = &format!("{}/test_du_default", env!("CARGO_TARGET_TMPDIR"));

    let _ = fs::remove_dir_all(test_dir);

    let subdir = &format!("{test_dir}/subdir");
    let file1 = &format!("{test_dir}/file1");
    let file2 = &format!("{subdir}/file2");

    fs::create_dir_all(subdir).unwrap();
    fs::write(file1, "hello").unwrap();
    fs::write(file2, "world").unwrap();

    // Without -a, should only print directories (subdir and test_dir)
    du_test_with_checker(&[test_dir], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        // Should NOT contain file1 or file2
        assert!(
            !stdout.contains("file1"),
            "Default du should not list files"
        );
        assert!(
            !stdout.contains("file2"),
            "Default du should not list files"
        );
        // Should contain subdir and test_dir
        assert!(stdout.contains("subdir"), "Default du should list subdir");
        assert!(
            stdout.contains("test_du_default\n"),
            "Default du should list root"
        );
    });

    fs::remove_dir_all(test_dir).unwrap();
}

// Test -k option (1024-byte units)
#[test]
fn test_du_kilo() {
    let test_dir = &format!("{}/test_du_kilo", env!("CARGO_TARGET_TMPDIR"));

    let _ = fs::remove_dir_all(test_dir);
    fs::create_dir(test_dir).unwrap();

    let file1 = &format!("{test_dir}/file1");
    fs::write(file1, "x".repeat(2048)).unwrap();

    let blocks = fs::metadata(file1).unwrap().blocks();
    let size_512 = blocks;
    let size_1024 = blocks / 2;

    // Without -k: 512-byte units
    du_test_with_checker(&["-a", test_dir], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        assert!(
            stdout.contains(&format!("{size_512}\t")),
            "Expected 512-byte units"
        );
    });

    // With -k: 1024-byte units
    du_test_with_checker(&["-ak", test_dir], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        assert!(
            stdout.contains(&format!("{size_1024}\t")),
            "Expected 1024-byte units"
        );
    });

    fs::remove_dir_all(test_dir).unwrap();
}

// Test error handling for non-existent file
#[test]
fn test_du_nonexistent() {
    let nonexistent = "/tmp/posixutils_du_nonexistent_file_xyz123";
    let _ = fs::remove_file(nonexistent); // Ensure it doesn't exist

    du_test_with_checker_and_exit(&[nonexistent], 1, |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        assert!(
            stderr.contains("du:"),
            "Should print error message for nonexistent file"
        );
        assert_eq!(output.status.code(), Some(1), "Should exit with code 1");
    });
}

// Test non-directory file operand (should always be listed per POSIX BSD behavior)
#[test]
fn test_du_file_operand() {
    let test_dir = &format!("{}/test_du_file_op", env!("CARGO_TARGET_TMPDIR"));

    let _ = fs::remove_dir_all(test_dir);
    fs::create_dir(test_dir).unwrap();

    let file1 = &format!("{test_dir}/file1");
    fs::write(file1, "test content").unwrap();

    let size = fs::metadata(file1).unwrap().blocks();

    // File operand should always be listed (even without -a)
    du_test(&[file1], &format!("{size}\t{file1}\n"), "", 0);

    fs::remove_dir_all(test_dir).unwrap();
}

// Test symlink handling with -H (follow cmdline symlinks only)
#[test]
fn test_du_symlink_h() {
    let test_dir = &format!("{}/test_du_symlink_h", env!("CARGO_TARGET_TMPDIR"));

    let _ = fs::remove_dir_all(test_dir);
    fs::create_dir(test_dir).unwrap();

    let target_dir = &format!("{test_dir}/target");
    let link = &format!("{test_dir}/link");
    let target_file = &format!("{target_dir}/file");

    fs::create_dir(target_dir).unwrap();
    fs::write(target_file, "x".repeat(1024)).unwrap();
    std::os::unix::fs::symlink(target_dir, link).unwrap();

    let target_size =
        fs::metadata(target_dir).unwrap().blocks() + fs::metadata(target_file).unwrap().blocks();

    // With -H, following the symlink on cmdline should report target's size
    du_test_with_checker(&["-sH", link], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        // Should contain the target directory's size, not just symlink size
        let reported_size: u64 = stdout.split('\t').next().unwrap().trim().parse().unwrap();
        assert_eq!(
            reported_size, target_size,
            "With -H, should report target directory size"
        );
    });

    // Without -H, should report symlink's own size (just the link, not followed)
    du_test_with_checker(&["-s", link], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        let reported_size: u64 = stdout.split('\t').next().unwrap().trim().parse().unwrap();
        // Symlink size is typically very small (just the link itself)
        let link_size = fs::symlink_metadata(link).unwrap().blocks();
        assert_eq!(
            reported_size, link_size,
            "Without -H, should report symlink size only"
        );
    });

    fs::remove_dir_all(test_dir).unwrap();
}

// Test symlink handling with -L (follow all symlinks)
#[test]
fn test_du_symlink_l() {
    let test_dir = &format!("{}/test_du_symlink_l", env!("CARGO_TARGET_TMPDIR"));

    let _ = fs::remove_dir_all(test_dir);
    fs::create_dir(test_dir).unwrap();

    let target_file = &format!("{test_dir}/target");
    let subdir = &format!("{test_dir}/subdir");
    let link_in_dir = &format!("{subdir}/link");

    fs::create_dir(subdir).unwrap();
    fs::write(target_file, "x".repeat(2048)).unwrap();
    std::os::unix::fs::symlink(target_file, link_in_dir).unwrap();

    let target_size = fs::metadata(target_file).unwrap().blocks();
    let link_size = fs::symlink_metadata(link_in_dir).unwrap().blocks();

    // With -L, symlinks inside directories should be followed
    du_test_with_checker(&["-aL", subdir], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        // The link should report target file's size
        for line in stdout.lines() {
            if line.contains("link") {
                let reported_size: u64 = line.split('\t').next().unwrap().trim().parse().unwrap();
                assert_eq!(
                    reported_size, target_size,
                    "With -L, should report target file size for symlink"
                );
            }
        }
    });

    // Without -L, symlinks inside directories should NOT be followed
    du_test_with_checker(&["-a", subdir], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        for line in stdout.lines() {
            if line.contains("link") {
                let reported_size: u64 = line.split('\t').next().unwrap().trim().parse().unwrap();
                assert_eq!(
                    reported_size, link_size,
                    "Without -L, should report symlink size only"
                );
            }
        }
    });

    fs::remove_dir_all(test_dir).unwrap();
}

// Test hard link deduplication
#[test]
fn test_du_hardlink() {
    let test_dir = &format!("{}/test_du_hardlink", env!("CARGO_TARGET_TMPDIR"));

    let _ = fs::remove_dir_all(test_dir);
    fs::create_dir(test_dir).unwrap();

    let file1 = &format!("{test_dir}/file1");
    let file2 = &format!("{test_dir}/file2");

    // Create a file and a hard link to it
    fs::write(file1, "x".repeat(4096)).unwrap();
    fs::hard_link(file1, file2).unwrap();

    let file_size = fs::metadata(file1).unwrap().blocks();
    let dir_size = fs::metadata(test_dir).unwrap().blocks();

    // With hard links, the file should only be counted once
    // Total should be: dir_size + file_size (not dir_size + 2*file_size)
    let expected_total = dir_size + file_size;

    du_test_with_checker(&["-s", test_dir], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        let reported_size: u64 = stdout.split('\t').next().unwrap().trim().parse().unwrap();
        assert_eq!(
            reported_size, expected_total,
            "Hard links should only be counted once. Expected {}, got {}",
            expected_total, reported_size
        );
    });

    // With -a, both files should still be listed (but counted only once)
    du_test_with_checker(&["-a", test_dir], |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();
        // Both file1 and file2 should appear in output
        assert!(stdout.contains("file1"), "file1 should be listed with -a");
        assert!(stdout.contains("file2"), "file2 should be listed with -a");
    });

    fs::remove_dir_all(test_dir).unwrap();
}
