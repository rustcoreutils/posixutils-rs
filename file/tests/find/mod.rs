//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs::{remove_file, File};
use std::io::Write;
use std::process::{Command, Stdio};

use plib::testing::{get_binary_path, run_test, run_test_with_checker, TestPlan};

fn run_test_find(
    args: &[&str],
    expected_output: &str,
    expected_error: &str,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();

    run_test(TestPlan {
        cmd: String::from("find"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_output),
        expected_err: String::from(expected_error),
        expected_exit_code,
    });
}

/// Run find and compare outputs after sorting lines (for order-independent comparison)
fn run_test_find_sorted(
    args: &[&str],
    expected_lines: &[&str],
    expected_error: &str,
    expected_exit_code: i32,
) {
    run_test_find_sorted_env(
        args,
        &[],
        expected_lines,
        expected_error,
        expected_exit_code,
    );
}

/// Like [`run_test_find_sorted`] but with extra environment variables for the
/// child process. Used to pin `LC_ALL=C` for tests whose `-name` bracket ranges
/// (`[a-z]`) are otherwise `LC_COLLATE`-sensitive: `find` matches via libc
/// `fnmatch(3)`, and a UTF-8 locale on some libc implementations (notably the
/// BSD libc on macOS) collates uppercase letters into the `a`–`z` range. Pinning
/// the C locale gives the deterministic, portable ASCII semantics these tests
/// document.
fn run_test_find_sorted_env(
    args: &[&str],
    env: &[(&str, &str)],
    expected_lines: &[&str],
    expected_error: &str,
    expected_exit_code: i32,
) {
    let find_path = get_binary_path("find");

    let mut command = Command::new(&find_path);
    command.args(args).stdin(Stdio::null());
    for (key, value) in env {
        command.env(key, value);
    }
    let output = command.output().expect("failed to execute find");

    let actual_stdout = String::from_utf8_lossy(&output.stdout);
    let actual_stderr = String::from_utf8_lossy(&output.stderr);
    let actual_exit_code = output.status.code().unwrap_or(-1);

    // Sort actual output lines
    let mut actual_lines: Vec<&str> = actual_stdout.lines().collect();
    actual_lines.sort();

    // Sort expected lines
    let mut expected_sorted: Vec<&str> = expected_lines.to_vec();
    expected_sorted.sort();

    assert_eq!(
        actual_lines, expected_sorted,
        "stdout mismatch (sorted comparison)"
    );
    assert_eq!(actual_stderr, expected_error, "stderr mismatch");
    assert_eq!(actual_exit_code, expected_exit_code, "exit code mismatch");
}

#[test]
fn find_size_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-size", "+4"];

    // The different result of the command is due to a feature of the linux and macos operating systems,
    // namely the use of different file systems (ext4 on linux and APFS on macos).
    // Therefore, the size of folders differs depending on the operating system.
    #[cfg(not(target_os = "macos"))]
    let expected_output = format!("{}\n{}/file1.txt\n", test_dir, test_dir);

    #[cfg(target_os = "macos")]
    let expected_output = format!("{}/file1.txt\n", test_dir);

    run_test_find(&args, &expected_output, "", 0)
}

#[test]
fn find_name_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-name", "empty_file.txt"];

    let expected_output = format!("{}/empty_file.txt\n", test_dir);

    run_test_find(&args, &expected_output, "", 0)
}

#[test]
fn find_type_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args: [&str; 3] = [&test_dir, "-type", "f"];

    let file1 = format!("{}/empty_file.txt", test_dir);
    let file2 = format!("{}/file with space.txt", test_dir);
    let file3 = format!("{}/file1.txt", test_dir);
    let file4 = format!("{}/rust_file.rs", test_dir);

    run_test_find_sorted(&args, &[&file1, &file2, &file3, &file4], "", 0)
}

#[test]
fn find_mtime_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-mtime", "7000"];

    run_test_find(&args, "", "", 0)
}

#[test]
fn find_combination_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-size", "+4", "-print", "-size", "+2", "-print"];

    // The different result of the command is due to a feature of the linux and macos operating systems,
    // namely the use of different file systems (ext4 on linux and APFS on macos).
    // Therefore, the size of folders differs depending on the operating system.
    #[cfg(not(target_os = "macos"))]
    let expected_output = format!(
        "{}\n{}\n{}/file1.txt\n{}/file1.txt\n",
        test_dir, test_dir, test_dir, test_dir
    );

    #[cfg(target_os = "macos")]
    let expected_output = format!("{}/file1.txt\n{}/file1.txt\n", test_dir, test_dir);

    run_test_find(&args, &expected_output, "", 0)
}

#[test]
fn find_not_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args: [&str; 4] = [&test_dir, "!", "-path", "*.txt"];

    let dir = test_dir.clone();
    let file = format!("{}/rust_file.rs", test_dir);

    run_test_find_sorted(&args, &[&dir, &file], "", 0)
}

#[test]
fn find_or_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args: [&str; 6] = [&test_dir, "-path", "*.rs", "-o", "-path", "*.txt"];

    let file1 = format!("{}/empty_file.txt", test_dir);
    let file2 = format!("{}/file with space.txt", test_dir);
    let file3 = format!("{}/file1.txt", test_dir);
    let file4 = format!("{}/rust_file.rs", test_dir);

    run_test_find_sorted(&args, &[&file1, &file2, &file3, &file4], "", 0)
}

#[test]
fn find_and_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-path", "*.txt", "-a", "-size", "+2"];

    let expected_output = format!("{}/file1.txt\n", test_dir);

    run_test_find(&args, &expected_output, "", 0)
}

#[test]
fn find_space_argument_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-name", "file with space.txt"];

    let expected_output = format!("{}/file with space.txt\n", test_dir);

    run_test_find(&args, &expected_output, "", 0)
}

#[test]
fn find_no_user_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-nouser"];

    run_test_find(&args, "", "", 0)
}

#[test]
fn find_no_group_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-nogroup"];

    run_test_find(&args, "", "", 0)
}

#[test]
fn find_x_dev_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args: [&str; 2] = [&test_dir, "-xdev"];

    let dir = test_dir.clone();
    let file1 = format!("{}/empty_file.txt", test_dir);
    let file2 = format!("{}/file with space.txt", test_dir);
    let file3 = format!("{}/file1.txt", test_dir);
    let file4 = format!("{}/rust_file.rs", test_dir);

    run_test_find_sorted(&args, &[&dir, &file1, &file2, &file3, &file4], "", 0)
}

#[test]
fn find_perm_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-perm", "777"];

    run_test_find(&args, "", "", 0)
}

#[test]
fn find_links_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args: [&str; 3] = [&test_dir, "-links", "1"];

    let file1 = format!("{}/empty_file.txt", test_dir);
    let file2 = format!("{}/file with space.txt", test_dir);
    let file3 = format!("{}/file1.txt", test_dir);
    let file4 = format!("{}/rust_file.rs", test_dir);

    run_test_find_sorted(&args, &[&file1, &file2, &file3, &file4], "", 0)
}

#[test]
fn find_group_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-group", "name"];

    run_test_find(&args, "", "", 0)
}

#[test]
fn find_newer_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/newer", project_root);
    let path_to_test_file = format!("{}/qwe.txt", test_dir);
    let mut file = File::create(&path_to_test_file).unwrap();
    writeln!(file, "File content").unwrap();
    let args = [&test_dir, "-newer", &path_to_test_file];

    run_test_find(&args, "", "", 0);

    remove_file(&path_to_test_file).unwrap();
}

/// Run find and compare null-delimited output (for -print0 testing)
fn run_test_find_print0_sorted(args: &[&str], expected_paths: &[&str], expected_exit_code: i32) {
    let find_path = get_binary_path("find");

    let output = Command::new(&find_path)
        .args(args)
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute find");

    let actual_exit_code = output.status.code().unwrap_or(-1);

    // Split output on null bytes and filter empty entries
    let mut actual_paths: Vec<&str> = output
        .stdout
        .split(|&b| b == 0)
        .filter_map(|bytes| std::str::from_utf8(bytes).ok())
        .filter(|s| !s.is_empty())
        .collect();
    actual_paths.sort();

    let mut expected_sorted: Vec<&str> = expected_paths.to_vec();
    expected_sorted.sort();

    assert_eq!(
        actual_paths, expected_sorted,
        "stdout mismatch (null-delimited sorted comparison)"
    );
    assert!(
        output.stderr.is_empty(),
        "stderr should be empty: {:?}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert_eq!(actual_exit_code, expected_exit_code, "exit code mismatch");
}

#[test]
fn find_print0_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args: [&str; 4] = [&test_dir, "-type", "f", "-print0"];

    let file1 = format!("{}/empty_file.txt", test_dir);
    let file2 = format!("{}/file with space.txt", test_dir);
    let file3 = format!("{}/file1.txt", test_dir);
    let file4 = format!("{}/rust_file.rs", test_dir);

    run_test_find_print0_sorted(&args, &[&file1, &file2, &file3, &file4], 0)
}

#[test]
fn find_print0_with_name_filter() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-name", "*.txt", "-print0"];

    let file1 = format!("{}/empty_file.txt", test_dir);
    let file2 = format!("{}/file with space.txt", test_dir);
    let file3 = format!("{}/file1.txt", test_dir);

    run_test_find_print0_sorted(&args, &[&file1, &file2, &file3], 0)
}

// --- fnmatch / -iname (find-A) ---

/// Create a fresh temp dir with the given files; returns its path.
fn make_fnmatch_dir(tag: &str, files: &[&str]) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!("posixutils_find_{tag}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    for f in files {
        File::create(dir.join(f)).unwrap();
    }
    dir
}

#[test]
fn find_name_bracket_range() {
    // POSIX bracket expression [a-z] must match a single lowercase letter. The
    // range is LC_COLLATE-sensitive, so pin the C locale for portable ASCII
    // semantics (see run_test_find_sorted_env).
    let dir = make_fnmatch_dir("bracket", &["m", "Q", "9", "abc"]);
    let ds = dir.to_str().unwrap();
    let expect = format!("{ds}/m");
    run_test_find_sorted_env(
        &[ds, "-name", "[a-z]"],
        &[("LC_ALL", "C")],
        &[&expect],
        "",
        0,
    );
    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn find_name_bracket_negation() {
    let dir = make_fnmatch_dir("negation", &["m", "Q", "9"]);
    let ds = dir.to_str().unwrap();
    let e1 = format!("{ds}/Q");
    let e2 = format!("{ds}/9");
    // [!a-z] is the complement of the same LC_COLLATE-sensitive range; pin C.
    run_test_find_sorted_env(
        &[ds, "-name", "[!a-z]"],
        &[("LC_ALL", "C")],
        &[&e1, &e2],
        "",
        0,
    );
    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn find_iname_case_insensitive() {
    let dir = make_fnmatch_dir("iname", &["README.md", "other.txt"]);
    let ds = dir.to_str().unwrap();
    let expect = format!("{ds}/README.md");
    run_test_find_sorted(&[ds, "-iname", "readme.md"], &[&expect], "", 0);
    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn find_name_question_mark() {
    let dir = make_fnmatch_dir("qmark", &["ab", "abc", "a"]);
    let ds = dir.to_str().unwrap();
    let expect = format!("{ds}/ab");
    run_test_find_sorted(&[ds, "-name", "a?"], &[&expect], "", 0);
    std::fs::remove_dir_all(&dir).unwrap();
}

// FIND-5: -ok prompts and only runs the utility on an affirmative answer.
fn run_ok_test(answer: &str, expect_stdout_nonempty: bool) {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let target = format!("{}/tests/find/other/empty_file.txt", project_root);
    run_test_with_checker(
        TestPlan {
            cmd: String::from("find"),
            args: vec![
                target.clone(),
                String::from("-ok"),
                String::from("echo"),
                String::from("{}"),
                String::from(";"),
            ],
            stdin_data: String::from(answer),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        |_, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert_eq!(stdout.contains(&target), expect_stdout_nonempty);
        },
    );
}

#[test]
fn find_ok_accepted() {
    run_ok_test("y\n", true);
}

#[test]
fn find_ok_declined() {
    run_ok_test("n\n", false);
}
