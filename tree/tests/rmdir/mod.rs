//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test_with_checker, TestPlan};
use plib::tmp::{tempdir, TempDir};
use std::fs;
use std::path::Path;
use std::process::Output;

fn setup_test_env() -> (TempDir, String) {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    let dir_path = temp_dir.path().join("testdir");
    (temp_dir, dir_path.to_str().unwrap().to_string())
}

fn run_rmdir_test(args: Vec<&str>, expected_exit_code: i32, expected_err_substr: &str) {
    let plan = TestPlan {
        cmd: String::from("rmdir"),
        args: args.iter().map(|&s| s.into()).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code,
    };

    run_test_with_checker(plan, move |_, output: &Output| {
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains(expected_err_substr),
            "Expected substring not found in stderr: '{}'",
            stderr
        );
    });
}

#[test]
fn rmdir_remove_existing_directory() {
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    run_rmdir_test(vec![&dir_path], 0, "");

    // Ensure the directory has been removed
    assert!(!Path::new(&dir_path).exists());
}

#[test]
fn rmdir_remove_non_empty_directory() {
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");
    let file_path = Path::new(&dir_path).join("file.txt");
    fs::write(&file_path, b"test").expect("Unable to create test file");

    run_rmdir_test(vec![&dir_path], 1, "Directory not empty");

    // Ensure the directory still exists
    assert!(Path::new(&dir_path).exists());

    // Clean up
    fs::remove_file(file_path).expect("Unable to remove test file");
    fs::remove_dir(&dir_path).expect("Unable to remove test directory");
}

#[test]
fn rmdir_remove_non_existent_directory() {
    let (_temp_dir, dir_path) = setup_test_env();

    run_rmdir_test(vec![&dir_path], 1, "No such file or directory");

    // Ensure the directory still does not exist
    assert!(!Path::new(&dir_path).exists());
}

#[test]
fn rmdir_remove_directory_with_parents() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    let parent_dir = temp_dir.path().join("parent");
    let dir_path = parent_dir.join("testdir");

    fs::create_dir_all(&dir_path).expect("Unable to create test directories");

    run_rmdir_test(vec!["-p", dir_path.to_str().unwrap()], 0, "");

    // Ensure the directories have been removed
    assert!(!dir_path.exists());
    assert!(!parent_dir.exists());
}

// `rmdir -p` on a non-empty parent reports the parent that actually failed (not the
// original operand), removes the empty leaf chain, and exits 1.
#[test]
fn test_rmdir_p_names_failing_parent() {
    let test_dir = &format!(
        "{}/test_rmdir_p_names_failing_parent",
        env!("CARGO_TARGET_TMPDIR")
    );
    let a = &format!("{test_dir}/a");
    let c = &format!("{test_dir}/a/b/c");
    fs::create_dir_all(c).unwrap();
    fs::File::create(format!("{a}/keep")).unwrap(); // makes `a` non-empty

    let out = std::process::Command::new(env!("CARGO_BIN_EXE_rmdir"))
        .args(["-p", c])
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains(&format!("rmdir: {a}: ")),
        "diagnostic should name the failing parent: {stderr}"
    );
    assert!(!Path::new(c).exists(), "empty leaf chain removed");
    assert!(Path::new(a).exists(), "non-empty parent kept");

    fs::remove_dir_all(test_dir).unwrap();
}

/// Run the built `rmdir` binary on `args` inside `cwd`, returning (exit code, stderr).
fn run_rmdir_in(cwd: &Path, args: &[&str]) -> (Option<i32>, String) {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_rmdir"))
        .current_dir(cwd)
        .args(args)
        .output()
        .unwrap();
    (
        out.status.code(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

// GNU `--ignore-fail-on-non-empty`: a non-empty directory is kept silently and does
// not affect the exit status; an empty operand next to it is still removed.
#[test]
fn rmdir_ignore_fail_on_non_empty_skips_non_empty() {
    let temp_dir = tempdir().unwrap();
    let base = temp_dir.path();
    fs::create_dir_all(base.join("full")).unwrap();
    fs::File::create(base.join("full/file")).unwrap();
    fs::create_dir(base.join("empty")).unwrap();

    let (code, stderr) = run_rmdir_in(base, &["--ignore-fail-on-non-empty", "full", "empty"]);
    assert_eq!(code, Some(0), "stderr: {stderr}");
    assert_eq!(stderr, "");
    assert!(base.join("full/file").exists(), "non-empty directory kept");
    assert!(!base.join("empty").exists(), "empty directory removed");
}

// With `-p`, the walk up the parents stops silently at the first non-empty one:
// `a/b/c` and `a/b` are removed, `a` (holding `keep`) stays, and the exit status is 0.
#[test]
fn rmdir_ignore_fail_on_non_empty_with_parents() {
    let temp_dir = tempdir().unwrap();
    let base = temp_dir.path();
    fs::create_dir_all(base.join("a/b/c")).unwrap();
    fs::File::create(base.join("a/keep")).unwrap();

    let (code, stderr) = run_rmdir_in(base, &["-p", "--ignore-fail-on-non-empty", "a/b/c"]);
    assert_eq!(code, Some(0), "stderr: {stderr}");
    assert_eq!(stderr, "");
    assert!(!base.join("a/b").exists(), "empty chain removed");
    assert!(base.join("a/keep").exists(), "non-empty parent kept");
}

// Errors other than "not empty" are still reported and still fail.
#[test]
fn rmdir_ignore_fail_on_non_empty_reports_other_errors() {
    let temp_dir = tempdir().unwrap();
    let base = temp_dir.path();
    fs::create_dir(base.join("full")).unwrap();
    fs::File::create(base.join("full/file")).unwrap();

    let (code, stderr) = run_rmdir_in(base, &["--ignore-fail-on-non-empty", "full", "nope"]);
    assert_eq!(code, Some(1));
    assert!(
        stderr.contains("rmdir: nope: No such file or directory"),
        "stderr: {stderr}"
    );
    assert!(
        !stderr.contains("full"),
        "non-empty failure stays silent: {stderr}"
    );

    let (code, stderr) = run_rmdir_in(base, &["--ignore-fail-on-non-empty", "full/file"]);
    assert_eq!(code, Some(1));
    assert!(stderr.contains("Not a directory"), "stderr: {stderr}");
}

// Like GNU, a permission error on a directory that is in fact non-empty counts as
// "not empty" and is ignored; on an empty directory it is reported.
#[test]
fn rmdir_ignore_fail_on_non_empty_permission_denied() {
    if unsafe { libc::geteuid() } == 0 {
        return; // root ignores directory permissions
    }
    let temp_dir = tempdir().unwrap();
    let base = temp_dir.path();
    fs::create_dir_all(base.join("ro/full/x")).unwrap();
    fs::create_dir(base.join("ro/empty")).unwrap();
    let ro = base.join("ro");
    let mode = |m| {
        use std::os::unix::fs::PermissionsExt;
        fs::set_permissions(&ro, fs::Permissions::from_mode(m)).unwrap();
    };
    mode(0o555);

    let full = run_rmdir_in(base, &["--ignore-fail-on-non-empty", "ro/full"]);
    let empty = run_rmdir_in(base, &["--ignore-fail-on-non-empty", "ro/empty"]);
    mode(0o755);

    assert_eq!(full, (Some(0), String::new()));
    assert_eq!(empty.0, Some(1));
    assert!(
        empty.1.contains("rmdir: ro/empty: Permission denied"),
        "stderr: {}",
        empty.1
    );
}
