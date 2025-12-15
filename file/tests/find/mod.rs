use std::fs::{remove_file, File};
use std::io::Write;
use std::process::{Command, Stdio};

use plib::testing::{run_test, TestPlan};

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
    let project_root = env!("CARGO_MANIFEST_DIR");
    // Determine the target directory - cargo-llvm-cov uses a custom target dir
    let target_dir = std::env::var("CARGO_TARGET_DIR")
        .or_else(|_| std::env::var("CARGO_LLVM_COV_TARGET_DIR"))
        .unwrap_or_else(|_| String::from("target"));
    let profile = if cfg!(debug_assertions) {
        "debug"
    } else {
        "release"
    };
    let find_path = format!("{}/../{}/{}/find", project_root, target_dir, profile);

    let output = Command::new(&find_path)
        .args(args)
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute find");

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
    let project_root = env!("CARGO_MANIFEST_DIR");
    // Determine the target directory - cargo-llvm-cov uses a custom target dir
    let target_dir = std::env::var("CARGO_TARGET_DIR")
        .or_else(|_| std::env::var("CARGO_LLVM_COV_TARGET_DIR"))
        .unwrap_or_else(|_| String::from("target"));
    let profile = if cfg!(debug_assertions) {
        "debug"
    } else {
        "release"
    };
    let find_path = format!("{}/../{}/{}/find", project_root, target_dir, profile);

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
