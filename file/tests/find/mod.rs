use std::fs::{remove_file, File};
use std::io::Write;

use plib::{run_test, TestPlan};

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
    let args = [&test_dir, "-type", "f"];

    let expected_output = format!("{}/empty_file.txt\n{}/file with space.txt\n{}/file1.txt\n{}/rust_file.rs\n", test_dir, test_dir, test_dir, test_dir);

    run_test_find(&args, &expected_output, "", 0)
}

#[test]
fn find_mtime_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-mtime", "7"];

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
    let expected_output = format!("{}\n{}\n{}/file1.txt\n{}/file1.txt\n", test_dir, test_dir, test_dir, test_dir);

    #[cfg(target_os = "macos")]
    let expected_output = format!("{}/file1.txt\n{}/file1.txt\n", test_dir, test_dir);
    
    run_test_find(&args, &expected_output, "", 0)
}

#[test]
fn find_not_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "!", "-path", "*.txt"];

    let expected_output = format!("{}\n{}/rust_file.rs\n", test_dir, test_dir);

    run_test_find(&args, &expected_output, "", 0)
}

#[test]
fn find_or_test() {
    let project_root = env!("CARGO_MANIFEST_DIR");
    let test_dir = format!("{}/tests/find/other", project_root);
    let args = [&test_dir, "-path", "*.rs", "-o", "-path", "*.txt"];

    let expected_output = format!("{}/empty_file.txt\n{}/file with space.txt\n{}/file1.txt\n{}/rust_file.rs\n", test_dir, test_dir, test_dir, test_dir);

    run_test_find(&args, &expected_output, "", 0)
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
    let args = [&test_dir, "-xdev"];

    let expected_output = format!("{}\n{}/empty_file.txt\n{}/file with space.txt\n{}/file1.txt\n{}/rust_file.rs\n", test_dir, test_dir, test_dir, test_dir, test_dir);

    run_test_find(&args, &expected_output, "", 0)
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
    let args = [&test_dir, "-links", "1"];

    let expected_output = format!("{}/empty_file.txt\n{}/file with space.txt\n{}/file1.txt\n{}/rust_file.rs\n", test_dir, test_dir, test_dir, test_dir);

    run_test_find(&args, &expected_output, "", 0)
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