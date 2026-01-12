use super::fuser_test;
use std::{fs::File, path::PathBuf, process::Command, str};

/// Tests the basic functionality of `fuser` by ensuring it can find the PID of a process.
///
/// **Setup:**
/// - Starts a process running `tail -f` on a temporary file.
///
/// **Assertions:**
/// - Verifies that the PID of the process is included in the output of `fuser`.
#[test]
fn test_fuser_basic() {
    fn get_temp_file_path() -> PathBuf {
        let mut path = std::env::temp_dir();

        path.push("test_file");

        path
    }
    let binding = get_temp_file_path();
    let temp_file_path = binding.to_str().unwrap();
    File::create(temp_file_path).expect("Failed to create temporary file");

    let mut process = Command::new("tail")
        .arg("-f")
        .arg(temp_file_path)
        .spawn()
        .expect("Failed to start process");

    let pid = process.id();

    fuser_test(vec![temp_file_path.to_string()], "", 0, |_, output| {
        let stdout_str = str::from_utf8(&output.stdout).expect("Invalid UTF-8 in stdout");
        let pid_str = pid.to_string();
        assert!(
            stdout_str.contains(&pid_str),
            "PID {} not found in the output.",
            pid_str
        );
    });

    process.kill().expect("Failed to kill the process");
    std::fs::remove_file(temp_file_path).expect("Failed to remove temporary file");
}
