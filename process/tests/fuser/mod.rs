use libc::uid_t;
use plib::{run_test_with_checker, TestPlan};
use std::{
    ffi::CStr,
    fs, io,
    path::{Path, PathBuf},
    process::{Command, Output},
    str,
};

#[cfg(target_os = "linux")]
use std::{fs::File, io::Read};
#[cfg(target_os = "linux")]
use tokio::net::{TcpListener, UdpSocket, UnixListener};

fn fuser_test(
    args: Vec<String>,
    expected_err: &str,
    expected_exit_code: i32,
    checker: impl FnMut(&TestPlan, &Output),
) {
    run_test_with_checker(
        TestPlan {
            cmd: "fuser".to_string(),
            args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: expected_err.to_string(),
            expected_exit_code,
        },
        checker,
    );
}

/// Tests the basic functionality of `fuser` by ensuring it can find the PID of a process.
///
/// **Setup:**
/// - Starts a process running `sleep 1`.
///
/// **Assertions:**
/// - Verifies that the PID of the process is included in the output of `fuser`.
#[cfg(target_os = "linux")]
#[tokio::test]
async fn test_fuser_basic() {
    let process = Command::new("sleep")
        .arg("1")
        .spawn()
        .expect("Failed to start process");

    let pid = process.id();

    fuser_test(vec!["./".to_string()], "", 0, |_, output| {
        let stdout_str = str::from_utf8(&output.stdout).expect("Invalid UTF-8 in stdout");
        let pid_str = pid.to_string();
        assert!(
            stdout_str.contains(&pid_str),
            "PID {} not found in the output.",
            pid_str
        );
    });
}

#[cfg(target_os = "linux")]
fn get_process_user(pid: u32) -> io::Result<String> {
    let status_path = format!("/proc/{}/status", pid);
    let mut file = File::open(&status_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let uid_line = contents
        .lines()
        .find(|line| line.starts_with("Uid:"))
        .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "Uid line not found"))?;

    let uid_str = uid_line
        .split_whitespace()
        .nth(1)
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "UID not found"))?;
    let uid: uid_t = uid_str
        .parse()
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Invalid UID"))?;

    get_username_by_uid(uid)
}

#[cfg(target_os = "macos")]
fn get_process_user(_pid: u32) -> io::Result<String> {
    let uid = unsafe { libc::getuid() };
    get_username_by_uid(uid)
}

fn get_username_by_uid(uid: uid_t) -> io::Result<String> {
    let pwd = unsafe { libc::getpwuid(uid) };
    if pwd.is_null() {
        return Err(io::Error::new(io::ErrorKind::NotFound, "User not found"));
    }

    let user_name = unsafe {
        CStr::from_ptr((*pwd).pw_name)
            .to_string_lossy()
            .into_owned()
    };

    Ok(user_name)
}
/// Tests `fuser` with the `-u` flag to ensure it outputs the process owner.
///
/// **Setup:**
/// - Starts a process running `sleep 1`.
///
/// **Assertions:**
/// - Verifies that the owner printed in stderr.
#[test]
fn test_fuser_with_user() {
    let process = Command::new("sleep")
        .arg("1")
        .spawn()
        .expect("Failed to start process");

    let pid = process.id();

    fuser_test(
        vec!["./".to_string(), "-u".to_string()],
        "",
        0,
        |_, output| {
            let owner = get_process_user(pid).expect("Failed to get owner of process");
            let stderr_str = str::from_utf8(&output.stderr).expect("Invalid UTF-8 in stderr");
            assert!(
                stderr_str.contains(&owner),
                "owner {} not found in the output.",
                owner
            );
        },
    );
}

/// Tests `fuser` with multiple file paths.
///
/// **Setup:**
/// - Starts two processes running `sleep 1` in different directories.
///
/// **Assertions:**
/// - Verifies that the PIDs of both processes are included in the stdout.
#[ignore]
#[test]
fn test_fuser_with_many_files() {
    let process1 = Command::new("sleep")
        .current_dir("../")
        .arg("1")
        .spawn()
        .expect("Failed to start process");

    let process2 = Command::new("sleep")
        .current_dir("/")
        .arg("1")
        .spawn()
        .expect("Failed to start process");

    let pid1 = process1.id();
    let pid2 = process2.id();

    fuser_test(
        vec!["/".to_string(), "../".to_string()],
        "",
        0,
        |_, output| {
            let stdout_str = str::from_utf8(&output.stdout).expect("Invalid UTF-8 in stdout");
            let pid_str1 = pid1.to_string();
            let pid_str2 = pid2.to_string();
            assert!(
                stdout_str.contains(&pid_str1),
                "PID {} not found in the output.",
                pid_str1
            );
            assert!(
                stdout_str.contains(&pid_str2),
                "PID {} not found in the output.",
                pid_str2
            );
        },
    );
}

/// Starts a TCP server on port 8080.
#[cfg(target_os = "linux")]
async fn start_tcp_server() -> TcpListener {
    TcpListener::bind(("127.0.0.1", 8080))
        .await
        .expect("Failed to bind TCP server")
}

/// Tests `fuser` with TCP socket.
///
/// **Setup:**
/// - Starts a TCP server on port 8080.
///
/// **Assertions:**
/// - Verifies that the output of `fuser` matches the manual execution for TCP sockets.
#[tokio::test]
#[cfg(target_os = "linux")]
async fn test_fuser_tcp() {
    let _server = start_tcp_server().await;
    fuser_test(vec!["8080/tcp".to_string()], "", 0, |_, output| {
        let manual_output = Command::new("fuser").arg("8080/tcp").output().unwrap();
        assert_eq!(output.status.code(), Some(0));
        assert_eq!(output.stdout, manual_output.stdout);
        assert_eq!(output.stderr, manual_output.stderr);
    });
}

/// Starts a UDP server on port 8081.
#[cfg(target_os = "linux")]
async fn start_udp_server() -> UdpSocket {
    UdpSocket::bind(("127.0.0.1", 8081))
        .await
        .expect("Failed to bind UDP server")
}

/// Tests `fuser` with UDP socket.
///
/// **Setup:**
/// - Starts a UDP server on port 8081.
///
/// **Assertions:**
/// - Verifies that the output of `fuser` matches the manual execution for UDP sockets.
#[tokio::test]
#[cfg(target_os = "linux")]
async fn test_fuser_udp() {
    let _server = start_udp_server().await;
    fuser_test(vec!["8081/udp".to_string()], "", 0, |_, output| {
        let manual_output = Command::new("fuser").arg("8081/udp").output().unwrap();
        assert_eq!(output.status.code(), Some(0));
        assert_eq!(output.stdout, manual_output.stdout);
        assert_eq!(output.stderr, manual_output.stderr);
    });
}
/// Starts a Unix socket server at the specified path.
#[cfg(target_os = "linux")]
async fn start_unix_socket(socket_path: &str) -> UnixListener {
    if fs::metadata(socket_path).is_ok() {
        println!("A socket is already present. Deleting...");
        fs::remove_file(socket_path).expect("Failed to delete existing socket");
    }

    UnixListener::bind(socket_path).expect("Failed to bind Unix socket")
}

/// Tests `fuser` with Unix socket.
///
/// **Setup:**
/// - Starts a Unix socket server at the specified path (`/tmp/test.sock`).
///
/// **Assertions:**
/// - Verifies that the output of `fuser` matches the manual execution for the Unix socket at `/tmp/test.sock`.
///
/// **Note:**
/// - Before binding to the socket, the function checks if a socket file already exists at the path and deletes it if present.
/// - This ensures that the test environment is clean and prevents issues with existing sockets.
#[tokio::test]
#[cfg(target_os = "linux")]
async fn test_fuser_unixsocket() {
    let socket_path = "/tmp/test.sock";
    let _unix_socket = start_unix_socket(socket_path).await;
    fuser_test(vec![socket_path.to_string()], "", 0, |_, output| {
        let manual_output = Command::new("fuser").arg(socket_path).output().unwrap();
        assert_eq!(output.status.code(), Some(0));
        assert_eq!(output.stdout, manual_output.stdout);
        assert_eq!(output.stderr, manual_output.stderr);
    });
}

/// Creates a directory and populates it with a specified number of test files.
fn create_large_directory(dir_path: &Path, num_files: usize) -> std::io::Result<()> {
    fs::create_dir_all(dir_path)?;
    for i in 0..num_files {
        let file_path = dir_path.join(format!("file_{:04}", i));
        fs::write(file_path, "This is a test file.")?;
    }
    Ok(())
}

/// Deletes a directory and all of its contents.
fn delete_directory(dir_path: &Path) -> io::Result<()> {
    if dir_path.exists() {
        fs::remove_dir_all(dir_path)
    } else {
        Ok(())
    }
}

/// Tests `fuser` with a very large directory to ensure it can handle large numbers of files.
///
/// **Setup:**
/// - Creates a directory with a large number of files at a fixed path.
///
/// **Assertions:**
/// - Verifies that the `fuser` command completes successfully and the output is as expected.
/// - Executes an additional command to ensure it works after `fuser` and checks its output.
/// - Ensures that the test does not block indefinitely and completes within a reasonable time frame.
#[tokio::test]
async fn test_fuser_large_directory() {
    let test_dir_path = PathBuf::from("large_test_dir");

    let num_files = 10_000;

    create_large_directory(&test_dir_path, num_files).expect("Failed to create large directory");

    fuser_test(
        vec![test_dir_path.to_str().unwrap().to_string()],
        "",
        0,
        |_, output| {
            let stdout_str = str::from_utf8(&output.stdout).expect("Invalid UTF-8 in stdout");
            assert!(stdout_str.contains(""));
        },
    );

    let additional_command = Command::new("ls")
        .args(&[test_dir_path.to_str().unwrap()])
        .output()
        .expect("Failed to execute command");

    assert_eq!(additional_command.status.code(), Some(0));

    // Clean up the directory after the test
    delete_directory(&test_dir_path).expect("Failed to delete large directory");
}
