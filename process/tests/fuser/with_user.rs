mod with_user {
    use std::ffi::CStr;
    use std::fs::File;
    use std::process::Command;
    use std::{io, str};

    use libc::uid_t;

    use crate::fuser::fuser_test;

    /// Retrieves the user name of the process owner by process ID on Linux.
    ///
    /// **Arguments:**
    /// - `pid`: The process ID of the target process.
    ///
    /// **Returns:**
    /// - A `Result` containing the user name if successful, or an `io::Error`.
    #[cfg(target_os = "linux")]
    fn get_process_user(pid: u32) -> io::Result<String> {
        use std::io::Read;
        let status_path = format!("/proc/{}/status", pid);
        let mut file = File::open(&status_path).map_err(|e| {
            eprintln!("Failed to open {}: {}", status_path, e);
            e
        })?;
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

    /// Retrieves the user name of the process owner by process ID on macOS.
    ///
    /// **Arguments:**
    /// - `pid`: The process ID of the target process (not used here).
    ///
    /// **Returns:**
    /// - A `Result` containing the user name if successful, or an `io::Error`.
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
        let temp_file_path = std::env::temp_dir().join("test_file_with_user");
        let temp_file_path_clone = temp_file_path.clone();

        File::create(&temp_file_path_clone).expect("Failed to create temporary file");

        let mut process = Command::new("tail")
            .arg("-f")
            .arg(&temp_file_path_clone)
            .spawn()
            .expect("Failed to start process");

        let pid = process.id();
        let owner = get_process_user(pid).expect("Failed to get owner of process");

        fuser_test(
            vec![
                temp_file_path_clone.to_str().unwrap().to_string(),
                "-u".to_string(),
            ],
            "",
            0,
            |_, output| {
                let stderr_str = str::from_utf8(&output.stderr).expect("Invalid UTF-8 in stderr");

                assert!(
                    stderr_str.contains(&owner),
                    "Owner {} not found in the fuser output.",
                    owner
                );
            },
        );

        process.kill().expect("Failed to kill the process");
        std::fs::remove_file(temp_file_path).expect("Failed to remove temporary file");
    }
}
