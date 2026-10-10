//
// Copyright (c) 2024 Jeff Garzik
// Copyright (c) 2024 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

mod acl;

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

fn run_mkdir_test(args: Vec<&str>, expected_exit_code: i32, expected_err_substr: &str) {
    let plan = TestPlan {
        cmd: String::from("mkdir"),
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
fn test_create_single_directory() {
    let (_temp_dir, dir_path) = setup_test_env();

    run_mkdir_test(vec![&dir_path], 0, "");

    // Ensure the directory has been created
    assert!(Path::new(&dir_path).exists());

    // Clean up
    fs::remove_dir(&dir_path).expect("Unable to remove test directory");
}

#[test]
fn test_directory_already_exists() {
    let (_temp_dir, dir_path) = setup_test_env();
    fs::create_dir(&dir_path).expect("Unable to create test directory");

    run_mkdir_test(vec![&dir_path], 1, "File exists");

    // Ensure the directory still exists
    assert!(Path::new(&dir_path).exists());

    // Clean up
    fs::remove_dir(&dir_path).expect("Unable to remove test directory");
}

#[test]
fn test_invalid_mode() {
    let (_temp_dir, dir_path) = setup_test_env();

    run_mkdir_test(vec!["-m", "invalid", &dir_path], 1, "invalid mode string");

    // Ensure the directory has not been created
    assert!(!Path::new(&dir_path).exists());
}

#[test]
fn test_set_directory_mode() {
    let (_temp_dir, dir_path) = setup_test_env();

    run_mkdir_test(vec!["-m", "755", &dir_path], 0, "");

    // Ensure the directory has been created
    assert!(Path::new(&dir_path).exists());

    // Check the directory permissions
    let metadata = fs::metadata(&dir_path).expect("Unable to get directory metadata");
    let permissions = metadata.permissions();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        assert_eq!(permissions.mode() & 0o777, 0o755);
    }

    // Clean up
    fs::remove_dir(&dir_path).expect("Unable to remove test directory");
}

// XBD 12.2, Guideline 7: an option-argument may begin with '-'. Each option
// below used to have the word after it refused as an unknown option.
#[test]
fn option_argument_may_begin_with_hyphen() {
    plib::testing::assert_hyphen_option_argument("mkdir", &["-m", "-zq", "--help"]);
}

/// `mkdir -p a/b/c` in a scratch directory under `umask`, `seccomp` given the command to
/// install a filter in: the status and stderr, and the mode of each of `a`, `a/b`, `a/b/c`.
#[cfg(unix)]
fn mkdir_p_under_umask(
    umask: u32,
    seccomp: impl FnOnce(&mut std::process::Command),
) -> (Option<i32>, String, Vec<u32>) {
    use std::os::unix::fs::PermissionsExt;
    let temp = tempdir().unwrap();
    let mut command = std::process::Command::new("sh");
    command
        .args(["-c", "umask \"$0\" && exec \"$@\""])
        .arg(format!("{umask:03o}"))
        .arg(env!("CARGO_BIN_EXE_mkdir"))
        .args(["-p", "a/b/c"])
        .current_dir(temp.path())
        .stdin(std::process::Stdio::null());
    seccomp(&mut command);
    let out = command.output().unwrap();
    let mut modes = Vec::new();
    for name in ["a", "a/b", "a/b/c"] {
        let path = temp.path().join(name);
        if let Ok(meta) = fs::symlink_metadata(&path) {
            modes.push(meta.permissions().mode() & 0o7777);
        }
    }
    // Leave every directory removable.
    for name in ["a", "a/b", "a/b/c"] {
        let _ = fs::set_permissions(temp.path().join(name), fs::Permissions::from_mode(0o700));
    }
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    (out.status.code(), stderr, modes)
}

/// POSIX: `-p` intermediates get the umask-derived mode plus owner write and search, even
/// under a umask that denies the owner both; the last component gets the umask-derived mode
/// alone. The modes GNU mkdir 9.4 gives.
#[cfg(unix)]
#[test]
fn mkdir_p_gives_intermediates_owner_write_and_search_under_any_umask() {
    for (umask, modes) in [
        (0o300, vec![0o777, 0o777, 0o477]),
        (0o377, vec![0o700, 0o700, 0o400]),
        (0o022, vec![0o755, 0o755, 0o755]),
    ] {
        let got = mkdir_p_under_umask(umask, |_| {});
        assert_eq!(got, (Some(0), String::new(), modes), "umask {umask:03o}");
    }
}

/// The same with no way to change a mode through a pinned descriptor -- `fchmodat2`
/// missing (ENOSYS), as before Linux 6.6, and `fchmodat` refused (ENOENT), as a
/// `/proc/self/fd/N` that is not there would be: the intermediates' mode is had from
/// `mkdir` itself.
#[cfg(all(
    target_os = "linux",
    any(target_arch = "x86_64", target_arch = "aarch64")
))]
#[test]
fn mkdir_p_under_umask_0300_needs_no_proc() {
    use plib::testing::seccomp::{install_filter, op, JEQ, LD, RET, RET_ALLOW, RET_ERRNO};
    #[cfg(target_arch = "x86_64")]
    const SYS_FCHMODAT: u32 = 268;
    #[cfg(target_arch = "aarch64")]
    const SYS_FCHMODAT: u32 = 53;
    const SYS_FCHMODAT2: u32 = 452;
    let errno = |e: i32| RET_ERRNO | u32::try_from(e).unwrap();
    let got = mkdir_p_under_umask(0o300, |command| {
        install_filter(
            command,
            vec![
                op(LD, 0, 0, 0),
                op(JEQ, 0, 1, SYS_FCHMODAT2),
                op(RET, 0, 0, errno(libc::ENOSYS)),
                op(JEQ, 0, 1, SYS_FCHMODAT),
                op(RET, 0, 0, errno(libc::ENOENT)),
                op(RET, 0, 0, RET_ALLOW),
            ],
        )
    });
    assert_eq!(got, (Some(0), String::new(), vec![0o777, 0o777, 0o477]));
}
