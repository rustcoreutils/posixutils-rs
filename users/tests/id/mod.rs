//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{TestPlan, run_test, run_test_with_checker};
use regex::Regex;

fn id_test(args: &[&str], expected_out: &str) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();
    run_test(TestPlan {
        cmd: String::from("id"),
        args: str_args,
        stdin_data: String::new(),
        expected_out: String::from(expected_out),
        expected_err: String::new(),
        expected_exit_code: 0,
    });
}

fn id_test_with_checker<F>(args: &[&str], checker: F)
where
    F: FnMut(&TestPlan, &std::process::Output),
{
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();
    run_test_with_checker(
        TestPlan {
            cmd: String::from("id"),
            args: str_args,
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        },
        checker,
    );
}

// Get current uid/gid for comparison
fn get_current_uid() -> u32 {
    unsafe { libc::getuid() }
}

fn get_current_gid() -> u32 {
    unsafe { libc::getgid() }
}

fn get_current_euid() -> u32 {
    unsafe { libc::geteuid() }
}

fn get_current_egid() -> u32 {
    unsafe { libc::getegid() }
}

fn get_current_username() -> String {
    let uid = get_current_uid();
    let passwd = unsafe { libc::getpwuid(uid) };
    if passwd.is_null() {
        return String::new();
    }
    unsafe {
        std::ffi::CStr::from_ptr((*passwd).pw_name)
            .to_string_lossy()
            .to_string()
    }
}

fn get_current_groupname() -> String {
    let gid = get_current_gid();
    let grp = unsafe { libc::getgrgid(gid) };
    if grp.is_null() {
        return String::new();
    }
    unsafe {
        std::ffi::CStr::from_ptr((*grp).gr_name)
            .to_string_lossy()
            .to_string()
    }
}

// ============================================================================
// Default output format tests
// ============================================================================

#[test]
fn test_id_default_format() {
    // Default output should match: uid=UID(username) gid=GID(groupname) groups=...
    id_test_with_checker(&[], |_plan, output| {
        assert!(output.status.success(), "id should succeed");
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Check format: uid=NUM(name) gid=NUM(name) groups=NUM(name),...
        let re = Regex::new(r"^uid=\d+\([^)]+\) gid=\d+\([^)]+\) groups=\d+\([^)]+\)").unwrap();
        assert!(
            re.is_match(&stdout),
            "Default output format incorrect: {}",
            stdout
        );

        // Verify it ends with a newline
        assert!(stdout.ends_with('\n'), "Output should end with newline");

        // Verify no trailing comma before newline
        let trimmed = stdout.trim_end();
        assert!(
            !trimmed.ends_with(','),
            "Output should not have trailing comma"
        );
    });
}

#[test]
fn test_id_default_values() {
    // Verify the numeric values match libc calls
    let expected_uid = get_current_uid();
    let expected_gid = get_current_gid();
    let expected_username = get_current_username();

    id_test_with_checker(&[], |_plan, output| {
        assert!(output.status.success(), "id should succeed");
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Check uid value
        let uid_pattern = format!("uid={}({})", expected_uid, expected_username);
        assert!(
            stdout.contains(&uid_pattern),
            "Expected uid={} in output: {}",
            uid_pattern,
            stdout
        );

        // Check gid value
        let gid_pattern = format!("gid={}", expected_gid);
        assert!(
            stdout.contains(&gid_pattern),
            "Expected {} in output: {}",
            gid_pattern,
            stdout
        );
    });
}

// ============================================================================
// -u option tests (effective user ID)
// ============================================================================

#[test]
fn test_id_u() {
    // -u should output just the effective user ID
    let expected = format!("{}\n", get_current_euid());
    id_test(&["-u"], &expected);
}

#[test]
fn test_id_u_n() {
    // -u -n should output the effective username
    let expected = format!("{}\n", get_current_username());
    id_test(&["-u", "-n"], &expected);
}

#[test]
fn test_id_u_r() {
    // -u -r should output the real user ID
    let expected = format!("{}\n", get_current_uid());
    id_test(&["-u", "-r"], &expected);
}

#[test]
fn test_id_u_r_n() {
    // -u -r -n should output the real username
    let expected = format!("{}\n", get_current_username());
    id_test(&["-u", "-r", "-n"], &expected);
}

// ============================================================================
// -g option tests (effective group ID)
// ============================================================================

#[test]
fn test_id_g() {
    // -g should output just the effective group ID
    let expected = format!("{}\n", get_current_egid());
    id_test(&["-g"], &expected);
}

#[test]
fn test_id_g_n() {
    // -g -n should output the effective group name
    let expected = format!("{}\n", get_current_groupname());
    id_test(&["-g", "-n"], &expected);
}

#[test]
fn test_id_g_r() {
    // -g -r should output the real group ID
    let expected = format!("{}\n", get_current_gid());
    id_test(&["-g", "-r"], &expected);
}

#[test]
fn test_id_g_r_n() {
    // -g -r -n should output the real group name
    let expected = format!("{}\n", get_current_groupname());
    id_test(&["-g", "-r", "-n"], &expected);
}

// ============================================================================
// -G option tests (all group IDs)
// ============================================================================

#[test]
#[allow(non_snake_case)]
fn test_id_G_format() {
    // -G should output space-separated group IDs
    id_test_with_checker(&["-G"], |_plan, output| {
        assert!(output.status.success(), "id -G should succeed");
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Should be space-separated numbers ending with newline
        let re = Regex::new(r"^\d+( \d+)*\n$").unwrap();
        assert!(
            re.is_match(&stdout),
            "-G output format incorrect: {:?}",
            stdout
        );

        // Primary group should be first
        let gid = get_current_gid();
        assert!(
            stdout.starts_with(&format!("{}", gid)),
            "Primary group {} should be first in output: {}",
            gid,
            stdout
        );
    });
}

#[test]
#[allow(non_snake_case)]
fn test_id_G_n_format() {
    // -G -n should output space-separated group names
    id_test_with_checker(&["-G", "-n"], |_plan, output| {
        assert!(output.status.success(), "id -G -n should succeed");
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Should be space-separated names ending with newline
        // Names can contain alphanumerics, underscores, dots, and hyphens
        let re = Regex::new(r"^[\w._-]+( [\w._-]+)*\n$").unwrap();
        assert!(
            re.is_match(&stdout),
            "-G -n output format incorrect: {:?}",
            stdout
        );

        // Primary group name should be first
        let groupname = get_current_groupname();
        assert!(
            stdout.starts_with(&groupname),
            "Primary group {} should be first in output: {}",
            groupname,
            stdout
        );
    });
}

// ============================================================================
// Named user tests
// ============================================================================

#[test]
fn test_id_root() {
    // id root should show root's info
    id_test_with_checker(&["root"], |_plan, output| {
        assert!(output.status.success(), "id root should succeed");
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Should start with uid=0(root)
        assert!(
            stdout.starts_with("uid=0(root)"),
            "Expected uid=0(root), got: {}",
            stdout
        );

        // Should contain gid=0
        assert!(stdout.contains("gid=0"), "Expected gid=0 in output");
    });
}

#[test]
fn test_id_root_u() {
    // id -u root should output 0
    id_test(&["-u", "root"], "0\n");
}

#[test]
fn test_id_root_u_n() {
    // id -u -n root should output "root"
    id_test(&["-u", "-n", "root"], "root\n");
}

#[test]
#[allow(non_snake_case)]
fn test_id_root_G() {
    // id -G root should start with 0 (wheel/root group)
    id_test_with_checker(&["-G", "root"], |_plan, output| {
        assert!(output.status.success(), "id -G root should succeed");
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.starts_with("0"),
            "Expected output to start with 0, got: {}",
            stdout
        );
    });
}

// ============================================================================
// Error handling tests
// ============================================================================

#[test]
fn test_id_invalid_user() {
    // Invalid user should produce error and exit 1
    run_test(TestPlan {
        cmd: String::from("id"),
        args: vec![String::from("nonexistent_user_xyz_12345")],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::from("id: nonexistent_user_xyz_12345: no such user\n"),
        expected_exit_code: 1,
    });
}

// ============================================================================
// Option combination tests
// ============================================================================

#[test]
fn test_id_combined_options() {
    // Test various option combinations work correctly
    // -n alone should be ignored (default output)
    id_test_with_checker(&["-n"], |_plan, output| {
        assert!(output.status.success(), "id -n should succeed");
        let stdout = String::from_utf8_lossy(&output.stdout);
        // Should produce default output format
        assert!(
            stdout.contains("uid="),
            "Expected default format with -n alone"
        );
    });

    // -r alone should be ignored (default output)
    id_test_with_checker(&["-r"], |_plan, output| {
        assert!(output.status.success(), "id -r should succeed");
        let stdout = String::from_utf8_lossy(&output.stdout);
        // Should produce default output format
        assert!(
            stdout.contains("uid="),
            "Expected default format with -r alone"
        );
    });
}

#[test]
fn test_id_long_options() {
    // Test long option forms
    let expected_uid = format!("{}\n", get_current_euid());
    id_test(&["--user"], &expected_uid);

    let expected_gid = format!("{}\n", get_current_egid());
    id_test(&["--group"], &expected_gid);
}

// ============================================================================
// Current user tests (verify output matches libc)
// ============================================================================

#[test]
fn test_id_current_user_by_name() {
    // id <current_username> should produce same numeric values as id
    let username = get_current_username();
    let expected_uid = get_current_uid();

    id_test_with_checker(&[&username], |_plan, output| {
        assert!(output.status.success(), "id <username> should succeed");
        let stdout = String::from_utf8_lossy(&output.stdout);

        let expected_prefix = format!("uid={}({})", expected_uid, username);
        assert!(
            stdout.starts_with(&expected_prefix),
            "Expected to start with {}, got: {}",
            expected_prefix,
            stdout
        );
    });
}
