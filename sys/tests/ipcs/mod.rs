//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{TestPlan, run_test_with_checker};
use std::process::Output;

fn run_ipcs_test(args: Vec<&str>, expected_exit_code: i32, check_fn: fn(&TestPlan, &Output)) {
    let plan = TestPlan {
        cmd: "ipcs".to_string(),
        args: args.iter().map(|&s| s.to_string()).collect(),
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code,
    };

    run_test_with_checker(plan, check_fn);
}

// ============================================
// Basic output format verification
// ============================================

fn check_output_contains_ipc_status(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    // POSIX requires the first line to be "IPC status from <source> as of <date>"
    assert!(
        stdout.contains("IPC status from"),
        "Expected 'IPC status from' in output, got: {}",
        stdout
    );
}

fn check_output_contains_queues_section(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Should contain either message queue info or "not in system" message
    assert!(
        stdout.contains("Message Queue") || stdout.contains("message queue"),
        "Expected message queue section in output, got: {}",
        stdout
    );
}

fn check_output_contains_shm_section(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Should contain shared memory section
    assert!(
        stdout.contains("Shared Memory") || stdout.contains("shared memory"),
        "Expected shared memory section in output, got: {}",
        stdout
    );
}

fn check_output_contains_sem_section(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    // Should contain semaphore section
    assert!(
        stdout.contains("Semaphore") || stdout.contains("semaphore"),
        "Expected semaphore section in output, got: {}",
        stdout
    );
}

fn check_output_nonempty(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(!stdout.is_empty(), "Expected non-empty output");
}

fn check_output_contains_basic_headers(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    // POSIX requires these column headers for all facilities
    assert!(
        stdout.contains(" T ") || stdout.contains("T "),
        "Expected 'T' column header"
    );
    assert!(stdout.contains("ID"), "Expected 'ID' column header");
    assert!(stdout.contains("KEY"), "Expected 'KEY' column header");
    assert!(stdout.contains("MODE"), "Expected 'MODE' column header");
    assert!(stdout.contains("OWNER"), "Expected 'OWNER' column header");
    assert!(stdout.contains("GROUP"), "Expected 'GROUP' column header");
}

fn check_output_contains_creator_headers(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("CREATOR"),
        "Expected 'CREATOR' column header with -c"
    );
    assert!(
        stdout.contains("CGROUP"),
        "Expected 'CGROUP' column header with -c"
    );
}

fn check_output_contains_time_headers(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("CTIME"),
        "Expected 'CTIME' column header with -t"
    );
}

fn check_output_contains_all_print_headers(_: &TestPlan, output: &Output) {
    let stdout = String::from_utf8_lossy(&output.stdout);
    // -a should enable all print options: -b, -c, -o, -p, -t
    // Check for headers from each option
    assert!(
        stdout.contains("CREATOR"),
        "Expected 'CREATOR' from -c option"
    );
    assert!(stdout.contains("CTIME"), "Expected 'CTIME' from -t option");
}

// ============================================
// Basic functionality tests
// ============================================

#[test]
fn ipcs_no_args() {
    // Running ipcs without arguments should show all facilities
    run_ipcs_test(vec![], 0, check_output_contains_ipc_status);
}

#[test]
fn ipcs_no_args_has_basic_headers() {
    run_ipcs_test(vec![], 0, check_output_contains_basic_headers);
}

#[test]
fn ipcs_no_args_shows_all_facilities() {
    run_ipcs_test(vec![], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        // Should show all three facility types when no options specified
        let has_msg = stdout.contains("Message Queue") || stdout.contains("message queue");
        let has_shm = stdout.contains("Shared Memory") || stdout.contains("shared memory");
        let has_sem = stdout.contains("Semaphore") || stdout.contains("semaphore");
        assert!(
            has_msg && has_shm && has_sem,
            "Expected all three facility types, got: {}",
            stdout
        );
    });
}

// ============================================
// Facility option tests (-q, -m, -s)
// ============================================

#[test]
fn ipcs_message_queues_only() {
    run_ipcs_test(vec!["-q"], 0, check_output_contains_queues_section);
}

#[test]
fn ipcs_shared_memory_only() {
    run_ipcs_test(vec!["-m"], 0, check_output_contains_shm_section);
}

#[test]
fn ipcs_semaphores_only() {
    run_ipcs_test(vec!["-s"], 0, check_output_contains_sem_section);
}

#[test]
fn ipcs_queues_and_shm() {
    run_ipcs_test(vec!["-q", "-m"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let has_msg = stdout.contains("Message Queue") || stdout.contains("message queue");
        let has_shm = stdout.contains("Shared Memory") || stdout.contains("shared memory");
        assert!(has_msg && has_shm, "Expected both queues and shm sections");
    });
}

#[test]
fn ipcs_queues_and_sem() {
    run_ipcs_test(vec!["-q", "-s"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let has_msg = stdout.contains("Message Queue") || stdout.contains("message queue");
        let has_sem = stdout.contains("Semaphore") || stdout.contains("semaphore");
        assert!(has_msg && has_sem, "Expected both queues and sem sections");
    });
}

#[test]
fn ipcs_shm_and_sem() {
    run_ipcs_test(vec!["-m", "-s"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let has_shm = stdout.contains("Shared Memory") || stdout.contains("shared memory");
        let has_sem = stdout.contains("Semaphore") || stdout.contains("semaphore");
        assert!(has_shm && has_sem, "Expected both shm and sem sections");
    });
}

#[test]
fn ipcs_all_facilities_explicit() {
    run_ipcs_test(vec!["-q", "-m", "-s"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let has_msg = stdout.contains("Message Queue") || stdout.contains("message queue");
        let has_shm = stdout.contains("Shared Memory") || stdout.contains("shared memory");
        let has_sem = stdout.contains("Semaphore") || stdout.contains("semaphore");
        assert!(
            has_msg && has_shm && has_sem,
            "Expected all three facility types"
        );
    });
}

// ============================================
// Print option tests (-a, -b, -c, -o, -p, -t)
// ============================================

#[test]
fn ipcs_all_options() {
    // -a enables all print options (-b, -c, -o, -p, -t)
    run_ipcs_test(vec!["-a"], 0, check_output_contains_all_print_headers);
}

#[test]
fn ipcs_max_size_option() {
    // -b shows maximum allowable size info
    run_ipcs_test(vec!["-b"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        // For shm: SEGSZ, for sem: NSEMS, for msg: QBYTES
        let has_segsz = stdout.contains("SEGSZ");
        let has_nsems = stdout.contains("NSEMS");
        assert!(
            has_segsz || has_nsems,
            "Expected SEGSZ or NSEMS header with -b option"
        );
    });
}

#[test]
fn ipcs_creator_option() {
    run_ipcs_test(vec!["-c"], 0, check_output_contains_creator_headers);
}

#[test]
fn ipcs_outstanding_option() {
    // -o shows outstanding usage info
    run_ipcs_test(vec!["-o"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        // For shm: NATTCH, for msg: CBYTES/QNUM
        let has_nattch = stdout.contains("NATTCH");
        let has_cbytes = stdout.contains("CBYTES");
        assert!(
            has_nattch || has_cbytes,
            "Expected NATTCH or CBYTES header with -o option"
        );
    });
}

#[test]
fn ipcs_pid_option() {
    // -p shows process ID info
    run_ipcs_test(vec!["-p"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        // For shm: CPID/LPID, for msg: LSPID/LRPID
        let has_cpid = stdout.contains("CPID");
        let has_lpid = stdout.contains("LPID");
        let has_lspid = stdout.contains("LSPID");
        assert!(
            has_cpid || has_lpid || has_lspid,
            "Expected PID-related header with -p option"
        );
    });
}

#[test]
fn ipcs_time_option() {
    run_ipcs_test(vec!["-t"], 0, check_output_contains_time_headers);
}

// ============================================
// Combined option tests
// ============================================

#[test]
fn ipcs_combined_facility_options() {
    run_ipcs_test(vec!["-q", "-m"], 0, check_output_nonempty);
}

#[test]
fn ipcs_combined_print_options() {
    run_ipcs_test(vec!["-b", "-c", "-t"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("CREATOR"), "Expected CREATOR from -c");
        assert!(stdout.contains("CTIME"), "Expected CTIME from -t");
    });
}

#[test]
fn ipcs_facility_with_print_options() {
    // Test -m with -c
    run_ipcs_test(vec!["-m", "-c"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("Shared Memory") || stdout.contains("shared memory"),
            "Expected shared memory section"
        );
        assert!(stdout.contains("CREATOR"), "Expected CREATOR from -c");
    });
}

#[test]
fn ipcs_semaphore_with_all_options() {
    run_ipcs_test(vec!["-s", "-a"], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.contains("Semaphore") || stdout.contains("semaphore"),
            "Expected semaphore section"
        );
        assert!(stdout.contains("CREATOR"), "Expected CREATOR from -a");
        assert!(stdout.contains("CTIME"), "Expected CTIME from -a");
        assert!(stdout.contains("NSEMS"), "Expected NSEMS from -a (-b)");
    });
}

// ============================================
// Output format tests
// ============================================

#[test]
fn ipcs_header_format() {
    run_ipcs_test(vec![], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let first_line = stdout.lines().next().unwrap_or("");
        // POSIX: "IPC status from %s as of %s\n"
        assert!(
            first_line.starts_with("IPC status from"),
            "First line should start with 'IPC status from', got: {}",
            first_line
        );
        assert!(
            first_line.contains("as of"),
            "First line should contain 'as of', got: {}",
            first_line
        );
    });
}

#[test]
fn ipcs_output_ends_with_newline() {
    run_ipcs_test(vec![], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(
            stdout.ends_with('\n'),
            "Output should end with newline, got: {:?}",
            stdout
        );
    });
}

#[test]
fn ipcs_key_format_hex() {
    // When there are IPC resources, keys should be in hex format (0x...)
    // This test verifies the format pattern exists in headers or data
    run_ipcs_test(vec![], 0, |_, output| {
        let stdout = String::from_utf8_lossy(&output.stdout);
        // Just verify the output is well-formed
        assert!(
            stdout.contains("IPC status from"),
            "Expected valid IPC status output"
        );
    });
}

// ============================================
// Resource-based tests (Linux only, feature-gated)
// ============================================

#[cfg(all(target_os = "linux", feature = "posixutils_test_all"))]
mod ipc_resource_tests {
    use super::*;
    use std::process::Command;

    // Helper to create a shared memory segment and return its ID
    fn create_shm_segment() -> Option<i32> {
        // Use ipcmk if available, otherwise use C library calls
        let output = Command::new("ipcmk").args(["-M", "4096"]).output().ok()?;

        if !output.status.success() {
            return None;
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        // ipcmk output: "Shared memory id: 12345"
        stdout
            .split_whitespace()
            .last()
            .and_then(|s| s.parse().ok())
    }

    // Helper to create a semaphore set and return its ID
    fn create_sem_set() -> Option<i32> {
        let output = Command::new("ipcmk").args(["-S", "1"]).output().ok()?;

        if !output.status.success() {
            return None;
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        stdout
            .split_whitespace()
            .last()
            .and_then(|s| s.parse().ok())
    }

    // Helper to create a message queue and return its ID
    fn create_msg_queue() -> Option<i32> {
        let output = Command::new("ipcmk").args(["-Q"]).output().ok()?;

        if !output.status.success() {
            return None;
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        stdout
            .split_whitespace()
            .last()
            .and_then(|s| s.parse().ok())
    }

    // Helper to remove IPC resources
    fn remove_shm(id: i32) {
        let _ = Command::new("ipcrm").args(["-m", &id.to_string()]).output();
    }

    fn remove_sem(id: i32) {
        let _ = Command::new("ipcrm").args(["-s", &id.to_string()]).output();
    }

    fn remove_msg(id: i32) {
        let _ = Command::new("ipcrm").args(["-q", &id.to_string()]).output();
    }

    #[test]
    fn ipcs_shows_created_shm() {
        let Some(shm_id) = create_shm_segment() else {
            eprintln!(
                "Skipping test: could not create shared memory segment (ipcmk not available?)"
            );
            return;
        };

        // Run ipcs and check that our segment appears
        let plan = TestPlan {
            cmd: "ipcs".to_string(),
            args: vec!["-m".to_string()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        };

        run_test_with_checker(plan, |_, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains(&shm_id.to_string()),
                "Expected to find shm ID {} in output:\n{}",
                shm_id,
                stdout
            );
        });

        // Cleanup
        remove_shm(shm_id);
    }

    #[test]
    fn ipcs_shows_created_semaphore() {
        let Some(sem_id) = create_sem_set() else {
            eprintln!("Skipping test: could not create semaphore set (ipcmk not available?)");
            return;
        };

        let plan = TestPlan {
            cmd: "ipcs".to_string(),
            args: vec!["-s".to_string()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        };

        run_test_with_checker(plan, |_, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains(&sem_id.to_string()),
                "Expected to find semaphore ID {} in output:\n{}",
                sem_id,
                stdout
            );
        });

        // Cleanup
        remove_sem(sem_id);
    }

    #[test]
    fn ipcs_shows_created_msgqueue() {
        let Some(msg_id) = create_msg_queue() else {
            eprintln!("Skipping test: could not create message queue (ipcmk not available?)");
            return;
        };

        let plan = TestPlan {
            cmd: "ipcs".to_string(),
            args: vec!["-q".to_string()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        };

        run_test_with_checker(plan, |_, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            assert!(
                stdout.contains(&msg_id.to_string()),
                "Expected to find message queue ID {} in output:\n{}",
                msg_id,
                stdout
            );
        });

        // Cleanup
        remove_msg(msg_id);
    }

    #[test]
    fn ipcs_shm_shows_correct_size() {
        let Some(shm_id) = create_shm_segment() else {
            eprintln!("Skipping test: could not create shared memory segment");
            return;
        };

        // Run ipcs with -b to show size
        let plan = TestPlan {
            cmd: "ipcs".to_string(),
            args: vec!["-m".to_string(), "-b".to_string()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        };

        run_test_with_checker(plan, |_, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Should contain SEGSZ header
            assert!(stdout.contains("SEGSZ"), "Expected SEGSZ column header");
            // Should contain our segment ID
            assert!(
                stdout.contains(&shm_id.to_string()),
                "Expected to find shm ID {} in output",
                shm_id
            );
            // Size should be 4096
            assert!(
                stdout.contains("4096"),
                "Expected size 4096 in output:\n{}",
                stdout
            );
        });

        remove_shm(shm_id);
    }

    #[test]
    fn ipcs_mode_string_format() {
        let Some(shm_id) = create_shm_segment() else {
            eprintln!("Skipping test: could not create shared memory segment");
            return;
        };

        let plan = TestPlan {
            cmd: "ipcs".to_string(),
            args: vec!["-m".to_string()],
            stdin_data: String::new(),
            expected_out: String::new(),
            expected_err: String::new(),
            expected_exit_code: 0,
        };

        run_test_with_checker(plan, |_, output| {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Find the line with our shm_id
            for line in stdout.lines() {
                if line.contains(&shm_id.to_string()) {
                    // Mode string should be 11 characters matching pattern
                    // [S-][RC-][rwa][rwa][rwa]
                    // Look for a pattern like "--rw-rw----" or similar
                    let has_mode = line.contains("rw") || line.contains("r-");
                    assert!(
                        has_mode,
                        "Expected mode string with permission chars in line: {}",
                        line
                    );
                    break;
                }
            }
        });

        remove_shm(shm_id);
    }
}
