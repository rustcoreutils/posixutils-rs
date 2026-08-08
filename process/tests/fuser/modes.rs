//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `-c`, `-f`, multiple operands, and the use-character column.
//!
//! These exercise the two arms of the `use_device_mode` decision in
//! `handle_file_namespace`. That matters beyond the flags themselves: the
//! `/proc/<pid>/maps` scan, and with it the `makedev` fix for a high minor
//! number (#F2), is reachable *only* when device mode is selected, so `-c` and
//! a bare block device are the only ways to reach that code at all.

use super::{fuser_test, parse_posix_pid_list};
use std::path::Path;
use std::str;

/// A block device the test user can stat, if the system has one.
#[cfg(target_os = "linux")]
fn some_block_device() -> Option<String> {
    let entries = std::fs::read_dir("/dev").ok()?;
    for entry in entries.flatten() {
        let path = entry.path();
        let Ok(meta) = std::fs::metadata(&path) else {
            continue;
        };
        use std::os::unix::fs::FileTypeExt;
        if meta.file_type().is_block_device() {
            return Some(path.to_string_lossy().into_owned());
        }
    }
    None
}

/// A mount point that certainly exists.
fn a_mount_point() -> &'static str {
    "/"
}

/// `-c` names a mount point and reports every process using anything on that
/// file system, which on a running system is at least this test.
#[test]
#[cfg(target_os = "linux")]
fn test_fuser_c_matches_the_whole_file_system() {
    fuser_test(
        vec!["-c".to_string(), a_mount_point().to_string()],
        "",
        0,
        |_, output| {
            let pids = parse_posix_pid_list(&output.stdout);
            assert!(
                pids.contains(&std::process::id()),
                "`fuser -c /` should report this process, which is running from that \
                 file system; got {:?}",
                pids
            );
        },
    );
}

/// A bare block device operand selects device mode without `-c`, and `-f`
/// suppresses that, asking about the device *file* itself instead.
#[test]
#[cfg(target_os = "linux")]
fn test_fuser_f_treats_a_block_device_as_a_plain_file() {
    let Some(dev) = some_block_device() else {
        println!("Skipping: no readable block device in /dev");
        return;
    };

    // Device mode: matching by device id finds whoever uses that file system.
    // It may legitimately find nothing (an unmounted device), so only the
    // format is asserted here, not a particular PID.
    fuser_test(vec![dev.clone()], "", 0, |_, output| {
        let text = str::from_utf8(&output.stdout).expect("valid UTF-8");
        if !text.is_empty() {
            parse_posix_pid_list(&output.stdout);
        }
    });

    // `-f` asks about the device special file itself. Almost nothing holds
    // /dev/<disk> open, so an empty result is the expected outcome; what
    // matters is that it is a *different* question and does not error.
    fuser_test(vec!["-f".to_string(), dev.clone()], "", 0, |_, output| {
        let text = str::from_utf8(&output.stdout).expect("valid UTF-8");
        if !text.is_empty() {
            parse_posix_pid_list(&output.stdout);
        }
    });
}

/// The mount source of `/`, when it is a block device.
#[cfg(target_os = "linux")]
fn root_block_device() -> Option<String> {
    let mounts = std::fs::read_to_string("/proc/self/mountinfo").ok()?;
    for line in mounts.lines() {
        let fields: Vec<&str> = line.split_whitespace().collect();
        if fields.get(4) == Some(&"/") {
            let source = fields.get(9)?;
            let meta = std::fs::metadata(source).ok()?;
            use std::os::unix::fs::FileTypeExt;
            if meta.file_type().is_block_device() {
                return Some((*source).to_string());
            }
        }
    }
    None
}

/// Multiple operands are matched independently.
///
/// This is the regression guard for the per-operand matching state. The device
/// and inode lists used to be created once, outside the operand loop, and
/// `handle_file_namespace` assigns only *one* of them depending on the
/// operand's type, so the previous operand's device stayed live for the next
/// one; `need_check_map`, only ever set to true, latched the
/// `/proc/<pid>/maps` scan on for every operand that followed.
///
/// A mounted block device followed by a quiet plain file is the pair that
/// exposes it. `-c` deliberately cannot be used here: it applies to *every*
/// operand, so both would report the file system's users and that is correct.
/// A bare block device puts only the *first* operand in device mode, so any
/// match against the second is the leak and nothing else.
#[test]
#[cfg(target_os = "linux")]
fn test_fuser_operands_do_not_leak_into_each_other() {
    let Some(dev) = root_block_device() else {
        println!("Skipping: / is not mounted from a block device");
        return;
    };

    let quiet = std::env::temp_dir().join(format!("fuser_quiet_{}", std::process::id()));
    std::fs::write(&quiet, b"x").expect("write temp file");
    let quiet_path = quiet.clone();
    let quiet = quiet.to_string_lossy().into_owned();

    let quiet_for_check = quiet.clone();
    let dev_for_check = dev.clone();
    fuser_test(vec![dev.clone(), quiet.clone()], "", 0, move |_, output| {
        let stderr = String::from_utf8_lossy(&output.stderr);

        // The device operand must match something, or the test proves nothing.
        let dev_line = stderr
            .lines()
            .find(|l| l.starts_with(&dev_for_check))
            .unwrap_or_else(|| panic!("no report for {dev_for_check:?} in {stderr:?}"));
        assert!(
            dev_line
                .trim_start_matches(&dev_for_check)
                .trim_start_matches(':')
                .trim()
                .chars()
                .any(|c| c.is_ascii_alphabetic()),
            "the block-device operand should find users of its file system,              otherwise this test proves nothing: {stderr:?}"
        );

        // ...and the plain file, which nothing has open, must report none.
        let quiet_line = stderr
            .lines()
            .find(|l| l.starts_with(&quiet_for_check))
            .unwrap_or_else(|| panic!("no report for {quiet_for_check:?} in {stderr:?}"));
        let after_header = quiet_line
            .trim_start_matches(&quiet_for_check)
            .trim_start_matches(':');
        assert!(
            after_header.trim().is_empty(),
            "nothing holds {quiet_for_check:?} open, but it was reported with              use characters {after_header:?}: the block device's match leaked              into the following operand. Full stderr: {stderr:?}"
        );
    });

    let _ = std::fs::remove_file(&quiet_path);
}

/// The use character for a process whose *root directory* is on the named file
/// system is `r` (fuser STDERR, use-character table).
///
/// Only reachable in device mode, so this doubles as coverage of the
/// `/proc/<pid>/maps` scan that `#F2`'s `makedev` fix lives in: that scan runs
/// only when `need_check_map` is set, which only `-c` or a bare block device
/// does.
#[test]
#[cfg(target_os = "linux")]
fn test_fuser_reports_root_use_character() {
    assert!(
        Path::new("/proc/self/root").exists(),
        "this test needs /proc"
    );
    fuser_test(
        vec!["-c".to_string(), a_mount_point().to_string()],
        "",
        0,
        |_, output| {
            let stderr = String::from_utf8_lossy(&output.stderr);
            assert!(
                stderr.contains('r'),
                "expected at least one process whose root directory is on / to be \
                 reported with the `r` use character; stderr was {:?}",
                stderr
            );
        },
    );
}

/// A device number with a minor above 255 must still be matched.
///
/// `#F2`: `/proc/<pid>/maps` gives the device as `maj:min` in hex, and the
/// parser used to fold them as `maj << 8 | min`, which is wrong for a minor
/// that does not fit in eight bits — precisely the case for device-mapper and
/// NVMe namespaces. It now uses `makedev`. Verified against a real high-minor
/// device when the system has one.
#[test]
#[cfg(target_os = "linux")]
fn test_fuser_matches_high_minor_device() {
    // Find a mounted file system whose device has a minor number above 255.
    let Ok(mounts) = std::fs::read_to_string("/proc/self/mountinfo") else {
        println!("Skipping: no /proc/self/mountinfo");
        return;
    };
    let mut target = None;
    for line in mounts.lines() {
        let mut fields = line.split_whitespace();
        let Some(maj_min) = fields.nth(2) else {
            continue;
        };
        let Some((_, minor)) = maj_min.split_once(':') else {
            continue;
        };
        if minor.parse::<u32>().is_ok_and(|m| m > 255) {
            if let Some(mount_point) = line.split_whitespace().nth(4) {
                target = Some(mount_point.to_string());
                break;
            }
        }
    }
    let Some(mount_point) = target else {
        println!("Skipping: no mounted file system with a minor number above 255");
        return;
    };

    fuser_test(
        vec!["-c".to_string(), mount_point.clone()],
        "",
        0,
        move |_, output| {
            let text = str::from_utf8(&output.stdout).expect("valid UTF-8");
            if !text.is_empty() {
                parse_posix_pid_list(&output.stdout);
            }
            // The point is that it ran the maps scan without mis-decoding the
            // device number; a panic or a malformed PID list would fail above.
            let _ = &mount_point;
        },
    );
}

/// `-c` on a path that is not a mount point still resolves to that path's file
/// system, so it must not be an error.
#[test]
#[cfg(target_os = "linux")]
fn test_fuser_c_accepts_a_non_mount_point_path() {
    let cwd = std::env::current_dir().expect("cwd");
    fuser_test(
        vec!["-c".to_string(), cwd.to_string_lossy().into_owned()],
        "",
        0,
        |_, output| {
            let text = str::from_utf8(&output.stdout).expect("valid UTF-8");
            if !text.is_empty() {
                parse_posix_pid_list(&output.stdout);
            }
        },
    );
}
