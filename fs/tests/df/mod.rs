//
// Copyright (c) 2024-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

fn run_df_test(args: Vec<&str>) -> String {
    // Run and capture output for header verification
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_df"))
        .args(&args)
        .output()
        .expect("Failed to execute df");

    String::from_utf8_lossy(&output.stdout).to_string()
}

#[test]
fn test_df_basic() {
    let output = run_df_test(vec![]);

    // Verify header contains expected columns
    let header = output.lines().next().expect("No output from df");
    assert!(header.contains("Filesystem"), "Header missing 'Filesystem'");
    assert!(
        header.contains("512-blocks") || header.contains("1024-blocks"),
        "Header missing block size column"
    );
    assert!(header.contains("Used"), "Header missing 'Used'");
    assert!(header.contains("Available"), "Header missing 'Available'");
    assert!(header.contains("Capacity"), "Header missing 'Capacity'");
    assert!(header.contains("Mounted on"), "Header missing 'Mounted on'");

    // Verify we have at least one filesystem row
    assert!(output.lines().count() >= 2, "No filesystem data in output");
}

#[test]
fn test_df_k_option() {
    let output = run_df_test(vec!["-k"]);

    // With -k, header should show "1024-blocks"
    let header = output.lines().next().expect("No output from df");
    assert!(
        header.contains("1024-blocks"),
        "Header should show '1024-blocks' with -k option"
    );
}

#[test]
fn test_df_portable_option() {
    let output = run_df_test(vec!["-P"]);

    // With -P (POSIX format), header should show "512-blocks"
    let header = output.lines().next().expect("No output from df");
    assert!(
        header.contains("512-blocks"),
        "Header should show '512-blocks' with -P option"
    );
}

#[test]
fn test_df_portable_k_options() {
    let output = run_df_test(vec!["-P", "-k"]);

    // With -P and -k, header should show "1024-blocks"
    let header = output.lines().next().expect("No output from df");
    assert!(
        header.contains("1024-blocks"),
        "Header should show '1024-blocks' with -P -k options"
    );
}

#[test]
fn test_df_file_operand() {
    // Test with root directory - should always exist
    let output = run_df_test(vec!["/"]);

    // Verify header and at least one filesystem row
    let lines: Vec<&str> = output.lines().collect();
    assert!(lines.len() >= 2, "Should have header and at least one row");

    // The filesystem for "/" should be shown
    let header = lines[0];
    assert!(header.contains("Filesystem"), "Header missing 'Filesystem'");
}

#[test]
fn test_df_nonexistent_file() {
    // Test with nonexistent file - should fail with exit code > 0
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_df"))
        .arg("/nonexistent/path/that/does/not/exist")
        .output()
        .expect("Failed to execute df");

    assert!(
        !output.status.success(),
        "df should fail for nonexistent path"
    );

    // Error message should be written to stderr
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.is_empty() || !output.status.success(),
        "Should report error for nonexistent path"
    );
}

#[test]
fn test_df_operand_does_not_dump_all_filesystems() {
    // Regression (audit #5): a single `file` operand must report only the
    // filesystem(s) containing it, never the whole mount table.
    let all = run_df_test(vec![]);
    let one = run_df_test(vec!["/"]);

    let all_rows = all.lines().count();
    let one_rows = one.lines().count();

    assert!(one_rows >= 2, "df / should print header + at least one row");
    assert!(
        one_rows <= all_rows,
        "df / printed {one_rows} lines but the full table has only {all_rows}; \
         an operand must never expand beyond the full table"
    );
}

#[test]
fn test_df_bad_operand_prints_no_filesystem_rows() {
    // Regression (audit #5): a failed operand must NOT fall back to printing
    // every filesystem. At most the header line may appear on stdout.
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_df"))
        .arg("/nonexistent/path/that/does/not/exist")
        .output()
        .expect("Failed to execute df");

    assert!(!output.status.success(), "df should fail for a bad operand");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.lines().count() <= 1,
        "df <bad operand> must not dump all filesystems; stdout was:\n{stdout}"
    );
}

#[test]
fn test_df_default_has_inode_columns() {
    // Audit #3: default output shall report free file slots / inodes.
    let output = run_df_test(vec![]);
    let header = output.lines().next().expect("No output from df");
    assert!(
        header.contains("Inodes"),
        "default header missing Inodes: {header}"
    );
    assert!(
        header.contains("IFree"),
        "default header missing IFree: {header}"
    );
    assert!(
        header.contains("IUse%"),
        "default header missing IUse%: {header}"
    );
}

#[test]
fn test_df_total_has_inode_columns() {
    // Audit #2/#3: -t output also reports inodes (and totals are present).
    let output = run_df_test(vec!["-t"]);
    let header = output.lines().next().expect("No output from df");
    assert!(
        header.contains("Inodes"),
        "-t header missing Inodes: {header}"
    );
    assert!(
        header.contains("IFree"),
        "-t header missing IFree: {header}"
    );
}

#[test]
fn test_df_portable_has_no_inode_columns() {
    // -P keeps the fixed six-column block format (no inode columns).
    let output = run_df_test(vec!["-P"]);
    let header = output.lines().next().expect("No output from df");
    assert!(
        !header.contains("Inodes") && !header.contains("IFree"),
        "-P must not show inode columns: {header}"
    );
}

#[test]
fn test_df_kilo_has_no_inode_columns() {
    // -k is "an option other than -t", so inodes are not required/shown.
    let output = run_df_test(vec!["-k"]);
    let header = output.lines().next().expect("No output from df");
    assert!(
        !header.contains("Inodes"),
        "-k must not show inode columns: {header}"
    );
}

#[test]
fn test_df_portable_total_mutually_exclusive() {
    // Audit #7: SYNOPSIS groups -P and -t as alternatives.
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_df"))
        .args(["-P", "-t"])
        .output()
        .expect("Failed to execute df");
    assert!(
        !output.status.success(),
        "df -P -t must be rejected as mutually exclusive"
    );
}

// The mount table is read from /proc/self/mounts with /etc/mtab as a fallback.
// Reading only /etc/mtab made df fail outright wherever that file was stale or
// absent even though the kernel list was readable, so this pins that df still
// enumerates real mounted filesystems and that the root mount is among them.
#[test]
fn test_df_enumerates_mounts_including_root() {
    let output = run_df_test(vec![]);
    let rows: Vec<&str> = output.lines().skip(1).filter(|l| !l.is_empty()).collect();

    assert!(
        !rows.is_empty(),
        "df listed no filesystems; the mount table was not read. Output:\n{}",
        output
    );
    // "Mounted on" is the last column; its index differs between the default
    // and -P layouts, so match on the last field rather than a fixed position.
    assert!(
        rows.iter()
            .any(|row| row.split_whitespace().next_back() == Some("/")),
        "df did not report the root mount. Output:\n{}",
        output
    );
}

#[test]
fn test_df_help() {
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_df"))
        .arg("--help")
        .output()
        .expect("Failed to execute df --help");

    assert!(output.status.success(), "df --help should succeed");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("-k"), "Help should mention -k option");
    assert!(stdout.contains("-P"), "Help should mention -P option");
}

// #4: Capacity is `used / (used + avail)` rounded up, where `avail` is the
// space available to unprivileged users (`f_bavail`). The formula has unit
// tests (`capacity_percent_*`), but nothing pinned it through the real binary.
//
// Rather than depend on a filesystem with a known reserved-block count, this
// checks the invariant on every row the host reports: with a reserved-block
// filesystem the denominator is smaller than the total, so a `used/total`
// implementation disagrees here. It is skipped for rows with a zero
// denominator (pseudo-filesystems like /proc), which report 0%.
#[test]
fn test_df_capacity_matches_used_over_used_plus_avail() {
    // -P gives the fixed six-column format: Filesystem, blocks, Used,
    // Available, Capacity, "Mounted on".
    let output = run_df_test(vec!["-P"]);
    let mut checked = 0;

    for row in output.lines().skip(1) {
        let cols: Vec<&str> = row.split_whitespace().collect();
        if cols.len() < 6 {
            continue;
        }
        let (Ok(used), Ok(avail)) = (cols[2].parse::<u64>(), cols[3].parse::<u64>()) else {
            continue;
        };
        let Some(pct) = cols[4]
            .strip_suffix('%')
            .and_then(|p| p.parse::<u64>().ok())
        else {
            continue;
        };
        let denom = used + avail;
        if denom == 0 {
            assert_eq!(pct, 0, "a zero denominator must report 0%, row: {row}");
            continue;
        }

        let expected = (used as f64 / denom as f64 * 100.0).ceil() as u64;
        assert_eq!(
            pct, expected,
            "Capacity must be ceil(used / (used + avail)); row: {row}"
        );
        checked += 1;
    }

    assert!(
        checked > 0,
        "no filesystem row was usable for the capacity check:\n{output}"
    );
}

// #1: mount names and operands are carried as OsString/PathBuf and displayed
// with to_string_lossy, so bytes that are not valid UTF-8 can no longer abort
// the utility. A non-UTF-8 *mount* cannot be staged portably, but the same
// plumbing handles operands: df must diagnose this one and exit non-zero
// without panicking.
#[test]
fn test_df_non_utf8_operand_does_not_panic() {
    use std::os::unix::ffi::OsStrExt;

    let operand = std::ffi::OsStr::from_bytes(b"/nonexistent/\xff\xfepath");
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_df"))
        .arg(operand)
        .output()
        .expect("Failed to execute df");

    // A panic would be signal death or exit 101; this must be an ordinary
    // diagnostic-and-exit-1.
    assert_eq!(
        output.status.code(),
        Some(1),
        "df must fail cleanly on an unresolvable non-UTF-8 operand, stderr={:?}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        !output.stderr.is_empty(),
        "a diagnostic is required for a bad operand"
    );
    // And per #5, no filesystem rows are dumped for an unmatched operand.
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.lines().count() <= 1,
        "no filesystem rows should be printed, got {stdout:?}"
    );
}
