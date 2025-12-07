//
// Copyright (c) 2024 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Integration tests for debug info generation (-g flag)
//

use crate::common::create_c_file;
use plib::testing::run_test_base;
use std::process::Command;

/// Test that -g flag generates debug sections in object file
#[test]
fn test_debug_sections_generated() {
    let c_file = create_c_file(
        "debug_test",
        r#"
int foo(int x) {
    return x + 1;
}

int main() {
    return foo(41);
}
"#,
    );
    let c_path = c_file.path().to_path_buf();
    let obj_path = std::env::temp_dir().join("pcc_debug_test.o");

    // Compile with -g -c to produce object file
    let output = run_test_base(
        "pcc",
        &vec![
            "-g".to_string(),
            "-c".to_string(),
            "-o".to_string(),
            obj_path.to_string_lossy().to_string(),
            c_path.to_string_lossy().to_string(),
        ],
        &[],
    );

    assert!(
        output.status.success(),
        "pcc -g -c failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    // Check for debug sections using platform-specific tools
    #[cfg(target_os = "macos")]
    {
        // On macOS, use otool to check for __DWARF segment
        let otool_output = Command::new("otool")
            .args(["-l", obj_path.to_str().unwrap()])
            .output()
            .expect("failed to run otool");

        let otool_stdout = String::from_utf8_lossy(&otool_output.stdout);
        assert!(
            otool_stdout.contains("__debug_line") || otool_stdout.contains("__DWARF"),
            "No debug sections found in object file. otool output:\n{}",
            otool_stdout
        );
    }

    #[cfg(target_os = "linux")]
    {
        // On Linux, use objdump or readelf to check for .debug sections
        let objdump_output = Command::new("objdump")
            .args(["-h", obj_path.to_str().unwrap()])
            .output()
            .expect("failed to run objdump");

        let objdump_stdout = String::from_utf8_lossy(&objdump_output.stdout);
        assert!(
            objdump_stdout.contains(".debug_line") || objdump_stdout.contains(".debug_info"),
            "No debug sections found in object file. objdump output:\n{}",
            objdump_stdout
        );
    }

    // Cleanup
    let _ = std::fs::remove_file(&obj_path);
}

/// Test that basic CFI directives (startproc/endproc) are emitted by default
/// Detailed CFI (cfi_def_cfa, cfi_offset) are only emitted with -g
#[test]
fn test_cfi_directives_default() {
    let c_file = create_c_file(
        "cfi_test",
        r#"
int main() {
    return 42;
}
"#,
    );
    let c_path = c_file.path().to_path_buf();

    // Compile with -S -o - to output assembly to stdout
    let output = run_test_base(
        "pcc",
        &vec![
            "-S".to_string(),
            "-o".to_string(),
            "-".to_string(),
            c_path.to_string_lossy().to_string(),
        ],
        &[],
    );

    assert!(
        output.status.success(),
        "pcc -S -o - failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let asm = String::from_utf8_lossy(&output.stdout);

    // Check for basic CFI directives (always emitted by default)
    assert!(
        asm.contains(".cfi_startproc"),
        "Missing .cfi_startproc in assembly output"
    );
    assert!(
        asm.contains(".cfi_endproc"),
        "Missing .cfi_endproc in assembly output"
    );
    // Detailed CFI (cfi_def_cfa, cfi_offset) are only emitted with -g
    assert!(
        !asm.contains(".cfi_def_cfa"),
        "Unexpected .cfi_def_cfa in assembly output without -g"
    );
}

/// Test that --fno-unwind-tables disables CFI directives
#[test]
fn test_cfi_directives_disabled() {
    let c_file = create_c_file(
        "no_cfi_test",
        r#"
int main() {
    return 42;
}
"#,
    );
    let c_path = c_file.path().to_path_buf();

    // Compile with -S -o - --fno-unwind-tables
    let output = run_test_base(
        "pcc",
        &vec![
            "-S".to_string(),
            "-o".to_string(),
            "-".to_string(),
            "--fno-unwind-tables".to_string(),
            c_path.to_string_lossy().to_string(),
        ],
        &[],
    );

    assert!(
        output.status.success(),
        "pcc -S -o - --fno-unwind-tables failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let asm = String::from_utf8_lossy(&output.stdout);

    // Check that CFI directives are NOT present
    assert!(
        !asm.contains(".cfi_startproc"),
        "Unexpected .cfi_startproc in assembly output with --fno-unwind-tables"
    );
    assert!(
        !asm.contains(".cfi_endproc"),
        "Unexpected .cfi_endproc in assembly output with --fno-unwind-tables"
    );
}

/// Test that -g emits .file, .loc, and detailed CFI directives
#[test]
fn test_debug_file_loc_directives() {
    let c_file = create_c_file(
        "file_loc_test",
        r#"
int main() {
    return 42;
}
"#,
    );
    let c_path = c_file.path().to_path_buf();

    // Compile with -g -S -o - to see generated assembly
    let output = run_test_base(
        "pcc",
        &vec![
            "-g".to_string(),
            "-S".to_string(),
            "-o".to_string(),
            "-".to_string(),
            c_path.to_string_lossy().to_string(),
        ],
        &[],
    );

    assert!(
        output.status.success(),
        "pcc -g -S -o - failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let asm = String::from_utf8_lossy(&output.stdout);

    // Check for .file directive
    assert!(
        asm.contains(".file 1"),
        "Missing .file directive in assembly output with -g"
    );

    // Check for .loc directive
    assert!(
        asm.contains(".loc 1"),
        "Missing .loc directive in assembly output with -g"
    );

    // Check for detailed CFI directives (only with -g)
    assert!(
        asm.contains(".cfi_def_cfa"),
        "Missing .cfi_def_cfa in assembly output with -g"
    );
}

/// Test that without -g, .file IS emitted (unconditionally) but .loc is NOT
#[test]
fn test_no_debug_no_loc() {
    let c_file = create_c_file(
        "no_debug_test",
        r#"
int main() {
    return 42;
}
"#,
    );
    let c_path = c_file.path().to_path_buf();

    // Compile without -g (using -S -o - for stdout)
    let output = run_test_base(
        "pcc",
        &vec![
            "-S".to_string(),
            "-o".to_string(),
            "-".to_string(),
            c_path.to_string_lossy().to_string(),
        ],
        &[],
    );

    assert!(
        output.status.success(),
        "pcc -S -o - failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let asm = String::from_utf8_lossy(&output.stdout);

    // .file is now emitted unconditionally
    assert!(
        asm.contains(".file 1"),
        "Missing .file directive in assembly output (should be unconditional)"
    );
    // .loc should NOT be present without -g
    assert!(
        !asm.contains(".loc 1"),
        "Unexpected .loc directive in assembly output without -g"
    );
}
