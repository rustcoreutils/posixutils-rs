//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use plib::testing::{run_test, TestPlan};
use std::fs::{self, File};
use std::io::Write;
use std::path::PathBuf;
use tempfile::TempDir;

/// Create a temporary .po file for testing
fn create_temp_po_file(content: &str) -> (TempDir, PathBuf) {
    let temp_dir = TempDir::new().unwrap();
    let po_path = temp_dir.path().join("test.po");
    let mut file = File::create(&po_path).unwrap();
    write!(file, "{}", content).unwrap();
    (temp_dir, po_path)
}

/// Test msgfmt with simple .po file
#[test]
fn test_msgfmt_simple() {
    let po_content = r#"
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"

msgid "Hello"
msgstr "Hola"
"#;

    let (temp_dir, po_path) = create_temp_po_file(po_content);
    let mo_path = temp_dir.path().join("test.mo");

    run_test(TestPlan {
        cmd: String::from("msgfmt"),
        args: vec![
            String::from("-o"),
            mo_path.to_str().unwrap().to_string(),
            po_path.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    // Verify the .mo file was created
    assert!(mo_path.exists(), "Output .mo file should exist");

    // Check it has valid .mo magic number
    let data = fs::read(&mo_path).unwrap();
    assert!(data.len() >= 4, "MO file should have at least 4 bytes");
    // Magic number should be 0x950412de (little-endian)
    assert_eq!(data[0], 0xde);
    assert_eq!(data[1], 0x12);
    assert_eq!(data[2], 0x04);
    assert_eq!(data[3], 0x95);
}

/// Test msgfmt with plural forms
#[test]
fn test_msgfmt_plural() {
    let po_content = r#"
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"
"Plural-Forms: nplurals=2; plural=(n != 1);\n"

msgid "One file"
msgid_plural "%d files"
msgstr[0] "Un archivo"
msgstr[1] "%d archivos"
"#;

    let (temp_dir, po_path) = create_temp_po_file(po_content);
    let mo_path = temp_dir.path().join("plural.mo");

    run_test(TestPlan {
        cmd: String::from("msgfmt"),
        args: vec![
            String::from("-o"),
            mo_path.to_str().unwrap().to_string(),
            po_path.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert!(mo_path.exists(), "Output .mo file should exist");
}

/// Test msgfmt skips fuzzy entries by default
#[test]
fn test_msgfmt_skip_fuzzy() {
    let po_content = r#"
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"

#, fuzzy
msgid "Fuzzy message"
msgstr "Mensaje difuso"

msgid "Normal message"
msgstr "Mensaje normal"
"#;

    let (temp_dir, po_path) = create_temp_po_file(po_content);
    let mo_path = temp_dir.path().join("fuzzy.mo");

    run_test(TestPlan {
        cmd: String::from("msgfmt"),
        args: vec![
            String::from("-o"),
            mo_path.to_str().unwrap().to_string(),
            po_path.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert!(mo_path.exists());
}

/// Test msgfmt with -f flag includes fuzzy entries
#[test]
fn test_msgfmt_include_fuzzy() {
    let po_content = r#"
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"

#, fuzzy
msgid "Fuzzy message"
msgstr "Mensaje difuso"
"#;

    let (temp_dir, po_path) = create_temp_po_file(po_content);
    let mo_path = temp_dir.path().join("with_fuzzy.mo");

    run_test(TestPlan {
        cmd: String::from("msgfmt"),
        args: vec![
            String::from("-f"),
            String::from("-o"),
            mo_path.to_str().unwrap().to_string(),
            po_path.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert!(mo_path.exists());
}

/// Test msgfmt with empty .po file
#[test]
fn test_msgfmt_empty() {
    let po_content = r#"
msgid ""
msgstr ""
"Content-Type: text/plain; charset=UTF-8\n"
"#;

    let (temp_dir, po_path) = create_temp_po_file(po_content);
    let mo_path = temp_dir.path().join("empty.mo");

    run_test(TestPlan {
        cmd: String::from("msgfmt"),
        args: vec![
            String::from("-o"),
            mo_path.to_str().unwrap().to_string(),
            po_path.to_str().unwrap().to_string(),
        ],
        stdin_data: String::new(),
        expected_out: String::new(),
        expected_err: String::new(),
        expected_exit_code: 0,
    });

    assert!(mo_path.exists());
}
