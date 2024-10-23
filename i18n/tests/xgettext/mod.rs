// SPDX-License-Identifier: MIT

use std::fs::{read_to_string, remove_file};

use tempfile::tempdir;

use plib::testing::{run_test, TestPlan};

fn xgettext_test(
    args: &[&str],
    input: String,
    expected_output: String,
    expected_error: String,
    expected_exit_code: i32,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();
    run_test(TestPlan {
        cmd: String::from("xgettext"),
        args: str_args,
        stdin_data: input,
        expected_out: expected_output,
        expected_err: expected_error,
        expected_exit_code,
    })
}

#[test]
fn test_xgettext_no_arg() {
    xgettext_test(
        &[],
        "".into(),
        "".into(),
        "xgettext: no input file given\n".into(),
        1,
    );
}

#[test]
fn test_xgettext() {
    let file_path = "messages.pot";

    xgettext_test(
        &["tests/xgettext/test_gettext.rs"],
        "".into(),
        "".into(),
        "".into(),
        0,
    );

    let content = read_to_string(file_path).expect("Unable to open po-file");
    assert_eq!(
        content,
        r#"msgid "Hello, world!"
msgstr ""

"#
    );
    let _ = remove_file(file_path);
}

#[test]
fn test_xgettext_domain() {
    let file_path = "domain.pot";

    xgettext_test(
        &["-d", "domain", "tests/xgettext/test_gettext.rs"],
        "".into(),
        "".into(),
        "".into(),
        0,
    );

    let content = read_to_string(file_path).expect("Unable to open po-file");
    assert_eq!(
        content,
        r#"msgid "Hello, world!"
msgstr ""

"#
    );
    let _ = remove_file(file_path);
}

#[test]
fn test_xgettext_pathname() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    let file_path = temp_dir.path().join("messages.pot");

    xgettext_test(
        &[
            "-p",
            &temp_dir.path().to_str().unwrap(),
            "tests/xgettext/test_gettext.rs",
        ],
        "".into(),
        "".into(),
        "".into(),
        0,
    );

    let content = read_to_string(file_path).expect("Unable to open po-file");
    assert_eq!(
        content,
        r#"msgid "Hello, world!"
msgstr ""

"#
    );
}

#[test]
fn test_xgettext_domain_pathname() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    let file_path = temp_dir.path().join("domain.pot");

    xgettext_test(
        &[
            "-d",
            "domain",
            "-p",
            &temp_dir.path().to_str().unwrap(),
            "tests/xgettext/test_gettext.rs",
        ],
        "".into(),
        "".into(),
        "".into(),
        0,
    );

    let content = read_to_string(file_path).expect("Unable to open po-file");
    assert_eq!(
        content,
        r#"msgid "Hello, world!"
msgstr ""

"#
    );
}

#[test]
fn test_xgettext_pathname_lines() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    let file_path = temp_dir.path().join("messages.pot");

    xgettext_test(
        &[
            "-n",
            "-p",
            &temp_dir.path().to_str().unwrap(),
            "tests/xgettext/test_gettext.rs",
        ],
        "".into(),
        "".into(),
        "".into(),
        0,
    );

    let content = read_to_string(file_path).expect("Unable to open po-file");
    assert_eq!(
        content,
        r#"#: tests/xgettext/test_gettext.rs:6
msgid "Hello, world!"
msgstr ""

"#
    );
}

#[ignore]
#[test]
fn test_xgettext_ngettext() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    let file_path = temp_dir.path().join("messages.pot");

    xgettext_test(
        &[
            "-n",
            "-p",
            &temp_dir.path().to_str().unwrap(),
            "tests/xgettext/test_ngettext.rs",
        ],
        "".into(),
        "".into(),
        "".into(),
        0,
    );

    let content = read_to_string(file_path).expect("Unable to open po-file");
    assert_eq!(
        content,
        r#"TODO"#
    );
}

#[ignore]
#[test]
fn test_xgettext_pgettext() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    let file_path = temp_dir.path().join("messages.pot");

    xgettext_test(
        &[
            "-n",
            "-p",
            &temp_dir.path().to_str().unwrap(),
            "tests/xgettext/test_pgettext.rs",
        ],
        "".into(),
        "".into(),
        "".into(),
        0,
    );

    let content = read_to_string(file_path).expect("Unable to open po-file");
    assert_eq!(
        content,
        r#"TODO"#
    );
}

#[ignore]
#[test]
fn test_xgettext_npgettext() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    let file_path = temp_dir.path().join("messages.pot");

    xgettext_test(
        &[
            "-n",
            "-p",
            &temp_dir.path().to_str().unwrap(),
            "tests/xgettext/test_npgettext.rs",
        ],
        "".into(),
        "".into(),
        "".into(),
        0,
    );

    let content = read_to_string(file_path).expect("Unable to open po-file");
    assert_eq!(
        content,
        r#"TODO"#
    );
}
