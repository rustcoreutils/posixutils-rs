// SPDX-License-Identifier: MIT

use std::fs::{read_to_string, remove_file};
use std::path::Path;

use pretty_assertions::assert_eq;
use tempfile::tempdir;

use plib::testing::{run_test, TestPlan};

fn xgettext_test<P: AsRef<Path>, P2: AsRef<Path>>(
    args: &[&str],
    output_file: P,
    expected_output_file: P2,
) {
    let str_args: Vec<String> = args.iter().map(|s| String::from(*s)).collect();
    run_test(TestPlan {
        cmd: String::from("xgettext"),
        args: str_args,
        stdin_data: "".into(),
        expected_out: "".into(),
        expected_err: "".into(),
        expected_exit_code: 0,
    });

    let output = read_to_string(output_file).expect("Unable to open po-file");
    let expected_out = read_to_string(expected_output_file).unwrap();
    assert_eq!(output, expected_out);
}

#[test]
fn test_xgettext_no_arg() {
    run_test(TestPlan {
        cmd: String::from("xgettext"),
        args: vec![],
        stdin_data: "".into(),
        expected_out: "".into(),
        expected_err: "xgettext: no input file given\n".into(),
        expected_exit_code: 1,
    });
}

#[test]
fn test_xgettext() {
    let output_file = "messages.pot";
    xgettext_test(
        &["tests/xgettext/test_gettext.rs"],
        output_file,
        "tests/xgettext/test_gettext_no_lines.pot",
    );
    let _ = remove_file(output_file);
}

#[test]
fn test_xgettext_domain() {
    let output_file = "domain.pot";
    xgettext_test(
        &["-d", "domain", "tests/xgettext/test_gettext.rs"],
        output_file,
        "tests/xgettext/test_gettext_no_lines.pot",
    );
    let _ = remove_file(output_file);
}

#[test]
fn test_xgettext_pathname() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    xgettext_test(
        &[
            "-p",
            &temp_dir.path().to_str().unwrap(),
            "tests/xgettext/test_gettext.rs",
        ],
        temp_dir.path().join("messages.pot"),
        "tests/xgettext/test_gettext_no_lines.pot",
    );
}

#[test]
fn test_xgettext_domain_pathname() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    xgettext_test(
        &[
            "-d",
            "domain",
            "-p",
            &temp_dir.path().to_str().unwrap(),
            "tests/xgettext/test_gettext.rs",
        ],
        temp_dir.path().join("domain.pot"),
        "tests/xgettext/test_gettext_no_lines.pot",
    );
}

#[test]
fn test_xgettext_pathname_lines() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    xgettext_test(
        &[
            "-n",
            "-p",
            &temp_dir.path().to_str().unwrap(),
            "tests/xgettext/test_gettext.rs",
        ],
        temp_dir.path().join("messages.pot"),
        "tests/xgettext/test_gettext.pot",
    );
}

#[test]
fn test_clap() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    xgettext_test(
        &[
            "-n",
            "-p",
            &temp_dir.path().to_str().unwrap(),
            "tests/xgettext/test_clap.rs",
        ],
        temp_dir.path().join("messages.pot"),
        "tests/xgettext/test_clap.pot",
    );
}

#[test]
fn test_xgettext_ngettext() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    xgettext_test(
        &[
            "-n",
            "-p",
            &temp_dir.path().to_str().unwrap(),
            "tests/xgettext/test_ngettext.rs",
        ],
        temp_dir.path().join("messages.pot"),
        "tests/xgettext/test_ngettext.pot",
    );
}

#[test]
fn test_xgettext_pgettext() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    xgettext_test(
        &[
            "-n",
            "-p",
            &temp_dir.path().to_str().unwrap(),
            "tests/xgettext/test_pgettext.rs",
        ],
        temp_dir.path().join("messages.pot"),
        "tests/xgettext/test_pgettext.pot",
    );
}

#[test]
fn test_xgettext_npgettext() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    xgettext_test(
        &[
            "-n",
            "-p",
            &temp_dir.path().to_str().unwrap(),
            "tests/xgettext/test_npgettext.rs",
        ],
        temp_dir.path().join("messages.pot"),
        "tests/xgettext/test_npgettext.pot",
    );
}

#[test]
fn test_xgettext_c_file() {
    let temp_dir = tempdir().expect("Unable to create temporary directory");
    xgettext_test(
        &[
            "-n",
            "-p",
            &temp_dir.path().to_str().unwrap(),
            "tests/xgettext/test_c.c",
        ],
        temp_dir.path().join("messages.pot"),
        "tests/xgettext/test_c.pot",
    );
}
