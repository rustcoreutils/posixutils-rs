//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Start-up file handling.

use super::capitalized::capitalized_fixture;
use crate::common::{copy_test_data, create_temp_mailrc, mailx_env};

/// A `Copy` command in a start-up file is diagnosed (case-folded match).
#[test]
fn startup_copy_is_diagnosed() {
    let mbox = copy_test_data("testdata.mbox");
    let mailrc = create_temp_mailrc("Copy 1\n");

    let output = mailx_env(
        &["-N", "-f", mbox.path().to_str().unwrap()],
        "quit\n",
        &[("MAILRC", mailrc.path().to_str().unwrap())],
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("not valid in a start-up file"),
        "Copy in a start-up file should be diagnosed: {}",
        stderr
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}

/// Lowercase `copy`/`save` are legal in a start-up file; only the capitalized
/// forms are not (spec 104557-104559).
#[test]
fn startup_rejects_only_the_capitalized_file_commands() {
    for (line, rejected) in [
        ("copy", false),
        ("save", false),
        ("Copy 1", true),
        ("Save 1", true),
    ] {
        let mbox = capitalized_fixture();
        let mbox_path = mbox.path().to_str().unwrap().to_string();
        let rc = create_temp_mailrc(&format!("{}\n", line));
        let rc_path = rc.path().to_str().unwrap().to_string();

        let output = mailx_env(&["-N", "-f", &mbox_path], "exit\n", &[("MAILRC", &rc_path)]);
        let err = String::from_utf8_lossy(&output.stderr);
        assert_eq!(
            err.contains("not valid in a start-up file"),
            rejected,
            "start-up handling of `{}` is wrong: {}",
            line,
            err
        );
        assert_eq!(output.status.code(), Some(0), "exit status");
    }
}
