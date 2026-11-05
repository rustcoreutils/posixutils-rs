//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Filename resolution for commands that write files.

use crate::common::{copy_test_data, mailx};

/// A filename derived from a message header must never reach the shell.
///
/// `Save`, `Copy`, `followup`, and `Followup` name their file after the author,
/// and command-mode filenames get shell word expansion (spec 104709) -- so
/// routing the two through one expander made a `From:` header a command line.
#[test]
fn author_derived_filename_never_reaches_the_shell() {
    for cmd in ["Save 1", "Copy 1", "followup 1\n."] {
        let dir = plib::tmp::TempDir::new().expect("temp dir");
        let mbox = dir.path().join("box");
        // A bare name: a path separator would be caught by the author-name
        // check on its own, which would hide whether the shell ran at all.
        std::fs::write(
            &mbox,
            "From x@example.com Fri Nov 29 10:00:00 2024\n\
             From: <`touch INJECTED`@example.com>\n\
             Subject: injection\n\
             \n\
             body\n\
             \n",
        )
        .expect("write mbox");

        let out = std::process::Command::new(plib::testing::get_binary_path("mailx"))
            .args(["-n", "-N", "-f", mbox.to_str().unwrap()])
            .current_dir(dir.path())
            .env("LC_ALL", "C")
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .and_then(|mut c| {
                use std::io::Write as _;
                c.stdin
                    .take()
                    .unwrap()
                    .write_all(format!("{}\nexit\n", cmd).as_bytes())?;
                c.wait_with_output()
            })
            .expect("run mailx");

        assert!(
            !dir.path().join("INJECTED").exists(),
            "`{}` executed a command from the From: header: {}",
            cmd,
            String::from_utf8_lossy(&out.stderr)
        );
    }
}

/// `save` must work to a target that is not a seekable, syncable regular file.
#[test]
fn save_to_a_character_device_succeeds() {
    let mbox = copy_test_data("testdata-single.mbox");
    let mbox_path = mbox.path().to_str().unwrap().to_string();

    let output = mailx(&["-n", "-N", "-f", &mbox_path], "save 1 /dev/null\nexit\n");
    let err = String::from_utf8_lossy(&output.stderr);
    assert!(
        err.trim().is_empty(),
        "saving to /dev/null reported an error: {}",
        err
    );
    assert_eq!(output.status.code(), Some(0), "exit status");
}
