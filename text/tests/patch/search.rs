//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! How far patch looks for a hunk's text.

use super::{cleanup_test_dir, run_patch_capture, setup_test_dir};
use std::fs;

// POSIX asks for a scan of "at least 1 000 bytes" either way; GNU patch scans
// the whole file. Debian's glibc patch local-ld-multiarch.diff places a hunk
// at line 507 of elf/Makefile whose text now sits at line 1557, after the
// patches before it in the series.
#[test]
fn test_patch_hunk_found_far_from_its_line() {
    let dir = setup_test_dir("far_hunk");
    let mut text = String::new();
    for i in 0..3000 {
        text.push_str(&format!("line {}\n", i));
    }
    fs::write(dir.join("f.txt"), &text).unwrap();
    let patch = dir.join("p");
    fs::write(
        &patch,
        "--- a/f.txt\n+++ b/f.txt\n@@ -5,3 +5,3 @@\n line 2500\n-line 2501\n+changed\n line 2502\n",
    )
    .unwrap();
    let (code, err) = run_patch_capture(vec![
        String::from("-d"),
        dir.to_str().unwrap().to_string(),
        String::from("-p1"),
        String::from("-F"),
        String::from("0"),
        String::from("-i"),
        patch.to_str().unwrap().to_string(),
    ]);
    assert_eq!(code, 0, "stderr: {}", err);
    let out = fs::read_to_string(dir.join("f.txt")).unwrap();
    assert!(out.contains("line 2500\nchanged\nline 2502\n"));
    assert!(!out.contains("line 2501\n"));
    cleanup_test_dir(&dir);
}
