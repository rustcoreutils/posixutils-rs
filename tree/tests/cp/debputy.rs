//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `-t DIR`, as debputy (dh_debputy) materializes every package:
//!
//! ```text
//! cp --reflink=auto -t DIR FILE...   (deb_materialization.py)
//! ```

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use plib::testing::get_binary_path;

fn scratch(tag: &str) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("cp_debputy_{tag}"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    dir
}

/// Run cp in `cwd`; return (stderr, exit code).
fn cp_in(cwd: &Path, args: &[&str]) -> (String, i32) {
    let out = Command::new(get_binary_path("cp"))
        .args(args)
        .current_dir(cwd)
        .stdin(Stdio::null())
        .output()
        .unwrap();
    (
        String::from_utf8_lossy(&out.stderr).into_owned(),
        out.status.code().unwrap_or(-1),
    )
}

#[test]
fn cp_target_directory_takes_every_operand_as_a_source() {
    let dir = scratch("sources");
    fs::create_dir(dir.join("dst")).unwrap();
    fs::write(dir.join("f"), "f\n").unwrap();
    fs::write(dir.join("g"), "g\n").unwrap();
    assert_eq!(
        cp_in(&dir, &["--reflink=auto", "-t", "dst", "f", "g"]),
        (String::new(), 0)
    );
    assert_eq!(fs::read_to_string(dir.join("dst/f")).unwrap(), "f\n");
    assert_eq!(fs::read_to_string(dir.join("dst/g")).unwrap(), "g\n");
    // One source too, and the option after the operands.
    assert_eq!(cp_in(&dir, &["f", "-t", "dst/"]), (String::new(), 0));
    fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn cp_target_directory_must_be_a_directory() {
    let dir = scratch("notdir");
    fs::write(dir.join("f"), "f\n").unwrap();
    fs::write(dir.join("g"), "g\n").unwrap();
    assert_eq!(
        cp_in(&dir, &["-t", "nod", "f"]),
        (
            "cp: target directory 'nod': No such file or directory\n".to_string(),
            1
        )
    );
    assert_eq!(
        cp_in(&dir, &["-t", "f", "g"]),
        ("cp: target directory 'f': Not a directory\n".to_string(), 1)
    );
    assert_eq!(fs::read_to_string(dir.join("f")).unwrap(), "f\n");
    assert!(!dir.join("nod").exists());
    fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn cp_target_directory_needs_a_source() {
    let dir = scratch("nosource");
    fs::create_dir(dir.join("dst")).unwrap();
    let (err, code) = cp_in(&dir, &["-t", "dst"]);
    assert_eq!(code, 1, "{err}");
    assert!(!err.is_empty());
    fs::remove_dir_all(&dir).unwrap();
}
