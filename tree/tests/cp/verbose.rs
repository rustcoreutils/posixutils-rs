//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! GNU `cp -v`: each file copied, directory made and link made is written to standard output as
//! `'source' -> 'destination'`, the names quoted as GNU coreutils 9.4 quotes them.  sysvinit
//! installs with `cp -afv`.

use std::fs;
use std::os::unix::fs::symlink;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use plib::testing::get_binary_path;

fn scratch(tag: &str) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("cp_verbose_{tag}"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    dir
}

/// Run cp in `cwd`; return (stdout, stderr, exit code).
fn cp_in(cwd: &Path, args: &[&str]) -> (String, String, i32) {
    let output = Command::new(get_binary_path("cp"))
        .args(args)
        .current_dir(cwd)
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute cp");
    (
        String::from_utf8_lossy(&output.stdout).into_owned(),
        String::from_utf8_lossy(&output.stderr).into_owned(),
        output.status.code().unwrap_or(-1),
    )
}

/// One file to a new name, and several into a directory.
#[test]
fn test_cp_verbose_files() {
    let dir = scratch("files");
    fs::write(dir.join("a"), "a").unwrap();
    fs::write(dir.join("b"), "b").unwrap();
    fs::create_dir(dir.join("t")).unwrap();

    assert_eq!(
        cp_in(&dir, &["-v", "a", "c"]),
        ("'a' -> 'c'\n".into(), String::new(), 0)
    );
    assert_eq!(
        cp_in(&dir, &["-v", "a", "b", "t"]),
        ("'a' -> 't/a'\n'b' -> 't/b'\n".into(), String::new(), 0)
    );
    assert_eq!(
        cp_in(&dir, &["-fv", "a", "t/"]),
        ("'a' -> 't/a'\n".into(), String::new(), 0)
    );

    // Nothing copied, nothing written: -n keeping a destination, and a failure.
    assert_eq!(
        cp_in(&dir, &["-nv", "a", "b"]),
        (String::new(), String::new(), 0)
    );
    let (out, _, status) = cp_in(&dir, &["-v", "nonexistent", "z"]);
    assert_eq!((out.as_str(), status), ("", 1));

    fs::remove_dir_all(&dir).unwrap();
}

/// -R: every directory made and every file copied, each on its line; a directory that already
/// exists and is copied into is not one that was made, and is not listed.
#[test]
fn test_cp_verbose_recursive() {
    let dir = scratch("recursive");
    fs::create_dir_all(dir.join("d/sub")).unwrap();
    fs::write(dir.join("d/sub/g"), "g").unwrap();

    let made = "'d' -> 'e'\n'd/sub' -> 'e/sub'\n'd/sub/g' -> 'e/sub/g'\n";
    assert_eq!(
        cp_in(&dir, &["-Rv", "d", "e"]),
        (made.into(), String::new(), 0)
    );
    let into = "'d' -> 'e/d'\n'd/sub' -> 'e/d/sub'\n'd/sub/g' -> 'e/d/sub/g'\n";
    assert_eq!(
        cp_in(&dir, &["-Rv", "d", "e"]),
        (into.into(), String::new(), 0)
    );
    assert_eq!(
        cp_in(&dir, &["-Rv", "d", "e"]),
        ("'d/sub/g' -> 'e/d/sub/g'\n".into(), String::new(), 0)
    );

    // -a: a symbolic link is listed as it is made.
    symlink("sub/g", dir.join("d/ln")).unwrap();
    fs::remove_dir_all(dir.join("d/sub")).unwrap();
    assert_eq!(
        cp_in(&dir, &["-av", "d", "f"]),
        ("'d' -> 'f'\n'd/ln' -> 'f/ln'\n".into(), String::new(), 0)
    );

    fs::remove_dir_all(&dir).unwrap();
}

/// --parents: each directory made on the way is written as GNU writes it, `source -> dest`
/// without quotes, before the copied file.
#[test]
fn test_cp_verbose_parents() {
    let dir = scratch("parents");
    fs::create_dir_all(dir.join("d/sub")).unwrap();
    fs::write(dir.join("d/sub/g"), "g").unwrap();
    fs::create_dir(dir.join("t")).unwrap();

    assert_eq!(
        cp_in(&dir, &["-v", "--parents", "d/sub/g", "t"]),
        (
            "d -> t/d\nd/sub -> t/d/sub\n'd/sub/g' -> 't/d/sub/g'\n".into(),
            String::new(),
            0
        )
    );
    // The directories exist now: only the file is listed.
    assert_eq!(
        cp_in(&dir, &["-v", "--parents", "d/sub/g", "t"]),
        ("'d/sub/g' -> 't/d/sub/g'\n".into(), String::new(), 0)
    );

    fs::remove_dir_all(&dir).unwrap();
}

/// Names are quoted as GNU coreutils quotes them: in single quotes, a name holding a single
/// quote in double quotes when nothing else in it is special to the shell or to C, and
/// otherwise with each single quote as `'\''` and each unprintable byte as `$'\ooo'`.
#[test]
fn test_cp_verbose_quoting() {
    let dir = scratch("quoting");
    fs::create_dir(dir.join("t")).unwrap();
    let cases = [
        ("sp ace", "'sp ace' -> 't/sp ace'\n"),
        ("q'uote", "\"q'uote\" -> \"t/q'uote\"\n"),
        ("a'b$c", "'a'\\''b$c' -> 't/a'\\''b$c'\n"),
        ("n\nl", "'n'$'\\n''l' -> 't/n'$'\\n''l'\n"),
        ("\x01z", "''$'\\001''z' -> 't/'$'\\001''z'\n"),
        ("#q'", "\"#q'\" -> 't/#q'\\'''\n"),
        ("\n'", "''$'\\n'\\''' -> 't/'$'\\n'\\'''\n"),
    ];
    for (name, expected) in cases {
        fs::write(dir.join(name), "x").unwrap();
        assert_eq!(
            cp_in(&dir, &["-v", name, "t/"]),
            (expected.into(), String::new(), 0),
            "{name:?}"
        );
    }

    fs::remove_dir_all(&dir).unwrap();
}
