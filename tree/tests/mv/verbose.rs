//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! GNU `mv -v`: each rename is written to standard output as `renamed 'source' -> 'dest'`; a
//! move across filesystems lists what it makes, copies and removes, in GNU coreutils 9.4's
//! wording.  findutils' build runs `mv -v bin/$i bin/$i.findutils`.

use std::fs;
use std::os::unix::fs::MetadataExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use plib::testing::get_binary_path;

fn scratch(tag: &str) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("mv_verbose_{tag}"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    dir
}

/// A fresh directory on a second filesystem, or `None` (the test is then skipped) when there is
/// no second filesystem to move to.
fn other_fs(tag: &str) -> Option<PathBuf> {
    let root = Path::new(option_env!("OTHER_PARTITION_TMPDIR").unwrap_or("/dev/shm"));
    let here = fs::metadata(env!("CARGO_TARGET_TMPDIR")).unwrap().dev();
    match fs::metadata(root) {
        Ok(md) if md.dev() != here => {}
        _ => {
            eprintln!("skipping: no second filesystem at {}", root.display());
            return None;
        }
    }
    let dir = root.join(format!("mv_verbose_{tag}"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir(&dir).unwrap();
    Some(dir)
}

/// Run mv in `cwd`; return (stdout, stderr, exit code).
fn mv_in(cwd: &Path, args: &[&str]) -> (String, String, i32) {
    let output = Command::new(get_binary_path("mv"))
        .args(args)
        .current_dir(cwd)
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute mv");
    (
        String::from_utf8_lossy(&output.stdout).into_owned(),
        String::from_utf8_lossy(&output.stderr).into_owned(),
        output.status.code().unwrap_or(-1),
    )
}

#[test]
fn test_mv_verbose_rename() {
    let dir = scratch("rename");
    for name in ["a", "b", "sp ace"] {
        fs::write(dir.join(name), name).unwrap();
    }
    fs::create_dir_all(dir.join("d/sub")).unwrap();
    fs::create_dir(dir.join("t")).unwrap();

    assert_eq!(
        mv_in(&dir, &["-v", "a", "c"]),
        ("renamed 'a' -> 'c'\n".into(), String::new(), 0)
    );
    assert_eq!(
        mv_in(&dir, &["-v", "b", "d", "t"]),
        (
            "renamed 'b' -> 't/b'\nrenamed 'd' -> 't/d'\n".into(),
            String::new(),
            0
        )
    );
    assert_eq!(
        mv_in(&dir, &["-v", "sp ace", "x y"]),
        ("renamed 'sp ace' -> 'x y'\n".into(), String::new(), 0)
    );

    // Nothing moved, nothing written.
    let (out, _, status) = mv_in(&dir, &["-v", "c", "c"]);
    assert_eq!((out.as_str(), status), ("", 1));

    fs::remove_dir_all(&dir).unwrap();
}

/// Across filesystems: each directory made, each file copied, then each file and directory of
/// the source removed.
#[test]
fn test_mv_verbose_across_filesystems() {
    let Some(other) = other_fs("across") else {
        return;
    };
    let dir = scratch("across");
    fs::create_dir_all(dir.join("d/sub")).unwrap();
    fs::write(dir.join("d/sub/g"), "g").unwrap();
    fs::write(dir.join("f"), "f").unwrap();
    let o = other.display();

    let expected = format!(
        "created directory '{o}/d'\n\
         created directory '{o}/d/sub'\n\
         copied 'd/sub/g' -> '{o}/d/sub/g'\n\
         removed 'd/sub/g'\n\
         removed directory 'd/sub'\n\
         removed directory 'd'\n"
    );
    assert_eq!(
        mv_in(&dir, &["-v", "d", &format!("{o}/d")]),
        (expected, String::new(), 0)
    );
    assert_eq!(
        mv_in(&dir, &["-v", "f", &format!("{o}/")]),
        (
            format!("copied 'f' -> '{o}/f'\nremoved 'f'\n"),
            String::new(),
            0
        )
    );
    assert!(other.join("d/sub/g").exists() && other.join("f").exists());

    fs::remove_dir_all(&dir).unwrap();
    fs::remove_dir_all(&other).unwrap();
}
