//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! GNU `cp -l`: hard links instead of copies.  gcc-defaults' rules run
//! `cp -l debian/substvars.native debian/$p.substvars`.

use std::fs;
use std::os::unix::fs::{symlink, MetadataExt};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use plib::testing::get_binary_path;

fn scratch(tag: &str) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("cp_link_{tag}"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    dir
}

/// Run cp in `cwd`; return (stderr, exit code).
fn cp_in(cwd: &Path, args: &[&str]) -> (String, i32) {
    let output = Command::new(get_binary_path("cp"))
        .args(args)
        .current_dir(cwd)
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute cp");
    assert!(output.stdout.is_empty());
    (
        String::from_utf8_lossy(&output.stderr).into_owned(),
        output.status.code().unwrap_or(-1),
    )
}

/// `(st_dev, st_ino)` of `path` itself, not followed.
fn ident(path: &Path) -> (u64, u64) {
    let md = fs::symlink_metadata(path).unwrap();
    (md.dev(), md.ino())
}

#[test]
fn cp_l_makes_a_hard_link() {
    let d = scratch("file");
    fs::write(d.join("substvars.native"), "a=b\n").unwrap();
    let (err, code) = cp_in(&d, &["-l", "substvars.native", "p.substvars"]);
    assert_eq!((err.as_str(), code), ("", 0));
    assert_eq!(
        ident(&d.join("p.substvars")),
        ident(&d.join("substvars.native"))
    );
}

#[test]
fn cp_l_into_a_directory() {
    let d = scratch("into_dir");
    fs::write(d.join("f"), "x\n").unwrap();
    fs::create_dir(d.join("dir")).unwrap();
    let (err, code) = cp_in(&d, &["-l", "f", "dir"]);
    assert_eq!((err.as_str(), code), ("", 0));
    assert_eq!(ident(&d.join("dir/f")), ident(&d.join("f")));
}

#[test]
fn cp_l_refuses_an_existing_destination_without_f() {
    let d = scratch("exists");
    fs::write(d.join("src"), "new\n").unwrap();
    fs::write(d.join("dst"), "old\n").unwrap();
    let (err, code) = cp_in(&d, &["-l", "src", "dst"]);
    assert_eq!(
        (err.as_str(), code),
        (
            "cp: cannot create hard link 'dst' to 'src': File exists\n",
            1
        )
    );
    assert_eq!(fs::read_to_string(d.join("dst")).unwrap(), "old\n");

    let (err, code) = cp_in(&d, &["-lf", "src", "dst"]);
    assert_eq!((err.as_str(), code), ("", 0));
    assert_eq!(ident(&d.join("dst")), ident(&d.join("src")));
    // The link was made under a temporary name and renamed over `dst`.
    let mut names: Vec<_> = fs::read_dir(&d)
        .unwrap()
        .map(|e| e.unwrap().file_name())
        .collect();
    names.sort();
    assert_eq!(names, ["dst", "src"]);

    // Already a link to the source: nothing to do, as in GNU.
    let (err, code) = cp_in(&d, &["-l", "src", "dst"]);
    assert_eq!((err.as_str(), code), ("", 0));
}

#[test]
fn cp_l_follows_a_symlink_operand_only_by_default_without_r() {
    let d = scratch("symlink_operand");
    fs::write(d.join("f"), "x\n").unwrap();
    symlink("f", d.join("sl")).unwrap();
    let (err, code) = cp_in(&d, &["-l", "sl", "a"]);
    assert_eq!((err.as_str(), code), ("", 0));
    assert_eq!(ident(&d.join("a")), ident(&d.join("f")));

    // -P links the symbolic link itself.
    let (err, code) = cp_in(&d, &["-lP", "sl", "b"]);
    assert_eq!((err.as_str(), code), ("", 0));
    assert_eq!(ident(&d.join("b")), ident(&d.join("sl")));
}

#[test]
fn cp_lr_makes_directories_and_links_files() {
    let d = scratch("recursive");
    fs::write(d.join("f"), "x\n").unwrap();
    fs::create_dir_all(d.join("src/sub")).unwrap();
    fs::write(d.join("src/g"), "g\n").unwrap();
    fs::write(d.join("src/sub/h"), "h\n").unwrap();
    symlink("../f", d.join("src/sl")).unwrap();
    let (err, code) = cp_in(&d, &["-lR", "src", "dst"]);
    assert_eq!((err.as_str(), code), ("", 0));
    assert!(fs::symlink_metadata(d.join("dst")).unwrap().is_dir());
    assert_ne!(ident(&d.join("dst")), ident(&d.join("src")));
    assert!(fs::symlink_metadata(d.join("dst/sub")).unwrap().is_dir());
    assert_eq!(ident(&d.join("dst/g")), ident(&d.join("src/g")));
    assert_eq!(ident(&d.join("dst/sub/h")), ident(&d.join("src/sub/h")));
    // A link found in the walk is not followed without -L: it is the link
    // itself that gains a name.
    assert_eq!(ident(&d.join("dst/sl")), ident(&d.join("src/sl")));
}

#[test]
fn cp_l_without_r_omits_a_directory() {
    let d = scratch("no_r");
    fs::create_dir(d.join("dir")).unwrap();
    let (err, code) = cp_in(&d, &["-l", "dir", "dir2"]);
    assert_eq!(
        (err.as_str(), code),
        ("cp: -r not specified; omitting directory 'dir'\n", 1)
    );
}

#[test]
fn cp_lf_keeps_the_destination_when_the_link_cannot_be_made() {
    // A link across filesystems fails (EXDEV): the destination it was to
    // replace must survive.  The source is under the target directory, the
    // destination in the system temporary directory; skip if they share one.
    let d = scratch("exdev");
    fs::write(d.join("src"), "new\n").unwrap();
    let other = plib::tmp::TempDir::new().unwrap();
    let dst = other.path().join("dst");
    fs::write(&dst, "old\n").unwrap();
    if fs::metadata(&d).unwrap().dev() == fs::metadata(other.path()).unwrap().dev() {
        eprintln!("skipped: no second filesystem");
        return;
    }
    let (err, code) = cp_in(&d, &["-lf", "src", dst.to_str().unwrap()]);
    assert_eq!(code, 1, "{err}");
    assert!(err.starts_with("cp: cannot create hard link"), "{err}");
    assert_eq!(fs::read_to_string(&dst).unwrap(), "old\n");
}

#[test]
fn cp_lp_does_not_take_a_symlink_to_the_same_file_for_the_link() {
    // Two symbolic links to one file are not the same file under -P.
    let d = scratch("same_referent");
    fs::write(d.join("f"), "x\n").unwrap();
    symlink("f", d.join("s1")).unwrap();
    symlink("f", d.join("s2")).unwrap();
    let (err, code) = cp_in(&d, &["-lP", "s1", "s2"]);
    assert_eq!(
        (err.as_str(), code),
        ("cp: cannot create hard link 's2' to 's1': File exists\n", 1)
    );
    let (err, code) = cp_in(&d, &["-lPf", "s1", "s2"]);
    assert_eq!((err.as_str(), code), ("", 0));
    assert_eq!(ident(&d.join("s2")), ident(&d.join("s1")));
}
