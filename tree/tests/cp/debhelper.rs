//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The GNU options debhelper passes to cp for every Debian package:
//!
//! ```text
//! cp -an --reflink=auto FILE DEST.tmp              (Dh_Lib restore_file_on_clean)
//! cp --reflink=auto -a SRC... DIR/                 (dh_install, dh_installdocs, ...)
//! cp --reflink=auto SRC... DIR                     (dh_installinfo)
//! cp --reflink=auto --parents -dp REL/PATH DIR/    (dh_install, dh_installdocs, ...)
//! cp --reflink=auto --parents -a REL/EMPTYDIR DIR/ (dh_install)
//! ```

use std::fs::{self, File};
use std::os::unix::fs::{symlink, MetadataExt, PermissionsExt};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, UNIX_EPOCH};

use plib::testing::get_binary_path;

fn scratch(tag: &str) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("cp_debhelper_{tag}"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    dir
}

/// Run cp in `cwd` under umask 022 (so expected modes do not depend on the
/// test runner's); return (stderr, exit code).
fn cp_in(cwd: &Path, args: &[&str]) -> (String, i32) {
    cp_in_umask(cwd, "022", args)
}

fn cp_in_umask(cwd: &Path, umask: &str, args: &[&str]) -> (String, i32) {
    let output = Command::new("sh")
        .arg("-c")
        .arg(format!("umask {umask}; exec \"$0\" \"$@\""))
        .arg(get_binary_path("cp"))
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

const OLD: u64 = 1_000_000_000;

fn set_mtime(path: &Path, secs: u64) {
    File::open(path)
        .unwrap()
        .set_modified(UNIX_EPOCH + Duration::from_secs(secs))
        .unwrap();
}

fn mode(path: &Path) -> u32 {
    fs::symlink_metadata(path).unwrap().mode() & 0o7777
}

#[test]
fn cp_an_keeps_an_existing_destination() {
    let d = scratch("an_exists");
    fs::write(d.join("src"), "new\n").unwrap();
    fs::write(d.join("dst"), "old\n").unwrap();
    let (err, code) = cp_in(&d, &["-an", "--reflink=auto", "src", "dst"]);
    assert_eq!((err.as_str(), code), ("", 0));
    assert_eq!(fs::read_to_string(d.join("dst")).unwrap(), "old\n");
}

#[test]
fn cp_an_copies_to_a_new_name_preserving_attributes() {
    let d = scratch("an_new");
    let src = d.join("src");
    fs::write(&src, "data\n").unwrap();
    fs::set_permissions(&src, fs::Permissions::from_mode(0o640)).unwrap();
    set_mtime(&src, OLD);
    let (err, code) = cp_in(&d, &["-an", "--reflink=auto", "src", "dst.tmp"]);
    assert_eq!((err.as_str(), code), ("", 0));
    let dst = d.join("dst.tmp");
    assert_eq!(fs::read_to_string(&dst).unwrap(), "data\n");
    assert_eq!(mode(&dst), 0o640);
    assert_eq!(fs::metadata(&dst).unwrap().mtime(), OLD as i64);
}

#[test]
fn cp_n_recursive_merges_without_overwriting() {
    let d = scratch("n_recursive");
    fs::create_dir_all(d.join("src")).unwrap();
    fs::write(d.join("src/a"), "new a\n").unwrap();
    fs::write(d.join("src/b"), "new b\n").unwrap();
    fs::create_dir_all(d.join("dst/src")).unwrap();
    fs::write(d.join("dst/src/a"), "old a\n").unwrap();
    // Only the user can create entries in `dst`, whatever the umask and the group, so the
    // `dst/src` found there takes the source's attributes (tests/cp/existing_dir.rs has the
    // group-writable cases).
    fs::set_permissions(d.join("dst"), fs::Permissions::from_mode(0o755)).unwrap();
    let (err, code) = cp_in(&d, &["-an", "src", "dst/"]);
    assert_eq!((err.as_str(), code), ("", 0));
    assert_eq!(fs::read_to_string(d.join("dst/src/a")).unwrap(), "old a\n");
    assert_eq!(fs::read_to_string(d.join("dst/src/b")).unwrap(), "new b\n");
}

#[test]
fn cp_a_archives_a_tree() {
    let d = scratch("a_tree");
    fs::create_dir_all(d.join("src/sub")).unwrap();
    fs::write(d.join("src/sub/f"), "f\n").unwrap();
    fs::hard_link(d.join("src/sub/f"), d.join("src/h")).unwrap();
    symlink("sub/f", d.join("src/l")).unwrap();
    symlink("nowhere", d.join("src/dangling")).unwrap();
    fs::set_permissions(d.join("src/sub"), fs::Permissions::from_mode(0o750)).unwrap();
    fs::set_permissions(d.join("src/sub/f"), fs::Permissions::from_mode(0o604)).unwrap();
    set_mtime(&d.join("src/sub/f"), OLD);
    set_mtime(&d.join("src/sub"), OLD + 1);
    fs::create_dir(d.join("out")).unwrap();

    let (err, code) = cp_in(&d, &["--reflink=auto", "-a", "src", "out/"]);
    assert_eq!((err.as_str(), code), ("", 0));

    let out = d.join("out/src");
    assert_eq!(fs::read_link(out.join("l")).unwrap(), Path::new("sub/f"));
    assert_eq!(
        fs::read_link(out.join("dangling")).unwrap(),
        Path::new("nowhere")
    );
    let f = fs::metadata(out.join("sub/f")).unwrap();
    let h = fs::metadata(out.join("h")).unwrap();
    assert_eq!(f.ino(), h.ino(), "hard links are preserved");
    assert_eq!(mode(&out.join("sub")), 0o750);
    assert_eq!(mode(&out.join("sub/f")), 0o604);
    assert_eq!(f.mtime(), OLD as i64);
    assert_eq!(
        fs::metadata(out.join("sub")).unwrap().mtime(),
        (OLD + 1) as i64
    );
}

#[test]
fn cp_a_several_sources_into_a_directory() {
    let d = scratch("a_several");
    fs::write(d.join("x"), "x\n").unwrap();
    symlink("x", d.join("y")).unwrap();
    fs::create_dir(d.join("out")).unwrap();
    let (err, code) = cp_in(&d, &["--reflink=auto", "-a", "x", "y", "out/"]);
    assert_eq!((err.as_str(), code), ("", 0));
    assert_eq!(fs::read_to_string(d.join("out/x")).unwrap(), "x\n");
    assert_eq!(fs::read_link(d.join("out/y")).unwrap(), Path::new("x"));
}

#[test]
fn cp_reflink_auto_without_a() {
    let d = scratch("reflink_plain");
    fs::write(d.join("x"), "x\n").unwrap();
    fs::create_dir(d.join("info")).unwrap();
    let (err, code) = cp_in(&d, &["--reflink=auto", "x", "info"]);
    assert_eq!((err.as_str(), code), ("", 0));
    assert_eq!(fs::read_to_string(d.join("info/x")).unwrap(), "x\n");
}

#[test]
fn cp_reflink_other_than_auto_is_refused() {
    let d = scratch("reflink_always");
    fs::write(d.join("x"), "x\n").unwrap();
    for arg in ["--reflink=always", "--reflink=never", "--reflink"] {
        let (_, code) = cp_in(&d, &[arg, "x", "y"]);
        assert_ne!(code, 0, "{arg}");
        assert!(!d.join("y").exists(), "{arg}");
    }
}

#[test]
fn cp_d_copies_a_link_as_a_link() {
    let d = scratch("d_link");
    fs::write(d.join("x"), "x\n").unwrap();
    symlink("x", d.join("l")).unwrap();
    let (err, code) = cp_in(&d, &["-d", "l", "m"]);
    assert_eq!((err.as_str(), code), ("", 0));
    assert_eq!(fs::read_link(d.join("m")).unwrap(), Path::new("x"));
}

/// `find pkg -type f -o -type l | xargs -I {} cp --parents -dp {} DEST/`
#[test]
fn cp_parents_dp_recreates_the_path() {
    let d = scratch("parents_dp");
    fs::create_dir_all(d.join("doc/a/b")).unwrap();
    fs::write(d.join("doc/a/b/f"), "f\n").unwrap();
    symlink("f", d.join("doc/a/b/l")).unwrap();
    fs::set_permissions(d.join("doc/a"), fs::Permissions::from_mode(0o750)).unwrap();
    fs::set_permissions(d.join("doc/a/b"), fs::Permissions::from_mode(0o705)).unwrap();
    set_mtime(&d.join("doc/a/b/f"), OLD);
    fs::create_dir(d.join("out")).unwrap();

    for rel in ["doc/a/b/f", "doc/a/b/l"] {
        let (err, code) = cp_in_umask(
            &d,
            "077",
            &["--reflink=auto", "--parents", "-dp", rel, "out/"],
        );
        assert_eq!((err.as_str(), code), ("", 0), "{rel}");
    }
    let out = d.join("out/doc");
    assert_eq!(fs::read_to_string(out.join("a/b/f")).unwrap(), "f\n");
    assert_eq!(fs::metadata(out.join("a/b/f")).unwrap().mtime(), OLD as i64);
    assert_eq!(fs::read_link(out.join("a/b/l")).unwrap(), Path::new("f"));
    // -p carries the source directories' modes onto the ones --parents made.
    assert_eq!(mode(&out.join("a")), 0o750);
    assert_eq!(mode(&out.join("a/b")), 0o705);
}

#[test]
fn cp_parents_without_p_masks_new_directories() {
    let d = scratch("parents_plain");
    fs::create_dir_all(d.join("a/b")).unwrap();
    fs::write(d.join("a/b/f"), "f\n").unwrap();
    fs::set_permissions(d.join("a"), fs::Permissions::from_mode(0o755)).unwrap();
    fs::create_dir(d.join("out")).unwrap();
    let (err, code) = cp_in_umask(&d, "077", &["--parents", "a/b/f", "out"]);
    assert_eq!((err.as_str(), code), ("", 0));
    assert_eq!(fs::read_to_string(d.join("out/a/b/f")).unwrap(), "f\n");
    assert_eq!(mode(&d.join("out/a")), 0o700);
}

#[test]
fn cp_parents_keeps_existing_directories() {
    let d = scratch("parents_existing");
    fs::create_dir_all(d.join("a/b")).unwrap();
    fs::write(d.join("a/b/f"), "f\n").unwrap();
    fs::set_permissions(d.join("a"), fs::Permissions::from_mode(0o700)).unwrap();
    fs::create_dir_all(d.join("out/a")).unwrap();
    fs::set_permissions(d.join("out/a"), fs::Permissions::from_mode(0o755)).unwrap();
    let (err, code) = cp_in(&d, &["--parents", "-dp", "a/b/f", "out/"]);
    assert_eq!((err.as_str(), code), ("", 0));
    assert_eq!(mode(&d.join("out/a")), 0o755);
    assert_eq!(fs::read_to_string(d.join("out/a/b/f")).unwrap(), "f\n");
}

/// The second pass dh_install makes, for empty directories.
#[test]
fn cp_parents_a_copies_an_empty_directory() {
    let d = scratch("parents_empty_dir");
    fs::create_dir_all(d.join("pkg/x/empty")).unwrap();
    fs::set_permissions(d.join("pkg/x/empty"), fs::Permissions::from_mode(0o710)).unwrap();
    fs::create_dir(d.join("out")).unwrap();
    let (err, code) = cp_in(
        &d,
        &["--reflink=auto", "--parents", "-a", "pkg/x/empty", "out/"],
    );
    assert_eq!((err.as_str(), code), ("", 0));
    assert!(fs::metadata(d.join("out/pkg/x/empty")).unwrap().is_dir());
    assert_eq!(mode(&d.join("out/pkg/x/empty")), 0o710);
}

#[test]
fn cp_parents_needs_a_directory_target() {
    let d = scratch("parents_not_dir");
    fs::write(d.join("x"), "x\n").unwrap();
    fs::write(d.join("file"), "").unwrap();
    for target in ["file", "missing"] {
        let (err, code) = cp_in(&d, &["--parents", "x", target]);
        assert_ne!(code, 0, "{target}");
        assert!(err.contains("--parents"), "{target}: {err}");
    }
}
