//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! The mode of a directory cp makes, without -p. POSIX cp step 2.e creates it with the
//! source's permission bits less the umask, OR'ed with S_IRWXU so cp can fill it; step 2.g
//! then changes its permission bits to the source's less the umask, once its contents are
//! copied. A directory cp finds already there keeps its mode.

use std::fs;
use std::os::unix::fs::{MetadataExt, PermissionsExt};
use std::os::unix::process::CommandExt;
use std::path::Path;
use std::process::{Command, Stdio};

use plib::testing::get_binary_path;
use plib::tmp::tempdir;

/// Run cp in `cwd` under `umask`, set in the child only; it must succeed silently.
fn cp_under_umask(cwd: &Path, umask: libc::mode_t, args: &[&str]) {
    let mut command = Command::new(get_binary_path("cp"));
    command.args(args).current_dir(cwd).stdin(Stdio::null());
    unsafe {
        command.pre_exec(move || {
            libc::umask(umask);
            Ok(())
        });
    }
    let output = command.output().expect("failed to execute cp");
    assert_eq!(
        (
            String::from_utf8_lossy(&output.stderr).as_ref(),
            output.status.code()
        ),
        ("", Some(0)),
        "cp {args:?} under umask {umask:o}"
    );
}

fn mode(path: &Path) -> u32 {
    fs::symlink_metadata(path).unwrap().mode() & 0o7777
}

fn set_mode(path: &Path, mode: u32) {
    fs::set_permissions(path, fs::Permissions::from_mode(mode)).unwrap();
}

/// `src` (0555) holding `ro` (0555, with a file in it), `priv` (0700) and `open` (0775).
fn make_source(dir: &Path) {
    let src = dir.join("src");
    for sub in ["ro", "priv", "open"] {
        fs::create_dir_all(src.join(sub)).unwrap();
    }
    fs::write(src.join("ro/f"), "f\n").unwrap();
    set_mode(&src.join("ro"), 0o555);
    set_mode(&src.join("priv"), 0o700);
    set_mode(&src.join("open"), 0o775);
    set_mode(&src, 0o555);
}

/// Each made directory ends with exactly the source's bits less the umask: S_IRWXU, added only
/// so cp could fill it, is taken back from the 0555 ones.
#[test]
fn cp_r_made_directories_get_source_mode_less_umask() {
    // (umask, src, ro, priv, open)
    let cases = [
        (0o022, 0o555, 0o555, 0o700, 0o755),
        (0o077, 0o500, 0o500, 0o700, 0o700),
    ];
    for (umask, top, ro, private, open) in cases {
        let dir = tempdir().unwrap();
        make_source(dir.path());
        cp_under_umask(dir.path(), umask, &["-R", "src", "out"]);
        let out = dir.path().join("out");
        assert_eq!(mode(&out), top, "operand directory, umask {umask:o}");
        assert_eq!(mode(&out.join("ro")), ro, "ro, umask {umask:o}");
        assert_eq!(mode(&out.join("priv")), private, "priv, umask {umask:o}");
        assert_eq!(mode(&out.join("open")), open, "open, umask {umask:o}");
        assert_eq!(fs::read_to_string(out.join("ro/f")).unwrap(), "f\n");
    }
}

/// A destination directory that already exists keeps its mode; the ones made below it do not.
#[test]
fn cp_r_existing_directory_keeps_its_mode() {
    for (umask, ro) in [(0o022, 0o555), (0o077, 0o500)] {
        let dir = tempdir().unwrap();
        make_source(dir.path());
        let existing = dir.path().join("out/src");
        fs::create_dir_all(&existing).unwrap();
        set_mode(&existing, 0o711);
        cp_under_umask(dir.path(), umask, &["-R", "src", "out"]);
        assert_eq!(mode(&existing), 0o711, "umask {umask:o}");
        assert_eq!(mode(&existing.join("ro")), ro, "umask {umask:o}");
    }
}

/// -p gives every directory the source's mode, whatever the umask.
#[test]
fn cp_rp_directories_ignore_umask() {
    let dir = tempdir().unwrap();
    make_source(dir.path());
    cp_under_umask(dir.path(), 0o077, &["-Rp", "src", "out"]);
    let out = dir.path().join("out");
    assert_eq!(mode(&out), 0o555);
    assert_eq!(mode(&out.join("ro")), 0o555);
    assert_eq!(mode(&out.join("priv")), 0o700);
    assert_eq!(mode(&out.join("open")), 0o775);
}

/// The directories `--parents` makes on the way get their source's mode less the umask, as GNU
/// cp gives them; one found there keeps its mode.
#[test]
fn cp_parents_made_directories_get_source_mode_less_umask() {
    for (umask, a, b) in [(0o022, 0o750, 0o555), (0o077, 0o700, 0o500)] {
        let dir = tempdir().unwrap();
        fs::create_dir_all(dir.path().join("a/b")).unwrap();
        fs::write(dir.path().join("a/b/f"), "f\n").unwrap();
        set_mode(&dir.path().join("a/b"), 0o555);
        set_mode(&dir.path().join("a"), 0o750);
        fs::create_dir(dir.path().join("out")).unwrap();
        cp_under_umask(dir.path(), umask, &["--parents", "a/b/f", "out"]);
        let out = dir.path().join("out");
        assert_eq!(mode(&out.join("a")), a, "a, umask {umask:o}");
        assert_eq!(mode(&out.join("a/b")), b, "a/b, umask {umask:o}");
        assert_eq!(fs::read_to_string(out.join("a/b/f")).unwrap(), "f\n");
    }
}

/// Only the nine permission bits are reset: the sticky bit `mkdir` gave the copy and a
/// set-group-ID bit inherited from the destination's parent stay, as GNU cp leaves them.
#[cfg(target_os = "linux")]
#[test]
fn cp_r_keeps_sticky_and_inherited_setgid() {
    let dir = tempdir().unwrap();
    let src = dir.path().join("src");
    fs::create_dir_all(src.join("t")).unwrap();
    set_mode(&src.join("t"), 0o1777);
    set_mode(&src, 0o555);
    let out = dir.path().join("out");
    fs::create_dir(&out).unwrap();
    set_mode(&out, 0o2755);
    cp_under_umask(dir.path(), 0o022, &["-R", "src", "out"]);
    assert_eq!(mode(&out.join("src")), 0o2555);
    assert_eq!(mode(&out.join("src/t")), 0o3755);
}
