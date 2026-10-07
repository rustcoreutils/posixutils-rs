//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `cp -n` racing a writer that creates the destination while cp runs. cp
//! checks for the destination and then creates it; the create is
//! `O_CREAT | O_EXCL`, so a file that appears in between is never written
//! to, whatever the timing.

use std::fs::{self, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use plib::testing::get_binary_path;

fn scratch(tag: &str) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("cp_race_{tag}"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    dir
}

#[test]
fn cp_n_never_overwrites_a_destination_that_appears_mid_copy() {
    let dir = scratch("no_clobber");
    let source = dir.join("source");
    let target = dir.join("target");
    fs::write(
        &source,
        b"SOURCE CONTENT, MUST NOT LAND IN A FILE cp DID NOT CREATE",
    )
    .unwrap();

    let deadline = Instant::now() + Duration::from_secs(2);
    let mut round: u64 = 0;
    while Instant::now() < deadline {
        round += 1;
        let _ = fs::remove_file(&target);
        let mut child = Command::new(get_binary_path("cp"))
            .args(["-n".as_ref(), source.as_os_str(), target.as_os_str()])
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("failed to execute cp");

        // Spread the writer's create across cp's lifetime.
        std::thread::sleep(Duration::from_micros((round * 397) % 1500));
        // `create_new` is O_CREAT | O_EXCL: success means the writer, not cp,
        // created the destination.
        let ours = OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&target);
        let we_created = ours.is_ok();
        if let Ok(mut file) = ours {
            file.write_all(b"keep").unwrap();
        }
        child.wait().unwrap();

        if we_created {
            assert_eq!(
                fs::read(&target).unwrap(),
                b"keep",
                "cp -n wrote into a destination it did not create (round {round})"
            );
        }
    }

    let _ = fs::remove_dir_all(&dir);
}

/// `cp -p --parents dir/f t` makes `t/dir`, opens it and later applies the
/// source directory's mode and owner through that descriptor. A directory it
/// has just made and that is swapped for a symbolic link before the open must
/// not be followed: the mode (and, as root, the owner) would land on the
/// link's target, and the copy inside it.
#[test]
fn cp_parents_never_follows_a_link_swapped_for_a_made_directory() {
    use std::os::unix::fs::{symlink, PermissionsExt};
    use std::sync::atomic::{AtomicBool, Ordering};

    let base = scratch("parents");
    let victim = base.join("victim");
    fs::create_dir(&victim).unwrap();
    fs::set_permissions(&victim, fs::Permissions::from_mode(0o755)).unwrap();
    fs::create_dir(base.join("dir")).unwrap();
    fs::write(base.join("dir/f"), b"f").unwrap();
    fs::set_permissions(base.join("dir"), fs::Permissions::from_mode(0o700)).unwrap();
    let made = base.join("t/dir");

    let deadline = Instant::now() + Duration::from_secs(2);
    let mut rounds = 0;
    while Instant::now() < deadline {
        rounds += 1;
        let _ = fs::remove_dir_all(base.join("t"));
        fs::create_dir(base.join("t")).unwrap();

        let done = AtomicBool::new(false);
        std::thread::scope(|scope| {
            scope.spawn(|| {
                while !done.load(Ordering::Relaxed) {
                    // Succeeds only on the empty directory cp just made.
                    if fs::remove_dir(&made).is_ok() {
                        let _ = symlink(&victim, &made);
                    }
                }
            });
            let _ = Command::new(get_binary_path("cp"))
                .args(["-p", "--parents", "dir/f", "t"])
                .current_dir(&base)
                .stdin(Stdio::null())
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status()
                .expect("failed to execute cp");
            done.store(true, Ordering::Relaxed);
        });

        let mode = fs::metadata(&victim).unwrap().permissions().mode() & 0o7777;
        let entries = fs::read_dir(&victim).unwrap().count();
        assert!(
            mode == 0o755 && entries == 0,
            "cp --parents acted through a symlink swapped for a directory it made \
             (round {rounds}): mode {mode:o}, {entries} entries"
        );
    }

    let _ = fs::remove_dir_all(&base);
}

/// A dangling symbolic link inside the destination tree of `cp -R` is not
/// written through: it could name any file. GNU refuses it in these words;
/// POSIX's write-through stays for a dangling link that is the operand.
#[test]
fn cp_r_does_not_write_through_a_dangling_link_below_the_operand() {
    use std::os::unix::fs::symlink;

    let base = scratch("dangling_below");
    fs::create_dir_all(base.join("src")).unwrap();
    fs::write(base.join("src/f"), b"f").unwrap();
    fs::create_dir_all(base.join("dst/src")).unwrap();
    symlink(base.join("elsewhere"), base.join("dst/src/f")).unwrap();

    let out = Command::new(get_binary_path("cp"))
        .args(["-R", "src", "dst"])
        .current_dir(&base)
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute cp");
    assert!(
        !base.join("elsewhere").exists(),
        "cp -R wrote through a dangling link in the destination tree"
    );
    assert_eq!(out.status.code(), Some(1));
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        "cp: not writing through dangling symlink 'dst/src/f'\n"
    );

    let _ = fs::remove_dir_all(&base);
}

/// `cp --parents` never follows a symbolic link in the destination path
/// below the target operand, made or found: one that "already exists" may
/// have been planted a moment before cp's `mkdirat`, and the two cannot be
/// told apart. (GNU follows a pre-existing one.)
#[test]
fn cp_parents_refuses_a_symlink_component_in_the_destination() {
    use std::os::unix::fs::symlink;

    let base = scratch("parents_symlink_component");
    fs::create_dir_all(base.join("dir/sub")).unwrap();
    fs::write(base.join("dir/sub/f"), b"f").unwrap();
    fs::create_dir_all(base.join("t")).unwrap();
    fs::create_dir(base.join("elsewhere")).unwrap();
    symlink("../elsewhere", base.join("t/dir")).unwrap();

    let out = Command::new(get_binary_path("cp"))
        .args(["--parents", "dir/sub/f", "t"])
        .current_dir(&base)
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute cp");
    assert_eq!(
        fs::read_dir(base.join("elsewhere")).unwrap().count(),
        0,
        "cp --parents followed a symlink in the destination path"
    );
    assert_eq!(out.status.code(), Some(1));
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("'t/dir' exists but is not a directory"),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );

    let _ = fs::remove_dir_all(&base);
}

/// `cp -R src dst` makes `dst/sub` with `mkdirat` and then opens it to copy
/// `src/sub`'s contents into. A directory swapped for a symbolic link in
/// between must not be followed.
#[test]
fn cp_r_never_follows_a_link_swapped_for_a_made_directory() {
    use std::os::unix::fs::symlink;
    use std::sync::atomic::{AtomicBool, Ordering};

    let base = scratch("recursive");
    let victim = base.join("victim");
    fs::create_dir(&victim).unwrap();
    // Many directories, so each run of cp offers many windows.
    const DIRS: usize = 64;
    for i in 0..DIRS {
        fs::create_dir_all(base.join(format!("src/sub{i}"))).unwrap();
        fs::write(base.join(format!("src/sub{i}/f")), b"f").unwrap();
    }
    let made: Vec<PathBuf> = (0..DIRS)
        .map(|i| base.join(format!("dst/sub{i}")))
        .collect();

    let deadline = Instant::now() + Duration::from_secs(2);
    let mut rounds = 0;
    while Instant::now() < deadline {
        rounds += 1;
        let _ = fs::remove_dir_all(base.join("dst"));

        let done = AtomicBool::new(false);
        std::thread::scope(|scope| {
            scope.spawn(|| {
                while !done.load(Ordering::Relaxed) {
                    for dir in &made {
                        // Succeeds only on an empty directory cp just made.
                        if fs::remove_dir(dir).is_ok() {
                            let _ = symlink(&victim, dir);
                        }
                    }
                }
            });
            let _ = Command::new(get_binary_path("cp"))
                .args(["-R", "src", "dst"])
                .current_dir(&base)
                .stdin(Stdio::null())
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status()
                .expect("failed to execute cp");
            done.store(true, Ordering::Relaxed);
        });

        let entries = fs::read_dir(&victim).unwrap().count();
        assert_eq!(
            entries, 0,
            "cp -R copied through a symlink swapped for a directory it made (round {rounds})"
        );
    }

    let _ = fs::remove_dir_all(&base);
}
