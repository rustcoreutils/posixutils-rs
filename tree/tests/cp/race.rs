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

/// An existing symbolic link inside the destination tree of `cp -R` is not written through
/// either: like a dangling one, it could name any file. (GNU follows it.)
#[test]
fn cp_r_does_not_write_through_a_symlink_below_the_operand() {
    use std::os::unix::fs::symlink;

    let base = scratch("symlink_below");
    fs::create_dir_all(base.join("src")).unwrap();
    fs::write(base.join("src/f"), b"source").unwrap();
    fs::create_dir_all(base.join("dst/src")).unwrap();
    fs::write(base.join("elsewhere"), b"untouched").unwrap();
    symlink(base.join("elsewhere"), base.join("dst/src/f")).unwrap();

    let out = Command::new(get_binary_path("cp"))
        .args(["-R", "src", "dst"])
        .current_dir(&base)
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute cp");
    assert_eq!(
        fs::read(base.join("elsewhere")).unwrap(),
        b"untouched",
        "cp -R wrote through a symlink in the destination tree"
    );
    assert_eq!(out.status.code(), Some(1));
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        "cp: not writing through symlink 'dst/src/f'\n"
    );

    let _ = fs::remove_dir_all(&base);
}

/// Run `cp -i source target` (in `base`), and when it asks to overwrite, let `swap` replace the
/// destination before answering yes. Returns cp's stderr.
fn cp_i_swapping_at_prompt(base: &std::path::Path, swap: impl FnOnce()) -> String {
    use std::io::Read;

    let mut child = Command::new(get_binary_path("cp"))
        .args(["-i", "source", "target"])
        .current_dir(base)
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .expect("failed to execute cp");
    let mut stderr = child.stderr.take().unwrap();
    let mut seen = Vec::new();
    let mut byte = [0u8; 1];
    while !seen.ends_with(b"? ") {
        assert_eq!(stderr.read(&mut byte).unwrap(), 1, "cp never prompted");
        seen.push(byte[0]);
    }
    // cp has checked the destination and is waiting: the window is held open.
    swap();
    child.stdin.take().unwrap().write_all(b"y\n").unwrap();
    stderr.read_to_end(&mut seen).unwrap();
    child.wait().unwrap();
    String::from_utf8_lossy(&seen).into_owned()
}

/// cp decided to overwrite a regular file; a symbolic link swapped in for it before the open
/// must not be followed.
#[test]
fn cp_never_writes_through_a_symlink_swapped_in_after_the_check() {
    use std::os::unix::fs::symlink;

    let base = scratch("swap_symlink");
    fs::write(base.join("source"), b"source").unwrap();
    fs::write(base.join("target"), b"old").unwrap();
    fs::write(base.join("victim"), b"untouched").unwrap();

    let stderr = cp_i_swapping_at_prompt(&base, || {
        fs::remove_file(base.join("target")).unwrap();
        symlink("victim", base.join("target")).unwrap();
    });
    assert_eq!(
        fs::read(base.join("victim")).unwrap(),
        b"untouched",
        "cp wrote through a symlink swapped in after its check; stderr: {stderr}"
    );

    let _ = fs::remove_dir_all(&base);
}

/// The source is opened after cp's checks (here held open by the -i prompt). A FIFO swapped in
/// for the regular file the walk saw must be refused, not opened: an `O_RDONLY` open of a FIFO
/// waits for a writer forever. Any other swapped-in file is refused too.
#[test]
fn cp_refuses_a_source_swapped_after_the_walk_saw_it() {
    use std::os::unix::ffi::OsStrExt;
    use std::os::unix::fs::OpenOptionsExt;

    let base = scratch("swap_source");
    fs::write(base.join("source"), b"source").unwrap();
    fs::write(base.join("target"), b"old").unwrap();
    let fifo = base.join("source");

    let (tx, rx) = std::sync::mpsc::channel();
    let base_for_cp = base.clone();
    let fifo_for_cp = fifo.clone();
    std::thread::spawn(move || {
        let stderr = cp_i_swapping_at_prompt(&base_for_cp, || {
            fs::remove_file(&fifo_for_cp).unwrap();
            let c = std::ffi::CString::new(fifo_for_cp.as_os_str().as_bytes()).unwrap();
            assert_eq!(unsafe { libc::mkfifo(c.as_ptr(), 0o600) }, 0);
        });
        let _ = tx.send(stderr);
    });
    match rx.recv_timeout(Duration::from_secs(20)) {
        Ok(stderr) => {
            assert_eq!(fs::read(base.join("target")).unwrap(), b"old");
            assert!(stderr.contains("changed"), "stderr: {stderr}");
        }
        Err(_) => {
            // Release cp, blocked in the FIFO's open, before failing.
            let _ = fs::OpenOptions::new()
                .write(true)
                .custom_flags(libc::O_NONBLOCK)
                .open(&fifo);
            panic!("cp opened a FIFO swapped in for its regular-file source and hung");
        }
    }

    let _ = fs::remove_dir_all(&base);
}

/// An existing destination that is a device is written to, not truncated: `cp f /dev/null`.
#[test]
fn cp_writes_to_an_existing_character_device() {
    let base = scratch("to_dev_null");
    fs::write(base.join("source"), b"source").unwrap();
    let out = Command::new(get_binary_path("cp"))
        .args(["source", "/dev/null"])
        .current_dir(&base)
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute cp");
    assert_eq!(
        out.status.code(),
        Some(0),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let _ = fs::remove_dir_all(&base);
}

/// cp decided to overwrite a regular file; a FIFO swapped in for it must be refused, not opened:
/// an `O_WRONLY` open of a FIFO waits for a reader forever.
#[test]
fn cp_refuses_a_fifo_swapped_in_for_the_destination() {
    use std::os::unix::ffi::OsStrExt;
    use std::os::unix::fs::{FileTypeExt, OpenOptionsExt};

    let base = scratch("swap_dest_fifo");
    fs::write(base.join("source"), b"source").unwrap();
    fs::write(base.join("target"), b"old").unwrap();
    let fifo = base.join("target");

    let (tx, rx) = std::sync::mpsc::channel();
    let base_for_cp = base.clone();
    let fifo_for_cp = fifo.clone();
    std::thread::spawn(move || {
        let stderr = cp_i_swapping_at_prompt(&base_for_cp, || {
            fs::remove_file(&fifo_for_cp).unwrap();
            let c = std::ffi::CString::new(fifo_for_cp.as_os_str().as_bytes()).unwrap();
            assert_eq!(unsafe { libc::mkfifo(c.as_ptr(), 0o600) }, 0);
        });
        let _ = tx.send(stderr);
    });
    match rx.recv_timeout(Duration::from_secs(20)) {
        Ok(stderr) => {
            assert!(
                fs::symlink_metadata(&fifo).unwrap().file_type().is_fifo(),
                "stderr: {stderr}"
            );
            // Refused at the open (no reader), or by the identity check.
            assert!(
                stderr.contains("No such device or address") || stderr.contains("changed"),
                "stderr: {stderr}"
            );
        }
        Err(_) => {
            // Release cp, blocked in the FIFO's open, before failing.
            let _ = fs::OpenOptions::new()
                .read(true)
                .custom_flags(libc::O_NONBLOCK)
                .open(&fifo);
            panic!("cp opened a FIFO swapped in for its destination and hung");
        }
    }

    let _ = fs::remove_dir_all(&base);
}

/// The same for another regular file renamed over the destination: the file opened must be the
/// one cp checked, or nothing is written -- not even a truncation.
#[test]
fn cp_never_writes_into_a_file_swapped_in_after_the_check() {
    let base = scratch("swap_file");
    fs::write(base.join("source"), b"source").unwrap();
    fs::write(base.join("target"), b"old").unwrap();
    fs::write(base.join("victim"), b"untouched").unwrap();

    let stderr = cp_i_swapping_at_prompt(&base, || {
        fs::hard_link(base.join("victim"), base.join("victim_link")).unwrap();
        fs::rename(base.join("victim_link"), base.join("target")).unwrap();
    });
    assert_eq!(
        fs::read(base.join("victim")).unwrap(),
        b"untouched",
        "cp wrote into a file swapped in after its check; stderr: {stderr}"
    );
    assert!(stderr.contains("changed"), "stderr: {stderr}");

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

/// POSIX writes through a dangling destination link named as the operand. A file that appears at
/// the link's target between cp's check and its open is written as the link's target, from the
/// start: replaced, never left with the tail of what it held.
#[test]
fn cp_through_a_dangling_operand_link_truncates_a_target_that_appears() {
    use std::os::unix::fs::symlink;

    let dir = scratch("dangling_appears");
    let source = dir.join("source");
    let link = dir.join("link");
    let referent = dir.join("referent");
    let staged = dir.join("staged");
    fs::write(&source, b"source").unwrap();
    let long = vec![b'X'; 4096];

    let deadline = Instant::now() + Duration::from_secs(2);
    let mut round: u64 = 0;
    while Instant::now() < deadline {
        round += 1;
        let _ = fs::remove_file(&referent);
        let _ = fs::remove_file(&link);
        symlink("referent", &link).unwrap();
        fs::write(&staged, &long).unwrap();
        let mut child = Command::new(get_binary_path("cp"))
            .args([source.as_os_str(), link.as_os_str()])
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("failed to execute cp");
        std::thread::sleep(Duration::from_micros((round * 397) % 1500));
        // Appears complete, or not at all if cp created it first.
        let _ = fs::hard_link(&staged, &referent);
        child.wait().unwrap();
        let _ = fs::remove_file(&staged);

        assert_eq!(
            fs::read(&referent).unwrap(),
            b"source",
            "cp wrote through a dangling link into a file that appeared, without truncating \
             it (round {round})"
        );
    }

    let _ = fs::remove_dir_all(&dir);
}

/// A large set-user-ID source with an old modification time, for the `-p` races below: the copy
/// takes long enough that the destination exists for a while before cp finishes with it.
fn big_setuid_source(dir: &std::path::Path) -> PathBuf {
    use std::os::unix::fs::PermissionsExt;
    let source = dir.join("source");
    fs::write(&source, vec![0x5a_u8; 32 << 20]).unwrap();
    fs::set_permissions(&source, fs::Permissions::from_mode(0o4755)).unwrap();
    let old = std::time::UNIX_EPOCH + Duration::from_secs(946_684_800);
    fs::File::options()
        .write(true)
        .open(&source)
        .unwrap()
        .set_times(fs::FileTimes::new().set_accessed(old).set_modified(old))
        .unwrap();
    source
}

/// `cp -p` applies the source's times, owner and mode to the file it created. Doing that by
/// name after closing the file acts on whatever the name holds by then: here a file renamed over
/// the destination mid-copy, which cp must leave exactly as it was (as root, an attacker's
/// executable would have been made set-user-ID to the source's owner).
#[test]
fn cp_p_never_applies_attributes_to_a_file_renamed_over_the_destination() {
    use std::os::unix::fs::{MetadataExt, PermissionsExt};

    let dir = scratch("p_rename_over");
    let source = big_setuid_source(&dir);
    let target = dir.join("target");
    let victim = dir.join("victim");

    let deadline = Instant::now() + Duration::from_secs(3);
    let mut swaps = 0;
    while Instant::now() < deadline {
        let _ = fs::remove_file(&target);
        fs::write(&victim, b"attacker").unwrap();
        fs::set_permissions(&victim, fs::Permissions::from_mode(0o600)).unwrap();
        let victim_ino = fs::metadata(&victim).unwrap().ino();

        let mut child = Command::new(get_binary_path("cp"))
            .args(["-p".as_ref(), source.as_os_str(), target.as_os_str()])
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("failed to execute cp");
        // Swap as soon as cp has created the destination.
        let mut swapped = false;
        while child.try_wait().unwrap().is_none() {
            if fs::symlink_metadata(&target).is_ok() {
                swapped = fs::rename(&victim, &target).is_ok();
                break;
            }
        }
        child.wait().unwrap();
        if !swapped {
            continue;
        }
        swaps += 1;

        let md = fs::symlink_metadata(&target).unwrap();
        if md.ino() == victim_ino {
            assert!(
                md.mode() & 0o7777 == 0o600 && md.mtime() != 946_684_800,
                "cp -p applied the source's attributes to a file renamed over its \
                 destination: mode {:o}, mtime {}",
                md.mode() & 0o7777,
                md.mtime()
            );
        }
    }
    assert!(swaps > 0, "no round swapped the destination mid-copy");

    let _ = fs::remove_dir_all(&dir);
}

/// `cp -p` of a set-user-ID file: until the copy is complete (and owned as the source is), the
/// destination must carry no set-user-ID bit and no group or other permissions. Creating it with
/// the source's full mode left a set-user-ID file owned by the invoker, with another user's
/// partial content, readable and executable by anyone.
#[test]
fn cp_p_creates_the_destination_without_setuid_or_group_other_bits() {
    use std::os::unix::fs::MetadataExt;

    let dir = scratch("p_create_mode");
    let source = big_setuid_source(&dir);
    let full = fs::metadata(&source).unwrap().len();
    let target = dir.join("target");

    for _ in 0..5 {
        let _ = fs::remove_file(&target);
        let mut child = Command::new(get_binary_path("cp"))
            .args(["-p".as_ref(), source.as_os_str(), target.as_os_str()])
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("failed to execute cp");
        while child.try_wait().unwrap().is_none() {
            if let Ok(md) = fs::symlink_metadata(&target) {
                // The mode is read first: a size read first could be stale by the time the
                // final mode is applied.
                let mode = md.mode() & 0o7777;
                if md.len() < full {
                    assert_eq!(
                        mode & 0o7077,
                        0,
                        "a partial cp -p destination had mode {mode:o}"
                    );
                }
            }
        }
        assert_eq!(
            fs::metadata(&target).unwrap().mode() & 0o7777,
            0o4755,
            "the finished copy keeps the source's mode"
        );
    }

    let _ = fs::remove_dir_all(&dir);
}

/// Run `cp args` in `base` repeatedly for two seconds while a thread replaces each of `made`
/// -- an empty directory cp has just made -- with a non-empty directory of its own. After each
/// run, no planted directory may have received anything from cp.
fn cp_racing_a_made_dir_swap(base: &std::path::Path, args: &[&str], made: &[PathBuf]) {
    use std::os::unix::fs::PermissionsExt;
    use std::sync::atomic::{AtomicBool, Ordering};

    let decoys: Vec<PathBuf> = (0..made.len())
        .map(|i| base.join(format!("decoy{i}")))
        .collect();
    let dest = made[0].parent().unwrap().to_path_buf();
    let deadline = Instant::now() + Duration::from_secs(2);
    let mut rounds = 0;
    while Instant::now() < deadline {
        rounds += 1;
        let _ = fs::remove_dir_all(&dest);
        fs::create_dir(&dest).unwrap();
        // Others may write here, which is what makes the swap possible.
        fs::set_permissions(&dest, fs::Permissions::from_mode(0o777)).unwrap();
        for decoy in &decoys {
            let _ = fs::remove_dir_all(decoy);
            fs::create_dir(decoy).unwrap();
            fs::write(decoy.join("planted"), b"").unwrap();
        }

        let done = AtomicBool::new(false);
        std::thread::scope(|scope| {
            scope.spawn(|| {
                while !done.load(Ordering::Relaxed) {
                    for (dir, decoy) in made.iter().zip(&decoys) {
                        // Succeeds only on the empty directory cp just made.
                        if fs::remove_dir(dir).is_ok() {
                            let _ = fs::rename(decoy, dir);
                        }
                    }
                }
            });
            let _ = Command::new(get_binary_path("cp"))
                .args(args)
                .current_dir(base)
                .stdin(Stdio::null())
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status()
                .expect("failed to execute cp");
            done.store(true, Ordering::Relaxed);
        });

        for dir in made {
            if dir.join("planted").exists() {
                let entries = fs::read_dir(dir).unwrap().count();
                assert_eq!(
                    entries, 1,
                    "cp copied into a directory swapped for one it made (round {rounds})"
                );
            }
        }
    }
}

/// `cp -R` makes each destination directory with `mkdirat` and then opens it. One swapped in
/// between by someone who can write the parent must not receive the copy.
#[test]
fn cp_r_never_copies_into_a_directory_swapped_for_a_made_one() {
    let base = scratch("made_dir_swap");
    const DIRS: usize = 32;
    for i in 0..DIRS {
        fs::create_dir_all(base.join(format!("src/sub{i}"))).unwrap();
        fs::write(base.join(format!("src/sub{i}/f")), b"f").unwrap();
    }
    let made: Vec<PathBuf> = (0..DIRS)
        .map(|i| base.join(format!("dst/sub{i}")))
        .collect();
    cp_racing_a_made_dir_swap(&base, &["-R", "src/.", "dst"], &made);
    let _ = fs::remove_dir_all(&base);
}

/// The same for the directories `cp --parents` makes.
#[test]
fn cp_parents_never_copies_into_a_directory_swapped_for_a_made_one() {
    let base = scratch("parents_made_dir_swap");
    fs::create_dir_all(base.join("dir")).unwrap();
    fs::write(base.join("dir/f"), b"f").unwrap();
    cp_racing_a_made_dir_swap(&base, &["--parents", "dir/f", "t"], &[base.join("t/dir")]);
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
