//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Output files are created, never opened through whatever already holds
//! their name: a symbolic link planted at the output name must neither be
//! written through nor have its target's attributes changed, and an input
//! that is not a regular file is refused instead of read.

use plib::testing::{get_binary_path, run_test_base};
use plib::tmp::TempDir;
use std::fs;
use std::os::unix::fs::{symlink, MetadataExt};
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::time::{Duration, Instant};

/// Text that compresses well, so compress never declines with status 2.
fn plaintext() -> Vec<u8> {
    b"attacker controlled plaintext\n".repeat(100)
}

/// A temp directory holding `att/` (where compress runs on files) and
/// `victim/` (what a planted link points into).
fn layout() -> (TempDir, PathBuf, PathBuf) {
    let td = plib::tmp::tempdir().unwrap();
    let att = td.path().join("att");
    let victim = td.path().join("victim");
    fs::create_dir(&att).unwrap();
    fs::create_dir(&victim).unwrap();
    (td, att, victim)
}

fn compress(args: &[&Path]) -> Output {
    let args: Vec<String> = args
        .iter()
        .map(|p| p.to_str().unwrap().to_string())
        .collect();
    run_test_base("compress", &args, b"")
}

/// Write `name.Z` holding the compressed form of `data`, via `compress -c`.
fn make_z(dir: &Path, name: &str, data: &[u8]) -> PathBuf {
    let plain = dir.join(format!("{name}.src"));
    fs::write(&plain, data).unwrap();
    let out = compress(&[Path::new("-c"), &plain]);
    assert_eq!(out.status.code(), Some(0), "test setup: compress -c");
    fs::remove_file(&plain).unwrap();
    let z = dir.join(format!("{name}.Z"));
    fs::write(&z, &out.stdout).unwrap();
    z
}

fn stderr(out: &Output) -> String {
    String::from_utf8_lossy(&out.stderr).into_owned()
}

/// Whether `path` holds exactly `want`; a bool, so a failure does not dump
/// kilobytes of bytes.
fn holds(path: &Path, want: &[u8]) -> bool {
    fs::read(path).is_ok_and(|got| got == want)
}

/// Whether `compress -c -d path` recovers `want`.
fn decompresses_to(path: &Path, want: &[u8]) -> bool {
    compress(&[Path::new("-c"), Path::new("-d"), path]).stdout == want
}

fn is_symlink(path: &Path) -> bool {
    fs::symlink_metadata(path)
        .map(|m| m.file_type().is_symlink())
        .unwrap_or(false)
}

/// A dangling `x.Z` link is an existing output: without `-f` and without a
/// terminal, compress refuses, and nothing is created at the link's target.
#[test]
fn test_compress_dangling_symlink_output_is_not_followed() {
    let (_td, att, victim) = layout();
    let input = att.join("x");
    fs::write(&input, plaintext()).unwrap();
    let target = victim.join("created");
    symlink(&target, att.join("x.Z")).unwrap();

    let out = compress(&[&input]);

    assert!(!target.exists(), "compress wrote through a dangling link");
    assert_eq!(out.status.code(), Some(1), "stderr={}", stderr(&out));
    assert!(stderr(&out).contains("already exists"), "{}", stderr(&out));
    assert!(is_symlink(&att.join("x.Z")), "the link must be left alone");
    assert!(holds(&input, &plaintext()), "input must remain");
}

/// The same for decompression: a dangling `x` link must not carry the
/// plaintext to wherever it points.
#[test]
fn test_uncompress_dangling_symlink_output_is_not_followed() {
    let (_td, att, victim) = layout();
    let z = make_z(&att, "x", &plaintext());
    let target = victim.join("pwned");
    symlink(&target, att.join("x")).unwrap();

    let out = compress(&[Path::new("-d"), &z]);

    assert!(!target.exists(), "uncompress wrote through a dangling link");
    assert_eq!(out.status.code(), Some(1), "stderr={}", stderr(&out));
    assert!(stderr(&out).contains("already exists"), "{}", stderr(&out));
    assert!(is_symlink(&att.join("x")), "the link must be left alone");
    assert!(z.exists(), "input must remain");
}

/// Under `-f` a live link at the output name is replaced by the output
/// itself; the file it pointed to keeps its contents and its mode.
#[test]
fn test_compress_force_replaces_symlink_output() {
    use std::os::unix::fs::PermissionsExt;

    let (_td, att, victim) = layout();
    let input = att.join("x");
    fs::write(&input, plaintext()).unwrap();
    fs::set_permissions(&input, fs::Permissions::from_mode(0o604)).unwrap();
    let target = victim.join("target");
    fs::write(&target, b"VICTIM").unwrap();
    fs::set_permissions(&target, fs::Permissions::from_mode(0o600)).unwrap();
    let output = att.join("x.Z");
    symlink(&target, &output).unwrap();

    let out = compress(&[Path::new("-f"), &input]);

    assert!(holds(&target, b"VICTIM"), "target was written");
    let target_mode = fs::metadata(&target).unwrap().mode() & 0o7777;
    assert_eq!(target_mode, 0o600, "target mode was changed");
    assert_eq!(out.status.code(), Some(0), "stderr={}", stderr(&out));
    assert!(!is_symlink(&output), "the link must be replaced");
    assert!(decompresses_to(&output, &plaintext()));
    assert!(!input.exists(), "input must be removed");
}

/// Under `-f` decompression replaces a live link the same way.
#[test]
fn test_uncompress_force_replaces_symlink_output() {
    let (_td, att, victim) = layout();
    let z = make_z(&att, "x", &plaintext());
    let target = victim.join("target");
    fs::write(&target, b"VICTIM").unwrap();
    let output = att.join("x");
    symlink(&target, &output).unwrap();

    let out = compress(&[Path::new("-d"), Path::new("-f"), &z]);

    assert!(holds(&target, b"VICTIM"), "target was written");
    assert_eq!(out.status.code(), Some(0), "stderr={}", stderr(&out));
    assert!(!is_symlink(&output), "the link must be replaced");
    assert!(
        holds(&output, &plaintext()),
        "output must hold the plaintext"
    );
    assert!(!z.exists(), "input must be removed");
}

/// `-f` never removes a directory that holds the output name.
#[test]
fn test_compress_force_refuses_directory_output() {
    let (_td, att, _victim) = layout();
    let input = att.join("x");
    fs::write(&input, plaintext()).unwrap();
    let output = att.join("x.Z");
    fs::create_dir(&output).unwrap();
    fs::write(output.join("keep"), b"keep").unwrap();

    let out = compress(&[Path::new("-f"), &input]);

    assert_eq!(out.status.code(), Some(1), "stderr={}", stderr(&out));
    assert!(
        holds(&output.join("keep"), b"keep"),
        "directory was changed"
    );
    assert!(holds(&input, &plaintext()), "input must remain");
}

/// Run compress with `args`, killing it if it has not finished in ten
/// seconds. Returns the exit status, or None on a timeout.
fn run_bounded(args: &[&Path]) -> Option<std::process::ExitStatus> {
    let mut child = Command::new(get_binary_path("compress"))
        .args(args)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .unwrap();
    let deadline = Instant::now() + Duration::from_secs(10);
    while Instant::now() < deadline {
        if let Some(status) = child.try_wait().unwrap() {
            return Some(status);
        }
        std::thread::sleep(Duration::from_millis(20));
    }
    let _ = child.kill();
    let _ = child.wait();
    None
}

fn mkfifo(path: &Path) {
    use std::os::unix::ffi::OsStrExt;
    let c = std::ffi::CString::new(path.as_os_str().as_bytes()).unwrap();
    // SAFETY: `c` is a valid NUL-terminated path for the duration of the call.
    let rc = unsafe { libc::mkfifo(c.as_ptr(), 0o600) };
    assert_eq!(rc, 0, "test setup: {}", std::io::Error::last_os_error());
}

/// A FIFO operand is not a regular file: compress refuses it at once
/// instead of blocking on an open that no writer will ever complete.
#[test]
fn test_compress_fifo_input_does_not_hang() {
    let (_td, att, _victim) = layout();
    let fifo = att.join("fifo");
    mkfifo(&fifo);

    let status = run_bounded(&[&fifo]).expect("compress hung on a FIFO");

    assert_eq!(status.code(), Some(1));
    assert!(fs::symlink_metadata(&fifo).is_ok(), "the FIFO must remain");
    assert!(!att.join("fifo.Z").exists(), "no output for a FIFO");
}

/// The same for decompression, reached through a symbolic link so the
/// refusal cannot rest on the operand's own file type.
#[test]
fn test_uncompress_fifo_input_does_not_hang() {
    let (_td, att, _victim) = layout();
    let fifo = att.join("fifo");
    mkfifo(&fifo);
    let link = att.join("x.Z");
    symlink(&fifo, &link).unwrap();

    let status = run_bounded(&[Path::new("-d"), &link]).expect("compress -d hung on a FIFO");

    assert_eq!(status.code(), Some(1));
    assert!(is_symlink(&link), "the operand must remain");
    assert!(!att.join("x").exists(), "no output for a FIFO");
}

/// A symbolic link operand names the file the user means: its target is
/// compressed into `link.Z`, and the link (not the target) is removed.
#[test]
fn test_compress_symlink_operand_removes_the_link() {
    let (_td, att, victim) = layout();
    let target = victim.join("data");
    fs::write(&target, plaintext()).unwrap();
    let link = att.join("x");
    symlink(&target, &link).unwrap();

    let out = compress(&[&link]);

    assert_eq!(out.status.code(), Some(0), "stderr={}", stderr(&out));
    assert!(!is_symlink(&link), "the link operand must be removed");
    assert!(holds(&target, &plaintext()), "target must be kept");
    assert!(decompresses_to(&att.join("x.Z"), &plaintext()));
}

/// Owner, mode (set-group-ID included, when this user may set it) and both
/// times travel from the input to the output.
#[test]
fn test_compress_preserves_owner_mode_and_times() {
    use std::os::unix::fs::PermissionsExt;

    let (_td, att, _victim) = layout();
    let input = att.join("x");
    fs::write(&input, plaintext()).unwrap();
    // The kernel may drop S_ISGID if the file's group is not ours; whatever
    // it kept is what the output must carry.
    fs::set_permissions(&input, fs::Permissions::from_mode(0o2640)).unwrap();
    let past = std::time::UNIX_EPOCH + Duration::from_nanos(978_307_200_123_456_789);
    let atime = std::time::UNIX_EPOCH + Duration::from_secs(1_000_000_000);
    let times = fs::FileTimes::new().set_accessed(atime).set_modified(past);
    fs::File::options()
        .write(true)
        .open(&input)
        .unwrap()
        .set_times(times)
        .unwrap();
    let before = fs::metadata(&input).unwrap();

    let out = compress(&[&input]);

    assert_eq!(out.status.code(), Some(0), "stderr={}", stderr(&out));
    let after = fs::metadata(att.join("x.Z")).unwrap();
    assert_eq!(after.mode() & 0o7777, before.mode() & 0o7777, "mode");
    assert_eq!((after.uid(), after.gid()), (before.uid(), before.gid()));
    assert_eq!(after.modified().unwrap(), before.modified().unwrap());
    assert_eq!(after.accessed().unwrap(), before.accessed().unwrap());
}
