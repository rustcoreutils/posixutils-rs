//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! How uudecode opens and finishes its output file: the mode it takes from
//! the data, and the files it must not hang on or damage.

use plib::testing::get_binary_path;
use plib::tmp::tempdir;
use std::fs;
use std::os::unix::ffi::OsStrExt;
use std::os::unix::fs::{FileTypeExt, PermissionsExt};
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::time::{Duration, Instant};

/// Run uudecode with `args`, killing it if it has not finished in ten
/// seconds. `None` means it hung.
fn uudecode_bounded(args: &[&Path]) -> Option<Output> {
    let mut child = Command::new(get_binary_path("uudecode"))
        .args(args)
        .env("LC_ALL", "C")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let deadline = Instant::now() + Duration::from_secs(10);
    while child.try_wait().unwrap().is_none() {
        if Instant::now() > deadline {
            let _ = child.kill();
            let _ = child.wait();
            return None;
        }
        std::thread::sleep(Duration::from_millis(20));
    }
    Some(child.wait_with_output().unwrap())
}

/// Write `input` to `dir/input` and decode it, with `-o out` when given.
fn decode(dir: &Path, input: &str, out: Option<&Path>) -> Option<Output> {
    let input_path = dir.join("input");
    fs::write(&input_path, input).unwrap();
    let o = PathBuf::from("-o");
    match out {
        Some(out) => uudecode_bounded(&[&o, out, &input_path]),
        None => uudecode_bounded(&[&input_path]),
    }
}

fn mode_of(path: &Path) -> u32 {
    fs::metadata(path).unwrap().permissions().mode() & 0o7777
}

/// Only the file access permission bits come from the `begin` line; the
/// set-user-ID and set-group-ID bits of `6755` are dropped.
#[test]
fn uudecode_drops_set_id_bits_from_the_header_mode() {
    let tmp = tempdir().unwrap();
    let dir = tmp.path();
    let target = dir.join("out");
    let input = format!("begin-base64 6755 {}\naGkK\n====\n", target.display());

    let output = decode(dir, &input, None).expect("uudecode hung");
    assert_eq!(output.status.code(), Some(0), "{output:?}");
    assert_eq!(fs::read(&target).unwrap(), b"hi\n");
    assert_eq!(mode_of(&target), 0o755, "mode {:o}", mode_of(&target));
}

/// The mode replaces the one an existing file had, whatever the umask.
#[test]
fn uudecode_sets_the_header_mode_on_an_existing_file() {
    let tmp = tempdir().unwrap();
    let dir = tmp.path();
    let target = dir.join("out");
    fs::write(&target, b"old contents, longer than the new").unwrap();
    fs::set_permissions(&target, fs::Permissions::from_mode(0o600)).unwrap();

    let output =
        decode(dir, "begin-base64 664 x\naGkK\n====\n", Some(&target)).expect("uudecode hung");
    assert_eq!(output.status.code(), Some(0), "{output:?}");
    assert_eq!(fs::read(&target).unwrap(), b"hi\n");
    assert_eq!(mode_of(&target), 0o664, "mode {:o}", mode_of(&target));
}

/// A FIFO with no reader at the output pathname is refused at once, not
/// waited on forever, and is left a FIFO.
#[test]
fn uudecode_does_not_hang_on_a_fifo_output() {
    let tmp = tempdir().unwrap();
    let dir = tmp.path();
    let fifo = dir.join("out");
    let c = std::ffi::CString::new(fifo.as_os_str().as_bytes()).unwrap();
    assert_eq!(unsafe { libc::mkfifo(c.as_ptr(), 0o600) }, 0);

    let output = decode(dir, "begin-base64 644 x\naGkK\n====\n", Some(&fifo));
    assert!(fs::symlink_metadata(&fifo).unwrap().file_type().is_fifo());

    let output = output.expect("uudecode hung opening a FIFO with no reader");
    assert_eq!(output.status.code(), Some(1), "{output:?}");
    let enxio = std::io::Error::from_raw_os_error(libc::ENXIO).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains(enxio.split(" (os error").next().unwrap()),
        "stderr: {stderr}"
    );
}

/// A character device is written, not truncated or chmodded.
#[test]
fn uudecode_writes_to_a_character_device() {
    let tmp = tempdir().unwrap();
    let null = Path::new("/dev/null");
    let before = mode_of(null);

    let output =
        decode(tmp.path(), "begin-base64 600 x\naGkK\n====\n", Some(null)).expect("uudecode hung");

    assert_eq!(output.status.code(), Some(0), "{output:?}");
    assert_eq!(mode_of(null), before);
}
