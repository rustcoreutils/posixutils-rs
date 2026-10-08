//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A wfile is created once, before processing begins, and every write for it
//! goes to the file created then, whatever has its name afterwards.

use plib::testing::get_binary_path;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

/// A fresh, empty scratch directory for one test.
fn scratch(name: &str) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("sed_wfile_{name}"));
    let _ = fs::remove_dir_all(&dir);
    fs::create_dir_all(&dir).unwrap();
    dir
}

/// Wait up to ten seconds for `path` to hold `want`.
fn wait_for(path: &Path, want: &str) {
    let deadline = Instant::now() + Duration::from_secs(10);
    while fs::read_to_string(path).unwrap_or_default() != want {
        assert!(
            Instant::now() < deadline,
            "{} never held {want:?}",
            path.display()
        );
        std::thread::sleep(Duration::from_millis(10));
    }
}

/// Run `sed -n script` with input fed in two parts. Once the first line has
/// reached `dir/out` as `first` (sed reads one line ahead, so it holds the
/// second), the file is renamed to `dir/kept` and `dir/out` becomes a symlink
/// to `dir/victim`; then the rest of the input is sent. Returns the contents
/// of `kept` and of `victim`.
fn swap_wfile_mid_run(name: &str, script: &str, first: &str) -> (String, String) {
    let dir = scratch(name);
    let (out, kept, victim) = (dir.join("out"), dir.join("kept"), dir.join("victim"));
    fs::write(&victim, "victim\n").unwrap();

    let mut child = Command::new(get_binary_path("sed"))
        .args(["-n", script])
        .current_dir(&dir)
        .env("LC_ALL", "C")
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let mut stdin = child.stdin.take().unwrap();
    stdin.write_all(b"a\nb\n").unwrap();
    stdin.flush().unwrap();
    wait_for(&out, first);

    fs::rename(&out, &kept).unwrap();
    std::os::unix::fs::symlink(&victim, &out).unwrap();
    stdin.write_all(b"c\n").unwrap();
    drop(stdin);
    let status = child.wait_with_output().unwrap();
    assert!(status.status.success(), "{status:?}");

    let result = (
        fs::read_to_string(&kept).unwrap(),
        fs::read_to_string(&victim).unwrap(),
    );
    fs::remove_dir_all(&dir).unwrap();
    result
}

#[test]
fn test_sed_w_keeps_writing_the_file_it_created() {
    let (kept, victim) = swap_wfile_mid_run("w", "w out", "a\n");
    assert_eq!(kept, "a\nb\nc\n");
    assert_eq!(
        victim, "victim\n",
        "sed wrote through a symlink made mid-run"
    );
}

#[test]
fn test_sed_s_w_flag_keeps_writing_the_file_it_created() {
    let (kept, victim) = swap_wfile_mid_run("s_w", "s/^/x/w out", "xa\n");
    assert_eq!(kept, "xa\nxb\nxc\n");
    assert_eq!(
        victim, "victim\n",
        "sed wrote through a symlink made mid-run"
    );
}
