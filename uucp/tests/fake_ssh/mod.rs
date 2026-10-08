//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! A stand-in `ssh` that runs the "remote" command on this host, so the
//! remote paths of uucp and uux can be tested without a network.

use plib::testing::get_binary_path;
use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};

/// The fake `ssh`. It is called as `ssh -T -o BatchMode=yes HOST COMMAND`;
/// it first runs `$FAKE_SSH_HOOK` (with `$PPID` the utility that called it),
/// then runs COMMAND with `sh`.
const SCRIPT: &str = "#!/bin/sh\neval \"$FAKE_SSH_HOOK\"\nexec /bin/sh -c \"$5\"\n";

/// A scratch directory holding a `bin/ssh` and an empty `tmp/` to serve as
/// `$TMPDIR`, removed when dropped.
pub struct FakeSsh {
    pub dir: PathBuf,
}

impl FakeSsh {
    pub fn new(name: &str) -> FakeSsh {
        let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("fake_ssh_{name}"));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(dir.join("bin")).unwrap();
        fs::create_dir(dir.join("tmp")).unwrap();
        let ssh = dir.join("bin/ssh");
        fs::write(&ssh, SCRIPT).unwrap();
        fs::set_permissions(&ssh, fs::Permissions::from_mode(0o755)).unwrap();
        FakeSsh { dir }
    }

    /// The directory given to the utility as `$TMPDIR`.
    pub fn tmp(&self) -> PathBuf {
        self.dir.join("tmp")
    }

    /// The names left in `$TMPDIR`, sorted.
    pub fn tmp_entries(&self) -> Vec<String> {
        let mut names: Vec<String> = fs::read_dir(self.tmp())
            .unwrap()
            .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
            .collect();
        names.sort();
        names
    }

    /// Run `cmd` with `args`, the fake `ssh` first on `PATH`, `$TMPDIR` set,
    /// and `hook` run by `ssh` before each command.
    pub fn run(&self, cmd: &str, args: &[&str], hook: &str) -> Output {
        let path = format!(
            "{}:{}",
            self.dir.join("bin").display(),
            std::env::var("PATH").unwrap_or_default()
        );
        Command::new(get_binary_path(cmd))
            .args(args)
            .env("PATH", path)
            .env("TMPDIR", self.tmp())
            .env("FAKE_SSH_HOOK", hook)
            .env("LC_ALL", "C")
            .stdin(Stdio::null())
            .output()
            .unwrap()
    }

    /// A path in the scratch directory.
    pub fn join(&self, name: &str) -> PathBuf {
        self.dir.join(name)
    }
}

impl Drop for FakeSsh {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.dir);
    }
}

/// The text of a file, or an empty string if it cannot be read.
pub fn read(path: &Path) -> String {
    fs::read_to_string(path).unwrap_or_default()
}
