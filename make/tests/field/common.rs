//
// Copyright (c) 2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use plib::testing::get_binary_path;

/// A scratch directory holding one test's makefile, removed when dropped.
pub struct Dir(PathBuf);

/// What one run of make produced.
#[derive(Debug)]
pub struct Run {
    pub stdout: String,
    pub stderr: String,
    pub code: Option<i32>,
}

impl Run {
    /// Assert that make succeeded and wrote exactly `expected`.
    #[track_caller]
    pub fn ok(&self, expected: &str) {
        assert_eq!(self.code, Some(0), "make failed: {self:?}");
        assert_eq!(self.stdout, expected, "stderr: {}", self.stderr);
    }
}

impl Dir {
    /// A fresh, empty directory named after the test.
    pub fn new(name: &str) -> Dir {
        let path = Path::new(env!("CARGO_TARGET_TMPDIR"))
            .join("make-field")
            .join(name);
        let _ = fs::remove_dir_all(&path);
        fs::create_dir_all(&path).expect("create test directory");
        Dir(path)
    }

    /// Write `contents` to `name` inside the directory.
    pub fn write(&self, name: &str, contents: &str) -> &Self {
        let path = self.0.join(name);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("create parent directory");
        }
        fs::write(path, contents).expect("write test file");
        self
    }

    /// Write an executable script `name` inside the directory, and wait
    /// until it can be run.
    ///
    /// `execve` refuses a file any process holds open for writing, and the
    /// other tests in this binary fork all the time: a `fork` while the
    /// script is being written copies its descriptor into the child, where
    /// `O_CLOEXEC` closes it only at that child's `exec`. Until then running
    /// the script fails with `ETXTBSY`, and make reports the failure.
    pub fn write_executable(&self, name: &str, contents: &str) -> &Self {
        use std::os::unix::fs::PermissionsExt;
        self.write(name, contents);
        let path = self.path(name);
        fs::set_permissions(&path, fs::Permissions::from_mode(0o755))
            .expect("make the script executable");
        for attempt in 0..10 {
            match Command::new(&path).output() {
                Err(e) if e.raw_os_error() == Some(libc::ETXTBSY) => {
                    std::thread::sleep(std::time::Duration::from_millis(10 << attempt.min(4)));
                }
                _ => break,
            }
        }
        self
    }

    /// The path of `name` inside the directory.
    pub fn path(&self, name: &str) -> PathBuf {
        self.0.join(name)
    }

    /// Run make in the directory with `args`.
    pub fn make(&self, args: &[&str]) -> Run {
        self.make_env(args, &[])
    }

    /// Run make in the directory with `args` and extra environment.
    ///
    /// `MAKEFLAGS` and `MAKELEVEL` are removed first: the test may itself be
    /// running under a make, whose flags must not leak into the one tested.
    pub fn make_env(&self, args: &[&str], env: &[(&str, &str)]) -> Run {
        let output = Command::new(get_binary_path("make"))
            .args(args)
            .current_dir(&self.0)
            .env_remove("MAKEFLAGS")
            .env_remove("MAKELEVEL")
            .envs(env.iter().copied())
            .output()
            .expect("run make");
        Run {
            stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
            stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
            code: output.status.code(),
        }
    }
}

impl Drop for Dir {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}
