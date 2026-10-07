//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `find -delete` racing an attacker who swaps a directory of the operand
//! for a symbolic link to a directory outside it. find walks through ftw
//! (descent by `openat(parent, name, O_DIRECTORY|O_NOFOLLOW)` plus a
//! `(dev, ino)` check, removal by `unlinkat(parent, name)`), so nothing it
//! enumerates or removes can lie outside the operand whatever the timing.
//! A walk by pathname enumerated the link's target as `OPERAND/sub/...` and
//! deleted its files.

use std::fs;
use std::os::unix::fs::symlink;
use std::path::Path;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use plib::testing::get_binary_path;

use super::scratch_dir;

const FILES: usize = 64;

fn populate(dir: &Path, prefix: &str) {
    fs::create_dir_all(dir.join("inner")).unwrap();
    for i in 0..FILES {
        fs::write(dir.join(format!("{prefix}{i}")), b"x").unwrap();
    }
    fs::write(dir.join("inner/deep"), b"x").unwrap();
}

fn victim_intact(victim: &Path) -> bool {
    (0..FILES).all(|i| victim.join(format!("v{i}")).is_file())
        && victim.join("inner/deep").is_file()
}

/// Swap `tree/sub` for a symlink to `victim` and back, for as long as
/// `running` says the walk is going on.
fn swap_while(tree: &Path, victim: &Path, mut running: impl FnMut() -> bool) {
    let sub = tree.join("sub");
    let hold = tree.join("hold");
    while running() {
        // Each step may lose to find removing what it names; that is fine.
        let _ = fs::rename(&sub, &hold);
        let _ = symlink(victim, &sub);
        std::thread::yield_now();
        // `remove_file` never removes a directory, only the link.
        let _ = fs::remove_file(&sub);
        let _ = fs::rename(&hold, &sub);
    }
}

#[test]
fn find_delete_never_leaves_the_operand_under_a_symlink_swap() {
    let base = scratch_dir("race_delete");
    let victim = base.join("victim");
    populate(&victim, "v");
    let tree = base.join("tree");

    let deadline = Instant::now() + Duration::from_secs(2);
    let mut rounds = 0;
    while Instant::now() < deadline {
        let _ = fs::remove_dir_all(&tree);
        populate(&tree.join("sub"), "t");

        let mut child = Command::new(get_binary_path("find"))
            .args([tree.as_os_str(), "-delete".as_ref()])
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("failed to execute find");
        swap_while(&tree, &victim, || child.try_wait().unwrap().is_none());
        child.wait().unwrap();
        rounds += 1;

        assert!(
            victim_intact(&victim),
            "find -delete removed files outside its operand (round {rounds})"
        );
    }
    assert!(rounds > 0);

    let _ = fs::remove_dir_all(&base);
}
