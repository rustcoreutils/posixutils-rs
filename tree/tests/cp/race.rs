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
