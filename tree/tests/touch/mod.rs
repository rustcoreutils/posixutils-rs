//
// Copyright (c) 2024-2026 Hemi Labs, Inc.
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

use std::fs;
use std::path::Path;
use std::process::Command;
use std::time::UNIX_EPOCH;

fn touch(env_tz: Option<&str>, args: &[&str]) -> std::process::Output {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_touch"));
    cmd.args(args);
    if let Some(tz) = env_tz {
        cmd.env("TZ", tz);
    }
    cmd.output().unwrap()
}

fn dir(name: &str) -> String {
    let d = format!("{}/{name}", env!("CARGO_TARGET_TMPDIR"));
    let _ = fs::remove_dir_all(&d);
    fs::create_dir_all(&d).unwrap();
    d
}

fn mtime_secs(p: &str) -> u64 {
    fs::metadata(p)
        .unwrap()
        .modified()
        .unwrap()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

fn atime_secs(p: &str) -> u64 {
    fs::metadata(p)
        .unwrap()
        .accessed()
        .unwrap()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs()
}

#[test]
fn test_touch_creates_file() {
    let d = dir("test_touch_creates_file");
    let f = format!("{d}/f");
    assert!(touch(None, &[&f]).status.success());
    assert!(Path::new(&f).exists());
    fs::remove_dir_all(&d).unwrap();
}

// Audit #TO2: the spec `-d` forms (T or space separator, comma/dot fraction, Z) are all accepted
// and yield the same instant (compared under a fixed UTC zone).
#[test]
fn test_touch_d_iso_forms() {
    let d = dir("test_touch_d_iso_forms");
    let mut times = vec![];
    for form in [
        "2007-11-12T10:15:30",
        "2007-11-12 10:15:30",
        "2007-11-12T10:15:30Z",
        "2007-11-12T10:15:30,000",
    ] {
        let f = format!("{d}/f");
        assert!(
            touch(Some("UTC"), &["-d", form, &f]).status.success(),
            "form {form}"
        );
        times.push(mtime_secs(&f));
    }
    assert!(
        times.iter().all(|&t| t == times[0]),
        "forms differ: {times:?}"
    );
    assert_ne!(times[0], 0);
    fs::remove_dir_all(&d).unwrap();
}

// Audit #TO3: -t interprets its fields in the local timezone (TZ), not UTC. Noon in New York
// (UTC-5 in January) is 5 hours later in epoch terms than noon UTC.
#[test]
fn test_touch_t_honors_tz() {
    let d = dir("test_touch_t_honors_tz");
    let fu = format!("{d}/fu");
    let fn_ = format!("{d}/fn");
    assert!(touch(Some("UTC"), &["-t", "200701011200", &fu])
        .status
        .success());
    assert!(
        touch(Some("America/New_York"), &["-t", "200701011200", &fn_])
            .status
            .success()
    );
    let diff = mtime_secs(&fn_) as i64 - mtime_secs(&fu) as i64;
    assert_eq!(diff, 5 * 3600, "expected a 5h TZ offset, got {diff}s");
    fs::remove_dir_all(&d).unwrap();
}

// Audit #TO4: `-c` on a missing file is silent and exits 0.
#[test]
fn test_touch_c_missing_silent() {
    let d = dir("test_touch_c_missing_silent");
    let f = format!("{d}/does_not_exist");
    let out = touch(None, &["-c", &f]);
    assert_eq!(out.status.code(), Some(0));
    assert!(out.stderr.is_empty());
    assert!(!Path::new(&f).exists());
    fs::remove_dir_all(&d).unwrap();
}

// Audit #TO5: sub-second precision is preserved.
#[test]
fn test_touch_subsecond_preserved() {
    let d = dir("test_touch_subsecond_preserved");
    let f = format!("{d}/f");
    assert!(
        touch(Some("UTC"), &["-d", "2020-05-05 01:02:03.123456789", &f])
            .status
            .success()
    );
    let nsec = fs::metadata(&f)
        .unwrap()
        .modified()
        .unwrap()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .subsec_nanos();
    assert_eq!(nsec, 123_456_789);
    fs::remove_dir_all(&d).unwrap();
}

// Audit #TO6: `-a -t <past>` on a new file sets only atime; mtime stays at the (current) creation
// time, not the past option time.
#[test]
fn test_touch_a_newfile_leaves_mtime_now() {
    let d = dir("test_touch_a_newfile_leaves_mtime_now");
    let f = format!("{d}/f");
    assert!(touch(Some("UTC"), &["-a", "-t", "200001010000", &f])
        .status
        .success());
    let a = atime_secs(&f);
    let m = mtime_secs(&f);
    assert!(
        a < 1_000_000_000,
        "atime should be the year-2000 option time: {a}"
    );
    assert!(
        m > a,
        "mtime should be ~now, not the past atime: m={m} a={a}"
    );
    fs::remove_dir_all(&d).unwrap();
}
