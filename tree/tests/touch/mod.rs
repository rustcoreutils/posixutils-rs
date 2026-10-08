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

// The spec `-d` forms (T or space separator, comma/dot fraction, Z) are all accepted
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

// -t interprets its fields in the local timezone (TZ), not UTC. Noon in New York
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

// `-c` on a missing file is silent and exits 0.
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

// Sub-second precision is preserved.
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

// `-a -t <past>` on a new file sets only atime; mtime stays at the (current) creation
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

/// Run touch with `args`, killing it if it has not finished in ten seconds.
/// `None` means it hung.
fn touch_bounded(args: &[&str]) -> Option<std::process::Output> {
    use std::process::Stdio;
    use std::time::{Duration, Instant};
    let mut child = Command::new(env!("CARGO_BIN_EXE_touch"))
        .args(args)
        .env("TZ", "UTC")
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

/// A FIFO with no reader is given its times without being opened, so touch
/// cannot wait on it, and is left a FIFO.
#[test]
fn test_touch_fifo_without_reader() {
    use std::os::unix::ffi::OsStrExt;
    use std::os::unix::fs::FileTypeExt;
    let d = dir("test_touch_fifo_without_reader");
    let f = format!("{d}/fifo");
    let c = std::ffi::CString::new(Path::new(&f).as_os_str().as_bytes()).unwrap();
    assert_eq!(unsafe { libc::mkfifo(c.as_ptr(), 0o600) }, 0);

    let out = touch_bounded(&["-t", "200001010000", &f]);
    let is_fifo = fs::symlink_metadata(&f).unwrap().file_type().is_fifo();
    let m = mtime_secs(&f);
    fs::remove_dir_all(&d).unwrap();

    let out = out.expect("touch hung on a FIFO with no reader");
    assert_eq!(out.status.code(), Some(0), "{out:?}");
    assert!(is_fifo);
    assert_eq!(m, 946_684_800);
}

/// `-c` writes no diagnostic for a file that does not exist, and a dangling
/// symlink names no existing file.
#[test]
fn test_touch_c_dangling_symlink_silent() {
    let d = dir("test_touch_c_dangling_symlink");
    let link = format!("{d}/link");
    std::os::unix::fs::symlink(format!("{d}/target"), &link).unwrap();

    let out = touch(None, &["-c", &link]);
    let created = Path::new(&format!("{d}/target")).exists();
    fs::remove_dir_all(&d).unwrap();

    assert_eq!(out.status.code(), Some(0), "{out:?}");
    assert!(out.stderr.is_empty(), "{out:?}");
    assert!(!created, "-c created the symlink's target");
}

/// An existing directory named with a trailing slash gets its times. Linux's
/// open(O_CREAT) refuses such a name with EISDIR before it looks at O_EXCL.
#[test]
fn test_touch_existing_dir_trailing_slash() {
    let d = dir("test_touch_existing_dir_trailing_slash");
    let sub = format!("{d}/sub");
    fs::create_dir(&sub).unwrap();

    let out = touch(Some("UTC"), &["-t", "200001010000", &format!("{sub}/")]);
    let m = mtime_secs(&sub);
    fs::remove_dir_all(&d).unwrap();

    assert_eq!(out.status.code(), Some(0), "{out:?}");
    assert_eq!(m, 946_684_800);
}

/// A trailing slash on a dangling symlink names a directory that does not
/// exist: the times cannot be set, and nothing is created.
#[test]
fn test_touch_dangling_symlink_trailing_slash() {
    let d = dir("test_touch_dangling_symlink_trailing_slash");
    let (link, target) = (format!("{d}/link"), format!("{d}/target"));
    std::os::unix::fs::symlink(&target, &link).unwrap();

    let out = touch(None, &[&format!("{link}/")]);
    let created = Path::new(&target).exists();
    fs::remove_dir_all(&d).unwrap();

    assert_eq!(out.status.code(), Some(1), "{out:?}");
    let enoent = std::io::Error::from_raw_os_error(libc::ENOENT).to_string();
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains(enoent.split(" (os error").next().unwrap()),
        "{stderr}"
    );
    assert!(!created);
}

/// POSIX touch creates a file that does not exist as creat() would, and
/// creat() follows a symlink: the dangling symlink's target is created, and
/// given the requested times.
#[test]
fn test_touch_dangling_symlink_creates_target() {
    let d = dir("test_touch_dangling_symlink_creates_target");
    let (link, target) = (format!("{d}/link"), format!("{d}/target"));
    std::os::unix::fs::symlink(&target, &link).unwrap();

    let out = touch(Some("UTC"), &["-t", "200001010000", &link]);
    let target_md = fs::symlink_metadata(&target);
    let link_is_symlink = fs::symlink_metadata(&link)
        .unwrap()
        .file_type()
        .is_symlink();
    let m = target_md.as_ref().ok().map(|_| mtime_secs(&target));
    fs::remove_dir_all(&d).unwrap();

    assert_eq!(out.status.code(), Some(0), "{out:?}");
    assert!(target_md.unwrap().is_file());
    assert!(link_is_symlink);
    assert_eq!(m, Some(946_684_800));
}

/// A link whose target was made just before touch runs (a FIFO with no
/// reader, a regular file with contents) names an existing file: touch gives
/// it its times without waiting on the FIFO or emptying the file.
#[test]
fn test_touch_symlink_to_a_target_made_just_before() {
    use std::os::unix::ffi::OsStrExt;
    let d = dir("test_touch_symlink_target_made_before");
    let (fifo_link, fifo) = (format!("{d}/fifo_link"), format!("{d}/fifo"));
    let (file_link, file) = (format!("{d}/file_link"), format!("{d}/file"));
    std::os::unix::fs::symlink(&fifo, &fifo_link).unwrap();
    std::os::unix::fs::symlink(&file, &file_link).unwrap();
    let c = std::ffi::CString::new(Path::new(&fifo).as_os_str().as_bytes()).unwrap();
    assert_eq!(unsafe { libc::mkfifo(c.as_ptr(), 0o600) }, 0);
    fs::write(&file, b"contents").unwrap();

    let out = touch_bounded(&["-t", "200001010000", &fifo_link, &file_link]);
    let (fifo_m, file_m) = (mtime_secs(&fifo), mtime_secs(&file));
    let contents = fs::read(&file).unwrap();
    fs::remove_dir_all(&d).unwrap();

    let out = out.expect("touch hung on a FIFO");
    assert_eq!(out.status.code(), Some(0), "{out:?}");
    assert_eq!((fifo_m, file_m), (946_684_800, 946_684_800));
    assert_eq!(contents, b"contents");
}

/// A file time as (seconds, nanoseconds) since the epoch.
type Stamp = (i64, u32);

/// Run `touch -d date` on a new file in a fresh directory, under a zone that
/// is neither UTC nor any offset below, so a result that leaned on `TZ` shows.
/// The file's (atime, mtime), or the failed output.
fn touch_d(date: &str) -> Result<(Stamp, Stamp), std::process::Output> {
    use std::os::unix::fs::MetadataExt;
    let dir = plib::tmp::tempdir().unwrap();
    let f = dir.path().join("f");
    let out = Command::new(env!("CARGO_BIN_EXE_touch"))
        .args(["-d", date])
        .arg(&f)
        .env("TZ", "America/New_York")
        .output()
        .unwrap();
    if !out.status.success() {
        assert!(!f.exists(), "a rejected -d created the file: {date:?}");
        return Err(out);
    }
    let md = fs::metadata(&f).unwrap();
    Ok((
        (md.atime(), md.atime_nsec() as u32),
        (md.mtime(), md.mtime_nsec() as u32),
    ))
}

/// The mtime `touch -d date` gives, both times set to the same whole second.
fn d_mtime(date: &str) -> i64 {
    match touch_d(date) {
        Ok((atime, mtime)) => {
            assert_eq!(atime, mtime, "{date:?}");
            assert_eq!(mtime.1, 0, "{date:?}");
            mtime.0
        }
        Err(out) => panic!("{date:?} rejected: {out:?}"),
    }
}

/// The RFC 5322 date Debian's base-files passes from `dpkg-parsechangelog
/// -SDate`. GNU touch 9.4 gives it stat `%Y` 1784307900, `%y` (in UTC)
/// 2026-07-17 17:05:00.000000000.
#[test]
fn test_touch_d_rfc5322_base_files() {
    assert_eq!(d_mtime("Fri, 17 Jul 2026 19:05:00 +0200"), 1_784_307_900);
}

/// The day name is optional, the day may be one digit, and the seconds may
/// be left out. Each value is what GNU touch 9.4 gives the same string.
#[test]
fn test_touch_d_rfc5322_optional_parts() {
    assert_eq!(d_mtime("17 Jul 2026 19:05:00 +0200"), 1_784_307_900);
    assert_eq!(d_mtime("Tue, 7 Jul 2026 19:05:00 +0200"), 1_783_443_900);
    assert_eq!(d_mtime("Tue, 07 Jul 2026 19:05:00 +0200"), 1_783_443_900);
    assert_eq!(d_mtime("Fri, 17 Jul 2026 19:05 +0200"), 1_784_307_900);
}

/// The zone offset is applied whatever its sign; `+0000` and `-0000` are UTC.
#[test]
fn test_touch_d_rfc5322_offsets() {
    let utc = 1_784_315_100; // 2026-07-17 19:05:00 UTC
    assert_eq!(d_mtime("Fri, 17 Jul 2026 19:05:00 +0000"), utc);
    assert_eq!(d_mtime("Fri, 17 Jul 2026 19:05:00 -0000"), utc);
    assert_eq!(
        d_mtime("Fri, 17 Jul 2026 19:05:00 +0530"),
        utc - 5 * 3600 - 1800
    );
    assert_eq!(d_mtime("Fri, 17 Jul 2026 19:05:00 -0700"), utc + 7 * 3600);
}

/// An offset can carry the instant across a year boundary either way.
#[test]
fn test_touch_d_rfc5322_year_boundary() {
    // 2027-01-01 00:30:00 UTC and 2026-12-31 23:30:00 UTC, as GNU touch gives.
    assert_eq!(d_mtime("Thu, 31 Dec 2026 23:30:00 -0100"), 1_798_763_400);
    assert_eq!(d_mtime("Fri, 1 Jan 2027 00:30:00 +0100"), 1_798_759_800);
}

/// Anything outside the strict grammar is refused, with nothing created.
#[test]
fn test_touch_d_rfc5322_rejections() {
    for date in [
        "Mon, 17 Jul 2026 19:05:00 +0200",    // 17 Jul 2026 is a Friday
        "Fri, 30 Feb 2026 19:05:00 +0200",    // no such day
        "Fri, 17 Jul 2026 24:00:00 +0200",    // hour 24
        "Fri, 17 Jul 2026 23:59:60 +0000",    // leap second
        "Fri, 17 Jul 2026 19:60:00 +0200",    // minute 60
        "Fri, 17 jul 2026 19:05:00 +0200",    // month not as date -R spells it
        "Fri, 17 July 2026 19:05:00 +0200",   // full month name
        "fri, 17 Jul 2026 19:05:00 +0200",    // day not as date -R spells it
        "Friday, 17 Jul 2026 19:05:00 +0200", // full day name
        "Fri 17 Jul 2026 19:05:00 +0200",     // day name without comma
        "Fri,17 Jul 2026 19:05:00 +0200",     // no space after the comma
        "Fri, 17 Jul 2026 19:05:00",          // missing zone
        "Fri, 17 Jul 2026 19:05:00 GMT",      // zone names
        "Fri, 17 Jul 2026 19:05:00 UT",
        "Fri, 17 Jul 2026 19:05:00 Z",
        "Fri, 17 Jul 2026 19:05:00 EST",
        "Fri, 17 Jul 2026 19:05:00 +200",    // zone not four digits
        "Fri, 17 Jul 2026 19:05:00 +0260",   // zone minutes 60
        "Fri, 17 Jul 2026 19:05:00 +2400",   // zone hours 24
        "Fri, 17 Jul 2026 19:05:00 +0200 x", // trailing text
        "Fri, 17 Jul 2026 19:05:00 +0200 ",  // trailing space
        " Fri, 17 Jul 2026 19:05:00 +0200",  // leading space
        "Fri,  17 Jul 2026 19:05:00 +0200",  // double space
        "Fri, 17  Jul 2026 19:05:00 +0200",
        "Fri,\t17 Jul 2026 19:05:00 +0200", // tab
        "Fri, 17\tJul 2026 19:05:00 +0200",
        "Fri, 017 Jul 2026 19:05:00 +0200",  // three-digit day
        "Fri, 17 Jul 26 19:05:00 +0200",     // two-digit year
        "Fri, 17 Jul 2026 9:05:00 +0200",    // one-digit hour
        "Fri, 17 Jul 2026 19:5:00 +0200",    // one-digit minute
        "Fri, 17 Jul 2026 19:05:00.5 +0200", // fractional seconds
        "Fri, 17 Jul 2026 +0200",            // no time
    ] {
        let out = touch_d(date).expect_err(date);
        assert_eq!(out.status.code(), Some(1), "{date:?}: {out:?}");
        assert!(!out.stderr.is_empty(), "{date:?}");
    }
}

/// Every date base-files' debian/timestamps gives its license files, the
/// POSIX form followed by ` UTC`. Each value is GNU touch 9.4's stat `%Y`.
#[test]
fn test_touch_d_utc_word_base_files() {
    for (date, secs) in [
        ("1999-08-26 12:06:20 UTC", 935_669_180),
        ("2004-12-19 20:30:25 UTC", 1_103_488_225),
        ("2017-04-03 11:00:00 UTC", 1_491_217_200),
        ("2017-04-03 20:00:00 UTC", 1_491_249_600),
        ("2017-04-25 22:26:15 UTC", 1_493_159_175),
        ("2017-09-30 07:14:21 UTC", 1_506_755_661),
        ("2019-02-18 09:59:20 UTC", 1_550_483_960),
        ("2022-02-10 06:14:38 UTC", 1_644_473_678),
        ("2023-09-11 21:49:40 UTC", 1_694_468_980),
        ("2023-09-11 21:49:41 UTC", 1_694_468_981),
        ("2024-09-18 13:56:22 UTC", 1_726_667_782),
        ("2024-09-18 14:33:26 UTC", 1_726_670_006),
        ("2024-09-18 14:33:27 UTC", 1_726_670_007),
        ("2024-09-18 14:33:28 UTC", 1_726_670_008),
        ("2024-09-18 14:33:29 UTC", 1_726_670_009),
        ("2026-01-12 21:19:44 UTC", 1_768_252_784),
        ("2026-05-29 10:00:00 UTC", 1_780_048_800),
    ] {
        assert_eq!(d_mtime(date), secs, "{date:?}");
    }
}

/// ` UTC` and ` GMT` mean what `Z` means, after either separator and with a
/// fraction, as GNU touch 9.4 gives them.
#[test]
fn test_touch_d_utc_word_forms() {
    let secs = 935_669_180; // 1999-08-26 12:06:20 UTC
    assert_eq!(d_mtime("1999-08-26T12:06:20Z"), secs);
    assert_eq!(d_mtime("1999-08-26T12:06:20 UTC"), secs);
    assert_eq!(d_mtime("1999-08-26 12:06:20 GMT"), secs);
    assert_eq!(d_mtime("1999-08-26T12:06:20 GMT"), secs);
    for date in ["1999-08-26 12:06:20.25 UTC", "1999-08-26T12:06:20,25 GMT"] {
        let ((_, _), mtime) = touch_d(date).unwrap();
        assert_eq!(mtime, (secs, 250_000_000), "{date:?}");
    }
}

/// Only a single space and then exactly `UTC` or `GMT` is the zone word.
#[test]
fn test_touch_d_utc_word_rejections() {
    for date in [
        "1999-08-26 12:06:20 utc",        // GNU accepts
        "1999-08-26 12:06:20 Utc",        // GNU accepts
        "1999-08-26 12:06:20 EST",        // GNU rejects
        "1999-08-26 12:06:20 UT",         // GNU accepts
        "1999-08-26 12:06:20 CET",        // GNU accepts, as UTC+1
        "1999-08-26 12:06:20Z UTC",       // GNU rejects
        "1999-08-26 12:06:20 +00:00 UTC", // a word after an offset
        "1999-08-26 12:06:20  UTC",       // GNU accepts
        "1999-08-26 12:06:20\tUTC",       // GNU accepts
        "1999-08-26 12:06:20UTC",         // GNU accepts
        "1999-08-26 12:06:20 UTC x",      // GNU rejects
        "1999-08-26 12:06:20 UTC ",       // GNU accepts
        "1999-08-26 12:06 UTC",           // no seconds, as with Z; GNU accepts
        "1999-08-26 UTC",                 // no time
        "UTC",
    ] {
        let out = touch_d(date).expect_err(date);
        assert_eq!(out.status.code(), Some(1), "{date:?}: {out:?}");
        assert!(!out.stderr.is_empty(), "{date:?}");
    }
}

/// The POSIX `-d` forms give the same instants as before, in `TZ` when they
/// have no zone.
#[test]
fn test_touch_d_posix_forms_unchanged() {
    // 2007-11-12 10:15:30 in New York (EST, UTC-5) and in UTC.
    let local = 1_194_880_530;
    let utc = local - 5 * 3600;
    assert_eq!(d_mtime("2007-11-12T10:15:30"), local);
    assert_eq!(d_mtime("2007-11-12 10:15:30"), local);
    assert_eq!(d_mtime("2007-11-12T10:15:30Z"), utc);
    assert_eq!(d_mtime("2007-11-12T10:15:30,000"), local);
    let ((_, _), mtime) = touch_d("2007-11-12T10:15:30.25Z").unwrap();
    assert_eq!(mtime, (utc, 250_000_000));
}

// XBD 12.2, Guideline 7: an option-argument may begin with '-'. Each option
// below used to have the word after it refused as an unknown option.
#[test]
fn option_argument_may_begin_with_hyphen() {
    for opt in ["-d", "-t", "-r"] {
        plib::testing::assert_hyphen_option_argument("touch", &[opt, "-zq", "--help"]);
    }
}
