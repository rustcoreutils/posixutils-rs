//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! NFSv4-style ACLs carried in a pax archive as `SCHILY.acl.ace`, in libarchive's text form:
//! written for a macOS extended ACL or a Linux NFSv4 mount's, and restored under `-p p` and
//! `-p e` only where the same kind is held -- elsewhere a loss, which fails the member and
//! narrows its mode, unless the ACL says nothing the mode does not.

use crate::common::{pax_record, run_pax_in_dir, ustar_trailer, Ustar};
use plib::tmp::tempdir;
use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;

/// An archive of the regular file `f` (mode 0754) whose extended header holds `records`.
fn archive_with(records: &[u8]) -> Vec<u8> {
    let mut archive = Ustar {
        name: b"PaxHeaders/f",
        typeflag: b'x',
        body: records,
        ..Default::default()
    }
    .member();
    archive.extend(
        Ustar {
            name: b"f",
            mode: 0o754,
            body: b"data\n",
            ..Default::default()
        }
        .member(),
    );
    archive.extend(ustar_trailer());
    archive
}

/// Extract `archive` with `pax -r -p p` in a fresh directory under `temp`; the run's output,
/// and the permission bits `f` ends with.
fn extract(temp: &Path, archive: &[u8]) -> (std::process::Output, u32) {
    let x = temp.join("x");
    let _ = fs::remove_dir_all(&x);
    fs::create_dir(&x).unwrap();
    fs::write(temp.join("a.pax"), archive).unwrap();
    let out = run_pax_in_dir(&["-r", "-p", "p", "-f", "../a.pax"], &x);
    let f = x.join("f");
    assert_eq!(fs::read(&f).unwrap(), b"data\n");
    let mode = fs::metadata(&f).unwrap().permissions().mode() & 0o7777;
    (out, mode)
}

/// An NFSv4 ACL naming someone cannot be held on a local Linux filesystem, which holds POSIX
/// ACLs or none: the member is extracted with a mode granting nobody but its owner anything,
/// and the run fails. One that says only what the mode does is no loss. Without -p p the
/// record is not used.
#[cfg(target_os = "linux")]
#[test]
fn pax_r_p_cannot_restore_an_ace_on_linux() {
    let temp = tempdir().unwrap();
    let named = pax_record(
        "SCHILY.acl.ace",
        b"user:65534:raRcs::allow:65534,owner@:rwxpaARWcCos::allow,everyone@:raRcs::allow",
    );
    let (out, mode) = extract(temp.path(), &archive_with(&named));
    assert_eq!(out.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("cannot set ACL"), "{stderr}");
    assert_eq!(mode, 0o700);

    let trivial = pax_record(
        "SCHILY.acl.ace",
        b"owner@:rwxpaARWcCos::allow,group@:rxaRcs::allow,everyone@:raRcs::allow",
    );
    let (out, mode) = extract(temp.path(), &archive_with(&trivial));
    assert_eq!(out.status.code(), Some(0), "{:?}", out.stderr);
    assert_eq!(mode, 0o754);

    let x = temp.path().join("x");
    fs::remove_dir_all(&x).unwrap();
    fs::create_dir(&x).unwrap();
    fs::write(temp.path().join("a.pax"), archive_with(&named)).unwrap();
    let out = run_pax_in_dir(&["-r", "-f", "../a.pax"], &x);
    assert_eq!(out.status.code(), Some(0), "{:?}", out.stderr);
}

/// An ace record that is not libarchive's text is that member's failure, as a bad POSIX ACL
/// record is.
#[test]
fn pax_r_p_narrows_the_mode_on_a_bad_ace() {
    let temp = tempdir().unwrap();
    let bad = pax_record("SCHILY.acl.ace", b"owner@:rwxq::allow");
    let (out, mode) = extract(temp.path(), &archive_with(&bad));
    assert_eq!(out.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("SCHILY.acl.ace"), "{stderr}");
    assert_eq!(mode, 0o700);
}

/// A member with both a POSIX ACL and an NFSv4 one is given the kind its file takes -- on
/// Linux the POSIX one -- but the other kind is lost: the run fails, and the mode is narrowed
/// to the owner's, which masks the named entries off. Skipped where the filesystem takes no
/// POSIX ACLs.
#[cfg(target_os = "linux")]
#[test]
fn pax_r_p_applies_the_kind_the_file_takes() {
    use std::os::fd::AsRawFd;
    let temp = tempdir().unwrap();
    let probe = fs::File::create(temp.path().join("probe")).unwrap();
    let named = plib::acl::Acl {
        access: Some(
            plib::acl::PosixAcl::from_text("u::rw-,u:65534:r--,g::r--,m::r--,o::---").unwrap(),
        ),
        ..Default::default()
    };
    if plib::acl::write_fd(probe.as_raw_fd(), &named).is_err() {
        eprintln!("note: this filesystem takes no ACLs; case skipped");
        return;
    }
    let mut records = pax_record(
        "SCHILY.acl.access",
        b"user::rwx,user:65534:r--,group::r-x,mask::r-x,other::r--",
    );
    records.extend(pax_record(
        "SCHILY.acl.ace",
        b"user:65534:raRcs::deny:65534",
    ));
    let (out, mode) = extract(temp.path(), &archive_with(&records));
    assert_eq!(out.status.code(), Some(1), "{:?}", out.stderr);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("cannot set ACL"), "{stderr}");
    assert_eq!(mode, 0o700);
    let acl = plib::acl::read_path(&temp.path().join("x/f"), true).unwrap();
    let want = "user::rwx,user:65534:r--,group::r-x,mask::---,other::---";
    assert_eq!(
        acl.access,
        Some(plib::acl::PosixAcl::from_text(want).unwrap())
    );
    assert_eq!(acl.native, None);
}

/// `-o listopt=%(SCHILY.acl.ace)s` reports the record.
#[test]
fn pax_lists_the_ace_record() {
    let temp = tempdir().unwrap();
    let ace = "user:65534:raRcs::allow:65534,everyone@:raRcs::allow";
    fs::write(
        temp.path().join("a.pax"),
        archive_with(&pax_record("SCHILY.acl.ace", ace.as_bytes())),
    )
    .unwrap();
    let out = run_pax_in_dir(
        &["-f", "a.pax", "-o", "listopt=%(SCHILY.acl.ace)s"],
        temp.path(),
    );
    assert_eq!(out.status.code(), Some(0), "{:?}", out.stderr);
    assert_eq!(String::from_utf8_lossy(&out.stdout).trim_end(), ace);
}

/// The ACL entries `ls -le` shows for `path`: the numbered lines after the file's own.
#[cfg(target_os = "macos")]
fn ls_acl(path: &Path) -> Vec<String> {
    let out = std::process::Command::new("ls")
        .arg("-led")
        .arg(path)
        .output()
        .unwrap();
    assert!(out.status.success());
    String::from_utf8_lossy(&out.stdout)
        .lines()
        .skip(1)
        .map(|line| line.trim().to_string())
        .collect()
}

/// A macOS extended ACL is archived as `SCHILY.acl.ace` and restored by `-p e`, and copied by
/// `-rw -p e`: `ls -le` shows the same entries on the copy, with the same mode.
#[cfg(target_os = "macos")]
#[test]
fn pax_round_trips_a_macos_acl() {
    let temp = tempdir().unwrap();
    let src = temp.path().join("src");
    fs::create_dir(&src).unwrap();
    let f = src.join("f");
    fs::write(&f, "data\n").unwrap();
    fs::set_permissions(&f, fs::Permissions::from_mode(0o640)).unwrap();
    let chmod = std::process::Command::new("chmod")
        .args(["+a", "user:nobody allow read"])
        .arg(&f)
        .status()
        .unwrap();
    if !chmod.success() {
        eprintln!("note: this filesystem takes no ACLs; case skipped");
        return;
    }
    let want = ls_acl(&f);
    assert!(!want.is_empty() && want[0].contains("nobody"), "{want:?}");

    let out = run_pax_in_dir(&["-w", "-x", "pax", "-f", "../a.pax", "f"], &src);
    assert_eq!(out.status.code(), Some(0), "{:?}", out.stderr);
    let archive = fs::read(temp.path().join("a.pax")).unwrap();
    let text = String::from_utf8_lossy(&archive);
    assert!(
        text.contains("SCHILY.acl.ace=user:nobody:r::allow:"),
        "{text}"
    );
    assert!(text.contains(",owner@:"), "{text}");

    let x = temp.path().join("x");
    fs::create_dir(&x).unwrap();
    let out = run_pax_in_dir(&["-r", "-p", "e", "-f", "../a.pax"], &x);
    assert_eq!(out.status.code(), Some(0), "{:?}", out.stderr);
    assert_eq!(ls_acl(&x.join("f")), want);
    let mode = |p: &Path| fs::metadata(p).unwrap().permissions().mode() & 0o7777;
    assert_eq!(mode(&x.join("f")), 0o640);

    let c = temp.path().join("c");
    fs::create_dir(&c).unwrap();
    let out = run_pax_in_dir(&["-rw", "-p", "e", "f", "../c"], &src);
    assert_eq!(out.status.code(), Some(0), "{:?}", out.stderr);
    assert_eq!(ls_acl(&c.join("f")), want);
    assert_eq!(mode(&c.join("f")), 0o640);
}
