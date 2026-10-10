//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! Extended attributes carried in a pax archive: `-x pax` writes each as a
//! `SCHILY.xattr.<name>` record holding its raw value, as GNU tar --xattrs and star do; `-p e`
//! alone restores them, from those and from libarchive's `LIBARCHIVE.xattr.<name>` records;
//! copy mode copies them under `-p e` too. Each case needs a filesystem that takes `user.`
//! attributes, and is skipped without one.

use crate::common::{pax_record, run_pax_in_dir, system_tool, ustar_trailer, Ustar};
use plib::tmp::{tempdir, TempDir};
use std::ffi::CString;
use std::fs;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::process::{Command, Output};

/// The attribute `name` of `path` (not followed), set to `value`; `false`, with a note,
/// where the filesystem takes no `user.` attributes.
fn set_xattr(path: &Path, name: &[u8], value: &[u8]) -> bool {
    let path = CString::new(path.as_os_str().as_bytes()).unwrap();
    let name = CString::new(name).unwrap();
    let (ptr, len) = (value.as_ptr().cast(), value.len());
    #[cfg(target_os = "linux")]
    let ret = unsafe { libc::lsetxattr(path.as_ptr(), name.as_ptr(), ptr, len, 0) };
    #[cfg(target_os = "macos")]
    let ret = unsafe {
        libc::setxattr(
            path.as_ptr(),
            name.as_ptr(),
            ptr,
            len,
            0,
            libc::XATTR_NOFOLLOW,
        )
    };
    if ret != 0 {
        let e = std::io::Error::last_os_error();
        assert_eq!(e.raw_os_error(), Some(libc::ENOTSUP), "setxattr: {e}");
        eprintln!("note: this filesystem takes no user. attributes; case skipped");
        return false;
    }
    true
}

/// The `user.` attributes of `path` (not followed), sorted, with their values.
fn user_xattrs(path: &Path) -> Vec<(Vec<u8>, Vec<u8>)> {
    let path = CString::new(path.as_os_str().as_bytes()).unwrap();
    let mut names = vec![0u8; 65536];
    #[cfg(target_os = "linux")]
    let n = unsafe { libc::llistxattr(path.as_ptr(), names.as_mut_ptr().cast(), names.len()) };
    #[cfg(target_os = "macos")]
    let n = unsafe {
        libc::listxattr(
            path.as_ptr(),
            names.as_mut_ptr().cast(),
            names.len(),
            libc::XATTR_NOFOLLOW,
        )
    };
    let n = usize::try_from(n).expect("listxattr");
    names.truncate(n);
    let mut all: Vec<_> = names
        .split(|&b| b == 0)
        .filter(|name| name.starts_with(b"user."))
        .map(|name| {
            let cname = CString::new(name).unwrap();
            let mut value = vec![0u8; 65536];
            #[cfg(target_os = "linux")]
            let n = unsafe {
                libc::lgetxattr(
                    path.as_ptr(),
                    cname.as_ptr(),
                    value.as_mut_ptr().cast(),
                    value.len(),
                )
            };
            #[cfg(target_os = "macos")]
            let n = unsafe {
                libc::getxattr(
                    path.as_ptr(),
                    cname.as_ptr(),
                    value.as_mut_ptr().cast(),
                    value.len(),
                    0,
                    libc::XATTR_NOFOLLOW,
                )
            };
            value.truncate(usize::try_from(n).expect("getxattr"));
            (name.to_vec(), value)
        })
        .collect();
    all.sort();
    all
}

fn assert_ok(out: &Output, what: &str) {
    assert_eq!(
        out.status.code(),
        Some(0),
        "{what}: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// A value with every byte a record could trip over: a NUL, a newline, `=`, a space, and
/// bytes that are not UTF-8.
const ODD_VALUE: &[u8] = b"\x00\x01\n\xff= \xc3";

/// The attributes `tree_with_xattrs` gives `f`; a name that is not UTF-8 only on Linux, where
/// a name is bytes.
fn file_xattrs() -> Vec<(&'static [u8], &'static [u8])> {
    let mut all: Vec<(&[u8], &[u8])> = vec![
        (b"user.foo", b"bar"),
        (b"user.empty", b""),
        (b"user.odd", ODD_VALUE),
        (b"user.n\xc3\xa9 x=y%25", b"named oddly"),
    ];
    if cfg!(target_os = "linux") {
        all.push((b"user.raw\xff", b"y"));
    }
    all
}

/// A scratch directory with `src` holding `f` (`file_xattrs`) and `d` (`user.dir`), and an
/// empty `x`; `None` where no `user.` attribute can be set.
fn tree_with_xattrs() -> Option<TempDir> {
    let temp = tempdir().unwrap();
    let src = temp.path().join("src");
    fs::create_dir_all(src.join("d")).unwrap();
    fs::create_dir(temp.path().join("x")).unwrap();
    fs::write(src.join("f"), "f\n").unwrap();
    for (name, value) in file_xattrs() {
        if !set_xattr(&src.join("f"), name, value) {
            return None;
        }
    }
    set_xattr(&src.join("d"), b"user.dir", b"dv").then_some(temp)
}

/// Each of `f` and `d` under `x` has the `user.` attributes it has under `src`.
fn assert_same_xattrs(temp: &Path, what: &str) {
    for name in ["f", "d"] {
        let want = user_xattrs(&temp.join("src").join(name));
        assert!(!want.is_empty());
        assert_eq!(
            user_xattrs(&temp.join("x").join(name)),
            want,
            "{what}: {name}"
        );
    }
}

/// Each of `f` and `d` under `x` has no `user.` attribute.
fn assert_no_xattrs(temp: &Path, what: &str) {
    for name in ["f", "d"] {
        assert_eq!(
            user_xattrs(&temp.join("x").join(name)),
            vec![],
            "{what}: {name}"
        );
    }
}

/// Empty `x` again.
fn clear_x(temp: &Path) -> std::path::PathBuf {
    let x = temp.join("x");
    fs::remove_dir_all(&x).unwrap();
    fs::create_dir(&x).unwrap();
    x
}

/// GNU tar, where the system has it: `--xattrs` means something else, or nothing, to others.
fn gnu_tar() -> Option<std::path::PathBuf> {
    let tar = system_tool("tar")?;
    let version = Command::new(&tar).arg("--version").output().ok()?;
    let gnu = String::from_utf8_lossy(&version.stdout).contains("GNU tar");
    if !gnu {
        eprintln!("skipping cross-tool check: tar is not GNU tar");
    }
    gnu.then_some(tar)
}

/// `-x pax` writes each attribute as GNU tar does -- the name and the value raw -- and
/// `-p e` restores them on a file and a directory; `-p p` does not.
#[test]
fn pax_round_trips_xattrs_under_p_e_only() {
    let Some(temp) = tree_with_xattrs() else {
        return;
    };
    let src = temp.path().join("src");
    let out = run_pax_in_dir(&["-w", "-x", "pax", "-f", "../a.pax", "f", "d"], &src);
    assert_ok(&out, "pax -w -x pax");
    let archive = fs::read(temp.path().join("a.pax")).unwrap();
    let contains = |needle: &[u8]| archive.windows(needle.len()).any(|w| w == needle);
    assert!(contains(b" SCHILY.xattr.user.foo=bar\n"));
    assert!(contains(b" SCHILY.xattr.user.empty=\n"));
    assert!(contains(
        &[b" SCHILY.xattr.user.odd=".as_slice(), ODD_VALUE, b"\n"].concat()
    ));
    assert!(contains(b" SCHILY.xattr.user.dir=dv\n"));
    // A `=` would end the keyword: GNU tar spells it `%3D`, and a `%` `%25`.
    assert!(contains(
        b" SCHILY.xattr.user.n\xc3\xa9 x%3Dy%2525=named oddly\n"
    ));

    let x = clear_x(temp.path());
    let out = run_pax_in_dir(&["-r", "-p", "e", "-f", "../a.pax"], &x);
    assert_ok(&out, "pax -r -p e");
    assert_same_xattrs(temp.path(), "pax -r -p e");

    let x = clear_x(temp.path());
    let out = run_pax_in_dir(&["-r", "-p", "p", "-f", "../a.pax"], &x);
    assert_ok(&out, "pax -r -p p");
    assert_no_xattrs(temp.path(), "pax -r -p p");
}

/// Neither ustar nor cpio has a place for an attribute: none is written, and nothing said.
#[test]
fn pax_writes_no_xattrs_but_in_the_pax_format() {
    let Some(temp) = tree_with_xattrs() else {
        return;
    };
    let src = temp.path().join("src");
    for format in ["ustar", "cpio"] {
        let out = run_pax_in_dir(&["-w", "-x", format, "-f", "../a", "f", "d"], &src);
        assert_ok(&out, format);
        assert!(out.stderr.is_empty(), "{format}");
        let archive = fs::read(temp.path().join("a")).unwrap();
        assert!(
            !archive.windows(5).any(|w| w == b"xattr"),
            "{format} archive holds an attribute"
        );
    }
}

/// Copy mode copies the attributes under `-p e`, and not under `-p p`.
#[test]
fn pax_rw_p_e_copies_xattrs() {
    let Some(temp) = tree_with_xattrs() else {
        return;
    };
    let src = temp.path().join("src");
    let out = run_pax_in_dir(&["-rw", "-p", "e", "f", "d", "../x"], &src);
    assert_ok(&out, "pax -rw -p e");
    assert_same_xattrs(temp.path(), "pax -rw -p e");

    clear_x(temp.path());
    let out = run_pax_in_dir(&["-rw", "-p", "p", "f", "d", "../x"], &src);
    assert_ok(&out, "pax -rw -p p");
    assert_no_xattrs(temp.path(), "pax -rw -p p");
}

/// A read-only file (0444) and directory (0555) still take their attributes under `-p e`, in
/// read and in copy mode, and keep their modes, as GNU tar --xattrs -xp gives them theirs:
/// write permission is lent to the file pax made while they are set.
#[test]
fn pax_p_e_sets_xattrs_on_read_only_members() {
    use std::os::unix::fs::PermissionsExt;
    let Some(temp) = tree_with_xattrs() else {
        return;
    };
    let src = temp.path().join("src");
    fs::set_permissions(src.join("f"), fs::Permissions::from_mode(0o444)).unwrap();
    fs::set_permissions(src.join("d"), fs::Permissions::from_mode(0o555)).unwrap();
    let mode = |path: &Path| fs::metadata(path).unwrap().permissions().mode() & 0o7777;
    let check = |out: &Output, what: &str| {
        assert_ok(out, what);
        assert_eq!(String::from_utf8_lossy(&out.stderr), "", "{what}");
        assert_same_xattrs(temp.path(), what);
        assert_eq!(mode(&temp.path().join("x/f")), 0o444, "{what}");
        assert_eq!(mode(&temp.path().join("x/d")), 0o555, "{what}");
    };
    let out = run_pax_in_dir(&["-w", "-x", "pax", "-f", "../a.pax", "f", "d"], &src);
    assert_ok(&out, "pax -w -x pax");

    let x = clear_x(temp.path());
    let out = run_pax_in_dir(&["-r", "-p", "e", "-f", "../a.pax"], &x);
    check(&out, "pax -r -p e");

    clear_x(temp.path());
    let out = run_pax_in_dir(&["-rw", "-p", "e", "f", "d", "../x"], &src);
    check(&out, "pax -rw -p e");
}

/// What GNU tar --xattrs writes, pax -r -p e restores; what pax writes, GNU tar --xattrs -xp
/// restores.
#[test]
fn pax_and_gnu_tar_read_each_others_xattrs() {
    let Some(tar) = gnu_tar() else {
        return;
    };
    let Some(temp) = tree_with_xattrs() else {
        return;
    };
    let src = temp.path().join("src");
    let x = temp.path().join("x");
    let out = Command::new(&tar)
        .args(["--xattrs", "--format=posix", "-cf", "../t.tar", "f", "d"])
        .current_dir(&src)
        .output()
        .unwrap();
    assert_ok(&out, "tar --xattrs -cf");
    let out = run_pax_in_dir(&["-r", "-p", "e", "-f", "../t.tar"], &x);
    assert_ok(&out, "pax -r -p e");
    assert_same_xattrs(temp.path(), "pax -r -p e of tar --xattrs -cf");

    let x = clear_x(temp.path());
    let out = run_pax_in_dir(&["-w", "-x", "pax", "-f", "../a.pax", "f", "d"], &src);
    assert_ok(&out, "pax -w -x pax");
    let out = Command::new(&tar)
        .args(["--xattrs", "-xpf", "../a.pax"])
        .current_dir(&x)
        .output()
        .unwrap();
    assert_ok(&out, "tar --xattrs -xpf");
    assert_same_xattrs(temp.path(), "tar --xattrs -xpf of pax -w -x pax");
}

/// A pax archive of one regular file `f`, owned by whoever runs the test (so that `-p e`
/// can give it its owner), after an `x` header holding `records`.
fn archive_of(records: &[u8]) -> Vec<u8> {
    let (uid, gid) = unsafe { (libc::getuid(), libc::getgid()) };
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
            body: b"data\n",
            uid,
            gid,
            ..Default::default()
        }
        .member(),
    );
    archive.extend(ustar_trailer());
    archive
}

/// Extract `archive` with `-r -p e` into a new scratch directory, `None` where it takes no
/// `user.` attributes.
fn extract_p_e(archive: &[u8]) -> Option<(TempDir, Output)> {
    let temp = tempdir().unwrap();
    if !set_xattr(temp.path(), b"user.probe", b"") {
        return None;
    }
    fs::write(temp.path().join("a.pax"), archive).unwrap();
    let out = run_pax_in_dir(&["-r", "-p", "e", "-f", "a.pax"], temp.path());
    Some((temp, out))
}

/// libarchive's records: the name %-encoded, the value base64 without padding. Where it
/// writes a `SCHILY.xattr` record beside one, with the name %-encoded too, that one is the
/// same attribute and is not set a second time under the encoded name.
#[test]
fn pax_r_p_e_reads_libarchive_xattrs() {
    let records = [
        pax_record("LIBARCHIVE.xattr.user.x%20y%3D", b"aGVsbG8"),
        pax_record("SCHILY.xattr.user.x%20y%3D", b"hello"),
        pax_record("LIBARCHIVE.xattr.user.plain", b"AAEK/w=="),
        pax_record("LIBARCHIVE.xattr.user.empty", b""),
    ]
    .concat();
    let Some((temp, out)) = extract_p_e(&archive_of(&records)) else {
        return;
    };
    assert_ok(&out, "pax -r -p e");
    assert_eq!(
        user_xattrs(&temp.path().join("f")),
        vec![
            (b"user.empty".to_vec(), b"".to_vec()),
            (b"user.plain".to_vec(), b"\x00\x01\n\xff".to_vec()),
            (b"user.x y=".to_vec(), b"hello".to_vec()),
        ]
    );
}

/// Attributes in a global `g` header are not applied, as neither GNU tar nor libarchive
/// applies them; a member's own are.
#[test]
fn pax_r_p_e_ignores_global_xattrs() {
    let global = pax_record("SCHILY.xattr.user.global", b"G");
    let mut archive = Ustar {
        name: b"GlobalHead",
        typeflag: b'g',
        body: &global,
        ..Default::default()
    }
    .member();
    archive.extend(archive_of(&pax_record("SCHILY.xattr.user.own", b"O")));
    let Some((temp, out)) = extract_p_e(&archive) else {
        return;
    };
    assert_ok(&out, "pax -r -p e");
    assert_eq!(
        user_xattrs(&temp.path().join("f")),
        vec![(b"user.own".to_vec(), b"O".to_vec())]
    );
}

/// A record that does not decode is the member's failure: it is extracted with none of its
/// attributes, the record named, and the run fails. Without `-p e` nothing reads it.
#[test]
fn pax_r_p_e_refuses_a_bad_xattr_record() {
    let cases: [(&str, &[u8]); 4] = [
        ("LIBARCHIVE.xattr.user.b64", b"aGV*sbG8"),
        ("LIBARCHIVE.xattr.user.pct%2", b"aGVsbG8"),
        ("LIBARCHIVE.xattr.user.nul%00", b"aGVsbG8"),
        ("SCHILY.xattr.", b"v"),
    ];
    for (keyword, value) in cases {
        let records = [
            pax_record("SCHILY.xattr.user.good", b"g"),
            pax_record(keyword, value),
        ]
        .concat();
        let archive = archive_of(&records);
        let Some((temp, out)) = extract_p_e(&archive) else {
            return;
        };
        assert_eq!(out.status.code(), Some(1), "{keyword}");
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(stderr.contains(keyword), "{keyword}: {stderr}");
        let f = temp.path().join("f");
        assert_eq!(fs::read(&f).unwrap(), b"data\n");
        assert_eq!(user_xattrs(&f), vec![], "{keyword}");

        fs::remove_file(&f).unwrap();
        let out = run_pax_in_dir(&["-r", "-p", "p", "-f", "a.pax"], temp.path());
        assert_ok(&out, "pax -r -p p");
        let out = run_pax_in_dir(&["-v", "-f", "a.pax"], temp.path());
        assert_ok(&out, "pax -v");
    }
}

/// A value over 64 KiB, the most Linux holds, is refused.
#[test]
fn pax_r_p_e_refuses_an_oversized_xattr() {
    let records = pax_record("SCHILY.xattr.user.big", &vec![b'v'; 65537]);
    let Some((temp, out)) = extract_p_e(&archive_of(&records)) else {
        return;
    };
    assert_eq!(out.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("SCHILY.xattr.user.big"), "{stderr}");
    assert_eq!(user_xattrs(&temp.path().join("f")), vec![]);
}

/// The archive's record of an attribute is listed as any other record is.
#[test]
fn pax_lists_an_xattr_record() {
    let temp = tempdir().unwrap();
    let records = pax_record("SCHILY.xattr.user.foo", b"bar");
    fs::write(temp.path().join("a.pax"), archive_of(&records)).unwrap();
    let out = run_pax_in_dir(
        &["-f", "a.pax", "-o", "listopt=%(SCHILY.xattr.user.foo)s %F"],
        temp.path(),
    );
    assert_ok(&out, "pax -o listopt");
    assert_eq!(String::from_utf8_lossy(&out.stdout), "bar f\n");
}

/// Only `user.` attributes are restored from an archive, as GNU tar 1.35 --xattrs -xpf
/// restores them: the rest are dropped without a word, as GNU drops them. One the file
/// refuses -- a `user.` one recorded for a symbolic link, which takes none (EPERM) -- is
/// warned of, naming the member, and the run still succeeds, as with GNU tar; it is never
/// set on what the link names.
#[cfg(target_os = "linux")]
#[test]
fn pax_r_p_e_restores_user_attributes_only_and_warns_as_gnu_tar() {
    let records = [
        pax_record("SCHILY.xattr.user.ok", b"1"),
        pax_record("SCHILY.xattr.nonamespace.x", b"2"),
        pax_record("SCHILY.xattr.trusted.x", b"3"),
    ]
    .concat();
    let mut archive = archive_of(&records);
    archive.truncate(archive.len() - ustar_trailer().len());
    let (uid, gid) = unsafe { (libc::getuid(), libc::getgid()) };
    let link_records = pax_record("SCHILY.xattr.user.onlink", b"L");
    archive.extend(
        Ustar {
            name: b"PaxHeaders/l",
            typeflag: b'x',
            body: &link_records,
            ..Default::default()
        }
        .member(),
    );
    archive.extend(
        Ustar {
            name: b"l",
            typeflag: b'2',
            linkname: b"f",
            uid,
            gid,
            ..Default::default()
        }
        .member(),
    );
    archive.extend(ustar_trailer());
    let Some((temp, out)) = extract_p_e(&archive) else {
        return;
    };
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "{stderr}");
    assert!(!stderr.contains("nonamespace"), "{stderr}");
    assert!(!stderr.contains("trusted.x"), "{stderr}");
    assert!(
        stderr.contains("pax: l: cannot set extended attribute user.onlink:"),
        "{stderr}"
    );
    assert_eq!(
        user_xattrs(&temp.path().join("f")),
        vec![(b"user.ok".to_vec(), b"1".to_vec())]
    );
}

/// The value of the attribute `name` of `path` (not followed), if it has one.
#[cfg(target_os = "linux")]
fn xattr_of(path: &Path, name: &str) -> Option<Vec<u8>> {
    let path = CString::new(path.as_os_str().as_bytes()).unwrap();
    let name = CString::new(name).unwrap();
    let mut value = vec![0u8; 65536];
    let n = unsafe {
        libc::lgetxattr(
            path.as_ptr(),
            name.as_ptr(),
            value.as_mut_ptr().cast(),
            value.len(),
        )
    };
    value.truncate(usize::try_from(n).ok()?);
    Some(value)
}

/// As root: `-p e` restores no file capability or `trusted.` attribute from an archive --
/// only its `user.` ones, as GNU tar does; copy mode copies a file's capability and
/// `trusted.` attributes, a link's to the link, as cp -a does. Skipped unless root, on a filesystem
/// that takes both.
#[cfg(target_os = "linux")]
#[test]
fn pax_p_e_as_root_takes_capabilities_from_files_not_archives() {
    if unsafe { libc::geteuid() } != 0 {
        eprintln!("note: not root; case skipped");
        return;
    }
    // Version 2, effective, CAP_NET_RAW permitted.
    let cap: Vec<u8> = [0x0200_0001u32, 1 << 13, 0, 0, 0]
        .iter()
        .flat_map(|w| w.to_le_bytes())
        .collect();
    let member = |name: &[u8], records: &[u8], typeflag: u8, linkname: &'static [u8]| {
        let mut out = Ustar {
            name: &[b"PaxHeaders/".as_slice(), name].concat(),
            typeflag: b'x',
            body: records,
            ..Default::default()
        }
        .member();
        out.extend(
            Ustar {
                name,
                typeflag,
                linkname,
                body: if typeflag == b'0' { b"data\n" } else { b"" },
                ..Default::default()
            }
            .member(),
        );
        out
    };
    let records = [
        pax_record("SCHILY.xattr.security.capability", &cap),
        pax_record("SCHILY.xattr.trusted.t", b"T"),
        pax_record("SCHILY.xattr.user.u", b"U"),
    ]
    .concat();
    let mut archive = member(b"f", &records, b'0', b"");
    let on_link = pax_record("SCHILY.xattr.trusted.onlink", b"L");
    archive.extend(member(b"l", &on_link, b'2', b"f"));
    archive.extend(ustar_trailer());
    let temp = tempdir().unwrap();
    fs::write(temp.path().join("a.pax"), &archive).unwrap();
    let x = temp.path().join("x");
    fs::create_dir(&x).unwrap();
    let out = run_pax_in_dir(&["-r", "-p", "e", "-f", "../a.pax"], &x);
    assert_ok(&out, "pax -r -p e");
    assert!(out.stderr.is_empty());
    assert_eq!(xattr_of(&x.join("f"), "user.u"), Some(b"U".to_vec()));
    assert_eq!(xattr_of(&x.join("f"), "security.capability"), None);
    assert_eq!(xattr_of(&x.join("f"), "trusted.t"), None);
    assert_eq!(xattr_of(&x.join("l"), "trusted.onlink"), None);

    // A file on disk with a capability and `trusted.` attributes, a link with one.
    let src = temp.path().join("src");
    fs::create_dir(&src).unwrap();
    fs::write(src.join("f"), "f\n").unwrap();
    std::os::unix::fs::symlink("f", src.join("l")).unwrap();
    if !set_xattr(&src.join("f"), b"security.capability", &cap) {
        return;
    }
    assert!(set_xattr(&src.join("f"), b"trusted.t", b"T"));
    assert!(set_xattr(&src.join("l"), b"trusted.onlink", b"L"));
    let y = temp.path().join("y");
    fs::create_dir(&y).unwrap();
    let out = run_pax_in_dir(&["-rw", "-p", "e", "f", "l", "../y"], &src);
    assert_ok(&out, "pax -rw -p e");
    assert_eq!(xattr_of(&y.join("f"), "security.capability"), Some(cap));
    assert_eq!(xattr_of(&y.join("f"), "trusted.t"), Some(b"T".to_vec()));
    assert_eq!(
        xattr_of(&y.join("l"), "trusted.onlink"),
        Some(b"L".to_vec())
    );
    assert_eq!(xattr_of(&y.join("f"), "trusted.onlink"), None);
}
