//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! cp -a and extended attributes: -a copies every attribute but the ACLs (which -p copies) and
//! the few libattr's `/etc/xattr.conf` names; -p and a plain copy copy none. Each failure to
//! copy one is silent and leaves the exit status alone, as GNU cp -a's are. Each case needs a
//! filesystem that takes `user.` attributes, and is skipped without one.

use plib::testing::{get_binary_path, set_xattr, xattrs};
use plib::tmp::{tempdir, Builder, TempDir};
use std::fs;
use std::os::unix::fs::MetadataExt;
use std::path::Path;
use std::process::{Command, Output, Stdio};

fn cp(cwd: &Path, args: &[&str]) -> Output {
    Command::new(get_binary_path("cp"))
        .args(args)
        .current_dir(cwd)
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute cp")
}

fn assert_ok(out: &Output) {
    assert_eq!(String::from_utf8_lossy(&out.stderr), "");
    assert_eq!(out.status.code(), Some(0));
}

fn pair(name: &str, value: &[u8]) -> (Vec<u8>, Vec<u8>) {
    (name.as_bytes().to_vec(), value.to_vec())
}

/// A scratch directory holding `d/f`, the directory with `user.foo=dir` and the file with
/// `user.foo=file`; `None` where the filesystem takes no `user.` attribute.
fn with_tree() -> Option<TempDir> {
    let temp = tempdir().unwrap();
    let d = temp.path().join("d");
    fs::create_dir(&d).unwrap();
    fs::write(d.join("f"), "f\n").unwrap();
    (set_xattr(&d, b"user.foo", b"dir") && set_xattr(&d.join("f"), b"user.foo", b"file"))
        .then_some(temp)
}

#[test]
fn cp_a_copies_the_xattrs_of_a_file_and_a_directory() {
    let Some(temp) = with_tree() else { return };
    assert_ok(&cp(temp.path(), &["-a", "d", "out"]));
    let out = temp.path().join("out");
    assert_eq!(xattrs(&out), [pair("user.foo", b"dir")]);
    assert_eq!(xattrs(&out.join("f")), [pair("user.foo", b"file")]);
}

/// A read-only file (0444) and directory (0555) take their attributes and keep their modes:
/// cp makes each at a mode of its own and gives it the source's last, as GNU cp -a does. An
/// existing read-only directory it copies into is not lent write permission: GNU cp -a leaves
/// its attributes as they are, without a word.
#[test]
fn cp_a_copies_the_xattrs_of_read_only_files_and_directories() {
    use std::os::unix::fs::PermissionsExt;
    let Some(temp) = with_tree() else { return };
    let d = temp.path().join("d");
    fs::set_permissions(d.join("f"), fs::Permissions::from_mode(0o444)).unwrap();
    fs::set_permissions(&d, fs::Permissions::from_mode(0o555)).unwrap();
    let mode = |path: &Path| fs::metadata(path).unwrap().mode() & 0o7777;
    assert_ok(&cp(temp.path(), &["-a", "d", "out"]));
    let out = temp.path().join("out");
    assert_eq!(xattrs(&out), [pair("user.foo", b"dir")]);
    assert_eq!(xattrs(&out.join("f")), [pair("user.foo", b"file")]);
    assert_eq!((mode(&out), mode(&out.join("f"))), (0o555, 0o444));

    let e = temp.path().join("e");
    fs::create_dir(&e).unwrap();
    assert!(set_xattr(&e, b"user.foo", b"empty"));
    fs::set_permissions(&e, fs::Permissions::from_mode(0o555)).unwrap();
    let found = temp.path().join("found");
    fs::create_dir_all(found.join("e")).unwrap();
    fs::set_permissions(found.join("e"), fs::Permissions::from_mode(0o555)).unwrap();
    let copied = cp(temp.path(), &["-a", "e", "found"]);
    let left = xattrs(&found.join("e"));
    for dir in [&d, &out, &e, &found.join("e")] {
        fs::set_permissions(dir, fs::Permissions::from_mode(0o755)).unwrap();
    }
    assert_ok(&copied);
    assert_eq!(left, []);
}

/// -p preserves mode, owner and times, -R alone nothing: neither copies an extended attribute.
#[test]
fn cp_p_and_plain_cp_copy_no_xattr() {
    let Some(temp) = with_tree() else { return };
    for (args, out) in [
        (&["-p", "d/f", "fp"][..], "fp"),
        (&["d/f", "fn"], "fn"),
        (&["-pR", "d", "dp"], "dp"),
        (&["-R", "d", "dr"], "dr"),
    ] {
        assert_ok(&cp(temp.path(), args));
        assert_eq!(xattrs(&temp.path().join(out)), [], "cp {args:?}");
    }
    assert_eq!(xattrs(&temp.path().join("dp/f")), []);
}

/// As GNU cp -a does, an attribute is added to an existing destination, file or directory, and
/// the destination's own others are kept.
#[test]
fn cp_a_onto_an_existing_destination_adds_to_its_xattrs() {
    let Some(temp) = with_tree() else { return };
    let e = temp.path().join("e");
    fs::create_dir_all(e.join("d")).unwrap();
    fs::write(e.join("d/f"), "old\n").unwrap();
    if !set_xattr(&e.join("d"), b"user.bar", b"old")
        || !set_xattr(&e.join("d/f"), b"user.bar", b"old")
    {
        return;
    }
    assert_ok(&cp(temp.path(), &["-a", "d", "e"]));
    assert_eq!(
        xattrs(&e.join("d")),
        [pair("user.bar", b"old"), pair("user.foo", b"dir")]
    );
    assert_eq!(
        xattrs(&e.join("d/f")),
        [pair("user.bar", b"old"), pair("user.foo", b"file")]
    );
}

/// The largest value Linux takes, 64 KiB, between two files in `/dev/shm` (tmpfs takes `user.`
/// attributes from Linux 6.6 on): it is read past cp's first buffer, whole.
#[test]
fn cp_a_copies_a_64k_value() {
    let Ok(temp) = Builder::new().tempdir_in("/dev/shm") else {
        eprintln!("note: no /dev/shm; test skipped");
        return;
    };
    let big: Vec<u8> = (0..65536u32).map(|i| (i % 251) as u8).collect();
    let f = temp.path().join("f");
    fs::write(&f, "f\n").unwrap();
    if !set_xattr(&f, b"user.big", &big) {
        return;
    }
    assert_ok(&cp(temp.path(), &["-a", "f", "g"]));
    assert_eq!(xattrs(&temp.path().join("g")), [pair("user.big", &big)]);
}

/// A name may hold any byte but NUL: control bytes, a byte that is no UTF-8, a space, a
/// newline; a value may be empty.
#[cfg(target_os = "linux")]
#[test]
fn cp_a_copies_names_with_odd_bytes() {
    let temp = tempdir().unwrap();
    let f = temp.path().join("f");
    fs::write(&f, "f\n").unwrap();
    let odd: [&[u8]; 3] = [b"user.\x01\xff ", b"user.a b\nc", b"user.empty"];
    for (i, name) in odd.iter().enumerate() {
        let value = if i == 2 { &b""[..] } else { &b"v"[..] };
        if !set_xattr(&f, name, value) {
            return;
        }
    }
    assert_ok(&cp(temp.path(), &["-a", "f", "g"]));
    assert_eq!(xattrs(&temp.path().join("g")), xattrs(&f));
    assert_eq!(xattrs(&f).len(), 3);
}

/// A destination whose filesystem takes no extended attribute: a message queue in
/// `/dev/mqueue`, made from an empty file. As with GNU cp -a, nothing is said and the exit
/// status is 0.
#[cfg(target_os = "linux")]
#[test]
fn cp_a_to_a_filesystem_without_xattrs() {
    let temp = tempdir().unwrap();
    let e = temp.path().join("e");
    fs::write(&e, "").unwrap();
    if !set_xattr(&e, b"user.foo", b"bar") {
        return;
    }
    let dest = format!("/dev/mqueue/posixutils-cp-xattr-{}", std::process::id());
    if fs::write(&dest, "").is_err() {
        eprintln!("note: no writable mqueue filesystem at /dev/mqueue; test skipped");
        return;
    }
    fs::remove_file(&dest).unwrap();
    let out = cp(temp.path(), &["-a", "e", &dest]);
    let _ = fs::remove_file(&dest);
    assert_ok(&out);
}

/// A value the destination cannot hold -- 64 KiB from `/dev/shm` onto a filesystem whose limit
/// is lower, ext4's block -- is not copied, and, as GNU cp -a says nothing of it, nothing is
/// said: the exit status is 0. Skipped where the scratch filesystem takes the value.
#[test]
fn cp_a_says_nothing_of_a_value_too_big_for_the_destination() {
    let Ok(shm) = Builder::new().tempdir_in("/dev/shm") else {
        eprintln!("note: no /dev/shm; test skipped");
        return;
    };
    let temp = tempdir().unwrap();
    let big = vec![b'x'; 65536];
    let probe = temp.path().join("probe");
    fs::write(&probe, "").unwrap();
    if fs::metadata(shm.path()).unwrap().dev() == fs::metadata(temp.path()).unwrap().dev() {
        eprintln!("note: /dev/shm is on the scratch filesystem; test skipped");
        return;
    }
    let f = shm.path().join("f");
    fs::write(&f, "f\n").unwrap();
    if !set_xattr(&f, b"user.big", &big) || !set_xattr(&f, b"user.small", b"s") {
        return;
    }
    if set_xattr(&probe, b"user.big", &big) {
        eprintln!("note: the scratch filesystem takes a 64 KiB value; test skipped");
        return;
    }
    let g = temp.path().join("g");
    assert_ok(&cp(temp.path(), &["-a", f.to_str().unwrap(), "g"]));
    assert_eq!(fs::read(&g).unwrap(), b"f\n");
    assert_eq!(xattrs(&g), [pair("user.small", b"s")]);
}

/// Only root can give a symbolic link or a FIFO an attribute (`trusted.`: Linux refuses
/// `user.` ones there). cp -a copies them, as GNU cp -a does -- a link's own, not its
/// referent's -- and a file capability survives the copy's change of owner.
#[cfg(target_os = "linux")]
#[test]
#[cfg_attr(
    not(all(feature = "posixutils_test_all", feature = "requires_root")),
    ignore
)]
fn cp_a_copies_the_xattrs_of_a_symlink_and_a_fifo_as_root() {
    if unsafe { libc::geteuid() } != 0 {
        eprintln!("note: requires root; test skipped");
        return;
    }
    let temp = tempdir().unwrap();
    let src = temp.path().join("src");
    fs::create_dir(&src).unwrap();
    fs::write(src.join("f"), "f\n").unwrap();
    std::os::unix::fs::symlink("f", src.join("sl")).unwrap();
    let fifo = std::ffi::CString::new(src.join("ff").to_str().unwrap()).unwrap();
    assert_eq!(unsafe { libc::mkfifo(fifo.as_ptr(), 0o644) }, 0);
    // cap_net_raw=ep as `setcap` writes it: VFS_CAP_REVISION_2 with the effective flag, then
    // permitted and inheritable sets of two 32-bit words each, all little-endian.
    let cap: [u8; 20] = [
        1, 0, 0, 2, 0, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ];
    if !set_xattr(&src.join("sl"), b"trusted.lnk", b"l")
        || !set_xattr(&src.join("ff"), b"trusted.ff", b"p")
        || !set_xattr(&src.join("f"), b"security.capability", &cap)
    {
        return;
    }
    assert_ok(&cp(temp.path(), &["-a", "src", "out"]));
    let out = temp.path().join("out");
    assert_eq!(xattrs(&out.join("sl")), [pair("trusted.lnk", b"l")]);
    assert_eq!(xattrs(&out.join("f")), [pair("security.capability", &cap)]);
    assert_eq!(xattrs(&out.join("ff")), [pair("trusted.ff", b"p")]);
}
