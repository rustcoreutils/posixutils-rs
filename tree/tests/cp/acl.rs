//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! cp -p and ACLs: wherever -p gives the copy the source's mode it gives it the source's ACLs
//! too, replacing whatever the destination had. The mode's group bits are an ACL's mask, so a
//! mode copied without its ACL turns the mask into the owning group's permission. Each case
//! needs `setfacl`/`getfacl` and a filesystem that takes ACLs, and is skipped without them.

use plib::testing::get_binary_path;
use plib::tmp::{tempdir, TempDir};
use std::fs;
use std::os::unix::fs::{MetadataExt, PermissionsExt};
use std::path::Path;
use std::process::{Command, Output, Stdio};

/// `setfacl args path`; `false`, with a note, where it is missing or the filesystem refuses.
fn setfacl(path: &Path, args: &[&str]) -> bool {
    let ok = Command::new("setfacl")
        .args(args)
        .arg(path)
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|s| s.success());
    if !ok {
        eprintln!("note: setfacl is missing or this filesystem takes no ACLs; case skipped");
    }
    ok
}

/// What `getfacl --omit-header` prints for `path`.
fn getfacl(path: &Path) -> String {
    let out = Command::new("getfacl")
        .args(["--omit-header", "--absolute-names"])
        .arg(path)
        .output()
        .expect("getfacl ran once setfacl did");
    assert!(out.status.success(), "getfacl {}", path.display());
    String::from_utf8(out.stdout).unwrap()
}

/// Whether `getfacl` shows more than the mode: a named entry, a mask or a default ACL.
fn has_acl(path: &Path) -> bool {
    getfacl(path)
        .lines()
        .any(|l| l.starts_with("mask::") || l.starts_with("default:"))
}

/// The mode field `ls -l` prints for `path`.
fn ls_mode(path: &Path) -> String {
    let out = Command::new(get_binary_path("ls"))
        .arg("-ld")
        .arg(path)
        .output()
        .unwrap();
    let text = String::from_utf8(out.stdout).unwrap();
    text.split_whitespace()
        .next()
        .unwrap_or_default()
        .to_string()
}

fn mode_of(path: &Path) -> u32 {
    fs::symlink_metadata(path).unwrap().mode() & 0o7777
}

fn set_mode(path: &Path, mode: u32) {
    fs::set_permissions(path, fs::Permissions::from_mode(mode)).unwrap();
}

/// Run cp with `args` in `cwd`.
fn cp(cwd: &Path, args: &[&str]) -> Output {
    Command::new(get_binary_path("cp"))
        .args(args)
        .current_dir(cwd)
        .stdin(Stdio::null())
        .output()
        .expect("failed to execute cp")
}

fn assert_ok(out: &Output) {
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    assert_eq!(stderr, "");
}

/// A scratch directory with the file `f` of mode 0644 holding `data`.
fn with_file() -> TempDir {
    let temp = tempdir().unwrap();
    let f = temp.path().join("f");
    fs::write(&f, "data\n").unwrap();
    set_mode(&f, 0o644);
    temp
}

/// The `acl` package's test/cp.test: cp copies an ACL only under -p. The ACL names `bin` with
/// rw-, which shows in the mode as group write permission (the mask); without -p the copy has
/// the mode less the umask and no ACL, with -p the very ACL, and `ls -l` marks it.
#[test]
fn cp_p_copies_the_acl_of_a_file() {
    let temp = with_file();
    let f = temp.path().join("f");
    if !setfacl(&f, &["-m", "u:bin:rw"]) {
        return;
    }
    assert_eq!(mode_of(&f), 0o664, "the mask shows in the group bits");
    assert!(ls_mode(&f).ends_with('+'), "{}", ls_mode(&f));

    assert_ok(&cp(temp.path(), &["f", "g"]));
    let g = temp.path().join("g");
    assert!(!has_acl(&g), "{}", getfacl(&g));
    assert!(!ls_mode(&g).ends_with('+'), "{}", ls_mode(&g));
    fs::remove_file(&g).unwrap();

    assert_ok(&cp(temp.path(), &["-p", "f", "g"]));
    assert_eq!(getfacl(&g), getfacl(&f));
    assert_eq!(mode_of(&g), mode_of(&f));
    assert_eq!(ls_mode(&g), "-rw-rw-r--+");
}

/// The rest of test/cp.test: `cp -rp` copies the ACL of a file in a directory. Without it the
/// mode's group bits -- the mask, rwx -- became the owning group's own permission: the copy
/// gave the group write and search permission the source withheld from it.
#[test]
fn cp_rp_copies_the_acls_of_a_tree() {
    let temp = tempdir().unwrap();
    let h = temp.path().join("h");
    fs::create_dir(&h).unwrap();
    set_mode(&h, 0o755);
    fs::write(h.join("x"), "blubb\n").unwrap();
    set_mode(&h.join("x"), 0o644);
    if !setfacl(&h, &["-R", "-m", "u:bin:rwx"]) {
        return;
    }
    assert_eq!(
        getfacl(&h.join("x")),
        "user::rw-\nuser:bin:rwx\ngroup::r--\nmask::rwx\nother::r--\n\n"
    );
    assert_ok(&cp(temp.path(), &["-rp", "h", "i"]));
    let i = temp.path().join("i");
    assert_eq!(fs::read_to_string(i.join("x")).unwrap(), "blubb\n");
    assert_eq!(getfacl(&i.join("x")), getfacl(&h.join("x")));
    assert_eq!(getfacl(&i), getfacl(&h));
    assert_eq!(mode_of(&i.join("x")), 0o674);
}

/// A directory's default ACL -- what its new entries inherit -- is copied too, alone or with
/// an access ACL.
#[test]
fn cp_rp_copies_a_default_acl() {
    let temp = tempdir().unwrap();
    for (name, args) in [
        ("only", &["-d", "-m", "u:65534:rx"][..]),
        ("both", &["-m", "u:65534:rwx,d:u:65534:r,d:g::rx"][..]),
    ] {
        let src = temp.path().join(name);
        fs::create_dir(&src).unwrap();
        if !setfacl(&src, args) {
            return;
        }
        let dest = format!("{name}.copy");
        assert_ok(&cp(temp.path(), &["-rp", name, &dest]));
        assert_eq!(getfacl(&temp.path().join(dest)), getfacl(&src), "{name}");
    }
}

/// -p replaces the ACL an existing destination file has: a source without one leaves it
/// without one. (GNU cp 9.4 keeps the destination's named entries, masked by the new mode --
/// granting them what the source's mode gives its group.)
#[test]
fn cp_p_removes_an_acl_the_source_lacks() {
    let temp = with_file();
    let g = temp.path().join("g");
    fs::write(&g, "old\n").unwrap();
    if !setfacl(&g, &["-m", "u:65534:rw"]) {
        return;
    }
    assert!(has_acl(&g));
    assert_ok(&cp(temp.path(), &["-p", "f", "g"]));
    assert!(!has_acl(&g), "{}", getfacl(&g));
    assert_eq!(mode_of(&g), 0o644);
    assert_eq!(fs::read_to_string(&g).unwrap(), "data\n");
}

/// Without -p no ACL is copied, and an existing destination's is left as it is, as GNU cp
/// leaves it.
#[test]
fn cp_without_p_copies_no_acl() {
    let temp = with_file();
    let f = temp.path().join("f");
    let g = temp.path().join("g");
    fs::write(&g, "old\n").unwrap();
    if !setfacl(&f, &["-m", "u:65534:r"]) || !setfacl(&g, &["-m", "u:65534:rw"]) {
        return;
    }
    let before = getfacl(&g);
    assert_ok(&cp(temp.path(), &["f", "h"]));
    assert!(!has_acl(&temp.path().join("h")));
    assert_ok(&cp(temp.path(), &["f", "g"]));
    assert_eq!(getfacl(&g), before);
}

/// A FIFO's ACL is copied with its mode under -a, as GNU cp copies it (on Linux, through the
/// descriptor that pins the node made).
#[cfg(target_os = "linux")]
#[test]
fn cp_a_copies_the_acl_of_a_fifo() {
    let temp = tempdir().unwrap();
    let p = temp.path().join("p");
    let c = std::ffi::CString::new(p.as_os_str().as_encoded_bytes()).unwrap();
    assert_eq!(unsafe { libc::mkfifo(c.as_ptr(), 0o644) }, 0);
    if !setfacl(&p, &["-m", "u:65534:r"]) {
        return;
    }
    assert_ok(&cp(temp.path(), &["-a", "p", "q"]));
    assert_eq!(getfacl(&temp.path().join("q")), getfacl(&p));
}

/// A directory found existing at the destination takes the source's ACL -- replacing its own --
/// exactly where it takes the source's mode: where nobody but the user could have created its
/// name. Elsewhere it keeps its ACL along with its mode, and that is diagnosed.
#[test]
fn cp_rp_gives_a_found_directory_the_acl_only_where_trusted() {
    for (dest_mode, trusted) in [(0o755, true), (0o777, false)] {
        let temp = tempdir().unwrap();
        let src = temp.path().join("src/d");
        fs::create_dir_all(&src).unwrap();
        set_mode(&src, 0o755);
        let found = temp.path().join("dest/d");
        fs::create_dir_all(&found).unwrap();
        set_mode(&found, 0o700);
        if !setfacl(&src, &["-m", "u:65534:rx"]) || !setfacl(&found, &["-m", "u:bin:rwx"]) {
            return;
        }
        let found_acl = getfacl(&found);
        set_mode(&temp.path().join("dest"), dest_mode);
        let out = cp(temp.path(), &["-pR", "src/d", "dest"]);
        let stderr = String::from_utf8_lossy(&out.stderr);
        if trusted {
            assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
            assert_eq!(getfacl(&found), getfacl(&src));
        } else {
            assert_eq!(out.status.code(), Some(1), "stderr: {stderr}");
            assert_eq!(getfacl(&found), found_acl);
        }
        set_mode(&temp.path().join("dest"), 0o755);

        // A source directory without an ACL removes the found one's where trusted.
        if trusted {
            Command::new("setfacl")
                .arg("-b")
                .arg(&src)
                .status()
                .unwrap();
            setfacl(&found, &["-m", "u:bin:rwx,d:u:bin:rwx"]);
            assert_ok(&cp(temp.path(), &["-pR", "src/d", "dest"]));
            assert!(!has_acl(&found), "{}", getfacl(&found));
        }
    }
}

/// A destination whose filesystem takes no ACL at all: a POSIX message queue made at a name
/// in `/dev/mqueue`, which an empty source copies to as an empty file. The ACL the source has
/// is lost, and that is an error, in GNU cp's words, with exit status 1 -- unless the source
/// has none, when the mode carries everything and nothing is said. Skipped without a
/// writable mqueue filesystem there.
#[cfg(target_os = "linux")]
#[test]
fn cp_p_to_a_filesystem_without_acls() {
    let temp = tempdir().unwrap();
    let e = temp.path().join("e");
    fs::write(&e, "").unwrap();
    let dest = format!("/dev/mqueue/posixutils-cp-acl-{}", std::process::id());
    if fs::write(&dest, "").is_err() {
        eprintln!("note: no writable mqueue filesystem at /dev/mqueue; test skipped");
        return;
    }
    fs::remove_file(&dest).unwrap();

    let out = cp(temp.path(), &["-p", "e", &dest]);
    let _ = fs::remove_file(&dest);
    assert_ok(&out);

    if !setfacl(&e, &["-m", "u:65534:r"]) {
        return;
    }
    let out = cp(temp.path(), &["-p", "e", &dest]);
    let _ = fs::remove_file(&dest);
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        format!("cp: preserving permissions for '{dest}': Operation not supported\n")
    );
    assert_eq!(out.status.code(), Some(1));

    // Without -p nothing of the ACL is asked for.
    let out = cp(temp.path(), &["e", &dest]);
    let _ = fs::remove_file(&dest);
    assert_ok(&out);
}

/// The directories `--parents` makes on the way take their source directories' ACLs under -p,
/// with their mode.
#[test]
fn cp_parents_p_copies_the_acls_of_the_directories_made() {
    let temp = tempdir().unwrap();
    let b = temp.path().join("a/b");
    fs::create_dir_all(&b).unwrap();
    fs::write(b.join("f"), "f\n").unwrap();
    fs::create_dir(temp.path().join("dst")).unwrap();
    if !setfacl(&b, &["-m", "u:65534:rwx,d:u:65534:rx"]) {
        return;
    }
    assert_ok(&cp(temp.path(), &["-p", "--parents", "a/b/f", "dst"]));
    assert_eq!(getfacl(&temp.path().join("dst/a/b")), getfacl(&b));
    assert_eq!(
        getfacl(&temp.path().join("dst/a")),
        getfacl(&temp.path().join("a"))
    );
}

/// An ACL that cannot be set must not leave the copy granting more than the source did. The
/// owning group has no permission, a named user rw- -- so the mode shows the mask, 0660; a
/// copy given that mode without the ACL hands the owning group rw-. Where the ACL is lost
/// (`/dev/mqueue`), the copy's group bits are the owning group's own: none.
#[cfg(target_os = "linux")]
#[test]
fn cp_p_losing_an_acl_does_not_widen_the_group() {
    let temp = tempdir().unwrap();
    let e = temp.path().join("e");
    fs::write(&e, "").unwrap();
    set_mode(&e, 0o600);
    let dest = format!("/dev/mqueue/posixutils-cp-widen-{}", std::process::id());
    if fs::write(&dest, "").is_err() {
        eprintln!("note: no writable mqueue filesystem at /dev/mqueue; test skipped");
        return;
    }
    fs::remove_file(&dest).unwrap();
    if !setfacl(&e, &["-m", "g::---,u:65534:rw,m::rw"]) {
        return;
    }
    assert_eq!(mode_of(&e), 0o660);
    let out = cp(temp.path(), &["-p", "e", &dest]);
    let mode = fs::metadata(&dest).map(|md| md.mode() & 0o7777);
    let _ = fs::remove_file(&dest);
    assert_eq!(out.status.code(), Some(1));
    assert_eq!(mode.unwrap(), 0o600);
}
