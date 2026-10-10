//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! POSIX.1e ACLs carried in a pax archive: `-x pax` writes them as `SCHILY.acl.access` and
//! `SCHILY.acl.default` records, as star, bsdtar and GNU tar --acls do; `-p p` and `-p e`
//! restore them with the mode; copy mode copies them under the same options, as cp -p does.
//! Each case needs `setfacl`/`getfacl` and a filesystem that takes ACLs, and is skipped
//! without them.

use crate::common::{pax_record, run_pax_in_dir, system_tool, ustar_trailer, Ustar};
use plib::testing::mode_and_acl;
use plib::tmp::{tempdir, TempDir};
use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::process::{Command, Stdio};

/// `setfacl args path`; `false`, with a note, when `setfacl` is missing or the filesystem
/// takes no ACLs.
fn setfacl(args: &[&str], path: &Path) -> bool {
    let set = Command::new("setfacl")
        .args(args)
        .arg(path)
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|status| status.success());
    if !set {
        eprintln!("note: setfacl is missing or this filesystem takes no ACLs; case skipped");
    }
    set
}

/// A scratch directory with `src` holding `f` (0640, an ACL granting uid 65534 `r--` and
/// group 65534 `rw-`) and `d` (0755, an access ACL naming uid 65534 and a default ACL), and an
/// empty `x`; `None` where no ACL can be set.
fn tree_with_acls() -> Option<TempDir> {
    let temp = tempdir().unwrap();
    let src = temp.path().join("src");
    fs::create_dir_all(src.join("d")).unwrap();
    fs::create_dir(temp.path().join("x")).unwrap();
    fs::write(src.join("f"), "f\n").unwrap();
    fs::set_permissions(src.join("f"), fs::Permissions::from_mode(0o640)).unwrap();
    fs::set_permissions(src.join("d"), fs::Permissions::from_mode(0o755)).unwrap();
    let set = setfacl(&["-m", "u:65534:r--,g:65534:rw-"], &src.join("f"))
        && setfacl(&["-m", "u:65534:r-x"], &src.join("d"))
        && setfacl(&["-d", "-m", "u:65534:rwx,g::r-x,o::---"], &src.join("d"));
    set.then_some(temp)
}

fn assert_ok(out: &std::process::Output, what: &str) {
    assert_eq!(
        out.status.code(),
        Some(0),
        "{what}: {}",
        String::from_utf8_lossy(&out.stderr)
    );
}

/// Each of `f` and `d` under `x` has the mode and ACLs it has under `src`.
fn assert_same_acls(temp: &Path, what: &str) {
    for name in ["f", "d"] {
        assert_eq!(
            mode_and_acl(&temp.join("x").join(name)),
            mode_and_acl(&temp.join("src").join(name)),
            "{what}: {name}"
        );
    }
}

/// GNU tar, where the system has it: `tar --acls` means something else to other tars.
fn gnu_tar() -> Option<std::path::PathBuf> {
    let tar = system_tool("tar")?;
    let version = Command::new(&tar).arg("--version").output().ok()?;
    let gnu = String::from_utf8_lossy(&version.stdout).contains("GNU tar");
    if !gnu {
        eprintln!("skipping cross-tool check: tar is not GNU tar");
    }
    gnu.then_some(tar)
}

#[test]
fn pax_round_trips_access_and_default_acls() {
    let Some(temp) = tree_with_acls() else {
        return;
    };
    let src = temp.path().join("src");
    let out = run_pax_in_dir(&["-w", "-x", "pax", "-f", "../a.pax", "f", "d"], &src);
    assert_ok(&out, "pax -w -x pax");
    let archive = fs::read(temp.path().join("a.pax")).unwrap();
    let text = String::from_utf8_lossy(&archive);
    assert!(text.contains("SCHILY.acl.access=user::rw-,user:"), "{text}");
    assert!(
        text.contains("SCHILY.acl.default=user::rwx,user:"),
        "{text}"
    );
    for p in ["p", "e"] {
        let x = temp.path().join("x");
        fs::remove_dir_all(&x).unwrap();
        fs::create_dir(&x).unwrap();
        let out = run_pax_in_dir(&["-r", "-p", p, "-f", "../a.pax"], &x);
        assert_ok(&out, "pax -r");
        assert_same_acls(temp.path(), &format!("pax -r -p {p}"));
    }
}

#[test]
fn pax_rw_p_copies_acls() {
    let Some(temp) = tree_with_acls() else {
        return;
    };
    let out = run_pax_in_dir(
        &["-rw", "-p", "e", "f", "d", "../x"],
        &temp.path().join("src"),
    );
    assert_ok(&out, "pax -rw -p e");
    assert_same_acls(temp.path(), "pax -rw -p e");
}

/// Without -p p the archived ACLs are not applied: the member is made by the normal
/// file-creation action, and here, with no default ACL above it, has none.
#[test]
fn pax_without_p_applies_no_acl() {
    let Some(temp) = tree_with_acls() else {
        return;
    };
    let src = temp.path().join("src");
    let out = run_pax_in_dir(&["-w", "-x", "pax", "-f", "../a.pax", "f", "d"], &src);
    assert_ok(&out, "pax -w -x pax");
    let x = temp.path().join("x");
    assert_ok(&run_pax_in_dir(&["-r", "-f", "../a.pax"], &x), "pax -r");
    fs::create_dir(x.join("c")).unwrap();
    assert_ok(
        &run_pax_in_dir(&["-rw", "f", "d", "../x/c"], &src),
        "pax -rw",
    );
    for name in ["f", "d", "c/f", "c/d"] {
        let acl = mode_and_acl(&x.join(name));
        assert!(
            !acl.contains("65534") && !acl.contains("mask"),
            "{name}: {acl}"
        );
    }
}

/// ustar and cpio have nowhere to put an ACL: nothing is written, and nothing said.
#[test]
fn pax_writes_no_acl_in_ustar_or_cpio() {
    let Some(temp) = tree_with_acls() else {
        return;
    };
    let src = temp.path().join("src");
    for format in ["ustar", "cpio"] {
        let out = run_pax_in_dir(&["-w", "-x", format, "-f", "../a", "f", "d"], &src);
        assert_ok(&out, format);
        assert!(out.stderr.is_empty(), "{format}: {:?}", out.stderr);
        let archive = fs::read(temp.path().join("a")).unwrap();
        assert!(
            !archive.windows(10).any(|w| w == b"SCHILY.acl"),
            "{format} holds an ACL record"
        );
    }
}

/// `-o listopt=%(SCHILY.acl.access)s` still reports the record pax now reads.
#[test]
fn pax_lists_the_acl_records() {
    let Some(temp) = tree_with_acls() else {
        return;
    };
    let src = temp.path().join("src");
    let out = run_pax_in_dir(&["-w", "-x", "pax", "-f", "../a.pax", "f"], &src);
    assert_ok(&out, "pax -w -x pax");
    let out = run_pax_in_dir(
        &["-f", "a.pax", "-o", "listopt=%(SCHILY.acl.access)s"],
        temp.path(),
    );
    assert_ok(&out, "pax listopt");
    let listed = String::from_utf8_lossy(&out.stdout);
    assert!(
        listed.starts_with("user::rw-,user:") && listed.contains(",mask::rw-,other::---"),
        "{listed}"
    );
}

/// A member whose ACL text cannot be read is still extracted, with a mode granting nobody but
/// its owner anything -- the ACL might have narrowed what the mode shows -- and the run fails.
#[test]
fn pax_r_p_narrows_the_mode_on_a_bad_acl() {
    let temp = tempdir().unwrap();
    let records = pax_record("SCHILY.acl.access", b"user::rwx,bogus,other::r--");
    let mut archive = Ustar {
        name: b"PaxHeaders/f",
        typeflag: b'x',
        body: &records,
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
    fs::write(temp.path().join("a.pax"), &archive).unwrap();
    let out = run_pax_in_dir(&["-r", "-p", "p", "-f", "a.pax"], temp.path());
    assert_eq!(out.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("SCHILY.acl.access"), "{stderr}");
    let f = temp.path().join("f");
    assert_eq!(fs::read(&f).unwrap(), b"data\n");
    let mode = fs::metadata(&f).unwrap().permissions().mode() & 0o7777;
    assert_eq!(mode, 0o700);

    // Without -p p the record is not used, and nothing is wrong.
    fs::remove_file(&f).unwrap();
    let out = run_pax_in_dir(&["-r", "-f", "a.pax"], temp.path());
    assert_ok(&out, "pax -r");
}

/// An ACL record on a member that takes none -- a symbolic link -- is ignored.
#[test]
fn pax_r_p_ignores_an_acl_on_a_symlink() {
    let records = pax_record(
        "SCHILY.acl.access",
        b"user::rwx,user:65534:rwx,group::r-x,mask::rwx,other::---",
    );
    let mut archive = Ustar {
        name: b"PaxHeaders/l",
        typeflag: b'x',
        body: &records,
        ..Default::default()
    }
    .member();
    archive.extend(
        Ustar {
            name: b"l",
            typeflag: b'2',
            linkname: b"target",
            ..Default::default()
        }
        .member(),
    );
    archive.extend(ustar_trailer());
    let temp = tempdir().unwrap();
    fs::write(temp.path().join("a.pax"), &archive).unwrap();
    let out = run_pax_in_dir(&["-r", "-p", "p", "-f", "a.pax"], temp.path());
    assert_ok(&out, "pax -r -p p");
}

#[test]
fn gnu_tar_extracts_pax_acls() {
    let Some(tar) = gnu_tar() else {
        return;
    };
    let Some(temp) = tree_with_acls() else {
        return;
    };
    let src = temp.path().join("src");
    let out = run_pax_in_dir(&["-w", "-x", "pax", "-f", "../a.pax", "f", "d"], &src);
    assert_ok(&out, "pax -w -x pax");
    let out = Command::new(tar)
        .args(["--acls", "-xpf", "../a.pax"])
        .current_dir(temp.path().join("x"))
        .output()
        .unwrap();
    assert_ok(&out, "tar --acls -xpf");
    assert_same_acls(temp.path(), "tar --acls -xpf of pax -w");
}

#[test]
fn pax_extracts_gnu_tar_acls() {
    let Some(tar) = gnu_tar() else {
        return;
    };
    let Some(temp) = tree_with_acls() else {
        return;
    };
    let out = Command::new(tar)
        .args(["--acls", "--format=posix", "-cf", "../a.pax", "f", "d"])
        .current_dir(temp.path().join("src"))
        .output()
        .unwrap();
    assert_ok(&out, "tar --acls -cf");
    let out = run_pax_in_dir(&["-r", "-p", "e", "-f", "../a.pax"], &temp.path().join("x"));
    assert_ok(&out, "pax -r -p e");
    assert_same_acls(temp.path(), "pax -r -p e of tar --acls -cf");
}

/// Without `/proc` a FIFO's ACLs cannot be read -- only an `O_PATH` pin of it can be
/// had, read through `/proc/self/fd/N` -- so it is taken to have none, as on a filesystem
/// that holds none: archived and copied without a word, with its mode.
#[cfg(all(
    target_os = "linux",
    any(target_arch = "x86_64", target_arch = "aarch64")
))]
#[test]
fn pax_archives_and_copies_fifos_without_procfs() {
    use plib::testing::seccomp::refuse_path_xattr_reads;
    let temp = tempdir().unwrap();
    let src = temp.path().join("src");
    fs::create_dir_all(&src).unwrap();
    fs::create_dir(temp.path().join("x")).unwrap();
    let fifo = std::ffi::CString::new(src.join("p").into_os_string().into_encoded_bytes()).unwrap();
    assert_eq!(unsafe { libc::mkfifo(fifo.as_ptr(), 0o640) }, 0);
    fs::set_permissions(src.join("p"), fs::Permissions::from_mode(0o640)).unwrap();
    let pax = |args: &[&str]| {
        let mut command = Command::new(env!("CARGO_BIN_EXE_pax"));
        command.args(args).current_dir(&src).stdin(Stdio::null());
        refuse_path_xattr_reads(&mut command);
        let out = command.output().unwrap();
        (
            out.status.code(),
            String::from_utf8_lossy(&out.stderr).into_owned(),
        )
    };
    assert_eq!(
        pax(&["-w", "-x", "pax", "-f", "../a.pax", "p"]),
        (Some(0), String::new())
    );
    assert_eq!(
        pax(&["-rw", "-p", "p", "p", "../x"]),
        (Some(0), String::new())
    );
    let mode = fs::symlink_metadata(temp.path().join("x/p"))
        .unwrap()
        .permissions()
        .mode();
    assert_eq!(mode & 0o7777, 0o640);
}

/// An archived ACL wider than the archived mode: the mode is what the member ends with --
/// the ACL's owner, mask and other entries follow it -- and its named entries are capped by
/// that mask.
#[test]
fn pax_r_p_gives_the_mode_over_a_wider_acl() {
    let temp = tempdir().unwrap();
    let probe = temp.path().join("probe");
    fs::write(&probe, "").unwrap();
    if !setfacl(&["-m", "u:65534:r--"], &probe) {
        return;
    }
    let records = pax_record(
        "SCHILY.acl.access",
        b"user::rwx,user:65534:rwx,group::rwx,mask::rwx,other::rwx",
    );
    let mut archive = Ustar {
        name: b"PaxHeaders/f",
        typeflag: b'x',
        body: &records,
        ..Default::default()
    }
    .member();
    archive.extend(
        Ustar {
            name: b"f",
            mode: 0o750,
            body: b"data\n",
            ..Default::default()
        }
        .member(),
    );
    archive.extend(ustar_trailer());
    fs::write(temp.path().join("a.pax"), &archive).unwrap();
    let x = temp.path().join("x");
    fs::create_dir(&x).unwrap();
    let out = run_pax_in_dir(&["-r", "-p", "p", "-f", "../a.pax"], &x);
    assert_ok(&out, "pax -r -p p");
    assert_eq!(
        mode_and_acl(&x.join("f")),
        "750 user::rwx user:65534:rwx group::rwx mask::r-x other::---"
    );
}
