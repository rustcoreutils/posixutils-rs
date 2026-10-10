//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `cp -p` on a kernel older than Linux 5.8, whose `utimensat` refuses
//! `AT_EMPTY_PATH` with EINVAL, and on a system without `/proc`. Each is
//! imitated by a seccomp filter that answers exactly the calls concerned so.

use plib::testing::seccomp::{
    install_filter, op, refuse_path_xattr_reads, JEQ, JSET, LD, RET, RET_ALLOW, RET_ERRNO,
};
use std::fs;
use std::os::unix::fs::MetadataExt;
use std::process::{Command, Stdio};

/// `utimensat` on x86_64 and aarch64.
#[cfg(target_arch = "x86_64")]
const SYS_UTIMENSAT: u32 = 280;
#[cfg(target_arch = "aarch64")]
const SYS_UTIMENSAT: u32 = 88;

/// Make `command`'s process answer `utimensat` with EINVAL when its flags
/// (the fourth argument) carry `AT_EMPTY_PATH`, as Linux before 5.8 does.
/// Every other call is allowed.
fn refuse_utimensat_empty_path(command: &mut Command) {
    let einval = RET_ERRNO | u32::try_from(libc::EINVAL).unwrap();
    let empty_path = u32::try_from(libc::AT_EMPTY_PATH).unwrap();
    install_filter(
        command,
        vec![
            op(LD, 0, 0, 0),
            op(JEQ, 0, 3, SYS_UTIMENSAT),
            op(LD, 0, 0, 16 + 8 * 3),
            op(JSET, 0, 1, empty_path),
            op(RET, 0, 0, einval),
            op(RET, 0, 0, RET_ALLOW),
        ],
    );
}

/// `cp -pR` of a symbolic link and a FIFO still preserves their times there.
#[test]
fn test_cp_p_preserves_node_times_without_utimensat_empty_path() {
    let test_dir = &format!(
        "{}/test_cp_p_preserves_node_times_without_utimensat_empty_path",
        env!("CARGO_TARGET_TMPDIR")
    );
    let _ = fs::remove_dir_all(test_dir);
    let src = format!("{test_dir}/src");
    fs::create_dir_all(&src).unwrap();
    std::os::unix::fs::symlink("anywhere", format!("{src}/link")).unwrap();
    super::mkfifo_at(&format!("{src}/fifo"), 0o640);
    let when = libc::timespec {
        tv_sec: 1_000_000_000,
        tv_nsec: 0,
    };
    for name in ["link", "fifo"] {
        let path = std::ffi::CString::new(format!("{src}/{name}")).unwrap();
        let (cwd, nofollow) = (libc::AT_FDCWD, libc::AT_SYMLINK_NOFOLLOW);
        let r = unsafe { libc::utimensat(cwd, path.as_ptr(), [when, when].as_ptr(), nofollow) };
        assert_eq!(r, 0);
    }

    let dst = format!("{test_dir}/dst");
    let mut command = Command::new(env!("CARGO_BIN_EXE_cp"));
    command.args(["-pR", &src, &dst]).stdin(Stdio::null());
    refuse_utimensat_empty_path(&mut command);
    let out = command.output().unwrap();
    assert_eq!(
        out.status.code(),
        Some(0),
        "stderr: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    for name in ["link", "fifo"] {
        let md = fs::symlink_metadata(format!("{dst}/{name}")).unwrap();
        assert_eq!(md.mtime(), 1_000_000_000, "{name}");
    }

    fs::remove_dir_all(test_dir).unwrap();
}

/// `cp -pR` of directories reads their ACLs through descriptors of their own,
/// never through `/proc`: without it a tree with no ACL at all copies without
/// a word, and one with an ACL keeps it.
#[test]
fn test_cp_p_copies_directory_acls_without_procfs() {
    let test_dir = &format!(
        "{}/test_cp_p_copies_directory_acls_without_procfs",
        env!("CARGO_TARGET_TMPDIR")
    );
    let _ = fs::remove_dir_all(test_dir);
    let src = format!("{test_dir}/src");
    fs::create_dir_all(format!("{src}/sub")).unwrap();
    fs::write(format!("{src}/sub/f"), "f").unwrap();

    let copy = |dst: &str| {
        let mut command = Command::new(env!("CARGO_BIN_EXE_cp"));
        command.args(["-pR", &src, dst]).stdin(Stdio::null());
        refuse_path_xattr_reads(&mut command);
        command.output().unwrap()
    };
    let out = copy(&format!("{test_dir}/plain"));
    assert_eq!(
        (
            out.status.code(),
            String::from_utf8_lossy(&out.stderr).as_ref()
        ),
        (Some(0), "")
    );

    if !plib::testing::grant_named_acl(std::path::Path::new(&format!("{src}/sub"))) {
        fs::remove_dir_all(test_dir).unwrap();
        return;
    }
    let out = copy(&format!("{test_dir}/acl"));
    assert_eq!(
        (
            out.status.code(),
            String::from_utf8_lossy(&out.stderr).as_ref()
        ),
        (Some(0), "")
    );
    let read = |p: String| plib::acl::read_path(std::path::Path::new(&p), true).unwrap();
    assert_eq!(
        read(format!("{test_dir}/acl/sub")),
        read(format!("{src}/sub"))
    );

    fs::remove_dir_all(test_dir).unwrap();
}

/// `cp -pR` of a FIFO without `/proc`: its ACLs can be read only through
/// `/proc/self/fd/N` of an `O_PATH` pin, so it is taken to have none -- as on a
/// filesystem that holds none -- and it is copied without a word, with its
/// mode.
#[test]
fn test_cp_p_copies_special_files_without_procfs() {
    use std::os::unix::fs::PermissionsExt;
    let test_dir = &format!(
        "{}/test_cp_p_copies_special_files_without_procfs",
        env!("CARGO_TARGET_TMPDIR")
    );
    let _ = fs::remove_dir_all(test_dir);
    let src = format!("{test_dir}/src");
    fs::create_dir_all(&src).unwrap();
    super::mkfifo_at(&format!("{src}/fifo"), 0o640);
    fs::set_permissions(format!("{src}/fifo"), fs::Permissions::from_mode(0o640)).unwrap();

    let dst = format!("{test_dir}/dst");
    let mut command = Command::new(env!("CARGO_BIN_EXE_cp"));
    command.args(["-pR", &src, &dst]).stdin(Stdio::null());
    refuse_path_xattr_reads(&mut command);
    let out = command.output().unwrap();
    assert_eq!(
        (
            out.status.code(),
            String::from_utf8_lossy(&out.stderr).as_ref()
        ),
        (Some(0), "")
    );
    let md = fs::symlink_metadata(format!("{dst}/fifo")).unwrap();
    assert_eq!(md.mode() & 0o7777, 0o640);

    fs::remove_dir_all(test_dir).unwrap();
}
