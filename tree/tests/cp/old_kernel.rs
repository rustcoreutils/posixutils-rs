//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//

//! `cp -p` on a kernel older than Linux 5.8, whose `utimensat` refuses
//! `AT_EMPTY_PATH` with EINVAL. The kernel is imitated by a seccomp filter
//! that answers exactly those calls so.

use std::fs;
use std::io;
use std::os::unix::fs::MetadataExt;
use std::process::{Command, Stdio};

/// `utimensat` on x86_64 and aarch64.
#[cfg(target_arch = "x86_64")]
const SYS_UTIMENSAT: u32 = 280;
#[cfg(target_arch = "aarch64")]
const SYS_UTIMENSAT: u32 = 88;

/// Make `command`'s process answer `utimensat` with EINVAL when its flags
/// (the fourth argument) carry `AT_EMPTY_PATH`, as Linux before 5.8 does,
/// through a seccomp filter installed just before exec. Every other call is
/// allowed.
fn refuse_utimensat_empty_path(command: &mut Command) {
    use std::os::unix::process::CommandExt;

    #[repr(C)]
    struct SockFilter {
        code: u16,
        jt: u8,
        jf: u8,
        k: u32,
    }
    #[repr(C)]
    struct SockFprog {
        len: u16,
        filter: *const SockFilter,
    }
    // BPF_LD|BPF_W|BPF_ABS, BPF_JMP|BPF_JEQ|BPF_K, BPF_JMP|BPF_JSET|BPF_K,
    // BPF_RET|BPF_K. In the seccomp data the system call number is at offset
    // 0 and argument N at 16 + 8 * N, its low word first (little-endian).
    const LD: u16 = 0x20;
    const JEQ: u16 = 0x15;
    const JSET: u16 = 0x45;
    const RET: u16 = 0x06;
    const RET_ERRNO: u32 = 0x0005_0000;
    const RET_ALLOW: u32 = 0x7fff_0000;
    const PR_SET_NO_NEW_PRIVS: libc::c_int = 38;
    const PR_SET_SECCOMP: libc::c_int = 22;
    const SECCOMP_MODE_FILTER: libc::c_ulong = 2;
    let einval = RET_ERRNO | u32::try_from(libc::EINVAL).unwrap();
    let empty_path = u32::try_from(libc::AT_EMPTY_PATH).unwrap();
    let op = |code, jt, jf, k| SockFilter { code, jt, jf, k };

    unsafe {
        command.pre_exec(move || {
            let filter = [
                op(LD, 0, 0, 0),
                op(JEQ, 0, 3, SYS_UTIMENSAT),
                op(LD, 0, 0, 16 + 8 * 3),
                op(JSET, 0, 1, empty_path),
                op(RET, 0, 0, einval),
                op(RET, 0, 0, RET_ALLOW),
            ];
            let prog = SockFprog {
                len: filter.len() as u16,
                filter: filter.as_ptr(),
            };
            if libc::prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) != 0
                || libc::prctl(PR_SET_SECCOMP, SECCOMP_MODE_FILTER, &prog) != 0
            {
                return Err(io::Error::last_os_error());
            }
            Ok(())
        });
    }
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
