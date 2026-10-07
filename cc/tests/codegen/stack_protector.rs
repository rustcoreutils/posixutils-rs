//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-fstack-protector` at run time: an array overrun that reaches the canary
// ends the program in `__stack_chk_fail`, and a protected program that
// overruns nothing runs as it would without the option. Which functions get
// a canary is `test_asm/codegen_stack_protector.rs`.
//

use crate::common::{compile_and_capture_aarch64, create_c_file, run_c17};
use std::process::{Command, Output};

/// Compile `src` for the host with `flags` and run it.
fn run_host(name: &str, src: &str, flags: &[&str]) -> Output {
    let c = create_c_file(name, src);
    let dir = plib::tmp::Builder::new()
        .prefix(&format!("c17_ssp_{name}_"))
        .tempdir()
        .expect("tempdir");
    let exe = dir.path().join(name);
    let mut args = flags.to_vec();
    args.extend_from_slice(&["-o", exe.to_str().unwrap(), c.path().to_str().unwrap()]);
    let built = run_c17(&args);
    assert!(
        built.success,
        "c17 {flags:?} failed on {name}:\n{}",
        built.stderr
    );
    assert_eq!(built.stderr, "", "c17 {flags:?} warned on {name}");
    Command::new(&exe).output().expect("run test binary")
}

/// Did `out` end in `__stack_chk_fail`? glibc reports the smash on stderr
/// before it aborts; Darwin's libc aborts with a message of its own.
fn smashed(out: &Output) -> bool {
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        // SIGABRT is 6 on Linux and on Darwin.
        const SIGABRT: i32 = 6;
        let aborted = out.status.signal() == Some(SIGABRT);
        let stderr = String::from_utf8_lossy(&out.stderr);
        aborted && (cfg!(target_os = "macos") || stderr.contains("*** stack smashing detected ***"))
    }
    #[cfg(not(unix))]
    {
        !out.status.success()
    }
}

/// An overrun of `buf` by however many bytes `n` says, which only the run
/// knows: `n` is volatile, so no optimizer can see the store is out of
/// bounds, and `ATTR` is replaced by the attribute a case needs.
const OVERRUN: &str = r#"
#include <stdio.h>
volatile int n = NBYTES;
ATTR __attribute__((noinline)) int victim(void) {
    char buf[16];
    for (int i = 0; i < n; i++)
        buf[i] = 'A' + (i & 7);
    return buf[3];
}
int main(void) {
    int r = victim();
    printf("returned %d\n", r);
    return r == 'D' ? 0 : 1;
}
"#;

fn overrun(bytes: u32, attr: &str) -> String {
    OVERRUN
        .replace("NBYTES", &bytes.to_string())
        .replace("ATTR", attr)
}

/// Each level that protects a 16-byte `char` array stops an overrun of it
/// at the return, at every optimization level.
#[test]
fn stack_protector_stops_an_overrun() {
    let cases = [
        ("-fstack-protector", ""),
        ("-fstack-protector-strong", ""),
        ("-fstack-protector-all", ""),
        (
            "-fstack-protector-explicit",
            "__attribute__((stack_protect))",
        ),
    ];
    for (flag, attr) in cases {
        let src = overrun(64, attr);
        for opt in ["-O0", "-O1", "-O2"] {
            let out = run_host("ssp_smash", &src, &[flag, opt]);
            assert!(
                smashed(&out),
                "{flag} {opt}: the overrun was not caught: {:?}\n{}",
                out.status,
                String::from_utf8_lossy(&out.stderr)
            );
        }
    }
}

/// The same program writing inside the array runs normally under every
/// level, and without one.
#[test]
fn stack_protector_leaves_a_correct_program_alone() {
    let cases = [
        (&[][..], ""),
        (&["-fstack-protector"][..], ""),
        (&["-fstack-protector-strong"][..], ""),
        (&["-fstack-protector-all"][..], ""),
        (
            &["-fstack-protector-explicit"][..],
            "__attribute__((stack_protect))",
        ),
    ];
    for (flags, attr) in cases {
        let src = overrun(16, attr);
        for opt in ["-O0", "-O1", "-O2"] {
            let mut all = flags.to_vec();
            all.push(opt);
            let out = run_host("ssp_ok", &src, &all);
            assert!(out.status.success(), "{all:?}: {:?}", out.status);
            assert_eq!(String::from_utf8_lossy(&out.stdout), "returned 68\n");
        }
    }
}

/// `no_stack_protector` exempts a function even from `-all`: the overrun goes
/// unchecked. Only what the run does not do is asserted -- end in the smash
/// handler -- since an unchecked overrun may do anything else.
#[test]
fn stack_protector_attribute_exempts() {
    let src = overrun(24, "__attribute__((no_stack_protector))");
    let out = run_host("ssp_exempt", &src, &["-fstack-protector-all", "-O0"]);
    assert!(!smashed(&out), "no_stack_protector was ignored");
}

/// A frame whose locals need more than the stack's own alignment is
/// addressed from a second base register, and a VLA moves the stack pointer
/// at run time: the canary is found through neither, and both still catch
/// an overrun of an array in the fixed frame. (An overrun of the VLA itself
/// first crosses the fixed frame's own slots, the VLA's address among them,
/// and is no more reliably caught by gcc's canary than by c17's.)
const FRAMES: &str = r#"
#include <setjmp.h>
#include <stdio.h>
volatile int n = NBYTES;
volatile int len = 32;
__attribute__((noinline)) int aligned(void) {
    _Alignas(64) char buf[64];
    for (int i = 0; i < 64 + n * 8; i++)
        buf[i] = 1;
    return buf[0] + ((unsigned long)buf & 63);
}
__attribute__((noinline)) int vla(void) {
    char buf[len];
    char fixed[16];
    int r = buf[len - 1] = 2;
    for (int i = 0; i < 16 + n * 4; i++)
        fixed[i] = 3;
    return r + fixed[0];
}
jmp_buf env;
__attribute__((noinline)) int jumper(int k) {
    char buf[16];
    for (int i = 0; i < 16; i++)
        buf[i] = (char)k;
    if (setjmp(env) == 0)
        longjmp(env, 1);
    return buf[15];
}
int main(int argc, char **argv) {
    int which = argv[1][0] - '0';
    if (which == 0) return aligned() == 1 ? 0 : 1;
    if (which == 1) return vla() == 5 ? 0 : 2;
    return jumper(7) == 7 ? 0 : 3;
}
"#;

/// [`FRAMES`] in each of its three modes.
fn frames(name: &str, bytes: u32, flags: &[&str]) -> Vec<Output> {
    let c = create_c_file(name, &FRAMES.replace("NBYTES", &bytes.to_string()));
    let dir = plib::tmp::Builder::new()
        .prefix(&format!("c17_ssp_{name}_"))
        .tempdir()
        .expect("tempdir");
    let exe = dir.path().join(name);
    let mut args = flags.to_vec();
    args.extend_from_slice(&["-o", exe.to_str().unwrap(), c.path().to_str().unwrap()]);
    let built = run_c17(&args);
    assert!(built.success, "c17 {flags:?}:\n{}", built.stderr);
    ["0", "1", "2"]
        .iter()
        .map(|w| Command::new(&exe).arg(w).output().expect("run"))
        .collect()
}

#[test]
fn stack_protector_over_aligned_vla_and_setjmp_frames() {
    for opt in ["-O0", "-O2"] {
        let flags = ["-fstack-protector-strong", opt];
        // In bounds: the aligned array's 64 bytes, the VLA's 32, setjmp.
        let ok = frames("ssp_frames_ok", 0, &flags);
        for (i, out) in ok.iter().enumerate() {
            assert!(out.status.success(), "{opt} mode {i}: {:?}", out.status);
        }
        let bad = frames("ssp_frames_bad", 16, &flags);
        assert!(
            smashed(&bad[0]),
            "{opt}: over-aligned overrun: {:?}",
            bad[0].status
        );
        assert!(smashed(&bad[1]), "{opt}: VLA overrun: {:?}", bad[1].status);
    }
}

/// aarch64, under qemu: the guard comes from `__stack_chk_guard` there.
#[test]
fn stack_protector_aarch64() {
    for opt in ["-O0", "-O2"] {
        let flags = ["-fstack-protector-strong", opt];
        let Some(out) = compile_and_capture_aarch64("ssp_a64_ok", &overrun(16, ""), &flags, &[])
        else {
            return;
        };
        assert!(out.status.success(), "{opt}: {:?}", out.status);
        assert_eq!(String::from_utf8_lossy(&out.stdout), "returned 68\n");
        let out = compile_and_capture_aarch64("ssp_a64_bad", &overrun(64, ""), &flags, &[])
            .expect("checked above");
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(
            !out.status.success() && stderr.contains("*** stack smashing detected ***"),
            "{opt}: {:?}\n{stderr}",
            out.status
        );
    }
}
