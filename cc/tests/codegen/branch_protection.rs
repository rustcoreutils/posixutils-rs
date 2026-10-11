//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// aarch64 `-mbranch-protection` programs, run under qemu, whose default CPU
// has both BTI and pointer authentication: a branch protected binary whose
// indirect branches land anywhere but on a landing pad dies with SIGILL,
// and a return address authenticated with the wrong signature faults.
//

use crate::common::{
    aarch64_cross_available, compile_and_run_aarch64_with, run_c17, work_dir, AARCH64_TARGET_ARGS,
};
use std::process::Command;

/// A whole program with no C library, so that every object in it is c17's
/// and carries the BTI note: only then does the linker mark the binary and
/// qemu guard its pages. Indirect calls to an external and to a static
/// function, a computed goto, a `__builtin_longjmp` back to its
/// `__builtin_setjmp`, and recursion through signed frames.
const FREESTANDING: &str = r#"
static void sys_exit(long code)
{
    register long x0 __asm__("x0") = code;
    register long x8 __asm__("x8") = 93;
    __asm__ volatile("svc 0" : : "r"(x0), "r"(x8) : "memory");
    for (;;)
        ;
}
static int twice(int x) { return 2 * x; }
int (*volatile fp)(int) = twice;
int ext_fn(int x) { return x + 1; }
int (*volatile ep)(int) = ext_fn;
__attribute__((noinline)) int pick(int i)
{
    static void *t[] = { &&a, &&b };
    goto *t[i];
a:  return 10;
b:  return 20;
}
void *buf[5];
static volatile int jumped;
__attribute__((noinline)) void jump(void) { __builtin_longjmp(buf, 1); }
int rec(int n) { return n ? n + rec(n - 1) : 0; }
#ifdef CONTROL
int bare(int x);
__asm__(".text\n.globl bare\n.type bare,%function\nbare:\n add w0, w0, 1\n ret\n"
        ".size bare,.-bare\n");
int (*volatile bp)(int) = bare;
#endif
#ifdef ATTR
__attribute__((target("branch-protection=bti+pac-ret+b-key")))
int bkeyed(int n) { return n ? n + bkeyed(n - 1) : 0; }
int (*volatile kp)(int) = bkeyed;
__attribute__((target("branch-protection=bti")))
static int bti_only(int x) { return x * 3; }
int (*volatile op)(int) = bti_only;
__attribute__((target("branch-protection=pac-ret")))
void *where(void) { return __builtin_return_address(0); }
#endif
#ifdef ATTR_NONE
__attribute__((target("branch-protection=none"))) int unpadded(int x) { return x + 1; }
int (*volatile up)(int) = unpadded;
#endif
__attribute__((noinline)) int run(void)
{
#ifdef ATTR
    if (kp(10) != 55) return 7;
    if (op(5) != 15) return 8;
    char *w = where();
    if (w < (char *)run || w > (char *)run + 4096) return 9;
#endif
#ifdef ATTR_NONE
    return up(1);
#endif
    if (fp(21) != 42) return 1;
    if (ep(1) != 2) return 2;
    if (pick(0) + pick(1) != 30) return 3;
    if (__builtin_setjmp(buf) == 0) {
        jumped = 1;
        jump();
        return 4;
    }
    if (!jumped) return 5;
    if (rec(10) != 55) return 6;
#ifdef CONTROL
    return bp(1);
#endif
    return 0;
}
void _start(void) { sys_exit(run()); }
"#;

/// Build [`FREESTANDING`] with `flags` and run it, returning how it ended:
/// its exit status, or the signal that killed it.
fn run_freestanding(name: &str, flags: &[&str]) -> (Option<i32>, Option<i32>) {
    use std::os::unix::process::ExitStatusExt;
    let dir = work_dir(name);
    let src = dir.path().join("p.c");
    let asm = dir.path().join("p.s");
    let exe = dir.path().join("p");
    std::fs::write(&src, FREESTANDING).unwrap();
    let (src, asm, exe) = (
        src.to_str().unwrap(),
        asm.to_str().unwrap(),
        exe.to_str().unwrap(),
    );
    let mut args = AARCH64_TARGET_ARGS.to_vec();
    args.extend_from_slice(flags);
    args.extend_from_slice(&["-S", "-o", asm, src]);
    let run = run_c17(&args);
    assert!(run.success, "{name} {flags:?}: {}", run.stderr);
    let link = Command::new("aarch64-linux-gnu-gcc")
        .args(["-nostdlib", "-static", "-o", exe, asm])
        .output()
        .expect("run the cross linker");
    assert!(
        link.status.success(),
        "{name}: {}",
        String::from_utf8_lossy(&link.stderr)
    );
    let notes = Command::new("readelf")
        .args(["-n", exe])
        .output()
        .expect("run readelf");
    let notes = String::from_utf8_lossy(&notes.stdout).into_owned();
    assert!(
        notes.contains("AArch64 feature: BTI, PAC"),
        "{name} {flags:?}:\n{notes}"
    );
    let status = Command::new("qemu-aarch64-static")
        .arg(exe)
        .status()
        .expect("run qemu-aarch64-static");
    (status.code(), status.signal())
}

/// Every landing pad c17 marks is where the program's indirect branches
/// arrive: the program runs under enforcement, at each level. The control --
/// the same program calling, through a pointer, an assembly function with no
/// landing pad -- dies with SIGILL, which proves the enforcement is there to
/// be passed.
#[test]
fn branch_protection_bti_enforced_program_runs() {
    if !aarch64_cross_available() {
        eprintln!("SKIP branch_protection_bti_enforced_program_runs: no aarch64 cross toolchain");
        return;
    }
    let control = run_freestanding(
        "bti_ctl",
        &["-O2", "-mbranch-protection=standard", "-DCONTROL"],
    );
    if control != (None, Some(4)) {
        eprintln!(
            "SKIP branch_protection_bti_enforced_program_runs: qemu enforces no BTI ({control:?})"
        );
        return;
    }
    for opt in ["-O0", "-O1", "-O2"] {
        for value in ["standard", "bti+pac-ret+b-key", "bti+pac-ret+leaf"] {
            let flag = format!("-mbranch-protection={value}");
            assert_eq!(
                run_freestanding("bti_run", &[opt, &flag]),
                (Some(0), None),
                "{opt} {flag}"
            );
        }
    }
}

/// `target("branch-protection=...")` builds one function as the option
/// would: functions signing with the B key, a static one with only `bti`
/// reached through a pointer, and the stripped return address of a signing
/// function all run under enforcement. The control -- a function whose
/// attribute takes the unit's protection away, called through a pointer --
/// dies with SIGILL, which shows the attribute is what decides.
#[test]
fn branch_protection_target_attribute_runs() {
    if !aarch64_cross_available() {
        eprintln!("SKIP branch_protection_target_attribute_runs: no aarch64 cross toolchain");
        return;
    }
    let control = run_freestanding(
        "bti_attr_ctl",
        &["-O2", "-mbranch-protection=standard", "-DATTR_NONE"],
    );
    if control == (Some(2), None) {
        panic!("the unpadded function was called without a fault: the attribute did nothing");
    }
    if control != (None, Some(4)) {
        eprintln!(
            "SKIP branch_protection_target_attribute_runs: qemu enforces no BTI ({control:?})"
        );
        return;
    }
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            run_freestanding("bti_attr", &[opt, "-mbranch-protection=standard", "-DATTR"]),
            (Some(0), None),
            "{opt}"
        );
    }
}

/// Signed return addresses against glibc: `setjmp`/`longjmp`,
/// `__builtin_return_address`, which hands back the address stripped of its
/// signature, and libgcc's unwinder walking out through every signed frame,
/// which it can only do if the CFI says where each frame signs and
/// authenticates.
#[test]
fn branch_protection_pac_ret_program_runs() {
    let src = r#"
#include <setjmp.h>
typedef int (*trace_fn)(void *ctx, void *arg);
extern int _Unwind_Backtrace(trace_fn, void *);
extern unsigned long _Unwind_GetIP(void *);
static jmp_buf jb;
static int frames, signed_ips;
__attribute__((noinline)) static void jump(void) { longjmp(jb, 7); }
__attribute__((noinline)) void *where(void) { return __builtin_return_address(0); }
static int count(void *ctx, void *arg)
{
    (void)arg;
    if (_Unwind_GetIP(ctx) >> 48)
        signed_ips++;
    frames++;
    return 0;
}
__attribute__((noinline)) int deep(int n)
{
    if (n == 0) {
        _Unwind_Backtrace(count, 0);
        return frames;
    }
    return deep(n - 1) + 0 * n;
}
int main(void)
{
    char *r = where();
    if (r < (char *)main || r > (char *)main + 4096) return 1;
    int x = setjmp(jb);
    if (x == 0) jump();
    if (x != 7) return 2;
    if (deep(5) < 7) return 3;
    if (signed_ips) return 4;
    return 0;
}
"#;
    for opt in ["-O0", "-O2"] {
        for value in ["standard", "pac-ret+b-key", "pac-ret+leaf+bti"] {
            let flag = format!("-mbranch-protection={value}");
            if let Some(status) =
                compile_and_run_aarch64_with("pac_run", src, &[opt, &flag, "-fexceptions"], &[])
            {
                assert_eq!(status, 0, "{opt} {flag}");
            }
        }
    }
}
