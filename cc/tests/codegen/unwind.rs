//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Unwind tables: the CFI rules that let `backtrace()`, a debugger, a profiler
// or the C++ runtime walk out of a c17 function. They are part of the default
// output, like gcc's `-fasynchronous-unwind-tables`, not something `-g` adds.
//

use crate::common::{compile_and_run_aarch64, create_c_file, run_c17};

/// Every frame shape the prologue has: callee-saved general and FP registers
/// live across a call, `alloca` and a VLA moving the stack pointer after the
/// prologue, a frame past what one `stp` pre-index can allocate (and past the
/// 4 KiB a single `sub` immediate holds), an over-aligned local, a variadic
/// function, and a mid-function return. Each calls on, so every one of them
/// is a frame the unwinder has to step through. Its CFI is walked
/// instruction by instruction in process by `cc/test_asm/codegen_unwind.rs`.
const FRAMES: &str = include_str!("unwind_frames.c");

/// The runnable program: [`FRAMES`] beneath a `backtrace()` call. Each of
/// the first eight return addresses has to lie in the function that made the
/// call -- the nearest function start below it -- and the walk has to reach at
/// least the C library frame that called main. A count alone is not enough: a
/// walk with wrong rules reads garbage and can count as deep as a right one.
const BACKTRACE_MAIN: &str = r#"
int main(void);

__attribute__((noinline, noclone)) int depth(void) {
    void *frames[64];
    void *fns[] = { (void *)depth, (void *)pressure, (void *)dyn, (void *)big,
                    (void *)mid, (void *)aligned, (void *)varargs, (void *)main };
    int n = backtrace(frames, 64);
    if (n < 9)
        return 100 + n;
    for (int i = 0; i < 8; i++) {
        char *pc = frames[i], *best = 0;
        int which = -1;
        for (int j = 0; j < 8; j++) {
            char *start = fns[j];
            if (start <= pc && (!best || start > best)) {
                best = start;
                which = j;
            }
        }
        if (which != i)
            return 10 + i;
    }
    return 0;
}

int main(void) {
    return varargs(0, 16) + sink - sink;
}
"#;

fn backtrace_program() -> String {
    format!("#include <execinfo.h>\n{FRAMES}{BACKTRACE_MAIN}")
}

/// Build `src` for the host with exactly `opts` -- no `-g`, which the shared
/// helpers always add and which is not what turns unwind tables on -- and run
/// it.
fn run_host_plain(name: &str, src: &str, opts: &[&str]) -> i32 {
    let c = create_c_file(name, src);
    let dir = plib::tmp::Builder::new()
        .prefix(&format!("c17_{name}_"))
        .tempdir()
        .expect("tempdir");
    let exe = dir.path().join("a.out");
    let mut args = opts.to_vec();
    let (exe_s, c_s) = (
        exe.to_string_lossy().into_owned(),
        c.path().to_string_lossy().into_owned(),
    );
    args.extend_from_slice(&["-o", &exe_s, &c_s]);
    let r = run_c17(&args);
    assert!(r.success, "c17 failed for {name} {opts:?}:\n{}", r.stderr);
    std::process::Command::new(&exe)
        .status()
        .expect("run")
        .code()
        .unwrap_or(-1)
}

/// `backtrace()` walks every frame, without `-g`.
///
/// The prologue's rules were emitted only under `-g`, so a plain build had
/// `.cfi_startproc` and nothing else: every function claimed its return
/// address was still at the entry stack pointer. On aarch64 the walk stopped
/// after one frame; on x86-64 it read past the frame and invented thirty.
#[test]
fn codegen_backtrace_walks_every_frame() {
    let src = backtrace_program();
    for opt in ["-O0", "-O2"] {
        assert_eq!(
            run_host_plain("backtrace", &src, &[opt]),
            0,
            "host backtrace at {opt}"
        );
        if let Some(code) = compile_and_run_aarch64("backtrace", &src, opt) {
            assert_eq!(code, 0, "aarch64 backtrace at {opt}");
        }
    }
}
