//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Stack frames past two gigabytes: displacements no 32-bit field holds,
// reached through a register on both targets.
//

use crate::common::{
    aarch64_cross_available, asm_for_at, compile_and_run, compile_and_run_aarch64_with,
};

/// A three-gigabyte frame, run: the array's far end written at a constant
/// displacement, a local declared after it and so past it -- also an inline
/// asm memory operand there -- a value kept across the calls, and the
/// variadic save area at the far end of the frame, read by `va_arg` -- under
/// the stack protector too, whose aarch64
/// layout moves the saved registers and a copy of the frame record to the
/// far end as well.
///
/// The frame is real but costs nothing: it runs on a thread whose stack is
/// an `MAP_NORESERVE` mapping, and only the few pages the program touches
/// are ever committed. No stack-clash protection, whose probes would touch
/// every page. If the mapping or the thread cannot be had, the program says
/// so (77..79) and the test is skipped rather than failed: that is the
/// host's limit, not the compiler's. c17 compiles it in a few megabytes.
///
/// The declarations every program here needs, and their sizes; the program
/// defines `thread_main`, which [`BIG_STACK_MAIN`] runs on the big stack.
const BIG_STACK: &str = r#"
#ifdef HAND_DECLARED
/* The aarch64 cross run has no sysroot headers: glibc's values. */
typedef unsigned long size_t;
typedef union { char size[64]; long align; } pthread_attr_t;
typedef unsigned long pthread_t;
int pthread_attr_init(pthread_attr_t *);
int pthread_attr_setstack(pthread_attr_t *, void *, size_t);
int pthread_create(pthread_t *, const pthread_attr_t *, void *(*)(void *), void *);
int pthread_join(pthread_t, void **);
void *mmap(void *, size_t, int, int, int, long);
#define PROT_READ 1
#define PROT_WRITE 2
#define MAP_PRIVATE 2
#define MAP_ANONYMOUS 0x20
#define MAP_NORESERVE 0x4000
#define MAP_FAILED ((void *)-1)
#else
#include <pthread.h>
#include <sys/mman.h>
#endif
#define ASIZE 0xC0000000UL
#define STACK (ASIZE + 0x1000000UL)
#define NI __attribute__((noinline))
static void *thread_main(void *arg);
"#;

/// `main`, running `thread_main` on a thread whose stack is the mapping.
const BIG_STACK_MAIN: &str = r#"
int main(void)
{
    char *base = mmap(0, STACK, PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0);
    if (base == MAP_FAILED)
        return 77;
    pthread_attr_t attr;
    pthread_t t;
    pthread_attr_init(&attr);
    if (pthread_attr_setstack(&attr, base, STACK))
        return 78;
    if (pthread_create(&t, &attr, thread_main, 0))
        return 79;
    pthread_join(t, 0);
    return (int)result;
}
"#;

/// The program `body`, between the declarations and `main`.
fn big_stack_program(body: &str) -> String {
    format!("{BIG_STACK}{body}{BIG_STACK_MAIN}")
}

const FAR_FRAME: &str = r#"
#include <stdarg.h>

#if defined(__x86_64__)
#define BUMP(x) __asm__ volatile("addq $1, %0" : "+m"(x))
#else
#define BUMP(x) __asm__ volatile("ldr x9, %0\n\tadd x9, x9, #1\n\tstr x9, %0" : "+m"(x) : : "x9")
#endif

volatile long sink;

NI int check(const char *s, long *tail)
{
    if (s[0] != 'a')
        return 1;
    if (s[ASIZE - 1] != 'b')
        return 2;
    if (*tail != 16)
        return 3;
    return 0;
}

NI long far_frame(int n, ...)
{
    char s[ASIZE];
    long tail = n * 3;
    long keep = n * 7;
    BUMP(tail);
    va_list ap;
    va_start(ap, n);
    long v = va_arg(ap, long);
    double d = va_arg(ap, double);
    va_end(ap);
    s[0] = 'a';
    s[ASIZE - 1] = 'b';
    int r = check(s, &tail);
    if (r)
        return r;
    sink = keep;
    if (v != 42 || d != 2.5)
        return 4;
    return keep == n * 7 ? 0 : 5;
}

static long result = -1;

static void *thread_main(void *arg)
{
    (void)arg;
    result = far_frame(5, 42L, 2.5);
    return 0;
}
"#;

/// The options each run is compiled with.
const RUNS: [&[&str]; 3] = [&["-O0"], &["-O2"], &["-O2", "-fstack-protector-all"]];

/// A run's exit status: 0, or one of the skips, which says so.
fn passed(name: &str, opts: &[&str], code: i32) -> bool {
    if (77..=79).contains(&code) {
        eprintln!("SKIP {name} {opts:?}: the host gave no 3 GiB thread stack ({code})");
        return true;
    }
    code == 0
}

#[test]
fn codegen_frame_past_two_gigabytes_runs() {
    for opts in RUNS {
        let args: Vec<String> = opts.iter().map(|s| s.to_string()).collect();
        let code = compile_and_run("far_frame", &big_stack_program(FAR_FRAME), &args);
        assert!(passed("far_frame", opts, code), "{opts:?}: exit {code}");
    }
}

#[test]
fn codegen_frame_past_two_gigabytes_runs_on_aarch64() {
    for opts in RUNS {
        let mut args = opts.to_vec();
        args.push("-DHAND_DECLARED");
        let src = big_stack_program(FAR_FRAME);
        if let Some(code) = compile_and_run_aarch64_with("a64_far_frame", &src, &args, &[]) {
            assert!(passed("a64_far_frame", opts, code), "{opts:?}: exit {code}");
        }
    }
}

/// Inline asm operands in a three-gigabyte frame that are values, not
/// objects: more are live across each statement than there are registers,
/// so a constraint that takes memory -- `"g"`, `"rm"` -- is handed the spill
/// slot, past two gigabytes; a register one is loaded from it, and a pinned
/// one (`"a"`) too. On x86-64 each such operand is written as an offset
/// from one address the far scratch holds, as no displacement field holds
/// the slot's; they used to be printed as the displacement itself, which
/// the assembler refuses. aarch64 gives these constraints registers, loaded
/// through its legalizer. Exit codes 1..=4 (77..79 skip).
const FAR_OPERANDS: &str = r#"
NI void touch(volatile char *s) { s[0] = 'a'; s[ASIZE - 1] = 'b'; }

#define V(i) long v##i = a * (i + 3) + b;
#define S(i) + v##i
#define ALL(M) M(0) M(1) M(2) M(3) M(4) M(5) M(6) M(7) M(8) M(9) M(10) M(11) \
    M(12) M(13) M(14) M(15) M(16) M(17)

NI long far_operands(long a, long b)
{
    volatile char big[ASIZE];
    touch(big);
    ALL(V)
    long r = 0, out = 0, cnt = 0;
#if defined(__x86_64__)
    __asm__("addq %1, %0\n\taddq %2, %0\n\taddq %3, %0\n\taddq %4, %0\n\t"
            "addq %5, %0\n\taddq %6, %0\n\taddq %7, %0\n\taddq %8, %0\n\t"
            "addq %9, %0\n\taddq %10, %0\n\taddq %11, %0\n\taddq %12, %0"
            : "+r"(r)
            : "g"(v0), "rm"(v1), "g"(v2), "rm"(v3), "g"(v4), "rm"(v5),
              "g"(v6), "rm"(v7), "g"(v8), "rm"(v9), "g"(v10), "rm"(v11));
    __asm__ volatile("addq $1, %0\n\taddq $2, %1\n\taddq $3, %2\n\taddq $4, %3"
                     : "+rm"(v12), "+g"(v13), "+rm"(v14), "+g"(v15));
    __asm__ volatile("movq %1, %0" : "=rm"(out) : "a"(v16));
    __asm__ volatile("movq %1, %%rcx\n\taddq %%rcx, %0" : "+d"(cnt) : "rm"(v17) : "rcx");
#else
    r = v0 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10 + v11;
    __asm__ volatile("add %0, %0, #1\n\tadd %1, %1, #2\n\tadd %2, %2, #3\n\tadd %3, %3, #4"
                     : "+r"(v12), "+r"(v13), "+r"(v14), "+r"(v15));
    __asm__ volatile("mov %0, %1" : "=r"(out) : "r"(v16));
    __asm__ volatile("add %0, %0, %1" : "+r"(cnt) : "r"(v17));
#endif
    if (big[0] != 'a' || big[ASIZE - 1] != 'b')
        return -1;
    long want = 0;
    for (long i = 0; i < 12; i++)
        want += a * (i + 3) + b;
    if (r != want)
        return -2;
    if (out != a * 19 + b || cnt != a * 20 + b)
        return -3;
    return r ALL(S) + out + cnt;
}

static long result = -1;

static void *thread_main(void *arg)
{
    (void)arg;
    long a = 3, b = 5, want = 0;
    for (long i = 0; i < 18; i++)
        want += a * (i + 3) + b;
    want += 1 + 2 + 3 + 4;
    for (long i = 0; i < 12; i++)
        want += a * (i + 3) + b;
    want += a * 19 + b + a * 20 + b;
    long got = far_operands(a, b);
    result = got < 0 ? -got : got == want ? 0 : 4;
    return 0;
}
"#;

#[test]
fn codegen_far_frame_asm_operands_run() {
    let src = big_stack_program(FAR_OPERANDS);
    for opts in RUNS {
        let args: Vec<String> = opts.iter().map(|s| s.to_string()).collect();
        let code = compile_and_run("far_operands", &src, &args);
        assert!(passed("far_operands", opts, code), "{opts:?}: exit {code}");
        let mut args = opts.to_vec();
        args.push("-DHAND_DECLARED");
        if let Some(code) = compile_and_run_aarch64_with("a64_far_operands", &src, &args, &[]) {
            assert!(
                passed("a64_far_operands", opts, code),
                "{opts:?}: exit {code}"
            );
        }
    }
}

/// The same operands compiled for each target from any host, `-S` only:
/// no displacement in the x86-64 assembly is past 32 bits -- the
/// assembler's rule, checked without one -- and the aarch64 assembly
/// assembles, where the cross assembler is installed.
#[test]
fn codegen_far_frame_asm_operands_assemble() {
    let src = big_stack_program(FAR_OPERANDS);
    for opt in ["-O0", "-O2"] {
        let x86 = asm_for_at(
            "c17_far_operands_",
            &src,
            &["--target=x86_64-unknown-linux-gnu", "-DHAND_DECLARED", opt],
        );
        let far: Vec<&str> = x86
            .lines()
            .filter(|l| displacements(l).any(|d| i32::try_from(d).is_err()))
            .collect();
        assert!(far.is_empty(), "{opt}: {far:#?}");
        assert!(
            x86.lines().any(|l| l.contains("(%r15)")),
            "{opt}: no operand is addressed from the far scratch:\n{x86}"
        );
        if aarch64_cross_available() {
            let a64 = asm_for_at(
                "c17_a64_far_operands_",
                &src,
                &["--target=aarch64-unknown-linux-gnu", "-DHAND_DECLARED", opt],
            );
            let dir = crate::common::work_dir("a64_far_operands_as");
            let s = dir.path().join("t.s");
            std::fs::write(&s, a64).unwrap();
            let status = std::process::Command::new("aarch64-linux-gnu-as")
                .arg("-o")
                .arg(dir.path().join("t.o"))
                .arg(&s)
                .status()
                .unwrap();
            assert!(status.success(), "{opt}");
        }
    }
}

/// The displacements of the x86-64 memory operands on `line`: each number
/// directly before a `(`.
fn displacements(line: &str) -> impl Iterator<Item = i64> + '_ {
    line.match_indices('(').filter_map(move |(i, _)| {
        let head = &line[..i];
        let start = head
            .rfind(|c: char| !(c.is_ascii_digit() || c == '-'))
            .map_or(0, |j| j + 1);
        head[start..].parse().ok()
    })
}
