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

use crate::common::{compile_and_run, compile_and_run_aarch64_with};

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
const FAR_FRAME: &str = r#"
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
#include <stdarg.h>

#if defined(__x86_64__)
#define BUMP(x) __asm__ volatile("addq $1, %0" : "+m"(x))
#else
#define BUMP(x) __asm__ volatile("ldr x9, %0\n\tadd x9, x9, #1\n\tstr x9, %0" : "+m"(x) : : "x9")
#endif

#define ASIZE 0xC0000000UL
#define STACK (ASIZE + 0x1000000UL)
#define NI __attribute__((noinline))

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
        let code = compile_and_run("far_frame", FAR_FRAME, &args);
        assert!(passed("far_frame", opts, code), "{opts:?}: exit {code}");
    }
}

#[test]
fn codegen_frame_past_two_gigabytes_runs_on_aarch64() {
    for opts in RUNS {
        let mut args = opts.to_vec();
        args.push("-DHAND_DECLARED");
        if let Some(code) = compile_and_run_aarch64_with("a64_far_frame", FAR_FRAME, &args, &[]) {
            assert!(passed("a64_far_frame", opts, code), "{opts:?}: exit {code}");
        }
    }
}
