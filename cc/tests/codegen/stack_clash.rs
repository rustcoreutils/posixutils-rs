//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-fstack-clash-protection` at run time: probed programs compute what
// unprobed ones do, and an allocation large enough to jump the guard page
// below a stack faults on the guard instead of writing whatever lies beyond
// it -- with the faulting frame and its caller still on the unwinder's walk.
// Where the probes go is `test_asm/codegen_stack_clash.rs`.
//

use crate::common::{compile_and_run, compile_and_run_aarch64_with};

const FLAG: &str = "-fstack-clash-protection";

/// Run `src` with the protection on: on the host at the matrix levels, -O0
/// and -O2, and on aarch64 under qemu at -O0 and -O2.
fn run_probed_everywhere(name: &str, src: &str) {
    let flag = vec![FLAG.to_string()];
    assert_eq!(compile_and_run(name, src, &flag), 0, "{name} at the matrix");
    for opt in ["-O0", "-O2"] {
        let opts = vec![opt.to_string(), FLAG.to_string()];
        assert_eq!(compile_and_run(name, src, &opts), 0, "{name} at {opt}");
        if let Some(code) = compile_and_run_aarch64_with(name, src, &[opt, FLAG], &[]) {
            assert_eq!(code, 0, "{name} on aarch64 at {opt}");
        }
    }
}

/// Large frames, `alloca` and VLAs in loops, recursion through large frames,
/// and a large by-value argument: the same answers with every one probed.
#[test]
fn codegen_stack_clash_programs_run() {
    let src = r#"
#include <string.h>

struct big { char c[70000]; };

__attribute__((noinline)) long sum_big(struct big b) {
    long s = 0;
    for (int i = 0; i < 70000; i += 1000)
        s += b.c[i];
    return s;
}

__attribute__((noinline)) long frame_1m(int k) {
    volatile char buf[1 << 20];
    for (int i = 0; i < (1 << 20); i += 4096)
        buf[i] = (char)((i >> 12) + k);
    long s = 0;
    for (int i = 0; i < (1 << 20); i += 4096)
        s += buf[i];
    return s;
}

__attribute__((noinline)) long alloca_loop(int n) {
    long s = 0;
    for (int r = 0; r < 3; r++) {
        char *p = __builtin_alloca(n + r);
        memset(p, r + 1, n + r);
        s += p[0] + p[n + r - 1];
    }
    return s;
}

__attribute__((noinline)) long vla_loop(int n) {
    long s = 0;
    for (int r = 0; r < 8; r++) {
        char v[n + r * 7];
        memset(v, r + 1, sizeof v);
        s += v[0] + v[sizeof v - 1];
    }
    return s;
}

__attribute__((noinline)) long recurse(int d) {
    volatile char buf[100000];
    buf[0] = (char)d;
    buf[99999] = (char)d;
    if (d == 0)
        return 0;
    return recurse(d - 1) + buf[0] + buf[99999];
}

static struct big g;

int main(void) {
    long want = 0;
    for (int i = 0; i < (1 << 20); i += 4096)
        want += (char)((i >> 12) + 3);
    if (frame_1m(3) != want)
        return 1;
    if (alloca_loop(1 << 20) != 2 * (1 + 2 + 3))
        return 2;
    if (alloca_loop(10) != 12 || alloca_loop(0 + 1) != 12)
        return 3;
    if (vla_loop(200000) != 2 * (1 + 2 + 3 + 4 + 5 + 6 + 7 + 8))
        return 4;
    if (vla_loop(16) != 72)
        return 5;
    if (recurse(20) != 2 * (20 * 21 / 2))
        return 6;
    for (int i = 0; i < 70000; i += 1000)
        g.c[i] = 1;
    if (sum_big(g) != 70)
        return 7;
    return 0;
}
"#;
    run_probed_everywhere("stack_clash_run", src);
}

/// A thread whose stack sits on a 64 KiB guard page, with another mapping
/// right below the guard, allocates more than its stack and guard together
/// -- as a frame, with `alloca`, as a VLA, and as a by-value argument. Each
/// must fault on the guard and leave the mapping below it untouched; the
/// handler then walks the stack from the faulting instruction, inside a
/// probe loop, and must find the allocating function and its caller.
/// Unprobed, the allocation lands in the mapping below and the thread
/// returns (exit 2) or writes it (exit 3).
#[test]
fn codegen_stack_clash_probes_reach_the_guard() {
    let src = r#"
#include <execinfo.h>
#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <unistd.h>

#define BELOW (64 * 1024)
#define GUARD (64 * 1024)
#define STACK (320 * 1024)
#define JUMP (STACK + GUARD + 16 * 1024)

static char *base;
static volatile int which, extra;
static char altstack[64 * 1024];
static void *fns[8];

struct jump { char c[JUMP]; };
static struct jump *src;

__attribute__((noinline)) void by_frame(void) {
    volatile char b[JUMP];
    b[0] = 1;
}
__attribute__((noinline)) void by_alloca(void) {
    volatile char *p = __builtin_alloca(JUMP + extra);
    p[0] = 1;
}
__attribute__((noinline)) void by_vla(void) {
    volatile char v[JUMP + extra];
    v[0] = 1;
}
__attribute__((noinline)) void take(struct jump j) { (void)j; }
__attribute__((noinline)) void by_argument(void) { take(*src); }

__attribute__((noinline)) void allocate(int k) {
    switch (k) {
    case 0: by_frame(); break;
    case 1: by_alloca(); break;
    case 2: by_vla(); break;
    default: by_argument(); break;
    }
    extra = 0;
}

/* The function `pc` is in: the nearest of `fns` below it, among ours. */
static int owner(void *pc) {
    int best = -1;
    for (int j = 0; j < 8; j++)
        if ((char *)fns[j] <= (char *)pc
            && (best < 0 || (char *)fns[j] > (char *)fns[best]))
            best = j;
    return best;
}

static void on_segv(int sig, siginfo_t *si, void *uc) {
    char *at = si->si_addr;
    (void)sig;
    (void)uc;
    for (int i = 0; i < BELOW; i++)
        if (base[i])
            _exit(3);
    if (at < base + BELOW || at >= base + BELOW + GUARD)
        _exit(4);
    void *frames[32];
    int n = backtrace(frames, 32);
    for (int i = 0; i + 1 < n; i++)
        if (owner(frames[i]) == which && owner(frames[i + 1]) == 4)
            _exit(0);
    _exit(5);
}

static void *thread_main(void *arg) {
    stack_t ss;
    memset(&ss, 0, sizeof ss);
    ss.ss_sp = altstack;
    ss.ss_size = sizeof altstack;
    if (sigaltstack(&ss, 0))
        _exit(6);
    allocate(which);
    (void)arg;
    _exit(2);
}

static int run(int k) {
    pid_t pid = fork();
    if (pid == 0) {
        which = k;
        base = mmap(0, BELOW + GUARD + STACK, PROT_READ | PROT_WRITE,
                    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        if (base == MAP_FAILED || mprotect(base + BELOW, GUARD, PROT_NONE))
            _exit(7);
        struct sigaction sa;
        memset(&sa, 0, sizeof sa);
        sa.sa_sigaction = on_segv;
        sa.sa_flags = SA_SIGINFO | SA_ONSTACK;
        /* macOS raises SIGBUS, not SIGSEGV, on a PROT_NONE page. */
        sigaction(SIGSEGV, &sa, 0);
        sigaction(SIGBUS, &sa, 0);
        pthread_attr_t attr;
        pthread_attr_init(&attr);
        if (pthread_attr_setstack(&attr, base + BELOW + GUARD, STACK))
            _exit(8);
        pthread_t t;
        if (pthread_create(&t, &attr, thread_main, 0))
            _exit(9);
        pthread_join(t, 0);
        _exit(10);
    }
    int status;
    if (waitpid(pid, &status, 0) != pid)
        return 11;
    if (!WIFEXITED(status))
        return 12;
    return WEXITSTATUS(status);
}

int main(void) {
    void *warm[4];
    fns[0] = (void *)by_frame;
    fns[1] = (void *)by_alloca;
    fns[2] = (void *)by_vla;
    fns[3] = (void *)by_argument;
    fns[4] = (void *)allocate;
    fns[5] = (void *)take;
    fns[6] = (void *)thread_main;
    fns[7] = (void *)main;
    src = calloc(1, sizeof *src);
    /* The first `backtrace` loads the unwinder; not in a signal handler. */
    backtrace(warm, 4);
    for (int k = 0; k < 4; k++) {
        int code = run(k);
        if (code)
            return 20 * (k + 1) + code;
    }
    return 0;
}
"#;
    run_probed_everywhere("stack_clash_guard", src);
}
