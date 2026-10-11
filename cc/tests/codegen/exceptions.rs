//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// `-fexceptions`: a `cleanup` variable's handler runs when its frame is
// unwound -- by thread cancellation, by `pthread_exit`, or by any other
// forced or foreign unwind passing through C frames -- as gcc arranges with
// an LSDA, `__gcc_personality_v0` and landing pads. glibc builds libio and
// nptl this way and relies on it for its locks.
//

use crate::common::{compile_and_run, compile_and_run_aarch64_with};

/// Run `src` with `-fexceptions` at -O0 and -O2, on the host and on aarch64
/// under qemu, and require exit status 0 from each.
fn run_everywhere(name: &str, src: &str) {
    for level in ["-O0", "-O2"] {
        let opts = [level.to_string(), "-fexceptions".to_string()];
        assert_eq!(compile_and_run(name, src, &opts), 0, "{name} at {level}");
        if let Some(rc) =
            compile_and_run_aarch64_with(name, src, &[level, "-fexceptions"], &["-lpthread"])
        {
            assert_eq!(rc, 0, "{name} on aarch64 at {level}");
        }
    }
}

/// A thread blocked in `pause()` inside the scope of a `cleanup` variable is
/// cancelled: the unwind gcc's cancellation does runs the handler on the way
/// out. Without an LSDA the frame is unwound and the handler never runs.
#[test]
fn cancellation_runs_cleanup_handlers() {
    let src = r#"
#include <pthread.h>
#include <unistd.h>

static volatile int cleaned;
static volatile int started;
static void cl(int *p) { cleaned += *p; }

static void *worker(void *arg) {
    (void)arg;
    int v __attribute__((cleanup(cl))) = 1;
    {
        int w __attribute__((cleanup(cl))) = 10;
        started = 1;
        for (;;)
            pause();
    }
    return 0;
}

int main(void) {
    pthread_t t;
    if (pthread_create(&t, 0, worker, 0))
        return 2;
    while (!started)
        usleep(1000);
    pthread_cancel(t);
    void *res;
    pthread_join(t, &res);
    if (res != PTHREAD_CANCELED)
        return 3;
    return cleaned == 11 ? 0 : 1;
}
"#;
    run_everywhere("eh_cancel", src);
}

/// glibc's `pthread_cleanup_push` is a `cleanup` variable in GNU C under
/// `__EXCEPTIONS`; the handler it pushes runs when the thread is cancelled.
#[test]
fn cancellation_runs_pthread_cleanup_push_handlers() {
    let src = r#"
#include <pthread.h>
#include <unistd.h>

static volatile int cleaned;
static volatile int started;
static void handler(void *p) { cleaned += *(int *)p; }

static void *worker(void *arg) {
    static int one = 1;
    (void)arg;
    pthread_cleanup_push(handler, &one);
    started = 1;
    for (;;)
        pause();
    pthread_cleanup_pop(0);
    return 0;
}

int main(void) {
#ifndef __EXCEPTIONS
    return 4;
#endif
    pthread_t t;
    if (pthread_create(&t, 0, worker, 0))
        return 2;
    while (!started)
        usleep(1000);
    pthread_cancel(t);
    pthread_join(t, 0);
    return cleaned == 1 ? 0 : 1;
}
"#;
    run_everywhere("eh_cleanup_push", src);
}

/// A forced unwind started two frames down passes through two C frames, each
/// with nested `cleanup` variables: every handler runs, innermost first, and
/// the stop function then leaves the unwind with `longjmp`.
#[test]
fn forced_unwind_runs_cleanups_in_order() {
    let src = r#"
#include <setjmp.h>
#include <stdlib.h>

struct _Unwind_Exception {
    unsigned long exception_class;
    void (*exception_cleanup)(int, struct _Unwind_Exception *);
    unsigned long private_1, private_2;
} __attribute__((__aligned__));
typedef int (*stop_fn)(int, int, unsigned long, struct _Unwind_Exception *,
                       void *, void *);
extern int _Unwind_ForcedUnwind(struct _Unwind_Exception *, stop_fn, void *);

static int seq[8], n;
static jmp_buf back;
static struct _Unwind_Exception exc;

static void cl(int *p) { seq[n++] = *p; }

static int stop(int version, int actions, unsigned long cls,
                struct _Unwind_Exception *e, void *ctx, void *param) {
    (void)version; (void)cls; (void)e; (void)ctx; (void)param;
    /* Every handler has run once the unwind is about to leave outer(). */
    if (n == 4 || (actions & 16 /* _UA_END_OF_STACK */))
        longjmp(back, 1);
    return 0; /* _URC_NO_REASON: keep unwinding */
}

__attribute__((noinline)) static void thrower(int k) {
    exc.exception_class = 0x43313700;
    if (k)
        _Unwind_ForcedUnwind(&exc, stop, 0);
}

__attribute__((noinline)) static int inner(int k) {
    int b __attribute__((cleanup(cl))) = 3;
    {
        int c __attribute__((cleanup(cl))) = 4;
        thrower(k);
        c += 10;
    }
    return b;
}

__attribute__((noinline)) static int outer(int k) {
    int a __attribute__((cleanup(cl))) = 1;
    int a2 __attribute__((cleanup(cl))) = 2;
    int r = inner(k);
    return r + a + a2;
}

int main(void) {
    /* The normal path first: 14, 3, then 2, 1. */
    if (outer(0) != 6 || n != 4 || seq[0] != 14 || seq[1] != 3 ||
        seq[2] != 2 || seq[3] != 1)
        return 2;
    n = 0;
    if (setjmp(back) == 0) {
        outer(1);
        return 3;
    }
    return n == 4 && seq[0] == 4 && seq[1] == 3 && seq[2] == 2 &&
                   seq[3] == 1
               ? 0
               : 1;
}
"#;
    run_everywhere("eh_forced", src);
}

/// The forced unwind again, through functions the optimizer inlines: a
/// small `static` function with cleanups of its own, called inside the
/// scope of its caller's cleanup, and a cleanup function that is inlined
/// into the landing pads. An unwind out of the inlined body runs its
/// cleanups and then its caller's.
#[test]
fn forced_unwind_through_inlined_cleanups() {
    let src = r#"
#include <setjmp.h>

struct _Unwind_Exception {
    unsigned long exception_class;
    void (*exception_cleanup)(int, struct _Unwind_Exception *);
    unsigned long private_1, private_2;
} __attribute__((__aligned__));
typedef int (*stop_fn)(int, int, unsigned long, struct _Unwind_Exception *,
                       void *, void *);
extern int _Unwind_ForcedUnwind(struct _Unwind_Exception *, stop_fn, void *);

static int seq[8], n;
static jmp_buf back;
static struct _Unwind_Exception exc;

static void cl(int *p) { if (n < 8) seq[n++] = *p; }

static int stop(int version, int actions, unsigned long cls,
                struct _Unwind_Exception *e, void *ctx, void *param) {
    (void)version; (void)cls; (void)e; (void)ctx; (void)param;
    if (n == 3 || (actions & 16))
        longjmp(back, 1);
    return 0;
}

__attribute__((noinline)) static void thrower(int k) {
    if (k)
        _Unwind_ForcedUnwind(&exc, stop, 0);
}

static inline int step(int k) {
    int s __attribute__((cleanup(cl))) = 7;
    int t __attribute__((cleanup(cl))) = 8;
    thrower(k);
    return s + t;
}

__attribute__((noinline)) static int run(int k) {
    int r __attribute__((cleanup(cl))) = 9;
    return step(k) + r;
}

int main(void) {
    if (run(0) != 24 || n != 3 || seq[0] != 8 || seq[1] != 7 || seq[2] != 9)
        return 2;
    n = 0;
    if (setjmp(back) == 0) {
        run(1);
        return 3;
    }
    return n == 3 && seq[0] == 8 && seq[1] == 7 && seq[2] == 9 ? 0 : 1;
}
"#;
    run_everywhere("eh_inlined", src);
}

/// `pthread_exit` from a function two calls down unwinds the thread's
/// frames: the cleanups of every frame on the way run, innermost first,
/// including those in a loop body and a statement expression.
#[test]
fn pthread_exit_runs_cleanups_of_every_frame() {
    let src = r#"
#include <pthread.h>

static int seq[16], n;
static void cl(int *p) { if (n < 16) seq[n++] = *p; }

__attribute__((noinline)) static void leave(int k) {
    int d __attribute__((cleanup(cl))) = 30 + k;
    if (k == 2)
        pthread_exit(0);
}

__attribute__((noinline)) static int middle(void) {
    int c __attribute__((cleanup(cl))) = 20;
    for (int k = 0; k < 4; k++) {
        int e __attribute__((cleanup(cl))) = 40 + k;
        ({ int f __attribute__((cleanup(cl))) = 50 + k; leave(k); f; });
    }
    return c;
}

static void *worker(void *arg) {
    (void)arg;
    int b __attribute__((cleanup(cl))) = 10;
    middle();
    return 0;
}

int main(void) {
    pthread_t t;
    if (pthread_create(&t, 0, worker, 0))
        return 2;
    pthread_join(t, 0);
    static const int want[] = {30, 50, 40, 31, 51, 41, 32, 52, 42, 20, 10};
    if (n != 11)
        return 1;
    for (int i = 0; i < 11; i++)
        if (seq[i] != want[i])
            return 1;
    return 0;
}
"#;
    run_everywhere("eh_pthread_exit", src);
}

/// glibc's libio takes its stream locks with `_IO_acquire_lock`, which only
/// exists under `__EXCEPTIONS` and is a `cleanup` variable; without the macro
/// the probe does not compile (`_IO_acquire_lock_needs_exceptions_enabled`).
#[test]
fn libio_acquire_lock_probe_compiles_and_unlocks() {
    let src = r#"
#include <stdio.h>

static int locked;
static void lockf_(FILE *f) { (void)f; locked++; }
static void unlockf_(FILE **f) { (void)f; locked--; }

#ifdef __EXCEPTIONS
# define _IO_acquire_lock(_fp) \
  do { FILE *_IO_acquire_lock_file __attribute__((cleanup(unlockf_))) = (_fp); \
       lockf_(_IO_acquire_lock_file);
#else
# define _IO_acquire_lock(_fp) _IO_acquire_lock_needs_exceptions_enabled
#endif
#define _IO_release_lock(_fp) ; } while (0)

static int put(FILE *fp, const char *s) {
    int r;
    _IO_acquire_lock(fp);
    r = fputs(s, fp);
    if (r < 0)
        break;
    r = 0;
    _IO_release_lock(fp);
    return r;
}

int main(void) {
    if (put(stdout, "") != 0)
        return 2;
    return locked == 0 ? 0 : 1;
}
"#;
    run_everywhere("eh_libio_lock", src);
}

/// Every normal way out of a scope still runs its cleanups once, and only
/// once, with `-fexceptions` on: the landing pads add paths, they change
/// none.
#[test]
fn normal_exits_are_unchanged_under_fexceptions() {
    run_everywhere("eh_normal_exits", crate::misc::cleanup_attr::CLEANUP);
}
