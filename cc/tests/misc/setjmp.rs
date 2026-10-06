//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// setjmp/longjmp Mega-Test
//
// Consolidates: ALL setjmp/longjmp tests
//

use crate::common::compile_and_run;

// ============================================================================
// Mega-test: setjmp/longjmp
// ============================================================================

#[test]
fn misc_setjmp_mega() {
    let code = r#"
typedef int jmp_buf[64];
extern int setjmp(jmp_buf);
extern void longjmp(jmp_buf, int);
extern int _setjmp(jmp_buf);
extern void _longjmp(jmp_buf, int);

jmp_buf env;
int count = 0;

void do_longjmp(int val) {
    longjmp(env, val);
}

void level3(int val) {
    longjmp(env, val);
}

void level2(int val) {
    level3(val);
}

void level1(int val) {
    level2(val);
}

int main(void) {
    // ========== BASIC SETJMP/LONGJMP (returns 1-9) ==========
    {
        // Basic setjmp/longjmp
        int val = setjmp(env);
        if (val == 0) {
            longjmp(env, 42);
            return 1;  // Should never reach here
        }
        if (val != 42) return 2;
    }

    // ========== MULTIPLE RETURNS (returns 10-19) ==========
    {
        count = 0;
        int val = setjmp(env);
        count++;
        if (count < 4) {
            longjmp(env, count * 10);
            return 10;
        }
        if (count != 4) return 11;
        if (val != 30) return 12;  // From count=3 jump
    }

    // ========== LONGJMP WITH 0 (returns 20-29) ==========
    {
        // Per C standard, longjmp(env, 0) causes setjmp to return 1
        int val = setjmp(env);
        if (val == 0) {
            longjmp(env, 0);
            return 20;
        }
        if (val != 1) return 21;
    }

    // ========== LONGJMP FROM FUNCTION (returns 30-39) ==========
    {
        int val = setjmp(env);
        if (val == 0) {
            do_longjmp(77);
            return 30;
        }
        if (val != 77) return 31;
    }

    // ========== LONGJMP FROM NESTED CALLS (returns 40-49) ==========
    {
        int val = setjmp(env);
        if (val == 0) {
            level1(123);
            return 40;
        }
        if (val != 123) return 41;
    }

    // ========== VOLATILE PRESERVATION (returns 50-59) ==========
    {
        volatile int x = 5;
        int val = setjmp(env);
        if (val == 0) {
            x = 10;
            longjmp(env, 1);
            return 50;
        }
        if (x != 10) return 51;
    }

    // ========== _SETJMP VARIANT (returns 60-69) ==========
    {
        int val = _setjmp(env);
        if (val == 0) {
            _longjmp(env, 55);
            return 60;
        }
        if (val != 55) return 61;
    }

    return 0;
}
"#;
    assert_eq!(compile_and_run("misc_setjmp_mega", code, &[]), 0);
}

/// gcc's `__builtin_setjmp` / `__builtin_longjmp`: a five-word buffer, the
/// second return is always 1, the jump comes from another function, and
/// `volatile` locals of the setjmp's frame survive it.
#[test]
fn misc_builtin_setjmp_and_longjmp() {
    crate::common::compile_and_run_everywhere(
        "builtin_setjmp",
        r#"
/* gcc's lightweight non-local goto: `__builtin_setjmp(buf)` with a
   five-word buffer returns 0, and 1 when `__builtin_longjmp(buf, 1)` --
   called from another function, never the setjmp's own -- jumps back to
   it. No signal mask is saved. The gcc.c-torture `*-chk` tests reach it
   through chk.h to recover from a deliberate overflow abort. */
static void *buf[5];
static void *outer[5];
static int depth;

__attribute__((noinline, noreturn)) static void jump(void **b) { __builtin_longjmp(b, 1); }

__attribute__((noinline)) static void deep(int n)
{
    if (n == 0)
        jump(buf);
    depth++;
    deep(n - 1);
}

__attribute__((noinline)) static int try_deep(void)
{
    volatile int phase = 0;
    if (__builtin_setjmp(buf)) {
        /* Locals the setjmp's frame owns survive, if volatile. */
        return phase == 1 ? depth : -1;
    }
    phase = 1;
    deep(5);
    return -2;
}

/* Two buffers: an inner jump does not disturb the outer one. */
__attribute__((noinline)) static int nested(void)
{
    volatile int steps = 0;
    if (__builtin_setjmp(outer)) {
        return steps;
    }
    steps += 1;
    if (__builtin_setjmp(buf) == 0) {
        steps += 10;
        jump(buf);
    }
    steps += 100;
    jump(outer);
    return -1;
}

/* Used in a condition with other code around it, and called repeatedly. */
__attribute__((noinline)) static int count_jumps(int times)
{
    volatile int n = 0;
    for (int i = 0; i < times; i++) {
        if (__builtin_setjmp(buf) == 0)
            jump(buf);
        else
            n++;
    }
    return n;
}

int main(void)
{
    if (try_deep() != 5) return 1;
    if (nested() != 111) return 2;
    if (count_jumps(7) != 7) return 3;
    return 0;
}
"#,
    );
}

/// What `__builtin_setjmp` leaves to the compiler: values computed before it
/// and read after it -- never modified, so not `volatile` -- must survive a
/// jump that comes through frames which used every callee-saved register;
/// the frame's over-aligned base register must be re-established; and the
/// caller's own callee-saved registers must come back intact, though the
/// jump skipped the epilogues that restore them.
#[test]
fn misc_builtin_setjmp_keeps_values_and_registers() {
    crate::common::compile_and_run_everywhere(
        "builtin_setjmp_values",
        r#"
static void *buf[5];

__attribute__((noinline, noreturn)) static void jump(void) { __builtin_longjmp(buf, 1); }

/* Uses many callee-saved registers, then jumps past its epilogue. */
__attribute__((noinline)) static long churn(long a, long b, long c, long d, int n)
{
    long w = a * 3, x = b * 5, y = c * 7, z = d * 11, v = a ^ d, u = b ^ c;
    for (int i = 0; i < n; i++) {
        w += x; x += y; y += z; z += v; v += u; u += w;
        if (i == n - 1)
            jump();
    }
    return w + x + y + z + u + v;
}

__attribute__((noinline)) static long keep(long a, long b, double f, int n)
{
    long p = a * 7 + 1, q = b * 13 + 2, r = a ^ b, s = a + b, t = a - b, k = a * b;
    double g = f * 2.5, h = f + 1.0;
    if (__builtin_setjmp(buf))
        return p + q + r + s + t + k + (long)(g + h);
    churn(p, q, r, s, n);
    return -1;
}

__attribute__((noinline)) static int aligned(int n)
{
    _Alignas(64) volatile int arr[16];
    volatile int marker = 0;
    for (int i = 0; i < 16; i++)
        arr[i] = i * n;
    if (__builtin_setjmp(buf)) {
        if (((unsigned long)&arr[0]) % 64)
            return -5;
        int sum = 0;
        for (int i = 0; i < 16; i++)
            sum += arr[i];
        return sum + marker;
    }
    marker = 1000;
    churn(1, 2, 3, 4, n);
    return -1;
}

__attribute__((noinline)) static long outer(long seed)
{
    long a = seed * 3, b = seed * 5, c = seed * 7, d = seed * 11, e = seed * 13, f = seed * 17;
    long r = keep(seed, seed + 1, 1.5, 4);
    return r + a + b + c + d + e + f;
}

int main(void)
{
    long p = 5 * 7 + 1, q = 6 * 13 + 2, r = 5 ^ 6, s = 5 + 6, t = 5 - 6, k = 5 * 6;
    long want = p + q + r + s + t + k + (long)(1.5 * 2.5 + 1.5 + 1.0);
    if (keep(5, 6, 1.5, 3) != want)
        return 1;
    if (aligned(2) != 2 * 120 + 1000)
        return 2;
    if (outer(5) != want + 5 * (3 + 5 + 7 + 11 + 13 + 17))
        return 3;
    return 0;
}
"#,
    );
}

/// gcc models a `__builtin_longjmp` as an edge from every call after the
/// setjmp to its receiver, so a plain local stored before the call that
/// jumps is read back at the receiver with that store's value -- `volatile`
/// is not needed. (gcc.c-torture execute/pr60003.)
#[test]
fn misc_builtin_setjmp_sees_stores_before_the_jumping_call() {
    crate::common::compile_and_run_everywhere(
        "builtin_setjmp_stores",
        r#"
static void *buf[5];

__attribute__((noinline)) static void baz(void) { __builtin_longjmp(buf, 1); }
static void bar(void) { baz(); }

__attribute__((noinline)) static int foo(int x)
{
    int a = 0;
    if (__builtin_setjmp(buf) == 0) {
        while (1) {
            a = 1;
            bar();
        }
    }
    return a == 0 ? 0 : x;
}

int main(void)
{
    return foo(3) == 3 ? 0 : 1;
}
"#,
    );
}

/// The setjmp's buffer is read by whichever compiler built the longjmp, so
/// both must agree on its layout -- and gcc's depends on
/// `-fcf-protection`. With return protection (`full`, `return`) it is
/// frame pointer, resume address, shadow-stack pointer, stack pointer, and
/// the longjmp unwinds the shadow stack; otherwise the shadow-stack word is
/// not there and the stack pointer is the third word. Each side here is built
/// with the same flag, in every pairing of c17 and gcc, at -O0 and -O2: a
/// gcc longjmp reading c17's stack pointer as a shadow-stack pointer runs
/// `incsspq` and dies of SIGILL, and a c17 longjmp reading gcc's
/// shadow-stack word as the stack pointer jumps with a null stack.
#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
#[test]
fn misc_builtin_setjmp_interop_under_cf_protection() {
    let jumper = r#"
void *buf[5];
__attribute__((noinline)) void jump(int depth)
{
    if (depth > 0)
        jump(depth - 1);
    __builtin_longjmp(buf, 1);
}
"#;
    let receiver = r#"
extern void *buf[5];
extern void jump(int);
__attribute__((noinline)) static int recv(int x)
{
    volatile int seen = x;
    if (__builtin_setjmp(buf))
        return seen + 1;
    jump(3);
    return -1;
}
int main(void)
{
    return recv(41) == 42 ? 0 : 1;
}
"#;
    for flag in ["-fcf-protection=none", "-fcf-protection=full"] {
        crate::common::interop_host_with("builtin_setjmp_cet", jumper, receiver, &[flag]);
    }
}

/// The same pairings under `-fcf-protection=full` with the shadow stack
/// really on, where the CPU and kernel have it: `main` turns it on itself
/// (`arch_prctl(ARCH_SHSTK_ENABLE)`), so every return after that is checked
/// against it, and it never returns from a frame entered before. The
/// longjmp is 601 frames deep, so it unwinds the shadow stack through
/// `incsspq`'s 255-entry loop as well as its remainder; a longjmp that left
/// the shadow stack where it was faults at the receiver's own `ret`.
/// Without shadow-stack support the program runs with it off, which is the
/// test above.
#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
#[test]
fn misc_builtin_setjmp_interop_with_the_shadow_stack_on() {
    let jumper = r#"
void *buf[5];
__attribute__((noinline)) void jump(int depth)
{
    if (depth > 0)
        jump(depth - 1);
    __builtin_longjmp(buf, 1);
}
"#;
    let receiver = r#"
extern void *buf[5];
extern void jump(int);
static inline __attribute__((always_inline)) long syscall2(long n, long a, long b)
{
    long r;
    __asm__ volatile("syscall" : "=a"(r) : "a"(n), "D"(a), "S"(b) : "rcx", "r11", "memory");
    return r;
}
__attribute__((noinline)) int recv(int x)
{
    volatile int seen = x;
    if (__builtin_setjmp(buf))
        return seen + 1;
    jump(600);
    return -1;
}
__attribute__((noinline)) int deeper(int n)
{
    if (n > 0)
        return deeper(n - 1) + 0;
    return recv(41);
}
int main(void)
{
    /* arch_prctl(ARCH_SHSTK_ENABLE, ARCH_SHSTK_SHSTK) */
    long on = syscall2(158, 0x5001, 1) == 0;
    /* A receiver deep in the stack, and calls and returns after it. */
    int r = deeper(300);
    r += deeper(2);
    if (!on)
        return r == 84 ? 0 : 1;
    /* exit_group: main's own return would not match the shadow stack. */
    syscall2(231, r == 84 ? 0 : 1, 0);
    for (;;)
        ;
}
"#;
    crate::common::interop_host_with(
        "builtin_setjmp_shstk",
        jumper,
        receiver,
        &["-fcf-protection=full"],
    );
}
