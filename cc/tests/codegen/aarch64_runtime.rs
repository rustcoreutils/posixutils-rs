//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// aarch64 wrong answers at run time, each found by building gcc's torture
// execute/ suite for aarch64 and running it under qemu. The host suite cannot
// see any of them.
//

use crate::common::compile_and_run_aarch64;

fn run_both_levels(name: &str, src: &str) {
    for opt in ["-O0", "-O2"] {
        if let Some(code) = compile_and_run_aarch64(name, src, opt) {
            assert_eq!(code, 0, "{name} at {opt}");
        }
    }
}

/// An integer `?:` borrowed allocatable registers for its operands.
///
/// `emit_select_int` loaded the condition and both arms into X10/X11/X12 --
/// or X11/X12/X13 when the destination was X10 -- but X12 and X13 are handed
/// out by the allocator. A live value there was overwritten by the select's
/// constant and read back wrong after it: below, `yi` came back as 4 and the
/// function answered 8 for 4. From `execute/20020615-1` and `pr95731`.
#[test]
fn aarch64_select_keeps_to_scratch_registers() {
    run_both_levels(
        "a64_select_scratch",
        r#"
typedef struct { int axes_swapped, x_inverted, y_inverted; } font_hints;
typedef struct { long x, y; } fixed_point;

__attribute__((noinline))
int line_hints(const font_hints *fh, const fixed_point *p0, const fixed_point *p1)
{
    long dx = p1->x - p0->x;
    long dy = p1->y - p0->y;
    long adx, ady;
    int xi = fh->x_inverted, yi = fh->y_inverted;
    int hints;
    if (xi) dx = -dx;
    if (yi) dy = -dy;
    if (fh->axes_swapped) {
        long t = dx; int ti = xi;
        dx = dy, xi = yi;
        dy = t, yi = ti;
    }
    adx = dx < 0 ? -dx : dx;
    ady = dy < 0 ? -dy : dy;
    if (dy != 0 && (adx <= ady >> 4)) {
        hints = dy > 0 ? 2 : 1;
        if (xi) hints ^= 3;
    } else if (dx != 0 && (ady <= adx >> 4)) {
        hints = dx < 0 ? 8 : 4;
        if (yi) hints ^= 12;
    } else
        hints = 0;
    return hints;
}

int main(void)
{
    static font_hints fh[] = {{0, 1, 0}, {0, 0, 1}, {0, 0, 0}};
    static fixed_point p[] = {{0x30000, 0x13958}, {0x30000, 0x18189},
                              {0x13958, 0x30000}, {0x18189, 0x30000}};
    if (line_hints(fh, p, p + 1) != 1) return 1;
    if (line_hints(fh + 1, p + 2, p + 3) != 8) return 2;
    if (line_hints(fh + 2, p + 2, p + 3) != 4) return 3;
    return 0;
}
"#,
    );
}

/// A function returning a large struct lost its return buffer across a call.
///
/// AAPCS64 passes the buffer's address in X8, which the allocator pinned the
/// hidden `__sret` parameter to for the whole function. X8 is not preserved
/// across a call -- and a call that itself returns a large aggregate loads it
/// with its own buffer -- but the spill of arguments live across calls only
/// looked at X0-X7. So the result was stored into the last callee's buffer and
/// the caller received zeros. From `execute/pr108498-1`.
#[test]
fn aarch64_sret_pointer_survives_a_call() {
    run_both_levels(
        "a64_sret_across_call",
        r#"
struct C { long a, b, c; };
__attribute__((noinline)) struct C make(long a, long b)
{
    struct C x = { a, b, a + b };
    return x;
}
__attribute__((noinline)) void clobber(void) { }
__attribute__((noinline)) struct C forward(void) { return make(6, 7); }
__attribute__((noinline)) struct C after_call(void)
{
    struct C t = make(4, 5);
    clobber();
    return t;
}
int main(void)
{
    struct C x = forward(), y = after_call();
    if (x.a != 6 || x.b != 7 || x.c != 13) return 1;
    if (y.a != 4 || y.b != 5 || y.c != 9) return 2;
    return 0;
}
"#,
    );
}

/// A zero extension from 32 bits kept the source register's upper half.
///
/// `Zext` 32->64 assumed the 32-bit value was already zero-extended in its X
/// register, and copied it with a 64-bit `mov`. A `Sext` to 32 bits writes
/// the whole register, so `(unsigned long long)(unsigned int)(short)-1` came
/// out 0xffffffffffffffff, and the unsigned division and comparison below went
/// with it. From `execute/pr19606` and `pr42544`.
#[test]
fn aarch64_zero_extension_from_32_bits_clears_the_upper_half() {
    run_both_levels(
        "a64_zext32",
        r#"
signed char a = -4;
__attribute__((noinline)) long long quot(void) { return ((unsigned int)(signed int)a) / 2LL; }
__attribute__((noinline)) long long rem(void) { return ((unsigned int)(signed int)a) % 5LL; }
__attribute__((noinline)) unsigned long long widen(signed short s) { return (unsigned int)s; }
__attribute__((noinline)) int above(signed short s) { return (unsigned int)s >= 0x100000000ULL; }
int main(void)
{
    if (quot() != 2147483646) return 1;
    if (rem() != 2) return 2;
    if (widen(-1) != 0xffffffffULL) return 3;
    if (above(-1)) return 4;
    return 0;
}
"#,
    );
}

/// `va_start` counted the named parameters' stack area without their
/// alignment.
///
/// A `long double` is sixteen-byte aligned on the stack (AAPCS64 stage C),
/// and the tally that locates the first variadic argument summed
/// eight-byte-rounded sizes instead of laying the parameters out the way
/// `allocate_arguments` does, so after an odd number of eightbytes it pointed
/// a slot short. From `execute/pr44942`.
#[test]
fn aarch64_va_start_after_aligned_stacked_parameters() {
    run_both_levels(
        "a64_va_start_aligned",
        r#"
#include <stdarg.h>
__attribute__((noinline)) double after(double a, double b, double c, double d, double e,
                                       double f, double g, long double h, double i,
                                       long double j, double k, long double l, double m,
                                       long double n, ...)
{
    va_list ap;
    va_start(ap, n);
    double o = va_arg(ap, double);
    va_end(ap);
    return o;
}
__attribute__((noinline)) int after_int(int a, int b, int c, int d, int e, int f, int g,
                                        long double h, int i, long double j, ...)
{
    va_list ap;
    va_start(ap, j);
    int o = va_arg(ap, int);
    va_end(ap);
    return o;
}
int main(void)
{
    if (after(0, 0, 0, 0, 0, 0, 0, 0.0L, 0, 0.0L, 0, 0.0L, 0, 0.0L, 1234.0) != 1234.0)
        return 1;
    if (after_int(0, 0, 0, 0, 0, 0, 0, 0.0L, 0, 0.0L, 4321) != 4321) return 2;
    return 0;
}
"#,
    );
}

/// `__builtin_signbit` is type-generic, and glibc's `signbit` relies on it.
///
/// c17 treated it as `double`-only and handed a `long double` to the
/// `double` emitter unconverted, which on aarch64 reads the low 64 bits of a
/// binary128 -- not where its sign is. From `execute/20080502-1`.
#[test]
fn aarch64_signbit_of_long_double() {
    run_both_levels(
        "a64_signbit_ld",
        r#"
__attribute__((noinline)) long double pick(long double x)
{
    return __builtin_signbit(x) ? 3.5L : 0.0L;
}
int main(void)
{
    volatile long double neg = -1.0L, pos = 1.0L, nz = -0.0L;
    volatile float f = -2.0f;
    if (pick(-1.0L) != 3.5L || pick(1.0L) != 0.0L) return 1;
    if (!__builtin_signbit(neg) || __builtin_signbit(pos)) return 2;
    if (!__builtin_signbit(nz)) return 3;
    if (!__builtin_signbit(f)) return 4;
    return 0;
}
"#,
    );
}
