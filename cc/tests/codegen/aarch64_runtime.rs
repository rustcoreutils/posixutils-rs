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

/// A logical operation with any constant, at both widths.
///
/// `and`/`orr`/`eor` take a bitmask immediate, which cannot hold zero,
/// all-ones or most ordinary numbers. Whether a constant encodes is decided in
/// one place, `legalize.rs`, which keeps an encodable one as an immediate
/// (printed at the operation's width, so a W-form operation never shows the
/// assembler a negative value) and puts any other in X15. Each result is
/// checked against the same operation with the mask read from a `volatile`,
/// which always takes the register path, so the program is its own oracle.
fn logical_masks_source() -> String {
    let masks64: [&str; 10] = [
        "0x0UL",
        "0xffffffffffffffffUL",
        "0xffUL",
        "0x3e8UL",
        "0x5555555555555555UL",
        "0x123456789abcdefUL",
        "0xffffffff00000000UL",
        "0xfffffffffffffff0UL",
        "0x8000000000000001UL",
        "0x00ff00ff00ff00ffUL",
    ];
    let masks32: [&str; 9] = [
        "0x0u",
        "0xffffffffu",
        "0xffu",
        "0x3e8u",
        "0x55555555u",
        "0x12345678u",
        "0xfffffff0u",
        "0x80000001u",
        "0x00ff00ffu",
    ];
    let mut body = String::new();
    let mut n = 0;
    for (ty, masks) in [
        ("unsigned long", &masks64[..]),
        ("unsigned int", &masks32[..]),
    ] {
        for m in masks {
            for op in ["&", "|", "^"] {
                n += 1;
                body.push_str(&format!(
                    "    {{ volatile {ty} vm = {m}; {ty} x = seed_{w};\n      \
                     if ((x {op} {m}) != (x {op} vm)) return {n};\n      \
                     {ty} y = x; y {op}= {m}; if (y != (x {op} vm)) return {n}; }}\n",
                    w = if ty == "unsigned long" { "l" } else { "i" },
                ));
            }
        }
    }
    format!(
        "volatile unsigned long seed_l = 0xdeadbeefcafebabeUL;\n\
         volatile unsigned int seed_i = 0xcafebabeu;\n\
         int main(void)\n{{\n{body}    return 0;\n}}\n"
    )
}

#[test]
fn aarch64_logical_immediates_of_any_value() {
    run_both_levels("a64_logical_masks", &logical_masks_source());
}

/// A global copied with an access wider than its alignment.
///
/// `ldr x0, [x0, :lo12:sym]` scales the symbol's low bits by the access size,
/// so the linker can encode it only when the symbol is a multiple of that
/// size. An 8-byte struct of `int`s is 4-aligned yet copied with one 64-bit
/// load, and placed after a `char` it sits at an odd multiple of 4: the link
/// failed with "relocation truncated to fit" (`execute/20040709-1..3`). The
/// low bits are now folded only when the symbol's known alignment covers the
/// access.
#[test]
fn aarch64_underaligned_global_copied_whole() {
    run_both_levels(
        "a64_lo12_align",
        r#"
#define NI __attribute__((noinline))
/* 8 bytes, but only 4-aligned, so a whole-struct copy is one 64-bit load
   from an address that need not be a multiple of 8. The `char`s before each
   one push it to an odd multiple of 4. */
struct P { int a, b; };
struct B { unsigned i : 6, j : 11, k : 15; unsigned l; };
char c1; struct P p;
char c2; struct B b;
char c3; struct P p2 = { 5, 6 };
NI struct P getp(void) { return p; }
NI struct B getb(void) { return b; }
NI struct P getp2(void) { return p2; }
NI int sum(void) { struct P x = p; struct B y = b; return x.a + x.b + y.k + (int)y.l; }
int main(void)
{
    p.a = 1; p.b = 2; b.k = 3; b.l = 4;
    if (getp().a != 1 || getp().b != 2) return 1;
    if (getb().k != 3 || getb().l != 4) return 2;
    if (getp2().a != 5 || getp2().b != 6) return 3;
    if (sum() != 10) return 4;
    return 0;
}
"#,
    );
}

/// A `long double` function that can fall off its end.
///
/// The implicit `return 0` is an integer immediate moved into the binary128
/// result register, and there is no general-to-Q `fmov`: the printer
/// panicked at -O0 (`compile/pr65540`). An immediate is now placed as a bit
/// pattern through the one helper every floating constant uses.
#[test]
fn aarch64_long_double_constants_and_fall_off_return() {
    run_both_levels(
        "a64_ld_consts",
        r#"
#define NI __attribute__((noinline))
NI long double k1(void) { return 1.5L; }
NI long double k2(long double x) { return x * 3.25L + 0.1L; }
NI int cmpz(long double x) { if (x > 0.0) return 1; else if (x < 0.0) return -1; return 0; }
NI long double absl_(long double x) { if (x > 0.0) return x; else if (x < 0.0) return -x; else return x; }
int main(void)
{
    if (k1() != 1.5L) return 1;
    if (k2(2.0L) != 2.0L * 3.25L + 0.1L) return 2;
    if (cmpz(-2.0L) != -1 || cmpz(3.0L) != 1 || cmpz(0.0L) != 0) return 3;
    if (absl_(-2.5L) != 2.5L || absl_(4.0L) != 4.0L) return 4;
    return 0;
}
"#,
    );
}

/// Where `va_start` finds the first variadic argument after every class of
/// named parameter.
///
/// `va_start`'s tally of the named parameters was its own walk, separate from
/// the one `allocate_arguments` used, and the two disagreed: a composite over
/// sixteen bytes travels as a pointer -- one register or one eight-byte slot --
/// where the tally charged its whole size, so `__stack` started sixteen bytes
/// late; and a zero-sized parameter takes nothing where the tally charged a
/// register. Both are one layout now, `param_layout`. Also covered: an HFA
/// forcing the floating-point bank onto the stack, a 16-aligned `__int128`
/// and `long double`, and a stacked by-reference composite with 16-aligned
/// members, whose pointer slot is eight-aligned.
#[test]
fn aarch64_va_start_after_every_named_parameter_class() {
    run_both_levels("a64_va_named", VA_NAMED);
}

const VA_NAMED: &str = r#"
#include <stdarg.h>
#define NI __attribute__((noinline))
struct Big { long a, b, c; };          /* over 16 bytes: passed as a pointer */
struct E { };                          /* zero-sized (GNU): not passed at all */
struct H { double a, b; };             /* HFA: two V registers */
struct L { long a; long double b; };   /* by reference, 16-aligned members */

NI long big_stacked(long a0, long a1, long a2, long a3, long a4, long a5, long a6, long a7,
                    struct Big big, ...)
{
    va_list ap; va_start(ap, big);
    long x = va_arg(ap, long);
    va_end(ap);
    return x + big.c;
}
NI long big_in_reg(long a0, struct Big big, ...)
{
    va_list ap; va_start(ap, big);
    long x = va_arg(ap, long), y = va_arg(ap, long);
    va_end(ap);
    return x + y + big.b;
}
NI int empty(int a, struct E e, ...)
{
    va_list ap; va_start(ap, e);
    int x = va_arg(ap, int);
    va_end(ap);
    return a + x;
}
NI double hfa(double d0, double d1, double d2, double d3, double d4, double d5, double d6,
              struct H h, ...)
{
    va_list ap; va_start(ap, h);
    double x = va_arg(ap, double);
    va_end(ap);
    return x + h.b;
}
NI long i128(long a0, long a1, long a2, long a3, long a4, long a5, long a6, __int128 q, ...)
{
    va_list ap; va_start(ap, q);
    long x = va_arg(ap, long);
    va_end(ap);
    return x + (long)(q >> 64);
}
NI double ldbl(double d0, double d1, double d2, double d3, double d4, double d5, double d6,
               double d7, long double x, ...)
{
    va_list ap; va_start(ap, x);
    double y = va_arg(ap, double);
    va_end(ap);
    return y + (double)x;
}
NI long by_ref_stacked(long a0, long a1, long a2, long a3, long a4, long a5, long a6, long a7,
                       long s0, struct L l, long after)
{
    return s0 + l.a + (long)l.b + after;
}
int main(void)
{
    struct Big b = {1, 2, 3};
    struct E e;
    struct H h = {0.5, 1.5};
    struct L l = {10, 20.0L};
    if (big_stacked(0, 0, 0, 0, 0, 0, 0, 0, b, 42L) != 45) return 1;
    if (big_in_reg(0, b, 40L, 50L) != 92) return 2;
    if (empty(1, e, 41) != 42) return 3;
    if (hfa(0, 0, 0, 0, 0, 0, 0, h, 2.5) != 4.0) return 4;
    if (i128(0, 0, 0, 0, 0, 0, 0, (__int128)7 << 64, 35L) != 42) return 5;
    if (ldbl(0, 0, 0, 0, 0, 0, 0, 0, 2.0L, 40.0) != 42.0) return 6;
    if (by_ref_stacked(0, 0, 0, 0, 0, 0, 0, 0, 1, l, 300) != 331) return 7;
    return 0;
}
"#;
