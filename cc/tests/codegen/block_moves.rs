//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Blocks of bytes the back ends move, and the two rules they obey.
//
// An object's bytes are moved in descending power-of-two chunks, and the moves
// are bounded: past a threshold the copy becomes a bulk primitive instead of an
// unrolled run. `memexpand::block_chunks` and `BlockOp::limit` state both rules
// for the IR, and `codegen_struct_copy_across_the_inline_threshold` records the
// incident that put them there.
//
// The back ends cannot call `memcpy` -- they are past the point where a call
// can be synthesized -- so their bulk primitive is `rep movsq` or a counted
// loop. But the chunk rule is the same, and each site that wrote its own copy
// of it either rounded the size *up* or dropped the bound:
//
//   * a two-SSE struct parameter stored both halves at eight bytes, so the
//     four-byte high half of a 12-byte struct wrote four bytes past it;
//   * the spilled-parameter prologue stepped eight regardless of width;
//   * `va_arg` of a large aggregate unrolled with no bound at all;
//   * the stacked-argument copy rounded the *source* read up, which is not the
//     same as the destination: argument slots really are eightbyte-granular, so
//     writing eight is correct where reading eight is not.
//
// Sizes here are deliberately not multiples of eight, and guards sit either
// side of the objects, so a rounded-up move shows as a wrong byte rather than
// landing in padding and passing by luck.
//

use crate::common::{compile_and_run, compile_and_run_everywhere};

/// The answers, with guards, across the sizes these paths classify differently.
///
/// The assembly checks above pin the widths; this pins that the values survive.
/// None of the sizes is a multiple of eight and every object is fenced, so an
/// over-copy shows up as a clobbered guard rather than as padding nobody reads.
#[test]
fn codegen_block_moved_parameters_keep_their_values() {
    let code = r#"
#include <stdarg.h>

struct P3 { int a, b, c; };            /* 12 bytes, spilled/stacked */
struct F3 { float x, y, z; };          /* 12 bytes, two SSE */
struct B7 { unsigned char c[7]; };     /* 7 bytes */
struct B13 { unsigned char c[13]; };   /* 13 bytes */

__attribute__((noinline)) int take_p3(long a, long b, long c, long d, long e,
                                      long f, struct P3 p)
{ return p.a + p.b + p.c; }

__attribute__((noinline)) float take_f3(struct F3 p) { return p.x + p.y + p.z; }

__attribute__((noinline)) int take_b7(struct B7 v)
{
    int s = 0;
    for (int i = 0; i < 7; i++) s += v.c[i];
    return s;
}

__attribute__((noinline)) int take_b13(long a, long b, long c, long d, long e,
                                       long f, struct B13 v)
{
    int s = 0;
    for (int i = 0; i < 13; i++) s += v.c[i];
    return s;
}

__attribute__((noinline)) int va_b13(int n, ...)
{
    va_list ap;
    va_start(ap, n);
    struct B13 v = va_arg(ap, struct B13);
    va_end(ap);
    int s = 0;
    for (int i = 0; i < 13; i++) s += v.c[i];
    return s;
}

int main(void)
{
    unsigned char lo = 0xA5;
    struct P3 p = {1, 2, 3};
    struct F3 f = {1.f, 2.f, 4.f};
    struct B7 b7;
    struct B13 b13;
    unsigned char hi = 0x5A;

    for (int i = 0; i < 7; i++) b7.c[i] = (unsigned char)(i + 1);
    for (int i = 0; i < 13; i++) b13.c[i] = (unsigned char)(i + 1);

    if (take_p3(1, 2, 3, 4, 5, 6, p) != 6) return 1;
    if (take_f3(f) != 7.f) return 2;
    if (take_b7(b7) != 28) return 3;
    if (take_b13(1, 2, 3, 4, 5, 6, b13) != 91) return 4;
    if (va_b13(0, b13) != 91) return 5;
    if (lo != 0xA5 || hi != 0x5A) return 6;
    return 0;
}
"#;
    assert_eq!(compile_and_run("block_moved_params", code, &[]), 0);
}

/// A zero-sized aggregate -- GNU's `struct {}`, or `struct { int a[0]; }` --
/// moves no bytes however it is read: by name, through `*p`, `s.m`, `a[i]`,
/// a compound literal, a conditional, a statement expression, a call's
/// result, or as an argument.
///
/// Read as an rvalue, it was "loaded" at zero bits wherever the reading site
/// spelled the by-address rule as `size > 64`, and the store of that value
/// was lowered as one byte -- over the member that follows it. The members
/// around it here are the guards.
#[test]
fn codegen_a_zero_sized_aggregate_moves_no_bytes() {
    let code = r#"
#include <stdarg.h>

struct E {};
struct Z { int a[0]; };
struct W { char lo; struct E e; char mid; struct Z z; char hi; };

struct E ge1, ge2, garr[4];
static struct E gs;

__attribute__((noinline)) struct E ret_e(void) { struct E e; return e; }
__attribute__((noinline)) struct Z ret_z(void) { struct Z z; return z; }
__attribute__((noinline)) struct E pass_e(struct E e) { return e; }
__attribute__((noinline)) int take(int a, struct E e, int b, struct Z z, int c)
{ (void)e; (void)z; return a * 100 + b * 10 + c; }
__attribute__((noinline)) int va_take(int n, ...)
{
    va_list ap;
    va_start(ap, n);
    int a = va_arg(ap, int);
    struct E e = va_arg(ap, struct E);
    int b = va_arg(ap, int);
    va_end(ap);
    (void)e;
    return n * 100 + a * 10 + b;
}

static int intact(const struct W *w)
{ return w->lo == 0x11 && w->mid == 0x22 && w->hi == 0x33; }

int main(int argc, char **argv)
{
    (void)argv;
    int c = argc > 0, i = argc;
    struct E a, b, arr[3];
    struct Z za, zarr[3];
    struct W w = {0x11, {}, 0x22, {}, 0x33};
    struct W *pw = &w;
    struct E *p = &a;
    static struct E sl;

    w.e = a;                              if (!intact(&w)) return 1;
    w.e = *p;                             if (!intact(&w)) return 2;
    w.e = arr[i];                         if (!intact(&w)) return 3;
    w.e = c ? a : b;                      if (!intact(&w)) return 4;
    w.e = (struct E){};                   if (!intact(&w)) return 5;
    w.e = ({ struct E t; t; });           if (!intact(&w)) return 6;
    w.e = ret_e();                        if (!intact(&w)) return 7;
    w.e = pass_e(w.e);                    if (!intact(&w)) return 8;
    w.e = ge1;                            if (!intact(&w)) return 9;
    w.e = gs;                             if (!intact(&w)) return 10;
    w.e = garr[i];                        if (!intact(&w)) return 11;
    w.e = sl;                             if (!intact(&w)) return 12;
    pw->e = pw->e;                        if (!intact(&w)) return 13;
    w.z = za;                             if (!intact(&w)) return 14;
    w.z = zarr[i];                        if (!intact(&w)) return 15;
    w.z = ret_z();                        if (!intact(&w)) return 16;
    w.z = *&pw->z;                        if (!intact(&w)) return 17;

    struct E x = w.e, y = *p, z = arr[i];
    (void)x; (void)y; (void)z;
    arr[i] = w.e; *p = pw->e; ge1 = ge2; gs = garr[i]; sl = a;
    if (!intact(&w)) return 18;

    if (take(1, w.e, 2, w.z, 3) != 123) return 19;
    if (take(4, ret_e(), 5, ret_z(), 6) != 456) return 20;
    if (take(7, c ? a : b, 8, zarr[i], 9) != 789) return 21;
    if (va_take(1, 2, w.e, 3) != 123) return 22;
    if (!intact(&w)) return 23;
    return 0;
}
"#;
    compile_and_run_everywhere("zero_sized_aggregate", code);
}
