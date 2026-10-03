//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// GNU cast to a union type: `(union U)expr`.
//
// The operand must have the type of one of the union's members, and the
// result is the union with that member initialized, exactly as
// `(union U){ .member = expr }` would be -- every other byte zero. The cast
// is not an lvalue.
//

use crate::common::compile_and_run_everywhere;

/// Every union size class (one register, two registers, memory), every
/// member kind, and every place a union value goes: a local, a static, a
/// nested initializer, an argument, a return value and a member access.
#[test]
fn cast_to_union_mega() {
    let code = r#"
#include <string.h>

struct pt { int x, y; };
typedef union { int i; long l; } V8;
typedef union { long l; double d; } W;
typedef union { char c; int i; } X;
typedef union { float f; unsigned u; } F;
typedef union { long l; double d; char c[12]; } V16;
typedef union { unsigned long long d; struct { unsigned long h, l; } s; } U;
typedef union { struct pt p; long l; } SP;
typedef union { char big[40]; double d; struct pt p; } BIG;
typedef union { const int ci; float f; } Q;
typedef union { int a[2]; int *p; } A;

W gw = (W)2.5;
static X gx = (X)(char)-1;
struct holder { int tag; W w; X x; } gh = { 7, (W)4.25, (X)(char)3 };

static double take_w(W w) { return w.d; }
static long take_big(BIG b) { return b.p.x + b.p.y; }
static W make_w(double d) { return (W)d; }
static U make_u(unsigned long long v) { return (U)v; }
static BIG make_big(struct pt p) { return (BIG)p; }

int main(void) {
    /* A union wider than 8 bytes: the operand is stored, not dereferenced. */
    {
        V8 a = (V8)5;
        V16 b = (V16)7L;
        U u = (U)9ULL;
        if (a.i != 5) return 1;
        if (b.l != 7) return 2;
        if (u.d != 9) return 3;
        if (u.s.h != 9) return 4;
    }

    /* The member whose type matches is the one initialized. */
    {
        W w = (W)1.5;
        if (w.d != 1.5) return 10;
        W w2 = (W)3L;
        if (w2.l != 3) return 11;
        F f = (F)2.0f;
        if (f.f != 2.0f) return 12;
        F f2 = (F)7u;
        if (f2.u != 7) return 13;
        double d = 6.5;
        if (((W)d).d != 6.5) return 14;
    }

    /* The other bytes are zero, as for (X){ .c = ... }. */
    {
        X x = (X)(char)-1;
        if (x.i != 255) return 20;
        char c = -2;
        X x2 = (X)c;
        if (x2.i != 254) return 21;
    }

    /* Pointer, struct and decayed-array members. */
    {
        int v = 42;
        int *pv = &v;
        A a = (A)pv;
        if (*a.p != 42) return 30;
        int arr[2] = { 8, 9 };
        A a2 = (A)arr;
        if (a2.p[1] != 9) return 31;
        struct pt p = { 3, 4 };
        SP sp = (SP)p;
        if (sp.p.x != 3 || sp.p.y != 4) return 32;
    }

    /* A union larger than 16 bytes. */
    {
        struct pt p = { 5, 6 };
        BIG b = (BIG)p;
        if (b.p.x != 5 || b.p.y != 6) return 40;
        BIG z;
        memset(&z, 0, sizeof z);
        z.p = p;
        if (memcmp(&b, &z, sizeof b) != 0) return 41;
        BIG b2 = (BIG)1.25;
        if (b2.d != 1.25) return 42;
    }

    /* As an argument and as a return value. */
    {
        if (take_w((W)8.5) != 8.5) return 50;
        struct pt p = { 10, 20 };
        if (take_big((BIG)p) != 30) return 51;
        if (make_w(9.75).d != 9.75) return 52;
        if (make_u(77).s.h != 77) return 53;
        if (make_big(p).p.y != 20) return 54;
    }

    /* Static and nested initializers. */
    {
        if (gw.d != 2.5) return 60;
        if (gx.i != 255) return 61;
        if (gh.tag != 7 || gh.w.d != 4.25 || gh.x.i != 3) return 62;
        struct holder h = { 1, (W)0.5, (X)(char)-1 };
        if (h.w.d != 0.5 || h.x.i != 255) return 63;
        static W sw = (W)-3.0;
        if (sw.d != -3.0) return 64;
    }

    /* Qualifiers on either side do not matter; the operand is read once. */
    {
        const int ci = 11;
        Q q = (Q)ci;
        if (q.ci != 11) return 70;
        volatile float vf = 1.5f;
        Q q2 = (Q)vf;
        if (q2.f != 1.5f) return 71;
        int n = 0;
        V8 a = (V8)n++;
        if (a.i != 0 || n != 1) return 72;
        V8 same = (V8)a;
        if (same.i != 0) return 73;
    }

    /* Constant operands, which an optimizer may fold through. */
    {
        if (((W)1.5).d + ((W)2.5).d != 4.0) return 80;
        if (((U)0x1122334455667788ULL).s.h != 0x1122334455667788UL) return 81;
        if (((X)(char)-1).i != 255) return 82;
        W w = (W)1.5;
        w = (W)2.75;
        if (w.d != 2.75) return 83;
    }
    return 0;
}
"#;
    compile_and_run_everywhere("cast_to_union_mega", code);
}
