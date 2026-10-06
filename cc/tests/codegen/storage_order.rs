//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's `scalar_storage_order` type attribute and pragma: the scalars of a
// struct or union declared big-endian are stored most significant byte
// first on these little-endian targets. Every program here was checked
// against gcc 13 at -O0 and -O2 on x86-64 and aarch64.
//

use crate::common::compile_and_run_everywhere;

/// Every scalar width, float and double in a big-endian struct, a pointer
/// that stays native, a little-endian struct, and a whole-struct copy.
#[test]
fn codegen_storage_order_scalars() {
    const SRC: &str = r#"/* Scalars of every width in a big-endian struct: each member's bytes are
   stored most significant first, and every read gives back what was
   written. A pointer is not a scalar for this attribute and stays native,
   as gcc documents. */
#include <string.h>

enum E { E_VAL = 0x01020304 };

struct __attribute__((scalar_storage_order("big-endian"))) BE {
    unsigned char c;
    short s;
    int i;
    long long ll;
    unsigned u;
    float f;
    double d;
    void *p;
    enum E e;
    _Bool b;
};

struct __attribute__((scalar_storage_order("little-endian"))) LE {
    int i;
    short s;
};

static int bytes_are(const void *at, const unsigned char *want, unsigned n)
{
    return memcmp(at, want, n) == 0;
}

#define AT(obj, m) ((const unsigned char *)&(obj) + __builtin_offsetof(__typeof__(obj), m))

static void fill(struct BE *x)
{
    x->c = 0xab;
    x->s = 0x0102;
    x->i = 0x01020304;
    x->ll = 0x0102030405060708LL;
    x->u = 0xfffffffeu;
    x->f = 1.0f;
    x->d = -2.0;
    x->p = (void *)x;
    x->e = E_VAL;
    x->b = 1;
}

int main(void)
{
    struct BE x;
    memset(&x, 0, sizeof x);
    fill(&x);

    static const unsigned char s_bytes[] = {1, 2};
    static const unsigned char i_bytes[] = {1, 2, 3, 4};
    static const unsigned char ll_bytes[] = {1, 2, 3, 4, 5, 6, 7, 8};
    static const unsigned char u_bytes[] = {0xff, 0xff, 0xff, 0xfe};
    static const unsigned char f_bytes[] = {0x3f, 0x80, 0, 0};
    static const unsigned char d_bytes[] = {0xc0, 0, 0, 0, 0, 0, 0, 0};

    if (!bytes_are(AT(x, c), (const unsigned char[]){0xab}, 1)) return 1;
    if (!bytes_are(AT(x, s), s_bytes, 2)) return 2;
    if (!bytes_are(AT(x, i), i_bytes, 4)) return 3;
    if (!bytes_are(AT(x, ll), ll_bytes, 8)) return 4;
    if (!bytes_are(AT(x, u), u_bytes, 4)) return 5;
    if (!bytes_are(AT(x, f), f_bytes, 4)) return 6;
    if (!bytes_are(AT(x, d), d_bytes, 8)) return 7;
    if (!bytes_are(AT(x, e), i_bytes, 4)) return 8;

    /* The pointer is stored as the target stores it. */
    void *native = (void *)&x;
    if (memcmp(AT(x, p), &native, sizeof native) != 0) return 9;

    if (x.c != 0xab || x.s != 0x0102 || x.i != 0x01020304) return 10;
    if (x.ll != 0x0102030405060708LL || x.u != 0xfffffffeu) return 11;
    if (x.f != 1.0f || x.d != -2.0 || x.p != (void *)&x) return 12;
    if (x.e != E_VAL || x.b != 1) return 13;

    /* Negative values sign-extend from the swapped bytes. */
    x.s = -2;
    x.i = -3;
    if (x.s != -2 || x.i != -3) return 14;
    long widened = x.s;
    if (widened != -2) return 15;

    /* A little-endian struct is the target's own order. */
    struct LE y;
    y.i = 0x01020304;
    y.s = 0x0102;
    int native_i = 0x01020304;
    if (memcmp(&y.i, &native_i, 4) != 0) return 16;
    if (y.i != 0x01020304 || y.s != 0x0102) return 17;

    /* A copy of the whole struct is a copy of its bytes. */
    struct BE z = x;
    if (memcmp(&z, &x, sizeof x) != 0) return 18;
    if (z.i != -3 || z.d != -2.0) return 19;
    return 0;
}
"#;
    compile_and_run_everywhere("storage_order_scalars", SRC);
}

/// Bit-fields allocated from the most significant bit, checked against
/// gcc's byte images, through aligned, straddling and packed carriers, and
/// read-modify-written without disturbing their neighbours.
#[test]
fn codegen_storage_order_bitfields() {
    const SRC: &str = r#"/* Bit-fields of a big-endian struct are allocated from the most significant
   bit of the first byte, as on a big-endian target: the byte images below
   are gcc's. Reads, stores, increments and compound assignments each touch
   only their own field's bits. */
#include <string.h>

#define BE __attribute__((scalar_storage_order("big-endian")))
struct A { short i : 12; signed char c1 : 1, c2 : 1, c3 : 1, c4 : 1; } BE;
struct B { int i : 24; char c1 : 1, c2 : 1, c3 : 1, c4 : 1, c5 : 1, c6 : 1, c7 : 1, c8 : 1; } BE;
struct C {
    unsigned a : 3;
    unsigned b : 7;
    unsigned short c : 9;
    unsigned long long d : 40;
    int e : 5;
} BE;
struct __attribute__((packed)) D {
    char x;
    unsigned a : 13;
    unsigned long long b : 50;
    unsigned char y;
} BE;
struct E { unsigned long long f0 : 29, f1 : 4, f2 : 31; } BE;

static const unsigned char a_img[] = {0x15, 0x5a};
static const unsigned char b_img[] = {0x12, 0x34, 0x56, 0x41};
static const unsigned char c_img[] = {0xb5, 0x40, 0xd2, 0x80, 0x00, 0x00, 0x00, 0x00,
                                      0x12, 0x34, 0x56, 0x78, 0x9a, 0xe8, 0x00, 0x00};
static const unsigned char d_img[] = {0x7f, 0xd5, 0xe4, 0x24, 0x68,
                                      0xac, 0xf1, 0x35, 0x78, 0xee};
static const unsigned char e_img[] = {0x00, 0x00, 0x00, 0xbc, 0x87, 0x65, 0x43, 0x21};

int main(void)
{
    struct A a;
    memset(&a, 0, sizeof a);
    a.i = 341;
    a.c1 = 1;
    a.c3 = 1;
    if (sizeof a != sizeof a_img || memcmp(&a, a_img, sizeof a) != 0) return 1;
    if (a.i != 341 || a.c1 != -1 || a.c2 != 0 || a.c3 != -1 || a.c4 != 0) return 2;

    struct B b;
    memset(&b, 0, sizeof b);
    b.i = 1193046;
    b.c2 = 1;
    b.c8 = 1;
    if (memcmp(&b, b_img, sizeof b) != 0) return 3;
    if (b.i != 1193046 || !b.c2 || !b.c8 || b.c1 || b.c7) return 4;

    struct C c;
    memset(&c, 0, sizeof c);
    c.a = 5;
    c.b = 0x55;
    c.c = 0x1a5;
    c.d = 0x123456789aULL;
    c.e = -3;
    if (sizeof c != sizeof c_img || memcmp(&c, c_img, sizeof c) != 0) return 5;
    if (c.a != 5 || c.b != 0x55 || c.c != 0x1a5 || c.d != 0x123456789aULL || c.e != -3)
        return 6;

    struct D d;
    memset(&d, 0, sizeof d);
    d.x = 0x7f;
    d.a = 0x1abc;
    d.b = 0x2123456789abcULL;
    d.y = 0xee;
    if (sizeof d != sizeof d_img || memcmp(&d, d_img, sizeof d) != 0) return 7;
    if (d.x != 0x7f || d.a != 0x1abc || d.b != 0x2123456789abcULL || d.y != 0xee) return 8;

    struct E e;
    memset(&e, 0, sizeof e);
    e.f0 = 23;
    e.f1 = 9;
    e.f2 = 0x7654321;
    if (memcmp(&e, e_img, sizeof e) != 0) return 9;
    if (e.f0 != 23 || e.f1 != 9 || e.f2 != 0x7654321) return 10;

    /* Read-modify-write leaves the neighbours alone. */
    c.b++;
    c.c += 2;
    --c.e;
    c.d *= 2;
    c.a ^= 7;
    if (c.a != 2 || c.b != 0x56 || c.c != 0x1a7 || c.e != -4) return 11;
    if (c.d != 0x2468acf134ULL) return 12;
    d.a -= 0xbc;
    d.b >>= 4;
    if (d.x != 0x7f || d.a != 0x1a00 || d.b != 0x2123456789abULL || d.y != 0xee) return 13;
    e.f1 = e.f1 + 6;
    if (e.f0 != 23 || e.f1 != 15 || e.f2 != 0x7654321) return 14;
    int old = a.i++;
    if (old != 341 || a.i != 342 || a.c1 != -1 || a.c3 != -1) return 15;
    return 0;
}
"#;
    compile_and_run_everywhere("storage_order_bitfields", SRC);
}

/// Arrays take the struct's order, nested structs keep their own, a union
/// takes the attribute, and so does a typedef of an existing struct.
#[test]
fn codegen_storage_order_nesting() {
    const SRC: &str = r#"/* What the order reaches. An array of scalars takes its struct's order, and
   so does an element reached through the array decayed in the same
   expression. A struct member keeps the order of its own type: a native
   struct nested in a big-endian one is native, and a big-endian struct
   nested in a native one is big-endian. Unions take the attribute too, and
   so does a typedef of an existing struct. */
#include <string.h>

#define BE __attribute__((scalar_storage_order("big-endian")))

struct Native { int v; };
struct BE Big { int v; short w; };

struct BE Outer {
    int arr[3];
    short grid[2][2];
    struct Native native;
    struct Big big;
    struct { int anon_v; } anon;
    struct Native natives[2];
};

struct Holder {
    int plain;
    struct Big big;
};

/* A typedef can name a struct in the other order: gcc's variant type, laid
   out as the struct is and stored big-endian. */
typedef struct Native BE NativeBE;

union BE U {
    unsigned int word;
    unsigned short half[2];
    unsigned char byte[4];
};

static const unsigned char be4[] = {1, 2, 3, 4};

static int native4(const void *at)
{
    int v = 0x01020304;
    return memcmp(at, &v, 4) == 0;
}

static int sum(struct Outer *o)
{
    int s = 0;
    for (int k = 0; k < 3; k++)
        s += o->arr[k];
    return s;
}

int main(void)
{
    struct Outer o;
    memset(&o, 0, sizeof o);
    o.arr[1] = 0x01020304;
    if (memcmp(&o.arr, (const unsigned char[]){0, 0, 0, 0, 1, 2, 3, 4}, 8) != 0) return 1;
    *(o.arr + 2) = 0x01020304;
    if (memcmp((char *)&o.arr + 8, be4, 4) != 0) return 2;
    if (o.arr[2] != 0x01020304 || *(o.arr + 1) != 0x01020304) return 3;
    o.arr[0] = 5;
    if (sum(&o) != 5 + 2 * 0x01020304) return 4;

    o.grid[1][0] = 0x0102;
    if (memcmp((char *)&o.grid + 4, be4, 2) != 0) return 5;
    if (o.grid[1][0] != 0x0102 || o.grid[0][1] != 0) return 6;

    o.native.v = 0x01020304;
    if (!native4(&o.native)) return 7;
    o.big.v = 0x01020304;
    if (memcmp(&o.big, be4, 4) != 0) return 8;
    o.anon.anon_v = 0x01020304;
    if (!native4(&o.anon)) return 9;
    o.natives[1].v = 0x01020304;
    if (!native4(&o.natives[1])) return 10;
    if (o.native.v != 0x01020304 || o.big.v != 0x01020304) return 11;
    if (o.anon.anon_v != 0x01020304 || o.natives[1].v != 0x01020304) return 12;

    /* A big-endian struct member of a native struct is big-endian, and
       reached through a pointer to the member struct it is too. */
    struct Holder h;
    h.plain = 0x01020304;
    h.big.v = 0x01020304;
    h.big.w = 0x0304;
    if (!native4(&h.plain)) return 13;
    if (memcmp(&h.big, (const unsigned char[]){1, 2, 3, 4, 3, 4}, 6) != 0) return 14;
    struct Big *pb = &h.big;
    pb->v += 1;
    if (h.big.v != 0x01020305 || pb->w != 0x0304) return 15;

    NativeBE nb;
    nb.v = 0x01020304;
    if (memcmp(&nb, be4, 4) != 0) return 19;
    if (nb.v != 0x01020304 || sizeof nb != sizeof(struct Native)) return 20;

    union U u;
    u.word = 0x01020304;
    if (memcmp(&u, be4, 4) != 0) return 16;
    if (u.half[0] != 0x0102 || u.half[1] != 0x0304) return 17;
    if (u.byte[0] != 1 || u.byte[3] != 4) return 18;
    return 0;
}
"#;
    compile_and_run_everywhere("storage_order_nesting", SRC);
}

/// Static and automatic initializers write each scalar and bit-field in the
/// struct's order.
#[test]
fn codegen_storage_order_initializers() {
    const SRC: &str = r#"/* Initializers: the image of a static object holds each scalar in its
   struct's order, bit-fields included, and an automatic object initialized
   by a list holds the same bytes. */
#include <string.h>

#define BE __attribute__((scalar_storage_order("big-endian")))

struct Native { int v; };

struct BE S {
    unsigned char c;
    short s;
    int i;
    long long ll;
    float f;
    double d;
    unsigned bf1 : 4, bf2 : 12;
    short arr[2];
    struct Native native;
};

static const unsigned char image[] = {
    0x7a, 0x00, 0x01, 0x02,                         /* c, pad, s */
    0x01, 0x02, 0x03, 0x04,                         /* i */
    0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, /* ll */
    0x3f, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* f, pad */
    0x40, 0x09, 0x21, 0xfb, 0x54, 0x44, 0x2d, 0x18, /* d */
    0x9a, 0xbc, 0x00, 0x05, 0xff, 0xfa, 0x00, 0x00, /* bf1, bf2, arr, pad */
};

struct S global = {0x7a, 0x0102, 0x01020304, 0x0102030405060708LL, 1.0f,
                   3.141592653589793, 9, 0xabc, {5, -6}, {0x01020304}};
static const struct S constant = {.ll = 0x0102030405060708LL, .s = 0x0102,
                                  .c = 0x7a, .i = 0x01020304, .f = 1.0f,
                                  .d = 3.141592653589793, .bf2 = 0xabc, .bf1 = 9,
                                  .arr = {5, -6}, .native = {0x01020304}};

static int check(const struct S *p)
{
    const unsigned char *b = (const unsigned char *)p;
    if (memcmp(b, image, sizeof image) != 0) return 1;
    int v = 0x01020304;
    if (memcmp(b + __builtin_offsetof(struct S, native), &v, 4) != 0) return 2;
    if (p->c != 0x7a || p->s != 0x0102 || p->i != 0x01020304) return 3;
    if (p->ll != 0x0102030405060708LL || p->f != 1.0f || p->d != 3.141592653589793)
        return 4;
    if (p->bf1 != 9 || p->bf2 != 0xabc || p->arr[0] != 5 || p->arr[1] != -6) return 5;
    if (p->native.v != 0x01020304) return 6;
    return 0;
}

int main(void)
{
    int r;
    if (sizeof(struct S) != sizeof image + 8) return 50;
    if ((r = check(&global)) != 0) return 10 + r;
    if ((r = check(&constant)) != 0) return 20 + r;
    /* Read straight from the constant, where the value may be folded. */
    if (constant.i != 0x01020304 || constant.arr[1] != -6 || constant.bf2 != 0xabc) return 30;
    struct S local = {0x7a, 0x0102, 0x01020304, 0x0102030405060708LL, 1.0f,
                      3.141592653589793, 9, 0xabc, {5, -6}, {0x01020304}};
    if ((r = check(&local)) != 0) return 40 + r;
    return 0;
}
"#;
    compile_and_run_everywhere("storage_order_initializers", SRC);
}

/// Compound assignment, `++` and `--`, a 16-byte integer and a complex
/// member.
#[test]
fn codegen_storage_order_compound_assignment() {
    const SRC: &str = r#"/* Compound assignment, increment and decrement read the member in its
   order and write it back in it, and the value of the expression is the
   native one. Also a 16-byte integer and a complex member, whose halves are
   each in the struct's order. */
#include <string.h>

struct __attribute__((scalar_storage_order("big-endian"))) S {
    short s;
    int i;
    unsigned long long u;
    float f;
    double d;
    __int128 w;
    _Complex double z;
};

static int be(const void *at, const unsigned char *want, unsigned n)
{
    return memcmp(at, want, n) == 0;
}

int main(void)
{
    struct S x;
    memset(&x, 0, sizeof x);
    x.s = 0x00ff;
    x.i = 0x01020300;
    x.u = 0xfeULL;
    x.f = 1.5f;
    x.d = 0.25;

    if (++x.s != 0x0100) return 1;
    if (x.s-- != 0x0100 || x.s != 0x00ff) return 2;
    x.i += 4;
    if (x.i != 0x01020304) return 3;
    if (!be((char *)&x + __builtin_offsetof(struct S, i),
            (const unsigned char[]){1, 2, 3, 4}, 4))
        return 4;
    x.i <<= 8;
    if (x.i != 0x02030400) return 5;
    x.u++;
    x.u |= 0x0100000000000000ULL;
    if (x.u != 0x01000000000000ffULL) return 6;
    if (!be((char *)&x + __builtin_offsetof(struct S, u),
            (const unsigned char[]){1, 0, 0, 0, 0, 0, 0, 0xff}, 8))
        return 7;
    x.f *= 2;
    x.d -= 0.5;
    if (x.f != 3.0f || x.d != -0.25) return 8;
    if (!be((char *)&x + __builtin_offsetof(struct S, f), (const unsigned char[]){0x40, 0x40, 0, 0}, 4))
        return 9;
    x.f++;
    if (x.f != 4.0f) return 10;

    x.w = ((__int128)0x0102030405060708LL << 64) | 0x090a0b0c0d0e0f10LL;
    if (!be((char *)&x + __builtin_offsetof(struct S, w),
            (const unsigned char[]){1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16}, 16))
        return 11;
    x.w += 0xf0;
    if ((long long)(x.w >> 64) != 0x0102030405060708LL || (long long)x.w != 0x090a0b0c0d0e1000LL)
        return 12;

    x.z = 1.0 + 2.0i;
    if (!be((char *)&x + __builtin_offsetof(struct S, z),
            (const unsigned char[]){0x3f, 0xf0, 0, 0, 0, 0, 0, 0, 0x40, 0, 0, 0, 0, 0, 0, 0}, 16))
        return 13;
    x.z *= 2.0;
    if (__real__ x.z != 2.0 || __imag__ x.z != 4.0) return 14;
    _Complex double copy = x.z;
    if (__imag__ copy != 4.0) return 15;
    return 0;
}
"#;
    compile_and_run_everywhere("storage_order_compound", SRC);
}

/// `#pragma scalar_storage_order` and its interaction with the attribute.
#[test]
fn codegen_storage_order_pragma() {
    const SRC: &str = r#"/* `#pragma scalar_storage_order` sets the order of the structs and unions
   defined after it, until `default` restores the target's; an attribute on
   the struct still decides for that struct. */
#include <string.h>

#pragma scalar_storage_order big-endian
struct P { int v; struct Inner { short h; } in; };
union PU { unsigned w; };
struct __attribute__((scalar_storage_order("little-endian"))) Overridden { int v; };
#pragma scalar_storage_order little-endian
struct L { int v; };
#pragma scalar_storage_order big-endian
struct B2 { int v; };
#pragma scalar_storage_order default
struct N { int v; };

static const unsigned char be4[] = {1, 2, 3, 4};

static int native4(const void *at)
{
    int v = 0x01020304;
    return memcmp(at, &v, 4) == 0;
}

int main(void)
{
    struct P p;
    p.v = 0x01020304;
    p.in.h = 0x0102;
    if (memcmp(&p, be4, 4) != 0) return 1;
    if (memcmp(&p.in, be4, 2) != 0) return 2;
    if (p.v != 0x01020304 || p.in.h != 0x0102) return 3;

    union PU u;
    u.w = 0x01020304;
    if (memcmp(&u, be4, 4) != 0) return 4;

    struct Overridden o;
    o.v = 0x01020304;
    if (!native4(&o)) return 5;

    struct L l;
    l.v = 0x01020304;
    if (!native4(&l)) return 6;

    struct B2 b;
    b.v = 0x01020304;
    if (memcmp(&b, be4, 4) != 0) return 7;

    struct N n;
    n.v = 0x01020304;
    if (!native4(&n)) return 8;
    return 0;
}
"#;
    compile_and_run_everywhere("storage_order_pragma", SRC);
}
