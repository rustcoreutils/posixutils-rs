//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Aggregates in the calling convention: structs, unions and HFAs
// passed and returned, in registers and in memory.
//

use crate::codegen::asm_probe::{asm_for_with, body_of, AARCH64_LINUX, X86_64_LINUX};
use crate::common::{
    compile_and_run, compile_and_run_aarch64, compile_and_run_everywhere, compile_and_run_optimized,
};

/// Test: function returning pointer/long with no explicit return gets 64-bit zero
#[test]
fn codegen_default_return_64bit() {
    let code = r#"
// Function returning pointer without explicit return
void *get_null(int flag) {
    if (flag) {
        return (void*)0;
    }
    // implicit return should be 64-bit zero, not 32-bit
}

// Function returning long without explicit return
long get_long_default(int flag) {
    if (flag) {
        return 42L;
    }
    // implicit return should be 64-bit zero
}

int main(void) {
    // Section 1: pointer default return
    void *p = get_null(0);
    if (p != (void*)0) return 1;

    // Section 2: long default return
    long l = get_long_default(0);
    if (l != 0L) return 2;

    // Section 3: explicit returns still work
    void *p2 = get_null(1);
    if (p2 != (void*)0) return 3;
    long l2 = get_long_default(1);
    if (l2 != 42L) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("default_return_64bit", code, &[]), 0);
}

/// Test: function returning pointer via call — return value is full 64 bits
#[test]
fn codegen_call_return_pointer() {
    let code = r#"
void *identity(void *p) {
    return p;
}

long return_long(long v) {
    return v;
}

int main(void) {
    // Section 1: pointer return value preserved through call
    long stack_var = 12345;
    void *p = &stack_var;
    void *q = identity(p);
    if (p != q) return 1;
    if (*(long*)q != 12345) return 2;

    // Section 2: long return value preserved through call
    long big = 0x123456789ABCDEF0L;
    long result = return_long(big);
    if (result != big) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("call_return_pointer", code, &[]), 0);
}

/// Test: emit_ret with 64-bit integer return value
#[test]
fn codegen_ret_64bit() {
    let code = r#"
long return_big(void) {
    return 0x123456789ABCDEF0L;
}

void *return_ptr(void *p) {
    return p;
}

unsigned long return_unsigned(void) {
    return 0xFFFFFFFFFFFFFFFFUL;
}

int main(void) {
    // Section 1: long return
    long v = return_big();
    if (v != 0x123456789ABCDEF0L) return 1;

    // Section 2: pointer return
    int x = 42;
    void *p = return_ptr(&x);
    if (*(int*)p != 42) return 2;

    // Section 3: unsigned long return
    unsigned long u = return_unsigned();
    if (u != 0xFFFFFFFFFFFFFFFFUL) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("ret_64bit", code, &[]), 0);
}

/// Regression: two-register integer struct return clobbered rax when
/// second source was allocated there (parallel-move problem).
#[test]
fn codegen_two_reg_int_struct_return() {
    let code = r#"
#include <stdint.h>

typedef struct { uint64_t low; uint64_t high; } u128_t;

static u128_t make_u128(uint64_t lo, uint64_t hi) {
    u128_t r;
    r.low = lo;
    r.high = hi;
    return r;
}

static u128_t add_u128(u128_t a, u128_t b) {
    u128_t r;
    r.low = a.low + b.low;
    uint64_t carry = (r.low < a.low) ? 1 : 0;
    r.high = a.high + b.high + carry;
    return r;
}

int main(void) {
    u128_t a = make_u128(100, 0);
    if (a.low != 100 || a.high != 0) return 1;

    u128_t b = make_u128(200, 0);
    if (b.low != 200 || b.high != 0) return 2;

    u128_t c = add_u128(a, b);
    if (c.low != 300 || c.high != 0) return 3;

    /* Test with carry */
    a = make_u128(0xFFFFFFFFFFFFFFFFULL, 0);
    b = make_u128(1, 0);
    c = add_u128(a, b);
    if (c.low != 0 || c.high != 1) return 4;

    /* Test chained calls — rvalue struct as call argument */
    u128_t total = make_u128(0, 0);
    for (int i = 0; i < 10; i++) {
        total = add_u128(total, make_u128(i, 0));
    }
    if (total.low != 45 || total.high != 0) return 5;

    /* Mixed values */
    c = add_u128(make_u128(7, 3), make_u128(5, 2));
    if (c.low != 12 || c.high != 5) return 6;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_two_reg_int_struct_return", code, &[]),
        0
    );
}

/// Regression test: small struct (<=64 bits) returned by value from a function
/// was stored as a raw register value. When assigned to a struct variable,
/// emit_assign's block_copy dereferenced it as a pointer → SIGSEGV.
/// Fixed by allocating local storage for small struct returns.
#[test]
fn codegen_small_struct_return() {
    let code = r#"
typedef struct { int a; int b; } Pair;

__attribute__((noinline))
Pair make_pair(int x, int y) {
    Pair p;
    p.a = x;
    p.b = y;
    return p;
}

__attribute__((noinline))
int sum_pair(Pair p) {
    return p.a + p.b;
}

int main(void) {
    /* Basic: return small struct and access fields */
    Pair p = make_pair(10, 20);
    if (p.a != 10) return 1;
    if (p.b != 20) return 2;

    /* Pass returned struct to another function */
    int s = sum_pair(make_pair(3, 7));
    if (s != 10) return 3;

    /* Assign return value to existing variable */
    Pair q;
    q = make_pair(100, 200);
    if (q.a != 100 || q.b != 200) return 4;

    /* Chain: use result in expression */
    Pair r = make_pair(make_pair(1, 2).a + 3, make_pair(4, 5).b + 6);
    if (r.a != 4 || r.b != 11) return 5;

    /* Single-field struct (common in error handling) */
    typedef struct { int err; } ErrCode;
    ErrCode e;
    e = (ErrCode){42};
    if (e.err != 42) return 6;

    /* Struct with two shorts (fits in single register) */
    typedef struct { short x; short y; } Point;
    Point pt;
    pt = (Point){100, 200};
    if (pt.x != 100 || pt.y != 200) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_small_struct_return", code, &[]), 0);
}

// Regression: do not fold a conditional branch whose target block does
// observable work before its noreturn-call terminator.
//
// Triggered originally by CPython's fork+spawn helper, which has the form
// `if (param != GLOBAL) { side_effect(); } work(); _exit(...);` — c17's
// DCE used to treat the whole block as "trivially unreachable" because the
// linearizer emits `Unreachable` after `_exit`, and then folded the cbr to
// an unconditional branch into the `side_effect()` arm.
#[test]
fn codegen_cbr_to_noreturn_call_not_folded() {
    let code = r#"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static int marker_target;
static void *MARK = &marker_target;

__attribute__((noinline))
static int run(void *cond_ptr) {
    if (cond_ptr != MARK) {
        fputs("BUG\n", stdout);
        fflush(stdout);
        exit(1);
    }
    fputs("OK\n", stdout);
    fflush(stdout);
    _exit(0);
    return 0;
}

int main(void) {
    run(MARK);
    return 99;
}
"#;
    assert_eq!(
        compile_and_run_optimized("cbr_to_noreturn_call_not_folded", code),
        0,
        "DCE folded a conditional branch whose target ended in a noreturn call + Unreachable, dropping the call"
    );
}

/// A register-returned aggregate must be stored through the frame's own base
/// register, not through `%rbp`.
///
/// When a local's alignment exceeds the stack's, the prologue realigns `%rsp`
/// and keeps the frame's base in a second register; `stack_mem` then addresses
/// every local relative to that. Six sites in the call path spelled
/// `-(slot + callee_saved_offset)(%rbp)` by hand instead, so under such a
/// frame the return value was written to an address nothing reads back --
/// `movq %rax, -112(%rbp)` followed by `movq 32(%rbx), %rax`. Four of the six
/// were return paths, which is why `struct S { long long a, b; } r = make();`
/// came back as zeros beside an `_Alignas(32)` local, in ordinary C with no
/// varargs involved.
///
/// Whether it is *visible* depends on what happens to occupy the address that
/// is written, so the levels are swept rather than trusted: the two-register
/// integer shape came back wrong at `-O1` and right at `-O0` and `-O2`.
#[test]
fn codegen_aggregate_return_into_an_over_aligned_frame() {
    let code = r#"
struct TwoInt  { long long a, b; };            /* RAX + RDX   */
struct TwoSse  { double a, b; };               /* XMM0 + XMM1 */
struct Mixed   { double a; long long b; };     /* XMM0 + RAX  */
struct MixedR  { long long a; double b; };     /* RAX + XMM0  */
struct OneInt  { int a; };                     /* RAX         */
struct OneSse  { float a, b; };                /* XMM0        */

__attribute__((noinline)) struct TwoInt m_ti(void){ struct TwoInt s={1,2}; return s; }
__attribute__((noinline)) struct TwoSse m_ts(void){ struct TwoSse s={1.5,2.5}; return s; }
__attribute__((noinline)) struct Mixed  m_mx(void){ struct Mixed  s={3.5,4}; return s; }
__attribute__((noinline)) struct MixedR m_mr(void){ struct MixedR s={5,6.5}; return s; }
__attribute__((noinline)) struct OneInt m_oi(void){ struct OneInt s={7}; return s; }
__attribute__((noinline)) struct OneSse m_os(void){ struct OneSse s={8.5f,9.5f}; return s; }
__attribute__((noinline)) double _Complex m_cd(void){ return __builtin_complex(10.5, 11.5); }
__attribute__((noinline)) float  _Complex m_cf(void){ return __builtin_complex(12.5f, 13.5f); }

int main(void)
{
    _Alignas(32) char pad[64];          /* forces the over-aligned frame */

    struct TwoInt ti = m_ti();
    struct TwoSse ts = m_ts();
    struct Mixed  mx = m_mx();
    struct MixedR mr = m_mr();
    struct OneInt oi = m_oi();
    struct OneSse os = m_os();
    double _Complex cd = m_cd();
    float  _Complex cf = m_cf();

    pad[0] = 1;
    pad[63] = 2;

    if (ti.a != 1 || ti.b != 2)                       return 1;
    if (ts.a != 1.5 || ts.b != 2.5)                   return 2;
    if (mx.a != 3.5 || mx.b != 4)                     return 3;
    if (mr.a != 5 || mr.b != 6.5)                     return 4;
    if (oi.a != 7)                                    return 5;
    if (os.a != 8.5f || os.b != 9.5f)                 return 6;
    if (__real__ cd != 10.5 || __imag__ cd != 11.5)   return 7;
    if (__real__ cf != 12.5f || __imag__ cf != 13.5f) return 8;
    if (pad[0] != 1 || pad[63] != 2)                  return 9;
    return 0;
}
"#;
    for opt in ["-O0", "-O1", "-O2", "-Os"] {
        assert_eq!(
            compile_and_run(
                &format!("codegen_agg_ret_over_aligned{opt}"),
                code,
                &[opt.to_string()]
            ),
            0,
            "at {opt}"
        );
    }

    // The behavioural check above only fails when the address written happens
    // to matter, so pin the property itself: in a function whose frame is
    // realigned, no aggregate-return store may name `%rbp`.
    let probe = r#"
struct TwoInt { long long a, b; };
struct TwoInt make(void);
void use(char *);
long realigned(void)
{
    _Alignas(32) char pad[64];   /* escapes, so it keeps its slot */
    struct TwoInt r = make();
    use(pad);
    return r.a + r.b + pad[0];
}
"#;
    let asm = asm_for_with("agg_ret_base_reg", X86_64_LINUX, probe, &["-O1"]);
    let body = body_of(&asm, "realigned");
    assert!(
        body.contains("andq $-32, %rsp"),
        "the frame is realigned:\n{body}"
    );
    for reg in ["%rax", "%rdx"] {
        for line in body.lines() {
            let line = line.trim();
            if line.starts_with(&format!("movq {reg}, ")) && line.contains("(%rbp)") {
                panic!(
                    "the aggregate-return store must go through the realigned \
                     frame base, not %rbp: `{line}`\n{body}"
                );
            }
        }
    }
}

/// AAPCS64 §5.4.2 gives a homogeneous floating-point aggregate one V register
/// per element, for one to four elements. Both sides of the aarch64 call
/// recognised only the two-element case: a three- or four-element HFA went out
/// whole in a general register while the callee read it from V0-V3, and it
/// consumed an integer slot, so the *next* integer argument was shifted along
/// as well. An aggregate small enough to sit in one register was passed as a
/// single value rather than split across the element registers.
///
/// Every shape below is checked against gcc's own layout, so the test is an
/// ABI conformance check as much as a regression test.
#[test]
fn codegen_hfa_param_element_counts() {
    let code = r#"
typedef struct { float a; }              F1;
typedef struct { float a, b; }           F2;
typedef struct { float a, b, c; }        F3;
typedef struct { float a, b, c, d; }     F4;
typedef struct { double a; }             D1;
typedef struct { double a, b; }          D2;
typedef struct { double a, b, c; }       D3;
typedef struct { float v[3]; }           FA;

double u_f1(F1 v) { return v.a; }
double u_f2(F2 v) { return v.a * 10 + v.b; }
double u_f3(F3 v) { return v.a * 100 + v.b * 10 + v.c; }
double u_f4(F4 v) { return v.a * 1000 + v.b * 100 + v.c * 10 + v.d; }
double u_d1(D1 v) { return v.a; }
double u_d2(D2 v) { return v.a * 10 + v.b; }
double u_d3(D3 v) { return v.a * 100 + v.b * 10 + v.c; }
double u_fa(FA v) { return v.v[0] * 100 + v.v[1] * 10 + v.v[2]; }

/* an HFA followed by an integer: the integer moves too if the aggregate
   wrongly consumes a general register */
double mixed(int n, F4 v, int m) {
    return v.a * 1000 + v.b * 100 + v.c * 10 + v.d + n * 100000 + m * 10000;
}

/* past V0-V7, so the tail is laid on the stack */
double overflow(F4 p, F4 q, D2 r, int n, double s) {
    return p.a * 1e7 + p.b * 1e6 + p.c * 1e5 + p.d * 1e4
         + q.a * 1e3 + q.b * 1e2 + q.c * 10 + q.d
         + r.a * 1e8 + r.b * 1e9 + n + s;
}

int main(void) {
    F1 f1 = {1};       if (u_f1(f1) != 1)    return 1;
    F2 f2 = {1, 2};    if (u_f2(f2) != 12)   return 2;
    F3 f3 = {1, 2, 3}; if (u_f3(f3) != 123)  return 3;
    F4 f4 = {1,2,3,4}; if (u_f4(f4) != 1234) return 4;
    D1 d1 = {1};       if (u_d1(d1) != 1)    return 5;
    D2 d2 = {1, 2};    if (u_d2(d2) != 12)   return 6;
    D3 d3 = {1, 2, 3}; if (u_d3(d3) != 123)  return 7;
    FA fa = {{1, 2, 3}}; if (u_fa(fa) != 123) return 8;

    if (mixed(1, f4, 2) != 121234) return 9;

    F4 q = {5, 6, 7, 8};
    D2 r = {9, 10};
    if (overflow(f4, q, r, 11, 0.5) != 10912345689.5) return 10;
    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_hfa_param_counts", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("codegen_hfa_param_counts_opt", code),
        0
    );
}

/// An HFA is returned in one V register per element, up to four, and its size
/// does not enter into it -- `struct { double a, b, c, d; }` is thirty-two
/// bytes and still comes back in V0-V3.
///
/// Three things had to agree before that worked on aarch64. The linearizer
/// claimed every aggregate over sixteen bytes for the hidden-pointer return,
/// on the stated grounds that nothing implemented a three-register HFA return.
/// The return emitter handled one element and two, so three or four fell
/// through and sent back the address of the callee's own frame slot -- which
/// the caller then dereferenced after the frame was gone. And an aggregate
/// returned in registers is written into a local by the caller, but that local
/// was only allocated at sixteen bytes or fewer, so a twenty-four-byte result
/// had nowhere to land and its first use read through a zero.
#[test]
fn codegen_hfa_return_element_counts() {
    let code = r#"
typedef struct { float a; }              F1;
typedef struct { float a, b; }           F2;
typedef struct { float a, b, c; }        F3;
typedef struct { float a, b, c, d; }     F4;
typedef struct { double a; }             D1;
typedef struct { double a, b; }          D2;
typedef struct { double a, b, c; }       D3;
typedef struct { double a, b, c, d; }    D4;

F1 m_f1(float x) { F1 r = {x};                   return r; }
F2 m_f2(float x) { F2 r = {x, x+1};              return r; }
F3 m_f3(float x) { F3 r = {x, x+1, x+2};         return r; }
F4 m_f4(float x) { F4 r = {x, x+1, x+2, x+3};    return r; }
D1 m_d1(double x){ D1 r = {x};                   return r; }
D2 m_d2(double x){ D2 r = {x, x+1};              return r; }
D3 m_d3(double x){ D3 r = {x, x+1, x+2};         return r; }
D4 m_d4(double x){ D4 r = {x, x+1, x+2, x+3};    return r; }

int main(void) {
    { F1 v = m_f1(1); if (v.a != 1) return 1; }
    { F2 v = m_f2(1); if (v.a*10 + v.b != 12) return 2; }
    { F3 v = m_f3(1); if (v.a*100 + v.b*10 + v.c != 123) return 3; }
    { F4 v = m_f4(1); if (v.a*1000 + v.b*100 + v.c*10 + v.d != 1234) return 4; }
    { D1 v = m_d1(1); if (v.a != 1) return 5; }
    { D2 v = m_d2(1); if (v.a*10 + v.b != 12) return 6; }
    { D3 v = m_d3(1); if (v.a*100 + v.b*10 + v.c != 123) return 7; }
    { D4 v = m_d4(1); if (v.a*1000 + v.b*100 + v.c*10 + v.d != 1234) return 8; }

    /* returned aggregate consumed in place, not through a named local */
    if (m_d3(2).b != 3) return 9;
    if (m_f4(2).d != 5) return 10;
    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_hfa_return_counts", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("codegen_hfa_return_counts_opt", code),
        0
    );
}

/// AAPCS64 §5.4.2 C.10 and SysV AMD64 §3.2.3 both put a composite of at most
/// sixteen bytes in two consecutive general registers. aarch64 handed over its
/// *address* instead, on both sides of the call, so c17 agreed with itself and
/// nothing in the suite noticed -- while every call across a c17/gcc boundary
/// was wrong. With a gcc caller it segfaulted: the callee read the first eight
/// bytes of the aggregate as if they were a pointer and dereferenced them.
///
/// Structs of four and eight bytes were already right; the broken range is
/// exactly the two-eightbyte one.
#[test]
fn codegen_composite_in_two_registers() {
    let code = r#"
typedef struct { int a; }              S4;
typedef struct { long a; }             S8;
typedef struct { int a, b, c; }        S12;
typedef struct { long a, b; }          S16;
typedef struct { int a; long b; }      M16;   /* mixed widths */
typedef struct { long a, b, c; }       S24;   /* over 16: still by pointer */

__attribute__((noinline)) long f4(S4 v)   { return v.a; }
__attribute__((noinline)) long f8(S8 v)   { return v.a; }
__attribute__((noinline)) long f12(S12 v) { return v.a * 100 + v.b * 10 + v.c; }
__attribute__((noinline)) long f16(S16 v) { return v.a * 10 + v.b; }
__attribute__((noinline)) long m16(M16 v) { return v.a * 10 + v.b; }
__attribute__((noinline)) long f24(S24 v) { return v.a * 100 + v.b * 10 + v.c; }

/* an integer argument after the composite: if the composite claims the wrong
   number of registers, this one moves too */
__attribute__((noinline)) long tail(int n, S16 v, int m) {
    return n * 10000 + v.a * 100 + v.b * 10 + m;
}
/* enough composites to run past X0-X7 and onto the stack */
__attribute__((noinline)) long many(S16 a, S16 b, S16 c, S16 d, S16 e) {
    return a.a + b.a * 10 + c.a * 100 + d.a * 1000 + e.a * 10000 + e.b * 100000;
}

int main(void) {
    S4 s4 = {1};
    S8 s8 = {2};
    S12 s12 = {1, 2, 3};
    S16 s16 = {1, 2};
    M16 m = {1, 2};
    S24 s24 = {1, 2, 3};

    if (f4(s4) != 1) return 1;
    if (f8(s8) != 2) return 2;
    if (f12(s12) != 123) return 3;
    if (f16(s16) != 12) return 4;
    if (m16(m) != 12) return 5;
    if (f24(s24) != 123) return 6;
    if (tail(9, s16, 7) != 90127) return 7;

    S16 a = {1, 0}, b = {2, 0}, c = {3, 0}, d = {4, 0}, e = {5, 6};
    if (many(a, b, c, d, e) != 654321) return 8;
    return 0;
}
"#;
    assert_eq!(compile_and_run("codegen_composite_two_regs", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("codegen_composite_two_regs_opt", code),
        0
    );
}

/// A struct whose alignment exceeds the argument area's own, passed by value.
///
/// `IncomingOff::take` rounded the *frame displacement* up to the argument's
/// alignment, but that displacement already carries the saved `%rbp` and the
/// return address. So an argument wanting 32-byte alignment and arriving first
/// went to `32(%rbp)` while the caller -- which measures from its outgoing
/// area's own base, correctly -- had written it at `16(%rbp)`. The callee read
/// the struct's second half and ran off its end.
///
/// Invisible below 32-byte alignment: 16 is already a multiple of 8 and of 16,
/// so only an argument wanting more than the area's own alignment can tell the
/// two bases apart. The ordinary-alignment cases are here so the fix cannot
/// regress them.
///
/// Six integer arguments come first in every signature: without them the
/// struct is passed in registers and the stack layout is never exercised.
#[test]
fn codegen_over_aligned_struct_passed_by_value() {
    let code = r#"
int printf(const char *, ...);

struct A32 { _Alignas(32) double v[4]; };   /* 32 bytes, 32-aligned */
struct A64 { _Alignas(64) double v[8]; };   /* 64 bytes, 64-aligned */
struct P8  { double v[4]; };                /* same size, ordinary alignment */

static int fill32(struct A32 *s, double b) { for (int i = 0; i < 4; i++) s->v[i] = b + i; return 0; }
static int fill64(struct A64 *s, double b) { for (int i = 0; i < 8; i++) s->v[i] = b + i; return 0; }

/* Six integer arguments exhaust the GP registers, so `x` is genuinely stacked
   rather than passed in registers -- otherwise the layout is never exercised. */
static int take_alone(int a, int b, int c, int d, int e, int f, struct A32 x) {
    if (a + b + c + d + e + f != 21) return 1;
    for (int i = 0; i < 4; i++) if (x.v[i] != 1.5 + i) return 2;
    return 0;
}

/* Two of them: the second must start at the first's end rounded up to 32,
   not at a further-padded address. */
static int take_two(int a, int b, int c, int d, int e, int f,
                    struct A32 x, struct A32 y) {
    if (a + b + c + d + e + f != 21) return 3;
    for (int i = 0; i < 4; i++) if (x.v[i] != 1.5 + i) return 4;
    for (int i = 0; i < 4; i++) if (y.v[i] != 100.5 + i) return 5;
    return 0;
}

/* A plain 8-byte-aligned stacked argument ahead of it, so the over-aligned one
   really has to be rounded up rather than merely landing right by luck. */
static int take_after_scalar(int a, int b, int c, int d, int e, int f,
                             long p, struct A32 x) {
    if (p != 77) return 6;
    for (int i = 0; i < 4; i++) if (x.v[i] != 1.5 + i) return 7;
    return 0;
}

/* An ordinary-alignment struct in the same position: the case that already
   worked, kept so the fix cannot regress it. */
static int take_plain(int a, int b, int c, int d, int e, int f,
                      long p, struct P8 x) {
    if (p != 77) return 8;
    for (int i = 0; i < 4; i++) if (x.v[i] != 1.5 + i) return 9;
    return 0;
}

static int take_64(int a, int b, int c, int d, int e, int f, struct A64 x) {
    for (int i = 0; i < 8; i++) if (x.v[i] != 1.5 + i) return 10;
    return 0;
}

/* Interleaved with the over-aligned one, to pin that the argument *after* it
   is placed from the right running offset. */
static int take_then_scalar(int a, int b, int c, int d, int e, int f,
                            struct A32 x, long q) {
    for (int i = 0; i < 4; i++) if (x.v[i] != 1.5 + i) return 11;
    if (q != 55) return 12;
    return 0;
}

/* Eight doubles exhaust the FP registers. On aarch64 this struct is a
   homogeneous floating-point aggregate and rides in d0-d3 otherwise, so
   without this the stacked path is never reached on that target at all. */
static int take_after_fps(double a, double b, double c, double d,
                          double e, double f, double g, double h,
                          struct A32 x) {
    if (a + b + c + d + e + f + g + h != 36.0) return 13;
    for (int i = 0; i < 4; i++) if (x.v[i] != 1.5 + i) return 14;
    return 0;
}

/* The same position, ordinary alignment: a regression guard for the case that
   already worked. */
static int take_after_fps_plain(double a, double b, double c, double d,
                                double e, double f, double g, double h,
                                struct P8 x) {
    for (int i = 0; i < 4; i++) if (x.v[i] != 1.5 + i) return 15;
    return 0;
}

int main(void) {
    struct A32 x, y;
    struct A64 big;
    struct P8 plain;
    fill32(&x, 1.5);
    fill32(&y, 100.5);
    fill64(&big, 1.5);
    for (int i = 0; i < 4; i++) plain.v[i] = 1.5 + i;

    int r;
    if ((r = take_alone(1, 2, 3, 4, 5, 6, x)))            { printf("take_alone %d\n", r); return r; }
    if ((r = take_two(1, 2, 3, 4, 5, 6, x, y)))           { printf("take_two %d\n", r); return r; }
    if ((r = take_after_scalar(1, 2, 3, 4, 5, 6, 77, x))) { printf("take_after_scalar %d\n", r); return r; }
    if ((r = take_plain(1, 2, 3, 4, 5, 6, 77, plain)))    { printf("take_plain %d\n", r); return r; }
    if ((r = take_64(1, 2, 3, 4, 5, 6, big)))             { printf("take_64 %d\n", r); return r; }
    if ((r = take_then_scalar(1, 2, 3, 4, 5, 6, x, 55)))  { printf("take_then_scalar %d\n", r); return r; }
    if ((r = take_after_fps(1,2,3,4,5,6,7,8, x)))         { printf("take_after_fps %d\n", r); return r; }
    if ((r = take_after_fps_plain(1,2,3,4,5,6,7,8, plain))){ printf("take_after_fps_plain %d\n", r); return r; }
    return 0;
}
"#;
    assert_eq!(compile_and_run("over_aligned_struct_arg", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("over_aligned_struct_arg_opt", code),
        0
    );
}

/// A register-sized aggregate read through a pointer yields its *value*.
///
/// An aggregate wider than a register travels by address; one that fits travels
/// as its value. The `Deref` lowering returned the address for a struct at
/// every size, while a union already loaded when it fit -- two kinds, one rule,
/// and they disagreed. A caller handed the address where the convention
/// promised the value stored the pointer instead:
///
///     struct { unsigned a, b; } q = *p;   /* q got `p`, not `*p` */
///
/// Only initialization showed it. Assignment goes through `emit_assign`, which
/// block-copies struct/union at every size, and member access takes the address
/// through `linearize_lvalue` -- which is why `(*p).a`, `p->a` and `q = *p` were
/// all correct while `struct S q = *p;` was not.
///
/// Found building sparse: its `dup_token` copies an eight-byte `struct
/// position` out of a pointer, so every macro expansion produced a token whose
/// position fields were a pointer's low bits, and the preprocessor walked off
/// the end of `input_streams`.
#[test]
fn codegen_register_sized_aggregate_through_a_pointer() {
    let code = r#"
struct S2 { unsigned char a, b; };
struct S4 { unsigned short a, b; };
struct S8 { unsigned int a, b; };
struct S16 { unsigned long a, b; };
union U8 { unsigned int a; unsigned short b; };

/* The shape sparse tripped on: bitfields packed into eight bytes. */
struct Pos { unsigned int type:6, stream:14, newline:1, whitespace:1, pos:10;
             unsigned int line:31, noexpand:1; };

static struct S8 mk8(void) { struct S8 r; r.a = 0x1111; r.b = 0x2222; return r; }

int main(void) {
    struct S2 a2 = {1, 2}, *p2 = &a2;
    struct S4 a4 = {3, 4}, *p4 = &a4;
    struct S8 a8 = {5, 6}, *p8 = &a8;
    struct S16 a16 = {7, 8}, *p16 = &a16;
    union U8 u = {0x5555}, *pu = &u;

    /* Initialization from a dereference, either side of the boundary. */
    struct S2 q2 = *p2;   if (q2.a != 1 || q2.b != 2) return 1;
    struct S4 q4 = *p4;   if (q4.a != 3 || q4.b != 4) return 2;
    struct S8 q8 = *p8;   if (q8.a != 5 || q8.b != 6) return 3;
    struct S16 q16 = *p16; if (q16.a != 7 || q16.b != 8) return 4;
    union U8 qu = *pu;    if (qu.a != 0x5555) return 5;

    /* Assignment and member access, which were already right. */
    struct S8 r8; r8 = *p8;  if (r8.a != 5 || r8.b != 6) return 6;
    if ((*p8).a != 5 || p8->b != 6) return 7;
    r8 = mk8();              if (r8.a != 0x1111 || r8.b != 0x2222) return 8;
    struct S8 s8 = mk8();    if (s8.a != 0x1111 || s8.b != 0x2222) return 9;

    /* Passing and returning the dereferenced value. */
    struct S8 t8 = *p8;
    if (t8.a != 5) return 10;

    /* Bitfields: the sparse shape. A pointer's low bits would land in
       `stream`, so check every field survives the copy. */
    struct Pos pos;
    pos.type = 5; pos.stream = 2; pos.newline = 1; pos.whitespace = 1;
    pos.pos = 9; pos.line = 12345; pos.noexpand = 0;
    struct Pos *pp = &pos;
    struct Pos cp = *pp;
    if (cp.type != 5 || cp.stream != 2 || cp.newline != 1) return 11;
    if (cp.whitespace != 1 || cp.pos != 9) return 12;
    if (cp.line != 12345 || cp.noexpand != 0) return 13;

    /* Through a second level of indirection. */
    struct S8 **pp8 = &p8;
    struct S8 v8 = **pp8;
    if (v8.a != 5 || v8.b != 6) return 14;

    /* An array element reached through a pointer. */
    struct S8 arr[2] = { {10, 11}, {12, 13} };
    struct S8 *pa = arr;
    struct S8 w8 = *(pa + 1);
    if (w8.a != 12 || w8.b != 13) return 15;

    return 0;
}
"#;
    assert_eq!(compile_and_run("reg_sized_aggregate_deref", code, &[]), 0);
    assert_eq!(
        compile_and_run("reg_sized_aggregate_deref_o2", code, &["-O2".to_string()]),
        0
    );
}

/// A struct returned in registers and then **discarded**.
///
/// `mem2reg` decides a local is dead by scanning `insn.src`. But a call
/// returning a two-register struct writes its result into a `__2reg_N` local
/// and names that local's `Sym` as the instruction's *target*, not as a
/// source. When the result is used, a following `symaddr` puts the Sym in a
/// `src` and it survives; when it is discarded — `one();` on a line by itself
/// — nothing ever reads it, so the pass concluded the local was dead and
/// dropped both the slot and the pseudo.
///
/// The backend then had a target with no storage behind it. `handle_two_reg_return`
/// emits `mov %rax, (%reg)` for a `Loc::Reg` destination, so it stored through
/// whatever that register happened to hold.
///
/// **This was wrong at -O0 too.** It only faulted once the inliner had run,
/// because `should_inline` admits a function this size only at -O2, but the
/// bad IR was there at every level and -O0 passed on luck about the register's
/// contents: a slightly different reduction segfaults at -O0 as well.
///
/// The boundaries are the ABI's: 8 bytes returns in one register and is fine,
/// 9-16 returns in two and was not, and 17+ uses a hidden pointer argument —
/// which lands in `src` and so survived. Both register files are covered,
/// since the FP path stores XMM0/XMM1 the same way.
#[test]
fn codegen_discarded_two_register_struct_return() {
    let code = r#"
struct I8  { int a, b; };
struct I12 { int a, b, c; };
struct I16 { int a, b, c, d; };
struct I20 { int a, b, c, d, e; };
struct F12 { float a, b, c; };
struct D16 { double a, b; };

static int calls;

static struct I8  i8(void)  { struct I8  s = {1,2};       calls++; return s; }
static struct I12 i12(void) { struct I12 s = {1,2,3};     calls++; return s; }
static struct I16 i16(void) { struct I16 s = {1,2,3,4};   calls++; return s; }
static struct I20 i20(void) { struct I20 s = {1,2,3,4,5}; calls++; return s; }
static struct F12 f12(void) { struct F12 s = {1,2,3};     calls++; return s; }
static struct D16 d16(void) { struct D16 s = {1,2};       calls++; return s; }

int main(void) {
    /* Discarded: the result is never read, which is the case that broke. */
    i8(); i12(); i16(); i20(); f12(); d16();
    if (calls != 6) return 1;

    /* Used: this path always worked and must keep working, since the fix
       changes which pseudos survive. */
    { struct I12 v = i12(); if (v.a != 1 || v.b != 2 || v.c != 3) return 2; }
    { struct I16 v = i16(); if (v.a != 1 || v.d != 4) return 3; }
    { struct D16 v = d16(); if (v.a != 1.0 || v.b != 2.0) return 4; }
    { struct F12 v = f12(); if (v.a != 1.0f || v.c != 3.0f) return 5; }
    { struct I20 v = i20(); if (v.a != 1 || v.e != 5) return 6; }
    { struct I8  v = i8();  if (v.a != 1 || v.b != 2) return 7; }
    if (calls != 12) return 8;

    /* Discarded again, in a loop, so the slot is reused rather than merely
       allocated once. */
    for (int k = 0; k < 3; k++) { i12(); d16(); }
    if (calls != 18) return 9;

    /* Discarded inside an expression whose value is also discarded. */
    (void)i16();
    if (calls != 19) return 10;
    return 0;
}
"#;
    for opt in ["-O0", "-O1", "-O2", "-Os"] {
        assert_eq!(
            compile_and_run(
                &format!("codegen_discarded_2reg{}", opt.replace('-', "_")),
                code,
                &[opt.to_string()]
            ),
            0,
            "discarded two-register struct return failed at {opt}"
        );
    }
}

/// The address of an element of a string literal is a static address, at any
/// depth and in an aggregate initializer as well as a scalar one.
///
/// The literal acquires an address by being interned, which needs `&mut
/// self` -- so the walk that reaches it has to be the mutable one. There
/// used to be two walks, and only the outer one could intern, which is why
/// `"X" + 1` worked and `&("X"[0])` was rejected: the first arrives with the
/// literal in hand, the second with an `Index` wrapped around it.
#[test]
fn codegen_address_of_string_literal_element() {
    let code = r#"
extern void abort(void);
extern int strcmp(const char *, const char *);

void *foo[] = {(void *)&("X"[0])};
char *bar[] = {"HELLO", "HELLO" + 2, &"HELLO"[3], &("HELLO"[4])};
struct S { void *p; char *q; } s = {(void *)&("AB"[1]), &"CD"[1]};
unsigned int *w[] = {(unsigned int *)&(U"AB"[1])};
char *scalar = &"WXYZ"[1];

int main(void)
{
    if (((char *)foo[0])[0] != 'X') abort();
    if (strcmp(bar[0], "HELLO")) abort();
    if (strcmp(bar[1], "LLO")) abort();
    if (strcmp(bar[2], "LO")) abort();
    if (strcmp(bar[3], "O")) abort();
    if (((char *)s.p)[0] != 'B') abort();
    if (strcmp(s.q, "D")) abort();
    if (*w[0] != 'B') abort();
    if (strcmp(scalar, "XYZ")) abort();
    return 0;
}
"#;
    for opt in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run("c17_addr_of_string_elem", code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

// ============================================================================
// Regression: pointer scaling by a type past the old 512 MB bound
// ============================================================================

/// Indexing scales by the element's **byte size**, at every size the compiler
/// accepts.
///
/// `size_bits` answers a *value* width in a `u32` and saturates for an
/// aggregate past `u32::MAX` bits. While the object-size bound was that same
/// number the saturation was unreachable -- the parser refused any type that
/// could reach it. Raising the bound made it reachable, and every site still
/// deriving a byte count as `size_bits / 8` began answering 536870911 for any
/// larger type: `&a[1][0] - &a[0][0]` on a `char[3][600000000]`, the stride of
/// an array of a 600 MB struct, and `p + 1` on a pointer to one. `sizeof` was
/// right throughout, so the sizes agreed with gcc while the addresses did not.
///
/// Asserted on the assembly rather than by running: the scale factor is what
/// was wrong, and no test should ask its machine for gigabytes to see it. The
/// objects are `extern` for the same reason -- nothing is defined, allocated
/// or dereferenced.
#[test]
fn codegen_pointer_scaling_past_the_old_object_bound() {
    const SRC: &str = r#"
extern char rows[3][600000000L];
struct Big { char x[600000000L]; };
extern struct Big bigs[2];

char *row(int i) { return rows[i]; }
struct Big *elem(int i) { return &bigs[i]; }
long stride(struct Big *p, int i) { return (char *)&p[i] - (char *)p; }
"#;

    // One target is enough, and x86-64 is the one that materialises the scale
    // as a literal: the element size is computed in `ir/linearize.rs`, before
    // any backend runs, so the defect was target-independent. aarch64 builds
    // the same constant with `movz`/`movk`, which would make this assertion
    // about instruction encoding rather than about the size.
    let asm = asm_for_with("pointer_scale", X86_64_LINUX, SRC, &["-O2"]);
    for func in ["row", "elem", "stride"] {
        let body = body_of(&asm, func);
        assert!(
            body.contains("600000000"),
            "{func} does not scale by the element size:\n{body}"
        );
        assert!(
            !body.contains("536870911"),
            "{func} scales by the saturated size_bits:\n{body}"
        );
    }
}

/// An aggregate is copied by its **byte size**, at every size the compiler
/// accepts.
///
/// The companion to `codegen_pointer_scaling_past_the_old_object_bound`, and
/// the same root cause: `size_bits` saturates at `u32::MAX` bits, and raising
/// the object-size bound made the saturation reachable. The sites that survived
/// that commit's audit were the ones that launder the bit count through a local
/// variable -- two of them spell it `let target_size_bytes = target_size / 8;`,
/// which no grep for `size_bits(..) / 8` can find -- through a `u32` field
/// (`struct_return_size`), or through `ArgClass::Indirect`'s payload.
///
/// Every shape below copied 536870911 bytes of a 600000000-byte object, on both
/// targets, at every optimization level. `a = b` is the one that matters most:
/// it is the plainest aggregate copy in the language.
///
/// Asserted on x86-64 alone, and the source is shaped to stay cheap. Both are
/// load-bearing, not stylistic -- each one avoids a different pre-existing
/// backend blowup that this test walked straight into and that took both Linux
/// CI runners down with SIGTERM:
///
/// - **x86-64 only.** Nothing to do with coverage: the length is computed in
///   `ir/` before any backend runs, so one target proves it, and x86-64 is the
///   one that materialises the constant as a literal rather than as
///   `movz`/`movk`. An `AARCH64_LINUX` assertion once cost **12.5 seconds and
///   16 GB** resident, because `initialize`'s 600 MB local went through an
///   unrolled zeroing of the frame. Nothing zeroes the frame now; nobody has
///   re-measured the rest of the aarch64 path at this size, so keep the list
///   as it is.
/// - **`by_value_param` does not pass its argument on.** The prologue copy is
///   the site under test. Sending a 600 MB aggregate used to cost 16 seconds
///   and 21.9 GB of compiler memory, one load/store pair per eightbyte; the
///   outgoing copy is a `rep movsq` now, but it is not what this test is about.
///
/// Everything here is `extern`; nothing is defined or run.
#[test]
fn codegen_aggregate_copy_length_past_the_old_object_bound() {
    const SRC: &str = r#"
struct Big { char x[600000000L]; };
extern struct Big src, dst;
void sink(struct Big *);

void assign(void) { dst = src; }
void initialize(void) { struct Big loc = src; sink(&loc); }
void by_value_param(struct Big p) { sink(&p); }
struct Big returns_it(void) { return src; }
unsigned long extent(void) { return __builtin_object_size(src.x, 0); }
"#;

    let asm = asm_for_with("aggregate_copy_length", X86_64_LINUX, SRC, &["-O2"]);
    for func in [
        "assign",
        "initialize",
        "by_value_param",
        "returns_it",
        "extent",
    ] {
        let body = body_of(&asm, func);
        assert!(
            body.contains("600000000"),
            "{func} does not use the aggregate's byte size:\n{body}"
        );
        assert!(
            !body.contains("536870911"),
            "{func} uses the saturated size_bits:\n{body}"
        );
    }
}

/// A struct or union through `?:`, and every other expression that yields one,
/// on both sides of the eight-byte line where the IR stops carrying an
/// aggregate's value and starts carrying its address.
///
/// `t = c ? v : u` on an eight-byte struct segfaulted on both targets: the
/// arms were loaded as values (the convention for a register-sized aggregate)
/// and the assignment then dereferenced the selected value as an address,
/// because `rvalue_addr` passed any non-`Sym` pseudo through as a pointer. It
/// now spills a register-sized aggregate value to a temporary. A larger `?:`
/// merged its arms' addresses at the aggregate's own width, and a struct
/// assignment expression yielded an address at every size; both follow the
/// convention now. Covered: assignment expressions, the comma operator,
/// statement expressions, nested and impure (call) arms, `?:` as an argument,
/// a return value, a member's base and an initializer, and unions.
const AGGREGATE_VALUE_SHAPES: &str = r#"
#define NI __attribute__((noinline))
#define T(N) \
struct s##N { unsigned char c[N]; }; \
union u##N { unsigned char c[N]; long pad; }; \
static int calls##N; \
NI struct s##N mk##N(int base) { struct s##N r; int i; calls##N++; for (i = 0; i < N; i++) r.c[i] = (unsigned char)(base + i); return r; } \
NI int sum##N(struct s##N s) { int i, t = 0; for (i = 0; i < N; i++) t += s.c[i]; return t; } \
NI struct s##N ret##N(int k, struct s##N a, struct s##N b) { return k ? a : b; } \
NI int test##N(void) { \
    struct s##N t, u = mk##N(1), v = mk##N(101), w; int i, k = 1; \
    w = (t = u); \
    for (i = 0; i < N; i++) if (w.c[i] != i + 1 || t.c[i] != i + 1) return 1; \
    t = (k, v); \
    for (i = 0; i < N; i++) if (t.c[i] != i + 101) return 2; \
    t = ({ struct s##N z = u; z; }); \
    for (i = 0; i < N; i++) if (t.c[i] != i + 1) return 3; \
    t = k ? (k > 5 ? u : v) : u; \
    for (i = 0; i < N; i++) if (t.c[i] != i + 101) return 4; \
    calls##N = 0; \
    t = k ? mk##N(50) : mk##N(60); \
    if (calls##N != 1) return 5; \
    for (i = 0; i < N; i++) if (t.c[i] != i + 50) return 6; \
    if (sum##N(k ? u : v) != sum##N(u)) return 7; \
    t = ret##N(0, u, v); \
    for (i = 0; i < N; i++) if (t.c[i] != i + 101) return 8; \
    if ((k ? u : v).c[N - 1] != N) return 9; \
    struct s##N x = k ? v : u; \
    for (i = 0; i < N; i++) if (x.c[i] != i + 101) return 10; \
    union u##N p, q, r; p.c[0] = 7; q.c[0] = 9; r = k ? q : p; if (r.c[0] != 9) return 11; \
    return 0; }
T(1) T(2) T(4) T(7) T(8) T(9) T(12) T(16) T(17) T(24) T(40)
int main(void)
{
    int r;
#define C(N) if ((r = test##N())) return N * 20 + r;
    C(1) C(2) C(4) C(7) C(8) C(9) C(12) C(16) C(17) C(24) C(40)
    return 0;
}
"#;

#[test]
fn codegen_aggregate_through_conditional_and_other_rvalues() {
    let src = AGGREGATE_VALUE_SHAPES;
    assert_eq!(compile_and_run("aggregate_rvalues", src, &[]), 0);
    let opts = vec!["-O2".to_string()];
    assert_eq!(compile_and_run("aggregate_rvalues_o2", src, &opts), 0);
    for opt in ["-O0", "-O2"] {
        if let Some(code) = compile_and_run_aarch64("aggregate_rvalues_a64", src, opt) {
            assert_eq!(code, 0, "aarch64 at {opt}");
        }
    }
}

/// The qualifiers written around `typeof(..)` or `_Atomic(..)` in a type-name
/// are part of the type it names. The type-name specifier loop returned as
/// soon as it had parsed the operand, so a leading `const` was dropped --
/// `_Generic` picked `default` for `const typeof(int) *` against a
/// `const int *` -- and a trailing one was left for the caller, which then
/// failed to parse it.
#[test]
fn codegen_type_name_keeps_qualifiers_around_typeof() {
    let src = r#"
const int *p;
volatile long *q;

int main(void)
{
    /* The qualifier before typeof is part of the association's type. */
    if (_Generic(p, const typeof(int) *: 1, default: 2) != 1)
        return 1;
    if (_Generic(q, volatile __typeof__(long) *: 1, default: 2) != 1)
        return 2;
    /* ... and so is one after it, which must still parse. */
    if (_Generic(p, typeof(int) const *: 1, default: 2) != 1)
        return 3;
    /* An unqualified typeof still names the unqualified type. */
    if (_Generic(p, typeof(int) *: 1, default: 2) != 2)
        return 4;
    if (sizeof(_Atomic(int) const) != sizeof(int))
        return 5;
    if (sizeof(typeof(short) volatile) != sizeof(short))
        return 6;
    return 0;
}
"#;
    compile_and_run_everywhere("type_name_keeps_qualifiers_around_typeof", src);
}

/// C17 6.7p1 lets declaration specifiers appear in any order, so a qualifier
/// or storage class may follow `typeof(..)`, `_Atomic(..)` or an enum
/// specifier as it may follow `int`. Those arms returned as soon as their
/// type was parsed (the enum arm consumed qualifiers only), and the next
/// specifier was read as the declarator's name. The block-scope `static`
/// enum must keep its value across calls.
#[test]
fn codegen_specifiers_may_follow_a_complete_type_specifier() {
    let src = r#"
typeof(int) const x = 1;
_Atomic(int) const y = 2;
enum E { A = 3, B } static e = B;
struct S { int v; } __attribute__((unused)) static s = { 5 };

static int counter(void)
{
    enum { Z, LAST = 100 } static n;    /* static: keeps its value */
    return ++n;
}

int main(void)
{
    if (x != 1 || y != 2 || e != B || s.v != 5)
        return 1;
    if (_Generic(&x, const int *: 0, default: 1))
        return 2;
    counter();
    counter();
    if (counter() != 3)
        return 3;
    typeof(long) volatile static w = 7;
    if (w != 7 || sizeof w != sizeof(long))
        return 4;
    return 0;
}
"#;
    compile_and_run_everywhere("specifiers_may_follow_a_complete_type_specifier", src);
}

/// `noreturn` belongs to the function *type*, since that is what a call site
/// reads. Only the first plain file-scope declarator put it there, so a
/// grouped, later or block-scope declarator produced a function the caller
/// believed could return -- visible at -O2 as the code after the call
/// surviving.
#[test]
fn codegen_noreturn_reaches_the_type_from_every_declarator() {
    let shapes = [
        ("first", "void f(void) __attribute__((noreturn));\n", ""),
        ("grouped", "void (f)(void) __attribute__((noreturn));\n", ""),
        ("later", "int x, f(void) __attribute__((noreturn));\n", ""),
        ("block", "", "void f(void) __attribute__((noreturn));"),
        ("block_keyword", "", "_Noreturn void f(void);"),
        // Written among the specifiers, the attribute belongs to every
        // declarator of the list, as gcc has it.
        (
            "specifier",
            "__attribute__((noreturn)) void e(void), f(void);\n",
            "",
        ),
        // And a prototype's attribute carries to a later redeclaration.
        (
            "redeclared",
            "void f(void) __attribute__((noreturn));\nvoid f(void);\n",
            "",
        ),
    ];
    for (name, file_decl, block_decl) in shapes {
        let src = format!("{file_decl}int g(void) {{ {block_decl} f(); return 12345; }}\n");
        for triple in [X86_64_LINUX, AARCH64_LINUX] {
            let asm = asm_for_with(&format!("noreturn_{name}"), triple, &src, &["-O2"]);
            assert!(
                !body_of(&asm, "g").contains("12345"),
                "{name} on {triple}: the code after a noreturn call must be dead:\n{asm}"
            );
        }
    }
    // The controls: without the attribute the return survives, so the probe
    // above can fail -- and an attribute on a *parameter* is the parameter's,
    // not the function's.
    for (name, decl, call) in [
        ("noreturn_control", "void f(void);", "f()"),
        (
            "noreturn_param",
            "void f(void (*cb)(void) __attribute__((noreturn)));",
            "f(0)",
        ),
    ] {
        let src = format!("{decl}\nint g(void) {{ {call}; return 12345; }}\n");
        let asm = asm_for_with(name, X86_64_LINUX, &src, &["-O2"]);
        assert!(body_of(&asm, "g").contains("12345"), "{name}:\n{asm}");
    }
}

/// The bound itself, not just the answer: above the threshold the zero-fill is
/// a `memset` call, below it is still stores.
///
/// The behavioural test above passes either way — a million unrolled stores
/// produce a correctly zeroed object, just not in a time anyone will wait for.
/// This is the test that the *bound* exists, and the negative half keeps it
/// from being satisfied by calling `memset` for every size, which would cost
/// more than the stores it replaced for a small object.
#[test]
fn codegen_a_large_aggregate_zero_is_a_memset_call() {
    use crate::codegen::asm_probe::{asm_for_with, AARCH64_LINUX, X86_64_LINUX};

    let src = |n: usize| {
        format!("void sink(char *);\nvoid probe(void) {{ char buf[{n}] = {{0}}; sink(buf); }}\n")
    };

    for triple in [X86_64_LINUX, AARCH64_LINUX] {
        // Comfortably over `INLINE_LIMIT_BYTES` (128).
        let big = asm_for_with("aggzero_big", triple, &src(4096), &["-O2"]);
        assert!(
            big.contains("memset"),
            "a 4096-byte zero-fill belongs in a memset call, not 512 stores, on {triple}:\n{big}"
        );

        // And the unrolled form is still used where it is cheaper than a call.
        let small = asm_for_with("aggzero_small", triple, &src(16), &["-O2"]);
        assert!(
            !small.contains("memset"),
            "a 16-byte zero-fill is cheaper unrolled than called, on {triple}:\n{small}"
        );
    }
}

/// Storing one field of an eight-byte aggregate leaves the other alone.
///
/// The x86-64 store lowering widens a 32-bit store at offset 0 of a local to
/// 64 bits, to clear stale upper bits when a narrow value goes into a wider
/// slot. Its own comment records the exception that needs: "struct/union
/// fields at offset 0 must use exact size to avoid clobbering the adjacent
/// field at offset 4". The exception asked whether the object was *larger than*
/// 64 bits, which an eight-byte aggregate is not -- so exactly the case the
/// comment describes was the one that fell through.
///
/// Only at `-O0`: with the optimizer on, the field is promoted out of memory
/// before the store lowering sees it.
#[test]
fn codegen_a_field_store_does_not_widen_over_its_neighbour() {
    let code = r#"
struct P { int x, y; };
struct S { struct P t; };
union U { struct P p; double d; };

int main(void)
{
    /* The reported shape: a designated override inside an eight-byte struct. */
    struct S a = { .t = {1, 2}, .t.x = 3 };
    if (a.t.x != 3 || a.t.y != 2) return 1;

    /* The same store reached other ways. */
    struct P b = {1, 2};
    b.x = 3;
    if (b.x != 3 || b.y != 2) return 2;

    struct P c;
    c.y = 2;
    c.x = 3;
    if (c.x != 3 || c.y != 2) return 3;

    struct P *p = &b;
    p->x = 9;
    if (b.x != 9 || b.y != 2) return 4;

    union U u;
    u.p.y = 7;
    u.p.x = 5;
    if (u.p.x != 5 || u.p.y != 7) return 5;

    /* Arrays are the same shape at the same size. */
    int arr[2] = {1, 2};
    arr[0] = 3;
    if (arr[0] != 3 || arr[1] != 2) return 6;

    /* Exactly eight bytes made of narrower fields. */
    struct Q { short a, b, c, d; } q = {1, 2, 3, 4};
    q.a = 9;
    if (q.a != 9 || q.b != 2 || q.c != 3 || q.d != 4) return 7;

    return 0;
}
"#;
    // `-O0` explicitly: the default matrix compiles at `-O`, where the field is
    // promoted out of memory before the store lowering ever sees it, so the
    // defect is invisible there.
    assert_eq!(
        compile_and_run("field_store_no_widen", code, &["-O0".to_string()]),
        0
    );
    assert_eq!(compile_and_run("field_store_no_widen_matrix", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("field_store_no_widen_opt", code),
        0
    );
}

/// The control: a narrow value stored into a wider scalar slot still leaves no
/// stale upper bits.
///
/// This is what the widening is for, and it is why the fix has to ask whether
/// the object is an aggregate rather than simply stop widening. Each case
/// writes a wide value into the slot first, so a store that failed to clear the
/// upper half would read it back.
#[test]
fn codegen_a_narrow_store_into_a_wide_slot_clears_it() {
    let code = r#"
int wide(void) { return -1; }

int main(void)
{
    /* Put a known wide pattern in the slot, then overwrite it narrowly. */
    long l = 0x7fffffff7fffffffL;
    int i = 5;
    l = i;
    if (l != 5) return 1;

    unsigned long ul = 0xffffffffffffffffUL;
    unsigned ui = 7;
    ul = ui;
    if (ul != 7UL) return 2;

    void *vp = (void *)0x7fffffffffffL;
    unsigned addr = 0;
    vp = (void *)(unsigned long)addr;
    if (vp != (void *)0) return 3;

    /* Through a call, so the value is not a constant the optimizer can see. */
    long l2 = 0x7fffffff7fffffffL;
    l2 = wide();
    if (l2 != -1L) return 4;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("narrow_store_clears_slot", code, &["-O0".to_string()]),
        0
    );
    assert_eq!(
        compile_and_run("narrow_store_clears_slot_matrix", code, &[]),
        0
    );
    assert_eq!(
        compile_and_run_optimized("narrow_store_clears_slot_opt", code),
        0
    );
}
