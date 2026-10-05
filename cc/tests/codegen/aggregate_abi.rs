//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Aggregates in the calling convention: structs, unions and HFAs
// passed and returned, in registers and in memory. The assembly checks
// compile in process, in `cc/test_asm/codegen_aggregate_abi.rs`.
//

use crate::common::{
    compile_and_run, compile_and_run_aarch64, compile_and_run_everywhere, compile_and_run_optimized,
};

/// One section per consolidated test, each under that test's own
/// documentation. Exit codes:
/// `codegen_default_return_64bit` 11..=14.
/// `codegen_call_return_pointer` 21..=23.
/// `codegen_ret_64bit` 31..=33.
/// `codegen_two_reg_int_struct_return` 41..=46.
/// `codegen_small_struct_return` 51..=57.
const SCALAR_AND_SMALL_AGGREGATE_RETURNS: &str = r#"
/* ====================================================================== */
/* codegen_default_return_64bit: exit codes 11..14 */
// Test: function returning pointer/long with no explicit return gets 64-bit zero
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

static int t_codegen_default_return_64bit(void) {
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

/* ====================================================================== */
/* codegen_call_return_pointer: exit codes 21..23 */
// Test: function returning pointer via call — return value is full 64 bits
void *identity(void *p) {
    return p;
}

long return_long(long v) {
    return v;
}

static int t_codegen_call_return_pointer(void) {
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

/* ====================================================================== */
/* codegen_ret_64bit: exit codes 31..33 */
// Test: emit_ret with 64-bit integer return value
long return_big(void) {
    return 0x123456789ABCDEF0L;
}

void *return_ptr(void *p) {
    return p;
}

unsigned long return_unsigned(void) {
    return 0xFFFFFFFFFFFFFFFFUL;
}

static int t_codegen_ret_64bit(void) {
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

/* ====================================================================== */
/* codegen_two_reg_int_struct_return: exit codes 41..46 */
// Regression: two-register integer struct return clobbered rax when
// second source was allocated there (parallel-move problem).
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

static int t_codegen_two_reg_int_struct_return(void) {
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

/* ====================================================================== */
/* codegen_small_struct_return: exit codes 51..57 */
// Regression test: small struct (<=64 bits) returned by value from a function
// was stored as a raw register value. When assigned to a struct variable,
// emit_assign's block_copy dereferenced it as a pointer → SIGSEGV.
// Fixed by allocating local storage for small struct returns.
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

static int t_codegen_small_struct_return(void) {
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

int main(void)
{
    int r;
    if ((r = t_codegen_default_return_64bit()) != 0) return 10 + r;
    if ((r = t_codegen_call_return_pointer()) != 0) return 20 + r;
    if ((r = t_codegen_ret_64bit()) != 0) return 30 + r;
    if ((r = t_codegen_two_reg_int_struct_return()) != 0) return 40 + r;
    if ((r = t_codegen_small_struct_return()) != 0) return 50 + r;
    return 0;
}
"#;

/// 64-bit scalar returns and register-sized aggregate returns, at the matrix
/// levels. Consolidates `codegen_default_return_64bit`,
/// `codegen_call_return_pointer`, `codegen_ret_64bit`,
/// `codegen_two_reg_int_struct_return` and `codegen_small_struct_return`.
#[test]
fn codegen_scalar_and_small_aggregate_returns() {
    assert_eq!(
        compile_and_run(
            "scalar_small_agg_returns",
            SCALAR_AND_SMALL_AGGREGATE_RETURNS,
            &[]
        ),
        0
    );
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

/// One section per consolidated test, each under that test's own
/// documentation. Exit codes:
/// `codegen_aggregate_return_into_an_over_aligned_frame` 11..=19.
/// `codegen_discarded_two_register_struct_return` 21..=30.
const REGISTER_AGGREGATE_RETURNS: &str = r#"
/* ====================================================================== */
/* codegen_aggregate_return_into_an_over_aligned_frame: exit codes 11..19 */
// A register-returned aggregate must be stored through the frame's own base
// register, not through `%rbp`.
//
// When a local's alignment exceeds the stack's, the prologue realigns `%rsp`
// and keeps the frame's base in a second register; `stack_mem` then addresses
// every local relative to that. Six sites in the call path spelled
// `-(slot + callee_saved_offset)(%rbp)` by hand instead, so under such a
// frame the return value was written to an address nothing reads back --
// `movq %rax, -112(%rbp)` followed by `movq 32(%rbx), %rax`. Four of the six
// were return paths, which is why `struct S { long long a, b; } r = make();`
// came back as zeros beside an `_Alignas(32)` local, in ordinary C with no
// varargs involved.
//
// Whether it is *visible* depends on what happens to occupy the address that
// is written, so the levels are swept rather than trusted: the two-register
// integer shape came back wrong at `-O1` and right at `-O0` and `-O2`.
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

static int t_codegen_aggregate_return_into_an_over_aligned_frame(void)
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

/* ====================================================================== */
/* codegen_discarded_two_register_struct_return: exit codes 21..30 */
// A struct returned in registers and then **discarded**.
//
// `mem2reg` decides a local is dead by scanning `insn.src`. But a call
// returning a two-register struct writes its result into a `__2reg_N` local
// and names that local's `Sym` as the instruction's *target*, not as a
// source. When the result is used, a following `symaddr` puts the Sym in a
// `src` and it survives; when it is discarded — `one();` on a line by itself
// — nothing ever reads it, so the pass concluded the local was dead and
// dropped both the slot and the pseudo.
//
// The backend then had a target with no storage behind it. `handle_two_reg_return`
// emits `mov %rax, (%reg)` for a `Loc::Reg` destination, so it stored through
// whatever that register happened to hold.
//
// **This was wrong at -O0 too.** It only faulted once the inliner had run,
// because `should_inline` admits a function this size only at -O2, but the
// bad IR was there at every level and -O0 passed on luck about the register's
// contents: a slightly different reduction segfaults at -O0 as well.
//
// The boundaries are the ABI's: 8 bytes returns in one register and is fine,
// 9-16 returns in two and was not, and 17+ uses a hidden pointer argument —
// which lands in `src` and so survived. Both register files are covered,
// since the FP path stores XMM0/XMM1 the same way.
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

static int t_codegen_discarded_two_register_struct_return(void) {
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

int main(void)
{
    int r;
    if ((r = t_codegen_aggregate_return_into_an_over_aligned_frame()) != 0) return 10 + r;
    if ((r = t_codegen_discarded_two_register_struct_return()) != 0) return 20 + r;
    return 0;
}
"#;

/// Register-returned aggregates landing in an over-aligned frame, and
/// discarded, swept over -O0, -O1, -O2 and -Os. Consolidates
/// `codegen_aggregate_return_into_an_over_aligned_frame` (whose assembly
/// half is in `cc/test_asm/codegen_aggregate_abi.rs`) and
/// `codegen_discarded_two_register_struct_return`.
#[test]
fn codegen_register_aggregate_returns_at_every_level() {
    for opt in ["-O0", "-O1", "-O2", "-Os"] {
        assert_eq!(
            compile_and_run(
                &format!("codegen_reg_agg_returns{}", opt.replace('-', "_")),
                REGISTER_AGGREGATE_RETURNS,
                &[opt.to_string()]
            ),
            0,
            "at {opt}"
        );
    }
}

/// One section per consolidated test, each under that test's own
/// documentation. Exit codes:
/// `codegen_hfa_param_element_counts` 21..=30.
/// `codegen_hfa_return_element_counts` 41..=50.
/// `codegen_composite_in_two_registers` 61..=68.
/// `codegen_over_aligned_struct_passed_by_value` 81..=95.
const AGGREGATE_ARGUMENTS_AND_RETURNS: &str = r#"
/* ====================================================================== */
/* codegen_hfa_param_element_counts: exit codes 21..30 */
// AAPCS64 §5.4.2 gives a homogeneous floating-point aggregate one V register
// per element, for one to four elements. Both sides of the aarch64 call
// recognised only the two-element case: a three- or four-element HFA went out
// whole in a general register while the callee read it from V0-V3, and it
// consumed an integer slot, so the *next* integer argument was shifted along
// as well. An aggregate small enough to sit in one register was passed as a
// single value rather than split across the element registers.
//
// Every shape below is checked against gcc's own layout, so the test is an
// ABI conformance check as much as a regression test.
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

static int t_codegen_hfa_param_element_counts(void) {
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

/* ====================================================================== */
/* codegen_hfa_return_element_counts: exit codes 41..50 */
// An HFA is returned in one V register per element, up to four, and its size
// does not enter into it -- `struct { double a, b, c, d; }` is thirty-two
// bytes and still comes back in V0-V3.
//
// Three things had to agree before that worked on aarch64. The linearizer
// claimed every aggregate over sixteen bytes for the hidden-pointer return,
// on the stated grounds that nothing implemented a three-register HFA return.
// The return emitter handled one element and two, so three or four fell
// through and sent back the address of the callee's own frame slot -- which
// the caller then dereferenced after the frame was gone. And an aggregate
// returned in registers is written into a local by the caller, but that local
// was only allocated at sixteen bytes or fewer, so a twenty-four-byte result
// had nowhere to land and its first use read through a zero.
typedef struct { float a; }              RF1;
typedef struct { float a, b; }           RF2;
typedef struct { float a, b, c; }        RF3;
typedef struct { float a, b, c, d; }     RF4;
typedef struct { double a; }             RD1;
typedef struct { double a, b; }          RD2;
typedef struct { double a, b, c; }       RD3;
typedef struct { double a, b, c, d; }    RD4;

RF1 m_f1(float x) { RF1 r = {x};                   return r; }
RF2 m_f2(float x) { RF2 r = {x, x+1};              return r; }
RF3 m_f3(float x) { RF3 r = {x, x+1, x+2};         return r; }
RF4 m_f4(float x) { RF4 r = {x, x+1, x+2, x+3};    return r; }
RD1 m_d1(double x){ RD1 r = {x};                   return r; }
RD2 m_d2(double x){ RD2 r = {x, x+1};              return r; }
RD3 m_d3(double x){ RD3 r = {x, x+1, x+2};         return r; }
RD4 m_d4(double x){ RD4 r = {x, x+1, x+2, x+3};    return r; }

static int t_codegen_hfa_return_element_counts(void) {
    { RF1 v = m_f1(1); if (v.a != 1) return 1; }
    { RF2 v = m_f2(1); if (v.a*10 + v.b != 12) return 2; }
    { RF3 v = m_f3(1); if (v.a*100 + v.b*10 + v.c != 123) return 3; }
    { RF4 v = m_f4(1); if (v.a*1000 + v.b*100 + v.c*10 + v.d != 1234) return 4; }
    { RD1 v = m_d1(1); if (v.a != 1) return 5; }
    { RD2 v = m_d2(1); if (v.a*10 + v.b != 12) return 6; }
    { RD3 v = m_d3(1); if (v.a*100 + v.b*10 + v.c != 123) return 7; }
    { RD4 v = m_d4(1); if (v.a*1000 + v.b*100 + v.c*10 + v.d != 1234) return 8; }

    /* returned aggregate consumed in place, not through a named local */
    if (m_d3(2).b != 3) return 9;
    if (m_f4(2).d != 5) return 10;
    return 0;
}

/* ====================================================================== */
/* codegen_composite_in_two_registers: exit codes 61..68 */
// AAPCS64 §5.4.2 C.10 and SysV AMD64 §3.2.3 both put a composite of at most
// sixteen bytes in two consecutive general registers. aarch64 handed over its
// *address* instead, on both sides of the call, so c17 agreed with itself and
// nothing in the suite noticed -- while every call across a c17/gcc boundary
// was wrong. With a gcc caller it segfaulted: the callee read the first eight
// bytes of the aggregate as if they were a pointer and dereferenced them.
//
// Structs of four and eight bytes were already right; the broken range is
// exactly the two-eightbyte one.
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

static int t_codegen_composite_in_two_registers(void) {
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

/* ====================================================================== */
/* codegen_over_aligned_struct_passed_by_value: exit codes 81..95 */
// A struct whose alignment exceeds the argument area's own, passed by value.
//
// `IncomingOff::take` rounded the *frame displacement* up to the argument's
// alignment, but that displacement already carries the saved `%rbp` and the
// return address. So an argument wanting 32-byte alignment and arriving first
// went to `32(%rbp)` while the caller -- which measures from its outgoing
// area's own base, correctly -- had written it at `16(%rbp)`. The callee read
// the struct's second half and ran off its end.
//
// Invisible below 32-byte alignment: 16 is already a multiple of 8 and of 16,
// so only an argument wanting more than the area's own alignment can tell the
// two bases apart. The ordinary-alignment cases are here so the fix cannot
// regress them.
//
// Six integer arguments come first in every signature: without them the
// struct is passed in registers and the stack layout is never exercised.
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

static int t_codegen_over_aligned_struct_passed_by_value(void) {
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

int main(void)
{
    int r;
    if ((r = t_codegen_hfa_param_element_counts()) != 0) return 20 + r;
    if ((r = t_codegen_hfa_return_element_counts()) != 0) return 40 + r;
    if ((r = t_codegen_composite_in_two_registers()) != 0) return 60 + r;
    if ((r = t_codegen_over_aligned_struct_passed_by_value()) != 0) return 80 + r;
    return 0;
}
"#;

/// HFAs of every element count as arguments and returns, composites in two
/// registers, and over-aligned structs by value, at the matrix levels and
/// at -O1. Consolidates `codegen_hfa_param_element_counts`,
/// `codegen_hfa_return_element_counts`, `codegen_composite_in_two_registers`
/// and `codegen_over_aligned_struct_passed_by_value`.
#[test]
fn codegen_aggregate_arguments_and_returns() {
    assert_eq!(
        compile_and_run("aggregate_args_rets", AGGREGATE_ARGUMENTS_AND_RETURNS, &[]),
        0
    );
    assert_eq!(
        compile_and_run_optimized("aggregate_args_rets_opt", AGGREGATE_ARGUMENTS_AND_RETURNS),
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

/// One section per consolidated test, each under that test's own
/// documentation. Exit codes:
/// `codegen_type_name_keeps_qualifiers_around_typeof` 11..=16.
/// `codegen_specifiers_may_follow_a_complete_type_specifier` 21..=24.
const TYPE_NAMES_AND_SPECIFIER_ORDER: &str = r#"
/* ====================================================================== */
/* codegen_type_name_keeps_qualifiers_around_typeof: exit codes 11..16 */
// The qualifiers written around `typeof(..)` or `_Atomic(..)` in a type-name
// are part of the type it names. The type-name specifier loop returned as
// soon as it had parsed the operand, so a leading `const` was dropped --
// `_Generic` picked `default` for `const typeof(int) *` against a
// `const int *` -- and a trailing one was left for the caller, which then
// failed to parse it.
const int *p;
volatile long *q;

static int t_codegen_type_name_keeps_qualifiers_around_typeof(void)
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

/* ====================================================================== */
/* codegen_specifiers_may_follow_a_complete_type_specifier: exit codes 21..24 */
// C17 6.7p1 lets declaration specifiers appear in any order, so a qualifier
// or storage class may follow `typeof(..)`, `_Atomic(..)` or an enum
// specifier as it may follow `int`. Those arms returned as soon as their
// type was parsed (the enum arm consumed qualifiers only), and the next
// specifier was read as the declarator's name. The block-scope `static`
// enum must keep its value across calls.
typeof(int) const x = 1;
_Atomic(int) const y = 2;
enum E { A = 3, B } static e = B;
struct S { int v; } __attribute__((unused)) static s = { 5 };

static int counter(void)
{
    enum { Z, LAST = 100 } static n;    /* static: keeps its value */
    return ++n;
}

static int t_codegen_specifiers_may_follow_a_complete_type_specifier(void)
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

int main(void)
{
    int r;
    if ((r = t_codegen_type_name_keeps_qualifiers_around_typeof()) != 0) return 10 + r;
    if ((r = t_codegen_specifiers_may_follow_a_complete_type_specifier()) != 0) return 20 + r;
    return 0;
}
"#;

/// Qualifiers around `typeof(..)` and specifiers after a complete type
/// specifier, everywhere. Consolidates
/// `codegen_type_name_keeps_qualifiers_around_typeof` and
/// `codegen_specifiers_may_follow_a_complete_type_specifier`.
#[test]
fn codegen_type_names_and_specifier_order() {
    compile_and_run_everywhere(
        "type_names_and_specifier_order",
        TYPE_NAMES_AND_SPECIFIER_ORDER,
    );
}

/// One section per consolidated test, each under that test's own
/// documentation. Exit codes:
/// `codegen_a_field_store_does_not_widen_over_its_neighbour` 11..=17.
/// `codegen_a_narrow_store_into_a_wide_slot_clears_it` 21..=24.
const NARROW_STORES_INTO_EIGHT_BYTE_SLOTS: &str = r#"
/* ====================================================================== */
/* codegen_a_field_store_does_not_widen_over_its_neighbour: exit codes 11..17 */
// Storing one field of an eight-byte aggregate leaves the other alone.
//
// The x86-64 store lowering widens a 32-bit store at offset 0 of a local to
// 64 bits, to clear stale upper bits when a narrow value goes into a wider
// slot. Its own comment records the exception that needs: "struct/union
// fields at offset 0 must use exact size to avoid clobbering the adjacent
// field at offset 4". The exception asked whether the object was *larger than*
// 64 bits, which an eight-byte aggregate is not -- so exactly the case the
// comment describes was the one that fell through.
//
// Only at `-O0`: with the optimizer on, the field is promoted out of memory
// before the store lowering sees it.
struct P { int x, y; };
struct S { struct P t; };
union U { struct P p; double d; };

static int t_codegen_a_field_store_does_not_widen_over_its_neighbour(void)
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

/* ====================================================================== */
/* codegen_a_narrow_store_into_a_wide_slot_clears_it: exit codes 21..24 */
// The control: a narrow value stored into a wider scalar slot still leaves no
// stale upper bits.
//
// This is what the widening is for, and it is why the fix has to ask whether
// the object is an aggregate rather than simply stop widening. Each case
// writes a wide value into the slot first, so a store that failed to clear the
// upper half would read it back.
int wide(void) { return -1; }

static int t_codegen_a_narrow_store_into_a_wide_slot_clears_it(void)
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

int main(void)
{
    int r;
    if ((r = t_codegen_a_field_store_does_not_widen_over_its_neighbour()) != 0) return 10 + r;
    if ((r = t_codegen_a_narrow_store_into_a_wide_slot_clears_it()) != 0) return 20 + r;
    return 0;
}
"#;

/// A field store into an eight-byte aggregate, and a narrow store into a
/// wide scalar slot, at -O0, the matrix levels and -O1. Consolidates
/// `codegen_a_field_store_does_not_widen_over_its_neighbour` and
/// `codegen_a_narrow_store_into_a_wide_slot_clears_it`.
#[test]
fn codegen_narrow_stores_into_eight_byte_slots() {
    // `-O0` explicitly: the default matrix compiles at `-O`, where the field is
    // promoted out of memory before the store lowering ever sees it, so the
    // defect is invisible there.
    let src = NARROW_STORES_INTO_EIGHT_BYTE_SLOTS;
    assert_eq!(
        compile_and_run("narrow_stores", src, &["-O0".to_string()]),
        0
    );
    assert_eq!(compile_and_run("narrow_stores_matrix", src, &[]), 0);
    assert_eq!(compile_and_run_optimized("narrow_stores_opt", src), 0);
}
