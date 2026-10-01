//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Calls and their arguments: argument registers, stack-passed
// parameters, and what a call may clobber.
//

use crate::common::{
    compile_and_run, compile_and_run_aarch64, compile_and_run_everywhere,
    compile_and_run_optimized, compile_with_host_cc,
};

// ============================================================================
// Test: Large struct parameter ABI (> 16 bytes passed by value on stack)
// ============================================================================

#[test]
fn codegen_large_struct_param_abi() {
    let code = r#"
#include <stdio.h>
#include <string.h>

/* 32-byte struct: must be passed by value on the stack per SysV AMD64 ABI */
struct Big {
    long a;
    long b;
    long c;
    long d;
};

/* 24-byte struct */
struct Medium {
    long x;
    long y;
    long z;
};

/* 40-byte struct */
struct Bigger {
    long v[5];
};

/* --- Section 1: Basic large struct parameter passing --- */

int check_big(struct Big s) {
    if (s.a != 10) return 1;
    if (s.b != 20) return 2;
    if (s.c != 30) return 3;
    if (s.d != 40) return 4;
    return 0;
}

int check_medium(struct Medium s) {
    if (s.x != 100) return 5;
    if (s.y != 200) return 6;
    if (s.z != 300) return 7;
    return 0;
}

int check_bigger(struct Bigger s) {
    if (s.v[0] != 1) return 8;
    if (s.v[1] != 2) return 9;
    if (s.v[2] != 3) return 10;
    if (s.v[3] != 4) return 11;
    if (s.v[4] != 5) return 12;
    return 0;
}

/* --- Section 2: Large struct with other args (register pressure) --- */

int check_big_with_int(int before, struct Big s, int after) {
    if (before != 99) return 13;
    if (s.a != 10) return 14;
    if (s.b != 20) return 15;
    if (s.c != 30) return 16;
    if (s.d != 40) return 17;
    if (after != 77) return 18;
    return 0;
}

/* --- Section 3: Multiple large struct params --- */

int check_two_bigs(struct Big s1, struct Big s2) {
    if (s1.a != 1) return 19;
    if (s1.d != 4) return 20;
    if (s2.a != 5) return 21;
    if (s2.d != 8) return 22;
    return 0;
}

/* --- Section 4: Large struct return + parameter (sret + stack param) --- */

struct Big make_and_check(struct Big input) {
    struct Big result;
    result.a = input.a + 1;
    result.b = input.b + 1;
    result.c = input.c + 1;
    result.d = input.d + 1;
    return result;
}

/* --- Section 5: Nested call with large struct --- */

int nested_check(struct Big s) {
    return check_big(s);
}

int main(void) {
    int rc;

    /* Section 1: Basic */
    struct Big b = {10, 20, 30, 40};
    rc = check_big(b);
    if (rc) return rc;

    struct Medium m = {100, 200, 300};
    rc = check_medium(m);
    if (rc) return rc;

    struct Bigger bg = {{1, 2, 3, 4, 5}};
    rc = check_bigger(bg);
    if (rc) return rc;

    /* Section 2: Mixed with int args */
    rc = check_big_with_int(99, b, 77);
    if (rc) return rc;

    /* Section 3: Multiple large structs */
    struct Big b1 = {1, 2, 3, 4};
    struct Big b2 = {5, 6, 7, 8};
    rc = check_two_bigs(b1, b2);
    if (rc) return rc;

    /* Section 4: Return + parameter */
    struct Big b3 = make_and_check(b);
    if (b3.a != 11) return 23;
    if (b3.b != 21) return 24;
    if (b3.c != 31) return 25;
    if (b3.d != 41) return 26;

    /* Section 5: Nested call */
    rc = nested_check(b);
    if (rc) return rc + 26;

    printf("OK\n");
    return 0;
}
"#;

    let exit_code = compile_and_run("large_struct_param_abi", code, &[]);
    assert_eq!(
        exit_code, 0,
        "Large struct param ABI test failed with exit code {}",
        exit_code
    );
}

/// `sizeof` of a variably-modified *type-name* is computed at run time, and
/// evaluates its size expressions exactly once (C17 6.5.3.4p2).
///
/// The test above covers `sizeof` of a declared VLA *object*, which worked.
/// The type-name form answered **0** at every shape, because the dimension
/// expressions are dropped where the type-name is parsed and cannot be
/// recovered afterwards -- `int[n]`, `int[m]` and `int[]` all intern to one
/// `TypeId`, and the array arm of `size_bits` reads its absent extent as zero.
///
/// Two consequences beyond the wrong number, both covered here: the size
/// expression was never evaluated at all, so `sizeof(int[f()])` called `f`
/// zero times; and the bogus 0 was still an integer constant expression, so
/// `int z[sizeof(int[n])];` silently became a zero-length array.
#[test]
fn codegen_sizeof_of_a_variably_modified_type_name() {
    let code = r#"
int calls;
int f(void) { calls++; return 4; }

int main(void) {
    int n = 4, m = 3;

    /* ===== the value, at every shape (returns 1-9) ===== */
    if (sizeof(int[n])      != 16) return 1;
    if (sizeof(int[3][n])   != 48) return 2;
    if (sizeof(int[n][3])   != 48) return 3;
    if (sizeof(int[n][m])   != 48) return 4;
    if (sizeof(int[n+1])    != 20) return 5;
    if (sizeof(char[n])     != 4)  return 6;
    if (sizeof(long[n])     != 32) return 7;

    /* ===== controls that already worked (returns 10-19) ===== */
    int a[n];
    int b[3][n];
    if (sizeof a           != 16) return 10;
    if (sizeof b           != 48) return 11;
    if (sizeof(int[4])     != 16) return 12;
    if (sizeof(int[3][4])  != 48) return 13;
    if (sizeof(int(*)[n])  != sizeof(void*)) return 14;   /* a pointer */

    /* ===== the operand is evaluated, exactly once (returns 20-29) ===== */
    calls = 0;
    if (sizeof(int[f()]) != 16) return 20;
    if (calls != 1) return 21;

    /* once per evaluation of the sizeof, i.e. per iteration */
    calls = 0;
    for (int i = 0; i < 3; i++) {
        if (sizeof(int[f()]) != 16) return 22;
    }
    if (calls != 3) return 23;

    /* not evaluated when control never reaches it */
    calls = 0;
    if (0 && sizeof(int[f()]) == 16) return 24;
    if (calls != 0) return 25;

    calls = 0;
    { int cond = 0; unsigned long z = cond ? sizeof(int[f()]) : 7u; if (z != 7) return 26; }
    if (calls != 0) return 27;

    /* a pointer-to-VLA type-name does NOT evaluate its extent (gcc agrees) */
    calls = 0;
    if (sizeof(int(*)[f()]) != sizeof(void*)) return 28;
    if (calls != 0) return 29;

    /* ===== the result is not an integer constant expression (30-39) ===== */
    /* it is a run-time value, so an array declared with it is itself a VLA */
    { int z[sizeof(int[n])]; if (sizeof z != 64) return 30; }

    return 0;
}
"#;
    assert_eq!(compile_and_run("sizeof_vm_type_name", code, &[]), 0);
    assert_eq!(
        compile_and_run_optimized("sizeof_vm_type_name_opt", code),
        0
    );
}

// ============================================================================
// Regression: XMM registers must be spilled across function calls
// ============================================================================

#[test]
fn codegen_xmm_spill_across_calls() {
    let code = r#"
#include <math.h>
#include <stdio.h>

/* External function to force a real call (prevents inlining) */
extern int check_type(void *ptr);

struct obj {
    int type_tag;
    double value;
};

int check_type(void *ptr) {
    struct obj *o = (struct obj *)ptr;
    return o->type_tag == 1;
}

double get_value(struct obj *o) {
    return o->value;
}

int main(void) {
    struct obj a = { .type_tag = 1, .value = 1.5 };
    struct obj b = { .type_tag = 1, .value = 2.5 };

    /* Load a.value, then make a function call, then use a.value */
    double va = get_value(&a);
    int is_valid = check_type(&b);  /* call between load and use */
    double vb = get_value(&b);

    if (!is_valid) return 1;

    /* va should still be 1.5, not clobbered by the call */
    if (va != 1.5) return 10;
    if (vb != 2.5) return 11;
    if (va == vb) return 12;  /* they must not be equal */

    /* Test with math library calls */
    double x = exp(1.0);
    double y = sqrt(4.0);  /* call between loads */
    double z = exp(2.0);

    /* x should still be e^1, not clobbered */
    if (x < 2.71 || x > 2.72) return 20;
    if (y < 1.99 || y > 2.01) return 21;
    if (z < 7.38 || z > 7.40) return 22;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("xmm_spill_across_calls", code, &["-lm".to_string()]),
        0
    );
}

/// Test: C11 nullability qualifiers (_Nonnull, _Nullable, _Null_unspecified)
/// are parsed and ignored in pointer declarators and function parameters.
/// macOS system headers use these extensively.
#[test]
fn codegen_nullability_qualifiers() {
    let code = r#"
/* Nullability qualifiers on pointer declarators */
int * _Nonnull get_ptr(int * _Nullable p) {
    static int fallback = 0;
    return p ? p : &fallback;
}

/* Nullability on function pointer */
typedef void (* _Nonnull callback_t)(int);

void invoke(callback_t cb, int val) {
    cb(val);
}

static int captured = 0;
void capture(int v) { captured = v; }

/* _Null_unspecified variant */
int * _Null_unspecified identity_ptr(int * _Null_unspecified p) {
    return p;
}

int main(void) {
    int x = 42;
    int *p = get_ptr(&x);
    if (*p != 42) return 1;

    int *q = get_ptr((int * _Nullable)0);
    if (*q != 0) return 2;

    invoke(capture, 99);
    if (captured != 99) return 3;

    int *r = identity_ptr(&x);
    if (*r != 42) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("nullability_qualifiers", code, &[]), 0);
}

/// AAPCS64 derives an argument's alignment from the members and ignores the
/// type's own `__attribute__((aligned(N)))`.
///
/// c17 asked `types.alignment()`, which includes that attribute, at three
/// aarch64 sites that then disagreed with each other: the caller and the
/// callee both rounded a 32-byte-aligned struct's stack slot to 32, while
/// `va_arg` capped at 16. Caller and callee agreeing is why a *named* call
/// worked c17-to-c17 and failed against gcc; `va_arg` differing is why a
/// *variadic* one failed even c17-to-c17. gcc rounds all three to 8.
///
/// The shapes below are the ones that separate the candidate rules. A type's
/// own attribute must not count; a *member's* must; packing must, floored at
/// 8; an attributed typedef must not; and a naturally 16-aligned type keeps
/// its 16. Nine leading doubles or longs put the argument past the register
/// file so the stack rule is the one under test.
///
/// aarch64 only: the rule is AAPCS64's, and x86-64's SysV genuinely honours
/// over-alignment -- that side is covered by
/// `codegen_over_aligned_argument_area`.
#[test]
fn codegen_aarch64_argument_alignment_follows_the_members() {
    let code = r#"
#include <stdarg.h>

struct Plain  { double a, b, c, d; };                                  /* members want 8  */
struct __attribute__((aligned (32))) Over { double a, b, c, d; };      /* attribute: 32   */
struct __attribute__((aligned (16))) Over16 { long long a, b; };       /* attribute: 16   */
struct MemAl  { long long a __attribute__((aligned (16))); long long b; }; /* member: 16   */
struct Packed { __int128 x; } __attribute__((packed));                 /* packed to 1     */
struct Nat16  { __int128 x; };                                         /* natural 16      */

#define NAMED(NAME, TY, CHECK)                                            \
    __attribute__((noinline)) static int NAME(double p0, double p1,       \
        double p2, double p3, double p4, double p5, double p6, double p7, \
        double p8, TY s, int tail)                                        \
    { (void)p0; (void)p8; return ((CHECK) && tail == 7) ? 0 : 1; }

NAMED(n_plain,  struct Plain,  s.a == 1 && s.d == 4)
NAMED(n_over,   struct Over,   s.a == 1 && s.d == 4)
NAMED(n_over16, struct Over16, s.a == 1 && s.b == 2)
NAMED(n_memal,  struct MemAl,  s.a == 1 && s.b == 2)
NAMED(n_packed, struct Packed, (long long)s.x == 42)
NAMED(n_nat16,  struct Nat16,  (long long)s.x == 42)

#define VA(NAME, TY, CHECK)                                    \
    __attribute__((noinline)) static int NAME(int n, ...)      \
    {                                                          \
        va_list ap; va_start(ap, n);                           \
        while (n--) (void)va_arg(ap, double);                  \
        TY s = va_arg(ap, TY);                                 \
        int tail = va_arg(ap, int);                            \
        va_end(ap);                                            \
        return ((CHECK) && tail == 7) ? 0 : 1;                 \
    }

VA(v_plain,  struct Plain,  s.a == 1 && s.d == 4)
VA(v_over,   struct Over,   s.a == 1 && s.d == 4)
VA(v_over16, struct Over16, s.a == 1 && s.b == 2)
VA(v_memal,  struct MemAl,  s.a == 1 && s.b == 2)
VA(v_packed, struct Packed, (long long)s.x == 42)
VA(v_nat16,  struct Nat16,  (long long)s.x == 42)

#define D9 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0

int main(void)
{
    /* The rule under test is AAPCS64's, and the shape that reaches it trips a
       separate x86-64 defect (see the Rust doc comment). Returning success
       elsewhere keeps this program honest when it is extracted and run on the
       host, as the aarch64 sweep script does. */
#if !defined(__aarch64__)
    return 0;
#else
    struct Plain  pl = { 1, 2, 3, 4 };
    struct Over   ov = { 1, 2, 3, 4 };
    struct Over16 o16 = { 1, 2 };
    struct MemAl  ma = { 1, 2 };
    struct Packed pk; pk.x = 42;
    struct Nat16  n16; n16.x = 42;

    if (n_plain(D9, pl, 7))  return 1;
    if (n_over(D9, ov, 7))   return 2;
    if (n_over16(D9, o16, 7)) return 3;
    if (n_memal(D9, ma, 7))  return 4;
    if (n_packed(D9, pk, 7)) return 5;
    if (n_nat16(D9, n16, 7)) return 6;

    if (v_plain(9, D9, pl, 7))  return 7;
    if (v_over(9, D9, ov, 7))   return 8;
    if (v_over16(9, D9, o16, 7)) return 9;
    if (v_memal(9, D9, ma, 7))  return 10;
    if (v_packed(9, D9, pk, 7)) return 11;
    if (v_nat16(9, D9, n16, 7)) return 12;

    return 0;
#endif
}
"#;
    for opt in ["-O0", "-O2"] {
        // Native when the host *is* aarch64 (the CI runner), cross-compiled
        // under qemu otherwise. Never run on an x86-64 host, for the reason in
        // the doc comment.
        if cfg!(target_arch = "aarch64") {
            assert_eq!(
                compile_and_run(
                    &format!("codegen_arg_align_members{opt}"),
                    code,
                    &[opt.to_string()]
                ),
                0,
                "native aarch64 at {opt}"
            );
        }
        if let Some(status) = compile_and_run_aarch64("codegen_arg_align_members_a64", code, opt) {
            assert_eq!(status, 0, "aarch64 at {opt}");
        }
    }
}

/// An argument more aligned than the call boundary needs the outgoing area's
/// *base* aligned, not just its offset within the area.
///
/// System V AMD64 places such an argument at an offset rounded to its own
/// alignment, and gcc makes that meaningful by dynamically realigning the
/// caller's stack so the area starts there too. c17 rounded the offset and
/// left the base at 16, and `va_arg` rounded the overflow pointer to a fixed
/// 16 rather than to the argument's alignment -- two errors in the same
/// direction, so a c17-built program agreed with itself and disagreed with
/// gcc by sixteen bytes.
///
/// That is why the load-bearing half of this test links c17 against the host
/// compiler in both directions: the behavioural run below passes on the
/// *unfixed* compiler too.
#[test]
fn codegen_over_aligned_argument_area() {
    let code = r#"
#include <stdarg.h>

struct __attribute__((aligned (32))) A32 { double a, b, c, d; };
struct __attribute__((aligned (64))) A64 { long q[4]; };
struct __attribute__((aligned (16))) A16 { long long a, b; };

__attribute__((noinline)) static int take32(int lead, struct A32 s)
{
    return (s.a == 1 && s.b == 2 && s.c == 3 && s.d == 4 && lead == 7) ? 0 : 1;
}

__attribute__((noinline)) static int take64(int lead, struct A64 s)
{
    return (s.q[0] == 5 && s.q[3] == 8 && lead == 7) ? 0 : 1;
}

/* Seven leading integers push the aggregate past the register file. */
__attribute__((noinline)) static int spilled32(int a, int b, int c, int d,
                                               int e, int f, int g,
                                               struct A32 s, int after)
{
    return (s.a == 1 && s.d == 4 && a == 1 && g == 7 && after == 99) ? 0 : 1;
}

/* `va_arg` rounds the overflow pointer to the argument's own alignment. The
   leading doubles push it past the SSE file so it really comes off the
   stack. */
__attribute__((noinline)) static int va32(int n, ...)
{
    va_list ap;
    va_start(ap, n);
    while (n--) (void)va_arg(ap, double);
    struct A32 s = va_arg(ap, struct A32);
    int after = va_arg(ap, int);
    va_end(ap);
    return (s.a == 1 && s.b == 2 && s.c == 3 && s.d == 4 && after == 99) ? 0 : 1;
}

__attribute__((noinline)) static int va64(int n, ...)
{
    va_list ap;
    va_start(ap, n);
    while (n--) (void)va_arg(ap, double);
    struct A64 s = va_arg(ap, struct A64);
    int after = va_arg(ap, int);
    va_end(ap);
    return (s.q[0] == 5 && s.q[3] == 8 && after == 99) ? 0 : 1;
}

__attribute__((noinline)) static int mixed(struct A16 p, struct A32 q, int tail)
{
    return (p.a == 10 && p.b == 11 && q.a == 1 && q.d == 4 && tail == 55) ? 0 : 1;
}

int main(void)
{
    struct A32 s32 = { 1, 2, 3, 4 };
    struct A64 s64 = { { 5, 6, 7, 8 } };
    struct A16 s16 = { 10, 11 };

    if (take32(7, s32)) return 1;
    if (take64(7, s64)) return 2;
    if (spilled32(1, 2, 3, 4, 5, 6, 7, s32, 99)) return 3;
    /* Nine leading doubles is the count that separates rounding to 16 from
       rounding to the argument's own alignment: with fewer, the two land in
       the same place and the defect is invisible. */
    if (va32(0, s32, 99)) return 4;
    if (va32(1, 1.0, s32, 99)) return 4;
    if (va32(5, 1.0, 2.0, 3.0, 4.0, 5.0, s32, 99)) return 4;
    if (va32(9, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, s32, 99)) return 4;
    if (va64(0, s64, 99)) return 5;
    if (va64(9, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, s64, 99)) return 5;
    if (mixed(s16, s32, 55)) return 6;
    return 0;
}
"#;
    for opt in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run(
                &format!("codegen_over_aligned_arg{opt}"),
                code,
                &[opt.to_string()]
            ),
            0,
            "at {opt}"
        );
    }

    // The part that actually pins the ABI: one unit from c17, the other from
    // the host compiler, in both directions.
    const CALLEE: &str = r#"
#include <stdarg.h>
struct __attribute__((aligned (32))) A32 { double a, b, c, d; };

int named(int lead, struct A32 s, int tail)
{
    return (lead == 7 && s.a == 1 && s.d == 4 && tail == 9) ? 0 : 1;
}

int variadic(int n, ...)
{
    va_list ap;
    va_start(ap, n);
    while (n--) (void)va_arg(ap, double);
    struct A32 s = va_arg(ap, struct A32);
    int tail = va_arg(ap, int);
    va_end(ap);
    return (s.a == 1 && s.d == 4 && tail == 9) ? 0 : 1;
}
"#;
    const CALLER: &str = r#"
struct __attribute__((aligned (32))) A32 { double a, b, c, d; };
int named(int lead, struct A32 s, int tail);
int variadic(int n, ...);

int main(void)
{
    struct A32 s = { 1, 2, 3, 4 };
    if (named(7, s, 9)) return 1;
    if (variadic(0, s, 9)) return 2;
#if !defined(__APPLE__)
    /* Not against clang: it disagrees with itself here, so no compiler can
       satisfy this in both directions. Measured twice on macOS CI -- its
       caller stacks the over-aligned aggregate at the next eight-byte
       granule (offset 72) and its `va_arg` rounds the cursor up to the
       type's 32, reading offset 96. A program built entirely with clang
       has the same defect. Whichever of the two c17 matches, the other
       direction of this cross-check fails; `cc/DECISIONS.md` records which.
       The pure-c17 runs above still cover the shape, and the named
       argument and the no-leading-argument variadic are checked against
       clang in both directions. */
    if (variadic(9, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, s, 9)) return 3;
#endif
    return 0;
}
"#;
    if let Some(status) = compile_with_host_cc("over_aligned_callee", CALLEE, CALLER) {
        assert_eq!(status, 0, "a host-compiled caller must reach a c17 callee");
    }
    if let Some(status) = compile_with_host_cc("over_aligned_caller", CALLER, CALLEE) {
        assert_eq!(status, 0, "a c17 caller must reach a host-compiled callee");
    }
}

/// A local whose alignment exceeds the stack's own forces the frame to be
/// addressed through a second base register. That register was still in the
/// allocatable pool, so the colorer handed it to an ordinary value and the
/// prologue's base was overwritten by the first thing that outlived a call --
/// every later local access then read through whatever integer that was.
///
/// The call is what makes it bite: it forces a callee-saved register, and the
/// base is callee-saved. The pressure below keeps enough values live across
/// the call that the base is reached rather than left spare.
#[test]
fn codegen_over_aligned_frame_base_reserved() {
    let code = r#"
int sink(int v) { return v; }

int over_aligned(int x) {
    _Alignas(64) char buf[128];
    int a = x + 1, b = x + 2, c = x + 3, d = x + 4;
    int e = x + 5, f = x + 6, g = x + 7, h = x + 8;
    buf[0] = 1;
    buf[64] = 2;
    /* every value above stays live across this call */
    int r = sink(x);
    if (((unsigned long)&buf[0] & 63UL) != 0UL) return 100;
    if (buf[0] != 1 || buf[64] != 2) return 101;
    if (a + b + c + d + e + f + g + h != 8 * x + 36) return 102;
    return r;
}

int main(void) {
    if (over_aligned(7) != 7) return 1;
    if (over_aligned(0) != 0) return 2;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_over_aligned_frame_base", code, &[]),
        0
    );
    assert_eq!(
        compile_and_run_optimized("codegen_over_aligned_frame_base_opt", code),
        0
    );
}

/// A struct small enough to travel in a register travels as its *value*, and
/// the value has to be loaded at the struct's width -- not at the largest
/// power of two that fits inside it.
///
/// Only three bytes reproduce this. Sizes 1, 2, 4 and 8 are machine widths;
/// 5, 6 and 7 take a different path; 3 is the one size that rounded *down*,
/// so a `struct { char b[3]; }` argument arrived with its first byte and two
/// zeros.
///
/// It also only reproduces for an rvalue -- `p(r())` rather than
/// `p(local)` -- because a named struct is passed from its address. Both
/// forms are asserted here; only the second half ever failed.
#[test]
fn codegen_small_struct_argument_travels_at_its_own_width() {
    let code = r#"
#define DEF(N)                                                              \
    struct S##N { char b[N]; };                                             \
    __attribute__((noinline)) static long p##N(struct S##N v) {             \
        long t = 0;                                                         \
        for (int i = 0; i < N; i++) t = t * 7 + v.b[i];                     \
        return t;                                                           \
    }                                                                       \
    __attribute__((noinline)) static struct S##N r##N(void) {               \
        struct S##N v;                                                      \
        for (int i = 0; i < N; i++) v.b[i] = (char)(i + 1);                 \
        return v;                                                           \
    }

DEF(1) DEF(2) DEF(3) DEF(4) DEF(5) DEF(6) DEF(7) DEF(8)
DEF(9) DEF(10) DEF(11) DEF(12) DEF(13) DEF(14) DEF(15) DEF(16)

/* t = sum over i of (i+1) * 7^(N-1-i) */
static long expected(int n) {
    long t = 0;
    for (int i = 0; i < n; i++) t = t * 7 + (i + 1);
    return t;
}

#define CHECK(N)                                                            \
    do {                                                                    \
        struct S##N local = r##N();                                         \
        if (p##N(local) != expected(N)) return N;                           \
        if (p##N(r##N()) != expected(N)) return 100 + N;                    \
    } while (0)

int main(void) {
    CHECK(1);  CHECK(2);  CHECK(3);  CHECK(4);
    CHECK(5);  CHECK(6);  CHECK(7);  CHECK(8);
    CHECK(9);  CHECK(10); CHECK(11); CHECK(12);
    CHECK(13); CHECK(14); CHECK(15); CHECK(16);

    /* A three-byte struct with a member of each kind, not just a char
       array -- the width comes from the struct, not from what is in it. */
    {
        struct M { char a; short b; };
        struct M m;
        m.a = 5; m.b = 0x1234;
        if (sizeof(struct M) != 4) return 200;
        if (m.a != 5 || m.b != 0x1234) return 201;
    }
    return 0;
}
"#;
    assert_eq!(
        compile_and_run("codegen_small_struct_arg_width", code, &[]),
        0
    );
    assert_eq!(
        compile_and_run_optimized("codegen_small_struct_arg_width_opt", code),
        0
    );
}

/// A zero-sized parameter occupies neither a register nor a stack slot, and
/// both sides of the call have to step over it the same way.
///
/// They did not. The call site's layout skipped it while the register setup
/// and the callee's prologue each charged a general register for it, so every
/// later argument was read from the register before the one it was written
/// to -- `f(z, 1, 2, ...)` lost its first `int`. With nine arguments past the
/// zero-sized one the index ran off the end of the six-register file and the
/// **compiler panicked**: `index out of bounds: the len is 6 but the index is
/// 6`, which is how `va-arg-22` failed to compile at all.
///
/// A zero-sized struct is a GNU extension, and `struct { char x[0]; }` and
/// `struct { }` are both spellings of it.
#[test]
fn codegen_zero_sized_parameter_consumes_no_register() {
    let code = r#"
typedef struct { char x[0]; } Z;
typedef struct { } E;
Z z;
E e;

/* The zero-sized argument in every position around a full register file. */
int first(Z q, int a, int b, int c, int d, int e2, int f, int g, int h) {
    (void)q; return a + b + c + d + e2 + f + g + h;
}
int middle(int a, int b, int c, Z q, int d, int e2, int f, int g, int h) {
    (void)q; return a + b + c + d + e2 + f + g + h;
}
int last(int a, int b, int c, int d, int e2, int f, int g, int h, Z q) {
    (void)q; return a + b + c + d + e2 + f + g + h;
}
int two(Z p, Z q, int a, int b, int c, int d, int e2, int f, int g, int h) {
    (void)p; (void)q; return a + b + c + d + e2 + f + g + h;
}
/* The empty-struct spelling, and a parameter the body actually reads. */
int empty(E q, int a, int b) { (void)q; return a * 10 + b; }
int weighted(Z q, int a, int b, int c, int d, int e2, int f, int g) {
    (void)q; return a * 1 + b * 2 + c * 3 + d * 4 + e2 * 5 + f * 6 + g * 7;
}
/* Mixed with a two-register struct, past the file. */
typedef struct { int a, b; } P;
int mixed(P p1, P p2, P p3, Z q, int a, int b, int c) {
    (void)q; return p1.a + p2.a + p3.a + a + b + c;
}
/* Variadic, with the zero-sized one among the fixed parameters. */
int variadic(Z q, int n, ...) { (void)q; return n; }
/* Returned by value, and passed several times over. */
Z ret_zero(void) { return z; }
int chain(Z q1, Z q2, Z q3, int a) { (void)q1; (void)q2; (void)q3; return a; }

/* Sub-`int` arguments spilling to the stack: an argument that did not fit
   consumes no register either, and counting one made the two sides disagree
   about every argument after it. */
char narrow(char a, char b, char c, char d, char e2,
            char f, char g, char h, char i, char j) {
    return (char)(a + b + c + d + e2 + f + g + h + i + j);
}

int main(void) {
    if (first(z, 1, 2, 3, 4, 5, 6, 7, 8) != 36) return 1;
    if (middle(1, 2, 3, z, 4, 5, 6, 7, 8) != 36) return 2;
    if (last(1, 2, 3, 4, 5, 6, 7, 8, z) != 36) return 3;
    if (two(z, z, 1, 2, 3, 4, 5, 6, 7, 8) != 36) return 4;
    if (empty(e, 3, 4) != 34) return 5;
    if (weighted(z, 1, 2, 3, 4, 5, 6, 7) != 140) return 6;

    P a = {1, 0}, b = {2, 0}, c = {3, 0};
    if (mixed(a, b, c, z, 4, 5, 6) != 21) return 7;

    if (variadic(z, 42, 1, 2, 3) != 42) return 8;
    Z r = ret_zero();
    (void)r;
    if (chain(z, z, z, 99) != 99) return 9;
    if (narrow(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) != 55) return 10;
    if (sizeof(Z) != 0 || sizeof(E) != 0) return 11;
    return 0;
}
"#;
    assert_eq!(compile_and_run("cg_zero_sized_param", code, &[]), 0);
    assert_eq!(
        compile_and_run("cg_zero_sized_param_o2", code, &["-O2".to_string()]),
        0
    );
}

/// A call through a function pointer converts its arguments to the pointee's
/// prototype, and the pointer survives the argument setup.
///
/// Two defects, both reached by any indirect call.
///
/// C17 6.5.2.2p1 lets the function designator be a function *or* a pointer to
/// one, and the prototype is on the function type either way. c17 read
/// `params` off the pointer, found none, and converted nothing: `void
/// (*p)(double) = f; p(1);` passed the integer 1 where a `double` was
/// expected and the callee read 0. With a mixed argument list every later
/// argument moved as well.
///
/// The pointer was then loaded into R11 *before* the arguments were set up --
/// and R10/R11 are that setup's own scratch, so the target was overwritten
/// and `call *%r11` jumped into whatever the last argument had addressed.
///
/// `930702-1` is the torture test, through a K&R definition.
#[test]
fn c99_call_through_a_pointer_uses_the_pointee_prototype() {
    let code = r#"
extern int printf(const char *, ...);

static double seen_d;
static int seen_i;
static long seen_l;
static float seen_f;

static void take_d(double a) { seen_d = a; }
static void take_di(double a, int b) { seen_d = a; seen_i = b; }
static void take_l(long a) { seen_l = a; }
static void take_f(float a) { seen_f = a; }
static void take_idi(int a, double b, int c) { seen_i = a + c; seen_d = b; }
static void take_b(_Bool b) { seen_i = b; }

typedef struct { double a, b; } D2;
typedef struct { long a, b, c, d; } Big;
static double st_a, st_b;
static long big_a;
static void take_st(D2 d, Big b, int i, double z, long l) {
    st_a = d.a; st_b = d.b; big_a = b.a; seen_i = i; seen_d = z; seen_l = l;
}
static double cre, cim;
static void take_cx(double _Complex z) { cre = __real__ z; cim = __imag__ z; }

static void take_stacked(long a, long b, long c, long d, long e,
                         long f, long g, long h, Big k, long l) {
    seen_l = a; big_a = k.a; seen_i = (int)l;
    (void)b; (void)c; (void)d; (void)e; (void)f; (void)g; (void)h;
}

/* The target reached through an array indexed at run time, and through a
   call -- the pointer has to survive a full argument list either way. */
typedef void (*DI)(double, int);
static DI tbl[2];
static DI pick(int i) { return tbl[i]; }

int main(void) {
    /* The conversion the prototype asks for. */
    { void (*p)(double) = take_d; p(1); if (seen_d != 1.0) return 1; }
    { void (*p)(double, int) = take_di; p(2, 7);
      if (seen_d != 2.0 || seen_i != 7) return 2; }
    { void (*p)(long) = take_l; p(3); if (seen_l != 3) return 3; }
    { void (*p)(float) = take_f; p(4); if (seen_f != 4.0f) return 4; }
    { void (*p)(int, double, int) = take_idi; p(5, 6, 8);
      if (seen_i != 13 || seen_d != 6.0) return 5; }

    /* `_Bool` converts as `!= 0`, not by truncation -- and this was wrong
       for a direct call too. */
    { void (*p)(_Bool) = take_b; p(42); if (seen_i != 1) return 6; }
    take_b(42);
    if (seen_i != 1) return 7;

    /* Aggregates and a complex parameter, where the argument setup uses the
       scratch registers the target was parked in. */
    { void (*p)(D2, Big, int, double, long) = take_st;
      D2 d = {1.5, 2.5}; Big b = {7, 8, 9, 10};
      p(d, b, 3, 4.5, 11);
      if (st_a != 1.5 || st_b != 2.5 || big_a != 7) return 8;
      if (seen_i != 3 || seen_d != 4.5 || seen_l != 11) return 9; }
    { void (*p)(double _Complex) = take_cx; p(4);
      if (cre != 4.0 || cim != 0.0) return 10; }

    /* The target from a table, and from a call. */
    tbl[0] = take_di;
    tbl[1] = take_di;
    { int k = 1; tbl[k](12, 13);
      if (seen_d != 12.0 || seen_i != 13) return 11; }
    pick(0)(14, 15);
    if (seen_d != 14.0 || seen_i != 15) return 12;

    /* A stacked aggregate argument, which both backends copy through the
       very register the call target sits in -- X16 on aarch64, R11 on
       x86-64. This is the shape that branched into the argument data. */
    {
        void (*p)(long, long, long, long, long, long, long, long, Big, long)
            = take_stacked;
        Big b = {77, 88, 99, 100};
        p(1, 2, 3, 4, 5, 6, 7, 8, b, 9);
        if (seen_l != 1 || big_a != 77 || seen_i != 9) return 14;
    }

    /* A direct call must keep working the same way. */
    take_d(16);
    if (seen_d != 16.0) return 15;
    return 0;
}
"#;
    assert_eq!(compile_and_run("c99_call_through_pointer", code, &[]), 0);
    assert_eq!(
        compile_and_run("c99_call_through_pointer_o2", code, &["-O2".to_string()]),
        0
    );
}

/// If-conversion must not make the right operand of `&&` run when the left
/// already decided.
///
/// This is the guarantee C makes and the reason the diamond exists at all.
/// Collapsing one whose arm calls a function, touches memory, or can trap
/// would be a miscompile that only shows up when the guard was load-bearing --
/// which is the usual reason the guard was written.
#[test]
fn codegen_short_circuit_still_short_circuits() {
    let code = r#"
#include <stdlib.h>

int calls;
__attribute__((noinline)) static int bump(void) { calls++; return 1; }

volatile int zero = 0;
volatile int one = 1;

int main(void)
{
    /* A call on the right of && must not run when the left is false. */
    if (zero && bump()) return 1;
    if (calls != 0) return 2;
    /* ...and must when it is true. */
    if (!(one && bump())) return 3;
    if (calls != 1) return 4;

    /* The || mirror. */
    if (!(one || bump())) return 5;
    if (calls != 1) return 6;
    if (!(zero || bump())) return 7;
    if (calls != 2) return 8;

    /* A division guarded by its own divisor must not be speculated: if the
       right operand ran unconditionally this traps. */
    { int d = zero; if (d != 0 && (100 / d) == 1) return 9; }

    /* A load guarded by a null check, likewise. */
    { int *p = (int *)0; if (p != 0 && *p == 0) return 10; }

    /* Side effects in the right operand happen exactly once. */
    { int n = 0; int r = (one && (n++, 1)); if (!r || n != 1) return 11; }

    return 0;
}
"#;
    for opt in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run("c17_short_circuit_guard", code, &[opt.to_string()]),
            0,
            "at {opt}"
        );
    }
}

/// A C source whose function takes `n` parameters of mixed classes -- `int`,
/// `double`, and a two-eightbyte struct -- and checks every one arrived. A
/// second function returns a large struct, so its parameters sit one `Arg`
/// index after the hidden return pointer.
fn many_params_source(n: usize) -> String {
    let param = |i: usize| match i % 3 {
        0 => format!("int p{i}"),
        1 => format!("double p{i}"),
        _ => format!("struct pair p{i}"),
    };
    let check = |i: usize| match i % 3 {
        0 => format!("if (p{i} != {i}) return {};\n", i + 1),
        1 => format!("if (p{i} != {i}.5) return {};\n", i + 1),
        _ => format!("if (p{i}.a != {i} || p{i}.b != -{i}) return {};\n", i + 1),
    };
    let arg = |i: usize| match i % 3 {
        0 => format!("{i}"),
        1 => format!("{i}.5"),
        _ => format!("(struct pair){{{i}, -{i}}}"),
    };
    let params: Vec<String> = (0..n).map(param).collect();
    let args: Vec<String> = (0..n).map(arg).collect();
    let checks: String = (0..n).map(check).collect();
    format!(
        "struct pair {{ long a, b; }};\n\
         struct big {{ long v[8]; }};\n\
         __attribute__((noinline)) int f({params}) {{\n{checks}return 0; }}\n\
         __attribute__((noinline)) struct big g({params}) {{\n\
         struct big r = {{{{0}}}};\n\
         r.v[0] = f({names});\n\
         return r; }}\n\
         int main(void) {{\n\
         int rc = f({args});\n\
         if (rc) return rc;\n\
         return (int)g({args}).v[0]; }}\n",
        params = params.join(", "),
        names = (0..n)
            .map(|i| format!("p{i}"))
            .collect::<Vec<_>>()
            .join(", "),
        args = args.join(", "),
    )
}

/// Each parameter's pseudo is found by one index lookup, not by scanning the
/// function's pseudos per parameter -- which made a function of 100,000
/// parameters (the gcc torture test `compile/limits-fndefn`) take minutes in
/// both backends. This pins that every parameter still reaches its own
/// register or stack slot across the mix of classes, with and without a
/// hidden return pointer.
#[test]
fn codegen_many_mixed_params_arrive_in_place() {
    let src = many_params_source(300);
    assert_eq!(compile_and_run("many_mixed_params", &src, &[]), 0);
    let opts = vec!["-O2".to_string()];
    assert_eq!(compile_and_run("many_mixed_params_o2", &src, &opts), 0);
    for opt in ["-O0", "-O2"] {
        if let Some(code) = compile_and_run_aarch64("many_mixed_params_a64", &src, opt) {
            assert_eq!(code, 0, "aarch64 at {opt}");
        }
    }
}

/// An attribute's integer argument is a constant expression, as it is to gcc.
/// The attribute parser read one token, and read that with Rust's `i64`
/// parser: `aligned(0x40)` and `aligned(16UL)` became 0 and were silently
/// ignored, `aligned(A)` for an enum constant and `aligned(sizeof(T))` were
/// dropped as unknown identifiers, and `vector_size(2 * sizeof(int))` was
/// rejected as "2 bytes".
#[test]
fn codegen_attribute_arguments_are_constant_expressions() {
    let src = r#"
#include <stdint.h>
enum { A = 64 };
#define LINE 0x40

char a __attribute__((aligned(0x40)));
char b __attribute__((aligned(16UL)));
char c __attribute__((aligned(A)));
char d __attribute__((aligned(sizeof(long double))));
char e __attribute__((aligned(2 * sizeof(int))));
char f __attribute__((aligned((LINE))));
struct S { char c; int x __attribute__((aligned(4 * sizeof(int)))); };
typedef int T __attribute__((aligned(0x20)));
typedef int V __attribute__((vector_size(2 * sizeof(int))));
typedef float W __attribute__((vector_size(sizeof(float) * 4)));
typedef unsigned char U __attribute__((vector_size(0x10)));

int main(void)
{
    char g __attribute__((aligned(0x20)));
    if (_Alignof(a) != 64 || (uintptr_t)&a % 64) return 1;
    if (_Alignof(b) != 16 || (uintptr_t)&b % 16) return 2;
    if (_Alignof(c) != 64 || (uintptr_t)&c % 64) return 3;
    if (_Alignof(d) != sizeof(long double)) return 4;
    if (_Alignof(e) != 2 * sizeof(int)) return 5;
    if (_Alignof(f) != 64 || (uintptr_t)&f % 64) return 6;
    if (_Alignof(struct S) != 16) return 7;
    if (_Alignof(T) != 32) return 8;
    if (sizeof(V) != 8 || sizeof(W) != 16 || sizeof(U) != 16) return 9;
    if ((uintptr_t)&g % 32) return 10;
    return 0;
}
"#;
    compile_and_run_everywhere("attribute_arguments", src);
}

/// The declaration specifiers are evaluated once per declaration, however
/// many declarators share them, so in `typeof(int[n++]) a, b;` gcc increments
/// `n` once and gives `a` and `b` one extent. c17 copied the size expression
/// into every declarator and evaluated it once each: `n` ended at 5 and `b`
/// was a different size from `a`. Also covered: a call as the extent, derived
/// declarators, a `for`-init, re-evaluation each time a loop reaches the
/// declaration, and `sizeof(typeof(int[n++]))` evaluating its operand once.
#[test]
fn codegen_typeof_vla_extent_is_evaluated_once_per_declaration() {
    let src = r#"
static int calls;
static int bump(int *p) { calls++; return (*p)++; }

static int one_declaration(void)
{
    int n = 3;
    /* One declaration, one evaluation: both objects get extent 3. */
    typeof(int[n++]) a, b;
    if (n != 4 || sizeof a != 3 * sizeof(int) || sizeof b != sizeof a)
        return 1;
    /* A function call as the extent, three declarators, a pointer and an
       array of the specifier type among them. */
    typeof(char[bump(&n)]) c, *pc = &c, d[2];
    if (calls != 1 || n != 5)
        return 2;
    if (sizeof c != 4 || sizeof *pc != 4 || sizeof d != 8)
        return 3;
    /* Qualified, two-dimensional, constant inner level. */
    volatile typeof(short[n++][2]) e, f;
    if (n != 6 || sizeof e != 5 * 2 * sizeof(short) || sizeof f != sizeof e)
        return 4;
    /* A later change to n does not resize what was already declared. */
    n = 100;
    if (sizeof a != 3 * sizeof(int) || sizeof b != 3 * sizeof(int))
        return 5;
    /* Each object is writable across its whole extent. */
    for (int i = 0; i < 3; i++)
        a[i] = b[i] = i + 1;
    if (a[2] + b[2] != 6)
        return 6;
    (void)e; (void)f;
    return 0;
}

static int for_init(void)
{
    int n = 2, total = 0;
    for (typeof(int[n++]) x, y; total == 0; total++) {
        if (n != 3 || sizeof x != 2 * sizeof(int) || sizeof y != sizeof x)
            return 10;
    }
    return 0;
}

static int in_a_loop(void)
{
    /* Reached three times, evaluated three times -- once each. */
    int n = 1;
    for (int k = 0; k < 3; k++) {
        typeof(long[n++]) p, q;
        if (sizeof p != (unsigned long)(k + 1) * sizeof(long) || sizeof q != sizeof p)
            return 20 + k;
    }
    return n == 4 ? 0 : 23;
}

static int sizeof_once(void)
{
    int n = 7;
    unsigned long s = sizeof(typeof(int[n++]));
    if (s != 7 * sizeof(int) || n != 8)
        return 30;
    return 0;
}

int main(void)
{
    int r;
    if ((r = one_declaration()) || (r = for_init()) || (r = in_a_loop()) || (r = sizeof_once()))
        return r;
    return 0;
}
"#;
    compile_and_run_everywhere("typeof_vla_extent_evaluated_once", src);
}

/// A library call whose lowering builds control flow, standing where control
/// cannot arrive.
///
/// `sqrt` is lowered with an errno check, which is a two-way branch, and the
/// builder took the block to hang it off with `current_bb.unwrap()`. After a
/// `goto`, and before a `switch`'s first `case`, there is no current block --
/// C17 6.8.4.2 gives such a statement no edge -- so the compiler panicked
/// outright on a statement it was about to throw away. Both forms are
/// checked, at every level, because the two-way is only built once the call
/// is lowered rather than left as a call.
#[test]
fn codegen_two_way_lowering_in_unreachable_code_compiles() {
    let code = r#"
#include <math.h>
double d;

static int after_goto(void) {
    goto skip;
    d = sqrt(d);
skip:
    return 0;
}

static int before_first_case(int x) {
    switch (x) {
        d = sqrt(d);
    case 1:
        return 0;
    }
    return 0;
}

int main(void) {
    d = 4.0;
    if (after_goto()) return 1;
    if (before_first_case(1)) return 2;
    /* The dead statements must not have run: `d` is untouched. */
    return d == 4.0 ? 0 : 3;
}
"#;
    for opt in ["-O0", "-O1", "-O2"] {
        assert_eq!(
            compile_and_run("two_way_unreachable", code, &[opt.to_string()]),
            0,
            "{opt}"
        );
    }
}

/// An argument of every class survives being used after a call, and after
/// being handed straight to one.
///
/// What an argument is comes from the parameter list. The back ends inferred
/// a `long double`, `__float128` or `__int128` argument's class from the
/// instructions using it, which held only while every parameter was copied
/// into a typed pseudo at entry. With the copies propagated away, a
/// `__float128` argument passed straight to a call got an 8-byte slot, was
/// stored with `movsd` and reloaded with `movups`.
#[test]
fn codegen_arguments_keep_their_class_without_entry_copies() {
    let src = r#"
__attribute__((noinline)) void clob(void) { volatile double x = 1; volatile long y = 2; (void)x; (void)y; }
__attribute__((noinline)) int pass_q(_Float128 a, _Float128 b) { return a < b; }
__attribute__((noinline)) int fi(int a) { clob(); return a; }
__attribute__((noinline)) double fd(double a) { clob(); return a; }
__attribute__((noinline)) long double fl(long double a) { clob(); return a; }
__attribute__((noinline)) _Float128 fq(_Float128 a, _Float128 b) { if (!pass_q(a, b)) return 0; clob(); return a + b; }
__attribute__((noinline)) __int128 fx(__int128 a) { clob(); return a; }
__attribute__((noinline)) float ff(float a, float b) { clob(); return a + b; }
struct S { long a, b; };
__attribute__((noinline)) long fs(struct S s) { clob(); return s.a + s.b; }
int main(void) {
    if (fi(7) != 7) return 1;
    if (fd(2.5) != 2.5) return 2;
    if (fl(3.5L) != 3.5L) return 3;
    if (fq(4.5F128, 5.0F128) != 9.5F128) return 4;
    if (fx((__int128)5 << 70) != ((__int128)5 << 70)) return 5;
    if (ff(1.5f, 2.0f) != 3.5f) return 6;
    struct S s = { 3, 4 };
    if (fs(s) != 7) return 7;
    return 0;
}
"#;
    if !cfg!(target_os = "linux") {
        return;
    }
    for level in ["-O0", "-O2"] {
        assert_eq!(
            compile_and_run(&format!("arg_classes{level}"), src, &[level.to_string()]),
            0,
            "{level}"
        );
        if let Some(rc) = compile_and_run_aarch64("arg_classes_a64", src, level) {
            assert_eq!(rc, 0, "aarch64 {level}");
        }
    }
}
