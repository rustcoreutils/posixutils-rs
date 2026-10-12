//
// Copyright (c) 2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Flag output operands, `"=@cc<cond>"`: the output is 1 when the condition
// holds on the flags the template leaves, else 0 -- what the Linux kernel's
// `CC_SET`/`CC_OUT` turn into when `__GCC_ASM_FLAG_OUTPUTS__` is defined.
//

use crate::common::compile_and_run_everywhere;

/// Every condition gcc names on each target, after a compare of `a` with
/// `b`, against the same condition computed in C.
#[test]
fn asm_flag_outputs_every_condition() {
    let src = r#"
#include <limits.h>
#ifndef __GCC_ASM_FLAG_OUTPUTS__
#error flag outputs are not advertised
#endif
#if defined(__x86_64__)
#define CMP "cmp %2, %1"
#elif defined(__aarch64__)
#define CMP "cmp %1, %2"
#endif
#define FLAG(cond) \
    __attribute__((noinline)) static int cc_##cond(long a, long b) { \
        int r; \
        __asm__(CMP : "=@cc" #cond(r) : "r"(a), "r"(b)); \
        return r; \
    }
#define CHECK(cond, expect) \
    if (cc_##cond(a, b) != (expect)) return 100 + __LINE__;

#if defined(__x86_64__)
FLAG(a) FLAG(ae) FLAG(b) FLAG(be) FLAG(c) FLAG(e) FLAG(z) FLAG(g) FLAG(ge)
FLAG(l) FLAG(le) FLAG(na) FLAG(nae) FLAG(nb) FLAG(nbe) FLAG(nc) FLAG(ne)
FLAG(ng) FLAG(nge) FLAG(nl) FLAG(nle) FLAG(no) FLAG(np) FLAG(ns) FLAG(nz)
FLAG(o) FLAG(p) FLAG(s)
#elif defined(__aarch64__)
FLAG(eq) FLAG(ne) FLAG(cs) FLAG(hs) FLAG(cc) FLAG(lo) FLAG(mi) FLAG(pl)
FLAG(vs) FLAG(vc) FLAG(hi) FLAG(ls) FLAG(ge) FLAG(lt) FLAG(gt) FLAG(le)
#endif

static int check(long a, long b) {
    unsigned long ua = a, ub = b;
    long diff;
    int ov = __builtin_sub_overflow(a, b, &diff);
    int neg = diff < 0;
#if defined(__x86_64__)
    int even = !__builtin_parity((unsigned char)diff);
    CHECK(a, ua > ub) CHECK(ae, ua >= ub) CHECK(b, ua < ub) CHECK(be, ua <= ub)
    CHECK(c, ua < ub) CHECK(e, a == b) CHECK(z, a == b) CHECK(g, a > b)
    CHECK(ge, a >= b) CHECK(l, a < b) CHECK(le, a <= b) CHECK(na, ua <= ub)
    CHECK(nae, ua < ub) CHECK(nb, ua >= ub) CHECK(nbe, ua > ub) CHECK(nc, ua >= ub)
    CHECK(ne, a != b) CHECK(ng, a <= b) CHECK(nge, a < b) CHECK(nl, a >= b)
    CHECK(nle, a > b) CHECK(no, !ov) CHECK(np, !even) CHECK(ns, !neg)
    CHECK(nz, a != b) CHECK(o, ov) CHECK(p, even)
    CHECK(s, neg)
#elif defined(__aarch64__)
    CHECK(eq, a == b) CHECK(ne, a != b) CHECK(cs, ua >= ub) CHECK(hs, ua >= ub)
    CHECK(cc, ua < ub) CHECK(lo, ua < ub) CHECK(mi, neg) CHECK(pl, !neg)
    CHECK(vs, ov) CHECK(vc, !ov) CHECK(hi, ua > ub) CHECK(ls, ua <= ub)
    CHECK(ge, a >= b) CHECK(lt, a < b) CHECK(gt, a > b) CHECK(le, a <= b)
#endif
    return 0;
}

int main(void) {
    long v[] = { 0, 1, -1, 3, 5, 0x80, 0x7f, LONG_MAX, LONG_MIN, LONG_MIN + 1 };
    int n = sizeof v / sizeof v[0];
    for (int i = 0; i < n; i++)
        for (int j = 0; j < n; j++) {
            int r = check(v[i], v[j]);
            if (r) return r;
        }
    return 0;
}
"#;
    compile_and_run_everywhere("asm_flag_outputs_all", src);
}

/// A flag output of every integer width, a pointer, one that lives in
/// memory -- a global, an element through a pointer -- several in one
/// statement beside a register output, one nothing reads, and the kernel's
/// `CC_SET`/`CC_OUT` on a read-modify-write memory operand.
#[test]
fn asm_flag_outputs_types_and_homes() {
    let src = r#"
#if defined(__x86_64__)
#define CMP "cmp %[b], %[a]"
#define EQ "e"
#define LT "l"
#define ULT "b"
#define BTS(nr, addr) "btsq %[" #nr "], %[" #addr "]"
#define BTS_CC "c"
#elif defined(__aarch64__)
#define CMP "cmp %[a], %[b]"
#define EQ "eq"
#define LT "lt"
#define ULT "lo"
#endif
#define CC_SET(c) "\n\t/* output condition code " #c "*/\n"
#define CC_OUT(c) "=@cc" c

_Bool gb;
long gl;

__attribute__((noinline)) int widths(long a, long b) {
    _Bool z; unsigned char c; short s; int i; long l; unsigned long long u; void *p;
    __asm__(CMP : CC_OUT(EQ)(z), CC_OUT(LT)(c), CC_OUT(LT)(s), CC_OUT(ULT)(i),
                  CC_OUT(EQ)(l), CC_OUT(LT)(u), CC_OUT(EQ)(p)
                : [a] "r"(a), [b] "r"(b));
    return z | c << 1 | s << 2 | i << 3 | (int)l << 4 | (int)u << 5 | (p != 0) << 6;
}

__attribute__((noinline)) void in_memory(long a, long b, int *out) {
    __asm__(CMP : CC_OUT(EQ)(gb), CC_OUT(LT)(gl), CC_OUT(ULT)(out[1])
                : [a] "r"(a), [b] "r"(b));
}

/* A register output computed by the template, and a flag output nothing
   reads, beside the one that is read. */
__attribute__((noinline)) long with_register(long a, long b, int *lt) {
    long d;
    int unused;
#if defined(__x86_64__)
    __asm__("mov %[a], %[d]\n\tsub %[b], %[d]"
            : [d] "=&r"(d), CC_OUT(LT)(*lt), CC_OUT(EQ)(unused) : [a] "r"(a), [b] "r"(b));
#elif defined(__aarch64__)
    __asm__("subs %[d], %[a], %[b]"
            : [d] "=r"(d), CC_OUT(LT)(*lt), CC_OUT(EQ)(unused) : [a] "r"(a), [b] "r"(b));
#endif
    return d;
}

/* More flag outputs live across a call than there are callee-saved
   registers: some are written to the frame. */
__attribute__((noinline)) int touch(int x) { return x + 1; }
__attribute__((noinline)) int across_call(long a, long b) {
    int f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11;
    __asm__(CMP : CC_OUT(EQ)(f0), CC_OUT(LT)(f1), CC_OUT(ULT)(f2), CC_OUT(EQ)(f3),
                  CC_OUT(LT)(f4), CC_OUT(ULT)(f5), CC_OUT(EQ)(f6), CC_OUT(LT)(f7),
                  CC_OUT(ULT)(f8), CC_OUT(EQ)(f9), CC_OUT(LT)(f10), CC_OUT(ULT)(f11)
                : [a] "r"(a), [b] "r"(b));
    int t = touch(0);
    return t + f0 + 2 * f1 + 4 * f2 + 8 * f3 + 16 * f4 + 32 * f5 + 64 * f6 + 128 * f7
           + 256 * f8 + 512 * f9 + 1024 * f10 + 2048 * f11;
}

#if defined(__x86_64__)
/* arch/x86/include/asm/bitops.h: arch_test_and_set_bit. */
__attribute__((noinline)) _Bool test_and_set_bit(long nr, volatile unsigned long *addr) {
    _Bool oldbit;
    __asm__ volatile(BTS(nr, addr) CC_SET(c)
                     : CC_OUT(BTS_CC)(oldbit), [addr] "+m"(*addr) : [nr] "Ir"(nr) : "memory");
    return oldbit;
}
#endif

int main(void) {
    if (widths(3, 3) != (1 | 16 | 64)) return 1;
    if (widths(-1, 2) != (2 | 4 | 32)) return 2;
    if (widths(2, -1) != 8) return 3;
    int out[2] = { 7, 7 };
    in_memory(-5, -5, out);
    if (gb != 1 || gl != 0 || out[1] != 0 || out[0] != 7) return 4;
    in_memory(1, 2, out);
    if (gb != 0 || gl != 1 || out[1] != 1) return 5;
    int lt = 9;
    if (with_register(10, 3, &lt) != 7 || lt != 0) return 6;
    if (with_register(3, 10, &lt) != -7 || lt != 1) return 7;
    /* -1 < 2 signed, not unsigned: the LT bits, 2, 16, 128 and 1024. */
    if (across_call(-1, 2) != 1 + 2 + 16 + 128 + 1024) return 10;
    if (across_call(4, 4) != 1 + 1 + 8 + 64 + 512) return 11;
#if defined(__x86_64__)
    unsigned long word = 0;
    if (test_and_set_bit(5, &word) != 0 || word != 32) return 8;
    if (test_and_set_bit(5, &word) != 1 || word != 32) return 9;
#endif
    return 0;
}
"#;
    compile_and_run_everywhere("asm_flag_outputs_homes", src);
}

/// Flag outputs of an `asm goto`, on the fall-through and on the jump to
/// each label: outputs are valid on every path out (gcc's rule since gcc
/// 11), so the jump reads the flags too. Several outputs, two labels, one
/// output in memory, and a register output beside them. c17 refused flag
/// outputs in an `asm goto`; gcc 13 takes them but reads them on the
/// fall-through only, leaving the jump's value whatever its register held.
#[test]
fn asm_flag_outputs_in_asm_goto() {
    let src = r#"
#if defined(__x86_64__)
#define CMP_JUMP "cmp %[b], %[a]\n\tjl %l[less]\n\tje %l[same]"
#define EQ "e"
#define LT "l"
#define ULT "b"
#elif defined(__aarch64__)
#define CMP_JUMP "cmp %[a], %[b]\n\tb.lt %l[less]\n\tb.eq %l[same]"
#define EQ "eq"
#define LT "lt"
#define ULT "lo"
#endif

int g_ult;

/* 100 * path + 4 * eq + 2 * lt + ult, read on whichever path was taken. */
__attribute__((noinline)) int classify(long a, long b) {
    int eq, lt;
    __asm__ goto(CMP_JUMP : "=@cc" EQ(eq), "=@cc" LT(lt), "=@cc" ULT(g_ult)
                 : [a] "r"(a), [b] "r"(b) : : less, same);
    return 300 + 4 * eq + 2 * lt + g_ult;
less:
    return 100 + 4 * eq + 2 * lt + g_ult;
same:
    return 200 + 4 * eq + 2 * lt + g_ult;
}

/* A register output written by the template beside a flag output. */
__attribute__((noinline)) long with_register(long a, long b, int *eq) {
    long d;
#if defined(__x86_64__)
    __asm__ goto("mov %[a], %[d]\n\tsub %[b], %[d]\n\tje %l[zero]"
                 : [d] "=&r"(d), "=@cc" EQ(*eq) : [a] "r"(a), [b] "r"(b) : : zero);
#elif defined(__aarch64__)
    __asm__ goto("subs %[d], %[a], %[b]\n\tb.eq %l[zero]"
                 : [d] "=r"(d), "=@cc" EQ(*eq) : [a] "r"(a), [b] "r"(b) : : zero);
#endif
    return d;
zero:
    return 1000 + d;
}

int main(void) {
    /* -1 < 2 signed, but not unsigned. */
    if (classify(-1, 2) != 100 + 2) return 1;
    if (classify(1, 2) != 100 + 2 + 1) return 2;
    if (classify(5, 5) != 200 + 4) return 3;
    if (classify(6, 5) != 300) return 4;
    if (classify(2, -1) != 300 + 1) return 5;
    int eq = 9;
    if (with_register(7, 7, &eq) != 1000 || eq != 1) return 6;
    if (with_register(9, 7, &eq) != 2 || eq != 0) return 7;
    return 0;
}
"#;
    compile_and_run_everywhere("asm_flag_outputs_goto", src);
}
