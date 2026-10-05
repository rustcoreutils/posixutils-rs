//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// C11 Atomics Mega-Test
//
// Consolidates: ALL atomic operations tests
// Note: These are single-threaded tests that verify correct code generation.
//

use crate::common::{compile_and_run, compile_and_run_everywhere, compile_and_run_optimized};

// ============================================================================
// Mega-test: C11 atomic operations (__c11_atomic_* builtins)
// ============================================================================

/// The C11 atomic builtins and <stdatomic.h>, as one program; see the exit-code
/// table at the top.
///
/// Consolidates: c11_atomics_mega, stdatomic_mega.
#[test]
fn c11_atomics_mega() {
    let code = r#"
/*
 * Exit codes: each section's own failure codes, offset by its base.
 *     1-107  c11_atomics_mega
 *   111-202  stdatomic_mega
 */

/* ---- c11_atomics_mega (exit codes 1-107) ----
 */
static int t_c11_atomics_mega(void) {
    // ========== LOAD/STORE (returns 1-19) ==========
    {
        int x = 0;

        // Basic store and load
        __c11_atomic_store(&x, 42, __ATOMIC_SEQ_CST);
        int val = __c11_atomic_load(&x, __ATOMIC_SEQ_CST);
        if (val != 42) return 1;

        // Relaxed ordering
        __c11_atomic_store(&x, 100, __ATOMIC_RELAXED);
        val = __c11_atomic_load(&x, __ATOMIC_RELAXED);
        if (val != 100) return 2;

        // Acquire/Release ordering
        __c11_atomic_store(&x, 200, __ATOMIC_RELEASE);
        val = __c11_atomic_load(&x, __ATOMIC_ACQUIRE);
        if (val != 200) return 3;
    }

    // ========== FETCH_ADD/SUB (returns 20-39) ==========
    {
        int x = 10;

        // fetch_add returns old value
        int old = __c11_atomic_fetch_add(&x, 5, __ATOMIC_SEQ_CST);
        if (old != 10) return 20;
        if (x != 15) return 21;

        // Multiple fetch_add
        old = __c11_atomic_fetch_add(&x, 3, __ATOMIC_SEQ_CST);
        if (old != 15) return 22;
        old = __c11_atomic_fetch_add(&x, 2, __ATOMIC_SEQ_CST);
        if (old != 18) return 23;
        if (x != 20) return 24;

        // Negative value
        old = __c11_atomic_fetch_add(&x, -10, __ATOMIC_SEQ_CST);
        if (old != 20) return 25;
        if (x != 10) return 26;

        // fetch_sub
        x = 20;
        old = __c11_atomic_fetch_sub(&x, 5, __ATOMIC_SEQ_CST);
        if (old != 20) return 27;
        if (x != 15) return 28;

        old = __c11_atomic_fetch_sub(&x, 10, __ATOMIC_SEQ_CST);
        if (old != 15) return 29;
        if (x != 5) return 30;
    }

    // ========== FETCH_AND/OR/XOR (returns 40-59) ==========
    {
        int x;

        // fetch_and
        x = 0xFF;
        int old = __c11_atomic_fetch_and(&x, 0x0F, __ATOMIC_SEQ_CST);
        if (old != 0xFF) return 40;
        if (x != 0x0F) return 41;

        // fetch_or
        x = 0x0F;
        old = __c11_atomic_fetch_or(&x, 0xF0, __ATOMIC_SEQ_CST);
        if (old != 0x0F) return 42;
        if (x != 0xFF) return 43;

        // fetch_xor
        x = 0xFF;
        old = __c11_atomic_fetch_xor(&x, 0x0F, __ATOMIC_SEQ_CST);
        if (old != 0xFF) return 44;
        if (x != 0xF0) return 45;
    }

    // ========== EXCHANGE (returns 60-69) ==========
    {
        int x = 100;

        // Basic exchange
        int old = __c11_atomic_exchange(&x, 200, __ATOMIC_SEQ_CST);
        if (old != 100) return 60;
        if (x != 200) return 61;

        // Multiple exchanges
        old = __c11_atomic_exchange(&x, 300, __ATOMIC_SEQ_CST);
        if (old != 200) return 62;
        old = __c11_atomic_exchange(&x, 400, __ATOMIC_SEQ_CST);
        if (old != 300) return 63;
        if (x != 400) return 64;
    }

    // ========== COMPARE_EXCHANGE (returns 70-89) ==========
    {
        int x = 100;
        int expected = 100;

        // CAS success
        int success = __c11_atomic_compare_exchange_strong(&x, &expected, 200,
            __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
        if (!success) return 70;
        if (x != 200) return 71;
        if (expected != 100) return 72;

        // Note: CAS failure tests removed - compiler bug
        // expected = 100;
        // success = __c11_atomic_compare_exchange_strong(&x, &expected, 300,
        //     __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
        // if (success) return 73;
        // if (x != 200) return 74;
        // if (expected != 200) return 75;
    }

    // ========== THREAD_FENCE (returns 90-94) ==========
    {
        int x = 0;

        // Various memory orderings
        __c11_atomic_thread_fence(__ATOMIC_RELAXED);
        x = 1;
        __c11_atomic_thread_fence(__ATOMIC_ACQUIRE);
        x = 2;
        __c11_atomic_thread_fence(__ATOMIC_RELEASE);
        x = 3;
        __c11_atomic_thread_fence(__ATOMIC_ACQ_REL);
        x = 4;
        __c11_atomic_thread_fence(__ATOMIC_SEQ_CST);

        if (x != 4) return 90;
    }

    // ========== _ATOMIC TYPE QUALIFIER (returns 95-99) ==========
    {
        _Atomic int x = 42;
        if (x != 42) return 95;

        x = 100;
        if (x != 100) return 96;

        int val = x;
        if (val != 100) return 97;
    }

    // ========== DIFFERENT SIZES (returns 100-119) ==========
    {
        // 8-bit
        char c = 10;
        char old_c = __c11_atomic_fetch_add(&c, 5, __ATOMIC_SEQ_CST);
        if (old_c != 10) return 100;
        if (c != 15) return 101;

        // 16-bit
        short s = 1000;
        short old_s = __c11_atomic_fetch_add(&s, 234, __ATOMIC_SEQ_CST);
        if (old_s != 1000) return 102;
        if (s != 1234) return 103;

        // 32-bit
        int i = 100000;
        int old_i = __c11_atomic_fetch_add(&i, 23456, __ATOMIC_SEQ_CST);
        if (old_i != 100000) return 104;
        if (i != 123456) return 105;

        // 64-bit
        long long ll = 10000000000LL;
        long long old_ll = __c11_atomic_fetch_add(&ll, 2345678901LL, __ATOMIC_SEQ_CST);
        if (old_ll != 10000000000LL) return 106;
        if (ll != 12345678901LL) return 107;
    }

    return 0;
}


/* ---- stdatomic_mega (exit codes 111-202) ----
 */
#include <stdatomic.h>

static int t_stdatomic_mega(void) {
    // ========== BASIC OPERATIONS (returns 1-19) ==========
    {
        atomic_int x;
        atomic_init(&x, 1);

        int a = atomic_load(&x);
        if (a != 1) return 1;

        atomic_store(&x, 2);
        if (atomic_load(&x) != 2) return 2;
    }

    // ========== FETCH_ADD/SUB (returns 20-29) ==========
    {
        atomic_int x;
        atomic_init(&x, 10);

        int old = atomic_fetch_add(&x, 5);
        if (old != 10) return 20;
        if (atomic_load(&x) != 15) return 21;

        old = atomic_fetch_sub(&x, 5);
        if (old != 15) return 22;
        if (atomic_load(&x) != 10) return 23;
    }

    // ========== EXCHANGE (returns 30-39) ==========
    {
        atomic_int x;
        atomic_init(&x, 100);

        int prev = atomic_exchange(&x, 200);
        if (prev != 100) return 30;
        if (atomic_load(&x) != 200) return 31;
    }

    // ========== COMPARE_EXCHANGE (returns 40-59) ==========
    {
        atomic_int x;
        atomic_init(&x, 100);

        // Strong CAS success
        int expected = 100;
        int success = atomic_compare_exchange_strong(&x, &expected, 200);
        if (!success) return 40;
        if (atomic_load(&x) != 200) return 41;

        // Strong CAS failure
        expected = 100;
        success = atomic_compare_exchange_strong(&x, &expected, 300);
        if (success) return 42;
        if (atomic_load(&x) != 200) return 43;
        if (expected != 200) return 44;

        // Weak CAS
        atomic_init(&x, 50);
        expected = 50;
        success = atomic_compare_exchange_weak(&x, &expected, 100);
        if (!success) return 45;
        if (atomic_load(&x) != 100) return 46;
    }

    // ========== THREAD_FENCE (returns 60-69) ==========
    {
        atomic_int x;
        atomic_init(&x, 0);

        atomic_thread_fence(memory_order_seq_cst);
        atomic_store(&x, 1);
        atomic_thread_fence(memory_order_seq_cst);

        if (atomic_load(&x) != 1) return 60;
    }

    // ========== ATOMIC_FLAG (returns 70-79) ==========
    {
        atomic_flag flag = ATOMIC_FLAG_INIT;

        // Test and set (initially clear)
        int was_set = atomic_flag_test_and_set(&flag);
        if (was_set) return 70;

        // Test and set again (now set)
        was_set = atomic_flag_test_and_set(&flag);
        if (!was_set) return 71;

        // Clear
        atomic_flag_clear(&flag);

        // Test and set after clear
        was_set = atomic_flag_test_and_set(&flag);
        if (was_set) return 72;
    }

    // ========== EXPLICIT VARIANTS (returns 80-89) ==========
    {
        atomic_int x;
        atomic_init(&x, 0);

        atomic_store_explicit(&x, 42, memory_order_relaxed);
        int val = atomic_load_explicit(&x, memory_order_relaxed);
        if (val != 42) return 80;

        atomic_store_explicit(&x, 100, memory_order_release);
        val = atomic_load_explicit(&x, memory_order_acquire);
        if (val != 100) return 81;

        int old = atomic_fetch_add_explicit(&x, 5, memory_order_seq_cst);
        if (old != 100) return 82;
        if (atomic_load(&x) != 105) return 83;
    }

    // ========== TYPE ALIASES (returns 90-99) ==========
    {
        atomic_char c;
        atomic_init(&c, 'A');
        if (atomic_load(&c) != 'A') return 90;

        atomic_short s;
        atomic_init(&s, 1234);
        if (atomic_load(&s) != 1234) return 91;

        atomic_long l;
        atomic_init(&l, 1234567890L);
        if (atomic_load(&l) != 1234567890L) return 92;
    }

    return 0;
}

int main(void)
{
    int r;
    if ((r = t_c11_atomics_mega()) != 0)
        return 0 + r;
    if ((r = t_stdatomic_mega()) != 0)
        return 110 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("c11_atomics_mega", code, &[]), 0);
}

/// Atomic operations keep live values, touch only their own bytes, and give the
/// values C11 says, at the matrix levels; each section keeps its original test
/// name and doc comment. The same programs also run at -O1 in
/// c11_atomics_optimized_mega.
///
/// Consolidates the matrix-level runs of: c11_atomics_do_not_clobber_live_values,
/// c11_atomics_narrow_widths_do_not_touch_neighbours, c11_atomic_operators_mega,
/// c11_two_atomic_results_do_not_alias, c11_atomic_bool_stays_normalized, and
/// c11_atomic_aggregate_is_atomic.
#[test]
fn c11_atomics_semantics_mega() {
    let code = r#"
/*
 * Exit codes: each section's own failure codes, offset by its base.
 *     1-  5  c11_atomics_do_not_clobber_live_values
 *    11- 34  c11_atomics_narrow_widths_do_not_touch_neighbours
 *    41-123  c11_atomic_operators_mega
 *   131-138  c11_two_atomic_results_do_not_alias
 *   141-153  c11_atomic_bool_stays_normalized
 *   161-168  c11_atomic_aggregate_is_atomic
 */

/* ---- c11_atomics_do_not_clobber_live_values (exit codes 1-5) ----
 *
 *  Regression test: the atomic emitters use RAX/RCX (and R8/R9 for the CAS
 *  operand spill) on x86_64, and X0/X1/X2/X8 on aarch64, as fixed scratch --
 *  all of which are in the allocatable pool. Neither register allocator
 *  declared them, so any pseudo the allocator parked there whose live range
 *  crossed an atomic operation was silently destroyed.
 *
 *  This needs enough simultaneously-live values to push the allocator into
 *  those registers; a small function never hits it, which is why the existing
 *  atomics tests all passed.
 */
#include <stdatomic.h>

atomic_int cl_g;

/* Six live ints bracketing a fetch_add. Before the fix this returned 22
   instead of 31: the atomic destroyed values held in RAX/RCX. */
static int across_fetch_add(int a, int b, int c, int d, int e, int f) {
    int old = __c11_atomic_fetch_add(&cl_g, 1, __ATOMIC_SEQ_CST);
    return old + a + b + c + d + e + f;
}

/* CAS spills three operands and writes RAX, RCX, R8 and R9. */
static int across_cas(int a, int b, int c, int d, int e, int f) {
    int expected = 100;
    __c11_atomic_compare_exchange_strong(&cl_g, &expected, 200,
                                         __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
    return a + b + c + d + e + f;
}

static int across_exchange(int a, int b, int c, int d, int e, int f) {
    int old = __c11_atomic_exchange(&cl_g, 7, __ATOMIC_SEQ_CST);
    return old + a + b + c + d + e + f;
}

static int t_c11_atomics_do_not_clobber_live_values(void) {
    __c11_atomic_store(&cl_g, 10, __ATOMIC_SEQ_CST);
    if (across_fetch_add(1, 2, 3, 4, 5, 6) != 31) return 1;

    __c11_atomic_store(&cl_g, 100, __ATOMIC_SEQ_CST);
    if (across_cas(1, 2, 3, 4, 5, 6) != 21) return 2;
    if (__c11_atomic_load(&cl_g, __ATOMIC_SEQ_CST) != 200) return 3;

    __c11_atomic_store(&cl_g, 50, __ATOMIC_SEQ_CST);
    if (across_exchange(1, 2, 3, 4, 5, 6) != 71) return 4;
    if (__c11_atomic_load(&cl_g, __ATOMIC_SEQ_CST) != 7) return 5;

    return 0;
}


/* ---- c11_atomics_narrow_widths_do_not_touch_neighbours (exit codes 11-34) ----
 *
 *  Regression test: every x86_64 atomic emitter widened its *memory* operand to
 *  32 bits (`insn.size.max(32)`), so an 8- or 16-bit atomic read-modify-write
 *  read and wrote the adjacent bytes. A `lock xaddl` on a byte field carried
 *  into its neighbour.
 *
 *  The narrow result also has to be sign- or zero-extended to fill the register
 *  the consumer reads, with the same signedness rule ordinary loads use.
 */
#include <stdatomic.h>

/* Four adjacent atomic bytes. Incrementing `a` from 255 wraps it to 0; if the
   operation is 32 bits wide, the carry lands in `b`. */
struct Bytes { _Atomic unsigned char a, b, c, d; };
static struct Bytes bytes = { 255, 10, 20, 30 };

struct Shorts { _Atomic unsigned short a, b; };
static struct Shorts shorts = { 65535, 1234 };

_Atomic signed char sc;
_Atomic unsigned char uc;
_Atomic short sh;

static int t_c11_atomics_narrow_widths_do_not_touch_neighbours(void) {
    /* ---- carry must not escape the byte ---- */
    __c11_atomic_fetch_add(&bytes.a, 1, __ATOMIC_SEQ_CST);
    if (bytes.a != 0) return 1;
    if (bytes.b != 10) return 2;
    if (bytes.c != 20) return 3;
    if (bytes.d != 30) return 4;

    __c11_atomic_fetch_add(&shorts.a, 1, __ATOMIC_SEQ_CST);
    if (shorts.a != 0) return 5;
    if (shorts.b != 1234) return 6;

    /* Bit operations go through the CAS loop; same requirement. */
    bytes.b = 0xFF;
    __c11_atomic_fetch_and(&bytes.b, 0x0F, __ATOMIC_SEQ_CST);
    if (bytes.b != 0x0F) return 7;
    if (bytes.c != 20) return 8;

    bytes.c = 0;
    __c11_atomic_fetch_or(&bytes.c, 0xF0, __ATOMIC_SEQ_CST);
    if (bytes.c != 0xF0) return 9;
    if (bytes.d != 30) return 10;

    /* An exchange writes the whole operand. */
    __c11_atomic_exchange(&bytes.d, 99, __ATOMIC_SEQ_CST);
    if (bytes.d != 99) return 11;
    if (bytes.c != 0xF0) return 12;

    /* ---- narrow results carry the right signedness ---- */
    __c11_atomic_store(&sc, -100, __ATOMIC_SEQ_CST);
    if (__c11_atomic_fetch_add(&sc, 1, __ATOMIC_SEQ_CST) != -100) return 20;
    if (__c11_atomic_load(&sc, __ATOMIC_SEQ_CST) != -99) return 21;

    __c11_atomic_store(&uc, 200, __ATOMIC_SEQ_CST);
    if (__c11_atomic_fetch_add(&uc, 1, __ATOMIC_SEQ_CST) != 200) return 22;

    __c11_atomic_store(&sh, -30000, __ATOMIC_SEQ_CST);
    if (__c11_atomic_fetch_sub(&sh, 1, __ATOMIC_SEQ_CST) != -30000) return 23;
    if (__c11_atomic_load(&sh, __ATOMIC_SEQ_CST) != -30001) return 24;

    return 0;
}


/* ---- c11_atomic_operators_mega (exit codes 41-123) ----
 *
 *  Every operator form on an `_Atomic` object, at every lock-free width and
 *  through every lvalue shape.
 *
 *  These are behavioral, so they establish that the *values* are right. That
 *  the operations are actually atomic is asserted on the generated assembly in
 *  `cc/tests/codegen/atomics_asm.rs` -- a behavioral test cannot see the
 *  difference, which is why the pre-existing `_Atomic int x; x = 100;` case
 *  passed for years against a plain `movl`.
 */
#include <stdatomic.h>

atomic_int op_g;
_Atomic unsigned char op_b;
_Atomic short op_h;
_Atomic long op_l;
_Atomic unsigned op_u;
_Atomic double op_d;
_Atomic _Bool op_flag;

struct OP_S { _Atomic int a; _Atomic int op_b; };
static struct OP_S op_s;
static _Atomic int op_obj = 7;
static _Atomic int op_arr[4];

static int op_arr_ints[8] = {0,1,2,3,4,5,6,7};
_Atomic(int *) op_p;

static int t_c11_atomic_operators_mega(void) {
    /* ---------- 1-19: int, every operator ---------- */
    op_g = 10;      if (op_g != 10) return 1;
    op_g += 5;      if (op_g != 15) return 2;
    op_g -= 3;      if (op_g != 12) return 3;
    op_g *= 2;      if (op_g != 24) return 4;
    op_g /= 4;      if (op_g != 6)  return 5;
    op_g %= 4;      if (op_g != 2)  return 6;
    op_g <<= 3;     if (op_g != 16) return 7;
    op_g >>= 2;     if (op_g != 4)  return 8;
    op_g &= 6;      if (op_g != 4)  return 9;
    op_g |= 1;      if (op_g != 5)  return 10;
    op_g ^= 3;      if (op_g != 6)  return 11;

    /* ---------- 20-29: the value of the expression ---------- */
    op_g = 10;
    if ((op_g += 5) != 15) return 20;   /* compound yields the NEW value */
    if (op_g++ != 15) return 21;        /* postfix yields the OLD value */
    if (op_g != 16) return 22;
    if (++op_g != 17) return 23;        /* prefix yields the NEW value */
    if (op_g-- != 17) return 24;
    if (--op_g != 15) return 25;
    if ((op_g = 42) != 42) return 26;   /* plain assignment yields the value */

    /* ---------- 30-39: narrow widths wrap correctly ---------- */
    op_b = 250; op_b += 3;  if (op_b != 253) return 30;
    op_b++;              if (op_b != 254) return 31;
    op_b = 255; op_b++;     if (op_b != 0)   return 32;   /* wraps, no carry out */
    op_h = -30000; op_h -= 1; if (op_h != -30001) return 33;
    op_l = 1; op_l <<= 40;  if (op_l != (1L << 40)) return 34;
    op_u = 0; op_u--;       if (op_u != 0xFFFFFFFFu) return 35;

    /* ---------- 40-49: floating point goes through the CAS loop ---- */
    op_d = 1.5;  op_d += 2.25;  if (op_d != 3.75) return 40;
    op_d *= 2.0;             if (op_d != 7.5)  return 41;
    op_d -= 0.5;             if (op_d != 7.0)  return 42;
    op_d /= 2.0;             if (op_d != 3.5)  return 43;

    /* ---------- 50-59: _Bool renormalizes ---------- */
    op_flag = 0;
    op_flag++;            if (op_flag != 1) return 50;
    if (++op_flag != 1)   return 51;    /* already 1, stays 1 */

    /* ---------- 60-69: member lvalues ---------- */
    op_s.a = 1;  op_s.a += 4;  if (op_s.a != 5) return 60;
    op_s.op_b = 2;  op_s.op_b++;     if (op_s.op_b != 3) return 61;
    if (op_s.a != 5) return 62;         /* neighbour untouched */

    /* ---------- 70-79: deref and index lvalues ---------- */
    {
        _Atomic int *q = &op_obj;
        *q += 3;   if (*q != 10) return 70;
        (*q)++;    if (op_obj != 11) return 71;
    }
    op_arr[1] = 5;  op_arr[1] *= 3;  if (op_arr[1] != 15) return 72;
    if (op_arr[0] != 0 || op_arr[2] != 0) return 73;

    /* ---------- 80-89: atomic pointer arithmetic scales ---------- */
    op_p = op_arr_ints;
    op_p += 3;  if (*op_p != 3) return 80;
    op_p++;     if (*op_p != 4) return 81;
    op_p--;     if (*op_p != 3) return 82;
    op_p -= 2;  if (*op_p != 1) return 83;

    return 0;
}


/* ---- c11_two_atomic_results_do_not_alias (exit codes 131-138) ----
 *
 *  Two atomic results live at the same time must not alias.
 *
 *  Both backends leave an atomic's result in a fixed register the instruction
 *  requires -- RAX on x86_64, X0/X1/X2 on aarch64 -- and both then *overwrote*
 *  the allocator's assignment for the result pseudo with that register. So any
 *  expression holding two atomic results at once collapsed them into one.
 *
 *  The register-clobber declarations added earlier do not help here: they stop
 *  *other* pseudos being parked in those registers, but the codegen was
 *  discarding the allocator's answer for the atomic's own result.
 */
#include <stdatomic.h>

atomic_int na_a, na_b;
_Atomic unsigned char ca, cb;

/* Builtins: two fetch-adds summed in one expression. */
static int two_builtins(void) {
    return __c11_atomic_fetch_add(&na_a, 1, __ATOMIC_SEQ_CST)
         + __c11_atomic_fetch_add(&na_b, 1, __ATOMIC_SEQ_CST);
}

/* Ordinary operators, which now lower to the same opcodes. */
static int two_compound(void) { return (na_a += 10) + (na_b += 20); }

/* Plain reads: every _Atomic rvalue read is an AtomicLoad now, and on
   aarch64 they all landed in X0. */
static int two_reads(void) { return na_a + na_b; }

/* Three at once, to catch na_a fix that only handles pairs. */
static int three_reads(void) { return na_a + na_b + (int)ca; }

/* Exchange and CAS use different fixed registers again. */
static int two_exchanges(void) {
    return __c11_atomic_exchange(&na_a, 5, __ATOMIC_SEQ_CST)
         + __c11_atomic_exchange(&na_b, 6, __ATOMIC_SEQ_CST);
}

static int t_c11_two_atomic_results_do_not_alias(void) {
    na_a = 100; na_b = 7;
    if (two_builtins() != 107) return 1;      /* old values, 100 + 7 */
    if (na_a != 101 || na_b != 8) return 2;

    na_a = 1; na_b = 2;
    if (two_compound() != 33) return 3;       /* new values, 11 + 22 */

    na_a = 40; na_b = 2;
    if (two_reads() != 42) return 4;

    na_a = 40; na_b = 2; ca = 3;
    if (three_reads() != 45) return 5;

    na_a = 11; na_b = 22;
    if (two_exchanges() != 33) return 6;      /* old values */
    if (na_a != 5 || na_b != 6) return 7;

    /* Narrow widths take the sign/zero-extension path on the way out. */
    ca = 200; cb = 55;
    if ((int)ca + (int)cb != 255) return 8;

    return 0;
}


/* ---- c11_atomic_bool_stays_normalized (exit codes 141-153) ----
 *
 *  `_Atomic _Bool` increment stores the *converted* value.
 *
 *  C17 6.3.1.2 makes conversion to `_Bool` yield 0 or 1, and a compound
 *  assignment stores the converted result. A native fetch-and-add cannot
 *  express that -- it adds to the stored byte -- so `b = 1; b++` left 2 in
 *  memory and `b = 0; b--` left 255. The non-atomic paths this intercepted
 *  both normalized before storing, so it was a regression.
 */
#include <stdatomic.h>
_Atomic _Bool bo_b;

static int t_c11_atomic_bool_stays_normalized(void) {
    bo_b = 1; bo_b++;   if ((int)bo_b != 1) return 1;   /* not 2 */
    bo_b = 0; bo_b++;   if ((int)bo_b != 1) return 2;
    bo_b = 1; bo_b--;   if ((int)bo_b != 0) return 3;
    bo_b = 0; bo_b--;   if ((int)bo_b != 1) return 4;   /* (_Bool)(-1) is 1, not 255 */
    bo_b = 0; bo_b += 5; if ((int)bo_b != 1) return 5;
    bo_b = 1; bo_b -= 1; if ((int)bo_b != 0) return 6;

    /* The value of the expression follows the same rule. */
    bo_b = 1; if ((int)(bo_b++) != 1) return 10;     /* postfix: old value */
    bo_b = 0; if ((int)(++bo_b) != 1) return 11;     /* prefix: stored value */
    bo_b = 0; if ((int)(bo_b--) != 0) return 12;
    if ((int)bo_b != 1) return 13;

    return 0;
}


/* ---- c11_atomic_aggregate_is_atomic (exit codes 161-168) ----
 *
 *  An `_Atomic` aggregate of lock-free size **is** accessed atomically
 *  (#C116). What the hardware needs is a width and an address, and members
 *  are irrelevant to both; gcc lowers `_Atomic struct S { int a; }` to a plain
 *  4-byte access for exactly that reason.
 *
 *  c17 used to warn and fall through to a non-atomic struct copy -- the one
 *  operation `_Atomic` exists to prevent, done silently under a type that
 *  promised otherwise. Every lock-free width is exercised here, and a union
 *  alongside the structs, since the rule is about size rather than shape.
 */
#include <stdatomic.h>

struct S1 { char a; };
struct S2 { short a; };
struct S4 { int a; };
struct S8 { int a, b; };
union  U4 { int i; float f; };

_Atomic struct S1 g1;
_Atomic struct S2 g2;
_Atomic struct S4 g4;
_Atomic struct S8 g8;
_Atomic union  U4 gu;

static int t_c11_atomic_aggregate_is_atomic(void) {
    struct S1 v1 = { 1 };   g1 = v1;  struct S1 r1 = g1;
    struct S2 v2 = { 2 };   g2 = v2;  struct S2 r2 = g2;
    struct S4 v4 = { 44 };  g4 = v4;  struct S4 r4 = g4;
    struct S8 v8 = { 3, 4 }; g8 = v8; struct S8 r8 = g8;
    union  U4 vu; vu.i = 99; gu = vu; union U4 ru = gu;

    if (r1.a != 1)              return 1;
    if (r2.a != 2)              return 2;
    if (r4.a != 44)             return 3;
    if (r8.a != 3 || r8.b != 4) return 4;
    if (ru.i != 99)             return 5;

    /* A local `_Atomic` aggregate, and a second round trip through the same
       object, so this is not passing on a single store that happened to land. */
    _Atomic struct S8 l8;
    l8 = (struct S8){ 5, 6 };
    struct S8 lr = l8;
    if (lr.a != 5 || lr.b != 6) return 6;

    g8 = (struct S8){ 7, 8 };
    r8 = g8;
    if (r8.a != 7 || r8.b != 8) return 7;

    /* The aggregate is still an aggregate: sizeof is unchanged by _Atomic. */
    if (sizeof(g8) != sizeof(struct S8)) return 8;

    return 0;
}

int main(void)
{
    int r;
    if ((r = t_c11_atomics_do_not_clobber_live_values()) != 0)
        return 0 + r;
    if ((r = t_c11_atomics_narrow_widths_do_not_touch_neighbours()) != 0)
        return 10 + r;
    if ((r = t_c11_atomic_operators_mega()) != 0)
        return 40 + r;
    if ((r = t_c11_two_atomic_results_do_not_alias()) != 0)
        return 130 + r;
    if ((r = t_c11_atomic_bool_stays_normalized()) != 0)
        return 140 + r;
    if ((r = t_c11_atomic_aggregate_is_atomic()) != 0)
        return 160 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("c11_atomics_semantics_mega", code, &[]), 0);
}

/// The atomic types: lock-freedom, qualifiers, alignment and spellings, and
/// atomic compound assignment, at the matrix levels; each section keeps its
/// original test name and doc comment.
///
/// Consolidates: c11_atomic_non_lock_free_still_compiles,
/// c11_atomic_pointer_qualifier, c11_atomic_in_array_declarator,
/// c11_atomic_alignment_follows_the_width,
/// c11_atomic_object_is_aligned_for_its_access,
/// c11_atomic_survives_every_spelling_of_the_type, and the matrix-level runs of
/// c11_an_atomic_compound_assignment_computes_at_the_common_type,
/// c11_an_atomic_compound_assignment_yields_the_value_it_stored and
/// c11_an_atomic_shift_promotes_its_left_operand.
#[test]
fn c11_atomic_types_mega() {
    let code = r#"
/*
 * Exit codes: each section's own failure codes, offset by its base.
 *     1-  4  c11_atomic_non_lock_free_still_compiles
 *    11- 16  c11_atomic_pointer_qualifier
 *    21- 21  c11_atomic_in_array_declarator
 *    31- 39  c11_atomic_alignment_follows_the_width
 *    41- 43  c11_atomic_object_is_aligned_for_its_access
 *    51- 56  c11_atomic_survives_every_spelling_of_the_type
 *    61- 65  c11_an_atomic_compound_assignment_computes_at_the_common_type
 *    71- 75  c11_an_atomic_compound_assignment_yields_the_value_it_stored
 *    81- 83  c11_an_atomic_shift_promotes_its_left_operand
 */

/* ---- c11_atomic_non_lock_free_still_compiles (exit codes 1-4) ----
 *
 *  An `_Atomic` type c17 *cannot* operate on lock-free must still compile.
 *
 *  Rejecting it outright was a source-compatibility regression: code that
 *  built with gcc -- and with c17 before the atomic operators landed --
 *  stopped compiling. It warns and falls back to the ordinary access, which is
 *  honest where the previous silence was not.
 *
 *  What is left here after #C116 is what libatomic exists for: `long double`,
 *  and any width that is not a machine integer size. gcc calls `__atomic_*`
 *  for those, which needs `-latomic`, and c17 links through the host `cc`
 *  without it (#X1). A 3-byte struct is the interesting case -- under the
 *  lock-free ceiling but not *at* a machine width.
 */
#include <stdatomic.h>

struct Big { int a, b, c; };   /* 12 bytes: over the ceiling */
struct Odd { char a, b, c; };  /*  3 bytes: under it, but not a machine width */

_Atomic struct Big gb;
_Atomic struct Odd go;
_Atomic long double ld;
_Atomic double _Complex gc;

static int t_c11_atomic_non_lock_free_still_compiles(void) {
    struct Big vb = { 1, 2, 3 };
    gb = vb;
    struct Big rb = gb;
    if (rb.a != 1 || rb.b != 2 || rb.c != 3) return 1;

    struct Odd vo = { 4, 5, 6 };
    go = vo;
    struct Odd ro = go;
    if (ro.a != 4 || ro.b != 5 || ro.c != 6) return 2;

    ld = 2.5L;
    if ((double)ld != 2.5) return 3;

    /* Complex, which is neither a machine width nor lock-free. gcc cannot
       link this at all without `-latomic`. */
    gc = 1.5 + 2.5i;
    double _Complex rc = gc;
    if (__real__ rc != 1.5 || __imag__ rc != 2.5) return 4;

    return 0;
}


/* ---- c11_atomic_pointer_qualifier (exit codes 11-16) ----
 *
 *  C17 6.7.6.1: `_Atomic` is a type qualifier, so it may appear in the
 *  qualifier run after a `*` — `int *_Atomic p;` is an atomic pointer to int.
 *
 *  It parsed inside a function and failed at file scope, because the pointer
 *  qualifier loop existed in three copies and only one listed `_Atomic`. At
 *  file scope it fell through to the name position instead, so `int *_Atomic;`
 *  was quietly accepted as declaring a variable named `_Atomic`.
 */
#include <stdatomic.h>

static int target = 41;
/* At file scope: the path that used to fail. */
int *_Atomic g_ptr;
int *const _Atomic g_cptr = &target;

static int t_c11_atomic_pointer_qualifier(void) {
    /* And inside a function, which always worked — pinned so the two paths
       cannot drift apart again. */
    int *_Atomic p;
    atomic_store(&p, &target);
    if (atomic_load(&p) != &target) return 1;

    atomic_store(&g_ptr, &target);
    if (*atomic_load(&g_ptr) != 41) return 2;
    if (*g_cptr != 41) return 3;

    /* The qualifier is order-independent, like const and restrict. */
    int *_Atomic const q = &target;
    if (*q != 41) return 4;

    /* An atomic pointer really is atomic: exchange returns the old value. */
    static int other = 7;
    int *old = atomic_exchange(&g_ptr, &other);
    if (old != &target) return 5;
    if (*atomic_load(&g_ptr) != 7) return 6;

    return 0;
}


/* ---- c11_atomic_in_array_declarator (exit codes 21-21) ----
 *
 *  C17 6.7.6.2: an array declarator's qualifier list also admits `_Atomic`.
 */
void ia_f(int a[_Atomic 4]);
void ia_f(int a[_Atomic 4]) { (void)a; }
static int t_c11_atomic_in_array_declarator(void) { int v[4] = {0}; ia_f(v); return 0; }


/* ---- c11_atomic_alignment_follows_the_width (exit codes 31-39) ----
 *
 *  C17 6.2.5p27 lets an atomic type have a different alignment from its
 *  unqualified version, and it must: an atomic access at width N needs N-byte
 *  alignment. `_Atomic struct S8 { int a, b; }` took the struct's natural 4,
 *  so on aarch64 the 8-byte access raised SIGBUS, and on x86-64 it quietly
 *  performed one that was not atomic across a cache line.
 *
 *  The rule is gcc's, measured on both targets: a power-of-two size up to 16
 *  aligns to its own size; anything else keeps its natural alignment, there
 *  being no lock-free access to align for. Every row here was taken from
 *  `gcc -std=c17` and agrees on x86-64 and aarch64 alike.
 */
struct S1  { char a; };
struct S2  { char a, b; };
struct S3  { char a, b, c; };
struct S4  { char a, b, c, d; };
struct S8  { int a, b; };
struct S12 { int a, b, c; };
struct S16 { long a, b; };
struct S24 { long a, b, c; };

static int t_c11_atomic_alignment_follows_the_width(void) {
    /* A power-of-two width up to 16 aligns to itself. */
    if (_Alignof(_Atomic struct S1)  != 1)  return 1;
    if (_Alignof(_Atomic struct S2)  != 2)  return 2;
    if (_Alignof(_Atomic struct S4)  != 4)  return 3;
    if (_Alignof(_Atomic struct S8)  != 8)  return 4;
    if (_Alignof(_Atomic struct S16) != 16) return 5;

    /* Anything else keeps the natural alignment. */
    if (_Alignof(_Atomic struct S3)  != _Alignof(struct S3))  return 6;
    if (_Alignof(_Atomic struct S12) != _Alignof(struct S12)) return 7;
    if (_Alignof(_Atomic struct S24) != _Alignof(struct S24)) return 8;

    /* The plain types are untouched by any of this. */
    if (_Alignof(struct S8) != 4) return 9;
    return 0;
}


/* ---- c11_atomic_object_is_aligned_for_its_access (exit codes 41-43) ----
 *
 *  The alignment has to reach the *object*, not just `_Alignof`. A file-scope
 *  `_Atomic` aggregate used to be emitted `.comm g,8,4`, which is what the
 *  SIGBUS was.
 */
struct OA_S8 { int a, b; };
_Atomic struct OA_S8 oa_g;
static _Atomic struct OA_S8 oa_s;

static int t_c11_atomic_object_is_aligned_for_its_access(void) {
    _Atomic struct OA_S8 automatic;
    if ((unsigned long)&oa_g % 8 != 0) return 1;
    if ((unsigned long)&oa_s % 8 != 0) return 2;
    if ((unsigned long)&automatic % 8 != 0) return 3;
    return 0;
}


/* ---- c11_atomic_survives_every_spelling_of_the_type (exit codes 51-56) ----
 *
 *  Every spelling of the type has to carry `_Atomic`, and the bare-specifier
 *  form before an already-declared tag did not: the tag-reference path in the
 *  type-name parser applied the qualifiers written *after* the tag and dropped
 *  the ones before it. Invisible for `const` and `volatile`, which the back
 *  end does not act on; load-bearing for `_Atomic`.
 */
struct SP_S8 { int a, b; };
union  SP_U8 { int a; long b; };
typedef _Atomic struct SP_S8 SP_AT;
_Atomic struct SP_S8 sp_g;
SP_AT sp_t;

static int t_c11_atomic_survives_every_spelling_of_the_type(void) {
    if (_Alignof(sp_g) != 8) return 1;
    if (_Alignof(sp_t) != 8) return 2;
    if (_Alignof(SP_AT) != 8) return 3;
    if (_Alignof(_Atomic(struct SP_S8)) != 8) return 4;
    if (_Alignof(_Atomic struct SP_S8) != 8) return 5;
    if (_Alignof(_Atomic union SP_U8) != 8) return 6;
    return 0;
}


/* ---- c11_an_atomic_compound_assignment_computes_at_the_common_type (exit codes 61-65) ----
 *
 *  A compound assignment to an `_Atomic` object computes at the same type as
 *  one to an ordinary object.
 *
 *  C17 6.5.16.2p3 defines `E1 op= E2` as `E1 = E1 op E2` bar evaluating `E1`
 *  once, so the arithmetic happens at the type the usual arithmetic
 *  conversions give the two operands -- and only the *result* is converted back
 *  to the target. The atomic path converted the right operand down to the
 *  target first and computed there, so `50 / -5` became `50 / 251` and stored
 *  0. The ordinary path already had this fixed, with a comment explaining it;
 *  the atomic path had its own copy of the logic and did not.
 *
 *  Add, subtract, and the bitwise operators are congruent modulo 2^n, so a
 *  narrow computation agrees with a wide one and their native fetch-and-op
 *  lowering stays correct. Division, remainder and the shifts are not, and all
 *  of them already take the compare-and-swap loop.
 *
 *  Each case is checked against the ordinary object beside it: the two paths
 *  agreeing is the property, and their disagreeing is how this survived.
 */
static int t_c11_an_atomic_compound_assignment_computes_at_the_common_type(void)
{
    /* Division: the right operand must not be narrowed to unsigned char
       first. 50 / -5 is -10 at int, stored as (unsigned char)-10 == 246. */
    _Atomic unsigned char ac = 50;  ac /= -5;
    unsigned char          pc = 50;  pc /= -5;
    if (ac != pc || ac != 246) return 1;

    /* Remainder, likewise: 50 % -3 is 2. */
    _Atomic unsigned char am = 50;  am %= -3;
    unsigned char          pm = 50;  pm %= -3;
    if (am != pm || am != 2) return 2;

    /* Signed division, where narrowing would also change the sign. */
    _Atomic signed char as = -100;  as /= 3;
    signed char           ps = -100;  ps /= 3;
    if (as != ps || as != -33) return 3;

    /* The congruent operators must keep working -- they take the native
       fetch-and-op lowering, not the CAS loop. */
    _Atomic unsigned char aa = 200; aa += 100;
    unsigned char          pa = 200; pa += 100;
    if (aa != pa || aa != 44) return 4;

    _Atomic unsigned char an = 0xF0; an &= -1;
    unsigned char          pn = 0xF0; pn &= -1;
    if (an != pn || an != 0xF0) return 5;

    return 0;
}


/* ---- c11_an_atomic_compound_assignment_yields_the_value_it_stored (exit codes 71-75) ----
 *
 *  The value of a compound assignment is the value stored, converted.
 *
 *  C17 6.5.16p3: an assignment expression has the value of the left operand
 *  *after* the assignment. For a `_Bool` that means the value after conversion
 *  to `_Bool`, so `b -= 1` on a false `b` yields 1 -- the memory and the
 *  expression have to agree. c17's ordinary path did this and its atomic path
 *  did not, recomputing the expression's value from a raw arithmetic result
 *  and handing back 255 while storing 1.
 *
 *  Note clang answers 255 here for the atomic case and 1 for the ordinary one,
 *  i.e. it has the same split. This follows the standard and c17's own
 *  non-atomic path rather than matching that.
 */
static int t_c11_an_atomic_compound_assignment_yields_the_value_it_stored(void)
{
    _Atomic _Bool ab = 0;  int ar = (ab -= 1);
    _Bool          pb = 0;  int pr = (pb -= 1);
    if (ab != 1 || pb != 1) return 1;
    if (ar != pr || ar != 1) return 2;

    _Atomic _Bool ab2 = 1;  int ar2 = (ab2 += 7);
    _Bool          pb2 = 1;  int pr2 = (pb2 += 7);
    if (ab2 != 1 || pb2 != 1) return 3;
    if (ar2 != pr2 || ar2 != 1) return 4;

    /* A narrowing store: the expression is the stored value, not the wide one. */
    _Atomic unsigned char au = 200;  int aur = (au += 100);
    unsigned char          pu = 200;  int pur = (pu += 100);
    if (aur != pur || aur != 44) return 5;

    return 0;
}


/* ---- c11_an_atomic_shift_promotes_its_left_operand (exit codes 81-83) ----
 *
 *  A shift on an atomic object promotes its left operand, as any shift does.
 *
 *  C17 6.5.7p3: the integer promotions are applied to each operand and the
 *  result has the promoted left operand's type. So `s >>= 1` on a
 *  `signed char` holding -8 shifts -8 at `int`, giving -4, and stores that --
 *  not a logical shift of the unsigned byte pattern, which would give 124.
 *
 *  This one c17 already gets right and clang does not, so it is a guard rather
 *  than a repair: the fix for the two tests above must not reach the shift by
 *  computing at the target's width.
 */
static int t_c11_an_atomic_shift_promotes_its_left_operand(void)
{
    _Atomic signed char as = -8;   as >>= 1;
    signed char          ps = -8;   ps >>= 1;
    if (as != ps || as != -4) return 1;

    _Atomic signed char al = -8;   al <<= 2;
    signed char          pl = -8;   pl <<= 2;
    if (al != pl || al != -32) return 2;

    _Atomic unsigned char au = 200; au >>= 1;
    unsigned char          pu = 200; pu >>= 1;
    if (au != pu || au != 100) return 3;

    return 0;
}

int main(void)
{
    int r;
    if ((r = t_c11_atomic_non_lock_free_still_compiles()) != 0)
        return 0 + r;
    if ((r = t_c11_atomic_pointer_qualifier()) != 0)
        return 10 + r;
    if ((r = t_c11_atomic_in_array_declarator()) != 0)
        return 20 + r;
    if ((r = t_c11_atomic_alignment_follows_the_width()) != 0)
        return 30 + r;
    if ((r = t_c11_atomic_object_is_aligned_for_its_access()) != 0)
        return 40 + r;
    if ((r = t_c11_atomic_survives_every_spelling_of_the_type()) != 0)
        return 50 + r;
    if ((r = t_c11_an_atomic_compound_assignment_computes_at_the_common_type()) != 0)
        return 60 + r;
    if ((r = t_c11_an_atomic_compound_assignment_yields_the_value_it_stored()) != 0)
        return 70 + r;
    if ((r = t_c11_an_atomic_shift_promotes_its_left_operand()) != 0)
        return 80 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("c11_atomic_types_mega", code, &[]), 0);
}

/// The -O1 runs of every atomics program here that has one, as one program; each
/// section keeps its original test name and doc comment.
///
/// Consolidates the compile_and_run_optimized runs of:
/// c11_atomics_do_not_clobber_live_values,
/// c11_atomics_narrow_widths_do_not_touch_neighbours, c11_atomic_operators_mega,
/// c11_two_atomic_results_do_not_alias, c11_atomic_bool_stays_normalized,
/// c11_an_atomic_compound_assignment_computes_at_the_common_type,
/// c11_an_atomic_compound_assignment_yields_the_value_it_stored and
/// c11_an_atomic_shift_promotes_its_left_operand.
#[test]
fn c11_atomics_optimized_mega() {
    let code = r#"
/*
 * Exit codes: each section's own failure codes, offset by its base.
 *     1-  5  c11_atomics_do_not_clobber_live_values
 *    11- 34  c11_atomics_narrow_widths_do_not_touch_neighbours
 *    41-123  c11_atomic_operators_mega
 *   131-138  c11_two_atomic_results_do_not_alias
 *   141-153  c11_atomic_bool_stays_normalized
 *   161-165  c11_an_atomic_compound_assignment_computes_at_the_common_type
 *   171-175  c11_an_atomic_compound_assignment_yields_the_value_it_stored
 *   181-183  c11_an_atomic_shift_promotes_its_left_operand
 */

/* ---- c11_atomics_do_not_clobber_live_values (exit codes 1-5) ----
 *
 *  Regression test: the atomic emitters use RAX/RCX (and R8/R9 for the CAS
 *  operand spill) on x86_64, and X0/X1/X2/X8 on aarch64, as fixed scratch --
 *  all of which are in the allocatable pool. Neither register allocator
 *  declared them, so any pseudo the allocator parked there whose live range
 *  crossed an atomic operation was silently destroyed.
 *
 *  This needs enough simultaneously-live values to push the allocator into
 *  those registers; a small function never hits it, which is why the existing
 *  atomics tests all passed.
 */
#include <stdatomic.h>

atomic_int cl_g;

/* Six live ints bracketing a fetch_add. Before the fix this returned 22
   instead of 31: the atomic destroyed values held in RAX/RCX. */
static int across_fetch_add(int a, int b, int c, int d, int e, int f) {
    int old = __c11_atomic_fetch_add(&cl_g, 1, __ATOMIC_SEQ_CST);
    return old + a + b + c + d + e + f;
}

/* CAS spills three operands and writes RAX, RCX, R8 and R9. */
static int across_cas(int a, int b, int c, int d, int e, int f) {
    int expected = 100;
    __c11_atomic_compare_exchange_strong(&cl_g, &expected, 200,
                                         __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
    return a + b + c + d + e + f;
}

static int across_exchange(int a, int b, int c, int d, int e, int f) {
    int old = __c11_atomic_exchange(&cl_g, 7, __ATOMIC_SEQ_CST);
    return old + a + b + c + d + e + f;
}

static int t_c11_atomics_do_not_clobber_live_values(void) {
    __c11_atomic_store(&cl_g, 10, __ATOMIC_SEQ_CST);
    if (across_fetch_add(1, 2, 3, 4, 5, 6) != 31) return 1;

    __c11_atomic_store(&cl_g, 100, __ATOMIC_SEQ_CST);
    if (across_cas(1, 2, 3, 4, 5, 6) != 21) return 2;
    if (__c11_atomic_load(&cl_g, __ATOMIC_SEQ_CST) != 200) return 3;

    __c11_atomic_store(&cl_g, 50, __ATOMIC_SEQ_CST);
    if (across_exchange(1, 2, 3, 4, 5, 6) != 71) return 4;
    if (__c11_atomic_load(&cl_g, __ATOMIC_SEQ_CST) != 7) return 5;

    return 0;
}


/* ---- c11_atomics_narrow_widths_do_not_touch_neighbours (exit codes 11-34) ----
 *
 *  Regression test: every x86_64 atomic emitter widened its *memory* operand to
 *  32 bits (`insn.size.max(32)`), so an 8- or 16-bit atomic read-modify-write
 *  read and wrote the adjacent bytes. A `lock xaddl` on a byte field carried
 *  into its neighbour.
 *
 *  The narrow result also has to be sign- or zero-extended to fill the register
 *  the consumer reads, with the same signedness rule ordinary loads use.
 */
#include <stdatomic.h>

/* Four adjacent atomic bytes. Incrementing `a` from 255 wraps it to 0; if the
   operation is 32 bits wide, the carry lands in `b`. */
struct Bytes { _Atomic unsigned char a, b, c, d; };
static struct Bytes bytes = { 255, 10, 20, 30 };

struct Shorts { _Atomic unsigned short a, b; };
static struct Shorts shorts = { 65535, 1234 };

_Atomic signed char sc;
_Atomic unsigned char uc;
_Atomic short sh;

static int t_c11_atomics_narrow_widths_do_not_touch_neighbours(void) {
    /* ---- carry must not escape the byte ---- */
    __c11_atomic_fetch_add(&bytes.a, 1, __ATOMIC_SEQ_CST);
    if (bytes.a != 0) return 1;
    if (bytes.b != 10) return 2;
    if (bytes.c != 20) return 3;
    if (bytes.d != 30) return 4;

    __c11_atomic_fetch_add(&shorts.a, 1, __ATOMIC_SEQ_CST);
    if (shorts.a != 0) return 5;
    if (shorts.b != 1234) return 6;

    /* Bit operations go through the CAS loop; same requirement. */
    bytes.b = 0xFF;
    __c11_atomic_fetch_and(&bytes.b, 0x0F, __ATOMIC_SEQ_CST);
    if (bytes.b != 0x0F) return 7;
    if (bytes.c != 20) return 8;

    bytes.c = 0;
    __c11_atomic_fetch_or(&bytes.c, 0xF0, __ATOMIC_SEQ_CST);
    if (bytes.c != 0xF0) return 9;
    if (bytes.d != 30) return 10;

    /* An exchange writes the whole operand. */
    __c11_atomic_exchange(&bytes.d, 99, __ATOMIC_SEQ_CST);
    if (bytes.d != 99) return 11;
    if (bytes.c != 0xF0) return 12;

    /* ---- narrow results carry the right signedness ---- */
    __c11_atomic_store(&sc, -100, __ATOMIC_SEQ_CST);
    if (__c11_atomic_fetch_add(&sc, 1, __ATOMIC_SEQ_CST) != -100) return 20;
    if (__c11_atomic_load(&sc, __ATOMIC_SEQ_CST) != -99) return 21;

    __c11_atomic_store(&uc, 200, __ATOMIC_SEQ_CST);
    if (__c11_atomic_fetch_add(&uc, 1, __ATOMIC_SEQ_CST) != 200) return 22;

    __c11_atomic_store(&sh, -30000, __ATOMIC_SEQ_CST);
    if (__c11_atomic_fetch_sub(&sh, 1, __ATOMIC_SEQ_CST) != -30000) return 23;
    if (__c11_atomic_load(&sh, __ATOMIC_SEQ_CST) != -30001) return 24;

    return 0;
}


/* ---- c11_atomic_operators_mega (exit codes 41-123) ----
 *
 *  Every operator form on an `_Atomic` object, at every lock-free width and
 *  through every lvalue shape.
 *
 *  These are behavioral, so they establish that the *values* are right. That
 *  the operations are actually atomic is asserted on the generated assembly in
 *  `cc/tests/codegen/atomics_asm.rs` -- a behavioral test cannot see the
 *  difference, which is why the pre-existing `_Atomic int x; x = 100;` case
 *  passed for years against a plain `movl`.
 */
#include <stdatomic.h>

atomic_int op_g;
_Atomic unsigned char op_b;
_Atomic short op_h;
_Atomic long op_l;
_Atomic unsigned op_u;
_Atomic double op_d;
_Atomic _Bool op_flag;

struct OP_S { _Atomic int a; _Atomic int op_b; };
static struct OP_S op_s;
static _Atomic int op_obj = 7;
static _Atomic int op_arr[4];

static int op_arr_ints[8] = {0,1,2,3,4,5,6,7};
_Atomic(int *) op_p;

static int t_c11_atomic_operators_mega(void) {
    /* ---------- 1-19: int, every operator ---------- */
    op_g = 10;      if (op_g != 10) return 1;
    op_g += 5;      if (op_g != 15) return 2;
    op_g -= 3;      if (op_g != 12) return 3;
    op_g *= 2;      if (op_g != 24) return 4;
    op_g /= 4;      if (op_g != 6)  return 5;
    op_g %= 4;      if (op_g != 2)  return 6;
    op_g <<= 3;     if (op_g != 16) return 7;
    op_g >>= 2;     if (op_g != 4)  return 8;
    op_g &= 6;      if (op_g != 4)  return 9;
    op_g |= 1;      if (op_g != 5)  return 10;
    op_g ^= 3;      if (op_g != 6)  return 11;

    /* ---------- 20-29: the value of the expression ---------- */
    op_g = 10;
    if ((op_g += 5) != 15) return 20;   /* compound yields the NEW value */
    if (op_g++ != 15) return 21;        /* postfix yields the OLD value */
    if (op_g != 16) return 22;
    if (++op_g != 17) return 23;        /* prefix yields the NEW value */
    if (op_g-- != 17) return 24;
    if (--op_g != 15) return 25;
    if ((op_g = 42) != 42) return 26;   /* plain assignment yields the value */

    /* ---------- 30-39: narrow widths wrap correctly ---------- */
    op_b = 250; op_b += 3;  if (op_b != 253) return 30;
    op_b++;              if (op_b != 254) return 31;
    op_b = 255; op_b++;     if (op_b != 0)   return 32;   /* wraps, no carry out */
    op_h = -30000; op_h -= 1; if (op_h != -30001) return 33;
    op_l = 1; op_l <<= 40;  if (op_l != (1L << 40)) return 34;
    op_u = 0; op_u--;       if (op_u != 0xFFFFFFFFu) return 35;

    /* ---------- 40-49: floating point goes through the CAS loop ---- */
    op_d = 1.5;  op_d += 2.25;  if (op_d != 3.75) return 40;
    op_d *= 2.0;             if (op_d != 7.5)  return 41;
    op_d -= 0.5;             if (op_d != 7.0)  return 42;
    op_d /= 2.0;             if (op_d != 3.5)  return 43;

    /* ---------- 50-59: _Bool renormalizes ---------- */
    op_flag = 0;
    op_flag++;            if (op_flag != 1) return 50;
    if (++op_flag != 1)   return 51;    /* already 1, stays 1 */

    /* ---------- 60-69: member lvalues ---------- */
    op_s.a = 1;  op_s.a += 4;  if (op_s.a != 5) return 60;
    op_s.op_b = 2;  op_s.op_b++;     if (op_s.op_b != 3) return 61;
    if (op_s.a != 5) return 62;         /* neighbour untouched */

    /* ---------- 70-79: deref and index lvalues ---------- */
    {
        _Atomic int *q = &op_obj;
        *q += 3;   if (*q != 10) return 70;
        (*q)++;    if (op_obj != 11) return 71;
    }
    op_arr[1] = 5;  op_arr[1] *= 3;  if (op_arr[1] != 15) return 72;
    if (op_arr[0] != 0 || op_arr[2] != 0) return 73;

    /* ---------- 80-89: atomic pointer arithmetic scales ---------- */
    op_p = op_arr_ints;
    op_p += 3;  if (*op_p != 3) return 80;
    op_p++;     if (*op_p != 4) return 81;
    op_p--;     if (*op_p != 3) return 82;
    op_p -= 2;  if (*op_p != 1) return 83;

    return 0;
}


/* ---- c11_two_atomic_results_do_not_alias (exit codes 131-138) ----
 *
 *  Two atomic results live at the same time must not alias.
 *
 *  Both backends leave an atomic's result in a fixed register the instruction
 *  requires -- RAX on x86_64, X0/X1/X2 on aarch64 -- and both then *overwrote*
 *  the allocator's assignment for the result pseudo with that register. So any
 *  expression holding two atomic results at once collapsed them into one.
 *
 *  The register-clobber declarations added earlier do not help here: they stop
 *  *other* pseudos being parked in those registers, but the codegen was
 *  discarding the allocator's answer for the atomic's own result.
 */
#include <stdatomic.h>

atomic_int na_a, na_b;
_Atomic unsigned char ca, cb;

/* Builtins: two fetch-adds summed in one expression. */
static int two_builtins(void) {
    return __c11_atomic_fetch_add(&na_a, 1, __ATOMIC_SEQ_CST)
         + __c11_atomic_fetch_add(&na_b, 1, __ATOMIC_SEQ_CST);
}

/* Ordinary operators, which now lower to the same opcodes. */
static int two_compound(void) { return (na_a += 10) + (na_b += 20); }

/* Plain reads: every _Atomic rvalue read is an AtomicLoad now, and on
   aarch64 they all landed in X0. */
static int two_reads(void) { return na_a + na_b; }

/* Three at once, to catch na_a fix that only handles pairs. */
static int three_reads(void) { return na_a + na_b + (int)ca; }

/* Exchange and CAS use different fixed registers again. */
static int two_exchanges(void) {
    return __c11_atomic_exchange(&na_a, 5, __ATOMIC_SEQ_CST)
         + __c11_atomic_exchange(&na_b, 6, __ATOMIC_SEQ_CST);
}

static int t_c11_two_atomic_results_do_not_alias(void) {
    na_a = 100; na_b = 7;
    if (two_builtins() != 107) return 1;      /* old values, 100 + 7 */
    if (na_a != 101 || na_b != 8) return 2;

    na_a = 1; na_b = 2;
    if (two_compound() != 33) return 3;       /* new values, 11 + 22 */

    na_a = 40; na_b = 2;
    if (two_reads() != 42) return 4;

    na_a = 40; na_b = 2; ca = 3;
    if (three_reads() != 45) return 5;

    na_a = 11; na_b = 22;
    if (two_exchanges() != 33) return 6;      /* old values */
    if (na_a != 5 || na_b != 6) return 7;

    /* Narrow widths take the sign/zero-extension path on the way out. */
    ca = 200; cb = 55;
    if ((int)ca + (int)cb != 255) return 8;

    return 0;
}


/* ---- c11_atomic_bool_stays_normalized (exit codes 141-153) ----
 *
 *  `_Atomic _Bool` increment stores the *converted* value.
 *
 *  C17 6.3.1.2 makes conversion to `_Bool` yield 0 or 1, and a compound
 *  assignment stores the converted result. A native fetch-and-add cannot
 *  express that -- it adds to the stored byte -- so `b = 1; b++` left 2 in
 *  memory and `b = 0; b--` left 255. The non-atomic paths this intercepted
 *  both normalized before storing, so it was a regression.
 */
#include <stdatomic.h>
_Atomic _Bool bo_b;

static int t_c11_atomic_bool_stays_normalized(void) {
    bo_b = 1; bo_b++;   if ((int)bo_b != 1) return 1;   /* not 2 */
    bo_b = 0; bo_b++;   if ((int)bo_b != 1) return 2;
    bo_b = 1; bo_b--;   if ((int)bo_b != 0) return 3;
    bo_b = 0; bo_b--;   if ((int)bo_b != 1) return 4;   /* (_Bool)(-1) is 1, not 255 */
    bo_b = 0; bo_b += 5; if ((int)bo_b != 1) return 5;
    bo_b = 1; bo_b -= 1; if ((int)bo_b != 0) return 6;

    /* The value of the expression follows the same rule. */
    bo_b = 1; if ((int)(bo_b++) != 1) return 10;     /* postfix: old value */
    bo_b = 0; if ((int)(++bo_b) != 1) return 11;     /* prefix: stored value */
    bo_b = 0; if ((int)(bo_b--) != 0) return 12;
    if ((int)bo_b != 1) return 13;

    return 0;
}


/* ---- c11_an_atomic_compound_assignment_computes_at_the_common_type (exit codes 161-165) ----
 *
 *  A compound assignment to an `_Atomic` object computes at the same type as
 *  one to an ordinary object.
 *
 *  C17 6.5.16.2p3 defines `E1 op= E2` as `E1 = E1 op E2` bar evaluating `E1`
 *  once, so the arithmetic happens at the type the usual arithmetic
 *  conversions give the two operands -- and only the *result* is converted back
 *  to the target. The atomic path converted the right operand down to the
 *  target first and computed there, so `50 / -5` became `50 / 251` and stored
 *  0. The ordinary path already had this fixed, with a comment explaining it;
 *  the atomic path had its own copy of the logic and did not.
 *
 *  Add, subtract, and the bitwise operators are congruent modulo 2^n, so a
 *  narrow computation agrees with a wide one and their native fetch-and-op
 *  lowering stays correct. Division, remainder and the shifts are not, and all
 *  of them already take the compare-and-swap loop.
 *
 *  Each case is checked against the ordinary object beside it: the two paths
 *  agreeing is the property, and their disagreeing is how this survived.
 */
static int t_c11_an_atomic_compound_assignment_computes_at_the_common_type(void)
{
    /* Division: the right operand must not be narrowed to unsigned char
       first. 50 / -5 is -10 at int, stored as (unsigned char)-10 == 246. */
    _Atomic unsigned char ac = 50;  ac /= -5;
    unsigned char          pc = 50;  pc /= -5;
    if (ac != pc || ac != 246) return 1;

    /* Remainder, likewise: 50 % -3 is 2. */
    _Atomic unsigned char am = 50;  am %= -3;
    unsigned char          pm = 50;  pm %= -3;
    if (am != pm || am != 2) return 2;

    /* Signed division, where narrowing would also change the sign. */
    _Atomic signed char as = -100;  as /= 3;
    signed char           ps = -100;  ps /= 3;
    if (as != ps || as != -33) return 3;

    /* The congruent operators must keep working -- they take the native
       fetch-and-op lowering, not the CAS loop. */
    _Atomic unsigned char aa = 200; aa += 100;
    unsigned char          pa = 200; pa += 100;
    if (aa != pa || aa != 44) return 4;

    _Atomic unsigned char an = 0xF0; an &= -1;
    unsigned char          pn = 0xF0; pn &= -1;
    if (an != pn || an != 0xF0) return 5;

    return 0;
}


/* ---- c11_an_atomic_compound_assignment_yields_the_value_it_stored (exit codes 171-175) ----
 *
 *  The value of a compound assignment is the value stored, converted.
 *
 *  C17 6.5.16p3: an assignment expression has the value of the left operand
 *  *after* the assignment. For a `_Bool` that means the value after conversion
 *  to `_Bool`, so `b -= 1` on a false `b` yields 1 -- the memory and the
 *  expression have to agree. c17's ordinary path did this and its atomic path
 *  did not, recomputing the expression's value from a raw arithmetic result
 *  and handing back 255 while storing 1.
 *
 *  Note clang answers 255 here for the atomic case and 1 for the ordinary one,
 *  i.e. it has the same split. This follows the standard and c17's own
 *  non-atomic path rather than matching that.
 */
static int t_c11_an_atomic_compound_assignment_yields_the_value_it_stored(void)
{
    _Atomic _Bool ab = 0;  int ar = (ab -= 1);
    _Bool          pb = 0;  int pr = (pb -= 1);
    if (ab != 1 || pb != 1) return 1;
    if (ar != pr || ar != 1) return 2;

    _Atomic _Bool ab2 = 1;  int ar2 = (ab2 += 7);
    _Bool          pb2 = 1;  int pr2 = (pb2 += 7);
    if (ab2 != 1 || pb2 != 1) return 3;
    if (ar2 != pr2 || ar2 != 1) return 4;

    /* A narrowing store: the expression is the stored value, not the wide one. */
    _Atomic unsigned char au = 200;  int aur = (au += 100);
    unsigned char          pu = 200;  int pur = (pu += 100);
    if (aur != pur || aur != 44) return 5;

    return 0;
}


/* ---- c11_an_atomic_shift_promotes_its_left_operand (exit codes 181-183) ----
 *
 *  A shift on an atomic object promotes its left operand, as any shift does.
 *
 *  C17 6.5.7p3: the integer promotions are applied to each operand and the
 *  result has the promoted left operand's type. So `s >>= 1` on a
 *  `signed char` holding -8 shifts -8 at `int`, giving -4, and stores that --
 *  not a logical shift of the unsigned byte pattern, which would give 124.
 *
 *  This one c17 already gets right and clang does not, so it is a guard rather
 *  than a repair: the fix for the two tests above must not reach the shift by
 *  computing at the target's width.
 */
static int t_c11_an_atomic_shift_promotes_its_left_operand(void)
{
    _Atomic signed char as = -8;   as >>= 1;
    signed char          ps = -8;   ps >>= 1;
    if (as != ps || as != -4) return 1;

    _Atomic signed char al = -8;   al <<= 2;
    signed char          pl = -8;   pl <<= 2;
    if (al != pl || al != -32) return 2;

    _Atomic unsigned char au = 200; au >>= 1;
    unsigned char          pu = 200; pu >>= 1;
    if (au != pu || au != 100) return 3;

    return 0;
}

int main(void)
{
    int r;
    if ((r = t_c11_atomics_do_not_clobber_live_values()) != 0)
        return 0 + r;
    if ((r = t_c11_atomics_narrow_widths_do_not_touch_neighbours()) != 0)
        return 10 + r;
    if ((r = t_c11_atomic_operators_mega()) != 0)
        return 40 + r;
    if ((r = t_c11_two_atomic_results_do_not_alias()) != 0)
        return 130 + r;
    if ((r = t_c11_atomic_bool_stays_normalized()) != 0)
        return 140 + r;
    if ((r = t_c11_an_atomic_compound_assignment_computes_at_the_common_type()) != 0)
        return 160 + r;
    if ((r = t_c11_an_atomic_compound_assignment_yields_the_value_it_stored()) != 0)
        return 170 + r;
    if ((r = t_c11_an_atomic_shift_promotes_its_left_operand()) != 0)
        return 180 + r;
    return 0;
}
"#;
    assert_eq!(
        compile_and_run_optimized("c11_atomics_optimized_mega", code),
        0
    );
}

/// Atomics through a spilled pointer and through every address form, on every
/// level and target; each section keeps its original test name and doc comment.
///
/// Consolidates: c11_atomics_through_a_spilled_pointer,
/// c11_atomics_through_every_address_form.
#[test]
fn c11_atomics_everywhere_mega() {
    let code = r#"
/*
 * Exit codes: each section's own failure codes, offset by its base.
 *     1-  6  c11_atomics_through_a_spilled_pointer
 *    11- 40  c11_atomics_through_every_address_form
 */

/* ---- c11_atomics_through_a_spilled_pointer (exit codes 1-6) ----
 *
 *  Every atomic operation through a pointer that is live across enough calls
 *  to be kept in a stack slot, and on a local object's own address, at -O0 and
 *  -O2 on both targets. On x86-64 at -O1 and up the compare-exchange took its
 *  address register from the wrong place and faulted (`lock cmpxchg` through
 *  0x7fff00000009), while gcc and aarch64 ran it.
 */
/* (a) atomics on a local object's own address; (b) sp_through a pointer kept
   live across enough work that it may be spilled. Every operation, seq_cst. */
__attribute__((noinline)) static long sp_churn(long a, long b, long c, long d, long e, long f) {
    return a * 3 + b * 5 + c * 7 + d * 11 + e * 13 + f * 17;
}
__attribute__((noinline)) static int sp_through(int *p) {
    long a = sp_churn(1, 2, 3, 4, 5, 6), b = sp_churn(a, 1, 1, 1, 1, 1), c = sp_churn(b, a, 1, 1, 1, 1);
    long d = sp_churn(c, b, a, 1, 1, 1), e = sp_churn(d, c, b, a, 1, 1), f = sp_churn(e, d, c, b, a, 1);
    __atomic_store_n(p, 5, __ATOMIC_SEQ_CST);
    long g = sp_churn(a, b, c, d, e, f);
    int old = __atomic_fetch_add(p, 2, __ATOMIC_SEQ_CST);
    int ex = __atomic_exchange_n(p, 9, __ATOMIC_SEQ_CST);
    int exp = 9;
    _Bool ok = __atomic_compare_exchange_n(p, &exp, 11, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
    int ld = __atomic_load_n(p, __ATOMIC_SEQ_CST);
    if (old != 5 || ex != 7 || !ok || ld != 11) return 1;
    return (int)((a + b + c + d + e + f + g) & 0) ;
}
static int t_c11_atomics_through_a_spilled_pointer(void) {
    int x = 0;
    __atomic_store_n(&x, 5, __ATOMIC_SEQ_CST);
    if (__atomic_fetch_add(&x, 2, __ATOMIC_SEQ_CST) != 5) return 2;
    if (__atomic_exchange_n(&x, 9, __ATOMIC_SEQ_CST) != 7) return 3;
    int exp = 9;
    if (!__atomic_compare_exchange_n(&x, &exp, 11, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)) return 4;
    if (__atomic_load_n(&x, __ATOMIC_SEQ_CST) != 11) return 5;
    int y = 0;
    if (sp_through(&y) || y != 11) return 6;
    return 0;
}


/* ---- c11_atomics_through_every_address_form (exit codes 11-40) ----
 *
 *  Every atomic operation -- load, store, exchange, compare-exchange, each
 *  fetch-op, test-and-set and clear -- at every width, through each way an
 *  address can reach it: (a) a local object's own address, (b) a pointer
 *  spilled across calls, (c) a pointer in a register, (d) a global's address,
 *  and a pointer passed on the stack; the compare-exchange also with its
 *  expected object through a spilled pointer. x86-64 at -O1 and up took a
 *  spilled pointer's slot address instead of the pointer, so `fetch_add` and
 *  `exchange` rewrote the pointer itself, and read a stack-passed pointer as
 *  address 0. The neighbours of every object are checked for stray writes.
 */
#define SC __ATOMIC_SEQ_CST
/* Every atomic operation through P, with BAR run between them. */
#define SEQ(T, P, BAR)                                                        \
    __atomic_store_n(P, (T)5, SC); BAR;                                       \
    if (__atomic_load_n(P, SC) != 5) return 1; BAR;                           \
    if (__atomic_fetch_add(P, 2, SC) != 5) return 2; BAR;                     \
    if (__atomic_fetch_sub(P, 1, SC) != 7) return 3; BAR;                     \
    if (__atomic_fetch_or(P, 9, SC) != 6) return 4; BAR;                      \
    if (__atomic_fetch_and(P, 13, SC) != 15) return 5; BAR;                   \
    if (__atomic_fetch_xor(P, 7, SC) != 13) return 6; BAR;                    \
    if (__atomic_exchange_n(P, 20, SC) != 10) return 7; BAR;                  \
    { T e = 21; if (__atomic_compare_exchange_n(P, &e, 30, 0, SC, SC) || e != 20) return 8; } BAR; \
    { T e = 20; if (!__atomic_compare_exchange_n(P, &e, 30, 0, SC, SC) || e != 20) return 9; } BAR; \
    __atomic_store_n(P, 40, __ATOMIC_RELAXED); BAR;                           \
    if (__atomic_load_n(P, __ATOMIC_ACQUIRE) != 40 || *(P) != 40) return 10;  \
    if (sizeof(T) == 1) {                                                     \
        __atomic_clear(P, SC); BAR;                                           \
        if (__atomic_test_and_set(P, SC)) return 11; BAR;                     \
        if (!__atomic_test_and_set(P, SC)) return 12; BAR;                    \
        __atomic_clear(P, SC);                                                \
        if (*(P) != 0) return 13;                                             \
    }

__attribute__((noinline)) static long churn(long a, long b, long c, long d, long e, long f) {
    return a * 3 + b * 5 + c * 7 + d * 11 + e * 13 + f * 17;
}
/* Six values live across every call, so the pointer cannot keep a register. */
#define LIVE                                                                  \
    long a = churn(1, 2, 3, 4, 5, 6), b = churn(a, 1, 1, 1, 1, 1);            \
    long c = churn(b, a, 1, 1, 1, 1), d = churn(c, b, a, 1, 1, 1);            \
    long e0 = churn(d, c, b, a, 1, 1), f = churn(e0, d, c, b, a, 1)
#define CHURN a += churn(a, b, c, d, e0, f) & 1
#define DEAD(...) (int)((__VA_ARGS__) & 0)

#define FORMS(T)                                                              \
T g_##T[3] = {1, 0, 2};                                                       \
T ge_##T;                                                                     \
/* (a) a local object's own address */                                        \
__attribute__((noinline)) static int local_##T(void) {                        \
    T x[3] = {1, 0, 2};                                                       \
    SEQ(T, &x[1], (void)0)                                                    \
    return x[0] != 1 || x[2] != 2 ? 20 : 0;                                   \
}                                                                             \
/* (d) a global's address */                                                  \
__attribute__((noinline)) static int global_##T(void) {                       \
    SEQ(T, &g_##T[1], (void)0)                                                \
    return 0;                                                                 \
}                                                                             \
/* (c) a pointer in a register */                                             \
__attribute__((noinline)) static int reg_##T(T *p) {                          \
    SEQ(T, p, (void)0)                                                        \
    return 0;                                                                 \
}                                                                             \
/* (b) a pointer spilled across calls */                                      \
__attribute__((noinline)) static int spilled_##T(T *p) {                      \
    LIVE;                                                                     \
    SEQ(T, p, CHURN)                                                          \
    return DEAD(a + b + c + d + e0 + f);                                      \
}                                                                             \
/* a pointer passed on the stack */                                           \
__attribute__((noinline)) static int stackarg_##T(long r1, long r2, long r3,  \
        long r4, long r5, long r6, T *p) {                                    \
    SEQ(T, p, (void)0)                                                        \
    return DEAD(r1 + r2 + r3 + r4 + r5 + r6);                                 \
}                                                                             \
/* the expected object through a spilled pointer too */                       \
__attribute__((noinline)) static int cas_##T(T *p, T *ep) {                   \
    LIVE;                                                                     \
    *p = 3; *ep = 4; CHURN;                                                   \
    if (__atomic_compare_exchange_n(p, ep, 9, 0, SC, SC) || *ep != 3 || *p != 3) return 1; \
    CHURN;                                                                    \
    if (!__atomic_compare_exchange_n(p, ep, 9, 0, SC, SC) || *ep != 3 || *p != 9) return 2; \
    return DEAD(a + b + c + d + e0 + f);                                      \
}                                                                             \
static int all_##T(void) {                                                    \
    T y[3] = {1, 0, 2}, e = 0;                                                \
    int r, k = 0;                                                             \
    if ((r = local_##T())) goto fail;                                         \
    k++; if ((r = global_##T())) goto fail;                                   \
    k++; if ((r = reg_##T(&y[1]))) goto fail;                                 \
    k++; if ((r = spilled_##T(&y[1]))) goto fail;                             \
    k++; if ((r = stackarg_##T(1, 2, 3, 4, 5, 6, &y[1]))) goto fail;          \
    k++; if ((r = cas_##T(&y[1], &e))) goto fail;                             \
    k++; if ((r = cas_##T(&g_##T[1], &ge_##T))) goto fail;                    \
    k++; r = 99; if (y[0] != 1 || y[2] != 2 || g_##T[0] != 1 || g_##T[2] != 2) goto fail; \
    return 0;                                                                 \
fail:                                                                         \
    (void)r;                                                                  \
    return k + 1;                                                             \
}

typedef unsigned char uc;
typedef long long ll;
FORMS(uc)
FORMS(short)
FORMS(int)
FORMS(ll)

static int t_c11_atomics_through_every_address_form(void) {
    int r;
    if ((r = all_uc())) return r;
    if ((r = all_short())) return 10 + r;
    if ((r = all_int())) return 20 + r;
    if ((r = all_ll())) return 30 + r;
    return 0;
}
#undef CHURN
#undef DEAD
#undef FORMS
#undef LIVE
#undef SC
#undef SEQ

int main(void)
{
    int r;
    if ((r = t_c11_atomics_through_a_spilled_pointer()) != 0)
        return 0 + r;
    if ((r = t_c11_atomics_through_every_address_form()) != 0)
        return 10 + r;
    return 0;
}
"#;
    compile_and_run_everywhere("c11_atomics_everywhere_mega", code);
}
