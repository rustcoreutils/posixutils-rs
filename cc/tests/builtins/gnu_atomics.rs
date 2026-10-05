//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// gcc's `__atomic_*` and `__sync_*` builtins.
//
// These are not an alternate spelling of C11 atomics that code could do
// without: `pycore_atomic.h`, `valgrind/config.h` and `pyconfig.h` all reach
// for them directly, so a translation unit that includes one did not compile.
//

use crate::common::{compile_and_run, compile_and_run_aarch64, compile_and_run_optimized};

/// Every operation in both families, at one width, against hand-computed
/// answers. Each case checks the *returned* value and the object afterwards,
/// because `fetch_op` and `op_fetch` differ only in the first.
const GNU_ATOMICS: &str = r#"
int main(void) {
    int v;

    v = 10;
    if (__sync_fetch_and_add(&v, 5) != 10 || v != 15) return 1;
    if (__sync_add_and_fetch(&v, 5) != 20 || v != 20) return 2;
    if (__sync_fetch_and_sub(&v, 5) != 20 || v != 15) return 3;
    if (__sync_sub_and_fetch(&v, 5) != 10 || v != 10) return 4;

    v = 0xF0; if (__sync_fetch_and_and(&v, 0x3C) != 0xF0 || v != 0x30) return 5;
    v = 0xF0; if (__sync_and_and_fetch(&v, 0x3C) != 0x30 || v != 0x30) return 6;
    v = 0xF0; if (__sync_fetch_and_or(&v, 0x0F) != 0xF0 || v != 0xFF) return 7;
    v = 0xF0; if (__sync_or_and_fetch(&v, 0x0F) != 0xFF || v != 0xFF) return 8;
    v = 0xF0; if (__sync_fetch_and_xor(&v, 0xFF) != 0xF0 || v != 0x0F) return 9;
    v = 0xF0; if (__sync_xor_and_fetch(&v, 0xFF) != 0x0F || v != 0x0F) return 10;

    /* nand is `~(old & val)`. No target has the instruction, so this is the
       CAS loop, and it is the operation the C11 set has no counterpart for. */
    v = 0xF0; if (__sync_fetch_and_nand(&v, 0x3C) != 0xF0 || v != (int)~0x30) return 11;
    v = 0xF0; if (__sync_nand_and_fetch(&v, 0x3C) != (int)~0x30) return 12;

    v = 7;
    if (!__sync_bool_compare_and_swap(&v, 7, 9) || v != 9) return 13;
    if (__sync_bool_compare_and_swap(&v, 7, 11) || v != 9) return 14;
    /* The `val` form answers the old value on both paths. */
    if (__sync_val_compare_and_swap(&v, 9, 13) != 9 || v != 13) return 15;
    if (__sync_val_compare_and_swap(&v, 9, 15) != 13 || v != 13) return 16;

    if (__sync_lock_test_and_set(&v, 42) != 13 || v != 42) return 17;
    __sync_lock_release(&v);
    if (v != 0) return 18;
    __sync_synchronize();

    v = 1;
    if (__atomic_load_n(&v, __ATOMIC_SEQ_CST) != 1) return 19;
    __atomic_store_n(&v, 3, __ATOMIC_SEQ_CST);
    if (v != 3) return 20;
    if (__atomic_exchange_n(&v, 4, __ATOMIC_SEQ_CST) != 3 || v != 4) return 21;
    if (__atomic_fetch_add(&v, 1, __ATOMIC_SEQ_CST) != 4 || v != 5) return 22;
    if (__atomic_add_fetch(&v, 1, __ATOMIC_SEQ_CST) != 6 || v != 6) return 23;
    if (__atomic_fetch_nand(&v, 3, __ATOMIC_SEQ_CST) != 6) return 24;

    {
        int expected = 8;
        v = 8;
        if (!__atomic_compare_exchange_n(&v, &expected, 10, 0,
                                         __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)) return 25;
        if (v != 10) return 26;
        /* On failure the observed value is written back through `expected`. */
        if (__atomic_compare_exchange_n(&v, &expected, 12, 0,
                                        __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)) return 27;
        if (expected != 10) return 28;
    }

    {
        char flag = 0;
        if (__atomic_test_and_set(&flag, __ATOMIC_SEQ_CST) != 0) return 29;
        if (flag == 0) return 30;
        if (__atomic_test_and_set(&flag, __ATOMIC_SEQ_CST) == 0) return 31;
        __atomic_clear(&flag, __ATOMIC_SEQ_CST);
        if (flag != 0) return 32;
    }

    __atomic_thread_fence(__ATOMIC_SEQ_CST);
    __atomic_signal_fence(__ATOMIC_SEQ_CST);

    if (!__atomic_always_lock_free(4, 0)) return 33;
    if (__atomic_always_lock_free(32, 0)) return 34;
    if (!__atomic_is_lock_free(8, 0)) return 35;
    return 0;
}
"#;

#[test]
fn builtins_gnu_atomics() {
    assert_eq!(compile_and_run("gnu_atomics", GNU_ATOMICS, &[]), 0);
}

#[test]
fn builtins_gnu_atomics_optimized() {
    assert_eq!(compile_and_run_optimized("gnu_atomics_opt", GNU_ATOMICS), 0);
}

#[test]
fn builtins_gnu_atomics_aarch64() {
    // aarch64 reaches the same operations through an LL/SC loop, and `nand`
    // reaches the CAS loop on both. A loop that is right on one target and
    // wrong on the other is the shape this catches.
    if let Some(code) = compile_and_run_aarch64("gnu_atomics_a64", GNU_ATOMICS, "-O2") {
        assert_eq!(code, 0);
    }
}

/// The GNU atomic builtins at every width, evaluating their operand once, and
/// mixed with C11 atomics, as one program; each section keeps its original test
/// name and doc comment.
///
/// Consolidates: builtins_gnu_atomics_at_every_width,
/// builtins_gnu_atomics_evaluate_the_operand_once and
/// builtins_gnu_atomics_agree_with_c11_atomics. The assembly cases are unit
/// tests in cc/test_asm/builtins_gnu_atomics.rs.
#[test]
fn builtins_gnu_atomics_mega() {
    let code = r#"
/*
 * Exit codes: each section's own failure codes, offset by its base.
 *     1-  8  builtins_gnu_atomics_at_every_width
 *    11- 13  builtins_gnu_atomics_evaluate_the_operand_once
 *    21- 25  builtins_gnu_atomics_agree_with_c11_atomics
 */

/* ---- builtins_gnu_atomics_at_every_width (exit codes 1-8) ----
 *
 *  Every integer width, because the memory operand has to be exactly as wide
 *  as the object: widening it made an 8- or 16-bit read-modify-write touch its
 *  neighbours, which is a bug this family would otherwise re-introduce.
 */
struct pack { unsigned char before; unsigned char v; unsigned char after; };
static int t_builtins_gnu_atomics_at_every_width(void) {
    { struct pack p = { 0xAA, 1, 0xBB };
      if (__sync_fetch_and_add(&p.v, 1) != 1 || p.v != 2) return 1;
      if (p.before != 0xAA || p.after != 0xBB) return 2; }
    { short v = 0x0102;
      if (__sync_fetch_and_or(&v, 0x0010) != 0x0102 || v != 0x0112) return 3; }
    { unsigned int v = 0x01020304u;
      if (__sync_fetch_and_xor(&v, 0xFFu) != 0x01020304u || v != 0x010203FBu) return 4; }
    { long long v = 0x0102030405060708LL;
      if (__sync_fetch_and_sub(&v, 8) != 0x0102030405060708LL) return 5;
      if (v != 0x0102030405060700LL) return 6; }
    { void *p = (void *)0; char buf[4];
      /* pointer arithmetic scales by the element size, as `+=` does */
      int *ip = (int *)buf;
      if (__sync_fetch_and_add(&ip, 1) != (int *)buf) return 7;
      if (ip != (int *)buf + 1) return 8;
      (void)p; }
    return 0;
}


/* ---- builtins_gnu_atomics_evaluate_the_operand_once (exit codes 11-13) ----
 *
 *  The operand is evaluated exactly once, although `*_and_fetch` and `nand`
 *  each read it twice in the lowering. Re-applying the operation works on the
 *  value already in hand, not on a second evaluation of the expression.
 */
int eo_calls;
int eo_one(void) { eo_calls++; return 1; }

static int t_builtins_gnu_atomics_evaluate_the_operand_once(void) {
    int v = 0;
    if (__sync_add_and_fetch(&v, eo_one()) != 1 || eo_calls != 1) return 1;
    eo_calls = 0;
    if (__sync_nand_and_fetch(&v, eo_one()) != (int)~1 || eo_calls != 1) return 2;
    eo_calls = 0;
    v = 5;
    if (__sync_fetch_and_add(&v, eo_one()) != 5 || eo_calls != 1 || v != 6) return 3;
    return 0;
}


/* ---- builtins_gnu_atomics_agree_with_c11_atomics (exit codes 21-25) ----
 *
 *  `_Atomic` objects, the C11 builtins and the GNU ones all reach the same
 *  memory. They lower through one path, so a program may mix them.
 */
#include <stdatomic.h>
static int t_builtins_gnu_atomics_agree_with_c11_atomics(void) {
    _Atomic int v = 0;
    atomic_store(&v, 5);
    if (__atomic_load_n(&v, __ATOMIC_SEQ_CST) != 5) return 1;
    if (__sync_fetch_and_add(&v, 3) != 5) return 2;
    if (atomic_load(&v) != 8) return 3;
    v += 2;
    if (__sync_val_compare_and_swap(&v, 10, 11) != 10) return 4;
    if (atomic_load(&v) != 11) return 5;
    return 0;
}

int main(void)
{
    int r;
    if ((r = t_builtins_gnu_atomics_at_every_width()) != 0)
        return 0 + r;
    if ((r = t_builtins_gnu_atomics_evaluate_the_operand_once()) != 0)
        return 10 + r;
    if ((r = t_builtins_gnu_atomics_agree_with_c11_atomics()) != 0)
        return 20 + r;
    return 0;
}
"#;
    assert_eq!(compile_and_run("builtins_gnu_atomics_mega", code, &[]), 0);
}

/// Every operation at every order it admits, with checked results, at -O0
/// and -O2 on the host and on aarch64. An order only changes which barrier
/// halves an instruction carries, so each must still compute the same
/// thing -- the LL/SC loop with a plain `ldxr`/`stxr` included.
#[test]
fn builtins_atomics_run_at_every_memory_order() {
    let code = r#"
#define T(c) do { if (!(c)) return __LINE__; } while (0)

/* Read-modify-writes and fences take every order. */
#define RMW(O) static int rmw_##O(void) {                                     \
    int v = 10; long l = 1; short s = 7; unsigned char f = 0;              \
    T(__atomic_exchange_n(&v, 20, O) == 10 && v == 20);                    \
    T(__atomic_fetch_add(&v, 5, O) == 20 && v == 25);                      \
    T(__atomic_add_fetch(&v, 5, O) == 30 && v == 30);                      \
    T(__atomic_fetch_sub(&v, 10, O) == 30 && v == 20);                     \
    T(__atomic_sub_fetch(&v, 10, O) == 10 && v == 10);                     \
    v = 0xF0; T(__atomic_fetch_and(&v, 0x3C, O) == 0xF0 && v == 0x30);     \
    v = 0xF0; T(__atomic_and_fetch(&v, 0x3C, O) == 0x30 && v == 0x30);     \
    v = 0xF0; T(__atomic_fetch_or(&v, 0x0F, O) == 0xF0 && v == 0xFF);      \
    v = 0xF0; T(__atomic_or_fetch(&v, 0x0F, O) == 0xFF && v == 0xFF);      \
    v = 0xF0; T(__atomic_fetch_xor(&v, 0xFF, O) == 0xF0 && v == 0x0F);     \
    v = 0xF0; T(__atomic_xor_fetch(&v, 0xFF, O) == 0x0F && v == 0x0F);     \
    v = 0xF0; T(__atomic_fetch_nand(&v, 0x3C, O) == 0xF0 && v == ~0x30);   \
    v = 0xF0; T(__atomic_nand_fetch(&v, 0x3C, O) == ~0x30 && v == ~0x30);  \
    T(__atomic_fetch_add(&l, 1L << 40, O) == 1 && l == (1L << 40) + 1);    \
    T(__atomic_exchange_n(&s, (short)-2, O) == 7 && s == -2);              \
    T(__atomic_test_and_set(&f, O) == 0 && f != 0);                        \
    T(__atomic_test_and_set(&f, O) != 0);                                  \
    __atomic_thread_fence(O);                                              \
    return 0; }

/* A load takes relaxed, consume, acquire and seq-cst. */
#define LOAD(O) static int load_##O(void) {                                   \
    int v = 42; long l = -3; unsigned char c = 200;                        \
    T(__atomic_load_n(&v, O) == 42);                                       \
    T(__atomic_load_n(&l, O) == -3);                                       \
    T(__atomic_load_n(&c, O) == 200);                                      \
    return 0; }

/* A store takes relaxed, release and seq-cst. */
#define STORE(O) static int store_##O(void) {                                 \
    int v = 0; long l = 0; unsigned char f = 1;                            \
    __atomic_store_n(&v, 42, O); T(v == 42);                               \
    __atomic_store_n(&l, 1L << 40, O); T(l == 1L << 40);                   \
    __atomic_clear(&f, O); T(f == 0);                                      \
    return 0; }

/* A compare-exchange, both outcomes, for one success/failure pair. */
#define CAS(S, F) static int cas_##S##_##F(void) {                            \
    int v = 5, e = 5; long l = 9, el = 8;                                  \
    T(__atomic_compare_exchange_n(&v, &e, 6, 0, S, F) && v == 6 && e == 5);\
    T(!__atomic_compare_exchange_n(&v, &e, 7, 0, S, F) && v == 6 && e == 6);\
    T(!__atomic_compare_exchange_n(&l, &el, 1, 1, S, F) && l == 9 && el == 9);\
    T(__atomic_compare_exchange_n(&l, &el, 1, 1, S, F) && l == 1);         \
    return 0; }

#define R __ATOMIC_RELAXED
#define C __ATOMIC_CONSUME
#define A __ATOMIC_ACQUIRE
#define REL __ATOMIC_RELEASE
#define AR __ATOMIC_ACQ_REL
#define SC __ATOMIC_SEQ_CST

RMW(R) RMW(C) RMW(A) RMW(REL) RMW(AR) RMW(SC)
LOAD(R) LOAD(C) LOAD(A) LOAD(SC)
STORE(R) STORE(REL) STORE(SC)
CAS(R, R) CAS(C, R) CAS(C, C) CAS(A, R) CAS(A, A) CAS(REL, R) CAS(REL, A)
CAS(AR, R) CAS(AR, A) CAS(SC, R) CAS(SC, A) CAS(SC, SC)

/* An order that is not a constant is seq-cst, whatever its value. */
static int dynamic(int o) {
    int v = 1, e = 1;
    T(__atomic_fetch_add(&v, 1, o) == 1 && __atomic_load_n(&v, o) == 2);
    T(__atomic_compare_exchange_n(&v, &e, 3, 0, o, o) == 0 && e == 2);
    return 0;
}

int main(void) {
    int (*tests[])(void) = {
        rmw_R, rmw_C, rmw_A, rmw_REL, rmw_AR, rmw_SC,
        load_R, load_C, load_A, load_SC,
        store_R, store_REL, store_SC,
        cas_R_R, cas_C_R, cas_C_C, cas_A_R, cas_A_A, cas_REL_R, cas_REL_A,
        cas_AR_R, cas_AR_A, cas_SC_R, cas_SC_A, cas_SC_SC,
    };
    for (unsigned i = 0; i < sizeof tests / sizeof tests[0]; i++) {
        int line = tests[i]();
        if (line) return line % 256 ? line % 256 : 1;
    }
    for (int o = 0; o <= 5; o++)
        if (dynamic(o)) return 2;
    return 0;
}
"#;
    crate::common::compile_and_run_everywhere("atomics_every_order", code);
}
