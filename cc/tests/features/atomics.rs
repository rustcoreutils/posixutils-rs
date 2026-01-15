//
// Copyright (c) 2025-2026 Jeff Garzik
//
// This file is part of the posixutils-rs project covered under
// the MIT License.  For the full license text, please see the LICENSE
// file in the root directory of this project.
// SPDX-License-Identifier: MIT
//
// Tests for C11 atomic builtins (__c11_atomic_*, <stdatomic.h>)
//
// Tests are aggregated to reduce compile/link cycles while maintaining coverage.
// Note: These are single-threaded tests that verify correct code generation.
//

use crate::common::compile_and_run;

// ============================================================================
// __c11_atomic_load and __c11_atomic_store: Basic load/store operations
// ============================================================================

#[test]
fn atomic_load_store() {
    let code = r#"
int main(void) {
    // Test 1: Basic store and load
    int x = 0;
    __c11_atomic_store(&x, 42, __ATOMIC_SEQ_CST);
    int val = __c11_atomic_load(&x, __ATOMIC_SEQ_CST);
    if (val != 42) return 1;

    // Test 2: Relaxed ordering
    __c11_atomic_store(&x, 100, __ATOMIC_RELAXED);
    val = __c11_atomic_load(&x, __ATOMIC_RELAXED);
    if (val != 100) return 2;

    // Test 3: Acquire/Release ordering
    __c11_atomic_store(&x, 200, __ATOMIC_RELEASE);
    val = __c11_atomic_load(&x, __ATOMIC_ACQUIRE);
    if (val != 200) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("atomic_load_store", code, &[]), 0);
}

// ============================================================================
// __c11_atomic_fetch_add: Returns old value, adds to atomic
// ============================================================================

#[test]
fn atomic_fetch_add() {
    let code = r#"
int main(void) {
    // Test 1: Basic fetch_add returns old value
    int x = 10;
    int old = __c11_atomic_fetch_add(&x, 5, __ATOMIC_SEQ_CST);
    if (old != 10) return 1;
    if (x != 15) return 2;

    // Test 2: Multiple fetch_add operations
    old = __c11_atomic_fetch_add(&x, 3, __ATOMIC_SEQ_CST);
    if (old != 15) return 3;
    old = __c11_atomic_fetch_add(&x, 2, __ATOMIC_SEQ_CST);
    if (old != 18) return 4;
    if (x != 20) return 5;

    // Test 3: Negative value
    old = __c11_atomic_fetch_add(&x, -10, __ATOMIC_SEQ_CST);
    if (old != 20) return 6;
    if (x != 10) return 7;

    return 0;
}
"#;
    assert_eq!(compile_and_run("atomic_fetch_add", code, &[]), 0);
}

// ============================================================================
// __c11_atomic_fetch_sub: Returns old value, subtracts from atomic
// ============================================================================

#[test]
fn atomic_fetch_sub() {
    let code = r#"
int main(void) {
    // Test 1: Basic fetch_sub returns old value
    int x = 20;
    int old = __c11_atomic_fetch_sub(&x, 5, __ATOMIC_SEQ_CST);
    if (old != 20) return 1;
    if (x != 15) return 2;

    // Test 2: Multiple fetch_sub operations
    old = __c11_atomic_fetch_sub(&x, 10, __ATOMIC_SEQ_CST);
    if (old != 15) return 3;
    if (x != 5) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("atomic_fetch_sub", code, &[]), 0);
}

// ============================================================================
// __c11_atomic_fetch_and/or/xor: Bitwise operations
// ============================================================================

#[test]
fn atomic_fetch_bitwise() {
    let code = r#"
int main(void) {
    // Test 1: fetch_and
    int x = 0xFF;
    int old = __c11_atomic_fetch_and(&x, 0x0F, __ATOMIC_SEQ_CST);
    if (old != 0xFF) return 1;
    if (x != 0x0F) return 2;

    // Test 2: fetch_or
    x = 0x0F;
    old = __c11_atomic_fetch_or(&x, 0xF0, __ATOMIC_SEQ_CST);
    if (old != 0x0F) return 3;
    if (x != 0xFF) return 4;

    // Test 3: fetch_xor
    x = 0xFF;
    old = __c11_atomic_fetch_xor(&x, 0x0F, __ATOMIC_SEQ_CST);
    if (old != 0xFF) return 5;
    if (x != 0xF0) return 6;

    return 0;
}
"#;
    assert_eq!(compile_and_run("atomic_fetch_bitwise", code, &[]), 0);
}

// ============================================================================
// __c11_atomic_exchange: Swaps value, returns old value
// ============================================================================

#[test]
fn atomic_exchange() {
    let code = r#"
int main(void) {
    // Test 1: Basic exchange
    int x = 100;
    int old = __c11_atomic_exchange(&x, 200, __ATOMIC_SEQ_CST);
    if (old != 100) return 1;
    if (x != 200) return 2;

    // Test 2: Multiple exchanges
    old = __c11_atomic_exchange(&x, 300, __ATOMIC_SEQ_CST);
    if (old != 200) return 3;
    old = __c11_atomic_exchange(&x, 400, __ATOMIC_SEQ_CST);
    if (old != 300) return 4;
    if (x != 400) return 5;

    return 0;
}
"#;
    assert_eq!(compile_and_run("atomic_exchange", code, &[]), 0);
}

// ============================================================================
// __c11_atomic_compare_exchange_strong: Compare-and-swap operation
// ============================================================================

#[test]
fn atomic_compare_exchange() {
    let code = r#"
int main(void) {
    // Test 1: CAS success - expected matches
    int x = 100;
    int expected = 100;
    int success = __c11_atomic_compare_exchange_strong(&x, &expected, 200, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
    if (!success) return 1;
    if (x != 200) return 2;
    if (expected != 100) return 3;  // expected unchanged on success

    // Test 2: CAS failure - expected doesn't match
    expected = 100;  // wrong expected value (x is now 200)
    success = __c11_atomic_compare_exchange_strong(&x, &expected, 300, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
    if (success) return 4;  // should fail
    if (x != 200) return 5;  // x unchanged on failure
    if (expected != 200) return 6;  // expected updated to actual value

    // Test 3: CAS success after updating expected
    success = __c11_atomic_compare_exchange_strong(&x, &expected, 300, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
    if (!success) return 7;
    if (x != 300) return 8;

    return 0;
}
"#;
    assert_eq!(compile_and_run("atomic_compare_exchange", code, &[]), 0);
}

// ============================================================================
// __c11_atomic_thread_fence: Memory barrier (verifies compilation)
// ============================================================================

#[test]
fn atomic_thread_fence() {
    let code = r#"
int main(void) {
    int x = 0;

    // Test various memory orderings compile correctly
    __c11_atomic_thread_fence(__ATOMIC_RELAXED);
    x = 1;
    __c11_atomic_thread_fence(__ATOMIC_ACQUIRE);
    x = 2;
    __c11_atomic_thread_fence(__ATOMIC_RELEASE);
    x = 3;
    __c11_atomic_thread_fence(__ATOMIC_ACQ_REL);
    x = 4;
    __c11_atomic_thread_fence(__ATOMIC_SEQ_CST);

    // Simple sanity check
    if (x != 4) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("atomic_thread_fence", code, &[]), 0);
}

// ============================================================================
// _Atomic type qualifier: Basic usage
// ============================================================================

#[test]
fn atomic_type_qualifier() {
    let code = r#"
int main(void) {
    // Test 1: _Atomic int declaration and assignment
    _Atomic int x = 42;
    if (x != 42) return 1;

    // Test 2: Assignment to atomic
    x = 100;
    if (x != 100) return 2;

    // Test 3: Read from atomic
    int val = x;
    if (val != 100) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("atomic_type_qualifier", code, &[]), 0);
}

// ============================================================================
// Different sizes: 8, 16, 32, 64-bit atomics
// ============================================================================

#[test]
fn atomic_different_sizes() {
    let code = r#"
int main(void) {
    // Test 1: 8-bit (char)
    char c = 10;
    char old_c = __c11_atomic_fetch_add(&c, 5, __ATOMIC_SEQ_CST);
    if (old_c != 10) return 1;
    if (c != 15) return 2;

    // Test 2: 16-bit (short)
    short s = 1000;
    short old_s = __c11_atomic_fetch_add(&s, 234, __ATOMIC_SEQ_CST);
    if (old_s != 1000) return 3;
    if (s != 1234) return 4;

    // Test 3: 32-bit (int)
    int i = 100000;
    int old_i = __c11_atomic_fetch_add(&i, 23456, __ATOMIC_SEQ_CST);
    if (old_i != 100000) return 5;
    if (i != 123456) return 6;

    // Test 4: 64-bit (long long)
    long long ll = 10000000000LL;
    long long old_ll = __c11_atomic_fetch_add(&ll, 2345678901LL, __ATOMIC_SEQ_CST);
    if (old_ll != 10000000000LL) return 7;
    if (ll != 12345678901LL) return 8;

    return 0;
}
"#;
    assert_eq!(compile_and_run("atomic_different_sizes", code, &[]), 0);
}

// ============================================================================
// __has_builtin: Verify atomic builtins are recognized
// ============================================================================

#[test]
fn atomic_has_builtin() {
    let code = r#"
int main(void) {
    // Test that __has_builtin returns 1 for C11 atomic builtins
#if !__has_builtin(__c11_atomic_init)
    return 1;
#endif
#if !__has_builtin(__c11_atomic_load)
    return 2;
#endif
#if !__has_builtin(__c11_atomic_store)
    return 3;
#endif
#if !__has_builtin(__c11_atomic_exchange)
    return 4;
#endif
#if !__has_builtin(__c11_atomic_compare_exchange_strong)
    return 5;
#endif
#if !__has_builtin(__c11_atomic_compare_exchange_weak)
    return 6;
#endif
#if !__has_builtin(__c11_atomic_fetch_add)
    return 7;
#endif
#if !__has_builtin(__c11_atomic_fetch_sub)
    return 8;
#endif
#if !__has_builtin(__c11_atomic_fetch_and)
    return 9;
#endif
#if !__has_builtin(__c11_atomic_fetch_or)
    return 10;
#endif
#if !__has_builtin(__c11_atomic_fetch_xor)
    return 11;
#endif
#if !__has_builtin(__c11_atomic_thread_fence)
    return 12;
#endif
#if !__has_builtin(__c11_atomic_signal_fence)
    return 13;
#endif

    // Verify a non-existent builtin returns 0
#if __has_builtin(__nonexistent_builtin)
    return 14;
#endif

    return 0;
}
"#;
    assert_eq!(compile_and_run("atomic_has_builtin", code, &[]), 0);
}

// ============================================================================
// C11 <stdatomic.h> tests - Standard interface
// ============================================================================

#[test]
fn stdatomic_load_store() {
    let code = r#"
#include <stdatomic.h>

int main(void) {
    atomic_int x;
    atomic_init(&x, 1);

    int a = atomic_load(&x);
    if (a != 1) return 1;

    atomic_store(&x, 2);
    if (atomic_load(&x) != 2) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("stdatomic_load_store", code, &[]), 0);
}

#[test]
fn stdatomic_fetch_add() {
    let code = r#"
#include <stdatomic.h>

int main(void) {
    atomic_int x;
    atomic_init(&x, 10);

    int old = atomic_fetch_add(&x, 5);
    if (old != 10) return 1;
    if (atomic_load(&x) != 15) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("stdatomic_fetch_add", code, &[]), 0);
}

#[test]
fn stdatomic_fetch_sub() {
    let code = r#"
#include <stdatomic.h>

int main(void) {
    atomic_int x;
    atomic_init(&x, 20);

    int old = atomic_fetch_sub(&x, 5);
    if (old != 20) return 1;
    if (atomic_load(&x) != 15) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("stdatomic_fetch_sub", code, &[]), 0);
}

#[test]
fn stdatomic_exchange() {
    let code = r#"
#include <stdatomic.h>

int main(void) {
    atomic_int x;
    atomic_init(&x, 100);

    int prev = atomic_exchange(&x, 200);
    if (prev != 100) return 1;
    if (atomic_load(&x) != 200) return 2;

    return 0;
}
"#;
    assert_eq!(compile_and_run("stdatomic_exchange", code, &[]), 0);
}

#[test]
fn stdatomic_compare_exchange_strong() {
    let code = r#"
#include <stdatomic.h>

int main(void) {
    atomic_int x;
    atomic_init(&x, 100);

    // Test 1: CAS success
    int expected = 100;
    int success = atomic_compare_exchange_strong(&x, &expected, 200);
    if (!success) return 1;
    if (atomic_load(&x) != 200) return 2;

    // Test 2: CAS failure
    expected = 100;  // wrong expected
    success = atomic_compare_exchange_strong(&x, &expected, 300);
    if (success) return 3;
    if (atomic_load(&x) != 200) return 4;
    if (expected != 200) return 5;  // expected updated to actual

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("stdatomic_compare_exchange_strong", code, &[]),
        0
    );
}

#[test]
fn stdatomic_compare_exchange_weak() {
    let code = r#"
#include <stdatomic.h>

int main(void) {
    atomic_int x;
    atomic_init(&x, 50);

    // Test weak CAS (may fail spuriously, but we test single-threaded)
    int expected = 50;
    int success = atomic_compare_exchange_weak(&x, &expected, 100);
    if (!success) return 1;
    if (atomic_load(&x) != 100) return 2;

    return 0;
}
"#;
    assert_eq!(
        compile_and_run("stdatomic_compare_exchange_weak", code, &[]),
        0
    );
}

#[test]
fn stdatomic_thread_fence() {
    let code = r#"
#include <stdatomic.h>

int main(void) {
    atomic_int x;
    atomic_init(&x, 0);

    atomic_thread_fence(memory_order_seq_cst);
    atomic_store(&x, 1);
    atomic_thread_fence(memory_order_seq_cst);

    if (atomic_load(&x) != 1) return 1;

    return 0;
}
"#;
    assert_eq!(compile_and_run("stdatomic_thread_fence", code, &[]), 0);
}

#[test]
fn stdatomic_flag() {
    let code = r#"
#include <stdatomic.h>

int main(void) {
    atomic_flag flag = ATOMIC_FLAG_INIT;

    // Test and set (should return 0/false initially)
    int was_set = atomic_flag_test_and_set(&flag);
    if (was_set) return 1;

    // Test and set again (should return 1/true now)
    was_set = atomic_flag_test_and_set(&flag);
    if (!was_set) return 2;

    // Clear the flag
    atomic_flag_clear(&flag);

    // Test and set after clear (should return 0/false)
    was_set = atomic_flag_test_and_set(&flag);
    if (was_set) return 3;

    return 0;
}
"#;
    assert_eq!(compile_and_run("stdatomic_flag", code, &[]), 0);
}

#[test]
fn stdatomic_explicit() {
    let code = r#"
#include <stdatomic.h>

int main(void) {
    atomic_int x;
    atomic_init(&x, 0);

    // Test explicit variants with memory ordering
    atomic_store_explicit(&x, 42, memory_order_relaxed);
    int val = atomic_load_explicit(&x, memory_order_relaxed);
    if (val != 42) return 1;

    atomic_store_explicit(&x, 100, memory_order_release);
    val = atomic_load_explicit(&x, memory_order_acquire);
    if (val != 100) return 2;

    int old = atomic_fetch_add_explicit(&x, 5, memory_order_seq_cst);
    if (old != 100) return 3;
    if (atomic_load(&x) != 105) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("stdatomic_explicit", code, &[]), 0);
}

#[test]
fn stdatomic_types() {
    let code = r#"
#include <stdatomic.h>

int main(void) {
    // Test various atomic type aliases
    atomic_char c;
    atomic_init(&c, 'A');
    if (atomic_load(&c) != 'A') return 1;

    atomic_short s;
    atomic_init(&s, 1234);
    if (atomic_load(&s) != 1234) return 2;

    atomic_int i;
    atomic_init(&i, 123456);
    if (atomic_load(&i) != 123456) return 3;

    atomic_long l;
    atomic_init(&l, 1234567890L);
    if (atomic_load(&l) != 1234567890L) return 4;

    return 0;
}
"#;
    assert_eq!(compile_and_run("stdatomic_types", code, &[]), 0);
}
