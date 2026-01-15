/*
 * Copyright (c) 2025-2026 Jeff Garzik
 *
 * This file is part of the posixutils-rs project covered under
 * the MIT License. For the full license text, please see the LICENSE
 * file in the root directory of this project.
 * SPDX-License-Identifier: MIT
 *
 * C11 <stdatomic.h> - Atomic operations
 */

#ifndef _STDATOMIC_H
#define _STDATOMIC_H

/* Memory order enum (C11 7.17.3) */
typedef enum memory_order {
    memory_order_relaxed = __ATOMIC_RELAXED,
    memory_order_consume = __ATOMIC_CONSUME,
    memory_order_acquire = __ATOMIC_ACQUIRE,
    memory_order_release = __ATOMIC_RELEASE,
    memory_order_acq_rel = __ATOMIC_ACQ_REL,
    memory_order_seq_cst = __ATOMIC_SEQ_CST
} memory_order;

/* Atomic type aliases (C11 7.17.6) */
typedef _Atomic(_Bool)              atomic_bool;
typedef _Atomic(char)               atomic_char;
typedef _Atomic(signed char)        atomic_schar;
typedef _Atomic(unsigned char)      atomic_uchar;
typedef _Atomic(short)              atomic_short;
typedef _Atomic(unsigned short)     atomic_ushort;
typedef _Atomic(int)                atomic_int;
typedef _Atomic(unsigned int)       atomic_uint;
typedef _Atomic(long)               atomic_long;
typedef _Atomic(unsigned long)      atomic_ulong;
typedef _Atomic(long long)          atomic_llong;
typedef _Atomic(unsigned long long) atomic_ullong;

/* Initialization (C11 7.17.2) */
#define ATOMIC_VAR_INIT(value) (value)
#define atomic_init(obj, val) __c11_atomic_init(obj, val)

/* Operations (C11 7.17.7) */
#define atomic_store(obj, val) \
    __c11_atomic_store(obj, val, __ATOMIC_SEQ_CST)
#define atomic_store_explicit __c11_atomic_store

#define atomic_load(obj) \
    __c11_atomic_load(obj, __ATOMIC_SEQ_CST)
#define atomic_load_explicit __c11_atomic_load

#define atomic_exchange(obj, val) \
    __c11_atomic_exchange(obj, val, __ATOMIC_SEQ_CST)
#define atomic_exchange_explicit __c11_atomic_exchange

#define atomic_compare_exchange_strong(obj, exp, des) \
    __c11_atomic_compare_exchange_strong(obj, exp, des, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)
#define atomic_compare_exchange_strong_explicit __c11_atomic_compare_exchange_strong

#define atomic_compare_exchange_weak(obj, exp, des) \
    __c11_atomic_compare_exchange_weak(obj, exp, des, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)
#define atomic_compare_exchange_weak_explicit __c11_atomic_compare_exchange_weak

#define atomic_fetch_add(obj, val) \
    __c11_atomic_fetch_add(obj, val, __ATOMIC_SEQ_CST)
#define atomic_fetch_add_explicit __c11_atomic_fetch_add

#define atomic_fetch_sub(obj, val) \
    __c11_atomic_fetch_sub(obj, val, __ATOMIC_SEQ_CST)
#define atomic_fetch_sub_explicit __c11_atomic_fetch_sub

#define atomic_fetch_or(obj, val) \
    __c11_atomic_fetch_or(obj, val, __ATOMIC_SEQ_CST)
#define atomic_fetch_or_explicit __c11_atomic_fetch_or

#define atomic_fetch_xor(obj, val) \
    __c11_atomic_fetch_xor(obj, val, __ATOMIC_SEQ_CST)
#define atomic_fetch_xor_explicit __c11_atomic_fetch_xor

#define atomic_fetch_and(obj, val) \
    __c11_atomic_fetch_and(obj, val, __ATOMIC_SEQ_CST)
#define atomic_fetch_and_explicit __c11_atomic_fetch_and

/* Fences (C11 7.17.4) */
#define atomic_thread_fence(order) __c11_atomic_thread_fence(order)
#define atomic_signal_fence(order) __c11_atomic_signal_fence(order)

/* Flag type (C11 7.17.8) */
typedef struct atomic_flag { atomic_bool _Value; } atomic_flag;

#define ATOMIC_FLAG_INIT { 0 }

#define atomic_flag_test_and_set(obj) \
    __c11_atomic_exchange(&(obj)->_Value, 1, __ATOMIC_SEQ_CST)
#define atomic_flag_test_and_set_explicit(obj, order) \
    __c11_atomic_exchange(&(obj)->_Value, 1, order)

#define atomic_flag_clear(obj) \
    __c11_atomic_store(&(obj)->_Value, 0, __ATOMIC_SEQ_CST)
#define atomic_flag_clear_explicit(obj, order) \
    __c11_atomic_store(&(obj)->_Value, 0, order)

/* kill_dependency (C11 7.17.3.2) */
#define kill_dependency(y) (y)

#endif /* _STDATOMIC_H */
