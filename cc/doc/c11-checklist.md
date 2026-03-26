# C11 Delta Checklist (C99 -> C11)

> **Standard Reference:** ISO/IEC 9899:2011 (C11)
> **Baseline:** C99 (see c99-checklist.md for full C99 coverage)
> **Scope:** Only new, changed, and removed items relative to C99

---

## Table of Contents

1. [New Keywords](#1-new-keywords-7)
2. [New Types](#2-new-types)
3. [New Type Qualifiers & Specifiers](#3-new-type-qualifiers--specifiers)
4. [New Operators & Expressions](#4-new-operators--expressions)
5. [New/Changed Declarations](#5-newchanged-declarations)
6. [New Lexical Elements](#6-new-lexical-elements)
7. [Type-Generic Selection (`_Generic`)](#7-type-generic-selection-_generic)
8. [Static Assertions (`_Static_assert`)](#8-static-assertions-_static_assert)
9. [Anonymous Structs and Unions](#9-anonymous-structs-and-unions)
10. [Alignment (`_Alignas` / `_Alignof`)](#10-alignment-_alignas--_alignof)
11. [`_Noreturn` Function Specifier](#11-_noreturn-function-specifier)
12. [Atomic Types and Operations](#12-atomic-types-and-operations)
13. [Thread-Local Storage (`_Thread_local`)](#13-thread-local-storage-_thread_local)
14. [Threading (`<threads.h>`)](#14-threading-threadsh)
15. [Unicode Support](#15-unicode-support)
16. [Memory Model](#16-memory-model)
17. [New Library Functions](#17-new-library-functions)
18. [New Standard Headers](#18-new-standard-headers)
19. [New/Changed Predefined Macros](#19-newchanged-predefined-macros)
20. [New Floating-Point Limit Macros](#20-new-floating-point-limit-macros)
21. [Removed Features](#21-removed-features)
22. [Changed Semantics](#22-changed-semantics)
23. [Optional Features Decision Matrix](#23-optional-features-decision-matrix)

---

## 1. New Keywords (+7)

- [x] `_Alignas` — alignment specifier
- [x] `_Alignof` — alignment query operator
- [x] `_Atomic` — atomic type qualifier/specifier
- [ ] `_Generic` — type-generic selection expression
- [x] `_Noreturn` — function specifier (never returns)
- [x] `_Static_assert` — compile-time assertion
- [x] `_Thread_local` — thread storage class specifier

---

## 2. New Types

### 2.1 Unicode Character Types
- [ ] `char16_t` — 16-bit character type for UTF-16 (from `<uchar.h>`)
- [ ] `char32_t` — 32-bit character type for UTF-32 (from `<uchar.h>`)

### 2.2 Atomic Types
- [x] `_Atomic(T)` — atomic version of type T
- [x] `_Atomic` as type qualifier on declarations
- [x] `atomic_bool` typedef
- [x] `atomic_char`, `atomic_schar`, `atomic_uchar`
- [x] `atomic_short`, `atomic_ushort`
- [x] `atomic_int`, `atomic_uint`
- [x] `atomic_long`, `atomic_ulong`
- [x] `atomic_llong`, `atomic_ullong`
- [ ] `atomic_intptr_t`, `atomic_uintptr_t`
- [ ] `atomic_size_t`, `atomic_ptrdiff_t`
- [ ] `atomic_intmax_t`, `atomic_uintmax_t`
- [ ] `atomic_char16_t`, `atomic_char32_t`, `atomic_wchar_t`
- [ ] `atomic_int_leastN_t`, `atomic_uint_leastN_t` (8/16/32/64)
- [ ] `atomic_int_fastN_t`, `atomic_uint_fastN_t` (8/16/32/64)

---

## 3. New Type Qualifiers & Specifiers

### 3.1 `_Atomic` Type Qualifier
- [x] `_Atomic` as qualifier on variable declarations
- [ ] `_Atomic` cannot qualify array types
- [ ] `_Atomic` cannot qualify function types
- [ ] `_Atomic` cannot qualify struct/union with VLA member

### 3.2 `_Thread_local` Storage Class
- [x] `_Thread_local` storage class specifier
- [x] `_Thread_local` with `static`
- [x] `_Thread_local` with `extern`
- [x] Only one storage class except `_Thread_local` + `static`/`extern`

### 3.3 `_Noreturn` Function Specifier
- [x] `_Noreturn` on function declarations
- [x] `_Noreturn` on function definitions
- [x] Undefined behavior if `_Noreturn` function returns

### 3.4 `_Alignas` Alignment Specifier
- [x] `_Alignas(type-name)` — align to type's requirement
- [x] `_Alignas(constant-expression)` — align to explicit value
- [x] `_Alignas` on variable declarations
- [x] `_Alignas` on struct/union members
- [x] Multiple `_Alignas` specifiers (strictest wins)
- [x] Cannot weaken natural alignment
- [x] Cannot apply to function parameters
- [x] Cannot apply to typedef
- [x] Cannot apply to bit-field

---

## 4. New Operators & Expressions

### 4.1 `_Alignof` Operator
- [x] `_Alignof(type-name)` — yields `size_t`
- [x] `_Alignof` on basic types
- [x] `_Alignof` on struct/union types
- [x] `_Alignof` on array types (returns element alignment)

### 4.2 `_Generic` Selection Expression
- [ ] `_Generic(controlling-expr, type: expr, ..., default: expr)` syntax
- [ ] Controlling expression type undergoes lvalue conversion
- [ ] Controlling expression is not evaluated
- [ ] Unselected association expressions not evaluated
- [ ] No two associations with compatible types (constraint)
- [ ] At most one `default` association (constraint)
- [ ] Result type/value is selected association expression
- [ ] Nested `_Generic` expressions

---

## 5. New/Changed Declarations

### 5.1 Static Assert Declaration
- [x] `_Static_assert(constant-expression, string-literal)` syntax
- [x] At file scope
- [x] At block scope
- [x] Inside struct/union declaration
- [x] Can use `sizeof`, `_Alignof` in constant expression
- [x] Compile-time diagnostic message on failure

### 5.2 Anonymous Struct/Union Members
- [x] Unnamed struct member within struct
- [x] Unnamed union member within struct
- [ ] Unnamed struct member within union
- [ ] Unnamed union member within union
- [x] Direct field access through containing type
- [x] Nested anonymous structs/unions
- [x] Initialization of anonymous members
- [x] `sizeof` on struct with anonymous members

### 5.3 `_Noreturn` Function Definitions
- [x] `_Noreturn` on function definitions

### 5.4 Grammar Additions
- [x] static_assert-declaration as declaration
- [ ] generic-selection as primary-expression

---

## 6. New Lexical Elements

### 6.1 Character Constants
- [ ] UTF-16 character constant `u'x'` (type `char16_t`)
- [ ] UTF-32 character constant `U'x'` (type `char32_t`)

### 6.2 String Literals
- [ ] UTF-8 string literal `u8"..."` (type `char[]`)
- [ ] UTF-16 string literal `u"..."` (type `char16_t[]`)
- [ ] UTF-32 string literal `U"..."` (type `char32_t[]`)
- [ ] Adjacent Unicode string literal concatenation rules

---

## 7. Type-Generic Selection (`_Generic`)

- [ ] `_Generic` keyword parsing
- [ ] Association list with type-name: expression pairs
- [ ] `default:` association
- [ ] Type matching after lvalue conversion (strips qualifiers, array->pointer, function->pointer)
- [ ] Controlling expression not evaluated
- [ ] Unselected associations not evaluated
- [ ] Constraint: no two compatible types in association list
- [ ] Constraint: at most one `default`
- [ ] Use in type-generic macros (`<tgmath.h>` rewrite)
- [ ] Nested `_Generic` expressions

---

## 8. Static Assertions (`_Static_assert`)

- [x] `_Static_assert(constant-expression, string-literal)` syntax
- [x] File scope static assertion
- [x] Block scope static assertion
- [x] Inside struct/union declaration
- [x] Can use `sizeof`, `_Alignof` in expression
- [x] Compile-time diagnostic message on failure
- [x] `static_assert` macro from `<assert.h>` expands to `_Static_assert`

---

## 9. Anonymous Structs and Unions

- [x] Unnamed struct member in enclosing struct
- [x] Unnamed union member in enclosing struct
- [ ] Unnamed struct member in enclosing union
- [ ] Unnamed union member in enclosing union
- [x] Direct field access through containing type
- [x] Nested anonymous structs/unions
- [x] Initialization of anonymous members
- [x] `sizeof` struct/union with anonymous members

---

## 10. Alignment (`_Alignas` / `_Alignof`)

### 10.1 `_Alignas` Specifier
- [x] `_Alignas(type-name)` specifier
- [x] `_Alignas(constant-expression)` specifier
- [x] On variable declarations
- [x] On struct/union members
- [x] Multiple `_Alignas` — strictest (largest) wins
- [x] Cannot weaken natural alignment
- [x] Cannot apply to function parameters
- [x] Cannot apply to typedef
- [x] Cannot apply to bit-field

### 10.2 `_Alignof` Operator
- [x] `_Alignof(type-name)` yields `size_t`
- [x] On basic types
- [x] On struct/union types
- [x] On array types (element alignment)

### 10.3 Convenience Macros (`<stdalign.h>`)
- [x] `alignas` macro expands to `_Alignas`
- [x] `alignof` macro expands to `_Alignof`
- [x] `__alignas_is_defined` expands to `1`
- [x] `__alignof_is_defined` expands to `1`

---

## 11. `_Noreturn` Function Specifier

- [x] `_Noreturn` on function declarations
- [x] `_Noreturn` on function definitions
- [x] Applies to: `exit()`, `abort()`, `_Exit()`, `quick_exit()`, `thrd_exit()`
- [x] Undefined behavior if `_Noreturn` function returns
- [x] `noreturn` macro from `<stdnoreturn.h>` expands to `_Noreturn`

---

## 12. Atomic Types and Operations

### 12.1 `_Atomic` Type Qualifier/Specifier
- [x] `_Atomic` type qualifier
- [x] `_Atomic(type-name)` type specifier
- [x] `_Atomic` on integer types
- [x] `_Atomic` on pointer types
- [ ] Cannot qualify array, function, struct-with-VLA
- [ ] Atomic compound assignment operators (`+=`, `-=`, etc.)
- [ ] Atomic pre/post increment/decrement
- [x] Implicit sequentially-consistent ordering for operators
- [x] `atomic_init(obj, value)` — non-atomic initialization
- [x] `ATOMIC_VAR_INIT(value)` — static initialization macro

### 12.2 Atomic Operations (`<stdatomic.h>`)
- [x] `atomic_store(obj, desired)` / `atomic_store_explicit(obj, desired, order)`
- [x] `atomic_load(obj)` / `atomic_load_explicit(obj, order)`
- [x] `atomic_exchange(obj, desired)` / `atomic_exchange_explicit(obj, desired, order)`
- [x] `atomic_compare_exchange_strong(obj, expected, desired)`
- [x] `atomic_compare_exchange_strong_explicit(obj, expected, desired, succ, fail)`
- [x] `atomic_compare_exchange_weak(obj, expected, desired)`
- [x] `atomic_compare_exchange_weak_explicit(obj, expected, desired, succ, fail)`
- [x] `atomic_fetch_add` / `atomic_fetch_add_explicit`
- [x] `atomic_fetch_sub` / `atomic_fetch_sub_explicit`
- [x] `atomic_fetch_or` / `atomic_fetch_or_explicit`
- [x] `atomic_fetch_xor` / `atomic_fetch_xor_explicit`
- [x] `atomic_fetch_and` / `atomic_fetch_and_explicit`

### 12.3 Memory Ordering
- [x] `memory_order_relaxed` — no synchronization
- [x] `memory_order_consume` — data-dependency ordering
- [x] `memory_order_acquire` — acquire fence
- [x] `memory_order_release` — release fence
- [x] `memory_order_acq_rel` — acquire + release
- [x] `memory_order_seq_cst` — sequentially consistent (default)
- [x] `atomic_thread_fence(order)` — thread fence
- [x] `atomic_signal_fence(order)` — signal fence
- [x] `kill_dependency(y)` — break dependency chain

### 12.4 Atomic Flag
- [x] `atomic_flag` type (guaranteed lock-free)
- [x] `ATOMIC_FLAG_INIT` initializer
- [x] `atomic_flag_test_and_set` / `atomic_flag_test_and_set_explicit`
- [x] `atomic_flag_clear` / `atomic_flag_clear_explicit`

### 12.5 Lock-Free Property Macros
- [x] `ATOMIC_BOOL_LOCK_FREE` (0, 1, or 2)
- [x] `ATOMIC_CHAR_LOCK_FREE`
- [x] `ATOMIC_CHAR16_T_LOCK_FREE`
- [x] `ATOMIC_CHAR32_T_LOCK_FREE`
- [x] `ATOMIC_WCHAR_T_LOCK_FREE`
- [x] `ATOMIC_SHORT_LOCK_FREE`
- [x] `ATOMIC_INT_LOCK_FREE`
- [x] `ATOMIC_LONG_LOCK_FREE`
- [x] `ATOMIC_LLONG_LOCK_FREE`
- [x] `ATOMIC_POINTER_LOCK_FREE`
- [ ] `atomic_is_lock_free(obj)` — runtime query

---

## 13. Thread-Local Storage (`_Thread_local`)

- [x] `_Thread_local` storage class specifier
- [x] `_Thread_local` with `static`
- [x] `_Thread_local` with `extern`
- [x] Each thread gets own instance
- [x] Initialized once per thread creation
- [x] File scope `_Thread_local` variables
- [x] Block scope `_Thread_local static` variables
- [ ] `thread_local` macro from `<threads.h>`

---

## 14. Threading (`<threads.h>`)

### 14.1 Thread Management
- [ ] `thrd_t` type
- [ ] `thrd_create(thr, func, arg)` — create thread
- [ ] `thrd_join(thr, res)` — join thread
- [ ] `thrd_detach(thr)` — detach thread
- [ ] `thrd_exit(res)` — exit current thread
- [ ] `thrd_current()` — current thread ID
- [ ] `thrd_equal(thr1, thr2)` — compare thread IDs
- [ ] `thrd_sleep(duration, remaining)` — sleep
- [ ] `thrd_yield()` — yield execution
- [ ] Return codes: `thrd_success`, `thrd_nomem`, `thrd_timedout`, `thrd_busy`, `thrd_error`

### 14.2 Mutexes
- [ ] `mtx_t` type
- [ ] `mtx_init(mtx, type)` — create mutex
- [ ] `mtx_lock(mtx)` — lock
- [ ] `mtx_trylock(mtx)` — try lock
- [ ] `mtx_timedlock(mtx, ts)` — timed lock
- [ ] `mtx_unlock(mtx)` — unlock
- [ ] `mtx_destroy(mtx)` — destroy
- [ ] Mutex types: `mtx_plain`, `mtx_recursive`, `mtx_timed`

### 14.3 Condition Variables
- [ ] `cnd_t` type
- [ ] `cnd_init(cond)` — create
- [ ] `cnd_signal(cond)` — signal one
- [ ] `cnd_broadcast(cond)` — signal all
- [ ] `cnd_wait(cond, mtx)` — wait
- [ ] `cnd_timedwait(cond, mtx, ts)` — timed wait
- [ ] `cnd_destroy(cond)` — destroy

### 14.4 Thread-Specific Storage
- [ ] `tss_t` type
- [ ] `tss_dtor_t` destructor type
- [ ] `tss_create(key, dtor)` — create key
- [ ] `tss_get(key)` — get value
- [ ] `tss_set(key, val)` — set value
- [ ] `tss_delete(key)` — delete key
- [ ] `TSS_DTOR_ITERATIONS` — max destructor iterations

### 14.5 Call Once
- [ ] `once_flag` type
- [ ] `ONCE_FLAG_INIT` initializer
- [ ] `call_once(flag, func)` — execute once

---

## 15. Unicode Support

### 15.1 Types
- [ ] `char16_t` type (from `<uchar.h>`)
- [ ] `char32_t` type (from `<uchar.h>`)

### 15.2 Character Constants
- [ ] `u'x'` character constant (type `char16_t`)
- [ ] `U'x'` character constant (type `char32_t`)

### 15.3 String Literals
- [ ] `u8"..."` string literal (UTF-8, type `char[]`)
- [ ] `u"..."` string literal (UTF-16, type `char16_t[]`)
- [ ] `U"..."` string literal (UTF-32, type `char32_t[]`)
- [ ] Adjacent Unicode string literal concatenation

### 15.4 Conversion Functions (`<uchar.h>`)
- [ ] `mbrtoc16()` — multibyte to char16_t
- [ ] `c16rtomb()` — char16_t to multibyte
- [ ] `mbrtoc32()` — multibyte to char32_t
- [ ] `c32rtomb()` — char32_t to multibyte

---

## 16. Memory Model

- [ ] "Sequenced before" replaces sequence points terminology
- [ ] "Synchronizes with" relation for atomic operations
- [ ] "Happens before" transitive ordering
- [ ] Data race on non-atomic shared variable = undefined behavior
- [ ] Sequential consistency guarantee for data-race-free programs

---

## 17. New Library Functions

### 17.1 `<stdlib.h>` Additions
- [ ] `aligned_alloc(alignment, size)` — aligned memory allocation
- [ ] `quick_exit(status)` — rapid program termination
- [ ] `at_quick_exit(func)` — register quick_exit handler (min 32 registrations)

### 17.2 `<time.h>` Additions
- [ ] `struct timespec` — seconds + nanoseconds
- [ ] `timespec_get(ts, base)` — get current time
- [ ] `TIME_UTC` — time base constant

### 17.3 `<stdio.h>` Additions
- [ ] `fopen()` exclusive create mode `"wx"` — fail if file exists
- [ ] `fopen()` exclusive create mode `"wbx"` — binary, fail if exists
- [ ] Atomic check-and-create semantics (like `O_CREAT|O_EXCL`)

---

## 18. New Standard Headers

- [x] `<stdalign.h>` — `alignas`, `alignof`, `__alignas_is_defined`, `__alignof_is_defined`
- [x] `<stdatomic.h>` — atomic types and operations *(optional)*
- [x] `<stdnoreturn.h>` — `noreturn` macro expands to `_Noreturn`
- [ ] `<threads.h>` — threading support *(optional)*
- [ ] `<uchar.h>` — `char16_t`, `char32_t`, conversion functions

### 18.1 `<stdalign.h>` Contents
- [x] `alignas` macro expands to `_Alignas`
- [x] `alignof` macro expands to `_Alignof`
- [x] `__alignas_is_defined` expands to `1`
- [x] `__alignof_is_defined` expands to `1`

### 18.2 `<stdnoreturn.h>` Contents
- [x] `noreturn` macro expands to `_Noreturn`

### 18.3 `<assert.h>` C11 Addition
- [x] `static_assert` macro expands to `_Static_assert`

---

## 19. New/Changed Predefined Macros

### 19.1 Changed
- [x] `__STDC_VERSION__` — `201112L` (was `199901L` in C99)

### 19.2 New Conditionally-Defined Macros
- [ ] `__STDC_UTF_16__` — char16_t is UTF-16 *(not defined — no Unicode support yet)*
- [ ] `__STDC_UTF_32__` — char32_t is UTF-32 *(not defined — no Unicode support yet)*
- N/A `__STDC_ANALYZABLE__` — Annex L supported *(will not implement)*
- N/A `__STDC_LIB_EXT1__` — Annex K supported *(will not implement)*
- [x] `__STDC_NO_ATOMICS__` — atomics not supported *(correctly NOT defined — atomics supported)*
- [x] `__STDC_NO_COMPLEX__` — complex not supported *(correctly NOT defined — complex supported)*
- [x] `__STDC_NO_THREADS__` — threads not supported *(correctly NOT defined — hosted impl, system libc provides threads)*
- [x] `__STDC_NO_VLA__` — VLAs not supported *(correctly NOT defined — VLAs supported)*

---

## 20. New Floating-Point Limit Macros (`<float.h>`)

- [x] `FLT_DECIMAL_DIG` — float round-trip decimal digits
- [x] `DBL_DECIMAL_DIG` — double round-trip decimal digits
- [x] `LDBL_DECIMAL_DIG` — long double round-trip decimal digits
- [x] `FLT_HAS_SUBNORM` — float subnormal support (−1, 0, or 1)
- [x] `DBL_HAS_SUBNORM` — double subnormal support
- [x] `LDBL_HAS_SUBNORM` — long double subnormal support
- [x] `FLT_TRUE_MIN` — smallest positive float subnormal
- [x] `DBL_TRUE_MIN` — smallest positive double subnormal
- [x] `LDBL_TRUE_MIN` — smallest positive long double subnormal

---

## 21. Removed Features

- [ ] `gets()` removed from `<stdio.h>` (use `fgets()`)

---

## 22. Changed Semantics

### 22.1 Features Made Optional
- [ ] VLAs now optional (test with `__STDC_NO_VLA__`; was mandatory in C99)
- [ ] Complex types now optional (test with `__STDC_NO_COMPLEX__`; was mandatory in C99)

### 22.2 Terminology Changes
- [ ] "Sequence points" replaced by "sequenced before" relation
- [ ] Formal memory model for multi-threaded execution

### 22.3 New Undefined Behaviors
- [ ] Data race on non-atomic shared variable
- [ ] `_Noreturn` function that returns

### 22.4 New Implementation-Defined Behaviors
- [ ] Alignment requirements for each type (exposed by `_Alignof`)
- [ ] Lock-free property of atomic types

---

## 23. Optional Features Decision Matrix

| Feature | Macro | Our Decision | Notes |
|---------|-------|-------------|-------|
| VLAs | `__STDC_NO_VLA__` | Supported | Already implemented in C99 |
| Complex types | `__STDC_NO_COMPLEX__` | Supported | Already implemented in C99 |
| Atomics | `__STDC_NO_ATOMICS__` | TBD | |
| Threads | `__STDC_NO_THREADS__` | TBD | |
| Bounds-checking (Annex K) | `__STDC_LIB_EXT1__` | Will not implement | Rarely supported; controversial |
| Analyzability (Annex L) | `__STDC_ANALYZABLE__` | Will not implement | |

---

## Progress Tracking

| Section | Items | Done | % |
|---------|-------|------|---|
| 1. New Keywords | 7 | 6 | 86% |
| 2. New Types | 16 | 10 | 63% |
| 3. New Qualifiers/Specifiers | 16 | 16 | 100% |
| 4. New Operators/Expressions | 12 | 4 | 33% |
| 5. New Declarations | 15 | 12 | 80% |
| 6. New Lexical Elements | 6 | 0 | 0% |
| 7. `_Generic` | 10 | 0 | 0% |
| 8. `_Static_assert` | 7 | 7 | 100% |
| 9. Anonymous Structs/Unions | 8 | 6 | 75% |
| 10. Alignment | 17 | 17 | 100% |
| 11. `_Noreturn` | 5 | 5 | 100% |
| 12. Atomics | 46 | 40 | 87% |
| 13. `_Thread_local` | 8 | 7 | 88% |
| 14. Threading | 33 | 0 | 0% |
| 15. Unicode | 12 | 0 | 0% |
| 16. Memory Model | 5 | 0 | 0% |
| 17. New Library Functions | 9 | 0 | 0% |
| 18. New Headers | 10 | 9 | 90% |
| 19. New/Changed Macros | 9 | 5 | 56% |
| 20. Float Limit Macros | 9 | 9 | 100% |
| 21. Removed Features | 1 | 0 | 0% |
| 22. Changed Semantics | 8 | 0 | 0% |
| **TOTAL** | **259** | **154** | **59%** |

---

*C11 delta checklist. Reference: ISO/IEC 9899:2011. For C99 baseline, see c99-checklist.md.*
