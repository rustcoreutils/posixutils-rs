# Compiler Builtins

Builtin functions supported by c17. GCC/Clang compatible.

Every entry below was compile-probed against this compiler; where behaviour
differs from gcc, the row says so rather than leaving the reader to find out.

## Variadic Functions

| Builtin | Description |
|---------|-------------|
| `__builtin_va_list` | Platform-specific va_list type |
| `__builtin_va_start(ap, last)` | Initialize va_list to first variadic arg |
| `__builtin_va_arg(ap, type)` | Get next arg of `type`, advance va_list |
| `__builtin_va_end(ap)` | Clean up va_list |
| `__builtin_va_copy(dest, src)` | Copy va_list |

## Byte Swapping

| Builtin | Description |
|---------|-------------|
| `__builtin_bswap16(x)` | Reverse bytes of 16-bit value |
| `__builtin_bswap32(x)` | Reverse bytes of 32-bit value |
| `__builtin_bswap64(x)` | Reverse bytes of 64-bit value |

## Bit Operations

| Builtin | Description |
|---------|-------------|
| `__builtin_ctz(x)` | Count trailing zeros in `unsigned int` (undefined if x==0) |
| `__builtin_ctzl(x)` | Count trailing zeros in `unsigned long` |
| `__builtin_ctzll(x)` | Count trailing zeros in `unsigned long long` |
| `__builtin_clz(x)` | Count leading zeros in `unsigned int` (undefined if x==0) |
| `__builtin_clzl(x)` | Count leading zeros in `unsigned long` |
| `__builtin_clzll(x)` | Count leading zeros in `unsigned long long` |
| `__builtin_popcount(x)` | Count set bits in `unsigned int` |
| `__builtin_popcountl(x)` | Count set bits in `unsigned long` |
| `__builtin_popcountll(x)` | Count set bits in `unsigned long long` |

## Type Introspection

| Builtin | Description |
|---------|-------------|
| `__builtin_constant_p(expr)` | Returns 1 if expr is compile-time constant |
| `__builtin_types_compatible_p(t1, t2)` | Returns 1 if types are compatible (ignores qualifiers) |

## Memory

| Builtin | Description |
|---------|-------------|
| `__builtin_alloca(size)` | Allocate `size` bytes on stack (freed on function return) |
| `__builtin_memset(dst, c, n)` | Set `n` bytes to `c` |
| `__builtin_memcpy(dst, src, n)` | Copy `n` bytes |
| `__builtin_memmove(dst, src, n)` | Copy `n` bytes (overlapping safe) |
| `__builtin_prefetch(addr, ...)` | Cache prefetch hint (no-op) |

## Control Flow

| Builtin | Description |
|---------|-------------|
| `__builtin_unreachable()` | Mark code path as unreachable (traps if reached) |
| `__builtin_expect(expr, c)` | Branch prediction hint (returns `expr` unchanged) |
| `__builtin_assume_aligned(ptr, align)` | Pointer alignment hint (returns `ptr` unchanged) |

## Structure Layout

| Builtin | Description |
|---------|-------------|
| `__builtin_offsetof(type, member)` | Byte offset of member within struct/union |
| `offsetof(type, member)` | Alias for `__builtin_offsetof` |

The member can be a chain like `field.subfield` or `arr[index].field`.

## Floating-Point Constants

| Builtin | Description |
|---------|-------------|
| `__builtin_inf()` | Positive infinity (`double`) |
| `__builtin_inff()` | Positive infinity (`float`) |
| `__builtin_infl()` | Positive infinity (`long double`) |
| `__builtin_huge_val()` | Positive infinity (`double`) |
| `__builtin_huge_valf()` | Positive infinity (`float`) |
| `__builtin_huge_vall()` | Positive infinity (`long double`) |
| `__builtin_nan(str)` | Quiet NaN (`double`) |
| `__builtin_nanf(str)` | Quiet NaN (`float`) |
| `__builtin_nanl(str)` | Quiet NaN (`long double`) |
| `__builtin_nans(str)` | Signaling NaN (`double`) |
| `__builtin_nansf(str)` | Signaling NaN (`float`) |
| `__builtin_nansl(str)` | Signaling NaN (`long double`) |

## Floating-Point Math

| Builtin | Description |
|---------|-------------|
| `__builtin_fabs(x)` | Absolute value (`double`) |
| `__builtin_fabsf(x)` | Absolute value (`float`) |
| `__builtin_fabsl(x)` | Absolute value (`long double`) |
| `__builtin_signbit(x)` | Returns non-zero if sign bit set (`double`) |
| `__builtin_signbitf(x)` | Returns non-zero if sign bit set (`float`) |
| `__builtin_signbitl(x)` | Returns non-zero if sign bit set (`long double`) |
| `__builtin_flt_rounds()` | Current FP rounding mode |

## Stack Introspection

| Builtin | Description |
|---------|-------------|
| `__builtin_frame_address(level)` | Frame pointer at `level` (0 = current) |
| `__builtin_return_address(level)` | Return address at `level` (0 = current) |

## Complex Numbers

| Builtin | Description |
|---------|-------------|
| `__builtin_complex(re, im)` | Build a complex value from two reals of the same type |

Used by `<complex.h>` for `I` and the `CMPLX`/`CMPLXF`/`CMPLXL` macros, which
exist precisely so `x + y*I` has an exact alternative that cannot corrupt an
infinite or NaN part.

**Limit:** only in a function body. A complex object with static storage
duration cannot be initialized at all — neither `double _Complex z = 1.0 +
2.0*I;` nor `CMPLX(1.0, 2.0)` — where gcc accepts both. See `#C11` in
`../audit.md`.

## Object Size and Fortification

| Builtin | Description |
|---------|-------------|
| `__builtin_object_size(ptr, type)` | Size of the object `ptr` points into |
| `__builtin___memcpy_chk`, `__builtin___memmove_chk`, `__builtin___memset_chk` | Checked memory operations |
| `__builtin___strcpy_chk`, `__builtin___strncpy_chk`, `__builtin___stpcpy_chk` | Checked string copies |
| `__builtin___strcat_chk`, `__builtin___strncat_chk` | Checked string concatenation |
| `__builtin___printf_chk`, `__builtin___fprintf_chk` | Checked formatted output |
| `__builtin___sprintf_chk`, `__builtin___snprintf_chk`, `__builtin___vsnprintf_chk` | Checked formatted output to a buffer |

These exist so glibc's fortified headers compile: with `_FORTIFY_SOURCE` set,
`<string.h>` and `<stdio.h>` rewrite their functions in terms of them.

**Limit:** `__builtin_object_size` always answers "unknown" (`(size_t)-1`)
where gcc computes the real size — 10 for a `char[10]`. glibc's fortification
is written to fall back to the unchecked call when the size is unknown, so
code **compiles and runs correctly but is not actually fortified**. Building
with `-D_FORTIFY_SOURCE=2` under c17 therefore buys nothing. See `#C12` in
`../audit.md`.

## C11 Atomic Builtins

| Builtin | Description |
|---------|-------------|
| `__c11_atomic_init(ptr, val)` | Non-atomic initialization |
| `__c11_atomic_load(ptr, order)` | Atomic load |
| `__c11_atomic_store(ptr, val, order)` | Atomic store |
| `__c11_atomic_exchange(ptr, val, order)` | Atomic swap, returns old value |
| `__c11_atomic_compare_exchange_strong(ptr, exp, des, succ, fail)` | Strong CAS |
| `__c11_atomic_compare_exchange_weak(ptr, exp, des, succ, fail)` | Weak CAS |
| `__c11_atomic_fetch_add(ptr, val, order)` | Atomic add, returns old value |
| `__c11_atomic_fetch_sub(ptr, val, order)` | Atomic subtract, returns old value |
| `__c11_atomic_fetch_and(ptr, val, order)` | Atomic AND, returns old value |
| `__c11_atomic_fetch_or(ptr, val, order)` | Atomic OR, returns old value |
| `__c11_atomic_fetch_xor(ptr, val, order)` | Atomic XOR, returns old value |
| `__c11_atomic_thread_fence(order)` | Thread memory fence |
| `__c11_atomic_signal_fence(order)` | Compiler barrier (signal fence) |

The `<stdatomic.h>` header maps the standard C11 names (`atomic_load`, `atomic_store`, etc.) to these builtins. `_Atomic` objects accessed through
ordinary operators — assignment, compound assignment, `++`/`--`, and plain
reads — are lowered to the same atomic instructions, so the builtins are not
the only way to reach them.

## Not implemented

Worth stating because their absence is silent and changes which branch a
system header takes.

| Builtin | Consequence |
|---------|-------------|
| `__builtin_isnan`, `__builtin_isinf`, `__builtin_isfinite`, `__builtin_isnormal`, `__builtin_fpclassify` | glibc's `<math.h>` falls back to calling `__isnanl` and friends. Those are conforming — C99 7.12.3.4 asks only for a nonzero value — but return glibc's raw class bits, so `isnan(x)` on a `long double` yields 65535 rather than gcc's 1. See `#C7` |
| `__builtin_classify_type`, `__real__`, `__imag__` | Needed by the host's `<tgmath.h>`; c17 bundles its own, built on `_Generic`, so this is not a blocker |
