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
| `__builtin_parity(x)` | Low bit of the population count of `unsigned int` |
| `__builtin_parityl(x)` | Same, `unsigned long` |
| `__builtin_parityll(x)` | Same, `unsigned long long` |
| `__builtin_clrsb(x)` | Redundant sign bits in `int` — the bits below the sign bit that repeat it. **Defined for every input**, unlike the `clz` family: 0 and -1 both answer 31 |
| `__builtin_clrsbl(x)` | Same, `long` |
| `__builtin_clrsbll(x)` | Same, `long long` |
| `__builtin_ffs(x)` | One-based index of the lowest set bit of `int`, 0 if none |
| `__builtin_ffsl(x)` | Same, `long` |
| `__builtin_ffsll(x)` | Same, `long long` |

## Type Introspection

| Builtin | Description |
|---------|-------------|
| `__builtin_constant_p(expr)` | Returns 1 if expr is compile-time constant |
| `__builtin_types_compatible_p(t1, t2)` | Returns 1 if types are compatible (ignores qualifiers) |
| `__builtin_choose_expr(c, a, b)` | `a` or `b` by the constant `c`; the untaken arm is not evaluated and need not even type-check |

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
| `__builtin_isnan(x)` | 1 if `x` is a NaN, else 0. Any real floating type |
| `__builtin_isinf(x)` | 1 if `x` is an infinity of either sign |
| `__builtin_isfinite(x)` | 1 if `x` is neither infinite nor NaN |
| `__builtin_isnormal(x)` | 1 if `x` is finite, non-zero and not subnormal |
| `__builtin_fpclassify(nan, inf, normal, subnormal, zero, x)` | Whichever of the five class codes describes `x` |
| `__builtin_flt_rounds()` | Current FP rounding mode |
| `__builtin_isinf_sign(x)` | +1 for +inf, -1 for -inf, 0 otherwise |
| `__builtin_sqrt(x)` | Square root. Calls the library `sqrt`, so it needs `-lm`; gcc folds a constant argument and does not |
| `__builtin_copysign(x, y)` | Magnitude of `x` with the sign of `y`. Calls the library `copysign` |

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

Usable at file scope and in a static initializer as well as in a function
body: `double _Complex g = 1.0 + 2.0*I;` and `CMPLX(3.0, 4.0)` both work, at
every precision. (This entry used to record the opposite as a limit; that was
fixed by `#C11` and the note outlived it.)

## Checked Arithmetic

C23 spells the generic three as `ckd_add`, `ckd_sub` and `ckd_mul`. Each
stores the wrapped result through the pointer and returns 1 if the true
result did not fit, 0 if it did — so the result is written either way.

| Builtin | Description |
|---------|-------------|
| `__builtin_add_overflow(a, b, *r)` | Type-generic; the operands and `*r` may differ in type |
| `__builtin_sub_overflow(a, b, *r)` | |
| `__builtin_mul_overflow(a, b, *r)` | |
| `__builtin_sadd_overflow(a, b, *r)` | Add, `int` |
| `__builtin_saddl_overflow(a, b, *r)` | Add, `long` |
| `__builtin_saddll_overflow(a, b, *r)` | Add, `long long` |
| `__builtin_uadd_overflow(a, b, *r)` | Add, `unsigned int` |
| `__builtin_uaddl_overflow(a, b, *r)` | Add, `unsigned long` |
| `__builtin_uaddll_overflow(a, b, *r)` | Add, `unsigned long long` |
| `__builtin_ssub_overflow(a, b, *r)` | Subtract, `int` |
| `__builtin_ssubl_overflow(a, b, *r)` | Subtract, `long` |
| `__builtin_ssubll_overflow(a, b, *r)` | Subtract, `long long` |
| `__builtin_usub_overflow(a, b, *r)` | Subtract, `unsigned int` |
| `__builtin_usubl_overflow(a, b, *r)` | Subtract, `unsigned long` |
| `__builtin_usubll_overflow(a, b, *r)` | Subtract, `unsigned long long` |
| `__builtin_smul_overflow(a, b, *r)` | Multiply, `int` |
| `__builtin_smull_overflow(a, b, *r)` | Multiply, `long` |
| `__builtin_smulll_overflow(a, b, *r)` | Multiply, `long long` |
| `__builtin_umul_overflow(a, b, *r)` | Multiply, `unsigned int` |
| `__builtin_umull_overflow(a, b, *r)` | Multiply, `unsigned long` |
| `__builtin_umulll_overflow(a, b, *r)` | Multiply, `unsigned long long` |

## Library Functions

The builtin of the same name as a library function. c17 emits a call to that
function, so the usual library rules apply — `__builtin_sqrt` needs `-lm`.
They exist so a translation unit may use one without having included the
header that declares it, which is what gcc allows and what glibc's fortified
headers rely on.

| Builtin | Description |
|---------|-------------|
| `__builtin_strlen(s)` | |
| `__builtin_strcmp(a, b)` | |
| `__builtin_abs(x)` | Absolute value, `int` |
| `__builtin_labs(x)` | Absolute value, `long` |
| `__builtin_llabs(x)` | Absolute value, `long long` |
| `__builtin_trap()` | Abnormal termination; lowered to `abort` |

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

`__builtin_object_size` computes real sizes — 10 for a `char[10]` — and the
`_chk` family has the implicit declarations glibc's headers expect.

**Limit:** `-D_FORTIFY_SOURCE=2` still buys nothing, but no longer because of
these builtins. c17 does not predefine `__OPTIMIZE__`, without which glibc
compiles no fortified wrapper at all; and enabling it needs
`__builtin_object_size` folded *after* inlining, since the wrapper measures its
own parameter and would otherwise be handed "unknown". See the
`_FORTIFY_SOURCE` entry in `TODO.md`, which is where this is tracked; it was
also `#C12` in `../audit.md` until that file was narrowed to conformance
findings alone.

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
| `__builtin_classify_type` | Needed by the host's `<tgmath.h>`; c17 bundles its own, built on `_Generic`, so this is not a blocker. `__has_builtin` answers 0, so guarded code is already correct |
| `__builtin_clear_padding` | Would have to walk a type to find its padding |
| `__builtin_setjmp`, `__builtin_va_arg_pack` | Not implemented; the ordinary `setjmp`/`longjmp` are |

`__real__` and `__imag__` used to be listed here and are **implemented** — see
`#C29` in `../audit.md`.
