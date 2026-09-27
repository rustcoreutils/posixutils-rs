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
| `__builtin_va_arg_pack()` | The caller's variadic arguments, spliced in at the call site. Only in an `always_inline` variadic function, and only as the last argument of a call |
| `__builtin_va_arg_pack_len()` | How many arguments the pack stands for. Same restriction |

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

The population counts, and the parities built on them, are inline and use only
baseline instructions: on x86-64 a branch-free SWAR sequence rather than
`popcnt`, which is not in x86-64-v1; on AArch64 `cnt` and `addv`. A constant
argument folds at `-O1` and above.

## Type Introspection

| Builtin | Description |
|---------|-------------|
| `__builtin_constant_p(expr)` | 1 if `expr` is a compile-time constant. **Level-dependent, as in gcc**: answered after propagation has run, so a local holding a constant is one at `-O1` and above and is not with the optimizer off. A literal is 1 at every level. The argument is never evaluated, whatever the answer |
| `__builtin_types_compatible_p(t1, t2)` | Returns 1 if types are compatible (ignores qualifiers) |
| `__builtin_classify_type(expr)` | A code for the argument's type family: 1 integer, 5 pointer, 8 real floating, 9 complex, 12 struct, 13 union. The usual conversions run first, so a `char`, an enumeration constant and a `_Bool` all answer 1, and an array, a function and a string literal all answer 5. The argument is not evaluated |
| `__builtin_choose_expr(c, a, b)` | `a` or `b` by the constant `c`; the untaken arm is not evaluated and need not even type-check |

## Memory

| Builtin | Description |
|---------|-------------|
| `__builtin_alloca(size)` | Allocate `size` bytes on stack (freed on function return) |
| `alloca(size)` | The same builtin under its bare name, as gcc predefines it. Unlike a `__builtin_*` spelling it is not reserved, so a declaration that is not a function displaces it; the one in `<alloca.h>` is a function and does not |
| `memset(dst, c, n)`, `__builtin_memset(dst, c, n)` | Set `n` bytes to `(unsigned char)c` |
| `memcpy(dst, src, n)`, `__builtin_memcpy(dst, src, n)` | Copy `n` bytes |
| `memmove(dst, src, n)`, `__builtin_memmove(dst, src, n)` | Copy `n` bytes (overlapping safe) |
| `__builtin_prefetch(addr, ...)` | Cache prefetch hint. Emits nothing, but `addr` is still **evaluated** — `__builtin_prefetch((q = p))` assigns `q`. The `rw` and locality arguments must be constants, so they have nothing to evaluate |

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

Each also has `f16`, `f32`, `f64` and `f128` forms (`__builtin_inff16()`,
`__builtin_nansf128(str)`, ...) giving a `_Float16`, `float`, `double` and
`_Float128` respectively; the `f128` forms exist only where `_Float128` does,
which is not macOS. A NaN's string names its payload; one that does not parse
as a number makes the quiet forms a call to `nan`, `nanf16` and so on.

## Library Functions Computed in Place

`abs`, `labs`, `llabs`, `imaxabs`, `fabs`, `fabsf`, `fabsl`, `copysign`,
`copysignf`, `copysignl`, `sqrt`, `sqrtf`, `sqrtl`, `floor`, `ceil`,
`trunc`, `round`, `rint`, `nearbyint`, `fmin`, `fmax` and `fma` and their
`f` forms, `creal`, `cimag` and `conj` in each precision, and `memcpy`,
`memset` and `memmove` are known to c17 by prototype, under their bare names
and their `__builtin_` spellings. One table in `parse/library_builtin.rs`
gives each its prototype and what it computes.

A `memcpy` or `memset` whose length is a constant of at most 128 bytes, or a
`memmove` of at most 64, is expanded into integer loads and stores of 8, 4, 2
and 1 bytes at every level, as gcc does at `-O2`; a longer or a variable
length is a call. The expansion is IR (`ir/memexpand.rs`), after inlining and
again in the optimizer's loop, so a length that inlining makes constant is
expanded too, and the stored bytes are forwarded like any others: `unsigned x
= 5, y; memcpy(&y, &x, 4); return y + 1;` returns the constant 6. No
alignment is assumed. `memmove` loads every chunk before it stores any, which
is what makes it right for an overlap in either direction, and why its limit
is lower -- every chunk is live at once. The limit is the one the linearizer's
own aggregate copies use, in the same chunks. The bare names are displaced
like `sqrt`: by a declaration that is not the `<string.h>` prototype, by
`-fno-builtin[-memcpy]`, and by the translation unit's own non-weak
definition, which glibc's fortify wrappers rely on.

At `-O0`, as in gcc, a libm function named by its bare spelling (`sqrt`) is
called rather than computed, and so is one that must still set `errno`
whatever its spelling -- unless its answer is a constant, which it is at
every level, so `static double d = floor(2.5);` compiles at `-O0` too; the magnitudes, `copysign` and the complex accessors
are computed in place at every level, and the block memory functions are
their IR operation at every level. A libm function computed in place is one
IR opcode keyed on its type; where the target has no instruction for that
type (binary128 on aarch64; `round`, `nearbyint`, `fmin`, `fmax` and `fma`
on x86-64) it becomes a call to the library function again after the
optimizer, which could still fold it.

However it is evaluated, what the program wrote is a call (C17 7.1.4p1). The
arguments are checked exactly as an ordinary call to a function of that
prototype checks them -- the same errors and warnings, in the same words --
and converted to the parameter type; and the result is a value, so
`creal(z) = 1.0`, `&creal(z)` and `++abs(i)` are errors, as they are for any
call.

## Floating-Point Math

| Builtin | Description |
|---------|-------------|
| `__builtin_fabs(x)` | Absolute value (`double`), computed in place |
| `__builtin_fabsf(x)` | Absolute value (`float`), computed in place |
| `__builtin_fabsl(x)` | Absolute value (`long double`), computed in place |
| `floor(x)`, `ceil(x)`, `trunc(x)`, `round(x)`, `rint(x)`, `nearbyint(x)`, their `f` forms, and their `__builtin_` spellings | The integer `x` rounds to, with `x`'s sign (`ceil(-0.5)` is `-0`), computed in place: `frintm`, `frintp`, `frintz`, `frinta`, `frintx` and `frinti` on aarch64; on x86-64, whose baseline has no `roundsd`, gcc's SSE2 sequences for `floor`, `ceil` and `trunc` (a truncating conversion, corrected by one) and `rint` (2^52 added and subtracted), each for a magnitude below 2^52 (2^23) -- anything larger, infinite or NaN is its own answer. Unlike gcc's, these set the sign of the result rather than or-ing it in, and `rint` rounds the value rather than its magnitude, so they are right in every rounding direction, not only the default one. `round` and `nearbyint` are calls on x86-64, as in gcc (the SSE2 `rint` raises *inexact*, which `nearbyint` must not). **A `float` argument to the `double` function is narrowed to the `f` form**: `(float)floor((double)x)` is `floorf(x)` exactly, because the result is an integer no greater in magnitude than `x`; the condition is the argument's type, not the result's, and the call is still a `double` (`sizeof floor(1.0f)` is 8), and still a call to `floor`: a definition of `floor` displaces it, and the program's `floor` receives the argument as a `double`. It is narrowed only where computed in place; at `-O0` the bare spelling calls `floor`, as in gcc. Only these six qualify -- `sin` and `log` are not exactly rounding, and narrowing one changes the last bit. Constants fold, in static initializers too, except a `rint` or `nearbyint` of a value that is not already an integer, whose answer is the current direction's; gcc leaves those too. The `l` forms are library aliases (below). Displaced like `sqrt`, a definition included |
| `fabs(x)`, `fabsf(x)`, `fabsl(x)` | The same three under their bare names, as gcc recognizes them whether or not `<math.h>` was included. Not reserved spellings, so they are displaced by a declaration that is not a function, by a function declaration whose type is not the library prototype (`struct S fabs(int)`), or by `-fno-builtin[-fabs]`. The bare name is still an object where it is not being called, so `double (*p)(double) = fabs;` names the library function. The argument is converted to the prototype's type first; the optimizer gains the one fact it needs to fold `fabs(x) < 0.0` to 0, and a constant argument folds. All three are computed in place by clearing the sign bit and nothing else -- never a call, so no program needs libm for them; `-0.0` becomes `+0.0` and a NaN, quiet or signalling, keeps its payload and raises nothing |
| `abs(x)`, `labs(x)`, `llabs(x)`, `imaxabs(x)` and their `__builtin_` spellings | Magnitude of an `int`, `long`, `long long` or `intmax_t`, computed in place as `(x ^ s) - s` with `s = x >> (width - 1)` -- never a call, at every level, as gcc does; a constant argument therefore folds. The argument is converted to the prototype's type first. The bare names are displaced like `fabs`, and a declaration with any other type (`struct S abs(int)`) makes the name an ordinary function, as in gcc; a translation unit's own compatible definition of one does **not** displace it, since defining a reserved library name is undefined (C17 7.1.3p2). `abs(INT_MIN)` wraps to `INT_MIN` |
| `copysign(x, y)`, `copysignf`, `copysignl` and their `__builtin_` spellings | `x` with the sign bit of `y`, computed in place on both targets by moving that one bit -- never a call, so no program needs libm for them. The sign is taken from a zero or a NaN as from anything else (`copysign(1.0, -0.0)` is `-1.0`), and nothing of `x` but its sign changes: a NaN keeps its payload, and a signalling one stays signalling and raises nothing. Both arguments are converted to the prototype's type first, and constant arguments fold, in a static initializer too (but, as in gcc, a call is never an integer constant expression). The bare names are displaced like `fabs`, by a declaration whose parameters are not both the prototype's or by `-fno-builtin[-copysign]`. `bits/floatn.h` reaches for `__builtin_copysignf`, so every spelling is load-bearing |
| `__builtin_signbit(x)` | 1 if the sign bit of `x` is set, else 0 -- for `-0.0` and a negative NaN too. Any real floating type, read at its own width (a `_Float16` widened to `float`, a `__float128` to `long double`); glibc's `<math.h>` `signbit` is this. Computed in place on both targets, never a call; of a constant it is an integer constant expression, as in gcc. C only asks for nonzero; gcc answers 1 for a constant but at run time the bit in place (`INT_MIN` for a `float`, 512 for an x86-64 `long double`), and c17 answers 1 at every width and level |
| `__builtin_signbitf(x)`, `__builtin_signbitl(x)` | The same, of `x` converted to `float` or `long double` |
| `__builtin_isnan(x)`, `__builtin_isnanf`, `__builtin_isnanl` | 1 if `x` is a NaN, else 0. Any real floating type; the suffix is accepted but not consulted, since the operand's own type decides |
| `__builtin_isinf(x)`, `__builtin_isinff`, `__builtin_isinfl` | 1 if `x` is an infinity of either sign |
| `__builtin_isfinite(x)` | 1 if `x` is neither infinite nor NaN. gcc has no `f`/`l` spelling of this one, or of `isnormal`, so neither does c17 |
| `__builtin_isnormal(x)` | 1 if `x` is finite, non-zero and not subnormal |
| `__builtin_fpclassify(nan, inf, normal, subnormal, zero, x)` | Whichever of the five class codes describes `x` |
| `__builtin_flt_rounds()` | Current FP rounding mode |
| `__builtin_isinf_sign(x)` | +1 for +inf, -1 for -inf, 0 otherwise |
| `sqrt(x)`, `sqrtf`, `sqrtl` and their `__builtin_` spellings | The correctly rounded square root, by the instruction: `sqrtsd`/`sqrtss` and x87 `fsqrt` on x86-64, `fsqrt` on aarch64; binary128 (`sqrtl` on aarch64 Linux) is a call. As in gcc, an argument below zero -- an ordered `x < 0`, so not `-0` and not a NaN -- still goes to the library, which sets `errno` to `EDOM`; `-fno-math-errno` drops that call and the program needs no libm. A constant argument folds, in a static initializer too, exactly as the instruction rounds it; a negative one is left to run time, and in a static initializer is not a constant. Under `-fno-math-errno`, `sqrt(x) < 0` folds to 0. Displaced like `fabs`, and also, as in gcc, by the translation unit's own definition of the function, wherever it is: its calls, above the definition too, reach it |
| `fmin(x, y)`, `fmax(x, y)`, `fma(x, y, z)`, their `f` forms, and their `__builtin_` spellings | The smaller and larger of two values -- a quiet NaN argument ignored for the other -- and `x * y + z` rounded once. `fminnm`, `fmaxnm` and `fmadd` on aarch64; calls on x86-64, as in gcc, whose baseline has no FMA and whose `minsd` does not ignore a NaN. Constants fold on both targets, exactly: `fma` with one rounding, and the zeros C leaves open as gcc folds them and `fminnm` answers, `fmin(+0, -0)` -0 and `fmax(-0, +0)` +0 (glibc's x86-64 functions answer the first argument). In a static initializer a NaN argument is not a constant, as in gcc. The `l` forms are library aliases (below). Displaced like `sqrt`, a definition included |
| `__builtin_fmaxl`, `fminl`, `fmal` | The `long double` forms |
| `__builtin_pow(x, y)`, `powf`, `powl` | `x` raised to `y` |
| `__builtin_ceill`, `floorl`, `truncl`, `roundl`, `rintl`, `nearbyintl` | The `long double` roundings |
| `__builtin_cbrt` | And its `f` and `l` spellings |
| `__builtin_sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh` | And their `f` and `l` spellings |
| `__builtin_exp`, `exp2`, `expm1`, `log`, `log2`, `log10`, `log1p`, `logb`, `tgamma`, `lgamma`, `erf`, `erfc` | And their `f` and `l` spellings |
| `__builtin_fmod`, `atan2`, `hypot`, `fdim`, `remainder`, `nextafter` | Two arguments; and their `f` and `l` spellings |
| `__builtin_modf(x, *ip)`, `__builtin_frexp(x, *e)`, `__builtin_ldexp(x, e)` | These three do **not** take a list of one type -- the second parameter is a pointer or an `int`. Declaring one uniformly sends that argument to the wrong register file, which is a silent wrong answer rather than a link error |

Every one of these is a call to the library function of the same name, so the
usual library rules apply. Their signatures come from **one table**, keyed by
the suffix: a `float` entry point takes and returns `float`, and getting that
wrong does not fail to link.

### The unordered-safe relations (C99 7.12.14)

| Builtin | Description |
|---------|-------------|
| `__builtin_isgreater(x, y)` | `x > y` |
| `__builtin_isgreaterequal(x, y)` | `x >= y` |
| `__builtin_isless(x, y)` | `x < y` |
| `__builtin_islessequal(x, y)` | `x <= y` |
| `__builtin_islessgreater(x, y)` | Ordered and unequal -- **not** `x != y`, which is *true* for an unordered pair |
| `__builtin_isunordered(x, y)` | At least one operand is a NaN |
| `__builtin_iseqsig(x, y)` | `x == y`, the C23 `iseqsig`. The answer is exact; see below for the exception it does not raise |

Every one of these is false for an unordered pair except `isunordered`, which
is the only one true for it. They exist in C because the ordinary relational
operators are specified to raise `FE_INVALID` on an unordered pair and these
are not; c17 emits the quiet compare (`ucomis*`, `fucomip`) for both, so the
two agree and there is nothing further to arrange.

`iseqsig` is the exception in the other direction: C23 7.12.17.1 has it raise
`FE_INVALID` for any unordered pair, a quiet NaN included, where `==` raises
it only for a signalling one. c17 emits the same quiet compare for it, so the
result is right and `FE_INVALID` is not raised for a quiet NaN -- the same gap
c17's `<` and `>` have, which are emitted with the quiet compare as well.

glibc's `<math.h>` **defines** `isgreater`, `isless`, `isunordered` and the
rest as these builtins, so a translation unit that includes the header and
uses one did not compile at all without them.

The operands go through the usual arithmetic conversions, as the operators
they stand for do. Each is evaluated exactly once: the relation is desugared
in the linearizer, not written out as `a < b` in the parser.


## Stack Introspection

| Builtin | Description |
|---------|-------------|
| `__builtin_frame_address(level)` | Frame pointer at `level` (0 = current) |
| `__builtin_return_address(level)` | Return address at `level` (0 = current) |
| `__builtin_extract_return_addr(addr)` | The identity on both targets c17 has. It exists for architectures that encode a flag in the return address -- ARM Thumb sets bit 0 -- and there is nothing to strip on x86-64 or AArch64, which is what gcc does there too |
| `__builtin___clear_cache(begin, end)` | Make instructions written as data visible to the fetcher. Lowered to libgcc's `__clear_cache`, which is the no-op on x86-64, where the caches are coherent, and does the work on AArch64, where a JIT is wrong without it |

## Complex Numbers

| Builtin | Description |
|---------|-------------|
| `__builtin_complex(re, im)` | Build a complex value from two reals of the same type |
| `__builtin_creal(z)`, `__builtin_crealf`, `__builtin_creall` | The real half, read in place as `__real__` reads it -- not a libm call. Unlike `__real__ z`, the result is a value, never an lvalue |
| `__builtin_cimag(z)`, `__builtin_cimagf`, `__builtin_cimagl` | The imaginary half |
| `__builtin_conj(z)`, `__builtin_conjf`, `__builtin_conjl` | The complex conjugate, computed in place as `~z` computes it, negating the imaginary half, so a zero imaginary part conjugates to **negative** zero, as it must, and `z` is evaluated once |
| `creal`, `crealf`, `creall`, `cimag`, `cimagf`, `cimagl`, `conj`, `conjf`, `conjl` | The same nine under their bare names, as gcc recognizes them. Displaced like `fabs`: by a declaration that is not a function or whose type is not the `<complex.h>` prototype, or by `-fno-builtin[-NAME]`; recognized only where called. For every spelling the argument converts to the complex type the suffix names first, as the prototype would convert it -- `crealf` of a `double _Complex` is the rounded `float` half, and `creal(3)` is 3.0 |

Used by `<complex.h>` for `I` and the `CMPLX`/`CMPLXF`/`CMPLXL` macros, which
exist precisely so `x + y*I` has an exact alternative that cannot corrupt an
infinite or NaN part.

Usable at file scope and in a static initializer as well as in a function
body: `double _Complex g = 1.0 + 2.0*I;` and `CMPLX(3.0, 4.0)` both work, at
every precision. (This entry used to record the opposite as a limit; that was
fixed by `#C11` and the note outlived it.)

The complex *integer* types are supported as well -- `_Complex int`,
`_Complex long`, `_Complex unsigned char` and the rest, a GNU extension. They
behave as two integers laid end to end: `sizeof` is twice the base, the halves
align to the base, and each half wraps at its own width. Multiply and divide
are open-coded rather than routed through `__mulsc3`/`__divsc3`, which exist
only for the floating formats and whose infinity recovery has no meaning for a
type that wraps. Imaginary constants may be integers too -- `2i` is a
`_Complex int` -- and `~z` is the conjugate for every complex type, floating
and integer alike, which is what gcc gives `~` on a complex operand.

`_Complex __int128` is thirty-two bytes and so travels in memory and returns
through the hidden pointer, on both targets.

Division uses **Smith's method**, matching gcc, rather than the textbook
formula `((ac + bd) + (bc - ad)i) / (c*c + d*d)`. The textbook form is exact
but overflows: `(4000000000u + 0i) / (2u + 0i)` needs `a * c` to hold 8e9,
which a 32-bit half cannot, and the quotient comes out 926258176. Smith's
method divides through by the larger half first, so the products stay near the
magnitude of the operands.

The cost is a branch and a truncation. Each step truncates toward zero, as any
integer division does, so `(-9 + 38i) / (5 + 6i)` is `6 + 1i` where the exact
quotient is `3 + 4i` -- gcc answers the same, because it is the same
algorithm. The standard specifies nothing here (the whole type is an
extension), so gcc's behaviour is the only available definition, and matching
it is the point.

## Checked Arithmetic

C23 spells the generic three as `ckd_add`, `ckd_sub` and `ckd_mul`. Each
stores the wrapped result through the pointer and returns 1 if the true
result did not fit, 0 if it did — so the result is written either way.

| Builtin | Description |
|---------|-------------|
| `__builtin_add_overflow(a, b, *r)` | Type-generic; the operands and `*r` may differ in type |
| `__builtin_sub_overflow(a, b, *r)` | |
| `__builtin_mul_overflow(a, b, *r)` | |
| `__builtin_add_overflow_p(a, b, v)` | The same question, answered without storing. `v` names the destination type with a *value* rather than a pointer; it is still evaluated, as gcc evaluates it, but its value is unused |
| `__builtin_sub_overflow_p(a, b, v)` | |
| `__builtin_mul_overflow_p(a, b, v)` | |
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
function, so the usual library rules apply — `__builtin_pow` needs `-lm`.
They exist so a translation unit may use one without having included the
header that declares it, which is what gcc allows and what glibc's fortified
headers rely on.

When the translation unit does declare the function, a call through the
`__builtin_` name is checked against that declaration exactly as a call through
the plain name is. When it does not, c17 declares the function itself. A
`<string.h>` or `<stdio.h>` function the optimizer knows (below) is declared
with the library's own prototype, and its arguments are checked against it;
for anything else c17 knows only how many parameters the entry point has, not
always their types, so such a call's arguments are not checked.

Once optimizing, a call to `strlen`, `strnlen`, `strcmp`, `strncmp`, `memcmp`,
`strchr`, `strrchr`, `index`, `rindex`, `memchr`, `strstr`, `strpbrk` or
`strcspn`, by either spelling, is computed in place where its arguments decide
the result, as gcc does: the bytes of a string literal or of a `const` `char`
array defined in the translation unit are read, `strcmp(p, "")` is the first
byte of `p`, and `strstr(p, "c")` is `strchr(p, 'c')`. A `strncmp` or `memcmp`
of the constant length 0 is 0 at every level. `-fno-builtin` and
`-fno-builtin-NAME` keep the bare name's call, as does a declaration of the
name with another prototype.

A call to `strcpy`, `stpcpy`, `strncpy`, `strcat`, `strncat` or `sprintf`
whose source string has a known length -- one string, or a choice among strings
of one length -- is a `memcpy` of that length and the terminator, as gcc makes
it: `strcat` finds the end with `strlen`, `strncpy` pads with a `memset` of zero
up to 128 bytes, and `sprintf` of a format with no conversion, or of `"%s"`,
answers the length. `stpcpy`, or `sprintf(d, "%s", s)`, of an unknown `s`
whose result is unused is `strcpy(d, s)`.


The call always reaches the *library's* function, never an inline definition
of the same name in the translation unit. That is the other half of what the
fortified headers rely on: an `always_inline` `extern inline` `strncpy` whose
body is `return __builtin_strncpy(...)` is not recursive, is inlined at every
call site, and leaves behind a call to the external `strncpy`. A call to a
function by its own name inside its own body is still recursion, as in gcc.

| Builtin | Description |
|---------|-------------|
| `__builtin_abort()` | |
| `__builtin_exit(status)` | |
| `__builtin_trap()` | Abnormal termination; lowered to `abort` |
| `__builtin_malloc(n)`, `__builtin_calloc(n, sz)`, `__builtin_realloc(p, n)`, `__builtin_free(p)` | The allocators. The three allocating forms return `void *` |
| `__builtin_memcmp(a, b, n)` | |
| `__builtin_mempcpy(dst, src, n)` | Returns the **end** of the copied region, unlike `memcpy` |
| `__builtin_strlen(s)`, `__builtin_strcmp(a, b)`, `__builtin_strncmp(a, b, n)` | |
| `__builtin_strcpy(d, s)`, `__builtin_strncpy(d, s, n)`, `__builtin_stpcpy(d, s)` | `stpcpy` returns the end of the copy |
| `__builtin_strcat(d, s)`, `__builtin_strncat(d, s, n)` | |
| `__builtin_strchr(s, c)`, `__builtin_strrchr(s, c)`, `__builtin_strstr(h, n)` | |
| `__builtin_printf(fmt, ...)`, `__builtin_sprintf(buf, fmt, ...)`, `__builtin_snprintf(buf, n, fmt, ...)` | Variadic after the format argument |
| `__builtin_puts(s)`, `__builtin_putchar(c)` | |
| `__builtin_fprintf(stream, fmt, ...)` | Variadic after the format argument |
| `__builtin_fputs(s, stream)`, `__builtin_fputc(c, stream)` | |
| `__builtin_fwrite(p, size, n, stream)` | Returns a size |
| `__builtin_memchr(p, c, n)` | Returns `void *` |
| `__builtin_index(s, c)`, `__builtin_rindex(s, c)` | The older spellings of `strchr`/`strrchr` |
| `__builtin_strpbrk(s, set)` | |
| `__builtin_strcasecmp(a, b)`, `__builtin_strncasecmp(a, b, n)` | The POSIX case-insensitive comparisons |
| `__builtin_strndup(s, n)` | |
| `__builtin_memcmp_eq(a, b, n)` | gcc's equality-only `memcmp`: it answers zero or non-zero rather than an ordering, which lets it use a wider compare. Answering the ordering as well implements it |
| `__builtin_stpncpy(d, s, n)` | Like `strncpy`, returning the end of what it wrote |
| `__builtin_strdup(s)` | |
| `__builtin_bcmp(a, b, n)` | The older spelling of `memcmp` |
| `__builtin_bzero(p, n)` | The older spelling of `memset(p, 0, n)`; returns `void` |
| `__builtin_strspn(s, set)`, `__builtin_strcspn(s, set)` | Return a size, not a pointer |
| `__builtin_bcopy(src, dst, n)` | Returns `void`, and takes the source **first**, unlike `memcpy` |
| `__builtin_printf_unlocked`, `__builtin_fprintf_unlocked`, `__builtin_fputs_unlocked` | glibc defines none of these, so a program using one supplies it — which is what gcc.c-torture's `builtins/` tests do |

The return types matter and are modelled: the string family returns `char *`,
the allocators and `mempcpy` return `void *`. Typing one of them `int` would
truncate the returned address to 32 bits — a silent wrong answer, since the
call still links and runs. So would getting the printf family's fixed-argument
count wrong on Apple arm64, where variadic arguments go on the stack while
fixed ones stay in registers.

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

**Limit:** `-D_FORTIFY_SOURCE=2` now compiles glibc's fortified wrappers and
emits `__*_chk` calls -- `__OPTIMIZE__` is predefined, and
`__builtin_va_arg_pack` makes their argument forwarding compile. It still does
not *check*: `__builtin_object_size` is folded at parse time, where a wrapper
measuring its own parameter can only answer "unknown", and folding it after
inlining is the remaining work. See the `_FORTIFY_SOURCE` entry in `DECISIONS.md`,
which is where this is tracked; it was also `#C12` in the conformance audit until that
file was narrowed to conformance findings alone.

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

## GNU Atomic Builtins

gcc's own spellings, which real C reaches for directly: `pycore_atomic.h`,
`valgrind/config.h` and `pyconfig.h` all use them, so a translation unit that
includes one of those did not compile without them.

| Builtin | Description |
|---------|-------------|
| `__atomic_load_n(p, order)`, `__atomic_store_n(p, v, order)` | |
| `__atomic_exchange_n(p, v, order)` | Swap, returning the old value |
| `__atomic_compare_exchange_n(p, expected, desired, weak, succ, fail)` | `expected` is a pointer, and the observed value is written back through it on failure |
| `__atomic_fetch_add/sub/and/or/xor/nand(p, v, order)` | Returns the value **before** |
| `__atomic_add/sub/and/or/xor/nand_fetch(p, v, order)` | Returns the value **after** |
| `__atomic_test_and_set(p, order)`, `__atomic_clear(p, order)` | |
| `__atomic_thread_fence(order)`, `__atomic_signal_fence(order)` | |
| `__atomic_always_lock_free(size, p)`, `__atomic_is_lock_free(size, p)` | Answered from the size: 1, 2, 4 and 8 are lock-free |
| `__sync_fetch_and_add/sub/and/or/xor/nand(p, v, ...)` | Sequentially consistent, returning the value before |
| `__sync_add/sub/and/or/xor/nand_and_fetch(p, v, ...)` | The same, returning the value after |
| `__sync_bool_compare_and_swap(p, old, new)` | Whether the exchange happened. `old` arrives **by value**, unlike the C11 and `__atomic_` forms |
| `__sync_val_compare_and_swap(p, old, new)` | The object's previous value |
| `__sync_lock_test_and_set(p, v)`, `__sync_lock_release(p)` | An exchange and a store of zero |
| `__sync_synchronize()` | A full fence |

`nand` is `~(old & val)` and has no instruction on any target, so it is always
the compare-exchange loop -- the same loop the other operations fall back to,
with one more instruction inside it.

The `__sync_*` family predates the C11 orders and is sequentially consistent;
each also accepts the trailing list of variables gcc documents and ignores.

An `__atomic_*` builtin's `order` argument is honoured by the load, store,
exchange and compare-exchange forms, which carry it into the instruction. The
**read-modify-write forms discard it** and are sequentially consistent
whatever it says: they go through `emit_atomic_rmw`, which an `_Atomic`
compound assignment also uses, and that is seq-cst by C17 6.5.16.2p3. A
stronger order than the one asked for is always correct and never wrong, so
this costs speed and not meaning -- but it does mean
`__atomic_fetch_add(p, v, __ATOMIC_RELAXED)` is not relaxed while
`__atomic_load_n(p, __ATOMIC_RELAXED)` is. Recorded in TODO.md.

`*_and_fetch` re-applies the operation to the value the exchange returned. That
is arithmetic on a value already in hand rather than a second access to the
object, and it reuses the operand *pseudo*, so `__sync_add_and_fetch(p, f())`
calls `f` exactly once.

`__GCC_HAVE_SYNC_COMPARE_AND_SWAP_{1,2,4,8}` is predefined, because it is now
a true statement about this compiler. It was withdrawn while the family was
unimplemented: a guarded `#ifdef` otherwise opened a branch that failed on an
undeclared identifier when the `#else` beside it would have compiled.

The `<stdatomic.h>` header maps the standard C11 names (`atomic_load`, `atomic_store`, etc.) to these builtins. `_Atomic` objects accessed through
ordinary operators — assignment, compound assignment, `++`/`--`, and plain
reads — are lowered to the same atomic instructions, so the builtins are not
the only way to reach them.

## Not implemented

Worth stating because their absence is silent and changes which branch a
system header takes.

| Builtin | Consequence |
|---------|-------------|
| `__builtin_clear_padding` | Would have to walk a type to find its padding |
| `__builtin_setjmp` | Not implemented; the ordinary `setjmp`/`longjmp` are |
| `__builtin_issignaling` | Distinguishes a signalling NaN from a quiet one. No system header uses it -- `<math.h>` has `issignaling` as its own macro -- so nothing fails to build without it |
| `__builtin_stack_save`, `__builtin_stack_restore` | The marks gcc puts around a VLA's lifetime. c17 frees a VLA at the end of its block without them |
| `__builtin_cexpi`, `__builtin_cpow` | Complex libm entry points gcc synthesizes; neither is declared by any header |

`__real__` and `__imag__` used to be listed here and are **implemented** — see
`#C29` in git log.
