# Compiler Builtins

The `__builtin_*`, `__sync_*`, `__atomic_*` and `__c11_atomic_*` functions
c17 recognises, what each becomes, and how to add one. Spellings follow gcc;
the `__c11_atomic_*` family and `__builtin_flt_rounds` follow clang. Where c17
differs from gcc the entry says so.

## Recognition

Every identifier in a primary expression is offered to `parse_builtin_expr`
(`parse/builtin_expr.rs`) unless `builtin_is_shadowed` says a declaration has
claimed the name. That function tries each family in turn and answers `None`
for a name it does not own, so a builtin is whatever some family matches --
the keyword tag is not consulted while parsing.

- **Reserved spellings** (`__builtin_*`, `__sync_*`, ...) are never
  displaced, whatever the program declares (C17 7.1.3).
- **Bare spellings** -- `alloca`, `offsetof`, `setjmp`, `_setjmp`, `longjmp`,
  `_longjmp`, and the library functions computed in place (`abs`, `fabs`,
  `sqrt`, `memcpy`, ...; see below) -- are displaced by `-fno-builtin`,
  `-fno-builtin-NAME`, a declaration that is not a function, and (library
  functions only) a function declaration whose type is not the library
  prototype. `offsetof` is displaced by any declaration.
- Any `__builtin___NAME` becomes a call to `__NAME` when a declaration of
  `__NAME` is in scope or c17 knows it (the `_chk` family); otherwise it is
  `undeclared function`.

### `__has_builtin`

Answered in `token/preprocess.rs` (`eval_has_builtin_expr`, inside `#if`) and
`token/preprocess_macro.rs` (`eval_has_builtin`, in running text), both from
`builtins.rs`: a name answers 1 when it is in `SUPPORTED_BUILTINS` (or tagged
`kw::BUILTIN`, which `builtins.rs` tests keep identical) and `available_on`
the target. `available_on` withholds only the `f128` constants on macOS,
which has no `_Float128`.

Differences from gcc's answers:

| Name | c17 | gcc | Why |
|------|-----|-----|-----|
| `alloca`, `abs`, `fabs`, `memcpy`, ... (bare) | 0 | 1 | Bare spellings are tagged 0 in `kw.rs`; `__has_builtin` asks about the reserved spelling |
| `offsetof` | 1 | 0 | Tagged `BUILTIN` |
| `__builtin_va_list` | 1 | 0 | It is a type, but listed |
| `__c11_atomic_*`, `__builtin_flt_rounds` | 1 | 0 | clang builtins gcc lacks |
| `__builtin___mempcpy_chk`, `__builtin___stpncpy_chk`, `__builtin___vsprintf_chk` | 0 | 1 | Work (glibc's fortified headers use them) but are not tagged |

`-fno-builtin` does not change any answer.

## Argument checking

Checked as an ordinary call is -- count, types, conversions, the usual
diagnostics in the usual words:

- the library functions computed in place (`LIBRARY_BUILTINS` rows made with
  `entry`), by either spelling;
- the bit builtins, against gcc's prototypes for them (`BIT_BUILTINS`);
- a `__builtin_X` that calls library function `X` when a declaration of `X`
  is in scope, or when `X` has a `known` row in `LIBRARY_BUILTINS` (the
  `<string.h>`/`<stdio.h>` functions the optimizer folds).

Checked by a dedicated diagnostic:

- `__builtin_signbit` and the unordered relations reject a non-floating
  argument, in gcc's words;
- `__builtin_choose_expr` requires a constant first argument;
- `__builtin_frame_address`/`__builtin_return_address` require a
  non-negative integer constant;
- `__builtin_fpclassify` requires exactly six arguments;
- `offsetof` requires a constant array index;
- `__builtin_nans*` requires a string literal naming a payload (gcc accepts
  any string);
- `__builtin_va_arg_pack*` require an `always_inline` variadic function.

Everything else is parsed by its fixed shape: a wrong argument count is a
parse error (`expected ')'`), and the argument types are **not** checked. In
particular, unlike gcc, c17 accepts without a diagnostic:

- a non-floating argument to `__builtin_isnan`, `isinf`, `isfinite`,
  `isnormal`, `isinf_sign` and `fpclassify`;
- `__builtin_complex` operands that are not floating or not of one type;
- a non-integer operand, or a non-pointer / pointer-to-non-integer result, to
  the checked-arithmetic builtins;
- a non-constant or out-of-range type argument to `__builtin_object_size`
  (treated as 0 and clamped to 0..3);
- a non-constant `rw`/locality to `__builtin_prefetch`;
- an operand of the wrong type to any atomic builtin, and a non-constant size
  to `__atomic_always_lock_free` (answered 0);
- `__builtin_va_start` in a function that is not variadic;
- a `__builtin_X` library call with neither a declaration nor a `known` row:
  c17 declares `X` itself with a placeholder prototype
  (`declare_chk_builtin`) and checks neither the count nor the types, so
  `__builtin_pow(1.0)` and `__builtin_sin(p)` compile.

## Variadic Functions

| Builtin | Description |
|---------|-------------|
| `__builtin_va_list` | Platform-specific `va_list` type (a type keyword) |
| `__builtin_va_start(ap, last)` | `Opcode::VaStart`. `last` must be an identifier; it is not checked to be the last parameter |
| `__builtin_va_arg(ap, type)` | `Opcode::VaArg`; an aggregate or complex result gets a frame temporary |
| `__builtin_va_end(ap)` | `Opcode::VaEnd` |
| `__builtin_va_copy(dest, src)` | `Opcode::VaCopy` |
| `__builtin_va_arg_pack()` | The caller's variadic arguments, spliced in by the inliner. Only in an `always_inline` variadic function (an error otherwise), and only as the last argument of a call (linearizer error otherwise) |
| `__builtin_va_arg_pack_len()` | How many arguments the pack stands for: `Opcode::VaArgPackLen`, replaced by a constant at inlining; a survivor is diagnosed by `opt::check_forwarding_resolved` |

## Byte Swapping and Bit Operations

| Builtin | Parameter | Becomes |
|---------|-----------|---------|
| `__builtin_bswap16(x)`, `bswap32`, `bswap64` | `unsigned short` / `unsigned int` / `unsigned long long` | `Opcode::Bswap16/32/64`; result of the parameter type |
| `__builtin_ctz(x)`, `ctzl`, `ctzll` | `unsigned int` / `unsigned long` / `unsigned long long` | `Opcode::Ctz32` / `Ctz64` (`l` and `ll` share); undefined for 0 |
| `__builtin_clz(x)`, `clzl`, `clzll` | as `ctz` | `Opcode::Clz32` / `Clz64`; undefined for 0 |
| `__builtin_popcount(x)`, `popcountl`, `popcountll` | as `ctz` | `Opcode::Popcount32` / `Popcount64` |
| `__builtin_parity(x)`, `parityl`, `parityll` | as `ctz` | Rewritten by the parser to `popcount(x) & 1` on one node, so `x` is evaluated once |
| `__builtin_clrsb(x)`, `clrsbl`, `clrsbll` | `int` / `long` / `long long` | Redundant sign bits: expanded by `linearize_clrsb` into `((x ^ (x >> w-1)) << 1 \| 1)` and a `Clz`. Defined for every input: 0 and -1 both answer 31 |
| `__builtin_ffs(x)`, `ffsl`, `ffsll` | `int` / `long` / `long long` | Of an integer constant expression, its value. Otherwise a call to the C library's `ffs`/`ffsl`/`ffsll` (gcc computes it inline), declared with that prototype unless the program declared it. The bare `ffs` is an ordinary function |

Every result other than a byte swap's is `int`. The prototypes are gcc's,
written once in `BIT_BUILTINS` (`parse/bit_builtin.rs`): a call is checked
by `check_call` as an ordinary call through that prototype is, and its
argument converted to the parameter type (C17 6.5.2.2p7), so
`__builtin_ctz(8.0)` is 3. A structure argument is an error and a pointer
the integer-from-pointer warning, each naming the builtin; a wrong argument
count is the ordinary call's error.

The population count uses only baseline instructions: on x86-64 a
branch-free SWAR sequence (`popcnt` is not in x86-64-v1), on AArch64 `cnt`
and `addv`. `clz` on x86-64 is `bsr` and an `xor` (no `lzcnt`).

Every bit builtin of an integer constant expression is one itself, as in
gcc: valid in a static initializer, a `case` label, an array bound, an
enumerator and `_Static_assert`. Each operation is evaluated by one rule,
`constfold::eval_bit_op`, which the C17 6.6 walk (`constexpr.rs`) applies to
the builtin's node, the parser to `ffs` of a constant (folded in place of
the library call), and `constfold::eval_unop` to the bit opcodes, so
`instcombine`, `sccp` and `vrp` fold a constant operand at `-O1` and above.
`ctz` and `clz` of a constant 0 fold to the operand width (32 or 64), the
value gcc folds them to on both targets; at run time they stay undefined.

## Type Introspection and Selection

| Builtin | Description |
|---------|-------------|
| `__builtin_constant_p(expr)` | 1 if `expr` is a compile-time constant. A parse-time constant (integer or floating) answers 1 at once; anything else answers 0 at `-O0`, and at `-O1`+ becomes `Opcode::ConstantP`, which `sccp` resolves to 1 if propagation proves the operand constant and `ir::lower` resolves to 0 otherwise -- so a local holding a constant is one only when optimizing, as in gcc. An operand with side effects answers 0 and is not linearized; the argument is never evaluated |
| `__builtin_types_compatible_p(t1, t2)` | `TypeTable::types_compatible` of two type names, top-level qualifiers ignored. **Bug:** an enumerated type is compatible with no integer type here (C17 6.7.2.2p4 makes it compatible with one; gcc uses `unsigned int`, or `int` with a negative enumerator), so `__builtin_types_compatible_p(enum E, unsigned)` is 0 where gcc answers 1, and `_Generic` misses the same association |
| `__builtin_classify_type(expr)` | A code for the type family after the usual conversions: 0 void, 1 integer (including `char`, enumerations and `_Bool`), 5 pointer (including arrays, functions and string literals), 8 real floating, 9 complex, 12 struct, 13 union. The argument is not evaluated |
| `__builtin_choose_expr(c, a, b)` | `a` or `b` by the constant `c`, selected in the parser; the other arm is parsed (so an undeclared name or unknown member in it is still an error) and then discarded, so it is never evaluated or linearized |

## Memory

| Builtin | Description |
|---------|-------------|
| `__builtin_alloca(size)`, `alloca(size)` | `Opcode::Alloca`, freed on return. An inlined callee's `alloca` is bracketed by `StackSave`/`StackRestore` so it is freed when the call would have returned. The bare `alloca` is displaced by a non-function declaration or `-fno-builtin[-alloca]`; `<alloca.h>`'s function declaration does not displace it |
| `memset`, `memcpy`, `memmove`, `mempcpy`, `bcopy` and their `__builtin_` spellings | Computed in place (next section) |
| `__builtin_prefetch(addr[, rw[, locality]])` | Emits nothing. `addr` is evaluated (`__builtin_prefetch((q = p))` assigns `q`); `rw` and locality are parsed and discarded without evaluation or the constant check gcc makes |

## Library Functions Computed in Place

One table, `LIBRARY_BUILTINS` in `parse/library_builtin.rs`, gives each of
these its prototype and its `InlineLibraryFn`: `abs`, `labs`, `llabs`,
`imaxabs`, `fabs`, `fabsf`, `fabsl`, `copysign`, `copysignf`, `copysignl`,
`sqrt`, `sqrtf`, `sqrtl`, `floor`, `ceil`, `trunc`, `round`, `rint`,
`nearbyint`, `fmin`, `fmax`, `fma` and their `f` forms, `creal`, `cimag` and
`conj` in each precision, and `memcpy`, `memset`, `memmove`, `mempcpy` and
`bcopy`, under their bare names and their `__builtin_` spellings.

What the program wrote is still a call (C17 7.1.4p1): the arguments are
checked exactly as an ordinary call to that prototype checks them, and
converted to the parameter types; the result is a value, so `creal(z) = 1.0`,
`&creal(z)` and `++abs(i)` are errors.

`LibraryCallPolicy::in_place` decides whether a call is computed or called.
Optimizing, every one is computed. At `-O0`, as in gcc, a libm function named
by its bare spelling (`sqrt`) is called, and so is one that must still set
`errno` whatever its spelling -- unless its answer is a constant, which is
folded at every level (`static double d = floor(2.5);` compiles at `-O0`).
The magnitudes, `copysign`, the complex accessors and the block memory
functions are computed at every level.

A libm function computed in place is one IR opcode keyed on its type
(`Sqrt`, `RoundToIntegral`, `FMin`, `FMax`, `Fma`; `Opcode::is_libm`). Where
the target has no instruction for that type (`ArchMapper::computes_in_place`:
binary128 on aarch64; `round`, `nearbyint`, `fmin`, `fmax` and `fma` on
x86-64) `call_library_fallbacks` turns it back into a call to the function
the instruction names, after the optimizer, which could still fold it.

Displacement: every bare name yields to `-fno-builtin[-NAME]`, a non-function
declaration and an incompatible function declaration (`struct S abs(int)`).
`sqrt`, the roundings, `fmin`, `fmax`, `fma` and the block memory functions
(`InlineLibraryFn::yields_to_a_definition`) also yield to the translation
unit's own non-weak definition, wherever it is -- calls above it reach it,
which glibc's fortify wrappers rely on. `abs`, `fabs`, `copysign` and the
complex accessors do not (defining a reserved library name is undefined, C17
7.1.3p2).

| Function | Computed as |
|----------|-------------|
| `abs`, `labs`, `llabs`, `imaxabs` | `(x ^ s) - s` with `s = x >> (width - 1)`; `abs(INT_MIN)` wraps. Constant in a static initializer, not an integer constant expression |
| `fabs`, `fabsf`, `fabsl` | `Opcode::Fabs`: clears the sign bit and nothing else, so `-0.0` becomes `+0.0` and a NaN keeps its payload and raises nothing. Never a call; `fabs(x) < 0.0` folds to 0 |
| `copysign`, `copysignf`, `copysignl` | `Opcode::CopySign`: moves one bit, from a zero or a NaN as from anything else. Never a call. `bits/floatn.h` uses `__builtin_copysignf` |
| `sqrt`, `sqrtf`, `sqrtl` | `Opcode::Sqrt`: `sqrtsd`/`sqrtss` and x87 `fsqrt` on x86-64, `fsqrt` on aarch64; binary128 (`sqrtl` on aarch64 Linux) is a call. With `-fmath-errno` (the default) an argument with `x < 0` (ordered, so not `-0` or NaN) still calls the library to set `EDOM`; `-fno-math-errno` drops that call, and then `sqrt(x) < 0` folds to 0. A constant folds, in a static initializer too, except a negative one |
| `floor`, `ceil`, `trunc`, `round`, `rint`, `nearbyint`, `f` forms | `Opcode::RoundToIntegral`: `frintm`, `frintp`, `frintz`, `frinta`, `frintx`, `frinti` on aarch64. On x86-64 (no `roundsd` in the baseline) gcc's SSE2 sequences for `floor`, `ceil`, `trunc` and `rint`, which set the result's sign rather than or-ing it in, so they are right in every rounding direction; `round` and `nearbyint` are calls. A `float` argument to the `double` function is computed by the `f` form (`NarrowedLibraryCall`): exact, because the result is an integer no larger than `x`; the call is still a `double` and still names `floor`. Constants fold, except a `rint`/`nearbyint` of a non-integer, whose answer depends on the rounding direction |
| `fmin`, `fmax`, `fma`, `f` forms | `fminnm`, `fmaxnm`, `fmadd` on aarch64; calls on x86-64 (no FMA in the baseline; `minsd` does not ignore a NaN). Constants fold on both targets: `fmin(+0, -0)` is -0 and `fmax(-0, +0)` +0; a NaN argument is not a constant in a static initializer |
| `creal`, `cimag`, `conj` (each precision) | The half read in place, as `__real__`/`__imag__` read it; `conj` negates the imaginary half as `~z` does, so a zero imaginary part becomes -0. The argument converts to the suffix's complex type first (`crealf` of a `double _Complex` is the rounded `float` half) |
| `memcpy`, `memset`, `memmove` | `Opcode::Memcpy` / `Memset` / `Memmove`, at every level |
| `mempcpy` | A `Memcpy` and an `Add`; `mempcpy` itself is never called |
| `bcopy(src, dst, n)` | A `Memmove` with the operands swapped; returns `void` |

The `l` forms of the roundings, `fmin`, `fmax` and `fma` are library calls
(below).

### Block memory functions

`ir/memexpand.rs` expands a `Memcpy` or `Memset` whose length is a constant
of at most 128 bytes (`INLINE_LIMIT_BYTES`), or a `Memmove` of at most 64
(`MOVE_LIMIT_BYTES`), into integer loads and stores of 8, 4, 2 and 1 bytes. It
runs at every level after inlining, and again inside the optimizer's loop, so
a length that inlining or SCCP makes constant is expanded too; the stored
bytes are then forwarded like any others (`unsigned x = 5, y; memcpy(&y, &x,
4); return y + 1;` returns the constant 6). No alignment is assumed.
`memmove` loads every chunk before storing any, which is why its limit is
lower. A longer or variable length is a call. The linearizer's own aggregate
copies use the same chunks and limit.

Optimizing, a `Memmove` whose blocks cannot overlap becomes a `Memcpy`
(`ir/libcall_fold/memory.rs`): its source is a string literal or a `const`
object defined here, or the two blocks are different locals, or one local and
one named object. Two different named objects are not enough: an alias or a
weak definition can put two names at one address.

## Floating-Point Constants

| Builtin | Description |
|---------|-------------|
| `__builtin_inf()`, `inff`, `infl`, `huge_val`, `huge_valf`, `huge_vall` | Positive infinity of `double`, `float`, `long double` |
| `__builtin_nan(str)`, `nanf`, `nanl` | Quiet NaN |
| `__builtin_nans(str)`, `nansf`, `nansl` | Signalling NaN |

Each also has `f16`, `f32`, `f64` and `f128` forms (`__builtin_inff16()`,
`__builtin_nansf128(str)`, ...) giving `_Float16`, `float`, `double` and
`_Float128`; the `f128` forms exist only where `_Float128` does (not macOS),
and are a parse error elsewhere. All come from `FLOAT_CONSTANT_BUILTINS` and
are literals, so they are constants everywhere.

A NaN's string literal names its payload, parsed as gcc's `strtoull`-based
reader parses it (`nan_payload`). A string that does not parse makes the
quiet forms a call to `nan`, `nanf16` and so on, and the signalling forms an
error.

## Floating-Point Classification and Comparison

| Builtin | Description |
|---------|-------------|
| `__builtin_signbit(x)` | 1 if the sign bit is set (also for `-0.0` and a negative NaN), else 0. Any real floating type, read at its own width (`_Float16` widened to `float`, `__float128` to `long double`). Of a constant it is an integer constant expression. gcc answers the bit in place at run time (`INT_MIN` for a `float`, 512 for an x86-64 `long double`); c17 answers 1 |
| `__builtin_signbitf(x)`, `__builtin_signbitl(x)` | The same, of `x` converted to `float` or `long double` |
| `__builtin_isnan(x)`, `isnanf`, `isnanl` | 1 if NaN. The suffix is not consulted; the operand's type decides |
| `__builtin_isinf(x)`, `isinff`, `isinfl` | 1 if an infinity of either sign |
| `__builtin_isinf_sign(x)` | +1 for +inf, -1 for -inf, 0 otherwise |
| `__builtin_isfinite(x)`, `__builtin_isnormal(x)` | As C's macros. No suffixed spellings, as in gcc |
| `__builtin_fpclassify(nan, inf, normal, subnormal, zero, x)` | Whichever of the five codes describes `x`; the codes are ordinary expressions |
| `__builtin_flt_rounds()` | The integer constant 1, whatever the current rounding mode (clang reads the mode; gcc has no such builtin) |

The classification builtins are `ExprKind::FpTest` / `FpClassify`, lowered by
`linearize_fp_test` / `linearize_fp_classify` into comparisons and bit tests;
`signbit` is `Opcode::Signbit`. Only `signbit` folds as a constant
expression.

### The unordered-safe relations (C99 7.12.14)

| Builtin | Description |
|---------|-------------|
| `__builtin_isgreater(x, y)` | `x > y` |
| `__builtin_isgreaterequal(x, y)` | `x >= y` |
| `__builtin_isless(x, y)` | `x < y` |
| `__builtin_islessequal(x, y)` | `x <= y` |
| `__builtin_islessgreater(x, y)` | Ordered and unequal -- **not** `x != y`, which is true for an unordered pair |
| `__builtin_isunordered(x, y)` | At least one operand is a NaN |
| `__builtin_iseqsig(x, y)` | `x == y` (C23 `iseqsig`) |

Each is false for an unordered pair except `isunordered`. The operands must
be real, at least one floating (gcc's rule), and go through the usual
arithmetic conversions; each is evaluated once, because the relation is an
`ExprKind::FpCompare` desugared in the linearizer. glibc's `<math.h>` defines
`isgreater` and the rest as these builtins.

c17 emits the quiet compare (`ucomis*`, `fucomip`, `fcmp`) for these and for
the ordinary relational operators alike, so the operators do not raise
`FE_INVALID` for a quiet NaN either, and `iseqsig` does not raise it for an
unordered pair as C23 7.12.17.1 requires. The results are exact.

## Stack Introspection and Non-Local Jumps

| Builtin | Description |
|---------|-------------|
| `__builtin_frame_address(level)` | `Opcode::FrameAddress`, walking `level` frame records |
| `__builtin_return_address(level)` | `Opcode::ReturnAddress` |
| `__builtin_extract_return_addr(addr)` | The identity on both targets (it exists for ARM Thumb's flag bit); `addr` is evaluated once |
| `__builtin___clear_cache(begin, end)` | A call to libgcc's `__clear_cache`: a no-op on x86-64, required on AArch64 for code written as data |
| `setjmp(env)`, `_setjmp(env)` | Bare names parsed as builtins: `Opcode::Setjmp`, calling the library function under the name its declaration gives it. `setjmp()` with no argument is an ordinary unprototyped call |
| `longjmp(env, val)`, `_longjmp(env, val)` | `Opcode::Longjmp`, a terminator |

`setjmp` and `longjmp` are displaced only by a declaration that is not a
function, or by `-fno-builtin`; `<setjmp.h>`'s function declarations keep
them.

## Control Flow and Hints

| Builtin | Description |
|---------|-------------|
| `__builtin_unreachable()` | `Opcode::Unreachable`, a terminator: `ud2` on x86-64, `brk #1` on aarch64 where it survives. Optimizing, a branch to it is dead and is removed |
| `__builtin_trap()` | A call to `abort` (gcc emits a trap instruction) |
| `__builtin_expect(expr, c)` | Its value is `expr`. `c` is evaluated for its side effects unless it is a literal |
| `__builtin_assume_aligned(ptr, align[, offset])` | Its value is `ptr`, with `ptr`'s own type (gcc's is `void *`). **Bug:** `align` and `offset` are discarded unevaluated, so `__builtin_assume_aligned(p, 16, k++)` leaves `k` unchanged where gcc increments it |

## Structure Layout

| Builtin | Description |
|---------|-------------|
| `__builtin_offsetof(type, member)`, `offsetof(type, member)` | Byte offset, as `unsigned long`; `ExprKind::OffsetOf`, folded by `constexpr::eval`. The member may be a chain (`field.sub`, `arr[i].field`) whose indices must be integer constants |

## Complex Numbers

| Builtin | Description |
|---------|-------------|
| `__builtin_complex(re, im)` | `ExprKind::BuiltinComplex`, of the complex type of `re`'s type. Usable in static initializers and at file scope (`<complex.h>`'s `I` and `CMPLX` macros use it), at every precision. Not checked: gcc requires two operands of one real floating type |
| `__builtin_creal`, `crealf`, `creall`, `cimag`, `cimagf`, `cimagl`, `conj`, `conjf`, `conjl` | Computed in place (above) |

Complex integer types (`_Complex int` and the rest, a GNU extension) are
supported as two integers laid end to end; multiply and divide are
open-coded, and divide uses Smith's method with truncating steps, exactly as
gcc does, so `(-9 + 38i) / (5 + 6i)` is `6 + 1i`. `_Complex __int128` is
32 bytes and travels in memory on both targets.

## Checked Arithmetic

C23 spells the generic three `ckd_add`, `ckd_sub`, `ckd_mul`. Each computes
the exact result, stores it wrapped to the destination type, and returns 1
if it did not fit; the store happens either way. All are
`ExprKind::CheckedArith`; mixed operand and result types, including
`__int128`, match gcc.

| Builtin | Description |
|---------|-------------|
| `__builtin_add_overflow(a, b, *r)`, `sub`, `mul` | Type-generic: the operands and `*r` may differ in type |
| `__builtin_add_overflow_p(a, b, v)`, `sub`, `mul` | The same question without storing. `v` names the result type by a value; it is evaluated, as in gcc, but unused |
| `__builtin_sadd_overflow`, `saddl`, `saddll`, `ssub*`, `smul*` | `int` / `long` / `long long` |
| `__builtin_uadd_overflow`, `uaddl`, `uaddll`, `usub*`, `umul*` | `unsigned int` / `unsigned long` / `unsigned long long` |

The fixed-type forms are parsed as the generic ones, so their arguments are
not converted to the named type first (and see Argument checking).

## Library Functions

A builtin of the same name as a library function is a call to that function
(`parse_library_builtin` strips `__builtin_`; `__builtin_trap` calls `abort`,
`__builtin_memcmp_eq` calls `memcmp`). The usual library rules apply --
`__builtin_pow` needs `-lm`. They exist so a translation unit may call one
without the header that declares it, as gcc allows and glibc's fortified
headers rely on. The call is `CalleeBinding::Library`: it reaches the
library's function, never an inline definition of the same name, so an
`always_inline` `extern inline` `strncpy` whose body is
`return __builtin_strncpy(...)` is not recursive.

How the callee is declared, and so what is checked, is described under
Argument checking. Return types are modelled (`chk_builtin_return_type`):
the string family returns `char *`, the allocators `void *`; a placeholder
prototype gets the libm parameter types from `libm_real_kind` (with `modf`,
`frexp` and `ldexp` special-cased), and the right count of fixed parameters
for the variadic ones, which matters on Apple arm64 where variadic arguments
go on the stack.

| Builtin | Notes |
|---------|-------|
| `__builtin_abort()`, `__builtin_exit(status)`, `__builtin_trap()` | `trap` calls `abort` |
| `__builtin_malloc`, `calloc`, `realloc`, `free` | |
| `__builtin_memcmp`, `memcmp_eq`, `bcmp`, `memchr`, `bzero` | `memcmp_eq` (equality only) calls `memcmp` |
| `__builtin_strlen`, `strcmp`, `strncmp`, `strcasecmp`, `strncasecmp` | |
| `__builtin_strcpy`, `strncpy`, `stpcpy`, `stpncpy`, `strcat`, `strncat`, `strdup`, `strndup` | |
| `__builtin_strchr`, `strrchr`, `index`, `rindex`, `strstr`, `strpbrk`, `strspn`, `strcspn` | |
| `__builtin_printf`, `sprintf`, `snprintf`, `fprintf`, `puts`, `putchar`, `fputs`, `fputc`, `fwrite` | |
| `__builtin_printf_unlocked`, `fprintf_unlocked`, `fputs_unlocked` | glibc defines none of these; a program using one supplies it (gcc.c-torture's `builtins/` tests do) |
| `__builtin_pow`, `fmod`, `atan2`, `hypot`, `fdim`, `remainder`, `nextafter` and `f`/`l` forms | |
| `__builtin_cbrt`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `asinh`, `acosh`, `atanh`, `exp`, `exp2`, `expm1`, `log`, `log2`, `log10`, `log1p`, `logb`, `tgamma`, `lgamma`, `erf`, `erfc` and `f`/`l` forms | |
| `__builtin_modf`, `frexp`, `ldexp` and `f`/`l` forms | The second parameter is a pointer or an `int` |
| `__builtin_ceill`, `floorl`, `truncl`, `roundl`, `rintl`, `nearbyintl`, `fminl`, `fmaxl`, `fmal` | The `long double` forms of functions computed in place |

### Calls the optimizer folds

A call to a function with a `known` row in `LIBRARY_BUILTINS` carries its
`LibFn` on the IR instruction (`known`), by either spelling where one exists,
and `ir/libcall_fold/` folds it once optimizing; `-fno-builtin[-NAME]` and an
incompatible declaration keep the bare name's call.

- `strlen`, `strnlen`, `strcmp`, `strncmp`, `memcmp`, `strchr`, `strrchr`,
  `index`, `rindex`, `memchr`, `strstr`, `strpbrk`, `strcspn`
  (`strings.rs`): computed where the arguments decide the result -- the
  bytes of a string literal or of a `const` `char` array defined here, and
  for `strlen`/`strnlen` a local array whose stores before the call are
  constant. `strcmp(p, "")` is the first byte of `p`; `strstr(p, "c")` is
  `strchr(p, 'c')`. `strnlen` has no `__builtin_` spelling.
- A `strncmp` or `memcmp` of the constant length 0 is 0 at every level
  (`fold_zero_length_compare`, in the parser).
- `strcpy`, `stpcpy`, `strncpy`, `strcat`, `strncat`, `sprintf`
  (`copies.rs`): with a source of known length (or a choice among strings
  of one length), a `memcpy` of that length and the terminator; `strcat`
  finds the end with `strlen`, `strncpy` pads with a `memset` up to 128
  bytes, and `sprintf` of a format with no conversion, or of `"%s"`, answers
  the length. An unused `stpcpy`, or `sprintf(d, "%s", s)`, of an unknown
  `s` is `strcpy(d, s)`.
- `printf`, `fprintf`, `vprintf`, `vfprintf`, `fputs`, their `_unlocked`
  forms and `__printf_chk`, `__vprintf_chk`, `__fprintf_chk`,
  `__vfprintf_chk` (`stdio.rs`), result unused: `printf("")` goes,
  `printf("x")` is `putchar('x')`, `printf("text\n")` and `printf("%s\n", s)`
  are `puts`, `printf("%c", c)` is `putchar(c)`; `fprintf(fp, "text")` and
  `fprintf(fp, "%s", s)` are `fputs`, `fprintf(fp, "%c", c)` is `fputc`;
  `fputs` of a known string is nothing, `fputc` or `fwrite`. The `v` forms
  fold only a format without `%`; the `_unlocked` forms are only ever
  dropped.

## Object Size and Fortification

| Builtin | Description |
|---------|-------------|
| `__builtin_object_size(ptr, type)` | Bytes left in the object `ptr` points into, folded **at parse time** (`parse_object_size_builtin`) from what the expression shows: an array, a string literal, `&lvalue`, member and constant-index chains, casts and constant pointer arithmetic. Unknown is `(size_t)-1` for types 0/1 and 0 for 2/3 |
| `__builtin___memcpy_chk`, `memmove_chk`, `mempcpy_chk`, `memset_chk` | Calls to `__memcpy_chk` etc. |
| `__builtin___strcpy_chk`, `stpcpy_chk`, `strncpy_chk`, `stpncpy_chk`, `strcat_chk`, `strncat_chk` | |
| `__builtin___printf_chk`, `fprintf_chk`, `sprintf_chk`, `snprintf_chk`, `vsprintf_chk`, `vsnprintf_chk` | |

With `_FORTIFY_SOURCE` set and `-O`, glibc's fortified wrappers compile and
emit `__*_chk` calls, but check nothing useful: a wrapper asks
`__builtin_object_size` of its own parameter, which at parse time is
unknown. Folding it after inlining is the remaining work; see the
`_FORTIFY_SOURCE` entry in `DECISIONS.md`.

## Atomic Builtins

### C11 (clang's `__c11_atomic_*`, behind `<stdatomic.h>`)

| Builtin | Description |
|---------|-------------|
| `__c11_atomic_init(ptr, val)` | A relaxed atomic store |
| `__c11_atomic_load(ptr, order)` | `Opcode::AtomicLoad` |
| `__c11_atomic_store(ptr, val, order)` | `Opcode::AtomicStore` |
| `__c11_atomic_exchange(ptr, val, order)` | `Opcode::AtomicSwap`; returns the old value |
| `__c11_atomic_compare_exchange_strong(ptr, exp, des, succ, fail)`, `_weak` | `Opcode::AtomicCas`; returns `_Bool`. `fail` is parsed and discarded; weak is implemented as strong |
| `__c11_atomic_fetch_add`, `sub`, `and`, `or`, `xor` | `Opcode::AtomicFetchAdd` etc.; return the old value |
| `__c11_atomic_thread_fence(order)` | `Opcode::Fence` |
| `__c11_atomic_signal_fence(order)` | The same `Opcode::Fence`: a hardware fence, stronger than the compiler barrier required |

`<stdatomic.h>` maps the standard names to these. `_Atomic` objects accessed
through ordinary operators are lowered to the same instructions.

### GNU (`__atomic_*`, `__sync_*`)

| Builtin | Description |
|---------|-------------|
| `__atomic_load_n(p, order)`, `__atomic_store_n(p, v, order)` | |
| `__atomic_exchange_n(p, v, order)` | Returns the old value |
| `__atomic_compare_exchange_n(p, expected, desired, weak, succ, fail)` | `expected` is a pointer, written back on failure; returns `int` (gcc: `bool`). `weak` only picks the node; both are strong |
| `__atomic_fetch_add/sub/and/or/xor/nand(p, v, order)` | Returns the value before |
| `__atomic_add/sub/and/or/xor/nand_fetch(p, v, order)` | Returns the value after |
| `__atomic_test_and_set(p, order)`, `__atomic_clear(p, order)` | An exchange of 1 compared with 0; a store of 0 |
| `__atomic_thread_fence(order)`, `__atomic_signal_fence(order)` | As the C11 fences |
| `__atomic_always_lock_free(size, p)`, `__atomic_is_lock_free(size, p)` | Answered by the parser from the constant size (`target::atomic_is_lock_free`: 1, 2, 4, 8); `p` is discarded |
| `__sync_fetch_and_add/sub/and/or/xor/nand(p, v, ...)` | Returns the value before |
| `__sync_add/sub/and/or/xor/nand_and_fetch(p, v, ...)` | Returns the value after |
| `__sync_bool_compare_and_swap(p, old, new)` | Whether the exchange happened; `old` is a value, not a pointer |
| `__sync_val_compare_and_swap(p, old, new)` | The previous value |
| `__sync_lock_test_and_set(p, v)`, `__sync_lock_release(p)` | A seq-cst exchange; a seq-cst store of 0 |
| `__sync_synchronize()` | A seq-cst fence |

The `__sync_*` forms accept and ignore gcc's trailing variable list. The
read-modify-write forms (`ExprKind::GnuAtomicRmw`) go through
`emit_atomic_rmw`, as `_Atomic` compound assignment does: a native
`AtomicFetch*` where one computes the operation exactly, otherwise a
compare-exchange loop -- always for `nand` (`~(old & val)`). `*_and_fetch`
re-applies the operation to the returned old value, so `v` is evaluated once.
`__GCC_HAVE_SYNC_COMPARE_AND_SWAP_{1,2,4,8}` is predefined.

### Memory orders

An order argument is evaluated, and recorded on the IR instruction only when
it is an integer literal after preprocessing (`__ATOMIC_*` and the
`memory_order_*` enumerators both are); anything else is seq-cst
(`eval_memory_order`). What the back ends then do with it:

- the GNU read-modify-write forms record seq-cst whatever was asked
  (TODO.md);
- fences honour it on both targets (`mfence`, `lfence`, `sfence` or nothing
  on x86-64; `dmb ish`, `ishld`, `ishst` or nothing on aarch64);
- x86-64 stores honour it (`xchg` for seq-cst, a plain `mov` otherwise);
  x86-64 loads are plain loads, correct for every order;
- aarch64 ignores it for loads (`ldar`), stores (`stlr`), exchange,
  compare-exchange and fetch-ops (`ldaxr`/`stlxr` loops).

A stronger order than requested is always correct, so this costs speed only:
`__atomic_load_n(p, __ATOMIC_RELAXED)` is an `ldar` on aarch64.

## Not Implemented

Their absence is silent and changes which branch a guarded header takes.

| Builtin | Consequence |
|---------|-------------|
| `__builtin_dynamic_object_size` | glibc's `_FORTIFY_SOURCE=3` falls back to `__builtin_object_size` |
| `__builtin_setjmp`, `__builtin_longjmp` | Use `setjmp`/`longjmp`, which are builtins (above) |
| `__builtin_strnlen`, `__builtin_vprintf`, `__builtin_vfprintf` | `strnlen`, `vprintf`, `vfprintf` are known by their bare names only |
| `__builtin_clear_padding` | Would have to walk a type to find its padding |
| `__builtin_issignaling` | `<math.h>` defines `issignaling` itself, so nothing fails to build |
| `__builtin_stack_save`, `__builtin_stack_restore` | c17 frees a VLA at the end of its block without them. The IR has `StackSave`/`StackRestore` for inlining, but no builtin reaches them |
| `__builtin_cexpi`, `__builtin_cpow` | Complex libm entry points gcc synthesizes; no header declares them |
| `__builtin_bswap128` | No 128-bit byte swap |

## Adding a Builtin

Every kind starts the same way.

1. **Name.** Add `(BUILTIN_FOO, "__builtin_foo", BUILTIN)` to
   `define_keywords!` in `kw.rs` (position does not matter; ids are looked
   up by spelling). A bare spelling the parser must match by id gets its own
   entry tagged `0`, so `__has_builtin` does not answer for it.
2. **`__has_builtin`.** Add the spelling to `SUPPORTED_BUILTINS` in
   `builtins.rs`. `test_supported_builtins_match_kw_tags` and
   `test_kw_builtin_tags_are_all_registered` fail until the two lists agree.
   A target-dependent builtin also needs a case in `available_on`.
3. **Parse.** Add an arm to the family function in `parse/builtin_expr.rs`
   that fits (`parse_checked_builtin`, `parse_float_builtin`,
   `parse_misc_builtin`, `parse_atomic_builtin`, ...), all reached from
   `parse_builtin_expr`. Use `expect_special`, `parse_assignment_expr` (one
   argument -- not `parse_expression`, which would eat the comma),
   `parse_type_name`, and `eval_const_expr` for arguments that must be
   constant. Check argument types here and report with `diag::error` /
   `diag::error_args` in gcc's wording (or `ParseError::new` if parsing
   cannot continue); convert arguments to their parameter types with
   `convert_operand`; build the node with `typed_expr`. A builtin of one
   argument with a prototype, like the bit builtins, is a row in
   `BIT_BUILTINS` (`parse/bit_builtin.rs`) instead, which checks and converts
   the argument as a call does. An argument you drop
   must keep its side effects: wrap it in `ExprKind::Comma` unless
   `is_literal_constant` says it has none.

### (a) Folded or rewritten in the parser

Return a literal (`ExprKind::IntLit`, `FloatLit`) or an existing node
(`__builtin_parity` builds `Popcount & 1`; `__sync_synchronize` builds a
`C11AtomicThreadFence`). Nothing past the parser changes. If the result must
be an integer constant expression or a static initializer and is not a
literal, teach `constexpr.rs` (`eval`, `eval_float`) the node. Floating
constants go in `FLOAT_CONSTANT_BUILTINS`.

### (b) A new IR opcode

1. **AST.** Add an `ExprKind` variant in `parse/ast.rs`, and to every match
   the compiler then flags: `Expr::operands` (`parse/ast.rs`),
   `extract_calls_from_expr` (`cflow.rs`), `extract_refs_from_expr`
   (`cxref.rs`), `is_pure_expr` and `linearize_expr` (`ir/linearize.rs`).
   Reusing an existing node avoids all of this.
2. **Linearize.** Add a case to `linearize_builtin` (or a `linearize_*`
   helper) that linearizes the operands, converts them if the parser did
   not, and emits `Instruction::new(Opcode::Foo)` with `with_target`,
   `with_src`, `with_type_and_size` (result type and width). If the operand
   type differs from the result's, set `src_typ`/`src_size`.
3. **Opcode.** Add the variant to `Opcode` in `ir/mod.rs`, its dump spelling
   in `Opcode::name`, and a row in the opcode table in `ir/README.md`. Then
   decide each predicate:
   - `is_terminator` if control does not fall through;
   - `may_access_memory` if it reads or writes memory, and then also
     `has_side_effects` (validator invariant I5) unless it is a pure load;
   - `has_side_effects` if it must not be deleted when its result is unused;
   - `Instruction::is_memory_barrier` if memory operations may not move
     across it (I2 requires it also be in `has_side_effects`);
   - `reads_another_type` if its operand type differs from its result type
     (I10 then requires `src_typ`/`src_size` on every instance);
   - `is_libm` if it computes a libm function a target may lack (it must
     then carry the library name via `with_func`, read back by
     `Instruction::library_callee`);
   - `is_atomic` for an atomic memory operation (the allocator reserves
     scratch registers for these).

   A placeholder resolved before code generation (like `ConstantP`, answered
   in `sccp` and `ir::lower`) must be added to `check_no_placeholders` (I4).
4. **Optimizer.** Nothing folds an opcode it does not know: `sccp::transfer`
   treats it as overdefined. If it can fold, add it to `constfold.rs`
   (`eval_unop`, `eval_funop`, `eval_fbinop`, `eval_fcvt`) and to the
   opcode list in `sccp::transfer`; algebraic simplifications go in
   `instcombine::try_simplify`. A memory-touching opcode is opaque to
   `effects::insn_effect` (`MemEffect::Unknown`), `escape.rs`, `loadfwd.rs`
   and `dse.rs` until taught otherwise -- conservative, but teach them if it
   matters (`Memcpy`/`Memset`/`Memmove` are the examples).
5. **Target mapping.** If a target has no instruction for it: a libm opcode
   gets a case in each `ArchMapper::computes_in_place`
   (`arch/x86_64/mapping.rs`, `arch/aarch64/mapping.rs`), and
   `call_library_fallbacks` turns it into a call after the optimizer; any
   other expansion goes in that target's `ArchMapper::map_insn`.
6. **Back ends.** Add an arm to `emit_insn` in `arch/x86_64/codegen.rs` and
   `arch/aarch64/codegen.rs`, with the emitter in that target's
   `features.rs` (or `float.rs`, `atomic.rs`). Both `emit_insn`s end in
   `_ => {}`, so a missing arm silently emits nothing. Fixed registers the
   lowering clobbers go in `opcode_constraints` / `opcode_clobbers_r10_r11`
   (x86-64) or `get_constraint_info_aarch64` /
   `opcode_clobbers_aarch64_scratches`; a lowering that calls a function
   goes in `is_call_like_x86_64` / `is_call_like_aarch64`. Name any library
   callee through `Linearizer::library_function_name`, never a literal in
   the back end, so an asm label on the declaration is honoured. On aarch64,
   out-of-range immediates and offsets are legalized in
   `arch/aarch64/legalize.rs`, not at the call site.

### (c) A library function

1. **Name.** The `__builtin_` spelling tagged `BUILTIN`, plus the bare name
   tagged `0` (named if a table refers to it).
2. **Called.** Add the `BUILTIN_` id to `is_library_builtin` in
   `parse/builtin_expr.rs`. Then give it a prototype, best first:
   - a `known(...)` row in `LIBRARY_BUILTINS` (`parse/library_builtin.rs`),
     which declares it with the real prototype, checks calls, and tags them
     with a new `LibFn` variant (`parse/ast.rs`; add it to `LibFn::only_reads`
     if it writes no memory);
   - a libm function: its stem in `libm_real_kind` and, for two or three
     parameters, `libm_arity`;
   - otherwise: its return type in `chk_builtin_return_type` and its fixed
     parameter count in `declare_chk_builtin` (placeholder, unchecked).
3. **Folded.** For a `LibFn`, add the fold to the family module in
   `ir/libcall_fold/` and dispatch it from `fold` in
   `ir/libcall_fold/mod.rs`. A fold that emits a call to a function no fold
   called before adds that name to `FOLD_CALLEES` (`ir/mod.rs`).
4. **Computed in place.** Instead of `known`, an `entry(...)` row with a new
   `InlineLibraryFn` variant, and decide its `arity`, `has_side_effects`,
   `yields_to_a_definition` and `narrows_exactly`; its level policy in
   `LibraryCallPolicy::in_place`; its computation in
   `compute_library_call` (`ir/linearize.rs`), usually a new opcode as in
   (b); and its constant fold in `constexpr.rs` (the `InlineLibraryCall`
   arms) so a static initializer accepts it.

### Tests

Per `cc/CLAUDE.md`, every change has unit and end-to-end tests.

- **Parser**: `parse/test_parser.rs` (e.g. `test_builtin_nan`), for the node,
  type and diagnostics. `parse/library_builtin.rs` has table invariants
  (`every_entry_takes_what_its_lowering_consumes`).
- **`__has_builtin`**: `token/test_preprocess.rs`, and
  `tests/builtins/has_feature.rs`.
- **IR**: when the IR changes, `ir/test_linearize.rs` for the emitted opcode
  (`test_frame_address_emits_opcode`, `test_memory_builtins_are_their_opcodes`)
  and unit tests in the pass that folds or simplifies it (`constfold.rs`,
  `sccp.rs`, `instcombine.rs`, `libcall_fold/`); a target-mapping change
  tests in `arch/*/mapping.rs`.
- **End to end**: `tests/builtins/<topic>.rs`. Use
  `compile_and_run_everywhere`, which runs at `-O0` and `-O2` on the host and
  under qemu on aarch64 when the cross toolchain is present -- `compile_and_run`
  alone never exercises the other back end, and `compile_and_run_optimized`
  is only `-O1`. Use `compile_expect_error` / `compile_expect_warning` for
  diagnostics, and compare against gcc before trusting an expected value.
