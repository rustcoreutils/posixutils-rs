# Attributes

This document describes attribute support in c17.

## Table of Contents

- [Overview](#overview)
- [Supported Attributes](#supported-attributes)
- [Compile-Time Attribute Queries](#compile-time-attribute-queries)
- [setjmp/longjmp Integration](#setjmplongjmp-integration)
- [References](#references)

## Overview

c17 supports GNU-style attributes that modify function, variable, and type behavior. Attributes can be specified using:

1. **GNU-style attributes**: `__attribute__((name))` or `__attribute__((__name__))`
2. **C11 keyword**: `_Noreturn`

All attribute names accept the double-underscore variant (`__name__`) as an alternative spelling.

### Where an attribute is written

An attribute applies to a declarator or to a whole declaration depending on
where it appears, and c17 follows gcc:

```c
int p, q __attribute__((aligned(64))), r;   /* q alone */
int __attribute__((aligned(64))) u, v;      /* u and v */
_Alignas(64) int w, x;                      /* w and x */
```

This matters for the per-symbol attributes -- `weak`, `section`, `visibility`
and `used` -- where naming the wrong symbol is an ABI change rather than a
missed optimization.

## Supported Attributes

Attributes fall into three categories based on implementation depth:

### Fully implemented (affects codegen)

| Attribute | Applies to | Effect |
|-----------|-----------|--------|
| `noreturn` | Functions | Emits trap after call; enables DCE |
| `packed` | Structs/unions | Removes padding from layout. Shares one alignment-cap rule with `#pragma pack`, which caps at `n` rather than 1; where both apply the tighter wins |
| `aligned` | Types, variables | Raises alignment; `.align` on a variable, layout and `_Alignof` on a type. Verified to match gcc |
| `sysv_abi` | Functions | Forces System V AMD64 calling convention |
| `ms_abi` | Functions | Forces Win64 calling convention |
| `constructor` | Functions | Runs before `main`, via `.init_array` (`__DATA,__mod_init_func` on Mach-O). An optional priority orders it against the other constructors; ELF encodes it in the section name, Mach-O has no equivalent and ignores it |
| `destructor` | Functions | Runs after `main` returns or on `exit`, via `.fini_array` / `__DATA,__mod_term_func`. Priorities as for `constructor` |
| `noinline` | Functions | The inliner leaves the function alone, whatever its size |
| `always_inline` | Functions | Inlined at every call site regardless of size, and at `-O0` too. `noinline` outranks it, as in gcc |
| `weak` | Functions, variables | `.weak` rather than `.globl`: another definition wins, and an unresolved reference is null rather than a link error. Honoured on a *declaration* with no definition too, which is the idiom the attribute exists for |
| `visibility` | Functions, variables | ELF `.hidden` / `.protected` / `.internal`; "default" is the *absence* of a directive, not a `.default` pseudo-op. Mach-O has only `.private_extern`, used for "hidden" and "internal". A zero-initialized variable leaves the `.comm` fast path rather than lose it |
| `section` | Functions, variables | Places the symbol in the named section, ahead of every other rule -- including the zero-initialized fast path, since `.comm` would let the linker choose. ELF flags follow the contents: `"ax"` for code, `"aw"` for mutable data, `"a"` for read-only data |

### Accepted but with no effect, and the program can tell

One entry, and it is a technicality rather than a gap:

| Attribute | Why the program cannot currently tell |
|-----------|---------------------------------------|
| `used` | c17 never prunes an unreferenced static, so keeping one alive is already what happens. gcc drops it at `-O2` and c17 does not. If dead-global elimination is ever added, `used` has to be consulted then, or this becomes a real divergence. See #C59 in `cc/audit.md` |

An attribute the compiler does not recognise is no longer dropped in silence:
it is a warning, suppressible with `-Wno-attributes`. `vector_size` is refused
outright, because leaving the type scalar cannot produce a correct program, and
`mode` warns -- it changes the type too, but glibc declares `register_t` with
it, so refusing would reject nearly every program. `const` is recognised in
both its bare and underscored spellings, as gcc accepts either.

### Parsed and accepted (no semantic effect)

These are parsed by `__attribute__((...))`, reported by `__has_attribute()`, but silently ignored by codegen. This is sufficient for compatibility with headers that use them.

| Attribute | Description |
|-----------|-------------|
| `unused` | Suppress unused warnings |
| `deprecated` | Mark as deprecated |
| `hot` | Optimize for speed |
| `cold` | Optimize for size |
| `warn_unused_result` | Warn if return value ignored |
| `format` | Printf/scanf format checking |
| `fallthrough` | Suppress switch fallthrough warning |
| `nonstring` | Mark char array as non-NUL-terminated |
| `malloc` | Mark as malloc-like allocator |
| `pure` | No side effects (reads memory) |
| `sentinel` | Require NULL sentinel argument |
| `no_sanitize_memory` | Disable memory sanitizer |
| `no_sanitize_address` | Disable address sanitizer |
| `no_sanitize_thread` | Disable thread sanitizer |

### `noreturn` details

Indicates that a function never returns to its caller. This enables the compiler to:
- Eliminate unreachable code after calls to the function
- Skip generating return sequences in the function (if defined)

#### Syntax

```c
// GNU-style attribute
void exit(int status) __attribute__((noreturn));
void abort(void) __attribute__((__noreturn__));

// C11 keyword
_Noreturn void my_exit(int code);
```

#### Implementation Notes

- The `noreturn` attribute is stored in the function type
- When calling a noreturn function, the compiler emits a trap instruction (ud2 on x86-64, brk on AArch64) after the call
- The Dead Code Elimination (DCE) pass treats calls to noreturn functions as roots

#### Common noreturn Functions

Standard library functions that are typically declared noreturn:
- `exit()`, `_Exit()`, `quick_exit()` - process termination
- `abort()` - abnormal termination
- `longjmp()`, `_longjmp()`, `siglongjmp()` - non-local jumps
- `pthread_exit()` - thread termination

### `packed` details

Removes inter-field padding from struct/union layout. Parsed from `__attribute__((packed))` on struct/union definitions. Applied during `compute_struct_layout()`.

```c
struct __attribute__((packed)) Example {
    char a;    // offset 0
    int b;     // offset 1 (not 4)
    short c;   // offset 5 (not 8)
};  // sizeof = 7 (not 12)
```

## Compile-Time Attribute Queries

### `__has_attribute(name)`

A preprocessor operator that returns 1 if the specified attribute is recognized, 0 otherwise. This covers all attributes in both tables above (fully implemented and parsed-only).

#### Syntax

```c
#if __has_attribute(noreturn)
#define NORETURN __attribute__((noreturn))
#else
#define NORETURN
#endif

NORETURN void my_exit(int code);
```

## setjmp/longjmp Integration

The `noreturn` attribute is essential for proper `setjmp`/`longjmp` semantics, as `longjmp` never returns to its caller.

```c
// Typical setjmp.h declarations
typedef int jmp_buf[64];  // Platform-specific size

extern int setjmp(jmp_buf env);
extern void longjmp(jmp_buf env, int val) __attribute__((noreturn));

// Alternative: C11 style
extern _Noreturn void longjmp(jmp_buf env, int val);
```

### How setjmp/longjmp Work

1. `setjmp(env)` saves the current execution context and returns 0
2. `longjmp(env, val)` restores the saved context, causing `setjmp` to return `val`
3. If `val` is 0, `setjmp` returns 1 instead (per C standard)
4. `longjmp` never returns to its caller - it jumps to the `setjmp` location

```c
#include <setjmp.h>

jmp_buf env;

void do_work(void) {
    // ... some work ...
    if (error_occurred) {
        longjmp(env, 1);  // Jump back to setjmp, never returns
    }
    // ... more work ...
}

int main(void) {
    if (setjmp(env) == 0) {
        // First time through
        do_work();
    } else {
        // Returned via longjmp
        printf("Error occurred\n");
    }
    return 0;
}
```

## References

- [GCC Function Attributes](https://gcc.gnu.org/onlinedocs/gcc/Function-Attributes.html)
- [Clang Attributes](https://clang.llvm.org/docs/AttributeReference.html)
- [C11 _Noreturn specifier](https://en.cppreference.com/w/c/language/_Noreturn)
- [setjmp/longjmp](https://en.cppreference.com/w/c/program/setjmp)
