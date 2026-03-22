# Attributes

This document describes attribute support in pcc.

## Table of Contents

- [Overview](#overview)
- [Supported Attributes](#supported-attributes)
- [Compile-Time Attribute Queries](#compile-time-attribute-queries)
- [setjmp/longjmp Integration](#setjmplongjmp-integration)
- [References](#references)

## Overview

pcc supports GNU-style attributes that modify function, variable, and type behavior. Attributes can be specified using:

1. **GNU-style attributes**: `__attribute__((name))` or `__attribute__((__name__))`
2. **C11 keyword**: `_Noreturn`

All attribute names accept the double-underscore variant (`__name__`) as an alternative spelling.

## Supported Attributes

Attributes fall into three categories based on implementation depth:

### Fully implemented (affects codegen)

| Attribute | Applies to | Effect |
|-----------|-----------|--------|
| `noreturn` | Functions | Emits trap after call; enables DCE |
| `packed` | Structs/unions | Removes padding from layout |
| `sysv_abi` | Functions | Forces System V AMD64 calling convention |
| `ms_abi` | Functions | Forces Win64 calling convention |

### Parsed and accepted (no semantic effect)

These are parsed by `__attribute__((...))`, reported by `__has_attribute()`, but silently ignored by codegen. This is sufficient for compatibility with headers that use them.

| Attribute | Description |
|-----------|-------------|
| `unused` | Suppress unused warnings |
| `aligned` | Alignment specification |
| `deprecated` | Mark as deprecated |
| `weak` | Weak symbol linkage |
| `section` | Place in named section |
| `visibility` | Symbol visibility (default, hidden, protected) |
| `constructor` | Run before main |
| `destructor` | Run after main |
| `used` | Prevent dead-stripping |
| `noinline` | Prevent inlining |
| `always_inline` | Force inlining |
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
