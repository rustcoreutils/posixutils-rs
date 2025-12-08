# Function Attributes

This document describes the function attribute support in pcc.

## Table of Contents

- [Overview](#overview)
- [Supported Attributes](#supported-attributes)
  - [`noreturn`](#noreturn)
- [Compile-Time Attribute Queries](#compile-time-attribute-queries)
  - [`__has_attribute(name)`](#__has_attributename)
- [setjmp/longjmp Integration](#setjmplongjmp-integration)
- [Future Attribute Support](#future-attribute-support)
- [References](#references)

## Overview

pcc supports function attributes that modify function behavior and enable compiler optimizations. Attributes can be specified using:

1. **GNU-style attributes**: `__attribute__((name))`
2. **C11 keyword**: `_Noreturn`

## Supported Attributes

### `noreturn`

Indicates that a function never returns to its caller. This enables the compiler to:
- Eliminate unreachable code after calls to the function
- Skip generating return sequences in the function (if defined)
- Warn about code paths that unexpectedly return

#### Syntax

```c
// GNU-style attribute
void exit(int status) __attribute__((noreturn));
void abort(void) __attribute__((__noreturn__));

// C11 keyword
_Noreturn void my_exit(int code);

// Combined (for maximum portability)
_Noreturn void fatal_error(const char *msg) __attribute__((noreturn));
```

#### Examples

**Declaring noreturn functions:**
```c
// External declarations (libc functions)
extern void exit(int status) __attribute__((noreturn));
extern void abort(void) __attribute__((noreturn));
extern void _Exit(int status) __attribute__((noreturn));

// C11 style
extern _Noreturn void longjmp(jmp_buf env, int val);
```

**Using noreturn for error handling:**
```c
_Noreturn void fatal(const char *msg) {
    fprintf(stderr, "Fatal: %s\n", msg);
    exit(1);
}

int process(int x) {
    if (x < 0) {
        fatal("negative value");
        // No return statement needed here - compiler knows fatal() doesn't return
    }
    return x * 2;
}
```

**Dead code elimination:**
```c
void example(int x) {
    if (x == 0) {
        exit(1);
        printf("unreachable\n");  // This code is dead - never executed
    }
}
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
- `execve()` and `exec*()` family (on success)

## Compile-Time Attribute Queries

### `__has_attribute(name)`

A preprocessor operator that returns 1 if the specified attribute is supported, 0 otherwise.

#### Syntax

```c
#if __has_attribute(noreturn)
    // noreturn attribute is available
#endif
```

#### Supported Attribute Names

| Name | Supported | Description |
|------|-----------|-------------|
| `noreturn` | Yes | Function never returns |
| `__noreturn__` | Yes | Alternative spelling for noreturn |
| `unused` | No | Suppress unused warnings |
| `packed` | No | Struct packing |
| `aligned` | No | Alignment specification |

#### Usage Patterns

**Conditional attribute usage:**
```c
#if __has_attribute(noreturn)
#define NORETURN __attribute__((noreturn))
#else
#define NORETURN
#endif

NORETURN void my_exit(int code);
```

**Portable header definitions:**
```c
// Works with pcc, GCC, and Clang
#if defined(__has_attribute)
#  if __has_attribute(noreturn)
#    define ATTR_NORETURN __attribute__((noreturn))
#  endif
#endif
#ifndef ATTR_NORETURN
#  define ATTR_NORETURN
#endif
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

## Future Attribute Support

The following attributes are planned but not yet implemented:

| Attribute | Description |
|-----------|-------------|
| `unused` | Suppress unused variable/parameter warnings |
| `packed` | Remove padding from struct/union |
| `aligned(n)` | Specify alignment requirement |
| `deprecated` | Mark as deprecated with optional message |
| `format` | Printf/scanf format string checking |
| `visibility` | Symbol visibility (default, hidden, protected) |

## References

- [GCC Function Attributes](https://gcc.gnu.org/onlinedocs/gcc/Function-Attributes.html)
- [Clang Attributes](https://clang.llvm.org/docs/AttributeReference.html)
- [C11 _Noreturn specifier](https://en.cppreference.com/w/c/language/_Noreturn)
- [setjmp/longjmp](https://en.cppreference.com/w/c/program/setjmp)
