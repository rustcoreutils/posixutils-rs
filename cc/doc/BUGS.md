# pcc Known Bugs

This file documents known bugs where valid C99 code fails to compile.

## Parse Errors on Valid C99 Code

### Compound Literals

**Status**: Parse error

**Valid C99 Code**:
```c
struct Point { int x, y; };
struct Point p = (struct Point){1, 2};  // Compound literal
```

**Error**:
```
parse error: unexpected token in expression
```

**C99 Reference**: 6.5.2.5

---

### Array Parameter with `static`

**Status**: Parse error

**Valid C99 Code**:
```c
void f(int a[static 10]);  // Guaranteed at least 10 elements
```

**Error**:
```
parse error: expected ']'
```

**C99 Reference**: 6.7.5.3

---

### Array Parameter with Qualifiers

**Status**: Parse error

**Valid C99 Code**:
```c
void f(int a[const 10]);   // const-qualified array parameter
void f(int a[restrict]);   // restrict-qualified
```

**Error**:
```
parse error: expected ']'
```

**C99 Reference**: 6.7.5.3

---

### `[*]` in Function Prototypes

**Status**: Parse error

**Valid C99 Code**:
```c
void f(int n, int arr[*]);  // VLA of unspecified size
```

**Error**:
```
parse error: unexpected token in expression
```

**C99 Reference**: 6.7.5.2

---

## Semantic/Codegen Bugs

### VLAs Allocate Fixed Storage

**Status**: Wrong code generated

**Valid C99 Code**:
```c
void f(int n) {
    int arr[n];  // Should allocate n*sizeof(int) bytes at runtime
}
```

**Expected Behavior**: Stack allocation should be computed at runtime based on `n`.

**Actual Behavior**: Fixed-size allocation; runtime size is ignored.

**C99 Reference**: 6.7.5.2
