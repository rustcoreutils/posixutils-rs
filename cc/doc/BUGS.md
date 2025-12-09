# pcc Known Bugs

This file documents known bugs where valid C99 code fails to compile.

## Parse Errors on Valid C99 Code

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
