# Compiler Builtins

This document describes the compiler builtin functions and types supported by pcc.

## Variadic Function Support

These builtins provide C99-compliant variadic function support, matching GCC and Clang behavior.

### `__builtin_va_list`

**Type**: Platform-specific variadic argument list type.

**Platform sizes**:
- x86-64 (System V ABI): 24 bytes (struct with gp_offset, fp_offset, overflow_arg_area, reg_save_area)
- aarch64-macos (Apple ARM64): 8 bytes (simple char* pointer)
- aarch64-linux (AAPCS64): 32 bytes (struct with stack, gr_top, vr_top, gr_offs, vr_offs)

**Usage**:
```c
__builtin_va_list ap;
```

### `__builtin_va_start(ap, last_param)`

**Purpose**: Initialize a va_list to point to the first variadic argument.

**Parameters**:
- `ap`: A variable of type `__builtin_va_list`
- `last_param`: The name of the last fixed parameter before the `...`

**Usage**:
```c
void my_printf(const char *fmt, ...) {
    __builtin_va_list ap;
    __builtin_va_start(ap, fmt);
    // ... use ap ...
    __builtin_va_end(ap);
}
```

### `__builtin_va_arg(ap, type)`

**Purpose**: Retrieve the next variadic argument of the specified type and advance the va_list.

**Parameters**:
- `ap`: A variable of type `__builtin_va_list`
- `type`: The type of the argument to retrieve (e.g., `int`, `long`, `char*`)

**Returns**: The next argument, interpreted as the specified type.

**Usage**:
```c
int value = __builtin_va_arg(ap, int);
char *str = __builtin_va_arg(ap, char*);
```

### `__builtin_va_end(ap)`

**Purpose**: Clean up a va_list after use. Required for portability, though it's a no-op on most platforms.

**Parameters**:
- `ap`: A variable of type `__builtin_va_list`

**Usage**:
```c
__builtin_va_end(ap);
```

### `__builtin_va_copy(dest, src)`

**Purpose**: Copy one va_list to another. The copy can be used independently of the original.

**Parameters**:
- `dest`: Destination `__builtin_va_list` (uninitialized)
- `src`: Source `__builtin_va_list` (previously initialized with va_start)

**Usage**:
```c
__builtin_va_list ap, ap_copy;
__builtin_va_start(ap, last_param);
__builtin_va_copy(ap_copy, ap);
// ap and ap_copy can now be used independently
__builtin_va_end(ap);
__builtin_va_end(ap_copy);
```

## Standard Header Compatibility

The system `<stdarg.h>` header typically defines macros that expand to these builtins:

```c
typedef __builtin_va_list va_list;
#define va_start(ap, param) __builtin_va_start(ap, param)
#define va_arg(ap, type)    __builtin_va_arg(ap, type)
#define va_end(ap)          __builtin_va_end(ap)
#define va_copy(dest, src)  __builtin_va_copy(dest, src)
```

This means code using the standard `va_list`, `va_start`, `va_arg`, `va_end`, and `va_copy` will work correctly once preprocessor support for `#include` is available.

## Implementation Notes

### Caller Side (Variadic Function Calls)

When calling a variadic function, pcc:
1. Passes fixed arguments normally (in registers per ABI)
2. Pushes variadic arguments onto the stack in reverse order
3. This ensures variadic arguments are accessible via simple stack pointer offsets

### Callee Side (Variadic Function Definitions)

When implementing a variadic function, pcc:
1. `va_start` initializes the overflow_arg_area to point to stack arguments (rbp+16 on x86-64)
2. `va_arg` reads from the overflow_arg_area and advances the pointer
3. `va_copy` performs a byte-for-byte copy of the va_list structure
4. `va_end` is currently a no-op

### Limitations

- Register save area is not currently implemented; all variadic arguments are passed on the stack
- This is ABI-compatible for calling external functions but may be less efficient for register-heavy variadic calls

## Byte-Swapping Builtins

These builtins reverse the byte order of integers, commonly used for endianness conversion between big-endian (network byte order) and little-endian (host byte order on x86/ARM).

### `__builtin_bswap16(x)`

**Signature**: `uint16_t __builtin_bswap16(uint16_t x)`

**Purpose**: Returns `x` with the order of bytes reversed.

**Example**: `0xABCD` becomes `0xCDAB`

**Usage**:
```c
unsigned short val = 0xABCD;
unsigned short swapped = __builtin_bswap16(val);  // 0xCDAB
```

**Implementation**:
- x86-64: Uses `ror $8, %ax` (rotate right 8 bits)
- AArch64: Uses `rev16` instruction

### `__builtin_bswap32(x)`

**Signature**: `uint32_t __builtin_bswap32(uint32_t x)`

**Purpose**: Returns `x` with the order of bytes reversed.

**Example**: `0x12345678` becomes `0x78563412`

**Usage**:
```c
unsigned int val = 0x12345678;
unsigned int swapped = __builtin_bswap32(val);  // 0x78563412
```

**Implementation**:
- x86-64: Uses native `bswap %eax` instruction
- AArch64: Uses `rev` instruction with 32-bit register

### `__builtin_bswap64(x)`

**Signature**: `uint64_t __builtin_bswap64(uint64_t x)`

**Purpose**: Returns `x` with the order of bytes reversed.

**Example**: `0x0102030405060708` becomes `0x0807060504030201`

**Usage**:
```c
unsigned long long val = 0x0102030405060708ULL;
unsigned long long swapped = __builtin_bswap64(val);  // 0x0807060504030201ULL
```

**Implementation**:
- x86-64: Uses native `bswap %rax` instruction
- AArch64: Uses `rev` instruction with 64-bit register

### Common Use Cases

**Network byte order conversion**:
```c
// Convert network byte order (big-endian) to host byte order (little-endian)
unsigned int ip_addr_host = __builtin_bswap32(ip_addr_network);

// Convert 16-bit port number
unsigned short port_host = __builtin_bswap16(port_network);
```

**Binary file format parsing**:
```c
// Read a big-endian 32-bit value from a file
unsigned int val = __builtin_bswap32(*(unsigned int *)buffer);
```

### Standard Header Compatibility

These builtins are compatible with GCC and Clang. The `<byteswap.h>` header (on Linux/glibc) typically provides macros that may expand to these builtins:

```c
#define bswap_16(x) __builtin_bswap16(x)
#define bswap_32(x) __builtin_bswap32(x)
#define bswap_64(x) __builtin_bswap64(x)
```

## Compile-Time Introspection

### `__builtin_constant_p(expr)`

**Signature**: `int __builtin_constant_p(expr)`

**Purpose**: Determines if an expression is known to be constant at compile time. Returns 1 if the expression is a constant, 0 otherwise.

**Parameters**:
- `expr`: Any C expression to test for constant-ness

**Returns**: Integer 1 if the expression is a compile-time constant, 0 otherwise.

**Important Notes**:
- The expression is **not evaluated** - side effects are discarded
- A return of 0 does not necessarily mean the expression isn't constant, only that the compiler cannot prove it's constant
- The result is itself a compile-time constant, allowing use in `#if` preprocessor directives (when supported) and static initializers

**Usage**:
```c
// Basic constant detection
if (__builtin_constant_p(42)) {
    // This branch is taken - 42 is constant
}

// Variable is not constant
int x = 10;
if (__builtin_constant_p(x)) {
    // This branch is NOT taken - x is a variable
}

// Constant expressions are detected
if (__builtin_constant_p(10 + 20 * 3)) {
    // This branch is taken - arithmetic on constants
}

// Common optimization pattern
#define my_abs(x) \
    (__builtin_constant_p(x) ? ((x) < 0 ? -(x) : (x)) : runtime_abs(x))
```

**What Counts as Constant**:
- Integer literals: `42`, `0xFF`
- Character literals: `'a'`, `'\n'`
- Arithmetic on constants: `10 + 20`, `3 * 4`
- Comparison on constants: `5 < 10`
- Logical operations: `1 && 1`, `0 || 1`
- Bitwise operations: `0xFF & 0x0F`
- Shift operations: `1 << 4`
- Conditional expressions with constant condition: `1 ? 100 : 200`
- Enum constants

**What Is NOT Constant**:
- Variables (even if initialized with a constant)
- Pointers to variables
- Function calls
- Array subscripts with variable indices

## Stack Allocation

### `__builtin_alloca(size)`

**Signature**: `void *__builtin_alloca(size_t size)`

**Purpose**: Allocates `size` bytes of memory on the stack frame of the calling function. The allocated memory is automatically freed when the function returns.

**Parameters**:
- `size`: The number of bytes to allocate

**Returns**: A `void*` pointer to the allocated memory, suitably aligned for any type.

**Important Notes**:
- The memory is allocated on the **stack**, not the heap
- Memory is automatically freed when the function **returns** (not when it goes out of scope)
- Do **not** return a pointer to alloca'd memory from a function - it becomes invalid after return
- Large allocations may cause stack overflow
- The allocation size does not need to be a compile-time constant

**Usage**:
```c
void process_data(int n) {
    // Allocate n integers on the stack
    int *arr = __builtin_alloca(n * sizeof(int));

    for (int i = 0; i < n; i++) {
        arr[i] = i * 2;
    }

    // arr is automatically freed when process_data returns
}

void example(void) {
    // Variable-length buffer without heap allocation
    char *buffer = __builtin_alloca(256);
    // Use buffer...
    // No need to free - automatically cleaned up on return
}
```

**Common Use Cases**:
- Variable-length temporary buffers when size is known at runtime
- Avoiding heap allocation overhead for small, short-lived allocations
- Implementing variable-length arrays (VLAs) internally

**Comparison with malloc**:
| Feature | `__builtin_alloca` | `malloc` |
|---------|-------------------|----------|
| Allocation location | Stack | Heap |
| Deallocation | Automatic on return | Manual with `free()` |
| Speed | Very fast | Slower |
| Size limit | Limited by stack size | Limited by heap/RAM |
| Persistence | Until function returns | Until explicitly freed |

**Implementation**:
- x86-64: Adjusts the stack pointer (`rsp`) and returns the new address
- AArch64: Adjusts the stack pointer (`sp`) and returns the new address

**Standard Header Compatibility**:
The `<alloca.h>` header typically provides:
```c
#define alloca(size) __builtin_alloca(size)
```

## Type Introspection

### `__builtin_types_compatible_p(type1, type2)`

**Signature**: `int __builtin_types_compatible_p(type1, type2)`

**Purpose**: Determines if two types are compatible, ignoring top-level type qualifiers (const, volatile, restrict). Returns 1 if the types are considered compatible, 0 otherwise.

**Parameters**:
- `type1`: A C type name (e.g., `int`, `char*`, `struct Point`)
- `type2`: A C type name to compare against

**Returns**: Integer 1 if the types are compatible, 0 otherwise.

**Important Notes**:
- Top-level qualifiers (`const`, `volatile`, `restrict`) are **ignored** when comparing types
- Signedness **matters**: `int` and `unsigned int` are NOT compatible
- Different struct/union types are NOT compatible, even if they have identical layouts
- The result is a compile-time constant

**Usage**:
```c
// Basic type comparison
if (__builtin_types_compatible_p(int, int)) {
    // This branch is taken - same types
}

// Different types
if (__builtin_types_compatible_p(int, long)) {
    // NOT taken - different types
}

// Qualifiers are ignored
if (__builtin_types_compatible_p(const int, int)) {
    // This branch IS taken - qualifiers ignored
}

if (__builtin_types_compatible_p(volatile int, int)) {
    // This branch IS taken
}

// Signedness matters
if (__builtin_types_compatible_p(unsigned int, int)) {
    // NOT taken - different signedness
}

// Pointer types
if (__builtin_types_compatible_p(int*, int*)) {
    // This branch is taken
}

if (__builtin_types_compatible_p(int*, char*)) {
    // NOT taken - different base types
}
```

**What Types Are Compatible**:
- Same basic types: `int` and `int`, `char` and `char`
- Same pointer types: `int*` and `int*`
- Same struct/union types: `struct Point` and `struct Point`
- Types differing only in qualifiers: `const int` and `int`

**What Types Are NOT Compatible**:
- Different basic types: `int` and `long`, `char` and `int`
- Different signedness: `int` and `unsigned int`
- Different pointer base types: `int*` and `char*`
- Different struct/union types (even with same layout): `struct Point` and `struct Size`
- `void*` and other pointer types: `void*` and `int*`

**Common Use Cases**:

**Type-generic macros**:
```c
#define process(x) \
    (__builtin_types_compatible_p(typeof(x), int) ? process_int(x) : \
     __builtin_types_compatible_p(typeof(x), float) ? process_float(x) : \
     process_generic(x))
```

**Static assertions**:
```c
#define STATIC_ASSERT_SAME_TYPE(a, b) \
    _Static_assert(__builtin_types_compatible_p(typeof(a), typeof(b)), \
                   "Types must be compatible")
```

**Standard Header Compatibility**:
This builtin is compatible with GCC and Clang. It is commonly used in conjunction with `typeof()` for type-generic programming.

## Count Trailing Zeros Builtins

These builtins count the number of trailing zero bits in an integer value.

### `__builtin_ctz(x)`

**Signature**: `int __builtin_ctz(unsigned int x)`

**Purpose**: Returns the number of trailing 0-bits in `x`, starting from the least significant bit position. If `x` is 0, the result is undefined.

**Parameters**:
- `x`: An unsigned int to count trailing zeros in

**Returns**: The count of trailing zero bits (0 to 31).

**Example**: `__builtin_ctz(8)` returns `3` because 8 = 0b1000 has 3 trailing zeros.

**Usage**:
```c
unsigned int val = 40;  // 0b101000
int zeros = __builtin_ctz(val);  // Returns 3
```

**Implementation**:
- x86-64: Uses `bsf` (bit scan forward) instruction
- AArch64: Uses `rbit` followed by `clz` (reverse bits, then count leading zeros)

### `__builtin_ctzl(x)`

**Signature**: `int __builtin_ctzl(unsigned long x)`

**Purpose**: Returns the number of trailing 0-bits in `x`. If `x` is 0, the result is undefined.

**Parameters**:
- `x`: An unsigned long to count trailing zeros in

**Returns**: The count of trailing zero bits (0 to 63 on LP64 systems).

**Usage**:
```c
unsigned long val = 1UL << 40;
int zeros = __builtin_ctzl(val);  // Returns 40
```

### `__builtin_ctzll(x)`

**Signature**: `int __builtin_ctzll(unsigned long long x)`

**Purpose**: Returns the number of trailing 0-bits in `x`. If `x` is 0, the result is undefined.

**Parameters**:
- `x`: An unsigned long long to count trailing zeros in

**Returns**: The count of trailing zero bits (0 to 63).

**Usage**:
```c
unsigned long long val = 0xFFFF000000000000ULL;  // 48 trailing zeros
int zeros = __builtin_ctzll(val);  // Returns 48
```

### Common Use Cases

**Finding the lowest set bit**:
```c
unsigned int lowest_bit = 1U << __builtin_ctz(x);
```

**Power of 2 detection** (combined with `__builtin_popcount` when available):
```c
// A number is a power of 2 if it has exactly one bit set
// Alternatively: (x & (x - 1)) == 0 && x != 0
```

**Bit manipulation algorithms**:
```c
// Clear the lowest set bit
x &= x - 1;

// Get index of lowest set bit
int idx = __builtin_ctz(x);
```

### Important Notes

- **Undefined behavior for zero**: If the input is 0, the result is undefined. Always check for zero before calling if the input could be zero.
- **Return type**: All three builtins return `int`, regardless of the input size.
- **Portability**: These builtins are compatible with GCC and Clang.

## Optimization Hints

### `__builtin_unreachable()`

**Signature**: `void __builtin_unreachable(void)`

**Purpose**: Indicates to the compiler that the code path is never reached at runtime. If control flow actually reaches this point, the behavior is undefined.

**Parameters**: None

**Returns**: Does not return (void)

**Important Notes**:
- If this builtin is actually executed at runtime, it will trap with an undefined instruction (ud2 on x86-64, brk #1 on AArch64)
- Use this to help the compiler optimize when you know certain code paths are impossible
- Commonly used after a `switch` statement that handles all cases to indicate the `default` is unreachable

**Usage**:
```c
enum State { START, RUNNING, DONE };

void handle_state(enum State s) {
    switch (s) {
    case START:
        init();
        break;
    case RUNNING:
        process();
        break;
    case DONE:
        cleanup();
        break;
    }
    // If we get here with an invalid state, trap
    __builtin_unreachable();
}
```

**Common Use Cases**:

**After exhaustive switch statements**:
```c
switch (value) {
case 0: return "zero";
case 1: return "one";
case 2: return "two";
default:
    __builtin_unreachable();
}
```

**Marking impossible conditions**:
```c
if (ptr == NULL) {
    // We validated ptr earlier, this should never happen
    __builtin_unreachable();
}
```

**Implementation**:
- x86-64: Emits `ud2` instruction (undefined instruction trap)
- AArch64: Emits `brk #1` instruction (software breakpoint)

**Standard Header Compatibility**:
This builtin is compatible with GCC and Clang.
