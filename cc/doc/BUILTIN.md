# Compiler Builtins

Builtin functions supported by pcc. GCC/Clang compatible.

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

## Control Flow

| Builtin | Description |
|---------|-------------|
| `__builtin_unreachable()` | Mark code path as unreachable (traps if reached) |

## Not Yet Implemented

| Builtin | Description |
|---------|-------------|
| `__builtin_expect(expr, c)` | Branch prediction hint |
| `__sync_synchronize()` | Full memory barrier |
| `__sync_fetch_and_add(ptr, val)` | Atomic fetch-and-add |
