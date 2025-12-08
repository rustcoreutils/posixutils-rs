# pcc Documentation

This directory contains detailed documentation for the pcc C compiler.

## Documents

| Document | Description |
|----------|-------------|
| [ATTR.md](ATTR.md) | Function attributes (`__attribute__`, `_Noreturn`, `__has_attribute`) |
| [BUILTIN.md](BUILTIN.md) | Compiler builtin functions (`__builtin_*`) |
| [TODO.md](TODO.md) | Technical debt, future features, and optimization passes |

## Quick Links

### Language Features

- **Attributes**: [ATTR.md](ATTR.md)
  - `__attribute__((noreturn))` - Function never returns
  - `_Noreturn` - C11 noreturn specifier
  - `__has_attribute()` - Compile-time attribute query

- **Builtins**: [BUILTIN.md](BUILTIN.md)
  - Variadic functions (`va_start`, `va_arg`, `va_end`, `va_copy`)
  - Byte swapping (`__builtin_bswap16/32/64`)
  - Introspection (`__builtin_constant_p`, `__builtin_types_compatible_p`)
  - Stack allocation (`__builtin_alloca`)
  - Bit operations (`__builtin_ctz`, `__builtin_ctzl`, `__builtin_ctzll`)
  - Control flow (`__builtin_unreachable`)
  - Non-local jumps (`setjmp`, `longjmp`)

### Development

- **TODO & Roadmap**: [TODO.md](TODO.md)
  - Technical debt items
  - C11 feature implementation plans
  - Optimization passes roadmap
  - Assembly peephole optimizations

## See Also

- [../README.md](../README.md) - Main pcc guide (building, testing, architecture)
