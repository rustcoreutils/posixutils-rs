# pcc Documentation

This directory contains detailed documentation for the pcc C compiler.

## Documents

| Document | Description |
|----------|-------------|
| [ATTR.md](ATTR.md) | Function attributes (`__attribute__`, `_Noreturn`, `__has_attribute`) |
| [BUILTIN.md](BUILTIN.md) | Compiler builtin functions (`__builtin_*`) |
| [C99.md](C99.md) | C99 compliance status and remaining work |
| [TODO.md](TODO.md) | Technical debt, future features, and optimization passes |
| [BUGS.md](BUGS.md) | Known bugs (parse errors on valid C99 code) |

## Quick Links

### Language Features

- **Attributes**: [ATTR.md](ATTR.md)
  - `__attribute__((noreturn))` - Function never returns
  - `_Noreturn` - C11 noreturn specifier
  - `__has_attribute()` - Compile-time attribute query

- **Inline Assembly**: See [../README.md](../README.md)
  - GCC extended asm syntax (`__asm__`, `asm`)
  - Constraints (`=r`, `+r`, `r`, `m`, register-specific)
  - Clobbers (`memory`, `cc`, registers)
  - Named operands, matching constraints
  - `asm goto` with label references

- **Builtins**: [BUILTIN.md](BUILTIN.md)
  - Variadic functions (`va_start`, `va_arg`, `va_end`, `va_copy`)
  - Byte swapping (`__builtin_bswap16/32/64`)
  - Introspection (`__builtin_constant_p`, `__builtin_types_compatible_p`)
  - Stack allocation (`__builtin_alloca`)
  - Bit operations (`__builtin_ctz/clz/popcount` families)
  - Control flow (`__builtin_unreachable`)
  - Non-local jumps (`setjmp`, `longjmp`)

### Compliance & Status

- **C99 Compliance**: [C99.md](C99.md)
  - Partial implementations
  - Implementation roadmap

- **Known Bugs**: [BUGS.md](BUGS.md)
  - Parse errors on valid C99 code
  - Codegen bugs

### Development

- **TODO & Roadmap**: [TODO.md](TODO.md)
  - Technical debt items
  - C11 feature implementation plans
  - Optimization passes roadmap
  - Assembly peephole optimizations

## See Also

- [../README.md](../README.md) - Main pcc guide (building, testing, architecture)
