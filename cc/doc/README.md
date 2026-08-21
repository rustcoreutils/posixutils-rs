# c17 Documentation

This directory contains detailed documentation for the c17 C compiler.

c17 implements **C17 (ISO/IEC 9899:2018) only**, plus selected GNU extensions.
There is one language mode: `-std=` is accepted because build systems pass it
unconditionally, but a request for an older revision is reported rather than
honoured (`-Wno-c17-dialect` silences it).

## Documents

| Document | Description |
|----------|-------------|
| [ATTR.md](ATTR.md) | Function attributes (`__attribute__`, `_Noreturn`, `__has_attribute`) |
| [BUILTIN.md](BUILTIN.md) | Compiler builtin functions (`__builtin_*`) |
| [TODO.md](TODO.md) | Technical debt, future features, and optimization passes |

Conformance is tracked in [../audit.md](../audit.md). The C99 and C11
checklists that used to live here were retired once satisfied: they had served
their purpose, and a checklist of 885 ticked boxes is a maintenance burden that
says less than the audit's record of what was actually probed.

## Quick Links

### Language Features

- **Attributes**: [ATTR.md](ATTR.md)
  - `__attribute__((noreturn))` - Function never returns
  - `_Noreturn` - C11 noreturn specifier
  - `__has_attribute()` - Compile-time attribute query

- **Inline Assembly**: See [../README.md](../README.md)
  - GCC extended asm syntax (`__asm__`, `asm`)
  - Constraints (`=r`, `+r`, `r`, `m`, register-specific)
  - SSE (`x`) and x87 (`t`, `u`) operand classes on x86-64
  - Vector (`w`) operands and the `b`/`h`/`s`/`d`/`q` width modifiers on AArch64
  - Clobbers (`memory`, `cc`, registers)
  - Named operands, matching constraints
  - `asm goto` with label references

- **GNU control-flow and initializer extensions**: See [../README.md](../README.md)
  - Case ranges (`case 1 ... 9:`)
  - Designated-initializer ranges (`[0 ... 3] = value`)
  - Computed goto (`&&label`, `goto *ptr`)

- **Builtins**: [BUILTIN.md](BUILTIN.md)
  - Variadic functions (`va_start`, `va_arg`, `va_end`, `va_copy`)
  - Byte swapping (`__builtin_bswap16/32/64`)
  - Introspection (`__builtin_constant_p`, `__builtin_types_compatible_p`)
  - Stack allocation (`__builtin_alloca`)
  - Bit operations (`__builtin_ctz/clz/popcount` families)
  - Control flow (`__builtin_unreachable`)
  - Non-local jumps (`setjmp`, `longjmp`)
  - Complex construction (`__builtin_complex`)
  - Fortification (`__builtin_object_size`, the `__builtin___*_chk` family)
  - and what is **not** implemented, where the absence is observable

### Development

- **TODO & Roadmap**: [TODO.md](TODO.md)
  - Technical debt items
  - Remaining feature work
  - Optimization passes roadmap
  - Assembly peephole optimizations
  - External test suites not yet run

## See Also

- [../README.md](../README.md) - Main c17 guide (building, testing, architecture)
