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

## Conformance

There is no open conformance punch list. `cc/audit.md` was retired once its
last gap closed, following the rule in the repository's `audits.md`: a crate
keeps an audit file only while it still has an open item, because a punch list
with nothing on it reads as coverage while asserting nothing. The C99 and C11
checklists went the same way earlier, once satisfied — 885 ticked boxes are a
maintenance burden that says less than a record of what was actually probed.

Every finding, every CONFORMS row and every probe is in git history.
`git log --follow -- cc/audit.md` recovers the file; `git log --grep '#C116'`
finds a single finding by number, in the commit that fixed it.

Two things that file established are worth keeping in front of you:

**Scope.** Conformance means POSIX.1-2024 and the ISO C standard it
incorporates, and nothing else. Engineering debt that is *not* a conformance
question belongs in [TODO.md](TODO.md) — `_FORTIFY_SOURCE` moved there because
it, `__builtin_object_size` and the `_chk` family appear nowhere in
POSIX.1-2024, and filing it as a conformance gap overstated what it was.

**How a conformance claim is established.** By probing the built binary
against the spec slice — not by reading the source, and not by trusting an
earlier write-up. Several findings were originally written from a premise that
a probe then disproved, and every unprobed ISO C row that got re-probed turned
out to be wrong.

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
  - Libc aliases (`__builtin_abort`, `__builtin_printf`, `__builtin_strcpy`, ...),
    callable without the declaring header
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
