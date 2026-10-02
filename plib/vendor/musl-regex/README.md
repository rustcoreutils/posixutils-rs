# musl's POSIX regex, vendored for Windows

The Windows C runtime has no `regcomp`/`regexec`, so on a Windows target
`plib::regex` uses musl's, compiled by `plib/build.rs` with the `cc` crate.
Unix targets do not compile any of this; they use the C library's regex.

## Origin

- musl 1.2.6, `https://musl.libc.org/releases/musl-1.2.6.tar.gz`
- SHA-256 `d585fd3b613c66151fc3249e8ed44f77020cb5e6c1e635a616d3f9f82460512a`;
  the release's `.asc` signature verifies against musl's key
  (`8364 8929 0BB6 B70F 99FF DA05 56BC DB59 3020 450F`)
- musl 1.2.6's `src/regex` files used here are byte-identical to 1.2.5's

| here | musl | state |
|---|---|---|
| `src/regcomp.c` | `src/regex/regcomp.c` | unmodified |
| `src/regexec.c` | `src/regex/regexec.c` | modified |
| `src/regerror.c` | `src/regex/regerror.c` | unmodified |
| `src/tre-mem.c` | `src/regex/tre-mem.c` | unmodified |
| `src/tre.h` | `src/regex/tre.h` | modified |
| `src/iswctype.c` | `src/ctype/iswctype.c` | modified |
| `include/regex.h` | `include/regex.h` | adapted |
| `include/plib_musl.h` | none | plib's |
| `include/locale_impl.h` | `src/internal/locale_impl.h` | plib's stand-in |
| `COPYRIGHT` | `COPYRIGHT` | unmodified |

## License

musl is MIT-licensed; see `COPYRIGHT`. The TRE-derived files (`reg*.c`,
`tre*`) are Copyright © 2001-2009 Ville Laurikari under a 2-clause BSD
license, whose text is at the top of each of those files. plib's own headers
are MIT, as the rest of posixutils-rs.

## Modifications

Every change inside a musl file is marked with a `plib:` comment.

- **Build environment** (`include/`). musl's sources include musl-internal
  headers. `include/regex.h` is musl's with `<features.h>` and
  `<bits/alltypes.h>` replaced by `plib_musl.h`, and `regoff_t` defined as
  `ptrdiff_t` (musl's pointer-sized `_Addr`; `long` is 32 bits on 64-bit
  Windows). `plib_musl.h` defines `hidden` (empty), `CHARCLASS_NAME_MAX` and
  `RE_DUP_MAX` (musl's values), and renames every external symbol to a
  `plib_` name: `regcomp`, `regexec`, `regfree`, `regerror` and the
  `__tre_mem_*` helpers by macro. `locale_impl.h` makes `LCTRANS_CUR` the
  identity.
- **Character classes** (`tre.h`, `iswctype.c`). `[[:class:]]` goes through
  musl's own `wctype`/`iswctype`, as `plib_wctype`/`plib_iswctype`, rather
  than the C runtime's: msvcrt's `wctype` has no `"blank"`. `iswctype.c`
  drops musl's `__iswctype_l`/`__wctype_l` and their weak aliases. The
  per-class predicates (`iswalpha`, `iswblank`, ...) are still the runtime's.
- **`long` is not pointer-sized on LLP64** (`tre.h`, `regexec.c`). musl's
  `ALIGN(ptr, long)` cast a pointer to `long` and aligned to `sizeof(long)`,
  which on 64-bit Windows truncates the pointer and leaves pointers and
  `regoff_t` 4-byte aligned. It now aligns to at least a pointer through
  `uintptr_t`, and the padding `regexec` allocates for it grows to match.
  Unchanged arithmetic on LP64 and ILP32.
- **16-bit `wchar_t`** (`tre.h`). `TRE_CHAR_MAX` is capped at `WCHAR_MAX`
  when that is below U+10FFFF, since transitions store characters as
  `wint_t`; otherwise a negated bracket's last range ran past what the type
  holds.
- **`regexec`'s `pmatch[restrict]`** is spelled `*restrict pmatch` (the same
  type): MSVC does not document support for a qualifier inside a parameter's
  `[]`.
- **Back-references after a multibyte character** (`regexec.c`, a bug in
  musl itself). The backtracking matcher, used for any pattern with a
  back-reference, assumed the lookahead character was one byte: the
  back-reference was compared from `str_byte - 1`, and the lookahead length
  was not restored on backtracking or when retrying from a later start. In
  UTF-8, `\(ü\)\1` never matched `aüü` and `\(a\)\1` never matched `üaa`.
  The comparison now starts `pos_add_next` bytes back, and the length is
  recomputed from `str_byte - string - pos` on both paths. A differential of
  4000 generated BRE patterns, each run on UTF-8 subjects and on the same
  subjects transliterated to ASCII, gave 226 disagreements before the fix
  and none after.

## Behavior on Windows

Matching decodes the subject with the C runtime's `mbtowc`, so it follows
`LC_CTYPE`; `plib::diag::init_locale` sets the UCRT's to UTF-8. `wchar_t`
is 16 bits, so characters above U+FFFF are not supported: `mbtowc` cannot
return one, and musl's matcher reports no match for a subject containing a
character it cannot decode (as it does for invalid UTF-8). A back-reference
under `REG_ICASE` compares bytes, so it matches only text in the same case
(glibc's ignores case there too).

The `-gnu` targets link msvcrt, which has no UTF-8 locale, so under Wine
the UTF-8 regex tests in `plib/src/regex.rs` find none and return early;
the MSVC build in CI runs them.
