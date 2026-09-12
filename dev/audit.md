# POSIX.1-2024 Conformance Audit — `dev/` utilities

Open items only, for `yacc`, `lex`, `ar`, `nm`, `strings`, `strip`. Closed
findings are in git history — `git log --grep '#A7'` finds one by number.

**Spec slices:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/{yacc,lex,ar,nm,strings,strip}.md`

## Open

None.

- [x] **`ar`: locale-driven `-tv` date format (#A7)** ✓ fixed 2026-09-12 —
  `test_ar_tv_date_uses_mtime_not_age` pinned `TZ` and the year (it is the #A1
  regression), but nothing asserted that `LC_TIME` selects the month rendering.
  `test_ar_tv_date_follows_lc_time` does, behind `posixutils_test_all`. It
  takes the expected month from the system's own locale data rather than a
  hard-coded name, because glibc falls back to C silently when a locale is
  missing, and skips when no installed locale renders September differently
  from C — so it cannot pass vacuously. Proven to fail by building
  `fr_FR.UTF-8` with `localedef` into a scratch `LOCPATH` and breaking `%b`.

Per `audits.md` §9 this file would now go away, its punch list being empty.
It is kept for the residuals below, which are dispositions rather than a
punch list and are recorded nowhere else; the next finding should be added
here rather than reviving it.

## Documented gaps that are not open work

- **`lex`: `yywrap`/`main` "shall appear only in the lex library."** Satisfied
  in intent by `#ifndef YY_NO_DEFAULT_YYWRAP` / `#ifndef YY_NO_DEFAULT_MAIN`,
  so a conforming application can suppress ours and supply its own from any
  translation unit. The literal clause needs a shipped `libl`, which was
  rejected on packaging grounds.
- **`strip`: no Mach-O test.** Needs macOS CI; the Linux reference host cannot
  produce a Mach-O fixture. `object` has no `build::macho`, so the refusal is
  loud and the supported-format list is documented.
- **`ar`: `TMPDIR` unconsulted.** By design — `plib::io::write_atomic` stages
  the temp file in the target's directory so the `rename(2)` stays
  intra-filesystem and atomic.
- **`lex`: a literal character above U+007F in a pattern is refused.** The
  generated scanner's alphabet is bytes, so such a pattern cannot match; flex
  matches it. Accepting it silently was the defect and is fixed — the
  diagnostic names the character and its position. `\NNN` and `\xNN` still
  name bytes 0x80..0xff and are checked against flex. See `dev/lex/README.md`.
- **`yacc`: a `//` comment between rules is a syntax error.** POSIX yacc
  comments are `/* */`; bison accepts `//` as an extension. Inside an action
  `//` is C and is handled. See `dev/yacc/README.md`.
