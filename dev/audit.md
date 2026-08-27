# POSIX.1-2024 Conformance Audit — `dev/` utilities

Open items only, for `yacc`, `lex`, `ar`, `nm`, `strings`, `strip`. Closed
findings are in git history — `git log --grep '#A7'` finds one by number.

**Spec slices:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/{yacc,lex,ar,nm,strings,strip}.md`

## Open

One, a test-coverage gap rather than a defect.

- [ ] **`ar`: locale-driven `-tv` date format (#A7)** — `test_ar_tv_date_uses_mtime_not_age`
  pins `TZ` and the year (it is the #A1 regression), but nothing asserts that
  `LC_TIME` selects the month/day rendering. `-tv` routes through
  `plib::locale::strftime`, which honors `LC_TIME`; only the assertion is absent.

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
