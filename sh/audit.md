# POSIX.1-2024 Conformance Audit — `sh` (shell command language)

Open items only. Closed findings are in git history — `git log --grep '#57'`
finds one by number; the regression tests for them live in
`sh/tests/integration.rs` (`audit_regressions`) and `sh/tests/pty/mod.rs`.

**Spec slices:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/2-shell-command-language/*.md`
and `/3-utilities/sh.md`.

## Open

One, an optional feature the grammar does not require.

- [ ] **#57 — `{varname}<file` (IO_LOCATION) redirection unimplemented.** §2.10
  defines `%token IO_LOCATION`; the lexer has no such token. The grammar makes
  it optional — "the token identifier IO_LOCATION **may** result" — and dynamic
  fd allocation into a named variable is a ksh93/bash extension rarely used in
  POSIX scripts. Accepted optional-feature gap, tracked as Minor.

## Documented gap that is not open work

- **Interpolated diagnostics are not `gettext`-wrapped.** The fixed-string
  diagnostics across the built-ins and the shell-core `CommandExecutionError`
  Display route through `gettextrs::gettext`, so `LC_MESSAGES` translates them
  once catalogs are installed. Roughly 78 *interpolated* diagnostics
  (`format!("util: …{var}…")`) still embed literal English, because Rust's
  `format!` requires a literal template. Wrapping each one's translatable
  fragment is mechanical and changes no behavior without `.mo` catalogs.
