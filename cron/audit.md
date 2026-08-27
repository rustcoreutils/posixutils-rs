# POSIX.1-2024 Conformance Audit — `cron/` utilities

Open items only, for `crontab`, `at`, `batch` and the `crond` daemon. Closed
findings are in git history — `git log --grep '#A7'` finds one by number.

**Spec slices:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/{crontab,at,batch}.md`
(`crond` is not POSIX-specified; it was audited as implicitly required by
`crontab`/`at`, against Vixie cron behavior.)

## Open

All three need something CI does not have. None is a known defect: in each
case the behavior is implemented and the *logic* is unit-tested as a pure
function; what is missing is the end-to-end assertion.

- [ ] **Job actually executing at its time (#A2/#X1)** — needs a running `crond`
  and a writable spool. `CRON_SPOOL_DIR` is a compile-time constant, so the
  round-trip tests skip unless the real spool is writable.
- [ ] **`at -r` ownership enforcement (#A7)** — needs two distinct users to
  submit as. The enforcement is present in `at.rs` and was re-verified
  2026-08-08; only the two-user test is missing.
- [ ] **`at`/`batch` spool execution (#X1)** — needs a running daemon.

Per the maintainer's decision these are skip-gated rather than silently
passing. No box here was ever ticked on the strength of a test CI never runs.
