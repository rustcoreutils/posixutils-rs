# POSIX.1-2024 Conformance Audits — `sys/` utilities

Open items only, for `getconf`, `ipcrm`, `ipcs`, `ps`, `uname`, `who`. Closed
findings are in git history — `git log --grep '#P1'` finds one by number.

**Spec slices:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/{getconf,ipcrm,ipcs,ps,uname,who}.md`

## Open

Both are test-coverage gaps, not findings. The behavior is implemented; only
the assertions are missing.

- [ ] **`ps`: header-suppression content** — `ps_empty_header` checks the exit
  status but not the output, so nothing pins that the header is actually absent.
- [ ] **`who`: `-T` state characters and the `-b` "system boot" line** — both
  are emitted but neither's content is asserted.

## Dispositioned, not open work

- **`_CS_POSIX_V8_*` confstr and the pure-header limit macros** (`getconf`) —
  not exposed by `libc`.
- **macOS SysV message queues** (`ipcs`/`ipcrm`) — platform limitation.
- **Crate-wide `.mo` translation catalogs** — tree-wide i18n decision,
  consistent with `dev/`.
- **macOS runtime verification** of the `ps`/`ipcs` platform paths — they
  compile and lint cleanly for `x86_64-apple-darwin`; runtime confirmation
  waits on CI.
