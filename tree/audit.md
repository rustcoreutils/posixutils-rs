# POSIX.1-2024 Conformance & Race-Safety Audit — `tree/` utilities + `ftw/`

Open items only, for the 16 `tree/` utilities — `cp`, `mv`, `rm`, `chmod`,
`chown`, `chgrp`, `mkdir`, `mkfifo`, `rmdir`, `link`, `unlink`, `ln`,
`readlink`, `du`, `touch`, `ls` — plus the `ftw/` race-free traversal crate
they depend on. Closed findings are in git history — `git log --grep '#F1'`
finds one by number.

Unlike the other crate audits, this one carries a filesystem-race /
symlink-swap lens on top of plain POSIX conformance, because these utilities
are the canonical TOCTOU targets and the canonical cause of accidental data
loss. **ID scheme:** `#F*` ftw, `#C*` cp, `#M*` mv, `#R*` rm, `#CM*` chmod,
`#CO*` chown, `#CG*` chgrp, `#MK*` mkdir, `#MF*` mkfifo, `#RD*` rmdir,
`#LK*` link, `#UN*` unlink, `#LN*` ln, `#RL*` readlink, `#DU*` du, `#TO*` touch,
`#LS*` ls.

## Open

One, a test-coverage gap rather than a defect.

- [ ] **`du -x` device boundary** — needs a real mount point, so CI cannot
  exercise it. The option is implemented and the non-crossing behavior was
  verified by hand against GNU `du`.

## Documented residuals in `ftw`

Neither is open work; both are recorded because they bound what the hardened
descent path guarantees.

- **`DeferredDir` has no `(dev, ino)` baseline.** The main descent path opens
  with `O_DIRECTORY` (plus `O_NOFOLLOW` when not following symlinks), then
  `fstat`s the opened fd and aborts on a mismatch against the pre-descent stat.
  The rare fd-conserving `DeferredDir` path — reached only beyond
  `RLIMIT_NOFILE − 7` simultaneously-open directories — captures no baseline,
  so it relies on `O_NOFOLLOW` alone. It fails closed, but does not detect a
  swap to a *different real directory*.
- **The deep-tree reopen still panics on a mid-walk race (#F3).** Failing
  closed, but a panic rather than an `err_reporter` call. Converting it needs a
  fallible reopen API threaded through three expression-context call sites in
  ftw's `dir_fd` resolution; deferred as low-impact for the same
  `RLIMIT_NOFILE` reason.
