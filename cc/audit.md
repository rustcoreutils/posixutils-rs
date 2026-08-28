# POSIX.1-2024 Conformance Audits — `cc/` utilities

This file collects per-utility POSIX conformance audits for the C compiler
crate. Each audit follows the playbook in `audits.md`.

**Crate:** `cc/` — `c17` (was `pcc`), `cflow`, `ctags`, `cxref`.
**Spec slices:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/{c17,cflow,ctags,cxref}.md`

## Scope

This file tracks conformance to POSIX.1-2024 and to the ISO C standard it
incorporates, and nothing else, so an open item here means exactly one thing:
a place where c17 knowingly diverges from the standard. Engineering debt that
is *not* a conformance question lives in `cc/doc/TODO.md` — `_FORTIFY_SOURCE`
(formerly #C12) moved there, since `_FORTIFY_SOURCE`, `__builtin_object_size`
and the `_chk` family appear nowhere in POSIX.1-2024.

Closed findings, the conformance matrices and the coverage notes are not kept
here. What each fix was, and why, is in the commit that made it —
`git log --grep '#C123'` finds one by number.

**How a claim in this file is established.** By probing the built binary
against the spec slice, not by reading the source and not by trusting an
earlier entry in this file — several findings were originally written from a
premise that a probe then disproved.

## Open

One, and it is a standing decision rather than pending work. It is kept here
so it does not get re-raised as a to-do.

- [ ] **#C55 — Trigraphs are off by default.** **SETTLED by maintainer decision — not deferred, not awaiting anything, and not to be re-raised.** The c17 APPLICATION USAGE says it outright (88224): "Some c17 compilers *not conforming to POSIX.1-2024* do not support trigraphs by default." #P11 implemented them behind `--trigraphs` because replacement reaches inside string literals, so `"What??!"` becomes `"What|"` — which is exactly what C17 phase 1 specifies and what the POSIX RATIONALE laments without granting an exemption. A deliberate divergence, not a missing feature; the fix is to flip the default and offer an opt-out.
