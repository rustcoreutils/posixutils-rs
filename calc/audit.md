# POSIX.1-2024 Conformance Audit — `calc/` utilities

Open items only, for `expr` and `bc`. Closed findings are in git history —
`git log --grep '#E14'` finds one by number.

**ID scheme:** `#E*` expr, `#B*` bc. Numbering continues the original audit
(2026-06-06, `git show 49de59d3^:calc/audit.md`), whose #E1–#E10 and #B1–#B12
are closed, as are #B1–#B12's successors and #E11–#E19 from the 2026-08-28
crate review.

## Open — `bc`

Two minor items; everything else the review found is closed.

- [ ] **#B13 — `(a=5)` prints nothing.** GNU prints `5`. `should_print` matches
  the top-level instruction and the parser folds parentheses away, so a
  parenthesised assignment is indistinguishable from a bare one. POSIX
  ("unless the main operator is an assignment", XCU bc) is ambiguous for the
  parenthesised form, so this is a GNU-compatibility divergence rather than a
  clear violation.
- [ ] **#B14 — Digit extraction for `obase > 16` is still quadratic.** The
  integer part goes through `to_str_radix` for bases up to 16, but a larger
  base has no radix conversion available and still divides once per digit.
  Only reachable with a large value *and* a large obase, which no fixture or
  real program does.

## Documented residuals

None is open work; each bounds what the closed findings guarantee.

- **`bc` recursion is bounded by a constant, not by the stack.** It runs its
  interpreter on a large stack so `MAX_EVAL_DEPTH` reports the limit rather
  than the process aborting on a guard page. The constant has to hold for
  unoptimized builds too, whose frames are several times larger, so the
  release binary stops well below what its stack could take: bc recursion runs
  some thousands deep, against GNU's tens of thousands. Where the address
  space is capped tightly enough to refuse that stack -- `RLIMIT_AS`, or
  strict overcommit -- bc falls back to the default one and runs; a program
  that recurses far enough to need the room then aborts as it did before the
  large stack existed, which is still better than not running at all.
- **`expr` computes in `i128`, not arbitrary precision.** POSIX does not
  mandate bignum and `i128` covers every realistic shell use, but GNU expr is
  arbitrary-precision, so `expr 10^40 + 1` reports "integer out of range"
  where GNU answers. Deliberate; the diagnostic at least names the real reason
  now. `bc`'s `Number` is in the same crate if this is ever revisited.
- **A closed standard descriptor sends output to `/dev/null`, and exits 0.**
  Both utilities claim fds 0–2 with `/dev/null` before anything opens a file,
  because otherwise the message catalog takes the closed slot and output is
  written into it. GNU instead fails with EBADF and exits non-zero. Writing to
  `/dev/null` and succeeding is what coreutils does, and is the safer of the
  two; the point of the guard is that output cannot reach an unrelated file.
- **The macOS empty-pattern fix is unverified on macOS.** `plib::regex` used
  to rewrite an empty pattern to `.*` there, so `expr abc : ''` returned 3
  instead of 0. An empty pattern is now matched directly, which is what
  glibc's `regcomp` already does for it — Linux behavior is unchanged and
  macOS should converge on it, but only CI can confirm that.

## Refuted — do not re-raise

Each was raised during a review and checked against the standard.

- **Unary minus binds tighter than `^`.** Table 3-3 lists rows in *decreasing*
  precedence: `++ --` / unary `−` / `^` / `*` `/` `%` / `+` `−` / assignment /
  relational. `-2^2` is `4`, and GNU agrees. (Also closed as #B7 in 2026-06.)
- **`sqrt(1)` at `scale=8` is `1.00000000`.** POSIX: "the scale of the result
  shall be the scale of the expression or the value of scale, whichever is
  larger". GNU's `1` is the deviation.
- **A leading `0` on a value below one is unspecified.** POSIX: "it is
  unspecified whether the character 0 is output". Ours prints `0.5` where GNU
  prints `.5`; the `.out` fixtures depend on it.
- **Rejecting a digit at or above `ibase` is permitted.** POSIX makes the
  behavior undefined; GNU clamps instead.
- **Rejecting one-line `define f(){ … }` is correct.** The formal grammar is
  `'{' NEWLINE opt_auto_define_list statement_list '}'`; GNU's leniency is the
  extension.
- **Line wrapping does not protect a multi-digit group.** GNU breaks at
  exactly 68 characters regardless of content, splitting a group where one
  falls across the boundary. An earlier reading of that as a separate defect
  was wrong — the group splitting seen at the time was the digit *width*.
- **`expr` cannot always tell an operator from an operand.** `expr X = =` is a
  syntax error here and `0` in GNU. POSIX APPLICATION USAGE is explicit: expr
  "is not required to be able to tell the difference between an operator and
  an operand except by the value", citing `expr = = =`.
- **`expr`'s `length`, `substr`, `index` and `match` are unspecified.** POSIX
  leaves them so; treating them as ordinary strings is conformant, and
  `expr length : length` gives 6 where GNU errors.
