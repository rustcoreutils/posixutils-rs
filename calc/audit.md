# POSIX.1-2024 Conformance Audit — `calc/` utilities

Open items only, for `expr` and `bc`. Closed findings are in git history —
`git log --grep '#B14'` finds one by number.

**ID scheme:** `#E*` expr, `#B*` bc. Numbering continues the original audit
(2026-06-06, `git show 49de59d3^:calc/audit.md`), whose #E1–#E10 and #B1–#B12
are all closed.

A crate-wide review on 2026-08-28 found 35 defects, reproduced against GNU bc
1.07.1 / GNU expr 9.4 and checked against the standard. The bc findings were
fixed across six commits; the expr ones are open below.

## Open — `expr`

`expr`'s evaluator, precedence and BRE handling were probed at length and are
correct. Every open item is at the edges: how operands are decoded, how they
are classified, and what happens when a write fails.

#### Major

- [ ] **#E11 — Non-UTF-8 operands abort.** `tokenize` collects
  `std::env::args()`, which panics on an argument that is not valid UTF-8:
  `expr "$(printf 'a\xffb')" : a` exits 101 where GNU prints `1`. POSIX
  operands are byte strings, and filenames in other encodings routinely reach
  `expr`. Needs `args_os` and byte-oriented handling.
- [ ] **#E12 — A failed write is not reported.** `expr 1 + 1 > /dev/full`
  panics with exit 101; with stdout closed it exits 0 having written nothing.
  POSIX makes exit 0 mean the result "was successfully written". `bc` was
  fixed the same way in the diagnostics commit — the descriptor guard and the
  reported flush belong here too.
- [ ] **#E13 — `|` returns a null `expr2` instead of zero.** POSIX
  (XCU expr, "returns the evaluation of expr2 **if it is not null**;
  otherwise, zero"): `expr '' \| ''` and `expr 0 \| ''` must print `0`, and
  print the empty string instead. The wrong rule is written into the comment
  in `logop` and repeated in the `expr_logops` test, so both need correcting.
- [ ] **#E14 — Integer conversion destroys the operand's text.**
  `parse_token` converts anything that parses as an integer and keeps only the
  value, so the original spelling is gone everywhere it still matters:
  `expr 007 : '.*'` gives `1` where POSIX gives `3`, and `expr 0 \| 007` gives
  `7` rather than `007`. POSIX treats an argument as an integer only where the
  operator requires it. Keep the lexeme; interpret it where arithmetic or
  comparison demands.
- [ ] **#E15 — `+5` is accepted as an integer.** POSIX defines an integer as
  "an (optional) unary **minus** followed by digits", but `i128::from_str`
  also accepts a leading plus, so `expr +5 + 1` gives `6` where GNU rejects
  it. The same parse in `token_is_null_or_zero` misclassifies the *string*
  `"+0"` as zero, which changes both the value and the exit status of an
  expression that captured it.

#### Minor

- [ ] **#E16 — `has_subexpr` misreads a bracket expression.** A backslash is
  an ordinary character inside `[...]` (XBD 9.3.5), so `[\(]` contains no
  subexpression; the flat scan treats it as one and returns the null string
  instead of the match length. `expr '(x' : '[\(]x'` gives empty where GNU
  gives `2`. The regex engine itself handles the bracket correctly.
- [ ] **#E17 — LC_CTYPE is ignored for the match length.** `matchop` counts
  `chars()`, always UTF-8: `LC_ALL=C expr 日本語 : '.*'` gives 3 where GNU
  gives 9. `cmpstr` already honours LC_COLLATE through `strcoll`; only `:` is
  locale-blind.
- [ ] **#E18 — Integer range is `i128`, and the diagnostic misleads.** POSIX
  does not mandate arbitrary precision, so the limit itself is defensible, but
  an operand too large for `i128` falls through to `Token::Str` and is
  reported as a "non-integer argument" — which it is not. `bc`'s `Number` is
  in the same crate if arbitrary precision is wanted.

## Open — `bc`

#### Minor

- [ ] **#B13 — `(a=5)` prints nothing.** GNU prints `5`. `should_print`
  matches the top-level instruction and the parser folds parentheses away, so
  a parenthesised assignment is indistinguishable from a bare one. POSIX
  ("unless the main operator is an assignment", XCU bc) is ambiguous for the
  parenthesised form, so this is a GNU-compatibility divergence rather than a
  clear violation.
- [ ] **#B14 — Digit extraction for `obase > 16` is still quadratic.** The
  integer part goes through `to_str_radix` for bases up to 16, but a larger
  base has no radix conversion available and still divides once per digit.
  Only reachable with a large value *and* a large obase, which no fixture or
  real program does.

## Documented residuals

Neither is open work in `calc`; both bound what the fixes above guarantee.

- **The standard-descriptor guard is bc's alone.** `bc` claims fds 0–2 with
  `/dev/null` before anything opens a file, because otherwise the message
  catalog takes a closed descriptor and program output is written into it.
  Every other utility in the workspace has the same exposure; the guard
  belongs in `plib` rather than in one `main`.
- **Recursion depth is bounded by a constant, not by the stack.** `bc` runs
  its interpreter on a large stack so `MAX_EVAL_DEPTH` reports the limit
  instead of the process aborting on a guard page. The constant is set to hold
  for unoptimized builds too, whose frames are several times larger, so the
  release binary stops well below what its stack could take: bc recursion runs
  some thousands deep, against GNU's tens of thousands.

## Refuted — do not re-raise

Each was raised during the review and checked against the standard.

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
  was wrong — the group splitting seen at the time was the digit *width*,
  fixed as part of the output work.
