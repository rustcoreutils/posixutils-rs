# POSIX.1-2024 Conformance Audits — `calc/` utilities

This file collects per-utility POSIX conformance audits for the calculator
utilities crate (`posixutils-calc`). Each audit follows the playbook in
`../audits.md`.

**Utilities:** `expr` (`expr.rs`, 378 lines), `bc` (`bc.rs` + `bc_util/`,
~4000 lines: `parser.rs`, `interpreter.rs`, `number.rs`, `instructions.rs`,
`grammar.pest`, `math_functions.bc`).
**Date:** 2026-06-06
**Method:** spec + full implementation read, then **behavioral verification** —
every Critical/Major claim below was reproduced by building the release
binaries and running real inputs. Several agent-proposed findings were
**refuted** by that step and are recorded as CONFORMS (see notes); they are
kept so a future auditor does not re-raise them.

> **Status (2026-06-06):** All actionable findings have been addressed across
> six phases on branch `calc-audit`. expr #E1–#E10 (Phases 1–2); bc #B1/#B9
> (Phase 3), #B2/#B10/#B11/#B12 (Phase 4), #B3/#B4/#B5 (Phase 5), #B6/#B8
> (Phase 6). #B7 (`-1^2`) was re-examined and is **not** a bug — GNU bc agrees
> with the current output (see note below). Three number.rs "rounds not
> truncates" findings raised during the audit were already refuted before
> publication. fixes verified against GNU `expr`/`bc`.

---

## Cross-cutting observations

- **No shared `plib::diag` adoption.** Both utilities hand-roll diagnostics.
  `expr` leans on `main() -> Result<_, Box<dyn Error>>` so errors print with a
  Rust `Error: "..."` prefix; `bc` routes *all* diagnostics to **stdout**.
  Neither uses a `<util>:`-prefixed stderr surface or `gettext()` on runtime
  messages. (`setlocale`/`textdomain` *are* installed in both `main`s.)
- **Regex flavor mismatch.** `expr`'s `:` operator must use POSIX **BRE**
  (XBD 9.3) but is wired to the Rust `regex` crate (ERE-ish, no BRE `\(...\)`,
  no leading anchor). `bc` has no regex surface.
- **No POSIX limit constants.** `bc` enforces `ibase`/`obase` lower bounds and
  the `ibase` upper bound, but never references `BC_SCALE_MAX`, `BC_BASE_MAX`
  (obase upper), `BC_DIM_MAX`, or `BC_STRING_MAX`.
- **`panic!`/`unwrap` on valid or adversarial input** appears in both
  (`expr` divide-by-zero; `bc` quit-in-`for`-in-function). POSIX `bc` is
  required to *recover* in interactive use.

---

## `expr`

**Implementation:** `expr.rs` (378 lines) + `tests/expr/mod.rs` (61 lines)
**Spec:** POSIX.1-2024, Vol. 3 §3, pp. 2963–2967
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/expr.md`

### TL;DR
`expr` handles the operator *set* and the integer/string typing rules, but the
**evaluation engine is wrong in three load-bearing ways**: it ignores operator
precedence (pure left-to-right), it implements the `:` matching operator with
Rust regex instead of anchored POSIX BRE (so the spec's own canonical
pathname example returns `0`), and it never sets the POSIX exit status (always
`0` on success, `1` on any error). Divide-by-zero panics. This utility needs a
real expression parser before it can be called conformant.

### Priority issues

#### Critical
- [x] **#E1 — Operator precedence ignored; strictly left-to-right.** ✓ Phase 1. Replaced the `tokens[0..3]` fold with a precedence-climbing evaluator honoring the spec table (`:` > `* / %` > `+ -` > comparisons > `&` > `|`, left-associative). `expr 2 + 3 \* 4` → `14`.
- [x] **#E2 — `:` matching uses Rust regex, not anchored POSIX BRE.** ✓ Phase 2. Switched to `plib::regex` BRE, anchored at start (`caps[0].start == 0`), with `\1` capture. `expr "//abc/file" : '.*/\(.*\)'` → `file`.
- [x] **#E3 — Divide / remainder by zero panics (exit 101), no diagnostic.** ✓ Phase 1. `checked_div`/`checked_rem` + explicit zero check → error, exit 2.
- [x] **#E4 — Exit status never reflects the result value.** ✓ Phase 1. `main` now exits 1 when the result is null/zero (empty, `0`, or integer 0), else 0.

#### Major
- [x] **#E5 — Error exit code is 1, not 2/>2.** ✓ Phase 1. Invalid expressions print `expr: <msg>` to stderr and exit 2.
- [x] **#E6 — `:` match length counts bytes, not characters.** ✓ Phase 2. Length is `chars().count()` of the anchored match. `expr éé : '.*'` → `2`.

#### Minor
- [x] **#E7 — `|` does not return expr2's value when expr1 is null/zero.** ✓ Phase 1. `logop` `|` arm now returns the evaluated expr2.
- [x] **#E8 — Integer range limited to `i64`; silent wrap / literal demotion.** ✓ Phase 1. Widened to `i128` with checked arithmetic (overflow → error, no wrap). (Per design decision: i128 + overflow errors, not bignum.)
- [x] **#E9 — `--` end-of-options not honored.** ✓ Phase 1. A single leading `--` is consumed before tokenizing. `expr -- -3 + 1` → `-2`.
- [x] **#E10 — Runtime diagnostics not localized / odd prefix.** ✓ Phase 1 (partial). Diagnostics now print as `expr: <message>` on stderr; string-level `gettext()` wrapping still deferred (consistent with the dev/ audit's "partial" status).

### Detailed conformance matrix

#### SYNOPSIS / argv parsing
- [x] No options defined; operands collected from argv — CONFORMS. `expr.rs:133-145`.
- [x] **`--` end-of-options** — #E9 ✓ Phase 1.

#### Operators / EXTENDED DESCRIPTION
- [x] **Precedence/associativity** — #E1 ✓ Phase 1.
- [x] Operator token set complete (`( ) | & = > >= < <= != + - * / % :`) — CONFORMS. `expr.rs:107-130`.
- [x] Integer-vs-string identification for arithmetic/comparison operands — CONFORMS. `expr.rs:179-199` (`cmpop` falls back to string compare; `intop` requires ints).
- [x] Unary-minus integer literals (`-5`) — CONFORMS. `expr.rs:125-128`.
- [x] **`|` null/zero return value** — #E7 ✓ Phase 1. `&` arm CONFORMS.
- [x] **`:` matching (BRE flavor, anchor, char length)** — #E2, #E6 ✓ Phase 2.
- [x] `length`/`substr`/`index`/`match` keywords — N/A (spec: unspecified results); current behavior is to treat them as strings → syntax error, which is acceptable.

#### STDIN / INPUT FILES / ENVIRONMENT
- [x] STDIN not used — CONFORMS.
- [x] `setlocale(LC_ALL,"")` + `textdomain` present — CONFORMS. `expr.rs:366-368`.
- [ ] **LC_COLLATE not honored for `=`/`!=` string compare** — DIVERGES (uses Rust `==`/`Ord`, byte order). `expr.rs:166-173`. Minor; fold into #E6/i18n work.

#### STDOUT / STDERR
- [x] Result + `<newline>` to stdout — CONFORMS. `expr.rs:375`.
- [x] **Diagnostics prefix + stderr channel** — #E10 ✓ Phase 1 (gettext deferred).

#### EXIT STATUS / CONSEQUENCES OF ERRORS
- [x] **Result-driven 0/1 status** — #E4 ✓ Phase 1.
- [x] **Invalid-expression status (2)** — #E5 ✓ Phase 1.
- [x] **Divide-by-zero → error, not panic** — #E3 ✓ Phase 1.

### Test coverage signal (`tests/expr/mod.rs`)
Covered: logical ops, integer ops, integer compare, string compare (4 cases each).
Now covered (added in Phases 1–2): operator precedence; `:` matching
(anchoring, `\(...\)` capture, char-vs-byte length); divide/remainder by zero;
exit-status assertions (0/1/2); `--` handling; multibyte operands.
Still not covered:
- [ ] LC_COLLATE-sensitive `=`/`!=` string comparison.

---

## `bc`

**Implementation:** `bc.rs` (118) + `bc_util/parser.rs` (1562) + `interpreter.rs` (1697) + `number.rs` (567) + `instructions.rs` (175) + `grammar.pest` + `math_functions.bc`; `tests/bc/mod.rs` (295)
**Spec:** POSIX.1-2024, Vol. 3 §3, pp. 2699–2713
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/bc.md`

### TL;DR
`bc` is in much better shape than `expr`: arithmetic **truncates** correctly
(division and `sqrt` verified — not rounded), the obase>16 multi-digit output
format matches the spec example, `ibase`/`obase` bounds are mostly enforced,
and the print-vs-assignment rule and relational-operator gating are correct.
The headline defects are a **crash** (`quit`/`break` inside a `for` body within
a function `panic!`s the process), **all diagnostics going to stdout** (breaks
`x=$(… | bc)`), missing **upper-bound limit checks** (`scale`, obase,
array index → unbounded allocation), and missing **70-column line wrapping**.

### Priority issues

#### Critical
- [x] **#B1 — `quit`/`break` inside a `for` body within a function panics.** ✓ Phase 3. `contains_quit` now recurses into `For` bodies (so quit is detected at definition-read time, matching GNU bc); `call_function` no longer panics on `ControlFlow::Quit`/`Break` (defensive). Added a regression test.
- [x] **#B2 — All diagnostics written to stdout, not stderr.** ✓ Phase 4. All error paths use `eprintln!`; program/partial output stays on stdout.

#### Major
- [x] **#B3 — `scale` upper bound (`BC_SCALE_MAX`) not enforced.** ✓ Phase 5. Rejects `scale > BC_SCALE_MAX` (`i32::MAX`, GNU-compatible).
- [x] **#B4 — `obase` upper bound (`BC_BASE_MAX`) not enforced.** ✓ Phase 5. Rejects `obase > BC_BASE_MAX` (`i32::MAX`).
- [x] **#B5 — Array index unbounded (`BC_DIM_MAX` not enforced).** ✓ Phase 5. Rejects `index >= BC_DIM_MAX` (16777215) before allocation.
- [x] **#B6 — 70-column line wrapping with `\`-continuation MISSING.** ✓ Phase 6. `to_string` output wraps (68 chars + `\` per continued line); verified byte-for-byte against GNU bc.
- [x] ~~**#B7 — Unary minus binds tighter than `^`.**~~ ✓ Phase 6 — **re-examined; actually CONFORMS.** GNU bc evaluates `-1^2` as `(-1)^2 == 1` (unary minus is *higher* precedence than `^` in the POSIX table), which the current implementation already matches. No change made.

#### Minor
- [x] **#B8 — `x^0` result scale is the `scale` register, not 0.** ✓ Phase 6. `pow` uses `!is_negative()` for the `b>=0` branch, so `x^0` has scale 0 (`scale=5; 2.5^0` → `1`).
- [x] **#B9 — Additional `panic!`/`unwrap` reachable on adversarial input.** ✓ Phase 3 (partial). The `call_function` quit/break panic is gone. The remaining sites (`number.rs` `to_digit`/`to_char`; top-level `return`/`break`) were verified **not user-reachable** — the grammar rejects top-level `return`/`break`, and `to_digit`/`to_char` are guarded by the parser/enforced bases. Left as internal invariants.
- [x] **#B10 — REPL prints `CTRL-D`/`CTRL-C` to stdout on EOF/interrupt.** ✓ Phase 4. EOF/interrupt now exit silently.
- [x] **#B11 — Exit status is 0 even after a file-not-found / error.** ✓ Phase 4. File-not-found and file-processing errors exit non-zero.
- [x] **#B12 — `math_functions.bc` loaded with `.expect()`.** ✓ Phase 4. The `-l` library load failure now prints a diagnostic and exits non-zero instead of panicking.

### Detailed conformance matrix

#### SYNOPSIS / OPTIONS / OPERANDS
- [x] `-l` math library option — CONFORMS (`bc.rs:54-60`); Bessel `j`, `s c a l e` all present in `math_functions.bc`.
- [x] `file...` operands processed in order, then stdin — CONFORMS. `bc.rs:62-76`.
- [x] File-not-found → stderr diagnostic + terminate — CONFORMS (channel). `bc.rs:68-72` (status: #B11).

#### Lexical / grammar (parser.rs, grammar.pest)
- [x] Longest-token (`<=`/`>=`/`==`/`!=` before single-char) — CONFORMS. `grammar.pest:23-29`.
- [x] `/* */` comments (non-nested) — CONFORMS. `grammar.pest:11`.
- [x] STRING = literal chars incl. newlines, no escape — CONFORMS. `grammar.pest:15`.
- [x] Keyword set + LETTER = `a-z` — CONFORMS. `grammar.pest:13`.
- [x] NUMBER text deferred to interpreter so `ibase` applies at runtime — CONFORMS. `instructions.rs:74`.
- [x] `\`-newline line continuation — CONFORMS (handled as whitespace + inside NUMBER). `grammar.pest:10,16`.
- [x] Relational operators valid **only** inside `if/while/for` conditions — CONFORMS. Verified: bare `1 < 2` is a parse error. `grammar.pest:79-84`.
- [x] Assignment precedence / right-assoc chains — CONFORMS (behaviorally). Verified: `a=b=3` → `3`, `a=1+2` → `3`. (The Pratt table models assignment as a primary; chains and precedence-vs-`+` are nonetheless correct.)
- [x] NEWLINE required after function-definition `{` — CONFORMS to the POSIX *formal* grammar (`function: … '{' NEWLINE …`). **Caveat:** single-line `define f(x) { … }` (no newline after `{`) is therefore rejected even though historical/GNU `bc` accepts it. Verified: `define f(x){<nl>…}` works; `define f(x) { … }` errors. Track as a usability divergence from historical practice, not a spec violation. `grammar.pest:86`.

#### EXTENDED DESCRIPTION — arithmetic & scale (number.rs)
- [x] **Division truncates** (not rounds) — CONFORMS. Verified: `scale=0;7/2`→`3`, `scale=2;2/3`→`0.66`. `number.rs:201-207`. *(Refutes the initial "rounds" hypothesis — `with_scale` truncates toward zero here.)*
- [x] **`sqrt` truncates** — CONFORMS. Verified: `scale=7;sqrt(2)`→`1.4142135`. `number.rs:242-248`.
- [x] Multiplication scale `min(a+b,max(scale,a,b))` — CONFORMS. `number.rs:193-198`.
- [x] Add/sub scale `max(a,b)` (exact) — CONFORMS. `number.rs:185-190`.
- [x] Division scale = `scale` register — CONFORMS. `number.rs:206`.
- [x] Modulus = `a-(a/b)*b`, mathematical when scale 0 — CONFORMS (spot-checked: `scale=0;10%3`→`1`, `scale=2;10.5%3`→`0`). `number.rs:235-240`.
- [x] Power: integer-exponent required, negative exponent supported, right-assoc — CONFORMS. `number.rs:209-233`. (scale of `x^0`: #B8.)
- [x] `length()` (significant digits, incl. trailing-zero integers) — CONFORMS. Verified: `length(1000)`→`4`, `length(0.001)`→`1`. `number.rs:177-179`.
- [x] Numeric-constant scale = fractional digit count — CONFORMS. `number.rs:94-99`.
- [x] **Power `x^0` scale** — #B8 ✓ Phase 6.

#### EXTENDED DESCRIPTION — registers / variables (interpreter.rs)
- [x] `ibase` enforced `2..16`; single-digit `A-F` → hex (`ibase=A`→base 10) — CONFORMS. Verified: `ibase=20` errors; `ibase=A;10`→`10`.
- [x] `obase` lower bound `>=2` — CONFORMS. Verified: `obase=1` errors.
- [x] **`obase` upper bound (`BC_BASE_MAX`)** — #B4 ✓ Phase 5.
- [x] **`scale` upper bound (`BC_SCALE_MAX`)** — #B3 ✓ Phase 5.
- [x] **Array `BC_DIM_MAX`** — #B5 ✓ Phase 5.
- [x] Defaults `ibase=obase=10`, `scale=0`; registers truncate to int — CONFORMS.

#### Output formatting (number.rs)
- [x] obase 2–16 digits `0-9A-F`; negative sign; zero → `0` — CONFORMS.
- [x] obase>16 multi-digit space-separated format — CONFORMS. Verified: `obase=25;1024`→` 01 15 24` (matches spec example `Δ01Δ15Δ24`).
- [x] **70-column wrap** — #B6 ✓ Phase 6.

#### Statements / control flow (interpreter.rs)
- [x] Expression statement prints value; assignment / `SetRegister` suppresses print — CONFORMS. `interpreter.rs:120-127`.
- [x] String statement prints the string — CONFORMS.
- [x] `if`/`while`/`for` (all three `for` exprs required), `break`, `return`/`return(e)`/bare, `define` replaces prior, recursion, `auto` first + pass-by-value — CONFORMS (per read; spot-checked `define`/recursion).
- [x] **`quit`/`break` in `for`-in-function** — #B1 ✓ Phase 3.

#### EXIT STATUS / CONSEQUENCES OF ERRORS
- [x] **Diagnostics on stderr** — #B2 ✓ Phase 4.
- [x] Interactive parse errors recover (REPL continues) — CONFORMS. `bc.rs:91-99`.
- [x] **`quit`-in-`for` no longer crashes recovery** — #B1 ✓ Phase 3.
- [x] Error exit status (#B11), startup `-l` load (#B12) — ✓ Phase 4.

### Test coverage signal (`tests/bc/mod.rs`)
Good breadth on arithmetic, scale, functions, control flow. Tests added in
Phases 3–6 now cover: `quit`/`break` inside a `for` in a function (#B1);
diagnostics channel + missing-file exit code (#B2/#B11); 70-column wrapping
(#B6, unit test); `obase`/`scale`/array-index limit rejection (#B3/#B4/#B5);
`x^0` scale (#B8). `-1^2` (#B7) conforms and matches GNU bc.

---

## Suggested PR groupings

- **PR A — "expr: real expression evaluator"**: #E1 (precedence), #E5 (error exit 2), and wire #E4 (result-driven exit) on the new evaluator. The biggest single correctness win.
- **PR B — "expr: POSIX BRE matching"**: #E2 (BRE + anchor + `\1`), #E6 (char-count length), LC_COLLATE note. Pairs with new `:` tests.
- **PR C — "expr: robustness"**: #E3 (div/rem by zero → exit 2), #E7 (`|` returns expr2), #E9 (`--`), #E10 (localized `expr:` stderr), #E8 (wide ints) as a stretch.
- **PR D — "bc: stop crashing"**: #B1 (`contains_quit` over `For` + remove the `panic!`), #B9 (replace user-reachable `panic!`/`unwrap`). Ship with the missing #B1 test.
- **PR E — "bc: diagnostics to stderr"**: #B2 (all error paths → stderr), #B10 (CTRL-D/CTRL-C noise), #B11 (non-zero error status), #B12. Consider adopting `plib::diag` (mirrors the `dev/` crate work).
- **PR F — "bc: POSIX limits"**: #B3 (`BC_SCALE_MAX`), #B4 (`BC_BASE_MAX`), #B5 (`BC_DIM_MAX`) with a shared limits module.
- **PR G — "bc: output + precedence fidelity"**: #B6 (70-col wrap), #B7 (`-1^2`), #B8 (`x^0` scale).
