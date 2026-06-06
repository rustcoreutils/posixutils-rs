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
- [ ] **#E1 — Operator precedence ignored; strictly left-to-right.** `expr.rs:290-350`. `eval_expression` repeatedly folds `tokens[0..3]` left to right with no precedence/associativity. Verified: `expr 2 + 3 \* 4` → `20` (POSIX requires `14`). Fix: replace the fold with a precedence-climbing/Pratt parser honoring the spec table (`:` > `* / %` > `+ -` > `= > >= < <= !=` > `&` > `|`, all left-associative).
- [ ] **#E2 — `:` matching uses Rust regex, not anchored POSIX BRE.** `expr.rs:236-265`. `Regex::new(&rhs)` then `re.captures` (unanchored, ERE-ish). Verified: `expr "//abc/file" : '.*/\(.*\)'` → `0` (POSIX `file` — `\(...\)` is a literal paren to Rust regex); `expr abcd : bc` → `2` (POSIX `0`, patterns are anchored to start). Fix: compile as BRE, anchor at string start, capture `\1`.
- [ ] **#E3 — Divide / remainder by zero panics (exit 101), no diagnostic.** `expr.rs:210-211` (`i1 / i2`, `i1 % i2`). Verified: `expr 6 / 0` panics. Fix: check `i2 == 0` and return an error → exit 2.
- [ ] **#E4 — Exit status never reflects the result value.** `expr.rs:365-378`. `main` returns `Ok(())` whenever evaluation succeeds, so exit is always `0`. Verified: `expr 0` → exit `0`, `expr 1 = 2` → exit `0` (POSIX requires exit `1` when the result is null or zero). Fix: after printing, `exit(1)` if the result is the empty string or numeric zero, else `exit(0)`.

#### Major
- [ ] **#E5 — Error exit code is 1, not 2/>2.** `expr.rs:372` (Err bubbles out of `main` → Rust `ExitCode::FAILURE` = 1). POSIX: `2` = invalid expression, `>2` = other error. Verified: `expr 1 +` → exit `1`. Fix: map invalid-expression errors to `exit(2)`.
- [ ] **#E6 — `:` match length counts bytes, not characters.** `expr.rs:261` (`caps.get(0).unwrap().len()`). Verified: `expr éé : '.*'` → `4` (POSIX `2`). Fix: count `chars()` of the matched slice (LC_CTYPE-aware).

#### Minor
- [ ] **#E7 — `|` does not return expr2's value when expr1 is null/zero.** `expr.rs:216-233` (`logop`): the `|` arm returns `Token::Integer(0)` when both sides are zero/null instead of the (string) value of expr2. e.g. `expr 0 \| ""` yields `0`, POSIX yields the empty string. Fix: return `rhs` (the evaluated expr2), not a synthesized `0`.
- [ ] **#E8 — Integer range limited to `i64`; silent wrap / literal demotion.** `expr.rs:31,125,207-211`. Out-of-`i64` literals fall through to `Str` (then arithmetic errors); in-range arithmetic wraps in release (`overflow-checks` off). Verified: `expr 99999999999999999999 + 1` → "not an integer". POSIX expr arithmetic is conventionally arbitrary/wide. Fix: use a wide/bignum integer and detect overflow.
- [ ] **#E9 — `--` end-of-options not honored.** `expr.rs:133-145` tokenizes every arg literally. Verified: `expr -- -3 + 1` → "wanted operator" (`--` became a string operand). POSIX (XBD 12.2 Guideline 10, cited in APPLICATION USAGE) expects `--` to protect leading-minus operands. Fix: strip a single leading `--` before tokenizing.
- [ ] **#E10 — Runtime diagnostics not localized / odd prefix.** `expr.rs:365` + the `&'static str` error strings. Errors surface as `Error: "syntax error: ..."` via the `Box<dyn Error>` return, on stderr. LC_MESSAGES has no effect on them. Fix: print `expr: <message>` to stderr via a localized surface.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing
- [x] No options defined; operands collected from argv — CONFORMS. `expr.rs:133-145`.
- [ ] **`--` end-of-options MISSING** — #E9.

#### Operators / EXTENDED DESCRIPTION
- [ ] **Precedence/associativity DIVERGES** — #E1.
- [x] Operator token set complete (`( ) | & = > >= < <= != + - * / % :`) — CONFORMS. `expr.rs:107-130`.
- [x] Integer-vs-string identification for arithmetic/comparison operands — CONFORMS. `expr.rs:179-199` (`cmpop` falls back to string compare; `intop` requires ints).
- [x] Unary-minus integer literals (`-5`) — CONFORMS. `expr.rs:125-128`.
- [ ] **`|` null/zero return value DIVERGES** — #E7. `&` arm CONFORMS.
- [ ] **`:` matching DIVERGES (regex flavor, anchor, byte length)** — #E2, #E6.
- [x] `length`/`substr`/`index`/`match` keywords — N/A (spec: unspecified results); current behavior is to treat them as strings → syntax error, which is acceptable.

#### STDIN / INPUT FILES / ENVIRONMENT
- [x] STDIN not used — CONFORMS.
- [x] `setlocale(LC_ALL,"")` + `textdomain` present — CONFORMS. `expr.rs:366-368`.
- [ ] **LC_COLLATE not honored for `=`/`!=` string compare** — DIVERGES (uses Rust `==`/`Ord`, byte order). `expr.rs:166-173`. Minor; fold into #E6/i18n work.

#### STDOUT / STDERR
- [x] Result + `<newline>` to stdout — CONFORMS. `expr.rs:375`.
- [ ] **Diagnostics prefix/localization** — #E10 (channel is stderr — OK).

#### EXIT STATUS / CONSEQUENCES OF ERRORS
- [ ] **Result-driven 0/1 status MISSING** — #E4.
- [ ] **Invalid-expression status (2) wrong** — #E5.
- [ ] **Divide-by-zero panic** — #E3.

### Test coverage signal (`tests/expr/mod.rs`)
Covered: logical ops, integer ops, integer compare, string compare (4 cases each).
Not covered:
- [ ] Operator precedence (`a + b * c`, mixed compare/arith).
- [ ] `:` matching operator at all (anchoring, `\(...\)` capture, byte vs char length).
- [ ] Divide/remainder by zero.
- [ ] Exit-status assertions (0 vs 1 vs 2).
- [ ] `--` handling; multibyte operands.

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
array index → unbounded allocation), missing **70-column line wrapping**, and
unary-minus binding tighter than `^`.

### Priority issues

#### Critical
- [ ] **#B1 — `quit`/`break` inside a `for` body within a function panics.** `interpreter.rs:280` (`panic!("reached quit or break in function call")`); root cause `interpreter.rs:111-118` (`contains_quit` recurses into `If`/`While` but **not** `For`). Verified: `define f(x){<nl>for(i=0;i<1;i++){<nl>quit<nl>}<nl>}<nl>f(0)` aborts with a Rust panic. POSIX: `quit` must stop execution; interactive `bc` must recover from errors, never crash. Fix: extend `contains_quit` (and break-detection) to `StmtInstruction::For { body, .. }`, or propagate the control-flow result instead of panicking.
- [ ] **#B2 — All diagnostics written to stdout, not stderr.** `bc.rs:39-40` (runtime errors + partial output), `bc.rs:66` (file parse errors), `bc.rs:96-97` (interactive parse errors) all use `print!`/`println!`. Spec line 87120: "standard error shall be used only for diagnostic messages." Verified: `echo '1/0' | bc` prints `runtime error … division by zero` on **stdout** (stderr empty). Fix: route every diagnostic through `eprintln!`. (Only `bc.rs:69` file-not-found is correct.)

#### Major
- [ ] **#B3 — `scale` upper bound (`BC_SCALE_MAX`) not enforced.** `interpreter.rs:~360` (`SetRegister(Scale)` accepts any `as_u64()`). Verified: `scale=100000000000; scale` → accepted; a subsequent `/`/`sqrt`/`pow` would allocate unboundedly. Fix: clamp/reject `scale > BC_SCALE_MAX`.
- [ ] **#B4 — `obase` upper bound (`BC_BASE_MAX`) not enforced.** `interpreter.rs:~374` (only `>= 2` checked). Verified: `obase=100000` accepted. Fix: reject `obase > BC_BASE_MAX`.
- [ ] **#B5 — Array index unbounded (`BC_DIM_MAX` not enforced).** `interpreter.rs:129-133` (`get_or_extend` resizes Vec to `index+1`), `:200-201` (only rejects values exceeding `u64`). `a[99999999]=1` would attempt a 100M-element allocation. Fix: reject `index >= BC_DIM_MAX`.
- [ ] **#B6 — 70-column line wrapping with `\`-continuation MISSING.** `number.rs` `to_string` emits one line. Spec lines 87368-87370. Verified: `obase=2; 2^200` → a single 201-char line. Fix: wrap output at 70 cols in the POSIX locale, ending continued lines with `\`.
- [ ] **#B7 — Unary minus binds tighter than `^`.** `parser.rs:27` (Pratt: `neg` registered as prefix above `pow`). Verified: `-1^2` → `1` (i.e. `(-1)^2`); standard/GNU `bc` and mathematical convention give `-(1^2) = -1`. Fix: rank unary minus below `^` in the Pratt table.

#### Minor
- [ ] **#B8 — `x^0` result scale is the `scale` register, not 0.** `number.rs:220` (`if other.0.is_positive()` is `false` for exponent `0`, so the `else` branch uses the `scale` register). Spec: `b>=0` → `min(a*b, max(scale,a))` = `0` when `b=0`. Verified: `scale=5; 2.5^0` → `1.00000` (should be `1`). Fix: use `!other.0.is_negative()` for the `b>=0` branch.
- [ ] **#B9 — Additional `panic!`/`unwrap` reachable on adversarial input.** `number.rs:10` (`to_digit` panics on out-of-range digit), `number.rs:22` (`to_char` for `val>15`), `interpreter.rs:~552-556` (`return`/`break` outside context). Mostly pre-screened by the parser, but spec requires interactive recovery; prefer `Result` returns. Minor (defense in depth).
- [ ] **#B10 — REPL prints `CTRL-D`/`CTRL-C` to stdout on EOF/interrupt.** `bc.rs:104,108`. Pollutes the output stream. Fix: drop or send to stderr.
- [ ] **#B11 — Exit status is 0 even after a file-not-found / error.** `bc.rs:68-72` returns `Ok(())`; `main`'s `rustyline::Result` exits 0. Spec leaves error status "unspecified," so this is *allowed*, but a non-zero status is friendlier. Minor.
- [ ] **#B12 — `math_functions.bc` loaded with `.expect()`.** `bc.rs:57-59` panics at startup if the embedded `-l` library fails to parse/exec. Developer gate, not user input. Minor.

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
- [ ] **Power `x^0` scale DIVERGES** — #B8.

#### EXTENDED DESCRIPTION — registers / variables (interpreter.rs)
- [x] `ibase` enforced `2..16`; single-digit `A-F` → hex (`ibase=A`→base 10) — CONFORMS. Verified: `ibase=20` errors; `ibase=A;10`→`10`.
- [x] `obase` lower bound `>=2` — CONFORMS. Verified: `obase=1` errors.
- [ ] **`obase` upper bound MISSING** — #B4.
- [ ] **`scale` upper bound MISSING** — #B3.
- [ ] **Array `BC_DIM_MAX` MISSING** — #B5.
- [x] Defaults `ibase=obase=10`, `scale=0`; registers truncate to int — CONFORMS.

#### Output formatting (number.rs)
- [x] obase 2–16 digits `0-9A-F`; negative sign; zero → `0` — CONFORMS.
- [x] obase>16 multi-digit space-separated format — CONFORMS. Verified: `obase=25;1024`→` 01 15 24` (matches spec example `Δ01Δ15Δ24`).
- [ ] **70-column wrap MISSING** — #B6.

#### Statements / control flow (interpreter.rs)
- [x] Expression statement prints value; assignment / `SetRegister` suppresses print — CONFORMS. `interpreter.rs:120-127`.
- [x] String statement prints the string — CONFORMS.
- [x] `if`/`while`/`for` (all three `for` exprs required), `break`, `return`/`return(e)`/bare, `define` replaces prior, recursion, `auto` first + pass-by-value — CONFORMS (per read; spot-checked `define`/recursion).
- [ ] **`quit`/`break` in `for`-in-function panics** — #B1.

#### EXIT STATUS / CONSEQUENCES OF ERRORS
- [ ] **Diagnostics on stdout** — #B2.
- [x] Interactive parse errors recover (REPL continues) — CONFORMS. `bc.rs:91-99`.
- [ ] **`quit`-in-`for` crash defeats recovery** — #B1.
- [ ] Error exit status (#B11), startup `.expect` (#B12) — Minor.

### Test coverage signal (`tests/bc/mod.rs`, 49 tests)
Good breadth on arithmetic, scale, functions, control flow. Not covered:
- [ ] `quit`/`break` inside a `for` body within a function (the #B1 panic).
- [ ] Diagnostics-channel assertion (stdout vs stderr) (#B2).
- [ ] 70-column line wrapping (#B6).
- [ ] `obase`/`scale` upper-bound and array-index bound rejection (#B3/#B4/#B5).
- [ ] `-1^2` precedence (#B7); `x^0` scale (#B8).

---

## Suggested PR groupings

- **PR A — "expr: real expression evaluator"**: #E1 (precedence), #E5 (error exit 2), and wire #E4 (result-driven exit) on the new evaluator. The biggest single correctness win.
- **PR B — "expr: POSIX BRE matching"**: #E2 (BRE + anchor + `\1`), #E6 (char-count length), LC_COLLATE note. Pairs with new `:` tests.
- **PR C — "expr: robustness"**: #E3 (div/rem by zero → exit 2), #E7 (`|` returns expr2), #E9 (`--`), #E10 (localized `expr:` stderr), #E8 (wide ints) as a stretch.
- **PR D — "bc: stop crashing"**: #B1 (`contains_quit` over `For` + remove the `panic!`), #B9 (replace user-reachable `panic!`/`unwrap`). Ship with the missing #B1 test.
- **PR E — "bc: diagnostics to stderr"**: #B2 (all error paths → stderr), #B10 (CTRL-D/CTRL-C noise), #B11 (non-zero error status), #B12. Consider adopting `plib::diag` (mirrors the `dev/` crate work).
- **PR F — "bc: POSIX limits"**: #B3 (`BC_SCALE_MAX`), #B4 (`BC_BASE_MAX`), #B5 (`BC_DIM_MAX`) with a shared limits module.
- **PR G — "bc: output + precedence fidelity"**: #B6 (70-col wrap), #B7 (`-1^2`), #B8 (`x^0` scale).
