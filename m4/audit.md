# POSIX.1-2024 Conformance Audit — `m4`

**Implementation:** `m4/` crate (~3.4 kloc): `main.rs` (19), `lib.rs` (201, argv +
`run`/`run_impl`), `main_loop.rs` (223, the tokenizer/expander), `state.rs` (125),
`input.rs` (244, byte-at-a-time reader + pushback), `output.rs` (249, divert
buffers 1–9, in-memory), `lexer.rs` (160, `MacroName` + libc ctype), `precedence.rs`,
`error.rs` (158), and `macros/{mod.rs 229, builtin.rs 1066, eval.rs 190,
user_defined.rs 99, trace.rs 79}`.
**Tests:** `m4/tests/integration.rs` (282) driving `m4/fixtures/integration_tests/*`
(≈60 enabled fixtures, 9 ignored, 7 expect-error). No inline `#[test]` units.
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 3090–3100.
**Reference:** no sliced spec tree exists on this host; the `m4` section was
extracted from `~/tmp/POSIX.2024.pdf` (`pdftotext` lines 150010–150620) into
`/tmp/m4-spec.txt`.
**Date:** 2026-06-11
**Method:** spec read in full; every implementation file read paired with the
spec; each Critical/Major finding confirmed by building `target/release/m4` and
running it (behavioral evidence inline). No code was modified. Extensions are
flagged only where they diverge from required behavior (per `audits.md`).

## TL;DR

The macro engine is broad and the common path is solid: the spec's own worked
EXAMPLES (`-D`/`-U`, `ifdef`), `$0`/`$1`–`$9`/`$*`/`$@`/`shift`, `define`/`pushdef`/
`popdef`/`undefine`, `divert`/`undivert` (numerical-order flush at EOF), `dnl`,
`ifelse` multi-pair restart, `incr`/`decr`, `eval` with parentheses/bitwise/
short-circuit, `include`/`sinclude`, `m4exit` (including the Issue-8 non-zero-after-
error rule), and `traceon`/`errprint`/`dumpdef` stderr routing all behave per
spec. The conformance gaps are concentrated and concrete. **Two reachable panics**
abort the process (`index` with an empty second argument; `eval` division/modulo
by zero) — Critical. **`eval` is materially incomplete**: it ignores the radix
(2nd) and min-digit (3rd) arguments, rejects octal/hex constants, and gives shift
and relational operators equal precedence. **`$#` is wrong for `macro()`** (reports
0 where the spec's EXAMPLE 2 requires 1). **`m4wrap` text is emitted verbatim, not
rescanned.** **`defn` cannot reproduce built-ins**, defeating the rename idiom the
spec calls out. A cross-cutting theme: recoverable "shall be an error" conditions
and an unopenable file operand **abort the whole run** with raw Rust diagnostics
instead of continuing, and all string handling is **byte-, not character-based**
(`LC_CTYPE`/`setlocale` absent).

## Priority issues

### Critical

- [x] **#1 — `index` with an empty second argument panics and aborts the process.** `m4/src/macros/builtin.rs:588-590` computes `first_arg.windows(second_arg.len())`; `slice::windows(0)` panics ("window size must be non-zero"). Verified: `index(\`abc',\`')` → `thread 'main' panicked at builtin.rs:589 … window size must be non-zero` (exit 101). An empty search string arises naturally from a nested expansion (`index(x, substr(...))`). GNU m4 returns `0`. Fix: special-case an empty second argument to position `0` before the `windows` call. **✓ Fixed (Phase A):** empty needle → `0` before the `windows` call; fixture `index_empty_second_arg`.
- [x] **#2 — `eval` division or modulo by zero panics and aborts the process.** `m4/src/macros/eval.rs:155-156` evaluate `lhs / rhs` / `lhs % rhs` directly; integer `/0` and `%0` panic in release as well as debug. Verified: `eval(1/0)` → `panicked at eval.rs:155 … attempt to divide by zero` (exit 101); `eval(1%0)` → `eval.rs:156` panic. POSIX APPLICATION USAGE (pp. 3096) treats divide-by-zero as undefined, but GNU m4 emits `divide by zero` and continues; a Rust panic with backtrace is a crash. Fix: detect a zero divisor, write a diagnostic, and yield a value (or recoverable error) without panicking. (Note: signed-overflow multiply wraps and over-wide shifts mask in release, so only `/0` and `%0` are release-panics; the same operators panic in debug/`cargo test`.) **✓ Fixed (Phase A):** `/0`,`%0` return a recoverable error and all eval arithmetic is now `wrapping_*` (no debug or release panics); a bad eval expression emits `m4:<file>:<line>: bad expression in eval: …`, expands to empty, sets a non-zero exit status, and continues (new `State::emit_error` infra + `run_impl` exit-status check). Fixtures `eval_divide_by_zero`, `eval_modulo_by_zero`.

### Major

- [x] **#3 — `eval` ignores its second (radix) and third (min-digits) arguments.** `EvalMacro::evaluate` (`m4/src/macros/eval.rs:23-37`) consumes only `args.next()` (the expression) and always emits base-10. POSIX (pp. 3093-3094): "The second argument … shall set the radix for the result … The third argument … sets the minimum number of digits in the result." Verified: `eval(10,2)` → `10` (want `1010`); `eval(7,10,8)` → `7` (want `00000007`). Fix: parse args 2–3, format the result in the requested radix (2–36) zero-padded to the requested width. **✓ Fixed (Phase B):** new `format_radix` (lowercase, GNU-style, sign-aware, `i64::MIN`-safe) + `parse_optional_decimal`; radix 2–36 default 10, width default 1; out-of-range radix / negative width / non-numeric → diagnostic + continue. Fixture `eval_radix`.
- [x] **#4 — `eval` does not accept octal or hexadecimal constants.** The numeric leaf parser `parse_positive_integer` (`eval.rs:50-52`, used at `eval.rs:146`) takes only ASCII decimal digits. POSIX (pp. 3093): "Systems shall support octal and hexadecimal numbers as in the ISO C standard." Verified: `eval(0xff)` → `Error code: Eof … Error parsing input` (exit 1, aborts); `eval(010)` → `10` (C octal would be `8`). Fix: recognise `0x`/`0X` hex and leading-`0` octal in the eval lexer. **✓ Fixed (Phase B):** new eval-only `parse_eval_number` accepts `0x`/`0X` hex, leading-`0` octal, and GNU `0b`/`0B` binary; the decimal-only built-ins (incr/decr/divert) are untouched, honoring "except for the first argument to eval … decimal." Fixture `eval_base_literals`.
- [x] **#5 — `eval` gives shift and relational operators equal precedence.** `eval.rs:130-135` assigns `<<`, `>>`, `<`, `<=`, `>`, `>=` all to precedence level 5; in C/§1.1.2.1 shift binds tighter than relational. Verified: `eval(2 < 1 << 3)` → `0` (C: `2 < (1<<3)` = `1`). Fix: place the shift operators at a level between additive (3) and relational. (Other operator levels — unary, `* / %`, `+ -`, `== !=`, `&`, `^`, `|`, `&&`, `||` — are ordered correctly; `eval((2+3)*4)`, `eval(1||2)`, `eval(~0)`, `eval(!5)` all verified correct.) **✓ Fixed (Phase B):** `<<`/`>>` moved to level 4 (between additive 3 and relational 5); `eval(2 < 1 << 3)` → `1`. Fixture `eval_shift_precedence`.
- [x] **#6 — `$#` is `0` for a macro called with empty parentheses `macro()`; the spec requires `1`.** Empty-paren calls never create the single empty argument: the `(`/`)` handling in `m4/src/main_loop.rs:130-161` leaves `frame.args` empty, and `$#` is `frame.args.len()` (`m4/src/macros/user_defined.rs:46-48`). POSIX (pp. 3092) and EXAMPLE 2 (pp. 3097-3098, `macro()a` → "called with 1 Arguments") require `$#` = 1 for `()`. Verified: `define(\`macro',\`$# args')macro()` → `0 args` (want `1 args`); `macro` (no parens) → `0 args` (correct); `macro(a,b)` → `2 args` (correct). Fix: when the opening `(` is consumed, seed `frame.args` with one empty argument. **✓ Fixed (Phase C):** the frame pushed on `(` seeds one empty argument (no-paren calls keep 0 args via the un-seeded `evaluate` path); spec EXAMPLE 2 now prints "called with 1 Arguments". Fixture `dollar_hash_args`.
- [x] **#7 — `m4wrap` text is written verbatim at EOF, not rescanned.** `m4/src/main_loop.rs:218-220` does `state.output.write_all(wrap)` over the stored arguments; POSIX (pp. 3094): "The first argument shall be processed when EOF is reached." "Processed" means re-tokenised/expanded. Verified: `m4wrap(\`incr(5)')` → `incr(5)` (want `6`); `define(\`x',\`7')m4wrap(\`x')` → `x` (want `7`). Fix: push each wrap string back onto the input and run it through the main loop (FIFO order is already correct via the `Vec`). **✓ Fixed (Phase C):** at final EOF the queued wrap text (drained FIFO) is pushed back onto the input and rescanned through the main loop (so it expands macros and may queue further m4wrap), then diversions are flushed. Fixture `m4wrap_rescan`.
- [x] **#8 — `defn` cannot reproduce built-in macros.** `DefnMacro::evaluate` (`m4/src/macros/builtin.rs:193-221`) only acts inside `if let MacroDefinitionImplementation::UserDefined`; for a built-in it pushes nothing, yielding the empty string. POSIX APPLICATION USAGE (pp. 3096): "The defn macro is useful for renaming macros, especially built-ins." Verified: `define(\`myincr',defn(\`incr'))myincr(5)` → empty output (want `6`). Fix: have `defn` of a built-in produce a token/quoted form that `define`/`pushdef` can store as a built-in alias. **✓ Fixed (Phase C):** `defn` of a built-in emits a quoted internal marker (`\x01name\x02`) that `define`/`pushdef` decode back into the built-in (preserving its `min_args`); a marker that instead reaches real output is voided (GNU behavior) by an `OutputState` filter. Fixture `defn_builtin_rename`. (Edge: a built-in token passed to a non-define macro, e.g. `len(defn(\`incr'))`, is not yet voided inside argument collection.)
- [ ] **#9 — Recoverable errors abort the entire run (with raw diagnostics) instead of continuing; an unopenable file operand aborts before any input is processed.** Any macro returning `Err` propagates out of `main_loop` → `run_impl` → process exit (`m4/src/lib.rs:141-153`, message via `format!("{error:#}")`). The file operand is opened with `?` at `m4/src/lib.rs:171` before the loop, so the first unreadable operand aborts the whole invocation. Verified: `m4 nope.m4 m4src` → `Error processing io … No such file or directory` (exit 1, `m4src` never processed — GNU prints `m4: cannot open …` and continues); `incr(foo)after` → `Error code: Fail … Error parsing input` (exit 1, `after` lost); `substr(\`hello',\`-1')` and `divert(10)` likewise abort. CONSEQUENCES OF ERRORS is "Default" (so abort-vs-continue is latitude), but the diagnostics are raw `ErrorKind` debug text with no `m4:` prefix and the behaviour diverges from every historical m4. Fix: convert per-macro argument errors into diagnostics that set the final exit status and continue; continue past an unreadable file operand.
- [ ] **#10 — String length/position/transliteration are byte-based; `LC_CTYPE` multibyte handling is absent.** `len` (`builtin.rs:560-562`), `index` (`588-590`), `substr` (`664-701`), and `translit` (`637-647`) all index raw bytes; no `setlocale` is ever called (see #13). POSIX (pp. 3090) lists `LC_CTYPE` as governing "interpretation of sequences of bytes of text data as characters." Verified: `len(é)` → `2` (UTF-8 bytes; want char count `1`). Macro-name scanning likewise uses byte `libc::isalpha` under the default "C" locale (`lexer.rs:131-159`). Fix: count/index by character under the active locale (mirrors the `awk` byte-vs-char remediation in `awk/audit.md`).

### Minor

- [ ] **#11 — `translit` with a single argument emits `0` and a mis-copied warning.** `builtin.rs:626-633` (the missing-second-arg arm, copied from `index`) writes `Warning too few arguments for index macro` and pushes back `0`. Verified: `translit(\`abc')` → `Warning too few arguments for index macro0`. Expected: the first argument unchanged (`abc`). Fix: with no second argument, push back the first argument verbatim.
- [ ] **#12 — `changecom` with a single argument does not reset the end-comment string to `<newline>`.** `builtin.rs:316-342` sets `comment_close_tag` only when `args_len >= 2`; a prior two-arg `changecom` leaves a stale close tag in effect. POSIX (pp. 3092): "With a single non-null argument, that argument shall become the begin-comment and the `<newline>` shall become the end-comment string." Fix: in the one-argument branch, set the close tag to `\n`.
- [ ] **#13 — No `setlocale`/`gettext`: `LC_MESSAGES`, `LC_CTYPE`, `NLSPATH`, and `LANG` are ignored and all diagnostics are hardcoded English.** Confirmed: no `setlocale`/`gettext`/`textdomain` and no `LC_*`/`NLSPATH`/`LANG` reads anywhere in `m4/src` (`main.rs` only calls `env_logger::init`). POSIX ENVIRONMENT VARIABLES (pp. 3090) lists all four. Fix: add `setlocale(LC_ALL, "")` at startup and route diagnostics through `gettext` (and through `stderr` with an `m4:` prefix, cf. #9).
- [ ] **#14 — `divert` to a stream number greater than 9 aborts.** `builtin.rs:971-975` and `output.rs:132-140` return an error for `>9`. POSIX (pp. 3093): "Behavior is implementation-defined if a stream number larger than 9 is specified" — erroring is permitted, but aborting the whole run (rather than discarding, like a negative stream, or supporting it like GNU) is user-hostile. Verified: `divert(10)hello` → `Error parsing first argument for \`divert' …` (exit 1). Fix: treat `>9` as discard, or support it.
- [ ] **#15 — Invalid `define`/macro-name conditions are silent by default.** `DefineMacro::define` (`builtin.rs:60-75`) reports an invalid name or missing definition only via `log::warn!`, which is gated behind `env_logger`/`RUST_LOG` (`main.rs:6`); with logging off, `define(\`1bad',x)` is silently dropped. Fix: emit such warnings to `stderr` unconditionally.
- [ ] **#16 — `-D`/`-U` are all applied before any file operand is processed, so they cannot interleave with operands.** `m4/src/lib.rs:179-195` applies the (correctly order-sorted) `-D`/`-U` directives in a block before `main_loop`. POSIX (pp. 3090): "options can be interspersed with operands." Verified the relative `-D`/`-U` order and basic interspersed parsing work (`m4 m4src -D VER=2` → defined), but `m4 -D X f1 -U X f2` would undefine `X` before `f1` is read. Fix: process `-D`/`-U` and file operands in a single command-line-ordered pass. (Lower-impact; most scripts define before the first operand.)
- [ ] **#17 — Several warnings lack an `m4:` prefix and a trailing newline.** `changecom`/`changequote` excess-arg warnings (`builtin.rs:345,369`), `ifelse` too-few/excess (`453,479`), and the `index`/`translit` warnings (`582,629`) are written without a newline and without the utility-name prefix, so they smear into following output/diagnostics. Fix: standardise on `m4: <message>\n`.

### Extensions present (informational — N/A)

- [x] `__file__` built-in (`mod.rs:121`, `builtin.rs:1053-1066`) — GNU-style extension; harmless.
- [x] `maketemp` built-in aliased to `mkstemp` (`mod.rs:89-90`) — POSIX Issue 8 (Austin Defect 1330) *removed* the obsolescent `maketemp`; provided here as an extension that creates+closes a file (unlike historical `maketemp`). Harmless but non-standard.
- [x] Divert buffers held in memory rather than temp files (`output.rs:80-93`) — spec-neutral implementation choice; noted by the code itself.

## Detailed conformance matrix

### Options / argv
- [x] `-s` line synchronization CONFORMS — emits `#line N "name"` directives (`input.rs:202-236`, `output.rs:218-226`); spec pp. 3090.
- [x] `-D name[=val]` CONFORMS — defines to value or null (`lib.rs:179-190`, `ArgumentDefine::parse` `lib.rs:25-52`).
- [x] `-U name` CONFORMS — undefines (`lib.rs:191-193`).
- [x] `-D`/`-U` relative order significant CONFORMS — directives sorted by CLI index (`lib.rs:114-126`); spec pp. 3090 ("order … shall be significant").
- [x] Options interspersed with operands (basic) CONFORMS — `m4 m4src -D VER=2` verified.
- [ ] **#16 PARTIAL** — `-D`/`-U` applied as a block before file processing; cannot interleave with operands.

### Operands / STDIN / input files
- [x] `file` operand processed in order CONFORMS — `lib.rs:167-177`; `two_files` fixture.
- [x] No operand / `-` reads stdin CONFORMS — `lib.rs:162-166` (empty → stdin). (`-` literal routing relies on shell; default-stdin path verified.)
- [x] Input is read as a byte text file CONFORMS — `input.rs:182-200` (streamed one byte at a time; a performance, not conformance, concern).
- [ ] **#9 (operand half) DIVERGES** — an unreadable file operand aborts the run before processing instead of diagnosing and continuing.

### Environment variables
- [ ] **#13 `LANG` MISSING** — not read.
- [ ] **#13 `LC_ALL` MISSING** — not read.
- [ ] **#13 `LC_CTYPE` MISSING** — not read; byte semantics only (#10).
- [ ] **#13 `LC_MESSAGES` MISSING** — not read; English diagnostics.
- [ ] **#13 `NLSPATH` (XSI) MISSING** — not read.

### Asynchronous events
- [x] Default signal handling N/A — spec ASYNCHRONOUS EVENTS = "Default" (pp. 3090); m4 is non-interactive, so no custom handlers are required. (No findings.)

### STDOUT / STDERR
- [x] Processed output to stdout CONFORMS — `output.rs:196-230`.
- [x] `errprint` → stderr CONFORMS — `builtin.rs:225-235`.
- [x] `traceon`/`traceoff` trace → stderr CONFORMS — `trace.rs:64-78` ("unspecified format" satisfied).
- [x] `dumpdef` defined text → stderr CONFORMS — `builtin.rs:708-753`.
- [ ] **#9/#13 PARTIAL** — diagnostic messages are English, un-prefixed, and not `LC_MESSAGES`-aware.

### Extended description — macro recognition
- [x] Name token = `[_a-zA-Z][_a-zA-Z0-9]*`, longest match CONFORMS — `lexer.rs:85-139`, `state.rs:46-71`.
- [x] Call requires `(` immediately after name; bare name = call with zero args CONFORMS — `main_loop.rs:86-110`.
- [x] Quote stripping, nested quotes, comment passthrough CONFORMS — `main_loop.rs:20-85`; `changequote`/`changecom` fixtures.
- [x] Unquoted comma/paren argument splitting; leading-whitespace trim CONFORMS — `main_loop.rs:130-180` (matches EXAMPLE 3, pp. 3098).
- [x] Arguments expanded during collection unless quoted CONFORMS — inner frames write into the outer arg buffer (`output.rs:14-37`); EXAMPLES 4–6 logic.
- [x] `$0`=name, `$1`–`$9`, `$*`, `$@` (quoted) CONFORMS — `user_defined.rs:44-84`; verified `$@`/`$*`/`shift`.
- [x] `${` unspecified → emitted literally CONFORMS — `user_defined.rs:80-83`.
- [ ] **#6 DIVERGES** — `$#` = 0 for `macro()` (spec requires 1).

### Extended description — built-in macros
- [x] `define` (preserve-current semantics), `pushdef`, `popdef`, `undefine` CONFORM — `builtin.rs:48-186`; fixtures `define_*`.
- [ ] **`defn` #8 PARTIAL** — user-defined only; cannot reproduce built-ins (`builtin.rs:193-221`).
- [x] `ifdef`, `ifelse` (3/4-5/6+ restart) CONFORM — `builtin.rs:435-527`; verified.
- [x] `incr`, `decr` CONFORM — `builtin.rs:389-433`; verified.
- [ ] **`eval` #2/#3/#4/#5** — panics on `/0`,`%0`; ignores radix/min-digit args; no octal/hex; shift vs relational precedence wrong.
- [x] `index` normal case CONFORMS (`builtin.rs:573-601`) — but **#1** panics on empty needle and **#10** counts bytes.
- [x] `len` CONFORMS (`builtin.rs:553-565`) — but **#10** counts bytes.
- [x] `substr` (start/len, beyond-end → null) CONFORMS (`builtin.rs:664-702`) — but **#9** aborts on non-numeric/negative and **#10** indexes bytes.
- [x] `translit` (literal `-`, allowed per RATIONALE pp. 3096) CONFORMS (`builtin.rs:611-652`) — but **#11** single-arg bug and **#10** byte-based.
- [x] `divert`/`divnum`/`undivert` (numerical-order EOF flush, negative=discard, undivert-into-buffer) CONFORM — `builtin.rs:953-1051`, `output.rs:142-163`; verified — but **#14** aborts for `>9`.
- [x] `dnl` CONFORMS — `builtin.rs:24-39`.
- [x] `include` (error if unreadable) / `sinclude` (silent) CONFORM — `builtin.rs:241-303`.
- [x] `syscmd` (`sh -c`, no redirection) / `sysval` CONFORM — `builtin.rs:890-944`.
- [x] `mkstemp` (create+close, error → diagnostic + non-zero exit, empty defining text) CONFORMS — `builtin.rs:764-836`; verified exit 1.
- [ ] **`m4wrap` #7 DIVERGES** — emitted verbatim, not rescanned (`main_loop.rs:218-220`).
- [x] `traceon`/`traceoff` CONFORM — `trace.rs`.
- [x] `dumpdef` CONFORMS — `builtin.rs:708-753` (HashMap iteration order for the no-arg form is unspecified, acceptable).

### Exit status / consequences of errors
- [x] Success → 0; `m4exit` sets the code CONFORMS — verified `m4exit(3)` → exit 3.
- [x] `m4exit(0)`/no-arg after a prior error → non-zero CONFORMS — Austin Defect 984 (pp. 3094); `M4exitMacro` `builtin.rs:845-865`; verified via `mkstemp` failure → exit 1.
- [ ] **PARTIAL** — the "prior error occurred" flag (`state.exit_error`) is set only by `mkstemp` (`builtin.rs:831`); most other recoverable errors abort instead (#9), so the `m4exit(0)`-after-error rule covers only the `mkstemp` case.
- [x] Errors → >0 CONFORMS — non-`Exit` errors map to exit 1 (`error.rs:131-147`).

## Test coverage signal

Enabled fixtures cover define/pushdef/popdef, divert/undivert (incl. nested), eval,
ifelse, ifdef, incr/index/len/substr/translit, dnl, shift, m4wrap, m4exit, include/
sinclude, trace, and recursion. Gaps (no fixture exercises the verified defects):

- [ ] `index`/`substr` with an **empty or zero-length** second argument (would catch #1).
- [ ] `eval` **division/modulo by zero** (would catch #2).
- [ ] `eval` **radix/min-digit** arguments and **octal/hex** literals (#3, #4).
- [ ] `eval` **shift-vs-relational** precedence, e.g. `2 < 1 << 3` (#5).
- [ ] `$#` for **`macro()`** empty parentheses — the spec's EXAMPLE 2 (#6).
- [ ] `m4wrap` whose argument **contains a macro call** (#7); the enabled `m4wrap` fixture does not exercise rescanning.
- [ ] `defn` of a **built-in** for renaming (#8).
- [ ] **Continue-after-error** behaviour and unreadable **file operand** (#9).
- [ ] **Multibyte** `len`/`index`/`substr` under a UTF-8 locale (#10).
- [ ] `translit` with a **single argument** (#11); `changecom` single-arg close reset (#12).
- [ ] The 9 `m4_test_ignore!` fixtures (`synclines_1/2`, `syscmd_sysval`, `define_eval_order_*`, `bsd`, `bsd_math`) remain disabled — known divergences to revisit.

## Suggested PR groupings

- **PR A — "Stop the crashes"** (#1, #2): guard `index` empty-needle and `eval` `/0`/`%0`; add regression fixtures. Smallest, highest-value.
- **PR B — "Make `eval` conform"** (#3, #4, #5): radix + min-digit output, octal/hex constants, shift precedence. Self-contained in `eval.rs`/`precedence.rs`.
- **PR C — "Argument & wrap semantics"** (#6, #7, #8): seed one empty arg on `(`; rescan `m4wrap`; `defn` of built-ins. Touches `main_loop.rs`/`user_defined.rs`/`builtin.rs`.
- **PR D — "Errors that don't abort"** (#9, #15, #17, partial #14): convert per-macro argument errors and unreadable operands into `m4:`-prefixed diagnostics that set exit status and continue; surface `define` warnings.
- **PR E — "Locale & characters"** (#10, #13): `setlocale(LC_ALL,"")`, `gettext` diagnostics, character-based `len`/`index`/`substr`/`translit`.
- **PR F — "Small spec edges"** (#11, #12, #16): `translit` single-arg, `changecom` close reset, command-line-ordered `-D`/`-U`/operands.
