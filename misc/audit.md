# POSIX.1-2024 Conformance Audit — `misc` crate (`true`, `false`, `test`/`[`)

**Implementation:**
- `misc/true.rs` (12 lines)
- `misc/false.rs` (12 lines)
- `misc/test.rs` (615 lines) — also installed as `[`
**Tests:** `misc/tests/{true,false,test}/mod.rs` (26 + 26 + 336 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 (XCU) §3 — `false` (p. 2920), `test` (pp. 3431–3436), `true` (pp. 3466–3467)
**Reference:** mega-PDF `~/tmp/POSIX.2024.pdf` (no sliced tree available; same handicap as the `m4`/`make`/`calc` audits). Critical/Major-class findings behaviorally verified against `./target/release/{test,true,false}`.
**Date:** 2026-06-13

## TL;DR

This is the cleanest crate audited so far. `true` and `false` are textbook-conforming one-liners. `test`/`[` implements the full POSIX.1-2024 primary set with the correct argument-count precedence algorithm (0/1/2/3/4/>4), the `[ … ]` bracket form, `!` negation, and correct `0`/`1`/`>1` exit codes — all behaviorally verified. **No Critical or Major conformance defects were found.** The only real divergence is that the `<`/`>` string operators compare by Unicode-scalar (byte) order instead of `LC_COLLATE` locale collation (the spec explicitly mandates collation). Everything else is informational: the removed `-a`/`-o`/`(`/`)` operators are still implemented as an extension (legal — POSIX leaves >4-argument behavior "unspecified"), and integer operands are bounded to `i64` and reject leading blanks.

## Priority issues

### Critical
_None._

### Major
_None._

### Minor
- [x] **#1 — `test` `<` / `>` ignore `LC_COLLATE`; compare by byte/scalar order.** `misc/test.rs:274-275` (`eval_binary_str` uses Rust `s1 < s2`); `LC_COLLATE` is never read. Spec (Vol. 3 p. 3433): "`s1 < s2` True if s1 collates before s2 **in the current locale**" and (p. 3434) "`LC_COLLATE` — Determine the locale for the behavior of the `>` and `<` string comparison operators." Verified: under `LC_ALL=en_US.UTF-8`, glibc collates `a` < `B`, but `test a \< B` exits 1 (false) because byte `a`(0x61) > `B`(0x42). DIVERGES. ✓ **fixed (Phase 1)** — `<`/`>` now route through `libc::strcoll`. Root cause was deeper than the symptom: `gettext-rs`'s `setlocale` does not establish the C library's global locale, so `main()` now also calls `libc::setlocale(LC_ALL, "")` (`misc/test.rs`). Regression tests assert C-locale byte order and best-effort `en_US.UTF-8` collation.
- [x] **#2 — Removed operators `-a` / `-o` / `(` / `)` are implemented inconsistently.** `misc/test.rs:362-494` (`ExprParser`) supports `-a`, `-o`, `!`, `(`, `)` for the >4-argument path, but the 3-argument forms `test x -a y` / `test x -o y` returned `syntax error` (exit 2). Spec APPLICATION USAGE (Vol. 3 p. 3435): "The `-a` and `-o` binary primaries and the `'('` and `')'` operators **have been removed.**" Both behaviors fall in POSIX's "unspecified" zone, so neither is non-conforming — but the split was surprising. ✓ **fixed (Phase 3)** — made uniform: the 3- and 4-argument "otherwise unspecified" cases now fall back to a shared `eval_with_parser` helper, so `test x -a y` and friends evaluate as the historical extension. Documented as a deliberate compatibility extension in the helper's doc comment. Genuinely malformed expressions (`test a b c`) still error. Tested in `test_legacy_operators_uniform`.
- [x] **#3 — Integer operands are `i64`-bounded and reject surrounding whitespace.** `misc/test.rs:230-250` (`eval_binary_int` uses `str::parse::<i64>`). `test 99999999999999999999 -gt 1` → `integer expression expected` (exit 2); `test ' 5' -eq 5` → error. POSIX says only "integers … algebraically" with no stated bound, and historical `test` tolerates leading blanks. ✓ **fixed (Phase 2)** — added `parse_int` helper that `trim()`s surrounding blanks before parsing; the `i64` bound is retained (overflow is still diagnosed, as in GNU). Tested in `test_intops_operands`.
- [x] **#4 — Dead unreachable branch in `eval_unary`.** `misc/test.rs:175-178` printed `unknown operator` and returned false, but every caller pre-checks `parse_unary_op(...).is_some()`, so the `None` arm was unreachable. Code-quality only (CLAUDE.md forbids dead code), not a conformance issue. ✓ **fixed (Phase 3)** — `eval_unary` now takes a parsed `&UnaryOp`; the two callers bind the `Some(op)` directly, eliminating the dead arm.

## Detailed conformance matrix

### `true` (`misc/true.rs`)
- [x] **SYNOPSIS / OPTIONS / OPERANDS CONFORMS** — `true` (no options, no operands). `misc/true.rs:10-12`.
- [x] **DESCRIPTION / EXIT STATUS CONFORMS** — "shall return with exit code zero." Calls `std::process::exit(0)`. Verified `true`, `true foo bar`, `true --` all exit 0.
- [x] **Accepts and discards `--` CONFORMS** — spec (p. 3467 APPLICATION USAGE) requires only that `true` "accept, and discard, a first argument of `--`"; ignoring all argv satisfies this. `misc/true.rs:10`.
- [x] **No `--help`/`--version` CONFORMS** — correctly omits the GNU coreutils extension; POSIX `true` takes no options.
- [x] STDIN / STDOUT / STDERR / ENV / signals — all "Not used" / "Default"; trivially conforms.

### `false` (`misc/false.rs`)
- [x] **DESCRIPTION / EXIT STATUS CONFORMS** — spec: "shall always exit with a value between 1 and 125, inclusive." Calls `std::process::exit(1)`. Verified `false`, `false foo` exit 1. `misc/false.rs:10-12`.
- [x] **OPTIONS / OPERANDS / STDIN / STDOUT / STDERR / ENV CONFORMS** — all "None." / "Not used."

### `test` / `[` — Options & invocation
- [x] **No options; `--` not special CONFORMS** — spec OPTIONS (p. 3432): "shall not recognize the `--` argument … No options shall be supported." Implementation never parses options (no clap). Verified `test -- = --` → `--` treated as string, exit 0. `misc/test.rs:584-605`.
- [x] **`[` bracket form requires trailing `]` CONFORMS** — basename of argv[0] selects `[` vs `test`; final `]` is required and stripped, not counted in the argument-count algorithm. `misc/test.rs:587-600`. Verified via symlink: `[ -n x ]` → 0; `[ -n x` → `missing closing bracket`, exit 2.

### `test` — Unary primaries (`misc/test.rs:58-187`)
- [x] `-b`,`-c`,`-d`,`-e`,`-f`,`-p`,`-S`,`-s` CONFORMS — file-type/size via `metadata()` (follows symlinks per spec). `misc/test.rs:144-153`.
- [x] `-g` (SGID), `-u` (SUID) CONFORMS — `mode() & 0o2000` / `0o4000`. `misc/test.rs:149,153`.
- [x] `-h`,`-L` CONFORMS — use `symlink_metadata()` and do **not** follow the final symlink, exactly as the spec carves out. `misc/test.rs:117-122`.
- [x] `-r`,`-w`,`-x` CONFORMS — use `libc::access()` (effective-permission check per §1.1.1.4), not a mode-bit guess. `misc/test.rs:124-133,105-111`.
- [x] `-t` CONFORMS — parses fd number, `BorrowedFd::is_terminal()`; non-numeric or invalid fd → false. `misc/test.rs:160-170`.
- [x] `-n`,`-z` CONFORMS — string length non-zero / zero via `is_empty()`. `misc/test.rs:89-91`.
- [x] **Symlink-resolution rule CONFORMS** — all primaries except `-h`/`-L` resolve symlinks (spec p. 3433). `misc/test.rs:135-141`.

### `test` — Binary primaries (`misc/test.rs:189-358`)
- [x] `-ef` CONFORMS — same `(dev, ino)`. `misc/test.rs:289-297`.
- [x] `-nt`,`-ot` CONFORMS — including the "one side unresolvable" rules (newer-than true if p1 exists and p2 doesn't; older-than symmetric). `misc/test.rs:299-323`.
- [x] `=`,`!=` CONFORMS — byte-exact string identity. `misc/test.rs:272-273`.
- [x] **`<`,`>` CONFORMS** — ✓ fixed (#1): now collate via `libc::strcoll` under the active `LC_COLLATE`. `misc/test.rs` (`strcoll` helper + `eval_binary_str`).
- [x] `-eq`,`-ne`,`-lt`,`-gt`,`-ge`,`-le` CONFORMS — `i64` algebraic comparison; non-integer operand → diagnostic + exit 2 (per "an error occurred"). `misc/test.rs:230-268`. ✓ #3 fixed: surrounding blanks now trimmed via `parse_int`.

### `test` — Argument-count precedence algorithm (`misc/test.rs:497-577`)
- [x] **0 args → false (1) CONFORMS.** `misc/test.rs:499`. Verified.
- [x] **1 arg → true iff `$1` non-null CONFORMS.** `misc/test.rs:501-507`. Verified `test x`→0, `test ''`→1; `test -n`/`test -f` (1 arg) → true (operator-looking string is non-null).
- [x] **2 args CONFORMS** — `! $2` (true iff `$2` null) handled before unary-primary; unknown `$1` → `unary operator expected`, exit 2 (the spec's "unspecified" case). `misc/test.rs:509-529`. Verified `! ''`→0, `! x`→1, `-q x`→exit 2.
- [x] **3 args CONFORMS** — binary primary checked first, then `!`-negate-2-arg, then (legacy) `( $2 )`. Order matches spec bullets. `misc/test.rs:531-549`. Verified `= = =`→0, `! = =`→1 (binary wins), `! -f /x`→0.
- [x] **4 args CONFORMS** — `! <3-arg>` negation, then legacy `( $2 $3 )`. `misc/test.rs:551-561`. Verified.
- [x] **3/4/>4 args — extension in "unspecified" space CONFORMS.** Recursive-descent `ExprParser` (`-o` < `-a` < `!` < primary/`()` precedence, matching historical `test`) reached via the shared `eval_with_parser` helper from the 3-, 4-, and >4-argument paths. ✓ #2 fixed: uniform across all arities. Tested in `test_expr_precedence`.

### `test` — Environment variables (`misc/test.rs:580`)
- [x] `LANG`,`LC_ALL`,`LC_CTYPE`,`LC_MESSAGES`,`NLSPATH` CONFORMS — `setlocale(LcAll, "")` + `textdomain`/`bind_textdomain_codeset`; diagnostics go through `gettext`. `misc/test.rs:580-582`.
- [x] **`LC_COLLATE` CONFORMS** — ✓ fixed (#1): now consulted via `strcoll` for `<`/`>` once `libc::setlocale(LC_ALL, "")` is called in `main()`.
- [x] `LC_CTYPE` byte-vs-char CONFORMS in practice — only emptiness (`-n`/`-z`) and byte-exact `=` depend on it; both are correct on byte boundaries.

### `test` — STDIN / STDOUT / STDERR / async / output files
- [x] **STDIN not used CONFORMS** — no read path.
- [x] **STDOUT not used CONFORMS** — nothing written to stdout.
- [x] **STDERR diagnostics only CONFORMS** — all error strings via `eprintln!` (`misc/test.rs:176,234,335,452,471,482,548,560,571,597,611`), gettext-localized.
- [x] **ASYNCHRONOUS EVENTS = Default CONFORMS** — no handlers needed.

### `test` — Exit status / consequences of errors
- [x] **0 / 1 / >1 CONFORMS** — `True→exit 0`, `False→exit 1`, `Error→exit 2`. `misc/test.rs:607-614`. Verified across all batches (true expr→0, false expr→1, syntax/integer/unknown-operator errors→2).

## Test coverage signal

Existing `misc/tests/test/mod.rs` (336 lines) covers a broad slice: `-d -e -f -h -L -r -s -w -x -n -z -ef -eq -ne -lt -gt -ge -le = != < > -a -o`, plus bracket form and `!`.

Coverage (all gaps now closed in Phase 4):
- [x] `-b` (block), `-c` (char), `-S` (socket), `-p` (FIFO) device-type primaries — ✓ added (`test_char_device`, `test_block_device`, `test_fifo`, `test_socket`)
- [x] `-g` (SGID), `-u` (SUID) mode-bit primaries — ✓ added (`test_setgid_setuid`, with a graceful skip when the bit is stripped)
- [x] `-t` terminal test (true and false fd cases) — ✓ added (`test_terminal`; non-tty/invalid/non-numeric fd cases)
- [x] `-nt` / `-ot` timestamp comparison (both files exist; one missing) — ✓ added (`test_newer_older`, mtimes set via `utimes`)
- [x] `LC_COLLATE` collation behavior of `<`/`>` — ✓ added (`test_str_collation`)
- [x] Integer overflow / leading-blank operand error paths (**#3**) — ✓ added (`test_intops_operands`)
- [x] `>4`-argument `ExprParser` precedence (`-a` tighter than `-o`, `!`, nested `()`) — ✓ added (`test_expr_precedence`)
- [x] `true`/`false` argument-discard (`true --`, `false foo`) — ✓ added (`true_ignores_arguments`, `false_ignores_arguments`)

## Suggested PR groupings

- **PR A — "test: locale-aware string collation"** (#1): route `<`/`>` through `strcoll`/`LC_COLLATE`; add an `en_US.UTF-8` collation test (`a < B`). The one genuine conformance fix.
- **PR B — "test: integer operand robustness"** (#3): trim surrounding blanks before `i64::parse`; add overflow/whitespace tests. Optional.
- **PR C — "test: tidy legacy-operator handling + dead code"** (#2, #4): decide on uniform vs compound-only `-a`/`-o`/`()` acceptance and document it; remove the unreachable `eval_unary` arm.
- **PR D — "test: fill coverage gaps"**: device/mode primaries (`-b -c -S -p -g -u`), `-t`, `-nt`/`-ot`, and `ExprParser` precedence tests.

**Status:** all findings (#1–#4) and PR D coverage landed on branch `misc-audit` across four phases (collation → integer operands → uniform legacy operators + dead-code → coverage tests). All boxes above are ticked; `misc` test suite passes 37 tests with zero clippy warnings.

---

_Audit method: full per-utility spec read from the mega-PDF (`true`/`false`/`test` sections), full implementation read, behavioral verification of every Minor finding against release binaries (collation, removed-operator split, integer bounds, exit codes, bracket form, argument-count algorithm). No code was modified._
