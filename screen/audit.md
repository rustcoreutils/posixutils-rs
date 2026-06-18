# POSIX.1-2024 Conformance Audits — `screen/` utilities

This file collects per-utility POSIX conformance audits for the terminal-screen
utilities crate. Each audit follows the playbook in `audits.md`.

**Crate:** `screen/` — `stty`, `tabs`, `tput` (three POSIX utilities) plus the
shared `osdata.rs` termios data tables used by `stty`.
**Date:** 2026-06-18
**Method:** static spec-vs-code audit against the sliced POSIX.1-2024 tree
(`~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/{stty,tabs,tput}.md`),
with every Critical/Major claim behaviorally confirmed by running the release
binaries (a PTY harness via `pty.fork()` was used for the TTY-only `stty`
set-mode paths).

---

## Cross-cutting observations

- **`osdata.rs` (262 lines)** is `stty`-private (`mod osdata;` in `stty.rs:13`);
  `tabs`/`tput` do not use it. It holds the speed table (`load_speeds`), the
  `^c`→control-char translation table (`load_cchar_xlat`), and the master
  termios parameter table (`load_params`). One data defect lives here: the
  speed map keys `"54"` to `B50` (`osdata.rs:32`) — see stty #6.
- **i18n is split.** All three call `setlocale(LC_ALL, "")` +
  `textdomain` + `bind_textdomain_codeset` in `main`. `tabs` and `tput` wrap
  their runtime diagnostics in `gettext()`; **`stty` does not** — every
  `Error::other(...)` / `format!` diagnostic in `stty.rs` is hardcoded English
  (stty #8), so `LC_MESSAGES` is inert for `stty`.
- **terminfo dependency.** `tabs` and `tput` are thin front-ends over the
  `terminfo` crate (`Database::from_env`/`from_name` + capability expansion).
  `stty` is a direct `termios(3)` client (`termios` crate). All three honor the
  `ASYNCHRONOUS EVENTS = Default` requirement (no signal handling needed; none
  present — correct).
- **Diagnostic prefix.** `stty` propagates errors through `main() -> Result<…,
  Box<dyn Error>>`, so the runtime prints `Error: <msg>` (Rust's default) rather
  than the conventional `stty: <msg>`. `tabs` prefixes `tabs:`; `tput` prefixes
  neither util name (stty #8, tput #T6 — Minor).

---

## `stty`

**Implementation:** `screen/stty.rs` (814 lines) + `screen/osdata.rs` (262 lines)
**Tests:** `screen/tests/stty/mod.rs` (26 lines — **no executable tests**; the file
is a comment explaining that stty needs a TTY and is verified only by manual
testing)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 3453–3462
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/stty.md`
**Date:** 2026-06-18

### TL;DR

The *display* paths (`-a` long form, `-g` compact save, default short form) and
the termios flag/cchar tables are broad and largely conforming, and the utility
correctly uses **standard input** for both get and set (the spec's security
RATIONALE). But the *set* path — stty's primary job — is severely broken on the
golden path: **`stty <single-operand>` (e.g. `stty sane`, `stty raw`, `stty cs8`)
panics** (`assert!(args.operands.len() > 1)`), and **every negation operand
(`-echo`, `-icanon`, …) is rejected by clap** as an unknown option because the
positional has no `allow_hyphen_values`. On top of those two crashes/rejections:
single-character control-character assignment (`stty erase x`) is unsupported,
the Issue-8 `rows`/`cols`/`size` window-size operands are entirely absent, and
the speed table mis-keys `B50` as `"54"`. Diagnostics are hardcoded English.

### Priority issues

#### Critical

- [x] **#1 — `stty <single mode operand>` panics (assertion failure, exit 101).** ✓ fixed (Phase 1): assertion changed to `!args.operands.is_empty()`; regression `test_stty_single_operand_no_panic`. `stty.rs:642` (`assert!(args.operands.len() > 1)`) at the top of `stty_set_long`. `stty_set` (`stty.rs:778-784`) routes any non-`pfmt1` operand list to `stty_set_long`, including a list of length 1. So `stty sane`, `stty raw`, `stty cs8`, `stty parenb`, `stty -8` etc. all abort. **Behaviorally confirmed under a PTY:** `stty parenb` → `panicked at screen/stty.rs:642 … assertion failed: args.operands.len() > 1`, exit 101. This is the most common stty usage form. Fix: change the assertion to `>= 1` (or delete it; the `while idx < len` loop already handles any length).
- [x] **#2 — Negation operands (`-echo`, `-icanon`, `-parenb`, …) are rejected by clap before stty ever sees them.** ✓ fixed (Phase 1): `#[arg(allow_hyphen_values = true)]` on `operands`; `-a`/`-g` still parse as options; regression `test_stty_negation_operand_applied`. `stty.rs:34-59` declares `operands: Vec<String>` with no `allow_hyphen_values`/`trailing_var_arg`. clap treats a leading-`-` token as an option cluster. **Behaviorally confirmed:** `stty -echo` → `error: unexpected argument '-e' found`, exit 2. The internal set logic *does* implement negation (`stty.rs:657-665` `strip_prefix("-")`), so the entire negation surface is dead code. Fix: add `#[arg(allow_hyphen_values = true)]` (or `trailing_var_arg`) to the `operands` field so `-flag` operands reach `stty_set_long`.

#### Major

- [x] **#3 — Single-character control-character assignment is unsupported.** ✓ fixed (Phase 2): a 1-char `op_arg` now sets the control char to that byte (rejecting code points > 0xff); regression `test_stty_single_char_cchar_assignment`. `stty.rs:418-441` (`set_ti_cchar_oparg`) accepts only `^-`/`undef`/`^c`; any other argument that is not a 2-char `^`-sequence returns `"Invalid cchar specification"`. POSIX 116246: "If string is a single character, the control character shall be set to that character." **Behaviorally confirmed:** `stty erase x` → `Invalid cchar specification`, exit 1. Fix: when `op_arg.chars().count() == 1`, set `*cc = that_char as cc_t`.
- [x] **#4 — Issue-8 window-size operands `rows`, `cols`, and the `size` informational query are entirely missing.** ✓ fixed (Phase 2): `rows`/`cols`/`size` handled in `stty_set_long` via `get_winsize`/`set_winsize` (`libc::ioctl` `TIOCGWINSZ`/`TIOCSWINSZ` on stdin); `size` prints `"%d %d\n"`; regression `test_stty_rows_cols_size`. Added by Austin Group Defects 1053/1532/1687 (CHANGE HISTORY, 116429). POSIX 116290-116301 mandates `rows number`, `cols number` (via `tcsetwinsize()`), and `size` (write `"%1d %1d\n", <rows>, <cols>` to stdout). `grep -nE '"rows"|"cols"|"size"|winsize|TIOCGWINSZ' stty.rs osdata.rs` → **0 matches**. `size` is the only Informational Query; without it `stty size` is "Unknown operand". Fix: add `rows`/`cols` operands using `TIOCSWINSZ`, and a `size` query using `TIOCGWINSZ`.
- [ ] **#5 — `saved settings` combination mode only round-trips this implementation's own `pfmt1:` blob.** `stty.rs:778-784` recognizes a saved-settings operand only when it `starts_with(HDR_SAVE)` (`"pfmt1"`, `stty.rs:32`). That is the format `-g` emits (`stty_show_compact`, `stty.rs:292-319`), so `stty "$(stty -g)"` round-trips — CONFORMS in spirit. Flagged Major only because the `-g` form is **not** the spec's "one line of printable portable-character-set tokens excluding whitespace" guarantee tested for portability, and a malformed blob path has an `assert_eq!(parts[0], HDR_SAVE)` (`stty.rs:488`) guarded only by the `starts_with` check — safe today, but brittle. No fix required if the round-trip contract is the only goal; documented for the next auditor.
- [ ] **#6 — Speed table mis-keys `B50` as `"54"`; `stty 50` and `ispeed/ospeed 50` fail.** `osdata.rs:32` (`("54", libc::B50)`). The decimal baud string for `B50` is `"50"`, not `"54"`. Consequences: `stty 50` / `stty ispeed 50` → `set_ti_speed` lookup miss → `Error::other("Invalid speed")` (`stty.rs:451`); and `stty 54` wrongly programs `B50`. Fix: change the key to `"50"`. (Also Minor: the table omits 460800/921600 and the non-decimal `exta`/`extb` aliases.)

#### Minor

- [ ] **#7 — Non-printable / printable-but-unmapped control chars are displayed as a raw decimal value.** `stty.rs:251-253` (`show_cchars`) and `stty.rs:146-150` (`stty_show_short`): when a `c_cc` value is not in the `^c` reverse map and not `\0`, the code prints `format!("{}", ti.c_cc[idx])` (a decimal integer). POSIX 116347-116351: the value shall be "either the character, or some visual representation of the character if it is non-printable, or `<undef>`". A printable assignment (e.g. erase set to `@`) prints `64` instead of `@` or `^?`-style. Fix: print the literal char when printable, else a `^X`/`\NNN`-style visual representation.
- [ ] **#8 — All `stty` diagnostics are hardcoded English and lack the `stty:` prefix.** Every `Error::other("…")`/`format!("Unknown operand {}", …)` in `stty.rs` (`:451, 504, 508, 687, 703, 712, 752`, etc.) is plain English, not `gettext()`-wrapped; and because they bubble through `main() -> Result<…, Box<dyn Error>>`, the user sees `Error: <msg>` rather than `stty: <msg>`. POSIX 116317-116319 (`LC_MESSAGES`). Fix: wrap strings in `gettext()` and emit via an explicit `eprintln!("stty: {}", …)` path (matching `tabs`).
- [ ] **#9 — `ek` and `sane` use hardcoded control-char defaults rather than the device's "system defaults".** `stty.rs:610-635`. POSIX 116287 (`ek`: "system defaults") / 116288 (`sane`: "reasonable, unspecified, values"). The hardcoded `0x7f`/`0x15`/`0x03`/… are *a* reasonable set and `sane`'s values are explicitly "unspecified", so this CONFORMS; flagged only because `ek` claims "system defaults" but cannot consult them. No fix required.
- [ ] **#10 — `-a`/`-g` silently ignore any accompanying operands.** `stty.rs:797-811`: when `args.all`/`args.save` is set, operands are never consulted. The SYNOPSIS separates `stty [-a|-g]` from `stty operand...`; `stty -a sane` should arguably be a usage error. clap's `group = "mode"` correctly makes `-a` and `-g` mutually exclusive, but neither conflicts with operands. Fix: reject operands when `-a`/`-g` is present.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

- [x] `-a` and `-g` mutually exclusive — `stty.rs:40,48` share clap `group = "mode"`.
- [ ] **Negation operands rejected by clap** (#2 Critical) — `operands` lacks `allow_hyphen_values` (`stty.rs:57`).
- [x] Bundled/standalone positive operands accepted — `stty cs8 cs7` reaches the set path (confirmed; failed only at `tcsetattr` on the test PTY, i.e. parsing/dispatch worked).
- [x] `--` end-of-options handled (clap default).

#### OPTIONS

| Opt | Status | Notes (file:line) |
|---|---|---|
| `-a` | CONFORMS | `stty.rs:797-799` → `stty_show_long`; emits `speed … baud;`, flag groups, `cchars`. |
| `-g` | PARTIAL | `stty.rs:801-802` → `stty_show_compact` emits a `pfmt1:`-prefixed colon-joined blob; round-trips with this stty (#5) but is a private form. |

#### OPERANDS

- [ ] **Control Modes** — `parenb/parodd/cs5-8/hupcl/hup/cstopb/cread/clocal` present (`osdata.rs:152-164`); baud `number`/`ispeed`/`ospeed` present but speed table buggy (#6).
- [ ] **Input Modes** — all present (`osdata.rs:178-192`); reachable only via the broken set path (#1/#2).
- [ ] **Output Modes** — `opost` (Base) + XSI `onlcr/ocrnl/onocr/onlret/ofill/ofdel/cr0-3/nl0-1/tab0-3/tabs/bs0-1/ff0-1/vt0-1` present (`osdata.rs:196-222`).
- [ ] **Local Modes** — `isig/icanon/iexten/echo/echoe/echok/echonl/noflsh/tostop` present (`osdata.rs:226-237`).
- [ ] **Special Control Character Assignments** — `eof/eol/erase/intr/kill/quit/susp/start/stop` mapped (`osdata.rs:247-255`); `^c` table complete (`osdata.rs:63-124`); `^-`/`undef`→0 handled (`stty.rs:424-427`); **single-char form missing** (#3 Major).
- [x] **`min`/`time`** — `osdata.rs:259-260` + `stty.rs:742-756` parse numeric `u8` args.
- [ ] **Combination Modes** — `evenp/parity/oddp/-parity/-evenp/-oddp/raw/cooked/nl/-nl/ek/sane/tabs/-tabs` handled (`stty.rs:533-638`); `raw` matches the POSIX literal recipe (cs8, disable erase/kill/intr/quit/eof/eol, `-opost`, `-inpck`); `saved settings` only for `pfmt1` blobs (#5).
- [ ] **Terminal Window Size (`rows`/`cols`) and `size` query MISSING** (#4 Major).

#### STDIN / INPUT FILES

- [x] Terminal state is read from and written to **standard input** — `stty.rs:794` `Termios::from_fd(STDIN_FILENO)`, `stty.rs:523,771` `tcsetattr(STDIN_FILENO, …)`. Matches POSIX RATIONALE 116401-116406 (security: stdin, not stdout).
- [x] No data is read from stdin — only `tcgetattr`/`tcsetattr` ioctls.
- [x] INPUT FILES "None" — no file operands consumed.

#### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL`/`LC_CTYPE` | PARTIAL | `setlocale(LC_ALL, "")` called (`stty.rs:787`); affects clap help, but byte/char interpretation of operands is plain Rust `str`. |
| `LC_MESSAGES` | MISSING (effect) | (#8) diagnostics hardcoded English, never `gettext()`-wrapped. |
| `NLSPATH` (XSI) | MISSING | No message-catalog wiring. |

#### ASYNCHRONOUS EVENTS

- [x] Default — no handlers required or present (`grep -nE 'SIGCONT|SIGWINCH|signal' stty.rs` → 0).

#### STDOUT / STDERR

- [x] With set-operands and no Informational Query: no stdout — set path writes nothing to stdout (`stty.rs:778-784`).
- [x] `-a` speed line: `"speed %d baud;"` when ispeed==ospeed, else `"ispeed … ospeed …"` — `stty.rs:74-78` (`ti_baud_str`). CONFORMS.
- [x] `-a` control chars: `"%s = %s;"` with `<undef>` for disabled — `stty.rs:256, 247-254`. CONFORMS (except value rendering, #7).
- [ ] **`size` query output `"%1d %1d\n"` MISSING** (#4).
- [x] Diagnostics → stderr — via `Error:`/`eprintln` (but English & misprefixed, #8).

#### EXIT STATUS / CONSEQUENCES OF ERRORS

- [x] 0 on success; >0 on error — `main` returns `Ok`/`Err` (`stty.rs:786-814`).
- [ ] **Exit 101 (panic) on `stty <single operand>`** (#1 Critical) — violates "0 success / >0 error" with an abnormal abort.
- [x] First bad operand aborts the operand list — `stty_set_long` returns `Err` on the first failure (`stty.rs:687-714`). CONSEQUENCES OF ERRORS = Default, so abort is permitted.

### Test coverage signal

`screen/tests/stty/mod.rs` contains **no `#[test]`** — only a comment that stty
needs a TTY. The unit-test-able pure functions are untested.

Not covered (all map to findings):
- [x] `stty <single operand>` does not panic (#1) — `test_stty_single_operand_no_panic` (PTY harness).
- [x] Negation operand `-echo` reaches the set path (#2) — `test_stty_negation_operand_applied`.
- [x] `stty erase x` single-char assignment (#3) — `test_stty_single_char_cchar_assignment`.
- [x] `rows`/`cols`/`size` (#4) — `test_stty_rows_cols_size`.
- [ ] `parse`/`merge_map`/`set_ti_flag`/`speed_to_str` pure-function unit tests (no TTY needed).
- [ ] `stty 50` selects `B50` (#6).

---

## `tabs`

**Implementation:** `screen/tabs.rs` (426 lines, incl. 11 unit tests at `:358-426`)
**Tests:** `screen/tests/tabs/mod.rs` (95 lines, 5 integration `#[test]`s) + the in-file unit tests
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 3463–3466 (XSI / interactive-utility option)
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/tabs.md`
**Date:** 2026-06-18

### TL;DR

`tabs` is in good shape. It supports the full XSI option set (`-a -a2 -c -c2 -c3
-f -p -s -u`), the repetitive `-0`..`-9` form (mapping `-0`→clear, default→`-8`),
the `n[[sep[+]n]...]` operand with comma/blank separators and `+N` increments
with strictly-ascending validation, `-T type`/`TERM`, and uses the terminfo
hardware-tab capabilities (`tbc`/`hts`) as the spec intends. Diagnostics are
`gettext()`-wrapped. The notable gaps are spec-Minor: a missing-TERM/`-T`
condition errors out instead of falling back to an "unspecified default terminal
type", `MAX_COLUMN` is hardcoded at 160, and a leading `+N` first operand is
tolerated.

### Priority issues

#### Major

- [ ] **#T1 — Unset `TERM` with no `-T` errors instead of using a default terminal type.** `tabs.rs:326-337`: `Database::from_env()` failing (TERM unset/null) prints "cannot determine terminal type" and exits 1. POSIX 116447-116449 / 116507-116508: when `-T` is absent and `TERM` is unset/null, "an unspecified default terminal type shall be used." Erroring is a divergence from the mandated fallback. Fix: fall back to a built-in default (e.g. `dumb`/`ansi`) instead of hard-failing. (Borderline Minor — the spec leaves the default "unspecified", but mandates that one be used.)

#### Minor

- [ ] **#T2 — `MAX_COLUMN` hardcoded at 160.** `tabs.rs:19`, used by `generate_repetitive_tabs` (`tabs.rs:156-169`). POSIX 116446: "The maximum number of tab stops allowed is terminal-dependent." A fixed cap is acceptable but should ideally derive from the terminal width (terminfo `cols`). Fix: query terminfo `cols` or document the cap.
- [ ] **#T3 — A leading `+N` first operand is silently accepted.** `tabs.rs:117-146` (`parse_tabstops`): the first token `+5` is treated as `0+5`. POSIX 116485-116486: the `+`-increment applies to any value "except the first one". Fix: reject a leading `+`.
- [ ] **#T4 — Custom-operand path does not enforce `n` ≤ a single column max / first stop ≥ 1 vs preset semantics.** `parse_tabstops` rejects a literal `0` (`tabs.rs:127-129`) and non-ascending order (`:140-142`) — correct — but does not bound values to the terminal; combined with #T2 the behavior for `tabs 500` is to emit a stop the terminal cannot honor. Minor.
- [ ] **#T5 — Output written even when stdout is not a terminal.** `set_hw_tabs` (`tabs.rs:249-285`) writes the clear/set sequence unconditionally. POSIX 116512-116514: "If standard output is not a terminal, undefined results occur." Undefined ⇒ writing is permitted; flagged only as a robustness note (no `isatty` guard). No fix required.

### Detailed conformance matrix

#### SYNOPSIS / OPTIONS

| Opt | Status | Notes (file:line) |
|---|---|---|
| `-n` (`-0`..`-9`) | CONFORMS | `tabs.rs:42-71, 174-204`; `-0`→clear, default (no opt)→`-8` (`tabs.rs:244-245`). |
| `-a` 1,10,16,36,72 | CONFORMS | `tabs.rs:207-209`. |
| `-a2` 1,10,16,40,72 | CONFORMS | multi-char option via `preprocess_args` `-a2`→`--a2` (`tabs.rs:23-33, 76`). |
| `-c` 1,8,12,16,20,55 | CONFORMS | `tabs.rs:213-215`. |
| `-c2` 1,6,10,14,49 | CONFORMS | `tabs.rs:217-218`. |
| `-c3` 1,6,10,…,67 | CONFORMS | `tabs.rs:219-223`. |
| `-f` 1,7,11,15,19,23 | CONFORMS | `tabs.rs:224-226`. |
| `-p` 1,5,9,…,61 | CONFORMS | `tabs.rs:227-231`. |
| `-s` 1,10,55 | CONFORMS | `tabs.rs:232-234`. |
| `-u` 1,12,20,44 | CONFORMS | `tabs.rs:235-237`. |
| `-T type` | CONFORMS | `tabs.rs:313-325`; `TERM` fallback at `:326-337` (but see #T1). |

#### OPERANDS / STDIN / INPUT FILES

- [x] `n[[sep[+]n]...]` with comma **or** blank separators — `tabs.rs:111` splits on `,`/whitespace.
- [x] `+N` increment relative to previous value — `tabs.rs:117-137`.
- [x] Strictly-ascending positive integers enforced — `tabs.rs:127-142`.
- [ ] **Leading `+N` first operand tolerated** (#T3).
- [x] STDIN "Not used" — no stdin reads.
- [x] INPUT FILES "None".

#### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL`/`LC_CTYPE` | CONFORMS | `setlocale(LC_ALL, "")` at `tabs.rs:288`. |
| `LC_MESSAGES` | CONFORMS (plumbed) | diagnostics wrapped in `gettext()` (`tabs.rs:125,141,149,254,262,275,319,332,344`). |
| `NLSPATH` (XSI) | MISSING | no catalog wiring (tree-wide gap). |
| `TERM` | PARTIAL | read via `Database::from_env` but no default-on-unset (#T1). |

#### ASYNCHRONOUS EVENTS / STDOUT / STDERR / EXIT STATUS

- [x] Default — no signal handling needed (none present).
- [x] Clear+set sequence to stdout in unspecified format — `set_hw_tabs` (`tabs.rs:249-285`).
- [x] Unsupported terminal → diagnostic to stderr + exit >0 — `tabs.rs:253-256` ("terminal does not support setting tab stops").
- [x] Diagnostics → stderr only — all via `eprintln!` with `tabs:` prefix.
- [x] 0 success / >0 error — `tabs.rs:355` / various `ExitCode::from(1)`.

### Test coverage signal

Good for a TTY-dependent utility: 11 unit tests cover `parse_tabstops`
(comma/blank/mixed separators, increments, non-ascending, invalid, zero) and
`generate_repetitive_tabs`; 5 integration tests cover `-T` unknown, `--help`,
`--version`, and error paths.

Not covered:
- [ ] Each XSI preset (`-a`/`-c3`/…) yields its exact documented list (`parse_cmd_line`, easily unit-testable without a TTY).
- [ ] `-0` produces an empty stop list; default equals `-8` (#T1-adjacent).
- [ ] Leading `+N` rejection (#T3).

---

## `tput`

**Implementation:** `screen/tput.rs` (173 lines)
**Tests:** `screen/tests/tput/mod.rs` (60 lines, 3 integration `#[test]`s)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 3504–3506
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/tput.md`
**Date:** 2026-06-18

### TL;DR

`tput` is the cleanest of the three. It supports exactly the three POSIX-locale
operands (`clear`/`init`/`reset`), maps the full POSIX exit-status ladder
(0/2/3/4/>4), continues past unavailable capabilities without error, processes
multiple operands, and honors `-T`/`TERM`. The gaps are Minor: `init`/`reset`
emit the *filename* of an `if`/`rf` (init/reset-file) capability instead of its
contents, an invalid operand aborts the whole list before any valid operand
runs, and there is no init→fallback for a missing reset string.

### Priority issues

#### Minor

- [ ] **#T1 — `init`/`reset` emit the init/reset *file* capability literally instead of catting its contents.** `tput.rs:42-45` (`InitFile`) and `:60-63` (`ResetFile`) expand and write the capability value, which for `if`/`rf` is a *pathname*; the spec intent is to send the file's *contents*. The code comments acknowledge this ("for now just output the capability"). Few terminfo entries use `if`/`rf`, so impact is low. Fix: read the named file and write its bytes. Also `iprog` (init program) is not executed.
- [ ] **#T2 — `reset` does not fall back to init strings when reset strings are absent.** `tput.rs:53-69` (`tput_reset`) emits only `rs1/rs2/rf/rs3`. Historical `tput reset` falls back to `is1/is2/if/is3` when the reset variants are missing. POSIX leaves init-vs-reset "unspecified," so this CONFORMS; flagged for parity with historical behavior. No fix required.
- [ ] **#T3 — An invalid operand aborts all earlier valid operands.** `tput.rs:132-137` pre-validates the whole operand list and returns exit 4 on the first invalid name *before* loading terminfo or running any operand. So `tput clear bogus` never clears. The design is deliberate (comment: invalid-operand exit 4 should outrank no-terminfo exit 3), and CONSEQUENCES OF ERRORS (117983) speaks to *unavailable* operations (which the loop does continue past, `tput.rs:165-170`), not *invalid* names. Borderline; flagged as a behavioral note. Fix (optional): run valid operands first, then report the invalid-operand status.

### Detailed conformance matrix

#### SYNOPSIS / OPTIONS / OPERANDS

- [x] `-T type` — `tput.rs:28-29, 151-157` (`Database::from_name`).
- [x] `TERM` fallback — `tput.rs:141-150` (`Database::from_env`); unset/unknown → exit 3 with diagnostic.
- [x] `clear` — `tput.rs:71-77` emits `ClearScreen` (`clear`).
- [x] `init` — `tput.rs:35-51` emits `is1/is2/if/is3` (but see #T1).
- [x] `reset` — `tput.rs:53-69` emits `rs1/rs2/rf/rs3` (but see #T1/#T2).
- [x] Unsupported operation is **not** an error — `tput.rs:36-49,54-67,72-75` skip absent capabilities via `if let Some(...)`; `process_operand` returns `Ok` (POSIX 117940-117941). CONFORMS.

#### STDIN / INPUT FILES / ENVIRONMENT

- [x] STDIN "Not used"; INPUT FILES "None".
- [x] `LANG`/`LC_ALL`/`LC_CTYPE`/`LC_MESSAGES` — `setlocale(LC_ALL, "")` (`tput.rs:112`); diagnostics `gettext()`-wrapped (`tput.rs:104,134,146,154`).
- [ ] **`NLSPATH` (XSI) MISSING** — no catalog wiring (tree-wide gap).
- [x] `TERM` — read via `Database::from_env` (`tput.rs:141`).

#### ASYNCHRONOUS EVENTS / STDOUT / STDERR

- [x] Default — no handlers needed (none present).
- [x] Sequences written to stdout — `*.expand().to(io::stdout())` throughout.
- [x] Diagnostics → stderr only — all via `eprintln!`.

#### EXIT STATUS / CONSEQUENCES OF ERRORS

- [x] 0 success — `tput.rs:18, 163-172`.
- [x] 2 usage error — `tput.rs:20, 120-127` (clap parse failure; missing required operand → exit 2, confirmed by `test_tput_no_operand`).
- [x] 3 no terminfo — `tput.rs:21, 143-157` (confirmed by `test_tput_invalid_terminal_type`).
- [x] 4 invalid operand — `tput.rs:22, 104-106, 132-137` (confirmed by `test_tput_invalid_operand`).
- [x] >4 (5) other error — `tput.rs:23, 89-101`.
- [x] Continue past unavailable capability — `tput.rs:165-170` keeps the worst exit code and continues. CONFORMS to CONSEQUENCES OF ERRORS.
- [ ] **Invalid operand aborts valid earlier operands** (#T3, Minor).

##### Note: `tput.rs:23` defines `EXIT_OTHER_ERROR = 5`

- [x] Reserved exit 1 ("Boolean operand not set") is correctly *not* used (commented out at `tput.rs:19`); POSIX RATIONALE 118012-118014 keeps 1 reserved for Boolean operands this implementation does not support. CONFORMS.

### Test coverage signal

3 integration tests cover the three error exit codes (4 invalid operand, 3 bad
`-T`, 2 missing operand). `clear`/`init`/`reset` output is verified only by
manual testing (terminfo-dependent).

Not covered:
- [ ] `tput clear` emits the terminal's `clear` sequence (needs a known `-T`, e.g. `-T xterm`).
- [ ] Multi-operand continue-past-unavailable behavior (#T3).
- [ ] `init`/`reset` file-capability handling (#T1).

---

## Suggested PR groupings

- **PR A — "stty: fix the set-mode golden path" (Critical)**: stty #1 (assertion → `>= 1`), stty #2 (`allow_hyphen_values` on operands). Smallest unit that makes `stty sane`/`stty -echo` work at all. Add the PTY-harness regression tests from this audit.
- **PR B — "stty: control-char + window-size operands"**: stty #3 (single-char assignment), stty #4 (`rows`/`cols`/`size` via `TIOC[GS]WINSZ`).
- **PR C — "stty: data + display fixes"**: stty #6 (`"50"` speed key), stty #7 (printable cchar rendering), stty #8 (gettext + `stty:` prefix), stty #10 (`-a`/`-g` + operands).
- **PR D — "stty: pure-function tests"**: unit tests for `parse_tabstops`-equivalent helpers, `set_ti_flag`, `merge_map`, `speed_to_str` (no TTY needed); closes the empty-test-file gap.
- **PR E — "tabs: terminal-default + operand polish"**: #T1 (default terminal on unset TERM), #T3 (leading `+N`), #T2 (cols-derived max).
- **PR F — "tput: init/reset file handling"**: #T1 (cat `if`/`rf` contents, run `iprog`), #T2 (init fallback), optionally #T3 (operand ordering).
