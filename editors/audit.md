# POSIX.1-2024 Conformance Audits — `editors/` utilities

This file collects per-utility POSIX conformance audits for the editors crate
(`ed`, `ex`, `vi`). Each audit follows the playbook in `../audits.md`.

**Date:** 2026-06-07
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3.
**Reference slices:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/{ed,ex,vi}.md`

The crate builds two binaries: `ed` (`ed_main.rs` + `ed/`) and `vi` (`vi_main.rs`
+ `vi/`). `ex` is the line-editor mode of the `vi` binary, selected when `argv[0]`
ends in `ex` (`vi/lib.rs:50-90`); it shares the buffer/file/search/options
machinery with `vi` and adds the `vi/ex/` command parser + executor.

Critical and Major findings below were **behaviorally verified** by building the
release binaries and driving them (`ed` via piped stdin scripts; `vi`/`ex` flags
via the CLI). Lines marked *(verified)* were confirmed by running the binary;
"absent" claims (signals, locale) were confirmed by `grep` over the source tree.

---

## Cross-cutting themes

Four patterns recur across all three editors. Fixing them at the shared layer
(`vi/search.rs`, `plib`) closes many per-utility items at once.

### 1. Regex flavor — `regex` crate is ERE, spec requires BRE

All three editors must use **Basic Regular Expressions** (XBD §9.3); the `regex`
crate implements ERE and does **not** support in-pattern back-references.

| Utility | Pattern path | State |
|---|---|---|
| `ed` | ~~`Regex::new(&pat)` direct~~ → `plib::regex` BRE | ✓ fixed (phase 1) — true libc BRE, back-refs + locale brackets |
| `vi` | ~~`convert_pattern` BRE→ERE~~ → `plib::regex` BRE | ✓ fixed (phase 4) — magic mode passes through to libc BRE (incl. `\<`/`\>`); nomagic escapes metacharacters |
| `ex` | ~~search via `convert_pattern`; addresses raw ERE~~ → `plib::regex` BRE | ✓ fixed (phase 4) — search, `:s` substitute, `:g` global, and address `/re/` all compile as BRE |
| all | — | ✓ fixed: LC_COLLATE/LC_CTYPE bracket ranges honored via libc regex (ed phase 1, vi/ex phase 4) |

### 2. Signal handling

| Signal | `ed` | `vi` / `ex` |
|---|---|---|
| SIGHUP | handler → writes buffer to `ed.hup` (`ed_main.rs:46-67`, `editor.rs:1541-1572`) | **none** — no buffer preservation |
| SIGINT | flag + `?` (`ed_main.rs:41-43`, `editor.rs:1523-1538`) | **none** (raw mode clears `ISIG`, so `^C` arrives as byte 0x03 and is dropped) |
| SIGQUIT | `SIG_IGN` (`ed_main.rs:54-55`) | **none** |
| SIGWINCH | N/A (non-visual) | **none** — terminal resize silently corrupts the display |
| SIGCONT | N/A | **none** — after `^Z`/resume the terminal is left in cooked mode |
| SIGTSTP | N/A | only `raise(SIGTSTP)` for `:suspend` (`editor/mod.rs:2415-2418`) |

`vi`/`ex` install **zero** signal handlers. SIGWINCH + SIGCONT are mandatory for
an interactive utility; SIGHUP preservation is mandated by both specs.

### 3. Exit-status propagation

~~`ed` prints `?` on every command error but never sets a non-zero exit status~~
✓ fixed (phase 2): `ed` now exits `>0` on any command/file error, matching GNU
ed. `vi`/`ex` already propagate a 0/1 exit code correctly.

### 4. Locale / i18n

~~`vi`/`ex` make no `setlocale()` call at all~~ ✓ fixed (phase 4): `run_editor`
now calls `setlocale(LC_ALL, "")`, so `LC_CTYPE`/`LC_COLLATE` drive the libc
regex engine and `LC_MESSAGES` is honored. `ed` already called `setlocale` and
its diagnostics are now `gettext()`-wrapped (phase 3). Word/case classification
in vi still uses Rust built-ins (minor; not a spec `shall`).

---

## `ed`

**Implementation:** `ed_main.rs` (118) + `ed/editor.rs` (1674) + `ed/parser.rs`
(883) + `ed/buffer.rs` (550) + `ed/error.rs` (66)
**Tests:** `tests/ed/mod.rs` (1394)
**Spec slice:** `…/3-utilities/ed.md`

### TL;DR
Core command set (`a c d e E f g G H h i j k l m n p P q Q r s t u v V w W x y z = ! #`),
addressing, marks, global commands, and the `ed.hup`/SIGINT machinery are
implemented and largely correct. Three headline gaps: the search/substitute
engine is **ERE, not BRE** (so any real BRE pattern — grouping, intervals,
back-references — fails or matches the wrong thing); **command errors never set a
non-zero exit status**; and an **identical-result substitution is wrongly
reported as "no match"** (`?`). EOF handling and a few error-policy details also
diverge.

### Priority issues

#### Critical
- [x] **#E1 — Search/substitute use ERE, not BRE.** ✓ fixed (phase 1): migrated `ed/editor.rs` off the `regex` crate to `plib::regex` (`RegexFlags::bre()`, libc `regcomp`/`regexec`). BRE `\(\)` grouping, `\{n,m\}` intervals, and in-pattern back-references `\1`–`\9` now work; matching is done on the newline-stripped line body for correct `^`/`$` anchoring. Tests: `test_ed_bre_*`, `test_ed_sub_*`.
- [x] **#E2 — Command errors never set exit status > 0.** ✓ fixed (phase 2): `Editor.error_occurred` is set in `print_error`; `main` exits 1 when set; file-load errors set it too. Also fixed a latent bug where `process::exit(1)` skipped the `BufWriter` flush (lost the trailing `?`) — `run()` now flushes before returning. Matches GNU ed exit codes. Tests: `test_ed_exit_status_*`.

#### Major
- [x] **#E3 — Identical-result substitution reported as no-match.** ✓ fixed (phase 2): the new `substitute_line` returns `Some` whenever the pattern matched (even when the text is unchanged), so the line is rewritten and the buffer marked modified; only a complete absence of matches is an error. Test: `test_ed_identity_substitute_marks_modified`.
- [x] **#E4 — EOF in command mode does not act as `q`.** ✓ fixed (phase 2): `run()` calls `handle_eof()` on `read_line()→None`, which fires the modified-buffer warning (and non-zero exit). Test: `test_ed_eof_acts_as_quit_warns_when_modified`.
- [x] **#E5 — EOF in input mode does not return to command mode.** ✓ fixed (phase 2): `handle_eof()` terminates input mode and finalizes the pending `a`/`i`/`c` with the lines collected so far, then acts as `q`. Tests: `test_ed_eof_in_input_mode_*`.
- [x] **#E6 — `G`/`V` interactive prompt allows `a`/`c`/`i`.** ✓ fixed (phase 2): the interactive prompt now forbids exactly `a c i g G v V` (spec ed.md §93609) and — per spec — *allows* `!` (which was wrongly forbidden). Test: `test_ed_global_interactive_forbids_append`.
- [x] **#E7 — Intermediate address offsets reject out-of-range values.** ✓ fixed (phase 2): `resolve_address_with_base` accumulates offsets in signed arithmetic and validates only the final address. Test: `test_ed_intermediate_address_out_of_range_ok`.

#### Minor
- [x] **#E8 — Diagnostics hardcoded English; regex not locale-aware.** ✓ fixed: LC_COLLATE/LC_CTYPE bracket ranges honored via libc BRE (phase 1); the `EdError` Display strings (shown by `h`/`H`) are now `gettext()`-wrapped (phase 3), so `LC_MESSAGES` can localize them. `setlocale`/`textdomain` were already wired in `ed_main.rs`.
- [x] **#E9 — `s` count-flag (nth occurrence) is fragile.** ✓ fixed (phase 1): the substitute loop (`substitute_line`) now enumerates matches via `captures_at` and splices the chosen occurrence without re-matching a substring. Test: `test_ed_sub_count_flag`.
- [x] **#E10 — Compound omitted-address separators.** ✓ examined (phase 2): the spec-relevant requirement — discarding excess *leading* addresses — already conforms (`1,2,3p` → `2,3`, matching GNU ed), and `;;p` matches GNU (`$`). The only divergence is bare `,,p`, which we resolve to `1,$` (vs GNU's `$,$`); POSIX leaves this degenerate all-omitted case unspecified, so the current behavior is defensible. No code change.
- [x] **#E11 — SIGHUP/SIGINT handler async-safety.** ✓ examined (phase 3): the handlers (`ed_main.rs:42-49`) only do an `AtomicBool::store`, which *is* async-signal-safe; `save_hup_file` runs later from `check_sighup()` in the main loop (normal context), not from the signal handler. No change needed.

### Detailed conformance matrix

#### SYNOPSIS / OPTIONS
- [x] `-p string` CONFORMS — `ed_main.rs:31-32`, enables prompt.
- [x] `-s` CONFORMS — suppresses byte counts and `!` completion marker; `ed_main.rs:34`, `editor.rs:771-773`.
- [x] `--`/option ordering CONFORMS — clap.

#### OPERANDS / STDIN / INPUT FILES
- [x] `file` operand loaded before commands — `ed_main.rs:96-109`.
- [x] **EOF-as-`q` (cmd mode)** — ✓ fixed (phase 2), #E4.
- [x] **EOF terminate-input-mode** — ✓ fixed (phase 2), #E5.

#### ENVIRONMENT VARIABLES
- [x] `HOME` CONFORMS — `ed.hup` fallback path, `editor.rs:1559`.
- [x] **`LC_COLLATE` / `LC_CTYPE`** — ✓ fixed (phase 1): bracket ranges/classes honored via libc BRE (#E8).
- [x] **`LC_MESSAGES`** — ✓ fixed (phase 3): diagnostic strings `gettext()`-wrapped (#E8). `NLSPATH` N/A (no catalog shipped).

#### ASYNCHRONOUS EVENTS
- [x] `SIGQUIT` CONFORMS (`SIG_IGN`).
- [x] `SIGINT` CONFORMS — prints `?`, returns to command mode.
- [x] `SIGHUP` CONFORMS — writes `ed.hup` then `$HOME/ed.hup`; handler is async-signal-safe (#E11 examined).

#### STDOUT / STDERR
- [x] `?` error reporting on stdout CONFORMS — `editor.rs:187`.
- [x] `H`/`h` verbose help CONFORMS — `editor.rs:575-589`.
- [x] Byte counts (`e`/`E`/`r`/`w`) CONFORMS, `-s`-suppressed — `editor.rs:196-200`.
- [x] Prompt to stdout, fatal diagnostics to stderr CONFORMS — `editor.rs:178-180`, `ed_main.rs` fatal path.

#### Commands
- [x] `a c d i j k l m n p P t = ` CONFORMS — see `buffer.rs`/`editor.rs` per command.
- [x] `e E f r w W` CONFORMS — incl. `r !cmd`/`w !cmd` shell forms (`editor.rs:622-691`).
- [x] `g v` CONFORMS — mark-then-iterate (`editor.rs:1186-1375`).
- [ ] **`G V` PARTIAL** — #E6.
- [x] `h H P q Q u` CONFORMS.
- [x] **`s` — BRE engine + count fixed (phase 1)**; #E3 (no-op marks modified) lands in phase 2. Flags `g p l n % & \&` and `\<newline>` split CONFORM.
- [x] `!cmd` CONFORMS — `%`/`!` expansion, completion marker (`editor.rs:710-775`).
- [ ] `x z #` are non-POSIX extensions (treated as wq / scroll / null) — N/A, harmless.

#### Addressing
- [x] `. $ n 'x +n -n + - , ;` and `/re/`,`?re?` wrap CONFORM — `parser.rs`, `editor.rs:265-391`.
- [ ] **Intermediate out-of-range DIVERGES** — #E7.
- [ ] **Compound `,,`/`;;` MISSING** — #E10.

#### EXIT STATUS / CONSEQUENCES OF ERRORS
- [x] **Command errors exit 0** — ✓ fixed (phase 2), #E2.
- [x] **Non-terminal stdin error policy** — ✓ fixed (phase 2): any error sets a non-zero exit status (folded into #E2).

### Test coverage signal
Not covered:
- [ ] BRE-specific patterns (`\(\)`, `\{n,m\}`, back-refs) — would catch #E1.
- [ ] Exit status after a command error — would catch #E2.
- [ ] Identical-result substitute (`s/x/x/`) — would catch #E3.
- [ ] EOF behavior in command/input mode — #E4/#E5.
- [ ] `G`/`V` forbidden sub-commands — #E6.

### Suggested PR groupings
- **PR ed-A — "BRE engine"**: #E1 (+ #E9 count rework, #E8 collation).
- **PR ed-B — "exit status & error policy"**: #E2, #E3.
- **PR ed-C — "EOF / input-mode"**: #E4, #E5.
- **PR ed-D — "global & addressing"**: #E6, #E7, #E10.

---

## `vi`

**Implementation:** `vi_main.rs` (16) + `vi/` ≈ 14.3k lines (entry/dispatch
`lib.rs`, `editor/`, `command/`, `mode/`, `buffer/`, `ui/`, `input/`, `search.rs`,
`options.rs`, `file.rs`, `register.rs`, `undo.rs`, `shell.rs`, `config.rs`).
**Tests:** `tests/integration/mod.rs` (2043), `tests/pty/mod.rs` (254),
`tests/headless/mod.rs` (797)
**Spec slice:** `…/3-utilities/vi.md`

### TL;DR
The visual command set is broad and mostly correct — motions, operators, insert
modes, registers, marks, undo, search, `:`-escape, `%`, counts, `ZZ` all work.
The dominant gaps are **systemic, not per-command**: **no signal handling**
(SIGWINCH/SIGCONT/SIGHUP all absent — resize corrupts the screen, `^Z`/resume
breaks the terminal, hangup loses the buffer); **`-r` and `-t` are hard errors**
that exit immediately *(verified)*; **no `setlocale`**; and search is an imperfect
BRE veneer over an ERE engine. A handful of parsed-but-unhandled commands
(`(` `)` `_`) silently do nothing.

### Priority issues

#### Critical
- [ ] **#V1 — SIGWINCH not handled.** No handler anywhere in `vi/`. Terminal resize mid-edit corrupts the display with no recovery. Fix: `sigaction` handler → atomic flag → main loop refreshes size + full redraw.
- [ ] **#V2 — SIGCONT not handled.** After `^Z` (or `:suspend`) + resume, the terminal stays in cooked mode. Fix: SIGCONT handler re-enables raw mode and redraws.
- [ ] **#V3 — SIGHUP not handled; no buffer preservation.** Spec: hangup/EOF-on-input ⇒ preserve buffer. Currently the buffer is lost. Fix: SIGHUP handler writes a recovery/`dead.letter`-style file then exits.

#### Major
- [ ] **#V4 — `-r` recovery hard-errors and exits.** `lib.rs:177`. *(verified: `vi -r` → "vi: recovery mode not supported", exit 1)*. Fix: at minimum list recoverable files / no-op instead of fatal.
- [ ] **#V5 — `-t tagstring` hard-errors; `^]` is a stub.** `lib.rs:188`, `editor/mod.rs:3071-3074`. *(verified: `vi -t main` → "vi: tag mode not supported", exit 1)*. Fix: parse `tags` (ctags format), literal-string lookup, jump.
- [ ] **#V6 — Sentence motions `(` / `)` parsed but unhandled.** In the parser's simple-command list but no arm in `execute_command`; they silently do nothing. Fix: implement `move_sentence_{forward,backward}` and wire them.
- [ ] **#V7 — `_` (line/first-non-blank) parsed but unhandled.** `command/parser.rs:272`; no executor arm. Fix: add arm → `current + count − 1`, first non-blank.
- [ ] **#V8 — `ISIG` cleared in raw mode → SIGINT dropped.** `ui/terminal.rs:74`. `^C` arrives as byte 0x03 and is silently discarded; spec wants the terminal alerted and partial command discarded. Fix: keep `ISIG` or install a SIGINT handler; bell + cancel pending command.
- [ ] **#V9 — `EXINIT=""` does not suppress `$HOME/.exrc`.** `editor/mod.rs:2023` checks `!exinit.is_empty()` before processing, so an empty-but-set `EXINIT` falls through and sources `.exrc`. Spec: presence (even empty) suppresses `.exrc`. Fix: branch on *is-set*, not *non-empty*.
- [x] **#V10 — No `setlocale`; LC_* ignored.** ✓ fixed (phase 4): `run_editor` calls `setlocale(LC_ALL, "")` (`vi/lib.rs`), enabling locale-aware libc regex and `LC_MESSAGES`. (Word/case ops still use Rust built-ins — minor, no spec `shall`.)

#### Minor
- [ ] **#V11 — `-w size` consumed but discarded.** `lib.rs:193-196`; never assigned to `options.window`/`scroll`. Fix: parse + assign.
- [ ] **#V12 — `stty` erase/kill chars not honored.** `mode/insert.rs:122,133` hardcode `^H`/`^U`. Fix: read `c_cc[VERASE]`/`c_cc[VKILL]` via `tcgetattr`.
- [ ] **#V13 — Missing `set` options.** `beautify`, `directory`, `edcompatible`, `mesg`, `prompt`, `redraw`, `remap`, `slowopen`, `warn` absent from `Options` (`options.rs`). Fix: add (stub where behavior is a no-op).
- [ ] **#V14 — `^L` and `^R` share one handler.** `editor/mod.rs:622-625`; both just mark for redraw. `^L` should clear the physical screen first; `^R` should refresh only `@`-flagged lines. Fix: split handlers.
- [ ] **#V15 — NUL-in-insert (re-insert last input) not implemented.** `mode/insert.rs`. Fix: on `Char('\0')` at insert start, replay last inserted text.
- [x] **#V16 — Search is imperfect BRE over ERE.** ✓ fixed (phase 4): `search.rs` now uses `plib::regex` (libc BRE). `convert_pattern` magic mode is a passthrough (libc handles `\(\) \{\} \<\>` and treats `+?|(){}` as literal); nomagic escapes metacharacters. `Substitutor` rewritten with `captures_at` + a back-reference-aware `build_replacement`. Tests: `test_substitute_bre_*`, `test_search_bre_grouping`.

### Detailed conformance matrix

#### OPTIONS
- [x] `-R` CONFORMS — `lib.rs:173`.
- [x] `-c command` / `+command` CONFORMS — `lib.rs:179-185,212-213`.
- [ ] **`-r` MISSING** — #V4.  **`-t` MISSING** — #V5.  **`-w` PARTIAL** — #V11.
- [ ] `-s`/`-h`/`--version` accepted (extensions / ex-only on vi) — DIVERGES, harmless.

#### OPERANDS / STDIN / INPUT FILES
- [x] Multiple-file list + first-file open CONFORMS — `FileManager`.
- [ ] **STDIN EOF not treated as SIGHUP PARTIAL** — folded into #V3.

#### ENVIRONMENT VARIABLES
- [x] `COLUMNS`/`LINES` CONFORMS — `ui/terminal.rs:141-152`.
- [x] `EXINIT`/`HOME` (basic) CONFORMS — `editor/mod.rs:2021-2035` (but #V9).
- [x] `SHELL` CONFORMS — `options.rs:142`.
- [x] **`LANG`/`LC_ALL`/`LC_COLLATE`/`LC_CTYPE`/`LC_MESSAGES`** — ✓ fixed (phase 4), #V10.
- [ ] **`TERM` PARTIAL** — read but no terminfo lookup.

#### ASYNCHRONOUS EVENTS
- [ ] **SIGWINCH / SIGCONT / SIGHUP MISSING** — #V1/#V2/#V3.
- [ ] **SIGINT PARTIAL** — #V8.  `:suspend` raises SIGTSTP but no SIGCONT re-entry (#V2).

#### Command set
- [x] Motions `h j k l w W b B e E 0 $ ^ f F t T ; , G H M L { } [[ ]] |` CONFORM — `command/motion.rs`, `editor/mod.rs`.
- [ ] **`(` `)` MISSING** (#V6); **`_` MISSING** (#V7).
- [x] Scrolling `^F ^B ^D ^U ^E ^Y z` CONFORM.
- [ ] **`^L`/`^R` PARTIAL** (#V14); **`^]` MISSING** (#V5).
- [x] Editing `i I a A o O c C cc d D dd x X r R y Y p P J ~ < > .` CONFORM.
- [ ] **`s`/`S` PARTIAL** — do not save deleted text to the named buffer (`editor/mod.rs:698,705`).
- [x] `u U`, marks `m ' \``, `: / ? n N % & @ " Q ZZ ! ^^` CONFORM (search is #V16).

#### Insert mode
- [x] ESC, `^H`, `^W`, `^U`, `^T` CONFORM — `mode/insert.rs`.
- [ ] **`^V` PARTIAL** (no visual feedback); **`^D` PARTIAL** (`0^D`/`^^D` edge cases); autoindent-on-blank PARTIAL; **stty erase/kill MISSING** (#V12); **NUL re-input MISSING** (#V15).

#### EXIT STATUS / CONSEQUENCES OF ERRORS
- [x] 0/1 exit code propagated — `lib.rs:73-140`, `vi_main.rs:15`.
- [ ] **Unrecoverable-error ⇒ preserve PARTIAL** — folded into #V3.

### Test coverage signal
Not covered:
- [ ] Signal handling (resize / suspend / hangup) — #V1-#V3.
- [ ] `-r` / `-t` behavior — #V4/#V5.
- [ ] Sentence motions `(` `)`, `_` — #V6/#V7.
- [ ] `EXINIT=""` vs `.exrc` ordering — #V9.

### Suggested PR groupings
- **PR vi-A — "signals"**: #V1, #V2, #V3, #V8 (shared `dead.letter`/recovery infra with ex-#X1/#X2).
- **PR vi-B — "missing motions"**: #V6, #V7, plus `s`/`S` register save.
- **PR vi-C — "startup/options"**: #V4, #V5, #V9, #V11, #V13.
- **PR vi-D — "locale & regex"**: #V10, #V16 (cross-cutting #1/#4).
- **PR vi-E — "insert-mode fidelity"**: #V12, #V14, #V15.

---

## `ex`

**Implementation (shared vi binary, ex mode):** `vi/lib.rs` (dispatch/argv),
`vi/ex/{mod,command,address,parser}.rs`, plus `vi/{file,search,options,shell}.rs`.
**Tests:** `tests/ex/mod.rs` (547)
**Spec slice:** `…/3-utilities/ex.md`

### TL;DR
The ex command vocabulary is large and the parser covers most of it
(`a c d g v m t co p nu s w wq x q e r set ! & < > = @ ya pu j`, addressing, the
`set` options). But ex inherits every cross-cutting gap from vi and adds several
of its own: **no signal handling / no `preserve`** so a modified buffer is lost on
hangup or unexpected EOF; **`-s` batch mode still sources EXINIT/`.exrc`**
*(verified by code path)*; **`-r`/`-t` hard-error and exit** *(verified)*; the
**address parser uses raw ERE** and **drops the offset after an address**; `;`
behaves like `,`; and `global`/`v` appears to run **single-pass** rather than the
mandated mark-then-execute.

### Priority issues

#### Critical
- [ ] **#X1 — No signal handlers (SIGHUP/SIGINT/SIGTERM).** None in the tree. Spec mandates SIGHUP ⇒ preserve. Fix: install handlers (shared with vi-#V1-#V3).
- [ ] **#X2 — `preserve` command + EOF/SIGHUP file preservation missing.** No `Preserve` variant in `ExCommand`. *(EOF in `run_ex_mode` at `editor/mod.rs:505-508` just sets `should_quit`)*. Fix: add `preserve`, save buffer to recovery dir; call it on EOF/SIGHUP when modified.
- [ ] **#X3 — `-s` does not suppress EXINIT / `.exrc`.** `lib.rs:109-110` calls `load_startup_config()` unconditionally; only the *error message* is gated on `silent_mode`. *(verified by code path)*. Fix: skip startup config entirely when `silent_mode`.
- [x] **#X4 — Address `/re/`,`?re?` use raw ERE.** ✓ fixed (phase 4): `address.rs` compiles address patterns with `plib::regex` BRE. (Note: a *separate* pre-existing parser bug captures the trailing delimiter into the pattern — e.g. `/cherry/` stores `cherry/` — so `/re/` addresses don't match; this is address-parser fidelity, tracked under #X-addressing for phase 10, not a regex-engine issue.)
- [ ] **#X5 — `-r` exits with error instead of listing recoverable files.** `lib.rs:177-178`. *(verified: exit 1)*. Fix: list-or-no-op.
- [ ] **#X6 — EOF on stdin not treated as SIGHUP.** `editor/mod.rs:505-508`; data loss when the buffer is modified. Fix: preserve before quitting (ties to #X2).

#### Major
- [ ] **#X7 — `global`/`v` appears single-pass.** `parser.rs:519-544` stores the command string and dispatches per match without a mark-first pass; line-inserting/deleting commands corrupt iteration. Fix: collect matching line numbers first, then execute against stable marks.
- [ ] **#X8 — `-t tagstring` hard-errors.** `lib.rs:187-189`. *(verified: exit 1)*. Fix: ctags lookup + jump (shared with vi-#V5).
- [ ] **#X9 — Address offset after an address is parsed then discarded.** `address.rs:296-310` computes the offset but never applies it to the base address. Fix: add the offset to the resolved address.
- [ ] **#X10 — `;` separator treated like `,`.** `address.rs:315`; does not set current line to the first address before parsing the second. Fix: resolve first address, set `.`, then parse second.
- [ ] **#X11 — stdin-not-a-tty does not auto-enable `-s`.** Spec: non-terminal stdin ⇒ behave as `-s`. Only the explicit flag sets `silent_mode` (`lib.rs:197`). Fix: `stdin().is_terminal()` check at startup.
- [ ] **#X12 — `substitute` gaps.** `parser.rs:506-508` errors on empty pattern instead of reusing the last RE; missing flags `l`, `#`, count; `~`, `%`, `\l\u\L\U` in replacement unimplemented; `c` (confirm) parsed but no interactive loop; `\n`-split doesn't split the buffer line. Fix: extend `SubstituteFlags` + `expand_replacement`.
- [ ] **#X13 — `shell` command does not pass `-i`.** `shell.rs` interactive path. Spec §`sh -i`. Fix: add `.arg("-i")`.
- [x] **#X14 — No `setlocale`; LC_* ignored.** ✓ fixed (phase 4): shared `setlocale(LC_ALL, "")` in `run_editor` (vi-#V10).

#### Minor
- [ ] **#X15 — Line-0 address rejected for `a`/`i`/`r`/`=`/`put`.** `address.rs:56-60`. Fix: allow 0 for the commands the spec lists.
- [ ] **#X16 — `'`/`` ` `` marks not resolvable; only a–z named marks.** `address.rs:228`. Fix: support the previous-context marks.
- [ ] **#X17 — Excess leading addresses not discarded.** `address.rs`. Fix: keep only the last two.
- [ ] **#X18 — Missing `~`, `recover` commands; `preserve` (see #X2).** `ex/command.rs` enum. Fix: add variants.
- [ ] **#X19 — `showmode` defaults `true`; spec default unset.** `options.rs:101`. Fix: default `false`.
- [ ] **#X20 — Missing `set` options & `warn` message before `!`.** Same list as vi-#V13; `warn`/`beautify`/`mesg`/`redraw`/`remap`/`slowopen`/`directory`/`edcompatible` absent. Fix: add.
- [ ] **#X21 — Unreadable `.exrc` silently ignored.** `config.rs:40` returns `None`; spec says it "shall be an error." Fix: surface the error.
- [ ] **#X22 — `write` ignores readonly / pathname-changed / partial-write rules.** Write path doesn't consult `FileManager.readonly` or enforce spec write rules 5/6. Fix: add the guards.
- [ ] **#X23 — `r !cmd` uses `Stdio::null()` for the command's stdin.** Spec: the editor's stdin. Fix: inherit stdin.

### Detailed conformance matrix

#### OPTIONS
- [x] `-R` CONFORMS; `-v` CONFORMS (`lib.rs`).
- [ ] **`-c`/`+command` PARTIAL** — run unconditionally, not only on first existing-file load.
- [ ] **`-s` PARTIAL** (#X3); **`-r` MISSING** (#X5); **`-t` MISSING** (#X8); **`-w` PARTIAL** (parsed, not applied).

#### OPERANDS / STDIN
- [ ] **EOF-as-SIGHUP MISSING** — #X6.  **`{LINE_MAX}` limit MISSING** — minor.
- [ ] **stdin-not-tty ⇒ `-s` MISSING** — #X11.

#### ENVIRONMENT VARIABLES
- [x] `HOME`, `SHELL` CONFORM (`options.rs:142`).
- [x] `EXINIT` works (but not suppressed under `-s` — #X3).
- [ ] **`LANG`/`LC_*` MISSING** (#X14); **`COLUMNS`/`LINES` MISSING** (ioctl only); **`TERM` PARTIAL** (read before mode applied).

#### ASYNCHRONOUS EVENTS
- [ ] **All signals MISSING** — #X1.

#### STDOUT / STDERR
- [x] Line output to stdout, diagnostics to stderr CONFORM.

#### Addressing
- [x] `. $ n +n -n % ,` CONFORM (`address.rs`).
- [x] **`/re/`,`?re?` now BRE** (#X4, phase 4). Still open: **trailing-delimiter capture in `/re/`** (phase 10), **offset dropped** (#X9); **`;`==`,`** (#X10); **line-0 rejected** (#X15); **`'`/`` ` `` marks** (#X16); **excess not discarded** (#X17).

#### Commands
- [x] `ar co/t d m nu p pu q(!) rew se(t) u ya = # & ya` CONFORM.
- [ ] **`g`/`v` single-pass** (#X7); **`s` gaps** (#X12); **`sh[ell]` no `-i`** (#X13).
- [ ] **PARTIAL (modifier/arg gaps):** `a`/`i`/`c` (no `!` autoindent toggle), `cd`, `e`/`n` (`+command` & file args), `f`, `j` (`!`, two-space rule), `l` (escape table), `o`, `q` (remaining-files check), `r` (#X23), `so`, `ta`, `vi[sual]`, `w`/`wq`/`x` (#X22), `z` (`!`/multi-type), `!` (warn), `@`/`@@`.
- [ ] **MISSING:** `pre[serve]`, `rec[over]`, `~` (#X18).

#### `set` options
- [x] Implemented: `ai ap aw eb exrc ic list magic nu para ro report scroll sections sh sw sm showmode ts tl tags term terse timeout window wm ws wa`.
- [ ] **MISSING:** `beautify directory edcompatible mesg prompt redraw remap slowopen warn` (#X20).
- [ ] **`showmode` default DIVERGES** (#X19).

#### EXIT STATUS / CONSEQUENCES OF ERRORS
- [x] 0/1 propagated; silent-mode error ⇒ exit 1.
- [ ] **stdin-tty distinction PARTIAL** — uses `silent_mode`, not actual tty test (#X11).

### Test coverage signal
Not covered:
- [ ] Signal handling / `preserve` / EOF preservation — #X1/#X2/#X6.
- [ ] `-s` suppressing EXINIT/`.exrc` — #X3.
- [ ] Address offset + `;` semantics — #X9/#X10.
- [ ] `global`/`v` with line-count-changing commands — #X7.
- [ ] substitute empty-pattern reuse, count, `l`/`#`, case escapes — #X12.

### Suggested PR groupings
- **PR ex-A — "signals & preserve"**: #X1, #X2, #X6 (+ vi-#V1-#V3 shared infra).
- **PR ex-B — "batch/startup"**: #X3, #X5, #X8, #X11, #X21.
- **PR ex-C — "addressing"**: #X4, #X9, #X10, #X15, #X16, #X17.
- **PR ex-D — "global & substitute"**: #X7, #X12, #X13, #X22, #X23.
- **PR ex-E — "options & misc"**: #X18, #X19, #X20, #X14 (locale, cross-cutting).

---

## Reference

When fixes land, tick the box and append `✓ fixed in PR #NNN` inline (do not
rewrite findings). Add an entry to the table in `../audits.md §9`.
