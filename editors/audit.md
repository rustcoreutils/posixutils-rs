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
| SIGHUP | handler → writes buffer to `ed.hup` (`ed_main.rs:46-67`, `editor.rs:1541-1572`) | ✓ fixed (phase 6) — preserves buffer to `$TMPDIR/vi.recover` (`recover.rs`) |
| SIGINT | flag + `?` (`ed_main.rs:41-43`, `editor.rs:1523-1538`) | ✓ fixed (phase 5) — `^C`/SIGINT rings bell + cancels command (`signals.rs`) |
| SIGQUIT | `SIG_IGN` (`ed_main.rs:54-55`) | **none** |
| SIGWINCH | N/A (non-visual) | ✓ fixed (phase 5) — resize → refresh size + redraw |
| SIGCONT | N/A | ✓ fixed (phase 5) — resume → re-enter raw mode + redraw |
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
- [x] **#V1 — SIGWINCH not handled.** ✓ fixed (phase 5): new `vi/signals.rs` installs a SIGWINCH handler (atomic flag); the input loop catches the `EINTR` (`reader.rs` now surfaces it as `ViError::Interrupted`), calls `terminal.refresh_size()`, and `refresh_screen()` redraws at the new size. PTY test: `test_pty_vi_resize_survives_and_saves`.
- [x] **#V2 — SIGCONT not handled.** ✓ fixed (phase 5): SIGCONT handler set; on resume the loop re-enables raw mode, re-enters the alternate screen, refreshes size, and redraws (`handle_pending_signals`).
- [x] **#V3 — SIGHUP not handled; no buffer preservation.** ✓ fixed (phase 6): new `vi/recover.rs` + SIGHUP/SIGTERM handlers (`signals.rs`). On hangup/termination, or EOF-on-input, a modified buffer is written to a recovery file under `$TMPDIR/vi.recover` (0600) and the user is mailed (best effort). *(behaviorally verified: `kill -HUP` on a modified ex session writes the recovery file.)*

#### Major
- [x] **#V4 — `-r` recovery hard-errors and exits.** ✓ fixed (phase 6): `vi -r` lists recoverable buffers; `vi -r file` recovers the newest saved buffer for that file (`Editor::recover`). Stale recovery files (>14 days) are pruned at startup.
- [x] **#V5 — `-t tagstring` hard-errors; `^]` is a stub.** ✓ fixed (phase 7): new `vi/tags.rs` parses ctags `tags` files (honoring `tags`/`taglength`), does a literal tagstring lookup, opens the target file, and jumps to the line-number or `/pattern/` address. Wired to `-t`, `:tag`, and `^]`.
- [x] **#V6 — Sentence motions `(` / `)` parsed but unhandled.** ✓ fixed (phase 8): `move_sentence_{forward,backward}` in `motion.rs` (POSIX boundaries: `.!?` + two spaces/EOL, or a blank line) wired into command and operator-motion dispatch. Tests: `motion::tests::test_sentence_*`, `test_pty_vi_sentence_motion_delete`.
- [x] **#V7 — `_` (line/first-non-blank) parsed but unhandled.** ✓ fixed (phase 8): `_` moves to the first non-blank of the line `count-1` lines down. Test: `test_pty_vi_underscore_first_nonblank`.
- [x] **#V8 — `ISIG` cleared in raw mode → SIGINT dropped.** ✓ fixed (phase 5): `^C` (byte 0x03 → `Key::Ctrl('c')`) now rings the bell and resets the command parser (`interrupt_command`); a SIGINT *signal* (e.g. `kill -INT`) is also caught via the handler and routed to the same path. PTY test: `test_pty_vi_interrupt_cancels_count`.
- [x] **#V9 — `EXINIT=""` does not suppress `$HOME/.exrc`.** ✓ examined (phase 9): the current code already branches on `var_os("EXINIT").is_some()` (not non-empty), so a set-but-empty `EXINIT` correctly suppresses `.exrc` (the audit's line ref was stale). Verified behaviorally.
- [x] **#V10 — No `setlocale`; LC_* ignored.** ✓ fixed (phase 4): `run_editor` calls `setlocale(LC_ALL, "")` (`vi/lib.rs`), enabling locale-aware libc regex and `LC_MESSAGES`. (Word/case ops still use Rust built-ins — minor, no spec `shall`.)

#### Minor
- [x] **#V11 — `-w size` consumed but discarded.** ✓ fixed (phase 8): `-w` parses its size into `EditorOptions.window`; `Editor::set_window` applies it.
- [ ] **#V12 — `stty` erase/kill chars not honored** (remaining, minor). Hardcoded `^H`/`^U` work for the common case; honoring `c_cc[VERASE]`/`VKILL` needs termios plumbing into insert mode. Deferred.
- [x] **#V13 — Missing `set` options.** ✓ fixed (phase 9): `beautify`, `directory`, `edcompatible`, `mesg`, `prompt`, `redraw`, `remap`, `slowopen`, `warn` added to `Options` with set/no/query support (`prompt` is wired to the ex prompt; the rest are accepted/stored).
- [x] **#V14 — `^L` and `^R` share one handler.** ✓ fixed (phase 8): `^L` clears the physical screen before redraw; `^R` does a plain redraw. (Per-`@`-line refresh remains a cosmetic nicety.)
- [ ] **#V15 — NUL-in-insert (re-insert last input) not implemented** (remaining, minor). Needs cross-insert-session storage of the previous insertion. Deferred.
- [x] **#V16 — Search is imperfect BRE over ERE.** ✓ fixed (phase 4): `search.rs` now uses `plib::regex` (libc BRE). `convert_pattern` magic mode is a passthrough (libc handles `\(\) \{\} \<\>` and treats `+?|(){}` as literal); nomagic escapes metacharacters. `Substitutor` rewritten with `captures_at` + a back-reference-aware `build_replacement`. Tests: `test_substitute_bre_*`, `test_search_bre_grouping`.

### Detailed conformance matrix

#### OPTIONS
- [x] `-R` CONFORMS — `lib.rs:173`.
- [x] `-c command` / `+command` CONFORMS — `lib.rs:179-185,212-213`.
- [x] **`-r`** ✓ (phase 6, #V4); **`-t`** ✓ (phase 7, #V5).  **`-w` PARTIAL** — #V11.
- [ ] `-s`/`-h`/`--version` accepted (extensions / ex-only on vi) — DIVERGES, harmless.

#### OPERANDS / STDIN / INPUT FILES
- [x] Multiple-file list + first-file open CONFORMS — `FileManager`.
- [x] **STDIN EOF treated as SIGHUP** — ✓ fixed (phase 6), #V3.

#### ENVIRONMENT VARIABLES
- [x] `COLUMNS`/`LINES` CONFORMS — `ui/terminal.rs:141-152`.
- [x] `EXINIT`/`HOME` CONFORMS — `editor/mod.rs` (#V9 examined: set-but-empty suppresses `.exrc`).
- [x] `SHELL` CONFORMS — `options.rs:142`.
- [x] **`LANG`/`LC_ALL`/`LC_COLLATE`/`LC_CTYPE`/`LC_MESSAGES`** — ✓ fixed (phase 4), #V10.
- [ ] **`TERM` PARTIAL** — read but no terminfo lookup.

#### ASYNCHRONOUS EVENTS
- [x] **SIGWINCH / SIGCONT / SIGHUP** — ✓ fixed (phases 5–6), #V1/#V2/#V3.
- [x] **SIGINT** — ✓ fixed (phase 5), #V8; `:suspend`/resume now redraws via the SIGCONT path (#V2).

#### Command set
- [x] Motions `h j k l w W b B e E 0 $ ^ f F t T ; , G H M L { } [[ ]] |` CONFORM — `command/motion.rs`, `editor/mod.rs`.
- [x] **`(` `)`** ✓ (phase 8, #V6); **`_`** ✓ (phase 8, #V7).
- [x] Scrolling `^F ^B ^D ^U ^E ^Y z` CONFORM.
- [x] **`^]`** ✓ tag jump (phase 7, #V5); **`^L`/`^R`** ✓ split (phase 8, #V14).
- [x] Editing `i I a A o O c C cc d D dd x X r R y Y p P J ~ < > .` CONFORM.
- [ ] **`s`/`S` PARTIAL** — do not save deleted text to the named buffer (`editor/mod.rs:698,705`).
- [x] `u U`, marks `m ' \``, `: / ? n N % & @ " Q ZZ ! ^^` CONFORM (search is #V16).

#### Insert mode
- [x] ESC, `^H`, `^W`, `^U`, `^T` CONFORM — `mode/insert.rs`.
- [ ] **`^V` PARTIAL** (no visual feedback); **`^D` PARTIAL** (`0^D`/`^^D` edge cases); autoindent-on-blank PARTIAL; **stty erase/kill MISSING** (#V12); **NUL re-input MISSING** (#V15).

#### EXIT STATUS / CONSEQUENCES OF ERRORS
- [x] 0/1 exit code propagated — `lib.rs:73-140`, `vi_main.rs:15`.
- [x] **Unrecoverable-error ⇒ preserve** — ✓ fixed (phase 6), #V3.

### Test coverage signal
Not covered:
- [ ] Signal handling (resize / suspend / hangup) — #V1-#V3.
- [ ] `-r` / `-t` behavior — #V4/#V5.
- [x] Sentence motions `(` `)`, `_` — ✓ fixed (phase 8), #V6/#V7.
- [x] `EXINIT=""` vs `.exrc` ordering — ✓ examined (phase 9), #V9; `.exrc` security unit-tested in `config.rs`.

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
- [x] **#X1 — No signal handlers (SIGHUP/SIGINT/SIGTERM).** ✓ fixed (phases 5–6): SIGINT (phase 5); SIGHUP/SIGTERM install in `run_editor` for both modes and trigger buffer preservation (phase 6).
- [x] **#X2 — `preserve` command + EOF/SIGHUP file preservation missing.** ✓ fixed (phase 6): added `ExCommand::Preserve` (`:pre[serve]`) and `:rec[over]`; the ex command loop preserves a modified buffer on EOF and on hangup. Integration test: `test_ex_preserve_and_recover_roundtrip`.
- [x] **#X3 — `-s` does not suppress EXINIT / `.exrc`.** ✓ fixed (phase 9): `load_startup_config()` is skipped entirely in silent mode (spec ex.md §94217). Test: `test_ex_silent_suppresses_exinit`.
- [x] **#X4 — Address `/re/`,`?re?` use raw ERE.** ✓ fixed (phase 4): `address.rs` compiles address patterns with `plib::regex` BRE. (Note: a *separate* pre-existing parser bug captures the trailing delimiter into the pattern — e.g. `/cherry/` stores `cherry/` — so `/re/` addresses don't match; this is address-parser fidelity, tracked under #X-addressing for phase 10, not a regex-engine issue.)
- [x] **#X5 — `-r` exits with error instead of listing recoverable files.** ✓ fixed (phase 6): shared with #V4.
- [x] **#X6 — EOF on stdin not treated as SIGHUP.** ✓ fixed (phase 6): EOF in ex command/insert mode now preserves a modified buffer before quitting.

#### Major
- [ ] **#X7 — `global`/`v` appears single-pass.** `parser.rs:519-544` stores the command string and dispatches per match without a mark-first pass; line-inserting/deleting commands corrupt iteration. Fix: collect matching line numbers first, then execute against stable marks.
- [x] **#X8 — `-t tagstring` hard-errors.** ✓ fixed (phase 7): shared `vi/tags.rs`; `ex -t` and `:tag` resolve via the ctags file. Tests: `tags::tests::*`, `test_ex_tag_lookup`.
- [ ] **#X9 — Address offset after an address is parsed then discarded.** `address.rs:296-310` computes the offset but never applies it to the base address. Fix: add the offset to the resolved address.
- [ ] **#X10 — `;` separator treated like `,`.** `address.rs:315`; does not set current line to the first address before parsing the second. Fix: resolve first address, set `.`, then parse second.
- [x] **#X11 — stdin-not-a-tty does not auto-enable `-s`.** ✓ fixed (phase 9): `run_editor` sets `silent_mode` when `stdin().is_terminal()` is false (spec ex.md §94234).
- [ ] **#X12 — `substitute` gaps.** `parser.rs:506-508` errors on empty pattern instead of reusing the last RE; missing flags `l`, `#`, count; `~`, `%`, `\l\u\L\U` in replacement unimplemented; `c` (confirm) parsed but no interactive loop; `\n`-split doesn't split the buffer line. Fix: extend `SubstituteFlags` + `expand_replacement`.
- [ ] **#X13 — `shell` command does not pass `-i`.** `shell.rs` interactive path. Spec §`sh -i`. Fix: add `.arg("-i")`.
- [x] **#X14 — No `setlocale`; LC_* ignored.** ✓ fixed (phase 4): shared `setlocale(LC_ALL, "")` in `run_editor` (vi-#V10).

#### Minor
- [ ] **#X15 — Line-0 address rejected for `a`/`i`/`r`/`=`/`put`.** `address.rs:56-60`. Fix: allow 0 for the commands the spec lists.
- [ ] **#X16 — `'`/`` ` `` marks not resolvable; only a–z named marks.** `address.rs:228`. Fix: support the previous-context marks.
- [ ] **#X17 — Excess leading addresses not discarded.** `address.rs`. Fix: keep only the last two.
- [x] **#X18 — Missing `~`, `recover` commands; `preserve`.** ✓ partial: `:preserve` and `:recover` added (phase 6); the `~` substitute-repeat command remains for phase 10.
- [x] **#X19 — `showmode` defaults `true`; spec default unset.** ✓ fixed (phase 9): default is now `false`.
- [x] **#X20 — Missing `set` options.** ✓ fixed (phase 9): shared with #V13. (The `warn`-message-before-`!` behavior is stored as the `warn` option; emitting the warning text is a minor follow-up.)
- [ ] **#X21 — Unreadable `.exrc` silently ignored** (remaining, minor). `read_safe_exrc` returns `None` for both missing and unreadable; distinguishing them (to error on exists-but-unreadable) needs a `Result` return. Deferred.
- [ ] **#X22 — `write` ignores readonly / pathname-changed / partial-write rules.** Write path doesn't consult `FileManager.readonly` or enforce spec write rules 5/6. Fix: add the guards.
- [ ] **#X23 — `r !cmd` uses `Stdio::null()` for the command's stdin.** Spec: the editor's stdin. Fix: inherit stdin.

### Detailed conformance matrix

#### OPTIONS
- [x] `-R` CONFORMS; `-v` CONFORMS (`lib.rs`).
- [ ] **`-c`/`+command` PARTIAL** — run unconditionally, not only on first existing-file load.
- [x] **`-r`** ✓ (phase 6, #X5); **`-t`** ✓ (phase 7, #X8).  **`-s` PARTIAL** — #X3; **`-w` PARTIAL** — parsed, not applied.

#### OPERANDS / STDIN
- [x] **EOF-as-SIGHUP** — ✓ fixed (phase 6), #X6.  **`{LINE_MAX}` limit MISSING** — minor.
- [x] **stdin-not-tty ⇒ `-s`** — ✓ fixed (phase 9), #X11.

#### ENVIRONMENT VARIABLES
- [x] `HOME`, `SHELL` CONFORM (`options.rs:142`).
- [x] `EXINIT` works interactively; suppressed under `-s`/non-tty — ✓ (phase 9), #X3.
- [ ] **`LANG`/`LC_*` MISSING** (#X14); **`COLUMNS`/`LINES` MISSING** (ioctl only); **`TERM` PARTIAL** (read before mode applied).

#### ASYNCHRONOUS EVENTS
- [x] **Signals (SIGINT/SIGHUP/SIGTERM)** — ✓ fixed (phases 5–6), #X1.

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
- [x] **Added (phase 9):** `beautify directory edcompatible mesg prompt redraw remap slowopen warn` (#X20).
- [x] **`showmode` default** ✓ fixed (phase 9), #X19.

#### EXIT STATUS / CONSEQUENCES OF ERRORS
- [x] 0/1 propagated; silent-mode error ⇒ exit 1.
- [x] **stdin-tty distinction** ✓ fixed (phase 9): non-tty ⇒ silent (#X11).

### Test coverage signal
Not covered:
- [ ] Signal handling / `preserve` / EOF preservation — #X1/#X2/#X6.
- [x] `-s` suppressing EXINIT/`.exrc` — ✓ fixed (phase 9), #X3.
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
