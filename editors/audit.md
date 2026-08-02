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

## Closeout (2026-08-02)

All three utilities closed out across ten phases. Two things are worth
recording.

**1. Several recorded blockers were wrong.** #X10's "requires threading the
separator through `AddressRange`" was a non-issue — all five constructors live
in `address.rs` and no struct literal exists elsewhere. #X15's "needs command
context threaded into resolution" was factually wrong — `a`/`i`/`=`/`put` never
called `resolve` at all. And the `;`-vs-`,` bug was in the **comma** path, the
opposite of what #X10 said. #X12's claim that the `c`/`p` flags "work" was
false: both were dead code with zero call sites.

**2. Auditing the audit found 14 defects no finding list contained.** One was
silent data loss.

| New | Sev | Summary |
|---|---|---|
| #X24 | **Critical** | `:1,5w out` silently wrote the **entire buffer**; `file::write_range` existed, was tested, and had zero callers |
| #X25 | Major | ex insert-class commands ignored every non-literal address — `$a` appended after line **1** |
| #X26 | Major | substitute `c` and `p` flags were parsed, stored, and dead |
| #X27 | Major | `:set ro` did not block `:w` (only `-R` did) |
| #V17 | Major | `s`/`S` were un-undoable and ignored counts and registers |
| #X28 | Minor | a substitute's pattern did not become the last RE |
| #X29 | Minor | `:x` always wrote; `x!`/`xit!` were unparsed |
| #X30 | Minor | a multibyte mark name panicked the address parser |
| #X31 | Minor | `read_shell_output` was a dead duplicate carrying the same defect |
| #V18 | Minor | `:r !cmd` did not drop raw mode around the child |
| #V19 | Minor | a second `u` after any change operator empties the line *(still open)* |
| — | Minor | `:l` was not unambiguous: caret notation, unescaped `\` and `$` |
| — | Minor | `@@` failed with `Buffer "@" is empty` |
| — | Minor | `j` implemented one of POSIX's five join rules |

Every fix was confirmed to fail against the pre-fix code before being
accepted. Where a reference existed it was used: `vim -e` for addressing and
write rules, and the spec's own worked examples for substitute case
conversion (ex.md §95726-95732), which now reproduce byte-for-byte. `vim -e`
was **rejected** as an oracle for substitute — in this harness it does not
apply even a plain `s/foo/XXX/`.

**Deferrals reconsidered, per instruction.** All ten recorded deferrals are
closed. #X12's `\n` buffer-splitting required a new `ChangeKind::ReplaceLines`
in the undo model, which was the honest reason it had been deferred. `TERM`
terminfo is the one item accepted as a deliberate non-goal, with rationale
recorded inline.

**Remaining open: 3**, all genuine and itemized — #V19, the insert-mode
`^V`/`^D`/autoindent partials, and the remaining ex command modifier gaps
(that box was one opaque checkbox covering ~15 commands; it is now a named
list, with `j`, `q`, `r`, `w`/`wq`/`x`, `l` and `@@` struck off).

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

**Implementation:** `ed_main.rs` (122) + `ed/editor.rs` (1732) + `ed/parser.rs`
(883) + `ed/buffer.rs` (550) + `ed/error.rs` (69) + `ed/mod.rs` (19)
**Tests:** `tests/ed/mod.rs` (1527, 157 tests)
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
- [x] **`G V`** — ✓ #E6 fixed; `test_ed_global_interactive_forbids_append`.
- [x] `h H P q Q u` CONFORMS.
- [x] **`s` — BRE engine + count fixed (phase 1)**; #E3 (no-op marks modified) lands in phase 2. Flags `g p l n % & \&` and `\<newline>` split CONFORM.
- [x] `!cmd` CONFORMS — `%`/`!` expansion, completion marker (`editor.rs:710-775`).
- [x] `x z #` are non-POSIX extensions (treated as wq / scroll / null) — N/A, harmless. *Not a finding; ticked so it stops reading as open work.*

#### Addressing
- [x] `. $ n 'x +n -n + - , ;` and `/re/`,`?re?` wrap CONFORM — `parser.rs`, `editor.rs:265-391`.
- [x] **Intermediate out-of-range** — ✓ #E7 fixed; `test_ed_intermediate_address_out_of_range_ok`.
- [x] **Compound `,,`/`;;`** — ✓ #E10 examined and conforming; `test_ed_comma_separator`, `test_ed_semicolon_separator`, `test_ed_semicolon_with_offset`, `test_ed_semicolon_current_line`.

#### EXIT STATUS / CONSEQUENCES OF ERRORS
- [x] **Command errors exit 0** — ✓ fixed (phase 2), #E2.
- [x] **Non-terminal stdin error policy** — ✓ fixed (phase 2): any error sets a non-zero exit status (folded into #E2).

### Test coverage signal
Not covered:
- [x] BRE-specific patterns (`\(\)`, `\{n,m\}`, back-refs) — `test_ed_bre_backreference_pattern`, `test_ed_bre_grouping_address`, `test_ed_bre_interval_address`, `test_ed_sub_backreference_replacement`.
- [x] Exit status after a command error — `test_ed_exit_status_on_command_error`, `test_ed_exit_status_clean_is_zero`.
- [x] Identical-result substitute (`s/x/x/`) — `test_ed_identity_substitute_marks_modified`, `test_ed_sub_identity_is_not_error`.
- [x] EOF behavior in command/input mode — `test_ed_eof_acts_as_quit_warns_when_modified`, `test_ed_eof_in_input_mode_finalizes_append`, `test_ed_eof_in_input_mode_terminates_then_quits`.
- [x] `G`/`V` forbidden sub-commands — `test_ed_global_interactive_forbids_append`.

### Suggested PR groupings
- **PR ed-A — "BRE engine"**: #E1 (+ #E9 count rework, #E8 collation).
- **PR ed-B — "exit status & error policy"**: #E2, #E3.
- **PR ed-C — "EOF / input-mode"**: #E4, #E5.
- **PR ed-D — "global & addressing"**: #E6, #E7, #E10.

---

## `vi`

**Implementation:** `vi_main.rs` (25) + `vi/` ≈ 15.8k lines (entry/dispatch
`lib.rs`, `editor/`, `command/`, `mode/`, `buffer/`, `ui/`, `input/`, `search.rs`,
`options.rs`, `file.rs`, `register.rs`, `undo.rs`, `shell.rs`, `config.rs`).
**Tests:** `tests/integration/mod.rs` (2052), `tests/pty/mod.rs` (377+),
`tests/headless/mod.rs` (806+)
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
- [x] **#V12 — `stty` erase/kill chars not honored** (~~remaining, minor~~). ~~Deferred.~~ **✓ fixed (2026-08-02).** `Terminal` now exposes `erase_char()`/`kill_char()` read from the termios captured *before* raw mode was entered (treating 0 and `_POSIX_VDISABLE` as "unset"), and `enter_insert` carries them into the `InsertState`. The hardcoded `^H`/`^U` still work; a configured erase/kill character now works too. Test `test_insert_honors_configured_erase_and_kill_chars`, driven at the `InsertState` level since a headless editor has no termios to read and the PTY harness cannot set `stty erase` for the child.
- [x] **#V13 — Missing `set` options.** ✓ fixed (phase 9): `beautify`, `directory`, `edcompatible`, `mesg`, `prompt`, `redraw`, `remap`, `slowopen`, `warn` added to `Options` with set/no/query support (`prompt` is wired to the ex prompt; the rest are accepted/stored).
- [x] **#V14 — `^L` and `^R` share one handler.** ✓ fixed (phase 8): `^L` clears the physical screen before redraw; `^R` does a plain redraw. (Per-`@`-line refresh remains a cosmetic nicety.)
- [x] **#V15 — NUL-in-insert (re-insert last input) not implemented** (~~remaining, minor~~). ~~Deferred.~~ **✓ fixed (2026-08-02).** `Editor` keeps the text of the last completed insert session and hands it to the next one, where a NUL replays it. Note the input reader maps byte 0 to `Key::Ctrl('@')`, not `Key::Char('\0')`, so the obvious match arm silently never fires — recorded because it cost a PTY round trip to find. PTY test `test_pty_vi_nul_reinserts_previous_input`.
- [x] **#V16 — Search is imperfect BRE over ERE.** ✓ fixed (phase 4): `search.rs` now uses `plib::regex` (libc BRE). `convert_pattern` magic mode is a passthrough (libc handles `\(\) \{\} \<\>` and treats `+?|(){}` as literal); nomagic escapes metacharacters. `Substitutor` rewritten with `captures_at` + a back-reference-aware `build_replacement`. Tests: `test_substitute_bre_*`, `test_search_bre_grouping`.

### Detailed conformance matrix

#### OPTIONS
- [x] `-R` CONFORMS — `lib.rs:173`.
- [x] `-c command` / `+command` CONFORMS — `lib.rs:179-185,212-213`.
- [x] **`-r`** ✓ (phase 6, #V4); **`-t`** ✓ (phase 7, #V5).  **`-w` PARTIAL** — #V11.
- [x] `-s`/`-h`/`--version` accepted (extensions / ex-only on vi) — DIVERGES, harmless. *Not a finding; ticked as an accepted extension.*

#### OPERANDS / STDIN / INPUT FILES
- [x] Multiple-file list + first-file open CONFORMS — `FileManager`.
- [x] **STDIN EOF treated as SIGHUP** — ✓ fixed (phase 6), #V3.

#### ENVIRONMENT VARIABLES
- [x] `COLUMNS`/`LINES` CONFORMS — `ui/terminal.rs:141-152`.
- [x] `EXINIT`/`HOME` CONFORMS — `editor/mod.rs` (#V9 examined: set-but-empty suppresses `.exrc`).
- [x] `SHELL` CONFORMS — `options.rs:142`.
- [x] **`LANG`/`LC_ALL`/`LC_COLLATE`/`LC_CTYPE`/`LC_MESSAGES`** — ✓ fixed (phase 4), #V10.
- [x] **`TERM`** — read, with no terminfo lookup. **Accepted as a deliberate non-goal (2026-08-02, maintainer decision):** the UI drives the terminal through its own `ui/terminal.rs` abstraction rather than capability strings, so a terminfo layer would add a dependency and a parallel capability path for no behavioral gain on any terminal this project targets.

#### ASYNCHRONOUS EVENTS
- [x] **SIGWINCH / SIGCONT / SIGHUP** — ✓ fixed (phases 5–6), #V1/#V2/#V3.
- [x] **SIGINT** — ✓ fixed (phase 5), #V8; `:suspend`/resume now redraws via the SIGCONT path (#V2).

#### Command set
- [x] Motions `h j k l w W b B e E 0 $ ^ f F t T ; , G H M L { } [[ ]] |` CONFORM — `command/motion.rs`, `editor/mod.rs`.
- [x] **`(` `)`** ✓ (phase 8, #V6); **`_`** ✓ (phase 8, #V7).
- [x] Scrolling `^F ^B ^D ^U ^E ^Y z` CONFORM.
- [x] **`^]`** ✓ tag jump (phase 7, #V5); **`^L`/`^R`** ✓ split (phase 8, #V14).
- [x] Editing `i I a A o O c C cc d D dd x X r R y Y p P J ~ < > .` CONFORM.
- [x] **`s`/`S`** — ✓ fixed (2026-08-02) as **#V17**. They were handled in the *pre-parser fast path* (`editor/mod.rs`, before keys reach the command parser), so besides not saving to a register they recorded **no undo** and accepted neither a count (`3s`) nor a register prefix (`"as`) — even though `'s'`/`'S'` were already in the parser's command table. Both fast-path arms are deleted; `s`/`S` now dispatch beside `c` and route through `command::operator::change`, inheriting its register, undo and cursor handling. Verified behaviorally identical to `cl`/`cc`, undo included. Tests `test_s_and_upper_s_behave_as_change_operators`, `test_s_accepts_a_count`, `test_upper_s_substitutes_whole_lines`, plus PTY `test_pty_vi_substitute_char_saves_to_register`.
- [ ] **#V19 — a second `u` after a change operator empties the line.** *(Found 2026-08-02 while verifying #V17; pre-existing, not introduced by it.)* `cl`/`s` on `abcdef` gives `Xbcdef`; the first `u` correctly yields `bcdef` (undoing the inserted text) but a second `u` yields an empty line rather than restoring `abcdef`. Affects the shared `change` operator, so `c`, `cc`, `s` and `S` alike. Out of scope for #V17, which was about `s`/`S` reaching that path at all; recorded here rather than silently absorbed.
- [x] `u U`, marks `m ' \``, `: / ? n N % & @ " Q ZZ ! ^^` CONFORM (search is #V16).

#### Insert mode
- [x] ESC, `^H`, `^W`, `^U`, `^T` CONFORM — `mode/insert.rs`.
- [ ] **`^V` PARTIAL** (no visual feedback); **`^D` PARTIAL** (`0^D`/`^^D` edge cases); autoindent-on-blank PARTIAL. ~~stty erase/kill MISSING (#V12)~~ ✓ fixed 2026-08-02; ~~NUL re-input MISSING (#V15)~~ ✓ fixed 2026-08-02.

#### EXIT STATUS / CONSEQUENCES OF ERRORS
- [x] 0/1 exit code propagated — `lib.rs:73-140`, `vi_main.rs:15`.
- [x] **Unrecoverable-error ⇒ preserve** — ✓ fixed (phase 6), #V3.

### Test coverage signal
Not covered:
- [x] Signal handling (resize / suspend / hangup) — `test_pty_vi_resize_survives_and_saves` (resizes the PTY so the kernel really delivers SIGWINCH), `test_pty_vi_interrupt_cancels_count`, and `test_ex_preserve_and_recover_roundtrip` for hangup preservation.
- [x] `-r` / `-t` behavior — exercised in `tests/ex/mod.rs` (`-r` round-trip; `test_ex_tag_lookup`), plus 5 unit tests in `vi/tags.rs`. Both flags share one code path with vi.
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
**Tests:** `tests/ex/mod.rs` (543+)
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
- [x] **#X7 — `global`/`v` two-pass.** ✓ examined (phase 10): `execute_ex_global` already collects all matching line numbers first, then executes (reversing for deletes). Conforms.
- [x] **#X8 — `-t tagstring` hard-errors.** ✓ fixed (phase 7): shared `vi/tags.rs`; `ex -t` and `:tag` resolve via the ctags file. Tests: `tags::tests::*`, `test_ex_tag_lookup`.
- [x] **#X9 — Address offset after an address is parsed then discarded.** ✓ fixed (phase 10): added `Address::Offset(base, n)`; `parse_address_range` now wraps both addresses with their trailing `+n`/`-n`. Tests: `test_ex_address_offset_*`. Also fixed the trailing-delimiter bug in `/re/` ex search (`split_search` in `editor/mod.rs`): the pattern no longer includes the closing delimiter, and a trailing `+n`/`-n` offset is honored (`test_ex_address_search_strips_delimiter`).
- [x] **#X10 — `;` separator treated like `,`** (~~remaining, minor~~). ~~Deferred.~~ **✓ fixed (2026-08-02).** The recorded blocker was overstated: all five `AddressRange` constructors live in `address.rs` and a repo-wide grep finds no struct literal elsewhere, so adding a `semi` field touched no external call site. Note the bug was in the *comma* path — both separators re-based the second address on the first. Now `3;+1p` is lines 3-4 while `3,+1p` resolves `+1` against the original current line; both match `vim -e`. `ed` models the same rule on the address rather than the range (`ed/parser.rs:171-179`). Test `test_ex_semicolon_rebases_second_address_comma_does_not`.
- [x] **#X11 — stdin-not-a-tty does not auto-enable `-s`.** ✓ fixed (phase 9): `run_editor` sets `silent_mode` when `stdin().is_terminal()` is false (spec ex.md §94234).
- [x] **#X12 — `substitute` gaps** (**all closed 2026-08-02**). ~~Deferred (large).~~ **Mostly fixed 2026-08-02.** *Correction to the original finding: the claim that `c`/`p` "work" was false — both were parsed and stored but had zero call sites (see #X26).* Landed: empty-pattern reuse (`s//repl/` and bare `s`), `l` and `#` flags, numeric count, `~` and the `\l \u \L \U \e \E` case escapes, plus `p` and `c` made real. `Substitutor::new`'s seven positional parameters became `SubstituteConfig`. **The POSIX spec's own worked examples (ex.md §95726-95732) now reproduce byte-for-byte** and are pinned as a test. `\n` buffer-splitting landed too: a replacement containing a newline now splits the line, the loop bound grows so later lines in the range are still visited, and — the reason it was deferred — the undo layer gained `ChangeKind::ReplaceLines`, a linewise range replace where the two runs may differ in length. `ChangeKind::Replace` (a within-line `delete_char` loop) is left untouched for ordinary edits, bounding the blast radius. The spec's canonical entry form also works now: a `<backslash><newline>` continues an ex command onto the next input line, which the reader previously cut in two, leaving the trailing `/` to parse as a bare search.
- [x] **#X26 — `c` and `p` substitute flags were dead code.** *(Found 2026-08-02, outside the original audit; Major — and it contradicts #X12's text.)* `SubstituteFlags::parse` set them and `Substitutor` stored them, but `needs_confirm()`/`should_print()` (`search.rs:468,473`) had **zero call sites**: `:s/a/b/c` substituted unconditionally with no prompt and `:s/a/b/p` printed nothing. **✓ fixed** — `p` (and the new `l`/`#`) echo each changed line through `ExResult::CommandOutput`; `c` prompts per match. Per the maintainer's decision the confirmation is read from **standard input in batch as well as interactively**, matching historical ex, so a script can answer inline; `y` accepts, anything else (including EOF) declines. Tests `test_ex_substitute_print_flag_emits_the_line`, `test_ex_substitute_number_and_list_flags`, `test_ex_substitute_confirm_flag_reads_stdin`.
- [x] **#X28 — a substitute's pattern did not become the last RE.** *(Found 2026-08-02.)* POSIX: the `s` pattern becomes "the last regular expression used in the editor", so a following bare `n` or `//` reuses it. `substitute` never recorded it, so the two never converged — which also blocked empty-pattern reuse. **✓ fixed** — `Editor::last_regex` is set by both search and substitute. Test `test_ex_substitute_empty_pattern_reuses_last_regex`.
- [x] **#X13 — `shell` command does not pass `-i`.** ✓ fixed (phase 10): `ShellExecutor::interactive` invokes the shell with `-i`.
- [x] **#X14 — No `setlocale`; LC_* ignored.** ✓ fixed (phase 4): shared `setlocale(LC_ALL, "")` in `run_editor` (vi-#V10).

#### Minor
- [x] **#X15 — Line-0 address rejected for `a`/`i`/`r`/`=`/`put`** (~~remaining, minor~~). ~~Deferred.~~ **✓ fixed (2026-08-02).** The recorded blocker was factually wrong: `a`/`i`/`=`/`put` never called `resolve` at all — they read a raw `usize` out of the parser (which is #X25) — so only `r` actually hit the rejection. Line 0 is now permitted via `AddrCtx::allow_zero` for the commands that insert *after* an address. `0r file` matches `vim -e`. Test `test_ex_line_zero_address_for_read`.
- [x] **#X16 — `'`/`` ` `` marks not resolvable in ex addresses** (~~remaining, minor~~). ~~Deferred.~~ **✓ fixed (2026-08-02).** The blocker was accurate but mechanical: introduced `AddrCtx { buffer, current, marks, allow_zero }` and routed every resolve site through `Editor::addr_ctx()`/`resolve_range`, which also absorbed #X15. Backtick is now accepted alongside `'` (#X30). `2ma a` then `'ap` matches `vim -e`. Test `test_ex_mark_usable_as_address`.
- [x] **#X17 — Excess leading addresses not discarded** (~~remaining, minor~~). ~~Deferred.~~ **✓ fixed (2026-08-02).** `parse_address_range` is now an accumulating loop that keeps only the last two addresses, modelled on `ed`'s `normalize_addrvec` (`ed/parser.rs:324-361`) — the algorithm transplants, the code does not. It also yields the trailing separator, which is exactly the flag #X10 needed, and adopts `ed`'s omitted-address defaults. Previously `1,2,3p` left a stray leading `,` for the command splitter and failed as "Invalid command"; it now prints lines 2-3, matching `vim -e`. Test `test_ex_excess_leading_addresses_are_discarded`.
- [x] **#X18 — Missing `~`, `recover` commands; `preserve`.** ✓ `:preserve` and `:recover` added (phase 6); **`~` added 2026-08-02.** `~` pairs the previous substitute's *replacement* with the **last RE**, which distinguishes it from `&` (previous pattern *and* replacement). Implementing it exposed that a *search* was not updating the last-RE state, so `~` after `/pat/` reused the stale substitute pattern — POSIX's "last RE used in the editor" spans both. Test `test_ex_tilde_uses_last_regex_with_previous_replacement` (confirmed to fail against the pre-fix code).
- [x] **#X19 — `showmode` defaults `true`; spec default unset.** ✓ fixed (phase 9): default is now `false`.
- [x] **#X20 — Missing `set` options.** ✓ fixed (phase 9): shared with #V13. (The `warn`-message-before-`!` behavior is stored as the `warn` option; emitting the warning text is a minor follow-up.)
- [x] **#X21 — Unreadable `.exrc` silently ignored** (~~remaining, minor~~). ~~Deferred.~~ **✓ fixed (2026-08-02).** `read_safe_exrc` now returns `io::Result<Option<String>>`: `Ok(None)` for absent *and* for a file rejected by the ownership/permission checks (a security decision, and historical ex is silent there), `Err` only for exists-but-unreadable, which the caller reports. Note the diagnostic is only reachable on a tty, since `-s` suppresses startup config (#X3) and piped stdin auto-enables `-s` (#X11) — so it is pinned by unit tests on `read_safe_exrc` rather than an integration test. The new test skips under uid 0, which can read a mode-000 file. Tests `test_read_safe_exrc_unreadable_is_err`, `test_read_safe_exrc_insecure_is_silent_skip_not_error`.
- [x] **#X22 — `write` ignores readonly / pathname-changed / partial-write rules** (~~remaining, moderate~~). ~~Deferred.~~ **✓ fixed (2026-08-02).** Severity was understated: the item contained **#X24**, silent data loss (below). All six numbered rules of `ex.md` §95502-95519 are now implemented in `Editor::write` — readonly (rule 2), named-target-exists (rule 3), pathname-changed-by-`:f`/`:r` (rule 5, via a new `FileManager::pathname_changed` flag cleared on a successful write), and partial-write-over-existing (rule 6). Rules 2/3/5 are overridable by `!` **or** the `writeany` option; **rule 6 is overridable by neither** — it appears in neither override list at §95516-95518. Behavior cross-checked against `vim -e` for the named-target case. Tests: `test_ex_write_range_over_existing_file_is_never_forced`, `test_ex_write_existing_other_file_requires_force`, `test_ex_set_readonly_blocks_write`.
- [x] **#X24 — `:1,5w file` silently wrote the ENTIRE buffer.** *(Found 2026-08-02, outside the original audit; Critical.)* `Editor::write` took the address range as a bare `bool` (`_range`) and discarded it, so every partial write emitted the whole buffer, reported the full line count as if correct, and exited 0. `file::write_range` already existed, was unit-tested (`tests/integration/mod.rs:831`), and had **zero callers**. **✓ fixed (2026-08-02)** — the range is resolved at dispatch and threaded in; output is now byte-identical to `vim -e` for `1,3w`. A partial write also no longer marks the buffer saved. Test `test_ex_write_range_writes_only_that_range` (confirmed to fail against the pre-fix code).
- [x] **#X27 — `:set ro` did not block `:w`.** *(Found 2026-08-02.)* POSIX rule 2 is written against the **`readonly` edit option**, but the write check consulted only `FileManager::is_readonly()`, which reflects the `-R` flag alone; `options.readonly` was a separate, unread field. `options.writeany` was likewise parsed, stored, and never read. **✓ fixed** — `-R` now sets the edit option (POSIX describes `-R` as doing exactly that), the check consults both, and `writeany` is honored as an override. Test `test_ex_set_readonly_blocks_write`.
- [x] **#X29 — `:x` wrote unconditionally.** *(Found 2026-08-02.)* `ex.md` §95537: `xit` on a buffer unmodified since the last complete write is equivalent to `quit`. `:x` and `:wq` parsed to the same `ExCommand::WriteQuit`, so they could not be told apart; `x!`/`xit!` were not parsed at all. **✓ fixed** — added an `xit` discriminator and the `x!`/`xit!` forms. Test `test_ex_xit_on_unmodified_buffer_does_not_write` asserts the file mtime is untouched.
- [x] **#X23 — `r !cmd` uses `Stdio::null()` for the command's stdin** (~~remaining, minor~~). ~~Deferred.~~ **✓ fixed (2026-08-02).** POSIX (ex.md §95278-95280): the program's standard input "shall be set to the standard input of the ex program when it was invoked". Now inherits. Two consequences handled with it: **#V18** — `execute_shell_read` did not drop raw mode around the child, unlike `execute_shell_command`, which matters once stdin is inherited; and **#X31** — `file.rs`'s `read_shell_output` was a dead duplicate of this logic with the same defect and zero callers, so it is deleted rather than left to be rediscovered. Test `test_ex_read_shell_command_inherits_stdin`.
- [x] **#X25 — ex insert-class commands ignored every non-literal address.** *(Found 2026-08-02, outside the original audit; Major.)* `a`, `i`, `pu`, `ma`, `o`, `z`, `=` and `r !cmd` carried a `usize`/`Option<usize>` that the *parser* extracted, and the parser could only read a literal `Address::Line(n)` — every other form fell through `unwrap_or(1)`. **Verified: `$a` on a 6-line buffer appended after line 1.** Same for `.a`, `/re/a`, `'ma`, `$=`, `$pu`, `.z`. **✓ fixed** — all eight variants now carry the `AddressRange` and resolve through `Editor::resolve_target_line`, so every address form works and line 0 (#X15) falls out in one place instead of being special-cased. `$a` now matches `vim -e` exactly. Test `test_ex_append_honors_non_literal_addresses`.
- [x] **#X30 — a multibyte mark name panicked the address parser.** *(Found 2026-08-02.)* `parse_address` read the mark with `chars().nth(1)` but then sliced `&input[2..]`, which is not a char boundary for a multibyte name. **✓ fixed** — slices by `char_indices`, and accepts the backtick form alongside `'`. Test `test_ex_multibyte_mark_name_does_not_panic`.

### Detailed conformance matrix

#### OPTIONS
- [x] `-R` CONFORMS; `-v` CONFORMS (`lib.rs`).
- [x] **`-c`/`+command`** — ✓ fixed (2026-08-02): now runs only when a file was actually opened, per POSIX; it previously ran unconditionally against an empty buffer.
- [x] **`-r`** ✓ (phase 6, #X5); **`-t`** ✓ (phase 7, #X8).  **`-s` PARTIAL** — #X3; **`-w` PARTIAL** — parsed, not applied.

#### OPERANDS / STDIN
- [x] **EOF-as-SIGHUP** — ✓ fixed (phase 6), #X6.  **`{LINE_MAX}` limit MISSING** — minor.
- [x] **stdin-not-tty ⇒ `-s`** — ✓ fixed (phase 9), #X11.

#### ENVIRONMENT VARIABLES
- [x] `HOME`, `SHELL` CONFORM (`options.rs:142`).
- [x] `EXINIT` works interactively; suppressed under `-s`/non-tty — ✓ (phase 9), #X3.
- [x] ~~**`LANG`/`LC_*` MISSING** (#X14)~~ ✓ fixed (phase 4, `setlocale` in `run_editor`); ~~**`COLUMNS`/`LINES` MISSING**~~ ✓ `ui/terminal.rs` honors both over the ioctl, with unit tests. **`TERM` PARTIAL** — accepted non-goal, see the vi entry above.

#### ASYNCHRONOUS EVENTS
- [x] **Signals (SIGINT/SIGHUP/SIGTERM)** — ✓ fixed (phases 5–6), #X1.

#### STDOUT / STDERR
- [x] Line output to stdout, diagnostics to stderr CONFORM.

#### Addressing
- [x] `. $ n +n -n % ,` CONFORM (`address.rs`).
- [x] **`/re/`,`?re?` BRE + delimiter + offset** ✓ (phases 4/10, #X4/#X9). Remaining minor: **`;`==`,`** (#X10); **line-0** (#X15); **`'`/`` ` `` marks** (#X16); **excess addresses** (#X17).

#### Commands
- [x] `ar co/t d m nu p pu q(!) rew se(t) u ya = # & ya` CONFORM.
- [x] ~~**`g`/`v` single-pass** (#X7)~~ ✓ examined and conforming (collects matching lines first, then executes); ~~**`s` gaps** (#X12)~~ ✓ closed 2026-08-02; ~~**`sh[ell]` no `-i`** (#X13)~~ ✓ fixed (`ShellExecutor::interactive` passes `-i`).
- [ ] **PARTIAL (modifier/arg gaps).** *Split into named items 2026-08-02; this box was one opaque checkbox covering ~15 commands.* Closed so far: **`j`** (`!` plus the full §95060-95070 rule set — two spaces after `.`, no separator before `)`, empty lines dropped — and the §95043-95057 count/address interaction), **`q`** (remaining-files check), **`r`** (#X23), **`w`/`wq`/`x`** (#X22/#X24/#X29). Also closed: **`l`** (full §95237-95244 escape table — XBD Table 5-1 sequences, three-digit octal per byte otherwise, `\$` for a literal `$`; it had been `^I`-style caret notation that escaped neither backslash nor `$`, so `:l` was not unambiguous, which is the entire point of the command) and **`@@`** (the parser took the second `@` as a buffer *name*, so it failed with `Buffer "@" is empty` instead of repeating the last buffer). `cd`, `so` and `ta` were probed and already work.
  Still open, itemized: `a`/`i`/`c` `!` autoindent toggle, `e`/`n` `+command` and file args, `f`, `o`, `vi[sual]`, `z` (`!`/multi-type), `!` warn message.
- [x] ~~**MISSING:** `pre[serve]`, `rec[over]`~~ ✓ both added (phase 6); ~~`~` (#X18)~~ ✓ added 2026-08-02.

#### `set` options
- [x] Implemented: `ai ap aw eb exrc ic list magic nu para ro report scroll sections sh sw sm showmode ts tl tags term terse timeout window wm ws wa`.
- [x] **Added (phase 9):** `beautify directory edcompatible mesg prompt redraw remap slowopen warn` (#X20).
- [x] **`showmode` default** ✓ fixed (phase 9), #X19.

#### EXIT STATUS / CONSEQUENCES OF ERRORS
- [x] 0/1 propagated; silent-mode error ⇒ exit 1.
- [x] **stdin-tty distinction** ✓ fixed (phase 9): non-tty ⇒ silent (#X11).

### Test coverage signal
Not covered:
- [x] Signal handling / `preserve` / EOF preservation — `test_ex_preserve_and_recover_roundtrip` (writes the buffer, checks the recovery file, re-runs with `-r`).
- [x] `-s` suppressing EXINIT/`.exrc` — ✓ fixed (phase 9), #X3.
- [x] Address offset — ✓ fixed (phase 10), #X9. `;` semantics — minor, #X10.
- [x] `global`/`v` with line-count-changing commands — `test_ex_global_delete` is exactly that (`g/…/d`); #X7 examined and conforming.
- [x] substitute empty-pattern reuse, count, `l`/`#`, case escapes — all closed 2026-08-02 with tests, including the spec's own worked examples (ex.md §95726-95732).

### Suggested PR groupings
- **PR ex-A — "signals & preserve"**: #X1, #X2, #X6 (+ vi-#V1-#V3 shared infra).
- **PR ex-B — "batch/startup"**: #X3, #X5, #X8, #X11, #X21.
- **PR ex-C — "addressing"**: #X4 ✓, #X9 ✓; remaining #X10, #X15, #X16, #X17.
- **PR ex-D — "global & substitute"**: #X7, #X12, #X13, #X22, #X23.
- **PR ex-E — "options & misc"**: #X18, #X19, #X20, #X14 (locale, cross-cutting).

---

## Reference

When fixes land, tick the box and append `✓ fixed in PR #NNN` inline (do not
rewrite findings). Add an entry to the table in `../audits.md §9`.
