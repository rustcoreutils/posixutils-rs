# POSIX.1-2024 Conformance Audit — `more`

**Implementation:** `display/more.rs` (3028 lines) + `display/tests/more/mod.rs` (1265 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 3226–3238
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/more.md`
**Date:** 2026-06-02

## TL;DR

`more` implements most spec surface area correctly — all eight options exist, all interactive commands are present, BRE search is properly wired through `plib::regex`, MORE-env precedence is honored. But several architectural requirements are not met: commands are read from stdin (not stderr/`/dev/tty`); the prompt is written to stdout (not stderr); no SIGCONT or SIGWINCH handlers are registered (window changes detected only by polling); the implicit-stdin path truncates to one line (so `cat foo | more` shows only the first line of `foo`); and `:n`/`:p` errors call `exit()` instead of trying the next/previous file per CONSEQUENCES OF ERRORS.

---

## Priority issues

### Critical

- [x] **#1 — Implicit-stdin path reads only one line.** ✓ fixed: implicit-stdin path now uses `read_to_string` (same as the `-`-operand path); also fixed a latent off-by-one in `print_all_input`'s buffer-source loop that was masked by the truncation. New regression test `test_0_files_multiline_stdin_not_truncated`. *Note: an intermediate version of this fix opened `CommandIO` (raw mode) before reading stdin, which caused `more` with no args + tty stdin to hang in raw mode with no Ctrl-D escape — `MoreControl::new` now slurps stdin first and engages raw mode only after.*
- [x] **#2 — User commands read from stdin, not stderr / `/dev/tty`.** ✓ fixed: new `CommandIO` struct opens stderr (if it is a readable terminal) or falls back to `/dev/tty` `O_RDWR|O_NOCTTY`, with `MoreError::NoCommandSource` and non-zero exit if neither is usable. Direct `libc::tcgetattr`/`cfmakeraw`/`tcsetattr`; cooked-mode restored in `Drop`. Input thread now reads from the chosen channel.

### Major

- [x] **#3 — No SIGCONT handler.** ✓ fixed: libc-only signal subsystem (no new crate dep). `handle_cont` re-applies the saved raw-mode termios (in case we were SIGTSTP'd back into cooked), sets `SIGCONT_PENDING`, and re-installs the SIGTSTP handler. Main loop processes the flag with an unconditional `resize() + refresh()` per POSIX Defect 1185.
- [x] **#4 — SIGWINCH detected only by polling, not by signal.** ✓ fixed: `handle_winch` sets `SIGWINCH_PENDING`; main loop drains the flag and calls `resize()`. The per-tick polling `self.resize()` in `get_input_with_update` is gone; the only remaining sleep is the 80 ms tick that bounds how soon a *signal flag* is noticed. (POSIX winsize refetch via termion's `terminal_size()`, which wraps the `TIOCGWINSZ` ioctl — equivalent to `tcgetwinsize`.)
- [x] **#5 — Prompt written to `self.tty` (stdout) instead of stderr.** ✓ fixed: `Terminal` now owns a `prompt_out: Box<dyn Write + Send>` separate from `tty` (stdout). Set to `CommandIO::writer` (stderr/`/dev/tty`) in interactive mode; stdout in test mode; `io::sink()` in filter mode. Content rendering still goes to stdout.
- [x] **#6 — Prompt missing current filename.** ✓ fixed: `Prompt::More` and `Prompt::Eof` are now structs carrying an `Option<String>` filename plumbed from `MoreControl::current_filename()`. Renders as `foo.txt: -- More --(NN%)` when known; degrades to the historical form for stdin sources.
- [ ] **#7 — `:n` / `:p` file-open errors call `exit()` instead of trying next/prev with non-zero exit.** `more.rs:2484` (handle_error), `2420-2435`. Fix: catch `FileRead` in `:n`/`:p` handlers, set `had_error`, advance/retreat, exit code at end.
- [ ] **#8 — `:e` filename not shell-word-expanded** (XCU 2.6). `more.rs:2210-2251`. Fix: run `wordexp(3)` (libc) on the filename, fail if it yields ≠1 path.
- [ ] **#9 — `''` (return-to-previous) doesn't track "large movement".** Only resets on file open. `more.rs:2382-2385`, 1340. Fix: save `last_line` before any command that moves more than one screenful.

### Minor

- [ ] **#10 — `v` command uses `+N` instead of POSIX-mandated `-c N`** for vi/ex. `more.rs:1916`. Fix: emit `["-c", &N.to_string(), "--", file_path]`.
- [ ] **#11 — `v` editor-name check is `editor == "vi"`** — fails for `/usr/bin/vi`, `vim`, etc. `more.rs:1907`. Fix: compare `Path::new(&editor).file_name()` against `"vi"`/`"ex"`.
- [ ] **#12 — Env-var precedence on resize ignored.** Termion's `terminal_size()` always wins, even when `LINES` / `COLUMNS` env is set. `more.rs:1532-1542`. Fix: re-check env vars inside `Terminal::resize()` and prefer them when set.
- [ ] **#13 — Undocumented `--test` flag exposed in clap surface.** `more.rs:106-112`. Fix: `#[arg(hide = true)]` or move behind a build-only feature gate.
- [ ] **#14 — `MoreError` strings hardcoded English.** Only clap help strings are gettext'd. `more.rs:198-246`. Fix: wrap in `gettext!()`.
- [ ] **#15 — `:t` shells out to `find` + `grep`, treating tagstring as a regex pattern.** `more.rs:1950-1983`. Fix: use literal-string match (or `grep -F`), escape tagstring.

---

## Detailed conformance matrix

### Options

- [ ] **`-c` PARTIAL.** Flag parsed (line 43); `print_over` field never consulted in render path — clear-on-first-screen and per-line redraw not observable.
- [x] `-e` CONFORMS (line 2148).
- [x] `-i` CONFORMS — `RegexFlags::bre().ignore_case()` at line 1649.
- [x] `-n` CONFORMS — overrides LINES/COLUMNS at line 1538.
- [ ] **`-p` PARTIAL.** Commands run on new-file display (lines 2501–2528). Spec requires that if any `-p` command fails, an informational message is written AND remaining `-p` commands for that file are suppressed — current code may `exit()` on error.
- [x] `-s` CONFORMS — squeeze propagates via `SeekPositions`.
- [ ] **`-t` PARTIAL** — see `:t` notes (literal vs regex tag matching).
- [x] `-u` CONFORMS — `plain` flag suppresses backspace/underscore/bold processing.
- [ ] **`+` as option prefix MISSING.** Spec allows but doesn't require; clap doesn't recognize it. (Lowest priority — optional.)

### Operands / STDIN

- [x] **No file operands → stdin** ✓ fixed (same as Critical #1).
- [x] `-` operand routes to stdin CONFORMS (line 1698 path).
- [x] Mixed `-` and file operands CONFORMS.
- [x] Multiple files CONFORMS.

### Environment variables

- [ ] **`COLUMNS` PARTIAL.** Read once at init (line 1394); not re-honored after resize.
- [ ] **`LINES` PARTIAL.** Same — env var not re-checked on SIGWINCH.
- [x] `MORE` CONFORMS — prepended before CLI args (lines 2983–3003); CLI wins on conflict.
- [x] `EDITOR` CONFORMS — line 1901, falls back to `vi`.
- [ ] **`TERM` MISSING (via termion).** Termion reads internally; we don't validate.
- [ ] **`LANG` MISSING.** Only `LC_ALL` set via `setlocale` at line 3006.
- [ ] **`LC_COLLATE` MISSING.**
- [ ] **`LC_CTYPE` MISSING.**
- [ ] **`LC_MESSAGES` MISSING.**
- [ ] `NLSPATH` (XSI) MISSING — optional XSI, acceptable; track anyway.

### Asynchronous events

- [x] **SIGCONT** ✓ fixed (same as Major #3).
- [x] **SIGWINCH as signal** ✓ fixed (same as Major #4).
- [ ] **`tcgetwinsize`-equivalent on resize PARTIAL** — termion ioctl called, env-var precedence ignored (same as Minor #12).

### STDOUT / STDERR

- [x] Content to stdout (non-tty) CONFORMS — `print_all_input` lines 1763–1829.
- [ ] **Content rendering (tty mode) DIVERGES** from "write to stdout" model — uses termion alternate screen on stdout. (Design-level; may be acceptable but worth confirming.)
- [x] **Prompt to stderr** ✓ fixed (same as Major #5).
- [x] **Prompt contains filename** ✓ fixed (same as Major #6).
- [x] EOF prompt contains next file CONFORMS — `Prompt::Eof` line 1622.

### Interactive commands

All 28 spec commands are wired (count-prefix supported on every command that takes one). Issues found:

- [ ] **`[count]s` PARTIAL.** Implemented as plain scroll; spec requires "screenful beginning with line `count` lines after last line on current screen".
- [ ] **`''` DIVERGES.** `last_line` only reset on file open (line 1340); never updated on "large movements" (>1 screenful) — same as Major #9.
- [ ] **`:e filename` PARTIAL.** No shell word expansion; no non-seekable check — same as Major #8.
- [ ] **`:n` / `:p` PARTIAL.** Errors call `exit()` not next/prev-with-error — same as Major #7.
- [ ] **`:t tagstring` PARTIAL.** Tag treated as regex via `grep`; spec means literal name — same as Minor #15.
- [ ] **`v` uses `+N` not `-c N`** — same as Minor #10.
- [ ] **`v` editor name compared as exact string** — same as Minor #11.
- [ ] **`=` uses basename instead of full pathname for files.** Spec allows omitting byte info for stdin (that case works).
- [ ] **`h` help text links to a POSIX 2018 URL** (line 2967), not 2024.

### Extended description / rendering

- [x] Line folding CONFORMS for ASCII.
- [ ] **Multi-column-character splitting** at column boundary is unspecified by POSIX; implementation behavior on a wide character straddling the column limit is undefined. Track for hardening.
- [ ] **Backspace/underscore/embolden (when `-u` absent) PARTIAL.** The 3-byte pattern match in `continious_styled_parse` / `last_styled_parse` (lines 743–848) doesn't implement the full POSIX sequences (`char + n*BS + n*'_'` for underline; `char + n*BS + char` for bold) for n > 1.
- [ ] **`\r` at EOL ignored PARTIAL.** `\r` participates in `line_len` rather than being discarded before `\n`.
- [ ] **Non-printable display PARTIAL.** Line parse breaks on first non-`\x08` control character (lines 673–675), truncating the line rather than displaying it in `ex print` notation.

### Exit status / consequences of errors

- [x] Exit `0` / `>0` CONFORMS (line 2206).
- [ ] **`:e` error affects exit code** when it should not. File state preserved, but exit code is touched.
- [ ] **`:n` / `:p` error → DIVERGES** (same as Major #7).

---

## Test coverage signal

Tests cover `-c`, `-e`, `-i`, `-n`, `-s`, `-u` and most basic interactive commands.

Not covered (each gap is a "write a test" task tied to fixing the corresponding bug):

- [ ] `''` large-movement semantics
- [ ] `:e` shell expansion
- [ ] `v` `-c` flag emission for vi/ex
- [ ] `:n` / `:p` error-path behavior (next file tried, exit code affected)
- [x] Prompt target (stdout vs stderr) — verified manually via `more file >/tmp/out 2>/tmp/err </dev/null`; tracked as audit checkbox closed by Major #5 fix. (No automated test added; would need a PTY harness.)
- [x] SIGCONT behavior (resume from `kill -STOP $$; kill -CONT $$`) — handler installed; covered by code path, no automated test (needs PTY harness).
- [x] SIGWINCH behavior (size change while paused at prompt) — handler installed; covered by code path, no automated test.
- [x] Implicit-stdin (`echo -e 'a\nb\nc' | more`) — would catch Critical #1 (covered by `test_0_files_multiline_stdin_not_truncated` and `test_0_files_empty_stdin`)
- [ ] `-p` command-failure suppression of remaining `-p` commands

---

## Suggested PR groupings

- **PR A — "POSIX I/O channels"**: Critical #1, #2 + Major #5, #6
- **PR B — "Signal handling + resize precedence"**: Major #3, #4 + Minor #12
- **PR C — "Command-mode conformance"**: Major #7, #8, #9 + Minor #10, #11
- **PR D — "Rendering correctness"**: backspace/embolden sequences, `\r` handling, non-printable display, `-c` redraw
- **PR E — "i18n + cleanup"**: Minor #13, #14 + locale env var coverage
