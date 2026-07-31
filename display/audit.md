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
- [x] **#7 — `:n` / `:p` file-open errors call `exit()` instead of trying next/prev with non-zero exit.** `more.rs:2484` (handle_error), `2420-2435`. ✓ **fixed (Phase 7)**: new `MoreControl::examine_adjacent_file` wraps `scroll_file_position`, and on a `FileRead` error sets a new `had_error` field, shows the error prompt, and steps one further in the same direction until a file opens or the list is exhausted — POSIX 107643-107646's "more shall attempt to examine the next file in the argument list, but the final exit status shall be affected". `exit()` now returns non-zero when `had_error` is set even without a fatal message. `handle_error`'s `FileRead` arm no longer aborts, which also gives `:e` the 107595-107597 behavior (report, keep the current file). Regression test `test_audit_next_file_error_affects_exit_status`; the success counterpart is already covered by `test_1_file`/`test_3_files`.
- [x] **#8 — `:e` filename not shell-word-expanded** (XCU 2.6). `more.rs:2210-2251`. ✓ **fixed (Phase 7)**: new `word_expand` calls `wordexp(3)` and is applied in `examine_file` to everything except more's own `#` and `-` tokens. The `libc` crate exposes neither `wordexp` nor `wordexp_t`, so both are declared locally — the structure layout is fixed by POSIX and identical on glibc and macOS. More than one resulting pathname is treated as an error (107593-107594 leaves it unspecified), so 107595-107597's "the current file and screen shall not change" applies. Behaviorally verified: `:e $VAR` and `:e ~/…` now report the *expanded* path. Unit tests `word_expand_applies_shell_expansions`, `word_expand_rejects_multiple_pathnames`.
- [x] **#9 — `''` (return-to-previous) doesn't track "large movement".** Only resets on file open. `more.rs:2382-2385`, 1340. ✓ **fixed (Phase 7)**: `execute` was split into a measuring wrapper around `execute_command`. The wrapper records the current line before the command and, if the line moved by more than a screenful afterwards, stores the pre-command line in `last_line` — implementing 107563-107565's "any movement of more than a screenful of lines" by *measuring* the move rather than by enumerating which commands qualify, so no command can be missed. `''` itself is excluded from tracking. New `SourceContext::screen_lines` supplies the threshold.

### Minor

- [x] **#10 — `v` command uses `+N` instead of POSIX-mandated `-c N`** for vi/ex. `more.rs:1916`. ✓ **fixed (Phase 7)**: emits `["-c", <line>, "--", <file>]` per 107618-107620. Regression test `test_audit_invoke_editor_arguments`.
- [x] **#11 — `v` editor-name check is `editor == "vi"`** — fails for `/usr/bin/vi`, `vim`, etc. `more.rs:1907`. ✓ **fixed (Phase 7)**: compares `Path::new(editor).file_name()`, matching 107617's "If the last pathname component in EDITOR is either vi or ex". Covered by the same regression test, which reaches a script named `vi` through a full path.
- [x] **#12 — Env-var precedence on resize ignored.** Termion's `terminal_size()` always wins, even when `LINES` / `COLUMNS` env is set. `more.rs:1532-1542`. ✓ **fixed (Phase 8)**: `Terminal::resize()` now consults `COLUMNS`/`LINES` on every call and prefers them over the ioctl result, per 107336 and 107357 ("override the system-selected ... size"), with `-n` still taking precedence over `LINES` per 107359. A shared `env_screen_size` helper treats unset, empty, unparsable, and zero values as absent so the system size is used instead; the startup read uses the same helper. Unit test `env_screen_size_rejects_unusable_values`; the resize path itself needs a PTY to observe, but the env values are demonstrably honored end-to-end (`LINES=9` renders 9 rows, `COLUMNS=20` renders 20 columns).
- [x] **#13 — Undocumented `--test` flag exposed in clap surface.** `more.rs:106-112`. ✓ **fixed (Phase 8)**: `#[arg(hide = true)]`. `more --help` no longer mentions it; the flag still works for the test harness.
- [x] **#14 — `MoreError` strings hardcoded English.** Only clap help strings are gettext'd. `more.rs:198-246`. ✓ **fixed (Phase 8)**: `thiserror`'s derived `Display` evaluates its format strings at compile time and so cannot call `gettext`. All three error enums (`MoreError`, `SeekPositionsError`, `SourceContextError`) now carry a hand-written `Display` that passes every message through `gettext`, keeping the existing wording so the tests' expected diagnostics are unchanged. Tree-wide caveat: no `.mo` catalogs ship, so `LC_MESSAGES` stays inert.
- [x] **#15 — `:t` shells out to `find` + `grep`, treating tagstring as a regex pattern.** `more.rs:1950-1983`. ✓ **fixed (Phase 7)**: `goto_tag` now compares the tagstring literally against each tags line's first tab-separated field, per 107607-107608's "the tag named by the tagstring argument". Behaviorally verified: with a tags file containing only `fooxbar`, `:t foo.bar` used to match it (regex `^foo.bar`) and silently open the wrong tag; it now reports the tag as not found. Both subprocesses are gone — a new `find_tags_files` walks the directory tree in-process (not following symbolic links, so a link loop cannot hang the walk), which also removed the now-unreachable `MoreError::CTagsFailed` variant. A tags entry naming an absolute path is no longer glued onto the tags directory prefix (`Path::join`). Unit test `find_tags_files_walks_subdirectories`.

---

## Detailed conformance matrix

### Options

- [ ] **`-c` PARTIAL.** Flag parsed (line 43); `print_over` field never consulted in render path — clear-on-first-screen and per-line redraw not observable.
- [x] `-e` CONFORMS (line 2148).
- [x] `-i` CONFORMS — `RegexFlags::bre().ignore_case()` at line 1649.
- [x] `-n` CONFORMS — overrides LINES/COLUMNS at line 1538.
- [ ] **`-p` PARTIAL.** Commands run on new-file display (lines 2501–2528). Spec requires that if any `-p` command fails, an informational message is written AND remaining `-p` commands for that file are suppressed — current code may `exit()` on error.
- [x] `-s` CONFORMS — squeeze propagates via `SeekPositions`.
- [x] **`-t`** CONFORMS ✓ fixed — the option shares `goto_tag`, so the literal-match fix of #15 covers it.
- [x] `-u` CONFORMS — `plain` flag suppresses backspace/underscore/bold processing.
- [ ] **`+` as option prefix MISSING.** Spec allows but doesn't require; clap doesn't recognize it. (Lowest priority — optional.)

### Operands / STDIN

- [x] **No file operands → stdin** ✓ fixed (same as Critical #1).
- [x] `-` operand routes to stdin CONFORMS (line 1698 path).
- [x] Mixed `-` and file operands CONFORMS.
- [x] Multiple files CONFORMS.

### Environment variables

- [x] **`COLUMNS`** CONFORMS ✓ fixed (#12) — re-consulted on every resize.
- [x] **`LINES`** CONFORMS ✓ fixed (#12) — re-consulted on every resize, with `-n` taking precedence (107359).
- [x] `MORE` CONFORMS — prepended before CLI args (lines 2983–3003); CLI wins on conflict.
- [x] `EDITOR` CONFORMS — line 1901, falls back to `vi`.
- [ ] **`TERM` MISSING (via termion).** Termion reads internally; we don't validate.
- [ ] **`LANG` MISSING.** Only `LC_ALL` set via `setlocale` at line 3006.
- [ ] **`LC_COLLATE` MISSING.**
- [ ] **`LC_CTYPE` MISSING.**
- [x] **`LC_MESSAGES`** CONFORMS as far as the tree allows ✓ fixed (#14) — all diagnostics are `gettext`'d; text stays English until `.mo` catalogs ship.
- [ ] `NLSPATH` (XSI) MISSING — optional XSI, acceptable; track anyway.

### Asynchronous events

- [x] **SIGCONT** ✓ fixed (same as Major #3).
- [x] **SIGWINCH as signal** ✓ fixed (same as Major #4).
- [x] **`tcgetwinsize`-equivalent on resize** CONFORMS ✓ fixed — termion ioctl called, env-var precedence now honored (same as Minor #12).

### STDOUT / STDERR

- [x] Content to stdout (non-tty) CONFORMS — `print_all_input` lines 1763–1829.
- [ ] **Content rendering (tty mode) DIVERGES** from "write to stdout" model — uses termion alternate screen on stdout. (Design-level; may be acceptable but worth confirming.)
- [x] **Prompt to stderr** ✓ fixed (same as Major #5).
- [x] **Prompt contains filename** ✓ fixed (same as Major #6).
- [x] EOF prompt contains next file CONFORMS — `Prompt::Eof` line 1622.

### Interactive commands

All 28 spec commands are wired (count-prefix supported on every command that takes one). Issues found:

- [ ] **`[count]s` PARTIAL.** Implemented as plain scroll; spec requires "screenful beginning with line `count` lines after last line on current screen".
- [x] **`''`** CONFORMS ✓ fixed — same as Major #9.
- [x] **`:e filename`** CONFORMS for word expansion ✓ fixed (#8); the non-seekable check remains unimplemented (107595: "including that it is a non-seekable file").
- [x] **`:n` / `:p`** CONFORMS ✓ fixed — same as Major #7.
- [x] **`:t tagstring`** CONFORMS ✓ fixed — same as Minor #15.
- [x] **`v` uses `-c N`** ✓ fixed — same as Minor #10.
- [x] **`v` editor name compared by last pathname component** ✓ fixed — same as Minor #11.
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
- [x] **`:e` error affects exit code** when it should not. ✓ fixed (Phase 7) — `handle_error`'s `FileRead` arm reports and continues instead of exiting, so only `:n`/`:p` set `had_error` (107647-107648).
- [x] **`:n` / `:p` error** CONFORMS ✓ fixed (same as Major #7).

---

## Test coverage signal

Tests cover `-c`, `-e`, `-i`, `-n`, `-s`, `-u` and most basic interactive commands.

Not covered (each gap is a "write a test" task tied to fixing the corresponding bug):

- [x] `''` large-movement semantics — implemented by measurement in Phase 7; no automated test (asserting on the alternate-screen byte stream needs a PTY harness)
- [x] `:e` shell expansion — `word_expand` unit tests (Phase 7)
- [x] `v` `-c` flag emission for vi/ex — `test_audit_invoke_editor_arguments` (Phase 7)
- [x] `:n` / `:p` error-path behavior (next file tried, exit code affected) — `test_audit_next_file_error_affects_exit_status` (Phase 7)
- [x] Prompt target (stdout vs stderr) — verified manually via `more file >/tmp/out 2>/tmp/err </dev/null`; tracked as audit checkbox closed by Major #5 fix. (No automated test added; would need a PTY harness.)
- [x] SIGCONT behavior (resume from `kill -STOP $$; kill -CONT $$`) — handler installed; covered by code path, no automated test (needs PTY harness).
- [x] SIGWINCH behavior (size change while paused at prompt) — handler installed; covered by code path, no automated test.
- [x] Implicit-stdin (`echo -e 'a\nb\nc' | more`) — would catch Critical #1 (covered by `test_0_files_multiline_stdin_not_truncated` and `test_0_files_empty_stdin`)
- [ ] `-p` command-failure suppression of remaining `-p` commands

---

## Suggested PR groupings

- **PR A — "POSIX I/O channels"**: Critical #1, #2 + Major #5, #6
- [x] **Phase 8 — "Env precedence, hidden flag, i18n"**: Minor #12, #13, #14 (Major #3, #4 landed earlier)
- [x] **Phase 7 — "Command-mode conformance"**: Major #7, #8, #9 + Minor #10, #11, #15
- **PR D — "Rendering correctness"**: backspace/embolden sequences, `\r` handling, non-printable display, `-c` redraw
- **PR E — "i18n + cleanup"**: Minor #13, #14 + locale env var coverage

---
---

# Part II — `echo`

**Implementation:** `display/echo.rs` (134 lines) + `display/tests/echo/mod.rs` (191 lines, 18 tests)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3 `echo`, pp. 2859–2862
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/echo.md`
**Date:** 2026-07-31
**Method:** full spec read + full implementation read + behavioral verification against the release binary (`target/release/echo`) and, where the spec leaves behavior implementation-defined, against GNU coreutils `echo`. Every "absent"/"wrong-value" claim below was reproduced from a shell, not inferred.

## TL;DR

`echo` is a 134-line utility and gets the *text* of the spec right: arguments joined with single `<space>`, trailing `<newline>`, no options recognized in the Utility Syntax Guideline sense, `--` treated as an ordinary string operand, and the full XSI escape set (`\a \b \c \f \n \r \t \v \\ \0num`). The defects are all in the plumbing around that core. **It aborts with a Rust panic (exit 101) on any non-UTF-8 argument** — `echo $'\xff'` prints a backtrace instead of a byte, which is a hard failure of "write its arguments to standard output" for exactly the data `echo` exists to emit. A second data-loss path is quieter: `echo -n foo > /dev/full` **exits 0** because the final buffered flush is never checked. The octal accumulator overflows a `u8` (`\0777` panics in a debug build, wraps in release). Everything else is Minor: an unknown escape loses its backslash, diagnostics are raw Rust `Debug` text, and the `-n`/escape combination is a BSD/System V hybrid rather than either historical behavior.

## Priority issues

### Critical

- [x] **#E1 — Non-UTF-8 argument panics (exit 101).** `echo.rs:117` uses `std::env::args()`, which is documented to panic when any argument is not valid Unicode. Verified: `echo $'\xff'` → `thread 'main' panicked at library/std/src/env.rs:876: called Result::unwrap() on an Err value: "\xFF"`, exit **101**, zero bytes on stdout. POSIX 93159: "The echo utility writes its arguments to standard output" — arguments are byte strings, not Unicode. ✓ **fixed (Phase 1)**: `main` now collects `std::env::args_os()` as `Vec<Vec<u8>>` via `OsStrExt::as_bytes`, joins with a `b' '` separator, and `translate_str` was replaced by the byte-oriented `translate_bytes`. Regression tests `test_echo_non_utf8_operand`, `test_echo_non_utf8_operand_mixed`.

### Major

- [x] **#E2 — Buffered output is never flushed-and-checked, so write errors are silently discarded.** `echo.rs:131` calls `io::stdout().write_all(...)` and `main` returns without `flush()`. Rust's `LineWriter` only pushes through on a `<newline>`, so the `-n` (and `\c`) paths leave the payload in the buffer, where the runtime's exit-time flush swallows the error. Verified: `echo -n hello > /dev/full` → **exit 0**, no diagnostic (GNU coreutils `echo`: exit 1). The default path happens to work (`echo hello > /dev/full` → exit 1) only because the trailing newline forces the flush inside `write_all`. POSIX 93218-93220: ">0 An error occurred". ✓ **fixed (Phase 1)**: `write_all(...).and_then(|()| stdout.flush())` on a locked handle, diagnosed and `ExitCode::FAILURE` on error. Regression test `test_echo_write_error_exit_status` (both the newline and `-n` paths).
- [x] **#E3 — Octal accumulator overflows `u8`.** `echo.rs:77`, `octal_value = octal_value * 8 + (digit - b'0')` on a `u8`. `\0777` = 511 overflows: **debug build panics** (`attempt to multiply with overflow`, exit 101, reproduced against `target/debug/echo`); release wraps to `0xFF`. POSIX 93185 defines `\0num` as "an 8-bit value that is the zero, one, two, or three-digit octal number", so a three-digit value above `377` is out of range and needs a defined answer, not an overflow. ✓ **fixed (Phase 1)**: accumulated in `u16`, masked with `& 0xff`. Regression tests `test_echo_octal_escape_out_of_range` and the `octal_escape_range` unit test.

### Minor

- [x] **#E4 — Unknown escape drops the `<backslash>`.** `echo.rs:87-94` emits only the character following the backslash: `echo '\q'` → `q`, where GNU `echo -e '\q'` → `\q`. POSIX leaves this implementation-defined (only the listed sequences are specified), but discarding the backslash loses data that no other implementation loses. ✓ **fixed (Phase 1)**: undefined sequences are emitted verbatim, backslash included. `test_echo_unknown_escape` updated to the new expectations plus the `unspecified_escape_is_verbatim` unit test.
- [x] **#E5 — Diagnostics are raw Rust `Debug` output with no utility prefix.** `main` returns `Result<(), Box<dyn Error>>`, so a write failure prints `Error: Os { code: 28, kind: StorageFull, message: "No space left on device" }` (verified). POSIX 93212: standard error is for diagnostic messages; every other audited utility uses an `echo: <message>` form. ✓ **fixed (Phase 1)**: `main` returns `ExitCode` and diagnostics go through `plib::diag::error` with a `gettext`'d message, yielding `echo: write error: <errno text>`.
- [x] **#E6 — `textdomain()` / `bind_textdomain_codeset()` failure aborts before any output.** `echo.rs:114-115` propagate with `?`, so a locale-setup failure makes `echo` print nothing and exit 1. ✓ **fixed (Phase 1)**: replaced by `plib::diag::init_locale("echo")`, which ignores catalog-setup errors by design.
- [ ] **#E7 — `-n` consumed while XSI escapes are processed (BSD/System V hybrid).** `echo.rs:120-127` strips a leading `-n` (BSD behavior) but `translate_str` unconditionally expands escapes (System V / XSI behavior). Under XSI (93170-93172) a first operand of `-` followed by characters from `{'e','E','n'}` "shall be treated as a string to be written", i.e. an XSI `echo -n` prints `-n`. Verified: ours consumes `-n`, and prints `-e`/`-E`/`-ne` literally. POSIX 93167-93169 makes this **implementation-defined** for non-XSI systems (Austin Group Defect 1222), so this is a legitimate documented choice — recorded so the choice is deliberate, not accidental. No action proposed; the existing test at `tests/echo/mod.rs:52-57` already codifies it.
- [x] **#E8 — `LC_CTYPE` not consulted.** Non-escape characters are re-encoded as UTF-8 via `char::encode_utf8` (`echo.rs:97-101`) regardless of locale. Harmless today because `args()` already forced UTF-8 validity, but it becomes live once #E1 is fixed byte-faithfully. ✓ **fixed (Phase 1)** as a consequence of #E1: `translate_bytes` copies operand bytes through unchanged, so no decode/re-encode step exists to be locale-sensitive.

## Detailed conformance matrix

### SYNOPSIS / OPTIONS

- [x] **No options recognized** CONFORMS (93164). There is no clap surface; only the implementation-defined `-n` (see #E7) is special-cased at `echo.rs:121`.
- [x] **`--` is a string operand, not an end-of-options marker** CONFORMS (93162-93163). Verified `echo -- hello` → `-- hello`; test at `tests/echo/mod.rs:166-171`.

### Operands

- [x] **Arguments separated by single `<space>`** CONFORMS — `args.join(" ")`, `echo.rs:129` (93208).
- [x] **No arguments → only `<newline>`** CONFORMS — empty join, newline appended at `echo.rs:105-107` (93159-93160); test `test_echo_no_args`.
- [x] **Non-UTF-8 operand** CONFORMS ✓ fixed (#E1).
- [x] **Escape sequences recognized across the joined string, not per-argument** CONFORMS — `\c` in an earlier operand suppresses later operands (`echo.rs:40-43` breaks the loop); test `test_echo_suppress_newline_c`.

### STDIN / INPUT FILES / OUTPUT FILES

- [x] **STDIN not used** CONFORMS (93188) — grep-verified: no `stdin` reference anywhere in `echo.rs`.
- [x] **No input files, no output files** CONFORMS (93190, 93214).

### Environment variables

- [x] `LANG` / `LC_ALL` CONFORMS in the weak sense — `setlocale(LcAll, "")` at `echo.rs:113` honors the standard precedence chain (93193-93196).
- [x] **`LC_CTYPE`** N/A ✓ (#E8) — operands are copied byte-for-byte, so there is no interpretation for `LC_CTYPE` to govern (93198).
- [x] **`LC_MESSAGES`** CONFORMS as far as the tree allows ✓ (#E5) — the single diagnostic is `gettext`'d and routed through `plib::diag` (93201-93203). Text stays English until `.mo` catalogs ship (see `NLSPATH` below).
- [ ] **`NLSPATH` MISSING** (XSI, 93204) — tree-wide gap: no `.mo` catalogs ship anywhere in the project. Tracked, not actionable per-utility.

### Asynchronous events

- [x] **Default** CONFORMS (93206) — no handlers installed, which is exactly what "Default" requires.

### STDOUT / STDERR

- [x] **Arguments to stdout, single-space separated, trailing `<newline>`** CONFORMS (93208-93210).
- [x] **Escape output transformations** CONFORMS — all XSI sequences present: `\a`(0x07) `\b`(0x08) `\c` `\f`(0x0c) `\n` `\r` `\t` `\v`(0x0b) `\\` `\0num`, `echo.rs:31-95`. Verified `\0303\0251` emits the two raw bytes `303 251` under `LC_ALL=C` (byte-faithful, not re-encoded).
- [x] **stderr used only for diagnostics** CONFORMS ✓ fixed (#E5) — `echo: <message>` via `plib::diag`.

### Exit status / consequences of errors

- [x] **0 on success** CONFORMS.
- [x] **>0 on error** CONFORMS ✓ fixed (#E1, #E2) — every output path is flushed and checked; no panic path remains.
- [x] **Consequences of errors: Default** CONFORMS (93222).

## Test coverage signal

18 tests, covering: no-args, multi-arg joining, `-n`, `\c`, every named escape, `\0` with 0/1/2/3 digits, non-octal termination, unknown escapes, trailing backslash, `--`, empty-string operands. Good breadth for the specified surface; the gaps are all on the defect paths.

Previously not covered, all added in Phase 1 (27 integration tests + 3 unit tests):

- [x] Non-UTF-8 operand byte-passthrough (#E1) — `test_echo_non_utf8_operand`, `test_echo_non_utf8_operand_mixed`, `non_utf8_operand_passes_through`
- [x] Write-failure exit status (`> /dev/full`) (#E2) — `test_echo_write_error_exit_status`
- [x] `\0777` / `\0400` out-of-range octal (#E3) — `test_echo_octal_escape_out_of_range`, `octal_escape_range`
- [x] Diagnostic text/format on failure (#E5) — asserted by `test_echo_write_error_exit_status`

## Suggested PR groupings

- [x] **Phase 1 — "echo: byte-faithful operands and output integrity"**: #E1, #E2, #E3, #E4, #E5, #E6, #E8. Landed as one commit rather than the three originally sketched: `echo.rs` is 134 lines and every finding touches the same two functions, so splitting it would have left the file half-migrated between commits.
- [x] **No action — #E7** (BSD `-n` retained; implementation-defined per Austin Group Defect 1222).

---
---

# Part III — `printf`

**Implementation:** `display/printf.rs` (1216 lines) + `display/tests/printf/mod.rs` (557 lines, 98 tests)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3 `printf`, pp. 3344–3351, plus **XBD Chapter 5 (File Format Notation)**, pp. 113–117, which the utility spec incorporates by reference for the entire conversion-specification grammar
**Reference slices:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/printf.md`, `~/tmp/posix.2024/sliced/xbd-base-definitions/5-file-format-notation/_chapter.md`
**Date:** 2026-07-31
**Method:** full read of both spec files + full implementation read + behavioral verification of every Critical/Major/Minor claim against `target/release/printf` side-by-side with GNU coreutils `printf` 9.x. Line numbers below are POSIX line numbers (`111892`-series for the utility page, `3549`-series for XBD Ch. 5).

## TL;DR

`printf` is the more complete of the two: it is byte-faithful end to end (`args_os` + `as_bytes`, `printf.rs:1191-1195`), the tokenizer handles flags/width/precision/specifier as a proper state machine, `%b` implements the full POSIX escape set including `\c` and precision-bounded output, format reuse matches the spec's own EXAMPLES byte-for-byte, and the optional floating-point conversions are supported. The defects concentrate in three places. First, **`%n$` numbered conversion specifications — added to POSIX by Issue 8 / Austin Group Defect 1592 — are not implemented**, and rather than diagnosing cleanly the tokenizer mis-parses `%2$s` into a `$` conversion and emits garbage. Second, **the unsigned conversions are not unsigned**: `%u`/`%o`/`%x`/`%X` print the *absolute value* of a negative argument (`printf '%u' -1` → `1`, where the spec and every other implementation give `18446744073709551615`), and `%u` of `INT64_MIN` is rejected outright as an invalid number. Third, **an empty precision (`%.f`, `%.s`) is treated as "no precision" instead of zero**, contradicting XBD 3562-3563's "a null digit string is treated as zero". A fourth, quieter defect matches `echo`'s: output is accumulated in one buffer and written once, and the final flush is unchecked, so `printf 'hello' > /dev/full` exits **0**. The remaining items are conversion-detail divergences (`%#.3o` double zero, zero-padded NaN, `NaN` spelling, `\400` arithmetic, `%a` default precision) and a locale gap (`LC_NUMERIC` is never consulted).

## Priority issues

### Major

- [x] **#P1 — `%n$` numbered conversion specifications MISSING, and mis-parsed into garbage.** `printf.rs:205-325` — the tokenizer has no positional-argument state, so `%2$s` is read as flags → width `2` → specifier `$`, falls through to `format_arg`'s catch-all (`printf.rs:1105-1108`), prints a diagnostic *and* the argument as a string. Verified: `printf '%2$s %1$s\n' one two` → stderr `unknown conversion specifier: $` (twice) and stdout `ones twos`, exit 1. POSIX 111973-111981 (item 8) makes this a normative feature of Issue 8, and 111994-111996 defines its interaction with format reuse. ✓ **fixed (Phase 3)**: `ConvSpec` gained `arg_number: Option<usize>`, filled by `parse_arg_number`, which probes a *clone* of the byte iterator and advances the real one only when a digit run is actually followed by `$` — so `%05d` still parses as flags + width. `do_printf` now tracks `base` (operands consumed by previous passes) alongside a per-pass `next_unnumbered` and `highest_numbered`, implementing 111994-111996's rebasing on format reuse; a missing numbered operand is diagnosed and forces a non-zero exit per 111998-112000. Gaps work (the spec's own `"%3$s %1$d\n"` example from 112064-112066 is a test). Mixing numbered and unnumbered argument-consuming conversions is left working independently — POSIX 111980-111981 explicitly makes that case *unspecified*. Regression tests `test_audit_p1_numbered_conversions`, `..._format_reuse`, `..._missing_operand`, plus `test_audit_unnumbered_conversions_unchanged` guarding the flags/width path.
- [x] **#P2 — `%u`, `%o`, `%x`, `%X` print the absolute value of a negative argument.** `printf.rs:918-921` converts via `value.unsigned_abs() as i64` and then formats through the *signed* path. Verified: `%u -1` → `1`, `%x -1` → `1`, `%o -1` → `1` (GNU: `18446744073709551615`, `ffffffffffffffff`, `1777777777777777777777`). XBD 3607-3608: the argument "shall be written as ... unsigned octal (o), unsigned decimal (u), or unsigned hexadecimal notation (x and X)". Compounding it, `unsigned_abs()` of `i64::MIN` round-trips back to a negative `i64`, so `format_integer` would print a leading `-` on an unsigned conversion. ✓ **fixed (Phase 2)**: `format_integer` now takes an explicit `magnitude: u64` plus a caller-decided `sign_char`, so the sign policy lives with the conversion; `format_arg_uint_base` reinterprets a negative operand's two's-complement bits (`as i64 as u64`) and always passes `sign_char: None`, which also makes `+`/`<space>` correctly inert on unsigned conversions per XBD 3569-3570. `format_integer_signed` carries the `d`/`i` policy. Regression test `test_audit_p2_unsigned_conversions_are_unsigned`.
- [x] **#P3 — `%u` of `-9223372036854775808` rejected as an invalid number.** `printf.rs:829-835, 884-891` strips the sign and parses the magnitude `9223372036854775808` as `i64`, which overflows. Verified: `printf '%u' -9223372036854775808` → `printf: -9223372036854775808: invalid number`, prints `0`, exit 1 (GNU: `9223372036854775808`). ✓ **fixed (Phase 2)**: `parse_integer_arg` accumulates into `i128`, which is wide enough for every value either a signed or an unsigned 64-bit conversion can render, and each conversion saturates into its own range afterwards. Regression test `test_audit_p3_unsigned_full_range`.
- [x] **#P4 — Empty precision (`%.f`, `%.s`, `%.e`) treated as absent, not as zero.** `printf.rs:288-302` — `ParseState::PrecisionValue` only assigns `conv_spec.precision` when the digit buffer is non-empty, so a bare `.` leaves it `None` and the default (6 for floats, unbounded for strings) applies. Verified: `%.f 10` → `10.000000` (GNU `10`); `%.s abc` → `abc` (GNU empty). XBD 3562-3563 is normative and unambiguous: "The precision shall take the form of a `<period>` ('.') followed by a decimal digit string; **a null digit string is treated as zero**." ✓ **fixed (Phase 4)**: `conv_spec.precision` is committed to `Some(0)` on the `.` transition and overwritten when digits follow. Regression test `test_audit_p4_empty_precision_is_zero`.
- [x] **#P5 — Final output flush is unchecked, so write errors are silently lost.** `printf.rs:1177` writes the accumulated buffer with `write_all` and `main` returns without `flush()`. Verified: `printf 'hello' > /dev/full` → **exit 0**, no diagnostic (GNU: exit 1); `printf 'hello\n' > /dev/full` → exit 1 only because the newline forces the flush. POSIX 112028-112030. ✓ **fixed (Phase 5)**: `do_printf` holds a locked stdout and ends with a checked `flush()`, so the buffered case reports too. (Same root cause as `echo` #E2, fixed in Phase 1.) Regression test `test_audit_p5_write_error_exit_status`.
- [x] **#P6 — Integer overflow prints `0` instead of the saturated value.** `printf.rs:888-891` returns `Err` on an out-of-range decimal, and the caller substitutes `0` (`printf.rs:944-945`). Verified: `printf '%d' 9999999999999999999999` → diagnostic + `0`, exit 1; GNU prints `9223372036854775807`. POSIX 112020-112024 requires the utility to "write the value accumulated at the time the error was detected to standard output", and the EXAMPLES table at 112117-112120 spells out the expected `2147483647` / `−2147483648` saturation with the note that it is "what would be expected as the return value from the `strtol()` function". ✓ **fixed (Phase 2)**: out-of-range magnitudes saturate (at the `i128` accumulator, then again into the conversion's own range) and emit the spec's own `arithmetic overflow` wording while keeping the non-zero exit. Since only digits of the operand's radix reach the parser, overflow is now the sole failure mode left for the numeric conversion itself. Regression test `test_audit_p6_overflow_saturates`.

### Minor

- [x] **#P7 — Zero value with precision 0 must produce no characters.** XBD 3615-3616. `printf.rs:364-372` only zero-*extends*; it never truncates. Verified: `%.0d 0` → `0`, `%.0o 0` → `0`, `%.0x 0` → `0` (GNU: empty in all three). ✓ **fixed (Phase 4)**: `format_integer` yields an empty digit string for that case, before precision padding. Regression test `test_audit_p7_zero_value_precision_zero`.
- [x] **#P8 — The `0` flag is applied to infinity and NaN.** XBD 3580-3582: leading zeros pad to the field width "**except when converting an infinity or NaN**". `printf.rs:737-742` has no such guard. Verified: `%08f inf` → `00000inf` (GNU `     inf`); `%08f nan` → `00000NaN` (GNU `     nan`). ✓ **fixed (Phase 4)**: `format_float` gates zero padding on `abs_value.is_finite()`. Regression test `test_audit_p8_zero_flag_ignored_for_nonfinite`.
- [x] **#P9 — `%f` of NaN prints `NaN`.** `printf.rs:696` falls through to Rust's `Display`, which spells it `NaN`. XBD 3629-3631 permits only the styles `[-]nan` and `[-]nan(n-char-sequence)` for `f`, with `NAN` reserved for `F`. Verified: `%f nan` → `NaN`, `%F nan` → `NAN` (correct), `%e nan` / `%g nan` → `nan` (correct — those paths have explicit NaN handling at `printf.rs:503-508, 559-564`). ✓ **fixed (Phase 4)**: a shared `format_nonfinite` helper now supplies the `inf`/`nan`/`INF`/`NAN` spellings for every floating-point conversion, and the `f`/`F` arm routes through the new `format_fixed`. Regression test `test_audit_p9_nonfinite_styles`.
- [x] **#P10 — `%#.<n>o` emits a redundant leading zero.** `printf.rs:352-361` prepends a literal `"0"` for alt-form octal *in addition to* the precision zero-fill. XBD 3574-3575 says `#` "shall **increase the precision** to force the first digit of the result to be a zero" — it is not a prefix. Verified: `%#.3o 8` → `0010` (GNU `010`); `%#.1o 8` → `010` (agrees, by luck). ✓ **fixed (Phase 4)**: `#` for base 8 now inserts a leading zero only when the precision-padded digits do not already start with one, and the `"0"` entry is gone from the prefix table (which is now `x`/`X` only). Regression test `test_audit_p10_alt_form_octal`.
- [x] **#P11 — `\400` in the format maps to byte `0x01` via `parsed - 255`.** `printf.rs:113-118`. The comment above `parse_octal_sequence` correctly describes the two implementations in the wild (BusyBox splits it, others mask to 8 bits), then implements a third behavior belonging to neither. Verified: ours → `0x01`, GNU → `0x00`. POSIX 111944-111946 defines `\ddd` only for values that fit a byte, so out-of-range input is unspecified — but subtracting 255 is arbitrary. ✓ **fixed (Phase 5)**: `parse_octal_sequence` accumulates in `u16` and masks with `& 0xff`, so `\400` is `\0` — agreeing with GNU and with `echo`'s `\0400` after #E3. The function is also infallible now (only octal digits reach it), so its `Result` is gone. Regression test `test_audit_p11_octal_escape_range`.
- [x] **#P12 — Quoted character constant with trailing bytes reports no error.** `printf.rs:823-826` returns `fully_consumed = true` after reading one character. Verified: `printf '%d\n' "'-3"` → `45`, **exit 0**; GNU prints `45` plus `warning: 3: character(s) following character constant have been ignored`. POSIX EXAMPLES 112128-112141: "Since the last two arguments contain more characters than used for the conversion, a diagnostic message is generated and **the exit status is non-zero**." ✓ **fixed (Phase 6)**: `fully_consumed` is now `arg_str.chars().count() == 2`, so a trailing byte is diagnosed and forces a non-zero exit. Note this follows the spec *against* GNU, which only warns and still exits 0. Regression test `test_audit_p12_quoted_constant_trailing_bytes`.
- [x] **#P13 — Numeric arguments are `trim()`ed, hiding trailing garbage.** `printf.rs:816` (integers) and `printf.rs:434` (floats). Verified: `printf '%d' '5 '` → `5`, exit 0; GNU → `5` plus `value not completely converted`, exit 1. Leading blanks are fine (`strtol` skips them), trailing ones are not. ✓ **fixed (Phase 6)**: `trim_start()` only, in both the integer and floating-point parsers. A related gap closed at the same time: an operand of *only* blanks was silently accepted as `0` with exit 0; it is not a C integer constant, so it now reports `expected numeric value` (matching GNU). An operand that is empty to begin with still yields zero — that is item 11's missing-operand substitution, not a conversion failure. Regression test `test_audit_p13_operand_whitespace`.
- [x] **#P14 — `%a` uses default precision 6 instead of an exact representation.** `printf.rs:684` supplies `unwrap_or(6)` to `format_hex_float`. XBD 3594-3598: with the precision missing and a binary `FLT_RADIX`, "the precision shall be sufficient for an exact representation of the value", with trailing zeros omitted. Verified: `%a 1` → `0x1.000000p+0` (glibc `0x8p-3`); `%a 3.5` → `0x1.c00000p+1` (glibc `0xep-2`). Both are arithmetically equal to the input and `a`/`A` are optional per 111951 (item 6), so this is cosmetic. ✓ **fixed (Phase 4)**: `format_hex_float` takes `Option<usize>` and emits the trailing-zero-trimmed mantissa when the precision is absent, nothing after the point when it is explicitly zero (XBD 3599). The truncation sub-item is fixed too — an explicit precision now rounds half-to-even on the mantissa, carrying into the digit before the point when needed (`%.0a 3.5` → `0x2p+1`). **A further defect surfaced while testing and was fixed in the same phase: `%A` emitted lowercase mantissa digits**, contrary to XBD 3600-3601 ("the letters \"ABCDEF\" for A conversion"). Regression test `test_audit_p14_hex_float_precision`.
- [x] **#P15 — `LC_NUMERIC` never consulted.** XBD 3623-3624 and 3637-3638 require `LC_NUMERIC` to select the radix character for `e`, `E`, `f`, `g`, `G`, and the utility page lists it at 111924-111926. All float paths use Rust's `format!`, which is locale-independent and always emits `.`. Code-verified (no `localeconv`/`LC_NUMERIC` reference in the file); not behaviorally reproducible on this host — no non-`C` numeric locale is installed (`locale -a` has no `de_DE`). ✓ **fixed (Phase 6)**: new `plib::locale::radix_char()` reads `localeconv()->decimal_point` (`localeconv` is exposed by the `libc` crate on both Linux and macOS, unlike `nl_langinfo`'s `RADIXCHAR` constant), cached behind a `OnceLock` in `printf.rs` and substituted into the formatted result. Behaviorally verified against a locally generated `de_DE.UTF-8` (`localedef` + `LOCPATH`): `LC_ALL=de_DE.UTF-8 printf '%.2f %e %g' 3.5 3.5 0.0001` → `3,50 3,500000e+00 0,0001`, matching GNU; `LC_ALL=C` still gives `3.50`.
- [x] **#P16 — Diagnostics are hardcoded English.** Eight of the nine `eprintln!`/`eprint!` sites (`printf.rs:785, 792, 904, 911, 936, 943, 1106, 1206`) emit untranslated text; only `printf.rs:1212` ("not enough arguments") is wrapped in `gettext`. POSIX 111921-111923 makes `LC_MESSAGES` govern diagnostic text. ✓ **fixed (Phase 6)**: every diagnostic now routes through `plib::diag::error` with a `gettext`'d message, giving a uniform newline-terminated `printf: <message>`. The wording follows the spec's own EXAMPLES table (112117-112122): `not completely converted`, `arithmetic overflow`, `expected numeric value`. Two message bugs were fixed in passing — the old `invalid number: {arg}` text produced `printf: ABC: invalid number: ABC` (operand printed twice), and the range-saturation paths added in Phase 2 set the exit status but printed **no** diagnostic at all (`printf '%d' 9999999999999999999999` exited 1 silently); a shared `report_overflow` now covers every saturation site. Tree-wide caveat: no `.mo` catalogs ship, so `LC_MESSAGES` is inert project-wide regardless. Regression test `test_audit_p16_diagnostics_are_prefixed`.
- [x] **#P17 — Two panic sites and a truncated diagnostic in `main`.** `printf.rs:1186-1189` `.unwrap()`s `textdomain` and `bind_textdomain_codeset` (both carry `// TODO unwrap` comments); `printf.rs:922-924` is an explicit `panic!("printf: BUG: invalid conversion specifier")` — currently unreachable, since `format_arg` dispatches only `u`/`o`/`x`/`X` into that function, but it is a live abort path if the dispatch table ever changes. Separately `printf.rs:1206` uses `eprint!` without a newline, so tokenizer errors print unterminated: verified `printf '%99999999999999999999d' 1` → `printf: number too large to fit in target type` with no trailing newline, exit 1. Fix: `.ok()` on the locale calls, return an error instead of panicking, `eprintln!`. ✓ **fixed (Phases 2 and 6)**: the `panic!` went in Phase 2 — the base/uppercase decision moved to `format_arg`'s dispatch, so `format_arg_uint_base` takes them as parameters and has no unreachable arm. Phase 6 replaced the two `.unwrap()`s with `plib::diag::init_locale("printf")`, which ignores catalog-setup errors by design, and the `eprint!` with `diag::error`, so tokenizer failures are newline-terminated. Covered by the tail of `test_audit_p16_diagnostics_are_prefixed`.
- [x] **#P18 — Whole output buffered in memory before the single write.** `printf.rs:1122, 1150, 1177` accumulate every byte into one `Vec` across all format-reuse passes. Correct, but unbounded: a large operand list materializes the entire output before a byte is emitted, and nothing is written if an early tokenizer error aborts. ✓ **fixed (Phase 5)**: tokens are written straight to the locked stdout handle as they are produced, so nothing larger than one conversion's output is held. Needed anyway for #P5's checked flush.

## Detailed conformance matrix

### SYNOPSIS / OPTIONS / OPERANDS

- [x] **No options** CONFORMS (111900) — no clap surface; `printf.rs:1193-1194` takes `argv[1]` as the format verbatim, so `printf -- '%s\n' a` correctly treats `--` as the *format*.
- [x] **`format` operand required** CONFORMS — missing → `printf: not enough arguments`, exit 1 (`printf.rs:1211-1214`); verified.
- [x] **Empty format** CONFORMS — no output, exit 0; verified.
- [x] **Byte-faithful operands** CONFORMS — `args_os()` + `as_bytes()` (`printf.rs:1191-1195`). Verified: `printf '%s\n' $'\xff\xfe'` emits `377 376 \n`. (Numeric conversions still require UTF-8 and diagnose otherwise, `printf.rs:811-813` — correct, since a numeric operand must be a character string.)

### STDIN / INPUT FILES / OUTPUT FILES

- [x] **STDIN not used** CONFORMS (111908) — grep-verified: no `stdin` reference in `printf.rs`.
- [x] **No input/output files** CONFORMS (111910, 111935).

### Environment variables

- [x] `LANG` / `LC_ALL` CONFORMS in the weak sense — `setlocale(LcAll, "")` at `printf.rs:1183`.
- [x] `LC_CTYPE` N/A in practice — every conversion is byte-oriented, which is what 112041-112044 prescribes for `%c` and for precision on `%b`/`%s`.
- [x] **`LC_MESSAGES`** CONFORMS as far as the tree allows ✓ fixed — every diagnostic is `gettext`'d and routed through `plib::diag`; see #P16.
- [x] **`LC_NUMERIC`** CONFORMS ✓ fixed — see #P15.
- [ ] **`NLSPATH` MISSING** (XSI, 111927) — tree-wide gap.

### Asynchronous events

- [x] **Default** CONFORMS (111929) — no handlers installed.

### Extended description — format string

- [x] **`<space>` outside a flag position is an ordinary character** CONFORMS (item 1, 111940-111941).
- [x] **`\ddd` octal in the format** CONFORMS (item 3, 111944-111946) — `printf.rs:179-183`; out-of-range values now mask to 8 bits ✓ (#P11).
- [x] **No stray blanks around `d`/`u` output, no stray zeros before `o`** CONFORMS (items 4-5, 111947-111950) — `format_integer` emits nothing it was not asked for.
- [x] **`a A e E f F g G` supported** CONFORMS as an encouraged extension (item 6, 111951 + 112039). Verified `%e`/`%f`/`%g` match GNU byte-for-byte across `1.005`, `1e10`, `0.1`, `1234.5678`, `0.0001`, `0.00001`, `100000`, `1000000`, `123456789`; exponent is always ≥2 digits and grows past two when needed (`1e100` → `1.000000e+100`), per XBD 3641-3643. Divergences confined to #P8, #P9, #P14.
- [x] **`%b` conversion** CONFORMS (item 7, 111952-111971) — `printf.rs:968-1029`: full escape set, `\0ddd` with up to three digits after the zero, `\c` stopping the remaining string, remaining operands, and the rest of the format (verified `printf '%b\n' a 'b\c' c` → `a\nb`, matching GNU), precision applied to the *converted* string (verified `%.2b abcdef` → `ab`).
- [x] **Numbered conversion specifications** CONFORMS ✓ fixed (items 8/10, 111972-111996) — see #P1.
- [x] **Argument consumption order** CONFORMS (item 9, 111982-111992) — `printf.rs:1141-1147`; `%%` correctly consumes nothing.
- [x] **Format reuse** CONFORMS (item 10, 111993) — `printf.rs:1128-1175`; rebased for numbered conversions in Phase 3 (111994-111996). The spec's own example `printf "%5d%4d\n" 1 21 321 4321 54321` reproduces byte-for-byte, including the synthesized `0` for the unmatched final `%4d`. A format with no argument-consuming conversion correctly stops after one pass (`printf.rs:1129-1131, 1172`), so `printf 'abc\n' x y` prints once and cannot loop forever.
- [x] **Missing operands** CONFORMS (item 11, 111997-112004) — `printf.rs:1144` substitutes an empty operand; `b`/`c`/`s` yield the null string, numeric conversions yield zero (`printf.rs:807-809`). A missing *numbered* operand additionally diagnoses and exits non-zero ✓ (Phase 3).
- [x] **Invalid conversion specification** — unspecified per item 12 (112005-112006). Ours diagnoses and then prints the operand as a string (`printf.rs:1105-1108`); GNU errors out. Acceptable; noted because the output is noisy (`printf '%z' abc` → `abc` on stdout).
- [x] **`%c` semantics** CONFORMS (item 13, 112007-112010 + XBD 3654) — first byte only, nothing written for an empty operand. Verified `%c abc` → `a`, `%c 'é'` → the single byte `0303`, `%c ''` → empty. The multi-byte truncation is exactly what 112041-112044 warns applications about.
- [x] **Integer operand forms** CONFORMS (112013-112018) — leading `+`/`-`, `0x`/`0X` hex, `0` octal, and `'c`/`"c` character constants all parse (`printf.rs:806-896`). Verified against the spec EXAMPLES: `printf '%d\n' 10 010 0x10` → `10 8 16`. Guarded after the Phase 2 parser rewrite by `test_audit_integer_operand_forms_unchanged`.
- [x] **Partial conversion is diagnosed, non-zero exit, remaining operands still processed, accumulated value still written** CONFORMS (112020-112024) — verified `printf 'a%db\n' ABC` → stdout `a0b`, diagnostic on stderr, exit 1 (GNU identical). ✓ the overflow *value* now saturates as the spec requires (#P6, Phase 2), and the two under-reporting paths (#P12, #P13) were closed in Phase 6.
- [x] **Incomplete use of a `b`/`c`/`s` operand is not an error** CONFORMS (112025-112026).

### Extended description — XBD Chapter 5 conversion details

- [x] **Flags `-` `+` `<space>` `#` `0`** CONFORMS (3568-3585) — verified `%-5s`, `%+d`, `% d`, `%#x`, `%05d`, `%-6d` all match GNU.
- [x] **`0` ignored when a precision is given for `d i o u x X`** CONFORMS (3583-3585) — `printf.rs:394` guards on `conv.precision.is_none()`; verified `%08.3d 42` → `     042`, matching GNU.
- [x] **Field width never truncates** CONFORMS (3662-3664) — `format_arg_string`/`format_integer` only pad.
- [x] **Default precision 1 for integers; precision zero-extends** CONFORMS (3612-3615) — verified `%.5d 42` → `00042`.
- [x] **Zero value with precision 0** CONFORMS ✓ fixed (3615-3616) — see #P7.
- [x] **`%s` precision bounds bytes written** CONFORMS (3655-3659) — verified `%5.2s`, `%.0s`.
- [x] **`%f` precision 0 suppresses the radix character** CONFORMS (3625-3626) — verified `%.0f 2.5` → `2`, `%.0f 3.5` → `4` (round-half-even, matching glibc).
- [x] **`%g` trailing-zero removal and `#` suppression thereof** CONFORMS (3646-3651) — verified `%g 100` → `100`, `%#g 100` → `100.000`, `%.0g 100` → `1e+02`.
- [x] **`#` with `o`** CONFORMS ✓ fixed (3574-3575) — see #P10.
- [x] **`0` with infinity/NaN** CONFORMS ✓ fixed (3580-3582) — see #P8.
- [x] **NaN style for `f`** CONFORMS ✓ fixed (3629-3631) — see #P9.
- [x] **`a`/`A` default precision** CONFORMS ✓ fixed (3594-3598) — see #P14.
- [x] **`%%`** CONFORMS (3660-3661).

### Exit status / consequences of errors

- [x] **0 on success, >0 on conversion error** CONFORMS (112028-112030) — `had_error` threaded through every conversion and returned from `do_printf` (`printf.rs:1119-1179, 1198-1204`).
- [x] **>0 on write error** CONFORMS ✓ fixed — see #P5.
- [x] **>0 on a partly-consumed quoted character constant** CONFORMS ✓ fixed — see #P12.
- [x] **Consequences of errors: Default** CONFORMS (112032).

## Test coverage signal

98 tests — the broadest of the three utilities in this crate. Covered: all integer/string/char conversions, `%b` with every escape, `%%`, character constants, hex/octal operand parsing, format reuse (including the spec's `%5d%4d` example), width/precision/left-justify/zero-pad combinations, all format-string escapes, missing operands, `\c` in both the format and a `%b` operand.

Not covered (each is a "write a test" task tied to the matching finding):

- [x] `%n$` numbered conversions (#P1) — added in Phase 3
- [x] Negative operands to `%u`/`%o`/`%x`/`%X` (#P2) and `i64::MIN` (#P3) — added in Phase 2
- [x] Empty precision `%.f` / `%.s` (#P4) — added in Phase 4
- [x] Write-failure exit status (#P5) — added in Phase 5
- [x] Integer overflow saturation (#P6) — added in Phase 2
- [x] `%.0d 0` (#P7), `%08f inf` (#P8), `%f nan` (#P9), `%#.3o` (#P10) — added in Phase 4
- [x] `\400` (#P11) — added in Phase 5
- [x] quoted constant with trailing bytes (#P12), trailing whitespace in a numeric operand (#P13) — added in Phase 6
- [x] Any floating-point test at all — `%f`/`%e`/`%g`/`%a` had **zero** tests despite ~280 lines of implementation (`printf.rs:424-758`); `test_audit_floating_point_conversions` plus the four conversion-specific tests added in Phase 4
- [x] `LC_NUMERIC` radix character (#P15) — verified in Phase 6 against a generated `de_DE.UTF-8`

## Suggested PR groupings

- [x] **Phase 2 — "printf: unsigned conversions"**: #P2, #P3, #P6 (+ #P17's `panic!`) — one theme (integer operand → value → unsigned formatting), one test module.
- [x] **Phase 3 — "printf: numbered conversion specifications"**: #P1 alone; touches the tokenizer and the reuse loop, deserves its own review.
- [x] **Phase 4 — "printf: precision and flag conformance"**: #P4, #P7, #P8, #P9, #P10, #P14 (+ the `%A` case defect) — all XBD Ch. 5 detail fixes, plus the crate's first floating-point tests.
- [x] **Phase 5 — "printf: output integrity"**: #P5 (checked flush) + #P11 (octal masking) + #P18 (incremental writing).
- [x] **Phase 6 — "printf: diagnostics and error reporting"**: #P12, #P13, #P15, #P16, #P17.

---

# Crate-level summary — `display/`

| Utility | Critical | Major | Minor | README stage |
|---|---|---|---|---|
| `more` | 2 (both fixed) | 7 (4 fixed) | 6 | Stage 6 — Audited |
| `echo` | 1 | 2 | 5 | Stage 3 (not promoted) |
| `printf` | 0 | 6 | 12 | Stage 3 (not promoted) |

**Cross-cutting themes across the crate:**

1. **Unchecked final flush.** `echo` #E2 and `printf` #P5 are the same bug: Rust's line-buffered stdout hides the write error whenever the output does not end in a `<newline>`, and neither `main` calls `flush()`. Both silently exit 0 after losing data. Fix them together.
2. **Byte fidelity is inconsistent.** `printf` is byte-faithful from `args_os` through to output; `echo` panics on the same input. `echo` should adopt `printf`'s pattern verbatim.
3. **Diagnostics.** `more`'s `MoreError` strings (#14), all of `echo`'s (there are none, only Rust `Debug` output — #E5), and seven of `printf`'s eight (#P16) are hardcoded English. None of the three uses `plib::diag`. A single crate-wide migration closes #14, #E5, #E6, #P16, #P17.
4. **`NLSPATH` / `.mo` catalogs** are absent tree-wide, so `LC_MESSAGES` is inert regardless of the above. Tracked at project level, not per-utility.

**Promotion status.** `echo` and `printf` are **deliberately not promoted** to README Stage 6 by this audit: `echo` still aborts (exit 101) on a non-UTF-8 operand (#E1) and both silently discard write errors on their most common non-newline paths (#E2/#P5). This follows the precedent set for `text/` `csplit`/`tr` and `users/` `talk` — audited, punch-list published, promotion deferred until the Critical/Major items are remediated with regression tests.
