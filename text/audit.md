# POSIX.1-2024 Conformance Audits — `text/` utilities

This file collects per-utility POSIX conformance audits for the text-processing
utilities crate (`text/`, 22 utilities). Each audit follows the playbook in
`audits.md` and mirrors the format established by `dev/audit.md`.

**Audit date:** 2026-06-25
**Method:** one read-only subagent per utility, each cross-referencing the
per-utility POSIX.1-2024 spec slice against the implementation and tests, with
Critical/Major findings verified against cited line ranges before publication.
**Scope:** audit only — no code was modified.

Utilities (by source size): `asa`, `comm`, `csplit`, `cut`, `diff` (+`diff_util/`),
`expand`, `fold`, `grep`, `head`, `join`, `nl`, `paste`, `patch` (+`patch_util/`),
`pr` (+`pr_util/`), `sed`, `sort`, `tail`, `tr`, `tsort`, `unexpand`, `uniq`, `wc`.

---

## `asa`

**Implementation:** `text/asa.rs` (152 lines)
**Tests:** `text/tests/asa/mod.rs` (330 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/asa.md`
**Date:** 2026-06-25

### TL;DR

The `asa` implementation is almost fully conformant. The single meaningful gap is an undocumented `-` (hyphen) triple-spacing extension that, while not prohibited by the spec (the spec leaves unknown control characters "unspecified"), diverges from the POSIX rationale's recommendation to treat unknown characters as `<space>`. Diagnostics use bare `eprintln!` instead of `plib::diag`, a cross-cutting style inconsistency relative to other audited utilities. All core carriage-control semantics, the `+`-first-line rule, multi-file handling, `stdin`/`-` operand, and exit codes are correct.

### Priority issues

#### Critical

- none.

#### Major

- none.

#### Minor

- [ ] **#1 — Undocumented `-` extension diverges from POSIX rationale recommendation.** `text/asa.rs:92-100`. The spec leaves the action for unrecognized first-characters "unspecified" and the rationale explicitly recommends treating them as `<space>`. The implementation instead provides a triple-spacing `-` extension that silently produces two blank lines for any input line whose control character is a hyphen. Portability trap: FORTRAN programs that accidentally write `-` as the first character get triple-spacing rather than single-spacing. Fix: remove the `-` arm and let it fall through to the `_` (space-equivalent) branch, or document it explicitly as a local extension.
- [ ] **#2 — Diagnostics bypass `plib::diag`.** `text/asa.rs:146`. Error messages are emitted via bare `eprintln!("{}: {}", filename.display(), e)` rather than through `plib::diag`. Every other audited utility touched during the datetime-audit cycle routes runtime diagnostics through `plib::diag::error()` / `plib::diag::init_locale()`. Fix: call `plib::diag::init_locale("asa")` in `main` and replace the `eprintln!` with `plib::diag::error(...)`.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Requirement | Status | Notes |
|---|---|---|
| `asa [file...]` — no options | CONFORMS | clap struct has no option fields; only `files: Vec<PathBuf>` |
| `--` end-of-options delimiter | CONFORMS | clap handles Guideline 10 by default |
| Unknown options rejected | CONFORMS | clap errors and exits non-zero |
| Option set (spec OPTIONS: None) | CONFORMS | No option flags accepted |

#### OPERANDS / STDIN / INPUT FILES

| Requirement | Status | Notes |
|---|---|---|
| `file` operand: text file pathname | CONFORMS | `input_reader(pathname, true)` opens each file |
| No files → read stdin | CONFORMS | `args.files.push(PathBuf::from("-"))` at line 138-139 |
| `-` operand → read stdin | CONFORMS | `input_reader` with `dashed_stdin=true` treats `-` as stdin |
| Input files shall be text files | CONFORMS | No binary enforcement needed; POSIX leaves handling implementation-defined |
| Multiple files processed sequentially | CONFORMS | Loop over `args.files` at line 143 |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG` | CONFORMS | `setlocale(LcAll, "")` at line 130 |
| `LC_ALL` | CONFORMS | Same `setlocale` call |
| `LC_CTYPE` | CONFORMS | Inherited through `setlocale(LcAll, "")` |
| `LC_MESSAGES` | CONFORMS | `textdomain` + `bind_textdomain_codeset` at lines 131-132 |
| `NLSPATH` (XSI) | CONFORMS | Handled by the gettextrs/libc layer |

#### ASYNCHRONOUS EVENTS

- [x] Default signal handling — CONFORMS. No signal handlers installed; Rust defaults apply.

#### STDOUT / STDERR

| Requirement | Status | Notes |
|---|---|---|
| STDOUT: modified text from input | CONFORMS | All content output via `print!`/`println!` to stdout |
| STDERR: None on success | CONFORMS | No spurious stderr on success |
| STDERR: diagnostic on error | PARTIAL | `eprintln!` at line 146 works but bypasses `plib::diag` (#2) |

#### EXTENDED DESCRIPTION

The implementation's lazy-newline pattern — emit the previous line's terminator as part of processing the *current* line — is architecturally clean and produces the correct output sequence.

| Control char | Spec requirement | Implementation | Status |
|---|---|---|---|
| `<space>` | Output rest of line | `_` arm, line 108 | CONFORMS |
| `0` | Output `<newline>` then rest of line (double-space) | lines 84-91 | CONFORMS |
| `1` | Advance to next page | lines 101-107: emits `\x0c` (form-feed) | CONFORMS |
| `+` | Replace previous `<newline>` to return to col 1; first char → `<space>` | lines 76-83: `\r`, first-line special-case line 77 | CONFORMS |
| Unknown chars | Action unspecified | `_` arm treats as `<space>` — matches rationale | CONFORMS |
| `-` (hyphen) | Unspecified by POSIX | lines 92-100: triple-spacing extension | DIVERGES (#1) |

Last-line/no-final-newline (lines 122-124) and multi-byte first character (`.chars().next()`, `len_utf8()` skip) both handled correctly. `first_line` is per-file (reset for each file), so `+` as the first control character of a second file is correctly treated as `<space>`.

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Requirement | Status | Notes |
|---|---|---|
| Exit 0: all files output | CONFORMS | `exit_code` stays 0 (line 141) |
| Exit >0: an error occurred | CONFORMS | `exit_code = 1` on `asa_file` error (line 145) |
| Continue after one file error | CONFORMS | `if let Err(e)` at line 144 continues the loop |

#### Cross-cutting (i18n, locale)

Locale setup at lines 130-132 satisfies POSIX. Error message text in `eprintln!` at line 146 is not wrapped in `gettext()` (minor; low-impact OS-provided text).

### Test coverage signal

Not covered:
- [ ] Non-existent file argument → exit 1 + diagnostic to stderr
- [ ] Mix of valid and invalid file arguments (continue, exit 1)
- [ ] `-` operand interleaved with real files (`asa file1 - file2`)
- [ ] `--` end-of-options followed by a filename beginning with `-`
- [ ] Single-byte input with no newline

### Suggested PR groupings

- **PR A — "Remove non-POSIX `-` triple-spacing extension"**: #1.
- **PR B — "Route diagnostics through plib::diag"**: #2.

---

## `comm`

**Implementation:** `text/comm.rs` (161 lines)
**Tests:** `text/tests/comm/mod.rs` (201 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/comm.md`
**Date:** 2026-06-25

### TL;DR

Mostly correct on the happy path (options, column suppression, tab-lead arithmetic, stdin via `-`, exit codes), but has one Major defect: line comparison uses Rust's native `<` operator on `String` (UTF-8 byte order), ignoring `LC_COLLATE`. `plib::locale::strcoll` exists in the workspace and is unused here. Two Minor issues: `--` end-of-options cannot reach operands starting with `-` (no `allow_hyphen_values`), and diagnostics use bare `eprintln!` (not `LC_MESSAGES`-aware).

### Priority issues

#### Critical

- none.

#### Major

- [ ] **#1 — Line comparison ignores LC_COLLATE — uses Rust byte order instead of `strcoll(3)`.** `text/comm.rs:106-109`. The comparisons `buf1 < buf2` / `buf2 < buf1` use Rust's `<` on `String` (UTF-8 byte order = the C/POSIX locale, but diverges from any non-POSIX locale). Spec requires comparison in "the collating sequence of the current locale". The workspace already provides `plib::locale::strcoll` (`plib/src/locale.rs:122`) but `comm` neither imports nor calls it. Under a non-C locale, `comm` and `sort` disagree on ordering → garbage output. Fix: replace the two comparisons with `plib::locale::strcoll(&buf1, &buf2)`.

#### Minor

- [ ] **#2 — `--` end-of-options with operands starting with `-`.** `text/comm.rs:23-38`. The `file1`/`file2` positional `PathBuf` fields lack `allow_hyphen_values`. Bare `-` (stdin) works, but a literal filename starting with `-` is misparsed as an unknown option. Fix: add `#[arg(allow_hyphen_values = true)]` or struct-level `allow_hyphen_values`.
- [ ] **#3 — No locale-aware diagnostic messages (LC_MESSAGES).** `text/comm.rs:157`. `eprintln!("{}", e)` emits the raw Rust I/O error string (always English) regardless of `LC_MESSAGES`. `textdomain` is initialized but the error path does not use a gettextrs-wrapped message. Fix: route through `plib::diag`/`gettext`.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Requirement | Status | Notes |
|---|---|---|
| `comm [-123] file1 file2` | CONFORMS | clap parses flags + 2 positional PathBufs |
| `-123` combined flags | CONFORMS | clap short-flag grouping |
| `-1 -2 -3` separate flags | CONFORMS | tested |
| `--` end-of-options | PARTIAL | honored, but operands lack `allow_hyphen_values` (#2) |
| Unknown option → error | CONFORMS | clap rejects |

#### OPTIONS

| Option | Status | Notes |
|---|---|---|
| `-1` suppress col 1 | CONFORMS | `comm.rs:41-43`, `NO1` mask |
| `-2` suppress col 2 | CONFORMS | `NO2` mask |
| `-3` suppress col 3 | CONFORMS | `NODUP` mask |
| `-123` write nothing | CONFORMS | all bits set, every `line_out` returns early |

#### OPERANDS / STDIN / INPUT FILES

| Requirement | Status | Notes |
|---|---|---|
| `file1`/`file2` required | CONFORMS | clap enforces exactly 2 |
| `file1 = '-'` reads stdin | CONFORMS | `input_reader` dashed_stdin; tested |
| `file2 = '-'` reads stdin | CONFORMS | tested |
| Both `-` → undefined | N/A | spec says undefined; no detection required |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG` | CONFORMS | `setlocale(LcAll, "")` line 138 |
| `LC_ALL` | CONFORMS | covered |
| `LC_COLLATE` | MISSING | comparison uses Rust `<`, not `strcoll` (#1) |
| `LC_CTYPE` | PARTIAL | activated; lines passed through intact |
| `LC_MESSAGES` | PARTIAL | `textdomain` set but error path bare `eprintln!` (#3) |
| `NLSPATH` (XSI) | PARTIAL | infra present; error path bare |

#### ASYNCHRONOUS EVENTS

- [x] Default — CONFORMS. No handlers; Rust default disposition.

#### STDOUT / STDERR

| Requirement | Status | Notes |
|---|---|---|
| Col1 `"%s\n"` | CONFORMS | line 47 |
| Col2 `"\t%s\n"` (or `"%s\n"` with -1) | CONFORMS | lines 54-58 |
| Both: `"\t\t%s\n"` / `"\t%s\n"` / `"%s\n"` per suppression | CONFORMS | lines 147-150 |
| All options → nothing | CONFORMS | early returns |
| Diagnostics to stderr | CONFORMS | line 157 |

#### EXTENDED DESCRIPTION

Spec: "None". The only normative subtlety is the Issue 8 tiebreak (collate-equal but non-identical lines → byte-by-byte POSIX-locale tiebreak); because the impl always uses byte order, the C-locale tiebreak is accidentally correct but the non-C primary comparison is wrong (#1).

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Status | Spec | Notes |
|---|---|---|
| Exit 0 | CONFORMS | success |
| Exit >0 | CONFORMS | `exit_code = 1` line 156 |
| Stderr on error | CONFORMS | line 157 |

#### Cross-cutting (i18n, collation, locale)

Core gap is collation (#1). Tests pass because they run under `LC_COLLATE=C`, where byte order == collation order.

### Test coverage signal

Not covered:
- [ ] LC_COLLATE non-C locale collation comparison
- [ ] Issue 8 tiebreak (equal-collating, non-identical lines)
- [ ] Unsorted input (document actual behavior)
- [ ] `--` delimiter allowing operands starting with `-`
- [ ] LC_MESSAGES affecting diagnostic text

### Suggested PR groupings

- **PR A — "Fix LC_COLLATE collation"**: #1.
- **PR B — "argv + diagnostics hygiene"**: #2, #3.

---

## `csplit`

**Implementation:** `text/csplit.rs` (1201 lines incl. inline tests)
**Tests:** `text/tests/csplit/mod.rs` (323 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/csplit.md`
**Date:** 2026-06-25

### TL;DR

Largely functional and handles the core splitting modes. Three correctness defects stand out: (1) the line counter resets to 1 after every `line_no` split, breaking absolute line numbers when multiple `line_no` operands are given without a `{num}` repeat; (2) a standalone `line_no` operand without a following `{num}` is never removed from the operand list, so it fires repeatedly every N lines instead of once; (3) the stdout byte-count may reflect a mutilated string length after the trailing newline is stripped before writing. Plus minor gaps in `\/`/`\%` escape translation, the signal-cleanup requirement, and a hardcoded `NAME_MAX`.

### Priority issues

#### Critical

- [ ] **#1 — Absolute line numbers broken with multiple `line_no` operands and no `{num}`.** `text/csplit.rs:209,214`. After a `line_no` split fires, `state.in_line_no` is reset to 1 (line 214). A subsequent `line_no` operand counts from the start of the new section, not the start of the file. `csplit file 5 10` splits at file-line 5 then at file-line 15 (perceived "line 10" of section 2), not file-line 10. Fix: maintain a monotonically increasing global file line counter; never reset it.
- [ ] **#2 — `line_no` operand without a following `{num}` never consumed.** `text/csplit.rs:218-225`. After a `line_no` fires, the operand list is only advanced if the next item is a `Repeat`. With no `Repeat`, the `LineNum` operand stays and (combined with #1's reset) fires every N lines indefinitely. `csplit file 5` should split once at line 5; instead it splits at 5, 10, 15, … to EOF. Fix: consume a non-repeated `LineNum` after it fires.
- [ ] **#3 — stdout byte count diverges from actual file size.** `text/csplit.rs:388`. Spec stdout format is `"%d\n", <file size in bytes>`. The impl prints `lines.len()`, but callers strip the trailing newline with `lines.pop()` before writing (e.g. lines 211, 255, 283, 319), so the printed count omits the newline and mismatches `wc -c` of the written file. Fix: report the size of what was actually written.

#### Major

- [ ] **#4 — ASYNCHRONOUS EVENTS: no signal handler for created-file cleanup.** `text/csplit.rs:626-635`. Cleanup runs only in the `Err` branch. If killed by a signal mid-run, created files are neither removed (default) nor retained-per-`-k`. Fix: register SIGINT/SIGTERM handlers mirroring the cleanup at lines 631-634.

#### Minor

- [ ] **#5 — `{*}` decrements `usize::MAX`, panics in debug builds.** `text/csplit.rs:334` (also 220). `Repeat(usize::MAX)` minus 1 panics on overflow in debug. Fix: special-case the unbounded repeat.
- [ ] **#6 — Escaped delimiter not translated into the BRE.** `text/csplit.rs:407-449`. The `\/` (and `\%`) escape is used to find the operand's end but the resulting pattern slice is passed to the BRE engine without translating `\/`→`/`, so `/proc\/sys/` compiles the literal `proc\/sys`. Fix: translate the escaped delimiter before compiling.
- [ ] **#7 — Hardcoded `NAME_MAX = 255`.** `text/csplit.rs:595-596`. Not queried via `pathconf`/`libc::NAME_MAX`. Portability minor.
- [ ] **#8 — No error when an operand references a line past EOF.** `text/csplit.rs:541`. Spec requires an error if an operand does not reference a line between the current position and EOF; remaining operands are instead silently ignored.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Item | Status | Notes |
|---|---|---|
| `csplit [-ks] [-f prefix] [-n number] file arg...` | CONFORMS | clap; `--` inherited |
| `-` for stdin | CONFORMS | line 184 |
| ≥1 `arg` required | PARTIAL | clap does not enforce min 1; zero args writes one file with entire input |

#### OPTIONS

| Option | Status | Notes |
|---|---|---|
| `-f prefix` | CONFORMS | default `"xx"`, line 22 |
| `-k` keep on error | PARTIAL | works for I/O errors; signals unhandled (#4) |
| `-n number` | CONFORMS | default 2 |
| `-s` suppress byte counts | CONFORMS | lines 33-35, 387-389 |
| prefix+digits > NAME_MAX → error | PARTIAL | `validate_prefix` fires; hardcoded constant (#7) |

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| `file` or `-` | CONFORMS | line 183-188 |
| `/rexp/[offset]` create | PARTIAL | works for offsets; escape not translated (#6) |
| `%rexp%[offset]` skip | PARTIAL | same escape gap (#6) |
| `line_no` absolute | DIVERGES | counter reset (#1); operand not consumed (#2) |
| `{num}` repeat | PARTIAL | correct for regex; broken for line_no (#1) |
| `{*}` unbounded | PARTIAL | debug panic (#5) |
| error if operand out of range | MISSING | silent ignore (#8) |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL` | CONFORMS | `setlocale(LcAll, "")` line 615 |
| `LC_COLLATE`/`LC_CTYPE` | CONFORMS | BRE via libc `regcomp` honors locale |
| `LC_MESSAGES` | PARTIAL | `textdomain` wired; many strings still English |
| `NLSPATH` | N/A | XSI; `bind_textdomain_codeset` present |

#### ASYNCHRONOUS EVENTS

- [ ] **`-k`/default signal cleanup MISSING** — no signal handler (#4).

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| `"%d\n"` file byte count | DIVERGES | `lines.len()` after newline strip (#3) |
| suppressed by `-s` | CONFORMS | line 387 |
| diagnostics to stderr | CONFORMS | line 630 |

#### EXTENDED DESCRIPTION

| Item | Status | Notes |
|---|---|---|
| BRE flavor | CONFORMS | `Regex::bre()` (no REG_EXTENDED) |
| `\/` / `\%` escape stripping | PARTIAL | not stripped before BRE compile (#6) |
| pos/neg/zero offset | CONFORMS | lines 237-326 |
| split up to but not incl. matched line | CONFORMS | |
| `%rexp%` no file for skipped section | CONFORMS | `lines.clear()` |
| `{num}` / `{*}` repetition | PARTIAL | regex OK; line_no broken (#1,#2); debug panic (#5) |
| file naming `prefix`+N digits | CONFORMS | line 112 |
| suffix overflow → error | CONFORMS | line 109 |
| remaining content → final file | CONFORMS | lines 198-200 |
| error if operand past EOF | MISSING | (#8) |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Item | Status | Notes |
|---|---|---|
| Exit 0 success | CONFORMS | line 638 |
| Exit >0 error | CONFORMS | line 629 |
| files removed on error (default) | CONFORMS | lines 631-634 |
| `-k` retains on error | CONFORMS | guarded by `!args.keep` |
| cleanup on signal | MISSING | (#4) |

#### Cross-cutting (BRE flavor, i18n, locale)

BRE entirely via libc `regcomp`/`regexec` (correct §9.3). The escape-stripping gap (#6) means patterns with an escaped delimiter compile the wrong BRE.

### Test coverage signal

Not covered:
- [ ] Multiple `line_no` operands without `{num}` (exposes #1/#2)
- [ ] `\/` / `\%` escape inside rexp (#6)
- [ ] Pattern not found before EOF → error (#8)
- [ ] Signal/interrupt with and without `-k` (#4)
- [ ] `{*}` in a debug build (#5)
- [ ] Byte-count output matches `wc -c` (#3)
- [ ] stdin (`-`) with regex operands

### Suggested PR groupings

- **PR A — "Absolute line-number counter"**: #1, #2.
- **PR B — "Byte-count accuracy"**: #3.
- **PR C — "Signal cleanup"**: #4.
- **PR D — "Escape translation + `{*}` overflow"**: #5, #6.
- **PR E — "Operand range validation + NAME_MAX"**: #7, #8.

---

## `cut`

**Implementation:** `text/cut.rs` (447 lines)
**Tests:** `text/tests/cut/mod.rs` (357 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/cut.md`
**Date:** 2026-06-25

### TL;DR

Covers common use cases and passes a solid test suite, but has five correctness defects. Most impactful: (1) `-f` with no `-d` ignores the default (tab) delimiter and emits every line verbatim; (2) the `-` operand is only honored as the sole argument; (3) the `-n` algorithm under-decrements and lacks the zero/drop logic; (4) range lists accept only comma, not `<blank>`; (5) non-UTF-8 byte output from `-b` is silently dropped with a stderr diagnostic instead of being written raw.

### Priority issues

#### Critical

- [ ] **#1 — `-f` without `-d` passes every line through verbatim instead of using tab default.** `text/cut.rs:337-344`. `cut_fields()` is only called when `args.delimiter` is `Some`; with `-f` and no `-d` the `else` branch unconditionally `println!`s the whole line. `cut -f 1 file` (ubiquitous, default tab) produces wrong output. Fix: default the delimiter to `'\t'` when `-f` is given without `-d`.
- [x] **#2 — `-` file operand only works as the sole argument.** `text/cut.rs:289`. FIXED (Phase 2): each operand is opened via `plib::io::input_stream_dashed`, so `-` reads stdin at any position. (The helper returns an unlocked `Stdin` handle so multiple `-` operands do not deadlock on the stdin lock.)

#### Major

- [ ] **#3 — Non-UTF-8 byte output from `-b` silently suppressed.** `text/cut.rs:328-331`. `String::from_utf8(bytes)` failure only `eprintln!`s and writes nothing; exit stays 0. `-b` selects bytes that need not be valid characters. Fix: write the `Vec<u8>` directly to stdout with `write_all`.
- [ ] **#4 — `-n` algorithm not conformant.** `text/cut.rs:129-135`. Spec: decrement `low` until it is a character's first byte; decrement `high` until it is a character's last byte (of the prior character); drop the element if `high == 0` or `low > high`. Impl decrements by 1 only, never iterates to a true boundary, and has no drop logic. Wrong for characters wider than 2 bytes. Fix: re-implement boundary search + drop logic.

#### Minor

- [ ] **#5 — Range list does not accept blank-separated lists.** `text/cut.rs:353`. Spec allows comma- *or* `<blank>`-separated lists; impl splits on comma only, so `-c "1 3 5"` misparses. Fix: split on comma-or-blank.
- [ ] **#6 — `cut_fields` uses `escape_debug()` to split.** `text/cut.rs:233-239`. A literal backslash delimiter becomes the two-char string `\\` and the line is split on that. Fix: split on the raw `char`.
- [ ] **#7 — File-error handling aborts remaining files.** `text/cut.rs:292-296`. `?` propagates the first open error, halting subsequent files; spec says only the exit status is affected. Fix: emit diagnostic, set exit code, continue.

(Note: `read_range` uses `range.len()==1` to detect single-number elements — fragile but currently masked by the `split('-')` element count; worth tidying.)

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Item | Status | Notes |
|---|---|---|
| `-b`/`-c`/`-f`/`-d`/`-s`/`-n` accepted | CONFORMS | lines 20-36 |
| mutual exclusivity of `-b`/`-c`/`-f` | PARTIAL | no error; spec leaves combination undefined |
| `--` end-of-options | CONFORMS | clap |
| `-` operand for stdin | PARTIAL | only sole operand (#2) |

#### OPTIONS

| Option | Status | Notes |
|---|---|---|
| `-b list` | PARTIAL | selection OK; non-UTF-8 output dropped (#3) |
| `-c list` | CONFORMS | `chars()`, correct under UTF-8 |
| `-f list` | DIVERGES | no default-tab fallback (#1) |
| `-d delim` | PARTIAL | `escape_debug` split bug (#6) |
| `-s` | CONFORMS | suppresses no-delimiter lines |
| `-n` | PARTIAL | single-decrement, no drop logic (#4) |
| list comma-separated | CONFORMS | line 353 |
| list blank-separated | MISSING | (#5) |
| `low-high`/`-high`/`low-` ranges | CONFORMS | lines 359-384 |
| out-of-order list elements | CONFORMS | sort at line 396 |

Note: a strictly decreasing single element (`5-3`) is rejected (correct: `low>high` invalid); out-of-order *across* elements (`-c 5,2`) is handled by the sort.

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| `file...` in order | CONFORMS | lines 292-296 |
| no operands → stdin | CONFORMS | line 289 |
| `-` → stdin | PARTIAL | sole-operand only (#2) |
| one file error does not abort rest | DIVERGES | `?` halts (#7) |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL` | CONFORMS | `setlocale(LcAll,"")` line 432 |
| `LC_CTYPE` | PARTIAL | `-c` uses Rust `char` (correct for UTF-8; not other encodings) |
| `LC_MESSAGES` | CONFORMS | gettextrs |
| `NLSPATH` (XSI) | N/A | handled by gettextrs |

#### ASYNCHRONOUS EVENTS

- [x] Default — CONFORMS.

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| one line per input line w/ newline | CONFORMS | `println!` |
| output to stdout | CONFORMS | |
| diagnostics to stderr only | PARTIAL | conversion error to stderr but line dropped (#3) |

#### EXTENDED DESCRIPTION

| Item | Status | Notes |
|---|---|---|
| elements in input data order | CONFORMS | sort + merge |
| element repeated → written once | CONFORMS | merge dedup |
| `-b -n` adjust to char boundaries | PARTIAL | single-decrement only (#4) |
| `-n` drop when high==0 or low>high | MISSING | (#4) |
| `-f` lines w/o delim passed through | CONFORMS | lines 262-264 |
| `-f` w/o delim suppressed by `-s` | CONFORMS | lines 265-268 |
| `-f` default delimiter is tab | DIVERGES | (#1) |

### Test coverage signal

Not covered:
- [ ] `cut -f 1` (default tab delimiter — golden-path regression)
- [ ] `cut -b 1-2 -n` on a 3-byte UTF-8 character
- [ ] continue after file error (`cut -b 1 /dev/null nonexistent other.txt`)
- [ ] blank-separated list `cut -c "1 3 5"`
- [ ] non-UTF-8 byte output (`-b` on binary data)
- [ ] `-` mixed with real files
- [ ] backslash delimiter (`-d \\ -f 1`)

### Suggested PR groupings

- **PR A — "Default tab delimiter for `-f`"**: #1.
- **PR B — "`-` operand in mixed file lists"**: #2.
- **PR C — "Raw byte output for `-b`"**: #3.
- **PR D — "`-n` boundary algorithm"**: #4.
- **PR E — "List parsing + delimiter splitting + file-error continuation"**: #5, #6, #7.

---

## `diff`

**Implementation:** `text/diff.rs` (201 lines) + `text/diff_util/` (10 files, ~1250 lines)
**Tests:** `text/tests/diff-tests.rs` (437 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/diff.md`
**Date:** 2026-06-25

### TL;DR

Core diff algorithm (histogram LCS), default output, `-e`, `-f`, `-r`, `-b`, and exit-status mapping are present. Context (`-c`/`-C`) and unified (`-u`/`-U`) output is partially implemented: the hunk machinery exists, but several spec-mandated format details are wrong or missing — single-line range abbreviation for context format, the `<frac>`/`<zone>` timezone fields for unified format, "no newline" placement, and `-C 0` exclusion. `-b` over-aggressively collapses interior whitespace. Existence-check errors print to stdout instead of stderr. Infinite-loop detection for `-r` is absent.

### Priority issues

#### Critical

- [ ] **#1 — `-b` whitespace normalization diverges from spec.** `text/diff_util/file_data.rs:26-43`. `normalize_whitespace()` collapses ALL whitespace runs including interior ones and trims leading whitespace. Spec: trailing whitespace before a newline is ignored and interior whitespace runs collapse to a single space — but leading space should not be trimmed/collapsed, producing different comparison semantics. Fix: collapse interior runs and trim trailing only; do not collapse leading space.
- [ ] **#2 — Existence-check errors written to stdout instead of stderr.** `text/diff_util/functions.rs:52-55`. `check_existance()` uses `println!()` for `"diff: {}: No such file or directory"`. Diagnostics on stdout corrupt piped output. Fix: `eprintln!`.

#### Major

- [ ] **#3 — `-r` infinite-loop detection absent.** `text/diff_util/dir_diff.rs:161-166`. Recursion has no visited-directory / inode-device tracking; symlink cycles hang or exhaust the stack. Spec mandates infinite-loop detection. Fix: track visited (dev,ino) and check before recursing.
- [ ] **#4 — Context-format single-line range printed in two-number form.** `text/diff_util/file_diff.rs:733-747`. Spec: one line → `*** %d ****`; ≥2 lines → `*** %d,%d ****`; empty range → preceding line / 0. Impl always emits `*** {start},{end} ****` (e.g. `*** 3,3 ****`). Rejected by spec-following `patch`. Fix: emit single-number form for length-1 ranges.
- [ ] **#5 — Unified header missing `<frac>` and `<zone>` fields.** `text/diff_util/functions.rs:30-33`. `system_time_to_unified_format()` emits only `%Y-%m-%d %H:%M:%S` — no fractional seconds, no `+HHMM` timezone. Spec mandates both; TZ changes have no effect. Fix: append fractional seconds and timezone offset.
- [ ] **#6 — `-C n` rejects n=0.** `text/diff.rs:43`. clap `.range(1..)` makes `diff -C 0` an error; spec places no floor on `-C`. Fix: remove the range restriction.

#### Minor

- [ ] **#7 — "No newline at end of file" placement in context/unified.** `text/diff_util/file_diff.rs:497-502,562-567`. Marker emitted once at the very end (for each file) rather than immediately after the relevant hunk line, and printed unconditionally even with no hunks. Fix: emit after the specific hunk line.
- [ ] **#8 — Default-format "no newline" only for the last hunk.** `text/diff_util/hunks.rs:98,109,118`. `is_last` guards all marker output; a no-trailing-newline file in a non-last hunk suppresses the marker.
- [ ] **#9 — Edit-script "no newline" uses a non-spec message.** `text/diff_util/hunks.rs:149-163,190-204`. For `-e`/`-f`, emits `"diff: {file}: No newline..."` via `println!`, corrupting the edit script fed to `ed`. Spec does not specify this message for edit-script formats.
- [ ] **#10 — `-f` multi-line range uses comma, not space.** `text/diff_util/hunks.rs:178-184`. `-f` ranges should be space-separated with command-letter first; impl uses comma for multi-line ranges.
- [ ] **#11 — Dir diff may call `file_diff` on a FIFO and hang.** `text/diff_util/dir_diff.rs:92-93`. `is_file()` skips dirs but a regular-file-vs-FIFO pair reaches `file_diff()`, which blocks. Spec says do not compare block/char/FIFO special files.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Item | Status | Notes |
|---|---|---|
| `diff [-c\|-e\|-f\|-u\|-C n\|-U n] [-br] file1 file2` | PARTIAL | all options registered; mutual-exclusion not enforced (last wins) |
| XBD §12.2 conventions | CONFORMS | clap |
| `--label`/`--label2` (non-POSIX) | DIVERGES | extensions accepted |

#### OPTIONS

| Option | Status | Notes |
|---|---|---|
| `-b` | PARTIAL | over-aggressive (#1) |
| `-c` | PARTIAL | single-line range (#4), no-newline placement (#7) |
| `-C n` | PARTIAL | rejects n=0 (#6); range format (#4) |
| `-e` | PARTIAL | lone-`.` escaping absent; no-newline format wrong (#9) |
| `-f` | PARTIAL | range separator (#10) |
| `-r` | PARTIAL | no infinite-loop detection (#3) |
| `-u` | PARTIAL | header lacks `<frac>`/`<zone>` (#5) |
| `-U n` | PARTIAL | header lacks `<frac>`/`<zone>` (#5) |

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| `file1 file2` regular files | CONFORMS | |
| `-` for stdin | CONFORMS | reads stdin to temp; rejects `diff - -` |
| both dirs | CONFORMS | `DirDiff::dir_diff` |
| one dir | PARTIAL | correct path; edge-case review noted |
| any file type | PARTIAL | binary heuristic; FIFO hang on dir compare (#11) |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL`/`LC_CTYPE`/`LC_MESSAGES` | CONFORMS | `setlocale` + gettext |
| `LC_TIME` | N/A→PARTIAL | context dates use `chrono::Local` (respects TZ, not LC_TIME names) |
| `TZ` | PARTIAL | context respects TZ; unified omits timezone field (#5) |
| `NLSPATH` | N/A | XSI |

#### ASYNCHRONOUS EVENTS

- [x] Default — CONFORMS.

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| no output when identical | CONFORMS | |
| default format `<`/`>`/`a`/`c`/`d` | CONFORMS | `hunks.rs:81-123` |
| context headers `***`/`---` | CONFORMS | `file_diff.rs:381-387` |
| context single-line range abbrev | DIVERGES | (#4) |
| context empty-range rule | MISSING | never produces 0 |
| context `- `/`! `/`+ ` prefixes | CONFORMS | |
| unified `--- `/`+++ ` headers | PARTIAL | missing `<frac>`/`<zone>` (#5) |
| unified `@@ -l,s +l,s @@` | CONFORMS | `file_diff.rs:647` |
| `-e` reverse order | CONFORMS | |
| `-f` forward, command-before-line | PARTIAL | range separator (#10) |
| "Only in %s: %s" | CONFORMS | `dir_diff.rs:203,210` |
| "Common subdirectories" | CONFORMS | `dir_diff.rs:169` |
| dir-mode `diff %s %s %s` banner | PARTIAL | `-c` short form emits `-C 3` not `-c` |
| diagnostics to stderr | DIVERGES | existence check on stdout (#2) |
| binary-file message | CONFORMS | |

#### EXTENDED DESCRIPTION

| Item | Status | Notes |
|---|---|---|
| default 1-based numbering | CONFORMS | |
| default no-newline — last hunk only | PARTIAL | (#8) |
| context/unified no-newline placement | DIVERGES | (#7) |
| `-e` lone-period escaping | MISSING | |
| `-C 0` zero-context allowed | MISSING | rejected at parse (#6) |
| dir: skip block/char/FIFO | PARTIAL | FIFO can hang (#11) |
| dir: no compare file to dir | CONFORMS | |
| `-r` infinite-loop detection | MISSING | (#3) |
| directory sort order | CONFORMS | byte order; POSIX leaves unspecified |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Code | Meaning | Status |
|---|---|---|
| 0 | no differences | CONFORMS |
| 1 | differences | CONFORMS |
| >1 | error | CONFORMS |

#### Cross-cutting (i18n, locale, date format)

Context date `"%a %b %e %T %Y"` matches spec; `chrono::Local` may not honor `LC_TIME` month/day names. Unified date string is correct per rationale, but the mandated timezone-offset and fractional-second fields are absent (#5).

### Test coverage signal

Not covered:
- [ ] Stdin (`-`) operand
- [ ] Identical files → exit 0, no output
- [ ] Error conditions → exit 2 (not found, permission)
- [ ] Single-line context header (`*** 5 ****`)
- [ ] Empty-range context header (`*** 0 ****`)
- [ ] Unified header timezone/fractional fields
- [ ] `-C 0` zero-context
- [ ] Files without trailing newline in context/unified modes
- [ ] FIFO/block-device in directory comparison
- [ ] `-r` symlink cycles
- [ ] `-e` lone-period escaping
- [ ] `-f` multi-line range separator

### Suggested PR groupings

- **PR A — "Critical: `-b` semantics + stderr routing"**: #1, #2.
- **PR B — "Context format correctness"**: #4, #6, #7.
- **PR C — "Unified header completeness"**: #5.
- **PR D — "`-r` safety"**: #3, #11.
- **PR E — "Edit-script + no-newline minor fixes"**: #8, #9, #10.

---

## `expand`

**Implementation:** `text/expand.rs` (169 lines)
**Tests:** `text/tests/expand/mod.rs` (29 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/expand.md`
**Date:** 2026-06-25

### TL;DR

The `UniStop` (single-integer `-t N`) path works correctly for default and custom uniform widths. The `Stops` (list `-t N,M,...`) path has two bugs that render it wrong on every input: an extra space per tab stop (off-by-one overshoot) and `cur_stop` never reset on newline (all lines after the first use wrong stops). `-t 0` panics with division-by-zero. Multibyte/wide-character column-width tracking under `LC_CTYPE` is entirely absent.

### Priority issues

#### Critical

- [ ] **#1 — Division-by-zero panic on `-t 0`.** `text/expand.rs:104`. `parse_tablist` accepts `0` and returns `UniStop(0)`; the loop evaluates `column % n` with `n=0` → panic. Spec requires a positive integer. Fix: reject zero.
- [ ] **#2 — `cur_stop` never reset on newline.** `text/expand.rs:75,97,119`. In the `Stops` variant `cur_stop` is initialized once per file and only incremented; the newline branch resets `column=1` but not `cur_stop=0`. Every line after the first uses exhausted/wrong stop indices. Fix: reset `cur_stop = 0` on newline.

#### Major

- [ ] **#3 — Off-by-one in `Stops` tab expansion.** `text/expand.rs:115-121`. After `while column < next_tab { space_out }`, an extra `space_out` (line 120) overshoots every stop by one column. With `-t 4,8`, a tab at column 1 emits 4 spaces (lands at col 5) instead of 3. Fix: remove the extra `space_out`.
- [ ] **#4 — Zero accepted in list tabstops.** `text/expand.rs:40-55`. `parse_tablist` does not reject `0` as the first list element (the `!v.is_empty()` guard is skipped). Spec requires each stop > 0. Fix: reject zero in lists.
- [ ] **#5 — Multibyte / wide-character column width not tracked.** `text/expand.rs:87-100`. The byte loop counts every non-special byte as +1 column regardless of encoding/display width; wide chars advance by byte count, not visual width. Spec requires `LC_CTYPE` to govern column width. Fix: use `wcwidth`/`unicode-width` per character.

#### Minor

- [ ] **#6 — `--tablist` long option exposed.** `text/expand.rs:22`. POSIX specifies only `-t tablist`. Non-standard but harmless.
- [x] **#7 — `-` operand treated as literal filename.** `text/expand.rs:70`. FIXED (Phase 2): opens via `plib::io::input_stream_dashed`; a `-` operand reads stdin at its position.

(Note: the backspace floor-at-column-1 is correct — 1-based column 1 == 0-based position 0 — not a bug.)

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Item | Status | Notes |
|---|---|---|
| `expand [-t tablist] [file...]` | CONFORMS | clap |
| `--` end-of-options | CONFORMS | clap default |
| multiple `file` operands | CONFORMS | line 26 |
| no files → stdin | CONFORMS | lines 155-156 |

#### OPTIONS

| Item | Status | Notes |
|---|---|---|
| `-t N` every N cols | CONFORMS | `UniStop` path |
| `-t 0` rejected | MISSING | panics (#1) |
| `-t N,M,...` list | PARTIAL | off-by-one (#3); no newline reset (#2) |
| separator comma/blank | CONFORMS | line 41 |
| ascending order enforced | CONFORMS | line 49 |
| zero in list rejected | MISSING | (#4) |
| beyond last stop → single space | CONFORMS | lines 113-114 |

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| `file` pathname | CONFORMS | |
| no files → stdin | CONFORMS | lines 155-156 |
| `-` as stdin alias | DIVERGES | literal filename (#7) |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL` | CONFORMS | `setlocale(LcAll,"")` line 134 |
| `LC_CTYPE` char interpretation | PARTIAL | byte loop ignores multibyte boundaries |
| `LC_CTYPE` column width | MISSING | no `wcwidth` (#5) |
| `LC_MESSAGES` | CONFORMS | textdomain + gettext |
| `NLSPATH` | N/A | XSI |

#### ASYNCHRONOUS EVENTS

- [x] Default — CONFORMS.

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| tabs → spaces | CONFORMS (UniStop) / PARTIAL (Stops, #3) | |
| non-tab/non-BS passed through | CONFORMS | line 99 |
| backspace copied | CONFORMS | line 91 |
| errors to stderr only | CONFORMS | lines 145, 164 |

#### EXTENDED DESCRIPTION

Spec: "None". Normative behavior in DESCRIPTION/OPTIONS.

| Item | Status | Notes |
|---|---|---|
| ALL tabs converted | CONFORMS | every `\t` processed |
| default 8-column stops | CONFORMS | line 150 |
| tab → next stop via spaces | CONFORMS (UniStop) | |
| backspace decrements (not below 0) | CONFORMS | lines 89-94 |
| list: next char at col N | DIVERGES | extra space → N+1 (#3) |
| stops reset per line | MISSING | (#2) |
| multibyte advances by visual width | MISSING | (#5) |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Item | Status | Notes |
|---|---|---|
| exit 0 success | CONFORMS | |
| exit >0 error | CONFORMS | lines 145, 163 |
| error message on bad file | CONFORMS | line 164 |
| continue after one file fails | CONFORMS | loop tracks `exit_code` |

#### Cross-cutting (i18n, locale, multibyte width)

`setlocale`/`textdomain` are present, but the core loop is byte-oriented (lines 87-100): multibyte sequences split across iterations and column width is always byte count, never display width. `LC_CTYPE` width requirement unmet (#5).

### Test coverage signal

Only two cases (empty input; `"a\tb\tc\n"` default 8-stop). Not covered:
- [ ] `-t N` single integer non-default
- [ ] `-t N,M,...` multi-stop list (entire `Stops` variant untested)
- [ ] multi-line input with `-t` list (exposes #2)
- [ ] tab at/past last stop (single-space rule)
- [ ] backspace column decrement
- [ ] multiple file operands; file not found → exit 1
- [ ] `-t 0` (currently panics, #1)
- [ ] zero value in list (#4)
- [ ] multibyte/wide-character column width (#5)

### Suggested PR groupings

- **PR A — "Critical: newline reset for `Stops`"**: #2.
- **PR B — "Reject zero + fix off-by-one"**: #1, #3, #4.
- **PR C — "Multibyte column width"**: #5.
- **PR D — "CLI hygiene + `-` stdin"**: #6, #7.

---

## `fold`

**Implementation:** `text/fold.rs` (189 lines)
**Tests:** `text/tests/fold/mod.rs` (78 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/fold.md`
**Date:** 2026-06-25

### TL;DR

Core folding logic is present and structurally sound, but the `-s` (break-on-space) path contains a critical index bug that corrupts every split when actual folding occurs. Column-width counting is byte-oriented rather than locale-aware, making multibyte handling wrong under any non-ASCII locale. The test suite is entirely non-exercising: all test inputs are shorter than the fold width, so no actual fold ever fires — every critical property of the utility is untested.

### Priority issues

#### Critical

- [ ] **#1 — `find_last_blank` returns reversed offset, used as forward index.** `text/fold.rs:91-95,139-141`. The function iterates `v.iter().rev().enumerate()` so `pos` is the distance from the end, but `fold_file` treats it as a forward index. For `hello world` (space at forward index 5) it finds reversed `pos`, then splices `data[6..]` and truncates to garbage. Every `-s` fold point produces wrong output when folding actually fires. Fix: return `v.len() - 1 - pos` (or use `rposition`).

#### Major

- [ ] **#2 — Column counting ignores display width under LC_CTYPE.** `text/fold.rs:73-75`. Without `-b`, `incr_column` adds 1 per byte; a 2-byte UTF-8 char adds 2, a 3-byte CJK char adds 3. Spec mandates `LC_CTYPE`-governed column width. Fix: decode UTF-8 and use a `wcwidth`/`unicode-width` lookup.
- [ ] **#3 — `is_whitespace()` used for "blank" in `-s` path.** `text/fold.rs:93`. Matches `\n`,`\r`,`\t`,`\v`,`\f` and all Unicode whitespace; POSIX `<blank>` is space and tab only. Can break at `\r`/`\v`. Fix: restrict to `b' '` and `b'\t'`.
- [ ] **#4 — Multibyte char may be split at an internal byte boundary.** `text/fold.rs:119-134`. The outer loop iterates raw bytes; a fold can land mid-UTF-8-sequence. Spec: a line shall not be broken in the middle of a character.

#### Minor

- [x] **#5 — `-` file operand does not invoke stdin.** `text/fold.rs:103`. FIXED (Phase 2): opens via `plib::io::input_stream_dashed`; `-` reads stdin.
- [ ] **#6 — Non-POSIX long options.** `text/fold.rs:24-35`. clap registers `--bytes`/`--spaces`/`--width`; POSIX specifies only `-b`/`-s`/`-w`.

(Note: backspace decrement and CR-reset to column 0 are handled; no explicit guard prevents a fold immediately before `\r`/`\b`, a quality concern under undefined-width inputs.)

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Item | Status | Notes |
|---|---|---|
| `fold [-bs] [-w width] [file...]` | CONFORMS | clap maps `-b`/`-s`/`-w` |
| `--` end-of-options | CONFORMS | clap |
| default width 80 | CONFORMS | line 30 |
| width must be positive | CONFORMS | `range(1..)` |

#### OPTIONS

| Option | Status | Notes |
|---|---|---|
| `-b` count bytes | CONFORMS | lines 58-59 |
| `-s` break at last blank | PARTIAL | reversed index (#1); blank too broad (#3) |
| `-w width` | CONFORMS | |

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| no operands → stdin | CONFORMS | lines 175-177 |
| operands processed in order | CONFORMS | lines 181-186 |
| `-` operand as stdin | DIVERGES | literal filename (#5) |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL` | CONFORMS | `setlocale(LcAll,"")` line 168 |
| `LC_CTYPE` (column width) | MISSING | raw byte count (#2) |
| `LC_MESSAGES` | PARTIAL | gettextrs init; English diagnostics |
| `NLSPATH` | N/A | |

#### ASYNCHRONOUS EVENTS

- [x] Default — CONFORMS.

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| output = input + inserted newlines, order preserved | CONFORMS | |
| diagnostics to stderr only | CONFORMS | line 184 |

#### EXTENDED DESCRIPTION

| Behavior | Status | Notes |
|---|---|---|
| backspace decrements column (≥0) | CONFORMS | lines 62-65 |
| no newline adjacent to backspace | PARTIAL | no explicit guard |
| carriage-return sets column to 0 | CONFORMS | lines 70-72 |
| no newline adjacent to CR | MISSING | no guard |
| tab advances to next stop | CONFORMS | lines 67-69 |
| `-s` break after last blank in width | PARTIAL | broken (#1,#3) |
| `-s` no-blank → normal fold | CONFORMS | lines 157-159 |
| width in columns w/o `-b` | PARTIAL | ASCII only (#2,#4) |
| width in bytes with `-b` | CONFORMS | |
| never break mid-character | MISSING | (#4) |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Item | Status | Notes |
|---|---|---|
| exit 0 success | CONFORMS | |
| exit >0 error | CONFORMS | line 183 |
| continue after one file error | CONFORMS | loop continues |

#### Cross-cutting (i18n, locale, multibyte width)

Locale initialized, but the core loop is byte-oriented: no UTF-8 decode, no `wcwidth`, folds at byte boundaries. `LC_CTYPE` width requirement unmet (#2, #4).

### Test coverage signal

Not covered (all test inputs fit within 80 columns, so no fold ever fires):
- [ ] Any input that actually causes a fold (`-w` narrow)
- [ ] `-s` with an actual fold point
- [ ] tab / backspace / carriage-return handling
- [ ] multibyte / wide-character column counting
- [ ] `-` operand as stdin; multiple file operands
- [ ] error exit (inaccessible file)
- [ ] word longer than width with `-s`; empty input; no trailing newline

### Suggested PR groupings

- **PR A — "Fix `-s` blank index + blank definition"**: #1, #3.
- **PR B — "Multibyte/LC_CTYPE column width"**: #2, #4.
- **PR C — "`-` stdin + CLI hygiene"**: #5, #6.
- **PR D — "Real folding test suite"**.

---

## `grep`

**Implementation:** `text/grep.rs` (453 lines)
**Tests:** `text/tests/grep/mod.rs` (1407 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/grep.md`
**Date:** 2026-06-25

### TL;DR

Broadly conformant, and the single best design decision is using POSIX `regcomp`/`regexec` via `plib::regex` (libc) — so BRE/ERE flavors, backreferences, equivalence classes, and collating symbols are spec-correct and locale-aware. Three issues stand out: (1) `dedup()` on the sorted pattern list silently drops duplicate patterns, which can flip the exit code; (2) `-i` with `-F` uses Rust `to_lowercase()` instead of locale `LC_CTYPE` folding; (3) diagnostics use Rust OS-error text, not `LC_MESSAGES`. `-G` (BRE alias) is absent but is a GNU extension, not POSIX.

### Priority issues

#### Critical

- [ ] **#1 — Pattern `dedup()` silently drops duplicate patterns.** `text/grep.rs:124-125`. `resolve()` sorts patterns by length then `dedup()`s. Spec: "All of the specified patterns shall be used when matching lines." Removing a duplicate (e.g. an empty match-all pattern supplied twice across `-e`/`-f`) can silently change the exit code from 0 to 1. Fix: do not remove patterns; if dedup is wanted for speed, use a membership check that preserves match semantics. (Sorting also reorders evaluation — permissible per "order of evaluation is unspecified," but removal is not.)

#### Major

- [ ] **#2 — `-i` with `-F` uses Unicode `to_lowercase()`, not locale `LC_CTYPE` folding.** `text/grep.rs:220,266-270`. The regex path correctly uses `REG_ICASE` (locale-aware); the fixed-string path folds with Rust Unicode case-folding, independent of locale. Wrong in e.g. Turkish (`I`↔`ı`). Fix: fold via `libc::towlower`, or route `-F -i` through a regex with escaped literals + `REG_ICASE`.
- [ ] **#3 — Diagnostics not locale-aware (LC_MESSAGES).** `text/grep.rs:326`. File-open errors use Rust `io::Error` Display (`"No such file or directory (os error 2)"`), not `LC_MESSAGES`-translated text. Fix: route through gettext.

#### Minor

- [ ] **#4 — `\r\n` line endings: only `\n` stripped.** `text/grep.rs:361-365`. A trailing `\r` remains in the matched line on DOS/Windows files. POSIX text files use `\n`, so quality issue rather than violation.
- [ ] **#5 — `-c`/`-l`/`-q` and `-E`/`-F` conflicts enforced as hard errors.** `text/grep.rs:76-87`. POSIX shows these as mutually exclusive but leaves conflicting use "undefined"; a hard error (exit 2) is a conformant, reasonable choice — noted for completeness.
- [ ] **#6 — Non-POSIX long options + absent `-G`.** Long option aliases (`--extended-regexp`, etc.) are harmless extensions; `-G` is a GNU extension, not required by POSIX.

(Verified non-issues: `-q` correctly exits 0 on first match even with later errors — the per-file early-exit at grep.rs:331-333 fires immediately once `any_matches` is set.)

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Item | Status | Notes |
|---|---|---|
| `-E`/`-F` | CONFORMS | route to ERE / fixed-string |
| `-G` (GNU only) | N/A | not in POSIX.1-2024 |
| `-c`/`-l`/`-q` | CONFORMS | output modes |
| `-i` | PARTIAL | regex OK; `-F` not locale-aware (#2) |
| `-n`/`-s`/`-v`/`-x` | CONFORMS | |
| `-e` / `-f` repeatable | CONFORMS | `Vec` |
| `--` / `-` stdin | CONFORMS | grep.rs:314-316 |

#### OPTIONS

| Option | Status | Notes |
|---|---|---|
| `-E` ERE / `-F` fixed / default BRE | CONFORMS | via libc regcomp |
| null pattern matches all | CONFORMS | macOS `.*` workaround; Linux empty string |
| multiple `-e`/`-f` combined | PARTIAL | dedup may drop patterns (#1) |
| `-i` | PARTIAL | (#2) |
| `-l` stdin name `(standard input)` | CONFORMS | grep.rs:316 |
| `-n` per-file reset | CONFORMS | |
| `-q` exit 0 on match even w/ errors | CONFORMS | grep.rs:331-333 |
| `-s` suppress errors not exit status | CONFORMS | |
| `-x` whole-line | CONFORMS | regex `^..$`; fixed equality |

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| first arg = pattern when no `-e`/`-f` | CONFORMS | grep.rs:108-116 |
| multiple files → `filename:` prefix | CONFORMS | grep.rs:389-391 |
| single file/stdin → no prefix | CONFORMS | |
| `-` → stdin | CONFORMS | grep.rs:314 |
| missing files with `-s` | CONFORMS | suppressed; `any_errors` set |
| `-e` newline-separated patterns | CONFORMS | split on `\n` |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL` | CONFORMS | `setlocale(LcAll,"")` |
| `LC_COLLATE` | PARTIAL | used by libc regex brackets; not in `-F` path |
| `LC_CTYPE` | PARTIAL | regex OK; `-F` Unicode-only (#2) |
| `LC_MESSAGES` | MISSING | OS error strings (#3) |
| `NLSPATH` | N/A | XSI |

#### ASYNCHRONOUS EVENTS

- [x] Default — CONFORMS.

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| matched lines to stdout | CONFORMS | |
| `-l` one filename per matched file | CONFORMS | grep.rs:380 |
| `-c` count per file | CONFORMS | grep.rs:418-424 |
| `-n` `line:` prefix | CONFORMS | grep.rs:394-396 |
| multi-file `filename:` prefix | CONFORMS | grep.rs:389-391 |
| `-q` writes nothing | CONFORMS | |
| diagnostics to stderr only | CONFORMS | |

#### EXTENDED DESCRIPTION / REGEX FLAVOR

Engine: `plib::regex::Regex` over POSIX libc `regcomp`/`regexec`.

| Item | Status | Notes |
|---|---|---|
| default BRE / `-E` ERE | CONFORMS | flag → `REG_EXTENDED` |
| BRE backrefs `\1`-`\9`, `\(\)`, `\{m,n\}` | CONFORMS | libc |
| ERE `()`, `{m,n}`, `+`,`?`,`\|` | CONFORMS | libc |
| bracket / `[:class:]` / `[=a=]` / `[.ch.]` | CONFORMS | libc |
| `-F` literal bytes; null matches all | CONFORMS | `contains` |
| `-x` anchoring (regex + fixed) | CONFORMS | |
| pattern OR across `-e`/`-f` | PARTIAL | dedup may drop (#1) |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Code | Status | Notes |
|---|---|---|
| 0 selected | CONFORMS | grep.rs:340 |
| 1 none selected | CONFORMS | grep.rs:338-339 |
| >1 error | CONFORMS | grep.rs:336-337 |
| `-q` 0 on selection even w/ error | CONFORMS | grep.rs:331-333 |
| invalid regex → exit >1 | CONFORMS | |

#### Cross-cutting (regex flavor BRE vs ERE, i18n, locale, collation)

libc regex is locale-aware and spec-correct. The only locale gap is `-F -i` (bypasses libc, #2) and diagnostic text (#3).

### Test coverage signal

Well-covered: all single options across BRE/ERE/fixed modes, multiple `-e`/`-f`, empty patterns, multi-file prefixes, `-` stdin, long aliases, invalid-pattern exit code, quiet+error orderings.

Not covered:
- [ ] dedup bug: two identical patterns from separate `-e` (no test confirms both retained) (#1)
- [ ] `-i -F` where Unicode vs locale folding diverge (Turkish `I`/`ı`) (#2)
- [ ] `LC_MESSAGES`-translated error messages (#3)
- [ ] ERE lazy quantifiers (POSIX.1-2024 §9.4.6 item 6)
- [ ] pattern file with trailing newline (empty last pattern → match-all)
- [ ] NUL-byte/binary input; `\r\n` line endings (#4)

### Suggested PR groupings

- **PR A — "Preserve all patterns (remove dedup)"**: #1.
- **PR B — "Locale-aware `-F -i` folding"**: #2.
- **PR C — "i18n diagnostics + `\r\n` handling"**: #3, #4.

---

## `head`

**Implementation:** `text/head.rs` (209 lines)
**Tests:** `text/tests/head/mod.rs` (269 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/head.md`
**Date:** 2026-06-25

### TL;DR

Largely correct. Two actionable gaps: (1) the `-` operand is not treated as stdin (`input_stream` called with `dashed_stdin = false`), so `head - file2` tries to open a literal file named `-`; (2) `head -n 0` / `head -c 0` exit with code 1 and a diagnostic instead of silently producing no output, diverging from universal consensus (the "positive integer" clause is an application-side constraint). The new POSIX.1-2024 `-c` option is correctly implemented; the obsolescent `-number` form is correctly absent.

### Priority issues

#### Critical

- none.

#### Major

- [x] **#1 — `-` operand not treated as stdin.** `text/head.rs:64`. FIXED (Phase 2): opens via `plib::io::input_stream_dashed`; `head -` reads stdin.
- [ ] **#2 — Zero count produces non-zero exit instead of empty output.** `text/head.rs:161-164,170-173`. Spec makes "positive integer" a caller constraint, not a utility-error trigger; GNU/BusyBox/uutils all emit empty output for `-n 0`/`-c 0`. The impl exits 1 with a diagnostic, breaking `head -n "$N"` where `$N` may be 0. Fix: remove the zero guards (the count loops already produce empty output for 0).

#### Minor

- [ ] **#3 — `--lines`/`--bytes` long options are non-POSIX.** `text/head.rs:26,32`. Harmless GNU extensions; POSIX specifies only `-n`/`-c`.
- [ ] **#4 — Zero-value diagnostics not routed through gettext.** `text/head.rs:162,171`. Hardcoded English (moot if #2 removes the guards).

(Verified CONFORMS: the multi-file `==> name <==` header format and first-vs-subsequent blank-line placement match the spec exactly, lines 55-60.)

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Item | Status | Notes |
|---|---|---|
| `head [-c number\|-n number] [file...]` | CONFORMS | clap mutual-exclusion group |
| `-n`/`-c` separate or joined arg | CONFORMS | lines 26, 32 |
| `-n`/`-c` mutually exclusive | CONFORMS | `N_C_GROUP` |
| `--` end-of-options | CONFORMS | clap |
| historical `-number` form | N/A | removed Issue 6; correctly absent |
| `--lines`/`--bytes` long options | PARTIAL | non-POSIX (#3) |

#### OPTIONS

| Option | Status | Notes |
|---|---|---|
| `-n number` first N lines (default 10) | CONFORMS | lines 96-137 |
| `-c number` first N bytes | CONFORMS | lines 69-94 |
| default `-n 10` | CONFORMS | lines 156-159 |
| file shorter than N → copy in full, no error | CONFORMS | EOF breaks loop |
| number must be positive | DIVERGES | rejects 0 with exit 1 (#2) |

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| `file...` in order | CONFORMS | lines 199-206 |
| no operand → stdin | CONFORMS | lines 188-190 |
| `-` operand → stdin | DIVERGES | literal filename (#1) |
| `-c` arbitrary data | CONFORMS | raw byte loop |
| `-n` text line counting | CONFORMS | counts `\n` |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL` | CONFORMS | `setlocale(LcAll,"")` line 145 |
| `LC_CTYPE` | PARTIAL | byte I/O; `-n` counts newlines so fine |
| `LC_MESSAGES` | PARTIAL | gettextrs init; hardcoded English diagnostics |
| `NLSPATH` | PARTIAL | textdomain set |

#### ASYNCHRONOUS EVENTS

- [x] Default — CONFORMS.

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| portions of input to stdout | CONFORMS | lines 87, 131 |
| single file: no header | CONFORMS | `want_header = files.len() > 1` |
| multi-file `==> name <==` header | CONFORMS | lines 55-60 |
| first header no leading newline | CONFORMS | `first` flag |
| subsequent header leading newline | CONFORMS | line 59 |
| diagnostics to stderr only | CONFORMS | |

#### EXTENDED DESCRIPTION

| Item | Status | Notes |
|---|---|---|
| line counting by `\n` | CONFORMS | line 118 |
| byte counting raw | CONFORMS | lines 69-94 |
| last line without newline copied | CONFORMS | written to EOF |
| `-n 0` / `-c 0` produce nothing | DIVERGES | exits 1 (#2) |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Item | Status | Notes |
|---|---|---|
| exit 0 success | CONFORMS | line 194 |
| exit >0 error | CONFORMS | line 201 |
| continue after per-file error | CONFORMS | loop continues |
| error to stderr not stdout | CONFORMS | line 202 |

#### Cross-cutting (i18n, locale)

Locale initialized; two hardcoded English diagnostics (lines 162, 171) bypass gettext (minor, consistent with siblings).

### Test coverage signal

Not covered:
- [ ] `-` as a file operand (stdin via dash) (#1)
- [ ] `-n 0` / `-c 0` exit-code behavior (property tests don't check exit code) (#2)
- [ ] multiple file operands with `==> name <==` headers
- [ ] non-existent file → non-zero exit
- [ ] single-file header suppression
- [ ] mixed `-` and named files

### Suggested PR groupings

- **PR A — "`-` operand → stdin"**: #1.
- **PR B — "Accept `-n 0`/`-c 0` as empty output"**: #2, #4.
- **PR C — "Multi-file header + `-` stdin tests"**.

---

## `join`

**Implementation:** `text/join.rs` (194 lines)
**Tests:** `text/tests/join/mod.rs` (145 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/join.md`
**Date:** 2026-06-25

### TL;DR

An early-stage stub that passes only the narrowest happy-path tests. At least nine distinct conformance defects exist, several producing wrong output on common inputs. Most severe: (1) the default output format is wrong whenever the join field is not field 1 of file 1; (2) the default field-separator semantics are wrong (splits on a single space, not sequences of blanks with leading-blank suppression); (3) `-a`/`-v` accept only a single file number (`u8`), so `-a 1 -a 2` and `-v 1 -v 2` are impossible; (4) `-o 0` panics at runtime; (5) the join is an O(N×M) nested rescan that ignores sort order; (6) `LC_COLLATE` collation is never used for key comparison.

### Priority issues

#### Critical

- [ ] **#1 — Default output format wrong when join field is not field 1 of file 1.** `text/join.rs:127`. Output is `fields1.join(" ") + " " + fields2[1..].join(" ")`. Spec: join field, then remaining file1 fields (before+after the join field), then remaining file2 fields. With `-1 2` this leaves the join field in place and always drops `fields2[0]` regardless of the file2 join field. Fix: reconstruct per spec around the join-field position.
- [ ] **#2 — `-o 0` (join-field specifier) panics.** `text/join.rs:98-120`. `-o` parsing asserts `f_num.len() == 2`; `"0".split('.')` yields one element → `assert_eq!` aborts. `0` is a mandatory POSIX.1-2024 outer-join feature. Fix: handle `"0"` before the `.`-split.
- [ ] **#3 — Cartesian-product / bookkeeping wrong for duplicate keys; sort order ignored.** `text/join.rs:73-135`. For each file1 line the code rescans all of file2; `matched_keys` is keyed on the key value, so `-v`/`-a` bookkeeping is wrong for repeated keys, and non-consecutive keys (the "must be sorted" precondition) produce wrong output. Fix: merge-join over collation-sorted input.
- [ ] **#4 — Default field-separator semantics wrong.** `text/join.rs:49-51`. `line.split(' ')` produces empty fields for multiple/leading blanks. Spec: multiple separators count as one; leading separators ignored. Fix: whitespace-sequence split for the default case.

#### Major

- [ ] **#5 — `-a` accepts a single file number; `-a 1 -a 2` impossible.** `text/join.rs:21-22,132`. `additional: u8`; only `-a 1` handled. file2 unpairable lines never output. Fix: allow both.
- [ ] **#6 — `-v` accepts a single file number; `-v 1 -v 2` impossible.** `text/join.rs:33-34,139-157`.
- [ ] **#7 — `-v` uses wrong algorithm / breaks on stdin.** `text/join.rs:137-157`. `matched_keys` is populated by file2's matched keys, so `-v 1` suppresses any file1 line whose key ever appeared in file2; also re-opens `file1_path` via `File::open`, crashing when file1 was stdin.
- [ ] **#8 — `LC_COLLATE` not used; key comparison is byte-equal.** `text/join.rs:91`. `key1 == key2` (byte equality). Spec requires collation comparison (`strcoll`). Fix: use `libc::strcoll`.
- [ ] **#9 — `-o` and `-a` output ignore `-t` separator (and `-o`/`-e` on unpairable lines).** `text/join.rs:124,133`. `res.join(" ")` and `fields1.join(" ")` hard-code space; unpairable `-a` lines ignore `-o`/`-e`.
- [ ] **#10 — stdin (`-`) can only be file1.** `text/join.rs:81-85,137`. A second `stdin.lock()` while the first is held; `-v` re-read crashes on stdin.

#### Minor

- [ ] **#11 — `-o` list does not support blank-separated elements.** `text/join.rs:27`. `value_delimiter = ','` only.
- [ ] **#12 — `assert_eq!`/`panic!` in code path.** `text/join.rs:99,120`. Produces Rust backtraces, not POSIX stderr diagnostics + exit >0.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Item | Status | Notes |
|---|---|---|
| full option synopsis | PARTIAL | `-a`/`-v` accept only one value each |
| `--` end-of-options | CONFORMS | clap |
| `-` as file operand | PARTIAL | file1 works; file2 broken (#10) |
| legacy `-j`/`-j1`/`-j2` | N/A | not required |

#### OPTIONS

| Option | Status | Notes |
|---|---|---|
| `-a 1` | PARTIAL | file2 unpairable never output |
| `-a 2` / `-a 1 -a 2` | MISSING | `u8` can't hold two (#5) |
| `-e string` | PARTIAL | only matched lines under `-o` |
| `-o list` comma | PARTIAL | `-o 0` panics (#2); separator ignores `-t` (#9) |
| `-o list` blank-separated | MISSING | (#11) |
| `-o 0` join-field | MISSING | panics (#2) |
| `-t char` | PARTIAL | input ok; output always space (#9) |
| `-t` default multi/leading blank | MISSING | (#4) |
| `-v 1`/`-v 2` | PARTIAL | wrong algorithm (#7) |
| `-v 1 -v 2` | MISSING | (#6) |
| `-1`/`-2` | CONFORMS | |

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| file1/file2 positional | CONFORMS | both required |
| `-` as file1 | CONFORMS | lines 67-70 |
| `-` as file2 | DIVERGES | double stdin lock (#10) |
| input must be sorted (collation) | MISSING | O(N×M) rescan (#3) |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL` | PARTIAL | `setlocale` line 179 |
| `LC_COLLATE` | MISSING | byte equality (#8) |
| `LC_CTYPE` | PARTIAL | char-based, not byte-boundary aware |
| `LC_MESSAGES` | PARTIAL | gettextrs init |

#### ASYNCHRONOUS EVENTS

- [x] Default — CONFORMS.

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| default output ordering | PARTIAL | wrong when join field ≠ field 1 (#1) |
| `-t` separator on output | MISSING | hard-coded space (#9) |
| `-o` field order | PARTIAL | works for matched, comma-list |
| stderr only for diagnostics | PARTIAL | panics bypass (#12) |

#### EXTENDED DESCRIPTION

| Behavior | Status | Notes |
|---|---|---|
| equality join on collation-equal keys | MISSING | byte `==` (#8) |
| join field + remaining file1 + remaining file2 | PARTIAL | (#1) |
| rearrange fields around join field | MISSING | (#1) |
| input sorted precondition | MISSING | (#3) |
| multi/leading blank = one separator | MISSING | (#4) |
| `-t` char each occurrence significant | PARTIAL | input ok; output wrong (#9) |
| cartesian product for duplicate keys | PARTIAL | by coincidence; bookkeeping wrong (#3) |
| `-a` unpairable output | PARTIAL | `-a 1` only; ignores `-t`/`-o`/`-e` (#5,#9) |
| `-v` only unpairable | PARTIAL | single file; wrong; stdin unsupported (#6,#7) |
| `-e` replaces empty `-o` fields | PARTIAL | matched lines only |
| `-o 0` join-field | MISSING | panics (#2) |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Item | Status | Notes |
|---|---|---|
| exit 0 success | CONFORMS | lines 185-192 |
| exit >0 error | CONFORMS | on `Err` |
| panics → no stderr message | DIVERGES | (#12) |

#### Cross-cutting (collation, i18n, locale)

`setlocale(LcAll,"")` is called but locale is never used afterward. Key comparison is Rust `==`; no `strcoll` anywhere. `LC_COLLATE` is effectively ignored for the utility's primary function.

### Test coverage signal

Existing tests cover only unique-key, space-separated, default-field happy paths.

Not covered:
- [ ] `-a 2`; `-a 1 -a 2`; `-v 1 -v 2`
- [ ] `-o 0`; `-o` blank-separated; `-o` with `-t` output separator
- [ ] `-e` with `-a` unpairable lines
- [ ] join on a field other than field 1 (#1)
- [ ] duplicate keys (cartesian product)
- [ ] multi-blank / leading-blank separators (#4)
- [ ] stdin as file2 (#10)
- [ ] locale-collation key comparison (#8)
- [ ] error exit for bad field numbers / unreadable files

### Suggested PR groupings

- **PR A — "Field-separator semantics"**: #4, plus `-t` output separator (#9).
- **PR B — "Default output reconstruction + `-o 0`"**: #1, #2.
- **PR C — "Multi-value `-a`/`-v` + unpairable formatting"**: #5, #6, #7.
- **PR D — "Merge-join + LC_COLLATE"**: #3, #8.
- **PR E — "stdin robustness + remove panics"**: #10, #11, #12.

---

## `nl`

**Implementation:** `text/nl.rs` (313 lines)
**Tests:** `text/tests/nl/mod.rs` (95 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/nl.md`
**Date:** 2026-06-25

### TL;DR

Mostly correct. The golden path (body numbering, `-v`, `-i`, `-n`, `-s`, `-w`, `-p`, logical-page delimiters, `-d` override) all work. Two defects stand out: the `pBRE` numbering filter uses Rust's `regex` crate (an ERE/Perl superset) instead of a POSIX BRE engine, and the `-l` blank-line counter is not reset when crossing a logical-page section delimiter, miscounting blank-line groups in multi-section files.

### Priority issues

#### Critical

- none.

#### Major

- [ ] **#1 — `pBRE` uses an ERE engine, not BRE.** `text/nl.rs:12,92-93`. The `-b pSTRING`/`-f pSTRING`/`-h pSTRING` arguments are spec'd as BRE (§9.3) but compiled with `regex::Regex::new()` (ERE/Perl). BRE constructs (`\(…\)`, `\{n\}` intervals, backreferences) and anchoring semantics diverge. The test file even notes "better regex support than the reference." Fix: use a POSIX BRE engine (e.g. `plib`/libc `regcomp` without `REG_EXTENDED`).
- [ ] **#2 — `-l` blank-line counter not reset across section boundaries.** `text/nl.rs:155,220-228`. `consecutive_blank_lines` is reset only on a non-empty line or when the join count is reached, never on a `\:`/`\:\:`/`\:\:\:` delimiter. Trailing blank lines from one section inflate the next section's run. Fix: reset the counter in the `non_text` branch.

#### Minor

- [ ] **#3 — Regex match is unanchored.** `text/nl.rs:261`. `is_match` matches anywhere (consistent with spec "contain"); but ERE anchoring (#1) differs from BRE at interval boundaries.
- [ ] **#4 — `LC_COLLATE`/`LC_CTYPE` not passed to the regex engine.** `text/nl.rs:291-293`. `setlocale` is called but the `regex` crate ignores it; locale-aware bracket expressions unsupported. (Resolved if #1 moves to libc regcomp.)
- [ ] **#5 — `-d` empty / >2-char argument exits 1.** `text/nl.rs:297-306`. Spec doesn't define these cases; exiting 1 is a plausible-but-unspecified interpretation.
- [ ] **#6 — Unnumbered lines emit aligned spaces rather than `<empty>`+suppressed separator.** `text/nl.rs:194-200`. Spec STDOUT wording suggests the number is blank and the separator omitted; impl emits `width+sep` spaces. This is the traditional universal behavior; low risk.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Item | Status | Notes |
|---|---|---|
| short options `-bdfhilnpsvw` | CONFORMS | clap |
| at most one `file` operand | CONFORMS | `Option<PathBuf>` |
| `--` end-of-options | CONFORMS | clap |
| `-` as stdin | CONFORMS | line 137 |
| no file → stdin | CONFORMS | line 143 |
| XSI marker | N/A | entire utility is XSI |

#### OPTIONS

| Option | Status | Notes |
|---|---|---|
| `-b a`/`-b t`/`-b n` | CONFORMS | |
| `-b pBRE` | DIVERGES | ERE engine (#1) |
| `-d delim` (2-char / 1-char) | CONFORMS | lines 297-306 |
| `-f`/`-h` types | CONFORMS | |
| `-i incr` | CONFORMS | |
| `-l num` | PARTIAL | no section reset (#2) |
| `-n ln`/`rn`/`rz` | CONFORMS | |
| `-p` | CONFORMS | |
| `-s sep` | CONFORMS | |
| `-v startnum` | CONFORMS | |
| `-w width` | CONFORMS | |

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| single `file` | CONFORMS | |
| `-` → stdin | CONFORMS | line 137 |
| no operand → stdin | CONFORMS | line 143 |
| input is text file | CONFORMS | `BufRead` |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL` | CONFORMS | `setlocale` |
| `LC_COLLATE`/`LC_CTYPE` | PARTIAL | ignored by regex engine (#4) |
| `LC_MESSAGES` | CONFORMS | gettextrs |
| `NLSPATH` | CONFORMS | textdomain |

#### ASYNCHRONOUS EVENTS

- [x] Default — CONFORMS.

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| `%6d` rn / `%06d` rz / `%-6d` ln | CONFORMS | |
| `<empty>`+separator suppressed for unnumbered | PARTIAL | emits spaces (#6) |
| separator between number and line | CONFORMS | |
| stderr only for diagnostics | CONFORMS | |

#### EXTENDED DESCRIPTION

| Item | Status | Notes |
|---|---|---|
| header/body/footer sections | CONFORMS | |
| default section = body | CONFORMS | line 154 |
| `\:\:\:`/`\:\:`/`\:` delimiters | CONFORMS | lines 147-149, 220-226 |
| delimiter line not numbered | CONFORMS | `non_text` |
| reset numbering at page delimiter | CONFORMS | lines 276-278 |
| `-p` suppresses reset | CONFORMS | line 276 |
| `-l` blank-line grouping | PARTIAL | (#2) |
| BRE matching for `pBRE` | DIVERGES | (#1) |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Item | Status | Notes |
|---|---|---|
| 0 success | CONFORMS | |
| >0 error | CONFORMS | I/O error / overflow |

#### Cross-cutting (BRE flavor, i18n, locale)

`pSTRING` uses the `regex` crate (ERE superset) — DIVERGES from BRE (#1); locale not consulted in bracket expressions (#4).

### Test coverage signal

Not covered:
- [ ] `-b pBRE` with BRE-specific syntax (`\(…\)`, `\{n\}`, backrefs)
- [ ] `-f`/`-h` with `pBRE`
- [ ] `-l N` grouping across a section delimiter (#2)
- [ ] `-d` with 0-length or >2-char argument
- [ ] non-existent file → non-zero exit
- [ ] line-number overflow; `-v` negative start; `-i` zero/negative
- [ ] multiple logical pages in sequence; `-w` wider than default; multi-char `-s`

### Suggested PR groupings

- **PR A — "BRE engine for `pSTRING`"**: #1, #4.
- **PR B — "Reset blank-line counter at section boundary"**: #2.
- **PR C — "Test coverage gaps"**.

---

## `paste`

**Implementation:** `text/paste.rs` (464 lines)
**Tests:** `text/tests/paste/mod.rs` (333 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/paste.md`
**Date:** 2026-06-25

### TL;DR

Functionally solid; passes a broad test suite. One Major gap: serial (`-s`) mode aborts entirely when any file fails to open instead of skipping bad files and continuing — the spec explicitly invokes Section 1.4 (continue-on-error) for `-s` mode. All four escape sequences (`\n`,`\t`,`\\`,`\0`) parse correctly; multiple `-` operands each sharing stdin one line at a time work correctly. No crashes or hangs.

### Priority issues

#### Critical

- none.

#### Major

- [ ] **#1 — Serial mode ignores Section 1.4 "continue on file-open error."** `text/paste.rs:232-287,443-452`. The spec's CONSEQUENCES OF ERRORS says for `-s` the utility "shall provide the default behavior described in Section 1.4" (diagnose and continue to the next operand, exit >0). `open_inputs()` opens ALL files before processing and returns `Err` on any failure, so a bad file at position 2 of 4 silently suppresses positions 3–4. Fix: in serial mode, open each file at processing time and continue past failures. (Parallel mode's eager open is correct, since the spec requires no stdout output on error and no output has been written yet.)

#### Minor

- [ ] **#2 — Empty-string file operand diagnostic lacks `paste:` prefix.** `text/paste.rs:251`. Style inconsistency; spec mandates stderr but not a prefix.
- [ ] **#3 — Non-POSIX `--serial`/`--delimiters` long options.** `text/paste.rs:27-35`. Harmless extensions.
- [ ] **#4 — `-d ""` accepted silently.** `text/paste.rs:167-229`. Spec says "may result in an error"; accepting it (as "no delimiter", matching GNU) is permitted.
- [ ] **#5 — `\0` followed by hex digit not detected.** `text/paste.rs:194`. Spec marks this "unspecified"; no warning emitted (not a violation; a warning would help).
- [ ] **#6 — Error strings not localized.** Raw string literals at paste.rs:218,251,258,275 bypass `gettext()`/`LC_MESSAGES`.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Item | Status | Notes |
|---|---|---|
| `paste [-s] [-d list] file...` | CONFORMS | clap |
| `--` end-of-options | CONFORMS | tests at mod.rs:111,144,179 |
| `--serial`/`--delimiters` | N/A | non-POSIX, harmless |
| no-operand error | CONFORMS | paste.rs:273-277 |
| `-dABC` adjacent arg | CONFORMS | clap |

#### OPTIONS

| Option / Behavior | Status | Notes |
|---|---|---|
| `-d list` parsed | CONFORMS | paste.rs:167 |
| `\t`/`\n`/`\\`/`\0` escapes | CONFORMS | paste.rs:193-208 |
| trailing unescaped `\` → error | CONFORMS | paste.rs:217-220 |
| unknown `\x` escape | PARTIAL | silently drops backslash (GNU); spec "unspecified" |
| `\0` + hex digit | PARTIAL | unspecified; no warning (#5) |
| `-d ""` | PARTIAL | accepted (#4) |
| delimiter cycles per output line | CONFORMS | `Cycle` + `reset()` |
| `-d` reset per file in `-s` | CONFORMS | paste.rs:347 |
| `-s` serial | CONFORMS | paste.rs:289 |
| `-s` empty-file → blank line | CONFORMS | paste.rs:310-311 |

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| `file` pathname | CONFORMS | |
| `-` reads stdin | CONFORMS | paste.rs:243-248 |
| multiple `-` share stdin cyclically | CONFORMS | `OnceCell<Rc<RefCell<Stdin>>>`; tests mod.rs:109,142 |
| multiple `-` in serial mode | CONFORMS | first drains; rest EOF |
| unlimited line lengths | CONFORMS | `read_until` |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL`/`LC_CTYPE` | CONFORMS | `setlocale(LcAll,"")` paste.rs:420 |
| `LC_MESSAGES` | PARTIAL | textdomain set; error strings raw (#6) |
| `NLSPATH` | CONFORMS | gettextrs |

#### ASYNCHRONOUS EVENTS

- [x] Default — CONFORMS.

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| output terminated by `\n` | CONFORMS | paste.rs:399, 311 |
| fields separated by delimiter/default tab | CONFORMS | paste.rs:400-403 |
| no trailing delimiter on last field | CONFORMS | `paste_file.last` check |
| diagnostics to stderr only | CONFORMS | |

#### EXTENDED DESCRIPTION

| Behavior | Status | Notes |
|---|---|---|
| parallel: one line from each file joined | CONFORMS | paste.rs:353 |
| unequal lengths: short files empty fields | CONFORMS | EOF check paste.rs:372 |
| delimiter cycles within a line | CONFORMS | |
| delimiter resets per output line (parallel) | CONFORMS | paste.rs:413 |
| `-s`: all lines of a file → one output line | CONFORMS | paste.rs:299-348 |
| `-s`: last `\n` not replaced | CONFORMS | paste.rs:310-311 |
| `-s`: delimiter resets per file | CONFORMS | paste.rs:347 |
| `-s`: empty file → blank line | CONFORMS | |
| all files opened upfront (parallel) | CONFORMS | avoids partial stdout |
| all files opened upfront (serial) | DIVERGES | must continue past bad files (#1) |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Item | Status | Notes |
|---|---|---|
| exit 0 success | CONFORMS | |
| exit >0 error | CONFORMS | `exit(1)` |
| parallel: bad file → stderr + no stdout | CONFORMS | eager open before I/O |
| serial: bad file → continue with next operand | DIVERGES | aborts (#1) |

#### Cross-cutting (i18n, locale, escape parsing)

`setlocale` called; delimiters parsed as UTF-8 `char` sequence; input treated as byte stream (non-UTF-8 test at mod.rs:206). Error strings not localized (#6).

### Test coverage signal

Well-covered: default/serial modes, single/multi-delimiter cycling, multiple `-` in both modes, escape combinations, non-UTF-8 passthrough, trailing-backslash error, `--`.

Not covered:
- [ ] serial mode with a file that fails to open (#1 is untested)
- [ ] parallel mode with unequal file lengths
- [ ] `-d '\n'` newline delimiter
- [ ] `-d '\0x'` (unspecified hex-after-null path)
- [ ] `paste - file` mixed stdin and file
- [ ] `LC_CTYPE` effect on delimiter classification
- [ ] empty file operand in serial mode
- [ ] very large number of `-` operands

### Suggested PR groupings

- **PR A — "Serial-mode continue-on-error"**: #1.
- **PR B — "Localize diagnostics + prefix"**: #2, #6.
- **PR C — "Missing test coverage"**.

---

## `patch`

**Implementation:** `text/patch.rs` (273 lines) + `text/patch_util/` (8 files, ~1050 lines)
**Tests:** `text/tests/patch/mod.rs` (1122 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/patch.md`
**Date:** 2026-06-25

### TL;DR

The golden path — applying a single unified or context diff to an existing file — works. The POSIX-mandated interactive filename prompt is a known stub. Seven additional gaps exist across backup semantics, file-deletion, `-o` multi-patch concatenation, `-D #else` form, `-p` absolute-path rule, the common-leading-blank normalization, and no-newline-at-EOF preservation. None are data-loss on the happy path, but three (backup once per file, file deletion, `-o` concatenation) are data-modifying behaviours whose absence can silently produce a wrong result.

### Priority issues

#### Critical

- [ ] **#1 — `write_output` always appends a trailing newline regardless of "No newline at end of file" markers.** `text/patch_util/file_ops.rs:141-144` uses `writeln!` unconditionally. Both `unified.rs:91` and `normal.rs:104-106` parse the `\ No newline at end of file` indicator but discard it — no flag stored on `Hunk`/`FilePatch`. A file whose last line lacks a newline is silently given one, mutating data. Fix: store a `no_trailing_newline` flag and suppress the final `writeln!`.
- [ ] **#2 — Files marked `is_delete_file` (`new_path = /dev/null`) are never removed.** `is_delete_file` (set at `unified.rs:48`, `context.rs:51`) is never read outside `types.rs`; no `fs::remove_file` call exists. A deletion patch silently writes an empty file instead of removing the target. Fix: action `is_delete_file` after applying.

#### Major

- [ ] **#3 — Interactive filename prompt (Filename Determination step 5) not implemented.** `text/patch_util/file_ops.rs:64-66` returns `NoTargetFile` where the spec requires prompting on the controlling terminal. Multi-file patches whose target can't be resolved by steps 1–3 are discarded with exit 2.
- [ ] **#4 — `-o outfile` with multiple patches for the same file does not concatenate versions.** `text/patch_util/file_ops.rs:139` always `File::create` (truncate). Spec requires concatenated intermediate versions. Benign for a single patch; wrong for multi-patch input.
- [ ] **#5 — `-b` backup written on every patch, not only the first.** `text/patch_util/file_ops.rs:114-128` overwrites the `.orig` each call; in a multi-patch run the `.orig` becomes an already-patched intermediate, destroying the true original.
- [ ] **#6 — Automatic reversal detection / user prompt missing.** `text/patch_util/applier.rs:201-204` detects `AlreadyApplied` (reverse match) but never prompts; the "try forward, else reversed, then prompt" logic does not exist.

#### Minor

- [ ] **#7 — `-p num` leading-slash rule for absolute paths not implemented.** `text/patch_util/file_ops.rs:83-92`. For `//foo`, `split('/')` yields `["","","foo"]` and `-p 1` gives `/foo` instead of `foo`; the leading-slash collapsing is absent.
- [ ] **#8 — Common leading blank sequence not stripped.** No normalization in `parser.rs` or the format parsers for a uniform leading-blank prefix across all patch lines.
- [ ] **#9 — `-D define` form omits the `#else` clause.** `text/patch_util/applier.rs:286-314` emits separate `#ifdef`/`#ifndef` blocks for adds/deletes, not the standard `#ifdef / new / #else / old / #endif` form.
- [ ] **#10 — Reject-file line numbers reflect patch-file positions, not approximate new-file positions.** `text/patch_util/file_ops.rs:171-176` writes parsed `old_start`/`new_start` with no cumulative-offset adjustment.

(Awareness: `strip_path` does not reject `..` components; a malicious patch with `../../etc/passwd` and `-p 2` could write outside CWD. POSIX does not prohibit this, but it is a path-traversal smell.)

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Item | Status | Notes |
|---|---|---|
| all 14 POSIX options parsed | CONFORMS | `patch.rs:39-98` (clap) |
| `-c`/`-e`/`-n`/`-u` mutually exclusive | CONFORMS | `patch.rs:104-112` |
| `-R`+`-e` rejected | CONFORMS | `patch.rs:115-117` |
| `--` end-of-options | CONFORMS | clap |
| single `file` operand | CONFORMS | `patch.rs:97-98` |

#### OPTIONS

| Option | Status | Notes |
|---|---|---|
| `-b` backup `.orig` | PARTIAL | written every call (#5) |
| `-c`/`-u`/`-n` force format | CONFORMS | |
| `-d dir` chdir | CONFORMS | `patch.rs:168-170` |
| `-D define` ifdef | PARTIAL | no `#else` (#9) |
| `-e` ed script | CONFORMS | applied by position |
| `-i patchfile` | CONFORMS | |
| `-l` loose blanks | CONFORMS | `applier.rs:242-247` |
| `-N` ignore applied | PARTIAL | skips, no prompt (#6) |
| `-o outfile` | PARTIAL | no concatenation (#4) |
| `-p num` strip | PARTIAL | absolute-path slash (#7) |
| `-R` reverse | PARTIAL | no auto-detect+prompt (#6) |
| `-r rejectfile` | CONFORMS | `file_ops.rs:161-164` |

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| `file` operand used first | CONFORMS | `file_ops.rs:25-27` |
| patch from stdin when `-i` absent | CONFORMS | `patch.rs:154-160` |
| format auto-detection | CONFORMS | `parser.rs:36-50` |
| multiple patches in one file | CONFORMS | `parser.rs:79-138` |
| `Index:` header | CONFORMS | per-format parsers |
| common leading blank strip | MISSING | (#8) |
| step 5 interactive prompt | MISSING | (#3) |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL` | CONFORMS | `setlocale` `patch.rs:246` |
| `LC_CTYPE` | PARTIAL | OS-level only |
| `LC_MESSAGES` | PARTIAL | gettextrs; yes/no prompt absent (prompt itself missing) |
| `LC_TIME` | N/A | timestamps only recognized, not generated |
| `NLSPATH` | N/A | XSI |

#### ASYNCHRONOUS EVENTS

| Item | Status | Notes |
|---|---|---|
| Default signal disposition | CONFORMS | spec says Default |
| temp-file cleanup | N/A | writes go directly to destination |

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| STDOUT not used | CONFORMS | no `println!` |
| diagnostics to stderr | CONFORMS | `eprintln!` |
| "Hunk #N FAILED" | CONFORMS | `patch.rs:233` |
| offset/fuzz messages to stderr | CONFORMS | `applier.rs:47-57` |
| step-5 prompt to stdout | MISSING | (#3) |

#### EXTENDED DESCRIPTION

| Item | Status | Notes |
|---|---|---|
| auto-detect format | CONFORMS | `parser.rs:21-51` |
| `-p` basename default | CONFORMS | `file_ops.rs:72-78` |
| `-p 0` full path | CONFORMS | `file_ops.rs:79-81` |
| `-p n` strip n | PARTIAL | abs-path `//` (#7) |
| filename determination steps 1-3 | CONFORMS | `file_ops.rs:39-52` |
| step 4 (SCCS) | N/A | XSI optional |
| step 5 (prompt) | MISSING | (#3) |
| fuzzy/offset hunk scan | CONFORMS | `applier.rs:137-159` |
| fuzz factor | CONFORMS | `applier.rs:162-199` |
| reject file = context format | CONFORMS | `file_ops.rs:179-224` |
| reject line numbers reflect new-file | PARTIAL | (#10) |
| `-R` reversal | PARTIAL | logic correct; no auto-detect+prompt (#6) |
| `-b` `.orig` only on first patch | DIVERGES | (#5) |
| `-b`+`-o` → `outfile.orig` | CONFORMS | `file_ops.rs:116-121` |
| `-D` ifdef wrapping | PARTIAL | (#9) |
| common leading blank strip | MISSING | (#8) |
| no-newline-at-EOF | DIVERGES | parsed but discarded (#1) |
| file deletion when new=`/dev/null` | MISSING | (#2) |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Exit | Meaning | Status |
|---|---|---|
| 0 | all hunks applied | CONFORMS |
| 1 | reject lines written | CONFORMS (`patch.rs:261`) |
| >1 (2) | error | CONFORMS |

#### Cross-cutting (i18n, locale, security of `-p` path traversal)

i18n via gettextrs for help strings; locale init at `patch.rs:246-248`. `strip_path` does not reject `..` components (path-traversal smell, #7 area).

### Test coverage signal

Not covered:
- [ ] no-newline-at-EOF preservation (#1)
- [ ] file deletion patch (`/dev/null` new file) (#2)
- [ ] multi-patch `-b` "only first" invariant (#5)
- [ ] `-o` with multiple patches concatenation (#4)
- [ ] absolute path with `-p 1` leading-slash stripping (#7)
- [ ] common leading blank normalization (#8)
- [ ] `-D #else` form (#9)
- [ ] reject-file hunk line-number adjustment (#10)
- [ ] auto-detect-reversed prompt (#6)

### Suggested PR groupings

- **PR A — "Critical data-mutation fixes"**: #1, #2.
- **PR B — "Backup + `-o` multi-patch semantics"**: #4, #5.
- **PR C — "`-D #else` + reject line numbers + leading-blank strip"**: #8, #9, #10.
- **PR D — "Interactive prompt + reversed auto-detect"**: #3, #6.

---

## `pr`

**Implementation:** `text/pr.rs` (577 lines) + `text/pr_util/` (args.rs 564, line_iterator.rs 161, line_transform.rs 143, page_iterator.rs 87, mod.rs 18)
**Tests:** `text/tests/pr/mod.rs` (308 lines, 14 tests)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/pr.md`
**Date:** 2026-06-25

### TL;DR

Solid multi-column, pagination, and option-parsing foundations. Seven conformance gaps: the date format uses `%d` (zero-pad) instead of the spec-mandated `%e` (space-pad); `-p` pause reads from stdin instead of `/dev/tty` and waits for `\r` instead of `\n`; XSI `-f` pauses before every page rather than only the first; `-s` is wrongly restricted to multi-column mode; `-e`/`-i` are not implicitly assumed for multi-column output; form-feed mid-page sets `num_nonpadding_lines` wrong; SIGINT diagnostic deferral is unimplemented. No crash/data-loss bugs, but two (date format, `-p` channel) affect every invocation of those features.

### Priority issues

#### Critical

- none.

#### Major

- [ ] **#1 — Date format `%d` vs `%e`.** `text/pr.rs:30`. `DATE_TIME_FORMAT = "%b %d %H:%M %Y"` zero-pads the day; spec mandates `%e` (space-pad). Every header on days 1–9 is wrong. Fix: use `"%b %e %H:%M %Y"`.
- [ ] **#2 — `-p` reads from stdin, not `/dev/tty`.** `text/pr.rs:66`. Spec: `/dev/tty` is used for `-p` responses. With stdin a pipe (the common case) it reads input data instead of waiting for the keyboard. Fix: open `/dev/tty`.
- [ ] **#3 — `-p` waits for `\r`, not `\n`.** `text/pr.rs:72`. POSIX Defect 1433 (Issue 8) made the trigger `<newline>`; the impl waits for `\r`, which never arrives under normal line discipline. Fix: wait for `\n`.
- [ ] **#4 — `-f` (XSI) pauses before every page instead of only the first.** `text/pr_util/args.rs:384-385` sets `pause = form_feed_with_pause` (same flag as `-p`). Spec: pause once before page 1. Fix: track separately.

#### Minor

- [ ] **#5 — `-e`/`-i` not implicitly assumed for multi-column output.** `text/pr_util/args.rs:395-408`. Spec: "`-e` and `-i` shall be assumed for multiple text-column output." Only activated when explicitly given.
- [ ] **#6 — `-s` falsely requires multi-column mode.** `text/pr_util/args.rs:99` `requires = "multi_column"` rejects `pr -s file`; POSIX places no such restriction.
- [ ] **#7 — `num_nonpadding_lines` wrong on form-feed mid-page.** `text/pr_util/page_iterator.rs:45-49` returns full capacity instead of the actual line count; in multi-column mode this miscomputes `required_rows` and can trip the `assert!` at `pr.rs:365`.
- [ ] **#8 — SIGINT diagnostic deferral not implemented.** No signal handler; spec says pr should flush accumulated error messages on interrupt when writing to a terminal.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Feature | Status | Notes |
|---|---|---|
| `+page` / `+page:last` | CONFORMS | `args.rs:164-168,244-252` |
| `-column` bare digit + clustered | CONFORMS | `args.rs:188-198` |
| no-value option clustering | CONFORMS | `args.rs:27` |
| optional-value clustering `-e/-i/-n/-s` | CONFORMS | trailing optional gets `=\t` default |
| `-h`/`-l`/`-o`/`-w` separate arg | CONFORMS | clap |
| `--` end-of-options | CONFORMS | clap |
| `-` / no operand → stdin | CONFORMS | `add_stdin_if_no_files` |
| `+nondigit` → file operand | CONFORMS | `args.rs:165` |

#### OPTIONS

| Option | Status | Notes |
|---|---|---|
| `+page` / `-column` | CONFORMS | |
| `-a` across | CONFORMS | |
| `-d` double-space | CONFORMS | `args.rs:389-392` |
| `-e[c][g]` expand tabs | CONFORMS | `line_transform.rs:19` |
| `-f` XSI pause-first-only | DIVERGES | every page (#4) |
| `-F` form-feed | CONFORMS | |
| `-h header` | CONFORMS | |
| `-i[c][g]` output tabs | CONFORMS | `line_transform.rs:42` |
| `-l lines` | CONFORMS | header/trailer auto-suppress when ≤10 |
| `-m` merge | CONFORMS | |
| `-n[c][w]` numbering | CONFORMS | |
| `-o offset` | CONFORMS | |
| `-p` pause | PARTIAL | stdin not `/dev/tty` (#2); `\r` not `\n` (#3) |
| `-r` no warnings | CONFORMS | |
| `-s[c]` separator | PARTIAL | wrongly requires multi-column (#6) |
| `-t` omit header/trailer | CONFORMS | |
| `-w width` | CONFORMS | default 72; 512 with `-s` |
| `-e`/`-i` assumed for multi-column | MISSING | (#5) |

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| `file...` sequential | CONFORMS | |
| `-` → stdin | CONFORMS | `input_stream(path,true)` |
| no operand → stdin | CONFORMS | |
| empty file → no output, no error | CONFORMS | |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL` | CONFORMS | `setlocale` `pr.rs:543` |
| `LC_CTYPE` | PARTIAL | `is_non_printable` checks only ASCII `<32` (`pr.rs:157-159`) |
| `LC_MESSAGES` | CONFORMS | `gettext("Page")` `pr.rs:92` |
| `LC_TIME` | PARTIAL | `chrono::format` does not call `strftime`/`nl_langinfo`; month names always English |
| `TZ` | CONFORMS | `chrono::Local` |
| `NLSPATH` | N/A | gettextrs |

#### ASYNCHRONOUS EVENTS

| Item | Status | Notes |
|---|---|---|
| SIGINT flush deferred errors | MISSING | no handler (#8) |
| deferred diagnostics when stdout is terminal | MISSING | printed immediately |

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| header `"\n\n%s %s Page %d\n\n\n"` | CONFORMS | `pr.rs:97` |
| date field `%b %e %H:%M %Y` | DIVERGES | uses `%d` (#1) |
| `<file>` field per file / empty for stdin / `-m` | CONFORMS | |
| `<file>` replaced by `-h` arg | CONFORMS | |
| 5-line header / 5-line trailer | CONFORMS | |
| `-t` suppresses header/trailer | CONFORMS | |
| diagnostics + `-p` alert to stderr | CONFORMS | |

#### EXTENDED DESCRIPTION

| Behavior | Status | Notes |
|---|---|---|
| default page 66 lines / 56 text | CONFORMS | `pr.rs:36` |
| multi-column equal width, truncate to width | CONFORMS | `pr.rs:200,216-221` |
| `-column` down-then-across (default) | CONFORMS | |
| `-a` across-then-down | CONFORMS | |
| form-feed mid-input page break | PARTIAL | `num_nonpadding_lines` wrong (#7) |
| `-m` merge side-by-side | CONFORMS | |
| `-w` default 72; 512 with `-s` | CONFORMS | |
| `-o` offset | CONFORMS | |
| `-n` numbering (default width 5, sep `\t`) | CONFORMS | |
| `-e`/`-i` tab formulas | CONFORMS | `line_transform.rs` |
| `+page` skip | CONFORMS | line-number continuity across skipped pages PARTIAL |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Item | Status | Notes |
|---|---|---|
| exit 0 success | CONFORMS | |
| exit >0 error | CONFORMS | |
| continue remaining files on error | CONFORMS | |
| `-r` suppresses file diagnostics | CONFORMS | |

#### Cross-cutting (i18n, locale, date format, column width)

`gettext("Page")` is `LC_MESSAGES`-sensitive, but `chrono::format` never consults `nl_langinfo(ABMON_*)`, so month abbreviations are always English (non-conformant under non-POSIX `LC_TIME`). `is_non_printable` is ASCII-only (`pr.rs:157-159`), so multibyte non-printables can misalign columns.

### Test coverage signal

Not covered:
- [ ] `-p` pause; `-f` form-feed-with-pause; `-F` vs `-f` distinction
- [ ] `-d` double-space; `-r` suppress-warnings
- [ ] `-s` without multi-column (#6 — currently rejected by clap)
- [ ] form-feed character in input triggering mid-page break (#7)
- [ ] multi-column with input tabs (verifying `-e`/`-i` assumed) (#5)
- [ ] locale-sensitive date output (`LC_TIME` non-POSIX)
- [ ] empty file with `-m`; SIGINT handling

### Suggested PR groupings

- **PR A — "Header date `%e` + tests/fixtures"**: #1.
- **PR B — "`-p` pause channel + trigger char"**: #2, #3.
- **PR C — "`-f` first-page-only + `-e`/`-i` assumed + `-s` constraint"**: #4, #5, #6.
- **PR D — "Form-feed page accounting + SIGINT deferral"**: #7, #8.

---

## `sed`

**Implementation:** `text/sed.rs` (2469 lines)
**Tests:** `text/tests/sed/mod.rs` (2315 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/sed.md`
**Date:** 2026-06-25

### TL;DR

Covers the core command set and passes a wide test suite, but carries several critical conformance defects: (1) the POSIX `l` command is unimplemented — `I` (non-POSIX) is used in its place and `l` is rejected as unknown; (2) the `-E`/ERE global flag is set *after* the script is compiled, so every `-E` invocation silently compiles patterns as BRE; (3) `a`/`r` output is concatenated into the pattern space instead of deferred; (4) `D` erroneously clears the hold space; (5) the `s///i` flag (mandatory since POSIX.1-2024) is absent; (6) `=` is suppressed under `-n`; (7) `y///` post-processes `\\n` in the pattern space; (8) wfiles are not pre-created before processing.

### Priority issues

#### Critical

- [ ] **#1 — `l` command unimplemented; `I` (non-POSIX) used instead.** `text/sed.rs:313,1487,1952-1957`. Lowercase `l` is never matched in `Script::parse`, so any `l` script hits the `_` arm → "unknown character 'l'". Additionally `print_multiline_binary` (`sed.rs:1265-1311`) uses `\x{:02x}` hex rather than the spec-mandated three-digit octal `\NNN`, and does not emit `\\` for backslash. Fix: dispatch `l`; emit octal per byte; emit `\\`.
- [ ] **#2 — ERE global flag set after the script is compiled.** `text/sed.rs:110,2415-2416`. `Script::parse` compiles every regex at line 110, but `*ERE.lock() = self.ere` runs later inside `Sed::sed()`. At parse time the global is always `false`, so `-E` patterns compile as BRE. Fix: set the ERE flag before `Script::parse`.
- [ ] **#3 — `a` appends text into the pattern space rather than scheduling deferred output.** `text/sed.rs:1881-1887`. Spec: `a` text is written just before the next input fetch. Appending to `pattern_space` lets later `s`/`d`/`y` in the same cycle match/alter it. Fix: deferred output queue.
- [ ] **#4 — `D` incorrectly clears the hold space.** `text/sed.rs:2324-2326`. The `NotReadNext` path calls `hold_space.clear()`; POSIX says `D` does not touch the hold space. Breaks any `h`/`H`/`g`/`G`/`x` + `D` script. Fix: remove the clear.

#### Major

- [ ] **#5 — `s///i` case-insensitive flag absent.** `text/sed.rs:1165-1233`. Mandatory since POSIX.1-2024 (Defect 779). The `i` char hits the `_` arm and is silently discarded; the substitution proceeds case-sensitively. Fix: add a case-insensitive `ReplaceFlag` → `REG_ICASE`.
- [ ] **#6 — `=` suppressed under `-n`.** `text/sed.rs:2037-2039`. Gated by `if !self.quiet`; POSIX (and GNU/BSD) print `=` regardless of `-n`. Fix: remove the guard.
- [ ] **#7 — `r` reads the file into the pattern space instead of deferring.** `text/sed.rs:2142-2158`. Same deferred-output requirement as `a`; file content participates in pattern-space ops for the rest of the cycle. Fix: deferred queue.
- [ ] **#8 — wfiles not pre-created before processing begins.** `text/sed.rs:2185-2215,1793-1801`. Spec: each wfile shall be created before processing. The impl opens in append mode at execution time, so an unmatched address leaves the wfile uncreated/stale. Fix: truncate-or-create all wfiles at startup.
- [ ] **#9 — `y///` post-processing replaces literal `\\n` in the pattern space.** `text/sed.rs:2256`. After transliteration, `pattern_space.replace("\\n","\n")` corrupts any literal backslash-n. The `\n` interpretation should happen while parsing the `y` operands. Fix: handle escapes at parse time.
- [ ] **#10 — `\n` in `s///` replacement not handled.** `text/sed.rs:1694-1734`. `update_pattern_space` strips all backslashes (`replace("\\","")`) and never converts `\`-newline into an embedded newline. Fix: convert escaped newline to a real newline in the replacement.

#### Minor

- [ ] **#11 — `c` with a 2-address range: per-line delete-and-restart for >2-line ranges questionable.** `text/sed.rs:2052-2089`.
- [ ] **#12 — address `0` rejected unconditionally** (`text/sed.rs:844-851`), so the common `0,/re/` form is unavailable (POSIX does not define `0,/re/`, so informational).
- [ ] **#13 — `a`/`i`/`c` multi-line POSIX text form unsupported.** `text/sed.rs:925-964` reads only to the first `\n` after the `\`.
- [ ] **#14 — `b`/`t` label parsing truncates at `#`.** `text/sed.rs:969-992`; `filter_comments` strips after `#`, so `b label#` jumps to `:label`.
- [ ] **#15 — relative `w`/`s///w` paths redirected into `CARGO_TARGET_TMPDIR`.** `text/sed.rs:1374-1381,2186-2195`. A production-correctness defect: `w output.txt` writes to the tmp dir, not the CWD. Fix: write relative wfile paths relative to the CWD.
- [ ] **#16 — global duplicate-label check rejects same-named labels in different scopes.** `text/sed.rs:1576-1614`.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Item | Status | Notes |
|---|---|---|
| `sed [-En] script [file...]` | CONFORMS | first non-option arg = script |
| `-e`/`-f` repeatable, order preserved | CONFORMS | `get_raw_script` |
| `--` operand separator | PARTIAL | clap handles; manual loop effect same |
| `-` as stdin file | CONFORMS | `sed.rs:2418` |

#### OPTIONS

| Option | Status | Notes |
|---|---|---|
| `-E` (ERE) | DIVERGES | flag set too late (#2) |
| `-r` (GNU alias) | MISSING | not POSIX; note only |
| `-e`/`-f` | CONFORMS | |
| `-n` suppress auto-print | PARTIAL | wrongly suppresses `=` (#6) |
| `-s`/`-i` (GNU) | N/A | not POSIX |

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| `file...` in order; concatenated line numbering | CONFORMS | |
| no files → stdin | CONFORMS | `sed.rs:106-108` |
| `-` → stdin | CONFORMS | `sed.rs:2418` |
| script as first operand | CONFORMS | |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL` | CONFORMS | `setlocale` |
| `LC_COLLATE`/`LC_CTYPE` | PARTIAL | regex may handle; not explicitly set |
| `LC_MESSAGES` | PARTIAL | gettext stub |

#### ASYNCHRONOUS EVENTS

- [x] Default — CONFORMS.

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| edited input to stdout | CONFORMS | |
| `-n` suppresses auto-print | CONFORMS | |
| `=` writes regardless of `-n` | DIVERGES | (#6) |
| diagnostics to stderr | CONFORMS | |

#### EXTENDED DESCRIPTION — addresses

| Address | Status | Notes |
|---|---|---|
| line number `N` / `$` / `/BRE/` / `\cBREc` | CONFORMS | |
| `addr1,addr2` range | CONFORMS | |
| `0,/re/` | DIVERGES | address 0 rejected (#12) |
| `!` negation | CONFORMS | |
| `first~step` (GNU) | MISSING | not POSIX |

#### EXTENDED DESCRIPTION — commands

| Command | Status | Notes |
|---|---|---|
| `{ }` | CONFORMS | |
| `a\` | DIVERGES | into pattern space (#3) |
| `b`/`t`/`T` | CONFORMS / CONFORMS / N/A | |
| `c\` | PARTIAL | range delete-without-print (#11) |
| `d` | CONFORMS | |
| `D` | DIVERGES | clears hold space (#4) |
| `g`/`G`/`h`/`H`/`x` | CONFORMS | |
| `i\` | CONFORMS | single-line only (#13) |
| `l` | MISSING | `I` used; `l` rejected (#1) |
| `n`/`N`/`p`/`P` | CONFORMS | |
| `q` | CONFORMS | |
| `r` | DIVERGES | into pattern space (#7) |
| `s///` | PARTIAL | no `i` flag (#5); `\n` in repl broken (#10) |
| `s///g`/`N`/`p` | CONFORMS | |
| `s///w` | PARTIAL | wfile not pre-created (#8) |
| `w` | PARTIAL | relative path redirected (#15); no pre-create (#8) |
| `y///` | DIVERGES | post-processes `\\n` (#9) |
| `:label` / `#` / `#n` | CONFORMS | |
| `=` | DIVERGES | suppressed by `-n` (#6) |

#### EXTENDED DESCRIPTION — BRE vs ERE

| Item | Status | Notes |
|---|---|---|
| BRE default / ERE with `-E` | DIVERGES | ERE flag too late (#2) |
| empty RE reuses last | CONFORMS | |
| `&` / `\1`-`\9` in replacement | CONFORMS | |
| `\n` in replacement splits line | DIVERGES | (#10) |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Item | Status | Notes |
|---|---|---|
| 0 success / >0 error | CONFORMS | |
| file-open error → stderr, continue | CONFORMS | `sed.rs:2426-2429` |

### Test coverage signal

Not covered:
- [ ] `l` command (only `I` tested) (#1)
- [ ] `s///i` (zero tests) (#5)
- [ ] multi-line `a\`/`i\`/`c\` text form (#13)
- [ ] wfile pre-creation (#8)
- [ ] `r` deferred output (#7)
- [ ] `\n` in `s///` replacement to split a line (#10)
- [ ] `=` output under `-n` (#6)
- [ ] `-E` mode with patterns differing from BRE (#2)
- [ ] `y///` with `\n` in operands (#9)
- [ ] `D` interaction with hold space (#4)

### Suggested PR groupings

- **PR A — "ERE flag timing"**: #2.
- **PR B — "Implement POSIX `l`"**: #1.
- **PR C — "Deferred output for `a`/`r`"**: #3, #7.
- **PR D — "`D` hold-space + `=` under `-n`"**: #4, #6.
- **PR E — "`s///i` + `\n` replacement + `y///` escapes"**: #5, #9, #10.
- **PR F — "wfile pre-create + relative-path fix"**: #8, #15.

---

## `sort`

**Implementation:** `text/sort.rs` (1011 lines)
**Tests:** `text/tests/sort/mod.rs` (616 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/sort.md`
**Date:** 2026-06-25

### TL;DR

The scaffolding (option parsing, `-c/-C/-m/-o/-u/-b/-d/-f/-i/-n/-r/-t/-k`) is present but the algorithmic core has deep defects. Default sort uses Rust byte-order `str::cmp` instead of `strcoll` locale collation. `-n` uses `f64` with a custom tokenizer that accepts `*`, skips leading-blank stripping, and truncates large integers. `-m` does not merge — it concatenates. `-u` uses a side-channel `duplicates` Vec only populated for exactly two `-k` keys. Global `-b`/`-t` (without `-k`) are wrongly rejected as errors. Keys beyond the second are silently ignored. The default field separator splits only on ASCII space, missing tab.

### Priority issues

#### Critical

- [ ] **#1 — No `strcoll`/locale collation — default sort is wrong-order.** `text/sort.rs:579-587,643-652`. `compare_lines`/`compare_key` use `str::cmp` / `to_uppercase().cmp()`. Spec requires the current locale's collating sequence (`strcoll`). Wrong in any non-C locale. Fix: `libc::strcoll` + whole-line byte tiebreak.
- [ ] **#2 — `-m` does not merge — it concatenates.** `text/sort.rs:884-898`. `merge_files` `io::copy`s each reader sequentially with no interleave. Presorted inputs come out unordered. Fix: k-way merge using the comparator.
- [ ] **#3 — `-u` unique logic broken for no-key and 3+-key cases.** `text/sort.rs:784-818`. The `duplicates` Vec relies on the comparator being called on adjacent equal pairs (not guaranteed post-sort), and ≥3 keys are ignored so dedup only fires when the first two keys are equal. Fix: compare adjacent lines after sorting.
- [ ] **#4 — `-C` emits a diagnostic to stderr.** `text/sort.rs:825-832,996`. Spec: `-C` is like `-c` but with no stderr warning; test `test_02e` asserts non-empty stderr. Fix: suppress stderr for `-C`.

#### Major

- [ ] **#5 — `-b` without `-k` rejected as an error.** `text/sort.rs:97-99`. POSIX allows global `-b`; the impl errors "Options '-b' can be used together with '-k'". Fix: apply global `-b` to all keys incl. the implicit whole-line key.
- [ ] **#6 — `-t` without `-k` rejected as an error.** `text/sort.rs:101-103`. POSIX allows global `-t char`.
- [ ] **#7 — `-n` uses `f64`; wrong tokenizer.** `text/sort.rs:287-334`. Accepts `*` (line 292); no leading-blank strip; `f64` loses precision >2^53; ignores `LC_NUMERIC` radix/thousands. Fix: integer/arbitrary-precision compare + locale-aware tokenizer.
- [ ] **#8 — Only two `-k` keys processed; the rest silently ignored.** `text/sort.rs:771-795`. POSIX requires ≥9 `-k` occurrences. Fix: iterate all keys.
- [ ] **#9 — No whole-line tiebreak when all specified keys compare equal.** `text/sort.rs:784-796`. Spec requires a final whole-line comparison (with `-r` still in effect).
- [ ] **#10 — `-c`/`-C` check uses sort-then-diff, not a sequential scan.** `text/sort.rs:825-843`. `find_first_difference` compares sorted vs original by index, not the first out-of-order input line.
- [ ] **#11 — Inconsistent trailing-newline handling between stdout (`println!`+`join`) and `-o`/`-m` (`writeln!`) paths.** `text/sort.rs:851-858`. Agrees for well-formed input but is fragile.

#### Minor

- [ ] **#12 — `-i` filter is ASCII-only, not LC_CTYPE.** `text/sort.rs:369-373`. `is_ascii_graphic()` excludes printable non-ASCII and strips space/tab.
- [ ] **#13 — `-d` uses Unicode `is_whitespace()`/`is_alphanumeric()`, not LC_CTYPE.** `text/sort.rs:350-354`.
- [ ] **#14 — Default blank separator splits only on ASCII space, not tab.** `text/sort.rs:511`. `<blank>` includes horizontal tab.
- [ ] **#15 — `update_range_field` merges modifiers across start/end.** `text/sort.rs:154-176`. A modifier on one half propagates to both.
- [ ] **#16–#18 — `-d`/`-i`, `-d`/`-n`, `-n`/`-i` rejected as mutually exclusive** (`text/sort.rs:82-94`) though POSIX does not declare them so (over-restrictive).
- [ ] **#19 — `usize::MAX - 1` magic sentinel for end-of-field.** `text/sort.rs:247-252,457-464` (fragile).
- [ ] **#20 — `-o`/`-m` does not guard same-file overwrite during read.** `text/sort.rs:847-855`. Under `-m`, `File::create` truncates an input that is also the output before reading.
- [x] **#21 — `-` operand only honored as the sole argument.** `text/sort.rs:955-956`. FIXED (Phase 2): each operand is opened via `plib::io::input_stream_dashed`, so `-` reads stdin at any position.
- [ ] **#22 — All errors exit 1, not >1.** `text/sort.rs:995`. Spec reserves >1 for errors, 1 for `-c`/`-C` disorder.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Item | Status | Notes |
|---|---|---|
| sort synopsis (full) | PARTIAL | clap parses all flags; `-k` Vec works |
| check synopsis (`-c`/`-C`) | PARTIAL | accepted; `-c` with >1 file accepted (spec shows one) |
| `+`/`+number`/`-number` obsolescent | N/A | removed in POSIX.2024 |
| `--` end-of-options | CONFORMS | clap |

#### OPTIONS

| Option | Status | Notes |
|---|---|---|
| `-c` | PARTIAL | sort-then-diff check (#10) |
| `-C` | DIVERGES | emits stderr (#4) |
| `-m` | DIVERGES | concatenates (#2) |
| `-o` | PARTIAL | unsafe under `-m` same-file (#20) |
| `-u` | PARTIAL | wrong for 0/3+ keys (#3) |
| `-d` | PARTIAL | Unicode not LC_CTYPE (#13); false mutual-exclusion (#16) |
| `-f` | PARTIAL | Rust `to_uppercase`, not `toupper` |
| `-i` | PARTIAL | ASCII-only (#12) |
| `-n` | PARTIAL | f64 + bad tokenizer (#7) |
| `-b` | DIVERGES | rejected without `-k` (#5) |
| `-r` | PARTIAL | post-sort `reverse()`; per-key `r` for keys 1-2 only |
| `-t char` | DIVERGES | rejected without `-k` (#6) |
| `-k keydef` | PARTIAL | only first two keys (#8); no tiebreak (#9) |
| `-M`/`-g`/`-h`/`-R`/`-z`/`-T`/`-S` | N/A | not POSIX.2024 |

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| no operands → stdin | CONFORMS | `sort.rs:957` |
| `-` → stdin | PARTIAL | sole-argument only (#21) |
| multiple file operands | CONFORMS | lines 961-965 |
| incomplete last line → add newline | CONFORMS | |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL` | CONFORMS | `setlocale` |
| `LC_COLLATE` | MISSING | byte order, no strcoll (#1) |
| `LC_CTYPE` | MISSING | Rust/Unicode classifiers (#12,#13) |
| `LC_NUMERIC` | MISSING | f64 ignores locale (#7) |
| `LC_MESSAGES` | PARTIAL | hardcoded English |
| `TMPDIR` | N/A | all in-memory |

#### ASYNCHRONOUS EVENTS

- [x] Default — CONFORMS (no temp files).

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| sorted output to stdout | CONFORMS | |
| no stdout under `-o`/`-c`/`-C` | CONFORMS | |
| stderr for `-c` disorder | CONFORMS | line 996 |
| stderr silent for `-C` | DIVERGES | (#4) |

#### EXTENDED DESCRIPTION

| Item | Status | Notes |
|---|---|---|
| default key = whole line | CONFORMS | |
| current-locale collation | MISSING | (#1) |
| equal-collating byte tiebreak | MISSING | |
| `-k field.char[mods]` | PARTIAL | keys 1-2; ≥3 ignored (#8) |
| 1-based field numbering | CONFORMS | |
| `last_character==0` → end of field | CONFORMS | |
| missing `field_end` → last char of line | CONFORMS | |
| default separator blank-transition | DIVERGES | ASCII space only (#14) |
| `-t` each occurrence significant | CONFORMS | |
| `-b` per-field vs global | PARTIAL | global rejected (#5) |
| whole-line tiebreak when all keys equal | MISSING | (#9) |
| `-u` keeps one of equal keys | PARTIAL | (#3) |
| ≥9 `-k` supported | MISSING | (#8) |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Code | Status | Notes |
|---|---|---|
| 0 success / `-c` sorted | CONFORMS | |
| 1 `-c`/`-C` disorder | CONFORMS | |
| >1 error | DIVERGES | all errors exit 1 (#22) |

#### Cross-cutting (collation, i18n, locale, numeric locale)

`setlocale` is called but `strcoll` is never used; numeric/character classification is Rust/Unicode throughout. The largest correctness risk is collation (#1) compounded by no whole-line tiebreak (#9).

### Test coverage signal

Not covered:
- [ ] `sort -b` / `sort -t :` global (without `-k`) — should succeed (#5,#6)
- [ ] three or more `-k` keys (#8)
- [ ] locale collation via strcoll (#1)
- [ ] `-m` interleaved merge (#2)
- [ ] numeric sort with locale thousands/radix; large integers >2^53 (#7)
- [ ] `-C` silence (#4)
- [ ] whole-line tiebreak (#9)
- [ ] `-` as non-sole operand (#21)
- [ ] `-o` same as input file safety (#20)
- [ ] exit code >1 on I/O error (#22)

### Suggested PR groupings

- **PR A — "Locale collation + whole-line tiebreak"**: #1, #9.
- **PR B — "True `-m` merge"**: #2.
- **PR C — "`-u` correctness + ≥9 keys"**: #3, #8.
- **PR D — "`-C` silence + exit-code >1"**: #4, #22.
- **PR E — "Global `-b`/`-t` + default separator"**: #5, #6, #14.
- **PR F — "`-n` numeric rewrite"**: #7.
- **PR G — "LC_CTYPE classifiers + `-` operand + same-file guard"**: #12, #13, #20, #21.

---

## `tail`

**Implementation:** `text/tail.rs` (507 lines)
**Tests:** `text/tests/tail/mod.rs` (249 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/tail.md`
**Date:** 2026-06-25

### TL;DR

Core byte/line-from-end and byte/line-from-start semantics are correct and well-tested. Three conformance gaps: (1) `-r` (a POSIX.1-2024 Issue 8 mandatory option) is entirely absent; (2) `-f` is uncertain/ineffective for a named FIFO operand; (3) `tail -n +0` emits all lines instead of treating line zero as non-conforming. The `-f` follow uses `notify_debouncer_full` (inotify/FSEvents) rather than the spec's sleep-poll loop, introducing platform-dependent behavior and the FIFO gap.

### Priority issues

#### Critical

- [ ] **#1 — `-r` option entirely missing (POSIX.1-2024 Issue 8).** `Args` (`text/tail.rs:92`) has no `-r` field and no reverse logic exists. `tail -r` is silently ignored or errors. Fix: add `-r` + reverse-order output.
- [ ] **#2 — `-f` uncertain/ineffective for a FIFO file operand.** `text/tail.rs:418-477,434`. Spec: for a regular file or a FIFO operand, do not terminate after the last line. `notify` watching a FIFO is not guaranteed to deliver `Modify` events, so `tail -f /path/to/fifo` may produce no output after the initial drain. Fix: detect FIFO via `is_fifo()` and fall back to a sleep-poll loop.

#### Major

- [ ] **#3 — `-n +0` emits all lines — spec calls `+0` non-conforming.** `text/tail.rs:225`. `us == 0` is clamped to `1` for `StartOfFile`, dumping the whole file; test `test_tail_18` cements this. Fix: reject or produce empty output for line zero.
- [ ] **#4 — `-f` exits when the followed file is removed.** `text/tail.rs:465`. On `Remove(File)` it `unwatch`es and the channel drains, terminating the process; users expect `tail -f` to keep waiting (log rotation). Fix: block/retry rather than exit.
- [ ] **#5 — Error diagnostics bypass gettext / `plib::diag`.** `text/tail.rs:449,503`. Hardcoded English strings (`"tail: {}: file truncated"`, `"tail: {}"`). Fix: route through gettext.

#### Minor

- [ ] **#6 — `allow_hyphen_values = true` on `-n`/`-c` may swallow `--`.** `text/tail.rs:93-99`. `tail -n -- file` could parse `--` as the number. Fix: verify `--` still ends options; add a regression test.
- [ ] **#7 — `print_n_bytes` short-read accumulation bug.** `text/tail.rs:292-323`. The `break` at line 318 fires on any `bytes_read < buffer_1.len()`, not only at EOF; a short read on a slow pipe terminates accumulation early and yields a wrong result. Fix: accumulate until a zero-byte read.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Item | Status | Notes |
|---|---|---|
| `tail [-f] [-c number\|-n number] [file]` | CONFORMS | mutually-exclusive `-c`/`-n` (line 118-121) |
| `tail -r [-n number] [file]` | MISSING | `-r` absent (#1) |
| `+` as option delimiter | N/A | obsolescent forms removed |
| `--` end-of-options | PARTIAL | `allow_hyphen_values` may interfere (#6) |
| single `[file]` operand | CONFORMS | `Option<PathBuf>` (line 105) |

#### OPTIONS

| Option | Status | Notes |
|---|---|---|
| `-c number` (last N bytes) | CONFORMS | lines 283-323 |
| `-c -N` | CONFORMS | line 65 |
| `-c +N` (from start, 1-based) | CONFORMS | lines 325-360 |
| `-c +1` (entire file) | CONFORMS | |
| `-n number` (last N lines) | CONFORMS | lines 158-221 |
| `-n -N` | CONFORMS | |
| `-n +N` (from start) | CONFORMS | |
| `-n +0` | DIVERGES | prints all lines (#3) |
| default `-n 10` | CONFORMS | line 126 |
| `-f` regular file | CONFORMS | lines 418-477 |
| `-f` stdin pipe/FIFO ignored | CONFORMS | `if let File` falls through for stdin |
| `-f` named FIFO operand | PARTIAL | notify may not work on FIFOs (#2) |
| `-f` exits on removal | DIVERGES | (#4) |
| `-r` | MISSING | (#1) |

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| no operand → stdin | CONFORMS | line 400 |
| `-` operand → stdin | CONFORMS | line 392 |
| single `[file]` operand | CONFORMS | line 395 |
| multiple file operands | N/A | synopsis shows single `[file]` |
| `==> name <==` headers | N/A | single-file synopsis |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL`/`LC_CTYPE` | CONFORMS | `setlocale` line 483; byte-level splitting locale-neutral |
| `LC_MESSAGES` | PARTIAL | error strings not gettext-wrapped (#5) |
| `NLSPATH` | CONFORMS | |

#### ASYNCHRONOUS EVENTS

- [x] Default — CONFORMS.

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| portion to stdout | CONFORMS | `write_all` |
| stderr for diagnostics only | CONFORMS | |
| diagnostic strings localized | PARTIAL | hardcoded English (#5) |

#### EXTENDED DESCRIPTION

Spec section is "None." (all semantics in OPTIONS). The `-f` mechanism uses `notify_debouncer_full` (within spec latitude for regular files); a 1 ms debounce may coalesce rapid writes, and `events.first().unwrap()` (line 439) would panic on an empty event list.

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Item | Status | Notes |
|---|---|---|
| exit 0 success | CONFORMS | line 498 |
| exit >0 error | CONFORMS | line 501 |
| `-c` and `-n` together → error | CONFORMS | lines 118-121 |

#### Cross-cutting (i18n, locale, follow mechanism)

notify-based `-f` is portable for regular files but uncertain for FIFOs (#2); `events.first().unwrap()` should be bounds-checked; diagnostics need gettext (#5).

### Test coverage signal

Not covered:
- [ ] `-r` option (#1)
- [ ] `-f` follow behavior (no integration test); FIFO operand (#2); stdin FIFO ignored
- [ ] file truncation during `-f`; file removal during `-f` (#4)
- [ ] `-n +0` (#3)
- [ ] `--` end-of-options with file operand (#6)
- [ ] `-c +1` (entire file from byte 1)
- [ ] error exit code/message when file does not exist
- [ ] LC_MESSAGES influence on diagnostics (#5)

### Suggested PR groupings

- **PR A — "Implement `-r`"**: #1.
- **PR B — "`-f` FIFO + removal handling"**: #2, #4.
- **PR C — "`-n +0` + short-read accumulation"**: #3, #7.
- **PR D — "gettext diagnostics + `--` handling"**: #5, #6.

---

## `tr`

**Implementation:** `text/tr.rs` (2484 lines)
**Tests:** `text/tests/tr/mod.rs` (795 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/tr.md`
**Date:** 2026-06-25

### TL;DR

Large and well-structured, covering all four modes (translate, delete, squeeze, delete+squeeze), all backslash escapes, `[:class:]`, `[=equiv=]`, `[c*n]`/`[c*]`, ranges, NUL, multibyte UTF-8, and complement. Primary gaps: (1) `-c` and `-C` are treated identically despite different semantics; (2) character classes are hardcoded ASCII, ignoring LC_CTYPE; (3) class names other than `[:lower:]`/`[:upper:]` are not rejected in string2 during translate mode; (4) `[=equiv=]` expands to only the literal character; (5) ranges ignore LC_COLLATE in non-POSIX locales. None are crash-level; (2)/(3) affect correctness in non-C locales.

### Priority issues

#### Critical

- none.

#### Major

- [ ] **#1 — `-c` and `-C` treated identically.** `text/tr.rs:103` (`complement = args.complement_char || args.complement_val`). Spec distinguishes `-c` (complement byte values, binary order) from `-C` (complement characters per LC_CTYPE, LC_COLLATE order). A `// TODO` at lines 101-102 acknowledges this. Fix: restrict `-c` to byte values 0-255; make `-C` enumerate LC_CTYPE characters.
- [ ] **#2 — Character classes are ASCII-only, ignoring LC_CTYPE.** `text/tr.rs:958-993`. All 12 classes expand to hardcoded ASCII ranges; spec requires LC_CTYPE membership (`[:alpha:]` should include `é` etc. in a UTF-8 locale). Fix: use libc `isalpha`/`iswctype`.
- [ ] **#3 — Class names other than `[:lower:]`/`[:upper:]` not rejected in string2 (translate mode).** Spec: only `lower`/`upper` are valid in string2 when not in `-ds` mode. The impl accepts `[:digit:]` etc. in string2 silently. Fix: validate in `generate_for_translation`.

#### Minor

- [ ] **#4 — `[=equiv=]` does not expand the full equivalence class.** `text/tr.rs:537-581`. Stores only the literal character; spec wants all LC_COLLATE-equivalent members. (Blocked without LC_COLLATE data.)
- [ ] **#5 — Ranges use Unicode code-point order, not LC_COLLATE.** `text/tr.rs:885-899`. Correct in POSIX/C locale; wrong for locales whose collation differs from code-point order.
- [ ] **#6 — `[c*]` in string1 rejection is correct but untested.** `text/tr.rs:1434-1438`.
- [ ] **#7 — `\` + non-octal/non-special char silently treated as the char** (`text/tr.rs:804-810`); within the spec's "unspecified" latitude but locked in by a test.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Requirement | Status | Notes |
|---|---|---|
| all four mode synopses | CONFORMS | clap; validated lines 50-63, 243-261 |
| `--` end-of-options | CONFORMS | clap |
| reject `-c`+`-C` together | CONFORMS | line 50-52 |

#### OPTIONS

| Option | Status | Notes |
|---|---|---|
| `-c` complement values (binary order) | PARTIAL | conflated with `-C` (#1) |
| `-C` complement chars (collation order) | PARTIAL | conflated with `-c` (#1) |
| `-d` delete | CONFORMS | line 2085 |
| `-s` squeeze | CONFORMS | |

#### OPERANDS / STDIN / INPUT FILES

| Requirement | Status | Notes |
|---|---|---|
| string1/string2 control strings | CONFORMS | line 409 |
| reads stdin / writes stdout (filter) | CONFORMS | lines 2036, 2059 |
| NUL processed, not stripped | CONFORMS | byte-level |
| string2 shorter than string1 → last char repeated | CONFORMS | lines 1252-1274 (allowed) |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL` | PARTIAL | `setlocale` line 296; not used functionally for classes |
| `LC_COLLATE` | MISSING | ranges + `-C` ordering ignore it (#1,#5) |
| `LC_CTYPE` | MISSING | class membership hardcoded ASCII (#2) |
| `LC_MESSAGES` | CONFORMS | gettextrs lines 297-298 |
| `NLSPATH` | N/A | XSI |

#### ASYNCHRONOUS EVENTS

- [x] Default — CONFORMS.

#### STDOUT / STDERR

| Requirement | Status | Notes |
|---|---|---|
| transformed input to stdout | CONFORMS | line 2059 |
| diagnostics to stderr only | CONFORMS | |
| octal-overflow warning | CONFORMS | lines 745-748 |

#### EXTENDED DESCRIPTION

| Construct | Status | Notes |
|---|---|---|
| `\octal` (1-3 digits, longest match) | CONFORMS | lines 663-761 |
| `\\ \a \b \f \n \r \t \v` | CONFORMS | lines 778-799 |
| `\other` / trailing `\` (unspecified) | CONFORMS | within latitude (#7) |
| `c-c` range (POSIX-locale order) | CONFORMS | lines 854-899; reversed range error |
| range LC_COLLATE order | MISSING | code-point order (#5) |
| `[:class:]` (12 classes) | PARTIAL | ASCII-only (#2) |
| unknown / empty class name → error | CONFORMS | lines 985-991 |
| `[:lower:]`/`[:upper:]` only valid in string2 | MISSING | not enforced (#3) |
| case conversion via locale toupper/tolower | MISSING | hardcoded ASCII pairs |
| `[=equiv=]` syntax / errors | CONFORMS | parsed; `[==]`/multi-char rejected |
| `[=equiv=]` full LC_COLLATE expansion | MISSING | literal only (#4) |
| `[c*n]` / `[c*0]` / `[c*]` | CONFORMS | lines 612-650; rejected in string1 |
| translate / `-d` / `-s` / `-ds` modes | CONFORMS | duplicate-in-string1 uses last mapping |
| `-c -d -s` complement applies to delete set | CONFORMS | line 256 |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Requirement | Status | Notes |
|---|---|---|
| exit 0 success / >0 error | CONFORMS | lines 305, 311 |
| errors to stderr | CONFORMS | |

#### Cross-cutting (i18n, locale, collation, character classes)

`setlocale` is called but not used for class membership, range collation, equivalence expansion, or `-C` ordering — the utility effectively always operates in the C/POSIX locale for these.

### Test coverage signal

Thorough for ASCII. Not covered:
- [ ] locale-aware class expansion (any non-C locale) (#2)
- [ ] `[:lower:]`/`[:upper:]` case conversion in non-ASCII locale
- [ ] rejection of `[:alpha:]` etc. in string2 (translate mode) (#3)
- [ ] LC_COLLATE range ordering (#5)
- [ ] `[=e=]` expanding to multiple members (#4)
- [ ] `-C` vs `-c` distinguishing behavior (#1)
- [ ] `[c*]` in string1 rejection (#6)
- [ ] empty string1/string2 (undefined per spec)

### Suggested PR groupings

- **PR A — "Reject non-lower/upper classes in string2"**: #3.
- **PR B — "Locale-aware character classes"**: #2.
- **PR C — "Distinguish `-c` from `-C`"**: #1.
- **PR D — "`[=equiv=]` + range collation (pending LC_COLLATE)"**: #4, #5.

---

## `tsort`

**Implementation:** `text/tsort.rs` (179 lines)
**Tests:** `text/tests/tsort/mod.rs` (317 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/tsort.md`
**Date:** 2026-06-25

### TL;DR

The core topological sort and output format are correct. Three gaps: the `-w` option (mandatory since POSIX.1-2024) is entirely absent; cycles always exit 0 instead of non-zero; and the `-` operand is not treated as stdin. Cycle reporting also lacks path reconstruction.

### Priority issues

#### Critical

- [ ] **#1 — `-w` option completely absent.** `text/tsort.rs:19-24` (`Args`). POSIX.1-2024 synopsis is `tsort [-w] [file]`; with `-w`, exit status = cycle count (capped). Neither the option nor counting exists; `tsort -w` errors out via clap. Fix: add `-w` + cycle counting.
- [ ] **#2 — Cycle detection does not set non-zero exit status.** `text/tsort.rs:139-151,170-177`. Spec: "If a diagnostic message is written, the final exit status shall be non-zero." `tsort_file` always returns `Ok(())`; tests at lines 233, 264 assert exit 0 for cycles. Fix: return non-zero on any cycle.

#### Major

- [x] **#3 — `-` operand not treated as stdin.** `text/tsort.rs:102`. FIXED (Phase 2): opens via `plib::io::input_stream_dashed`; `tsort -` reads stdin.

#### Minor

- [ ] **#4 — Odd-token input emits a warning but exits 0.** `text/tsort.rs:125-128`. Spec puts the pairing obligation on the application; warning-without-error is permissible but inconsistent.
- [ ] **#5 — Cycle reporting lists members, not cycle paths.** `text/tsort.rs:139-151`. Multiple independent cycles are dumped together under one header.
- [ ] **#6 — Cycle nodes emitted to both stderr and stdout** (`text/tsort.rs:142-151`, "match macOS behavior"); not prohibited but the comment indicates a non-POSIX extension.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Item | Status | Notes |
|---|---|---|
| `tsort` no args → stdin | CONFORMS | |
| `tsort [file]` | CONFORMS | |
| `-w` accepted | MISSING | (#1) |
| `--` end-of-options | CONFORMS | clap |

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| no operand → stdin | CONFORMS | |
| named file operand | CONFORMS | |
| `-` → stdin | DIVERGES | literal filename (#3) |
| input is text file | CONFORMS | |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL`/`LC_CTYPE` | CONFORMS | `setlocale` line 164 |
| `LC_MESSAGES` | PARTIAL | gettextrs; cycle line not gettext-wrapped (line 144) |
| `NLSPATH` | N/A | |

#### ASYNCHRONOUS EVENTS

- [x] Default — CONFORMS.

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| sorted items to stdout, one per line | CONFORMS | line 135 |
| cycle diagnostic to stderr | CONFORMS | lines 140, 144 |
| cycle nodes also to stdout | DIVERGES (minor) | lines 148-150 (#6) |

#### EXTENDED DESCRIPTION

| Item | Status | Notes |
|---|---|---|
| tokens separated by blanks/newlines | CONFORMS | line 109 |
| pair of different items → ordering | CONFORMS | line 118 |
| identical-pair → presence only | CONFORMS | line 115 |
| build partial → emit total order | CONFORMS | Kahn's, lines 63-98 |
| cycle: report to stderr + continue | CONFORMS | |
| cycle: non-zero exit | DIVERGES | (#2) |
| `-w`: exit = cycle count | MISSING | (#1) |
| unconstrained-node order impl-defined | CONFORMS | BTreeMap lexicographic |
| POSIX example | CONFORMS | `test_tsort_posix_example` |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Condition | Expected | Actual | Status |
|---|---|---|---|
| success, no cycles | 0 | 0 | CONFORMS |
| I/O error | >0 | 1 | CONFORMS |
| cycle (no `-w`) | >0 | 0 | DIVERGES (#2) |
| `-w`, N cycles | N | — | MISSING (#1) |

#### Cross-cutting (i18n, locale)

`setlocale` + textdomain + gettext present; cycle-member line `"tsort: {}"` (line 144) not gettext-wrapped.

### Test coverage signal

Not covered:
- [ ] `-w` option (#1)
- [ ] `-` operand as stdin (#3)
- [ ] non-zero exit on cycle (tests assert exit 0) (#2)
- [ ] cycle count under `-w`
- [ ] multiple independent cycles → separate reports (#5)
- [ ] `--` followed by a `-`-prefixed file operand

### Suggested PR groupings

- **PR A — "`-w` + non-zero exit on cycle"**: #1, #2.
- **PR B — "`-` operand → stdin"**: #3.
- **PR C — "Cycle-path reconstruction + i18n"**: #5, #6.

---

## `unexpand`

**Implementation:** `text/unexpand.rs` (191 lines)
**Tests:** `text/tests/unexpand/mod.rs` (75 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/unexpand.md`
**Date:** 2026-06-25

### TL;DR

Two Critical and three Major non-conformances. Most damaging: (1) `-t tablist` does not imply all-blanks conversion as POSIX requires, and (2) repeating tab stops are not implemented — only a single explicit stop is consumed, so any line with more leading spaces than the first stop produces wrong output. The `-a` path (`convert_all_blanks`) is architecturally broken: it discards inter-word structure, ignores column position, and uses only `tablist[0]`. No backspace handling; multibyte width ignored. `test_2` encodes an incorrect expected value that hides the repeating-stop bug.

### Priority issues

#### Critical

- [ ] **#1 — Repeating tab stops not implemented.** `text/unexpand.rs:85-97`. `for &tabstop in tablist` iterates each explicit stop once; for default `[8]` only one pass runs, so 16 leading spaces produce `\t` + 8 spaces instead of `\t\t`. Default is "equivalent to `-t 8`" → stops repeat every 8 columns. `test_2`'s 16-space ("Date") expectation encodes the wrong answer, masking the bug. Fix: treat a single-entry tablist as a repeating period.
- [ ] **#2 — `-t` does not imply all-blanks conversion.** `text/unexpand.rs:45-49,57-61`. Dispatch `if args.all_spaces && args.tablist.is_none()`; with `-t` and no `-a`, this is false → wrong function. Spec: with `-t`, conversion is not limited to leading blanks. Fix: condition `args.all_spaces || args.tablist.is_some()`.

#### Major

- [ ] **#3 — `convert_all_blanks` loses column position and structure.** `text/unexpand.rs:107-142`. `split_whitespaces` counts spaces from position 0 of each run, not the actual column; tab-stop alignment is relative to the run, not the line. Fix: single-pass column-tracking conversion.
- [ ] **#4 — `convert_all_blanks` ignores a multi-stop tablist.** `text/unexpand.rs:138` uses `tablist[0]` only.
- [ ] **#5 — Backspace column-decrement not implemented.** No `'\x08'` handling anywhere; spec requires backspace to be copied and decrement the column (min 1).

#### Minor

- [ ] **#6 — `parse_tablist` splits only on comma, not blank.** `text/unexpand.rs:30`. Spec allows "blank or comma".
- [ ] **#7 — `split_whitespaces` uses `is_whitespace()` not blank-only.** `text/unexpand.rs:113`. Matches `\n`/`\r`/`\f`/`\v`/Unicode spaces; POSIX blank is space+tab.
- [x] **#8 — `-` operand only honored as the sole file argument.** `text/unexpand.rs:40`. FIXED (Phase 2): operands are processed in order via `plib::io::input_stream_dashed`, so `-` reads stdin at any position.
- [ ] **#9 — Leading tab characters not treated as blanks for column tracking.** `text/unexpand.rs:70-105` stops collection at the first non-space.
- [ ] **#10 — Multibyte / wide-character column width ignored.** No `unicode-width`/`wcwidth`.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Item | Status | Notes |
|---|---|---|
| `unexpand [-a\|-t tablist] [file...]` | PARTIAL | `-a`/`-t` accepted; mutual exclusion not enforced (behavior diverges anyway) |
| `--` end-of-options | CONFORMS | clap |
| `-` as stdin | PARTIAL | sole-argument only (#8) |
| space-separated tablist | MISSING | comma only (#6) |

#### OPTIONS

| Option | Status | Notes |
|---|---|---|
| `-a` convert all blanks ≥2 before tab stop | PARTIAL | wrong column arithmetic (#3,#4) |
| `-t tablist` sets stops | PARTIAL | comma-only parse; repeating broken (#1); no `-a` implication (#2) |
| `-t` implies `-a` | MISSING | (#2) |
| `-t N` repeats every N cols | MISSING | (#1) |
| multi-number `-t` | PARTIAL | leading path iterates; all-blanks ignores all but first (#4) |
| no conversion beyond last stop | CONFORMS | remaining spaces literal |

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| `file` pathname | CONFORMS | |
| multiple files concatenated | CONFORMS | |
| no files → stdin | CONFORMS | |
| `-` → stdin | PARTIAL | sole-argument only (#8) |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL` | CONFORMS | `setlocale` line 176 |
| `LC_CTYPE` (char width) | MISSING | 1 col/char (#10) |
| `LC_MESSAGES` | CONFORMS | textdomain + gettext |
| `NLSPATH` | N/A | XSI |

#### ASYNCHRONOUS EVENTS

- [x] Default — CONFORMS.

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| output = input with conversions | PARTIAL | structural bugs |
| diagnostics to stderr only | CONFORMS | line 186 |

#### EXTENDED DESCRIPTION

Spec section "None" — algorithm is in DESCRIPTION/OPTIONS.

| Requirement | Status | Notes |
|---|---|---|
| default: leading blanks → tabs | PARTIAL | leading tabs not handled as blanks (#9) |
| default 8-col repeating stops | MISSING | (#1) |
| `-a`: ≥2 blanks before a stop | PARTIAL | wrong arithmetic (#3) |
| `-t`: disable leading-only restriction | MISSING | (#2) |
| backspace decrements column (min 1) | MISSING | (#5) |
| existing tabs preserved | PARTIAL | non-leading tabs pass through |
| never convert single space before non-stop | CONFORMS | |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Item | Status | Notes |
|---|---|---|
| 0 success / >0 error | CONFORMS | line 185 |
| error message to stderr | CONFORMS | |
| continue on later files | CONFORMS | |

#### Cross-cutting (i18n, locale, multibyte width)

`setlocale` called but all column arithmetic uses char counts (1 col each); no `unicode-width` dependency. Double-wide CJK chars misalign (#10).

### Test coverage signal

Not covered:
- [ ] repeating default tab stops (>8 leading spaces); `test_2` encodes the wrong value (#1)
- [ ] `-t N` with N≠8 repeating
- [ ] `-t` implying all-blanks (#2)
- [ ] space-separated tablist (#6)
- [ ] backspace in input (#5)
- [ ] leading tab interacting with conversion (#9)
- [ ] multibyte/wide characters (#10)
- [ ] multiple files; `-` among multiple operands (#8)
- [ ] error cases (non-existent file)

### Suggested PR groupings

- **PR A — "Repeating tab stops + fix `test_2`"**: #1.
- **PR B — "`-t` implies `-a`"**: #2.
- **PR C — "Rewrite `convert_all_blanks` (column tracking, multi-stop)"**: #3, #4.
- **PR D — "Backspace handling"**: #5.
- **PR E — "Tablist parsing + leading-tab + `-` operand"**: #6, #8, #9.

---

## `uniq`

**Implementation:** `text/uniq.rs` (220 lines)
**Tests:** `text/tests/uniq/mod.rs` (203 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/uniq.md`
**Date:** 2026-06-25

### TL;DR

Core deduplication is correct and the five options are present. Three substantive bugs: (1) the field-skip algorithm misidentifies field boundaries when lines start with blanks; (2) `-s` byte-indexes into a Rust `&str`, panicking on multi-byte UTF-8; (3) the output-file operand does not recognize `-` as stdout. The obsolescent `-N`/`+N` forms are correctly absent (removed in POSIX.1-2024).

### Priority issues

#### Critical

- [ ] **#1 — `-s N` byte-indexes into a UTF-8 string (panic on multi-byte chars).** `text/uniq.rs:153` (`processed_line[c..].to_string()`). `-s` counts *characters*, but `c` is used as a byte offset; `uniq -s 1` on `"öabc"` panics ("not a char boundary"). Fix: `chars().skip(c).collect()`.

#### Major

- [ ] **#2 — Field-skip leaves a leading blank in the comparison string.** `text/uniq.rs:136-147`. The `skip_while` counts each whitespace char as a boundary and stops at the first blank, so `-f 1` on `"a b c"` leaves `" b c"`. POSIX field = `[[:blank:]]*[^[:blank:]]*` (leading blanks belong to the field). A line beginning with a blank breaks immediately. Fix: skip leading blanks then non-blanks, N times.
- [x] **#3 — Output file operand `-` not recognized as stdout.** `text/uniq.rs:84-87`. FIXED (Phase 2): a `-` output_file operand now writes to stdout instead of creating a file named `-`.
- [ ] **#4 — `-c` with `-d`/`-u` is a hard error.** `text/uniq.rs:48-60`. POSIX marks the `[-c|-d|-u]` combination undefined, not mandatorily an error; rejecting is permitted but diverges from common practice. (Informational/Major.)

#### Minor

- [ ] **#5 — Over-skip returns the original line instead of a null string.** `text/uniq.rs:159-163`. When `-f`/`-s` consumes the whole line, spec says use a null string for comparison; the impl compares full original lines, so two lines that both reduce to empty compare unequal. Fix: return empty string.
- [ ] **#6 — `process_line` called twice per iteration** (`text/uniq.rs:95,98`); redundant work, not a correctness issue.
- [ ] **#7 — `-u -d` rejected** though POSIX only says undefined (`text/uniq.rs:50`).

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Item | Status | Notes |
|---|---|---|
| `[-c\|-d\|-u]` group | CONFORMS | clap + validate_args |
| `[-f fields]` / `[-s chars]` | CONFORMS | |
| `[input_file [output_file]]` | CONFORMS | two optional PathBufs |
| `--` end-of-options | CONFORMS | clap |
| `+`/`-N`/`+N` obsolescent | N/A | removed in POSIX.1-2024; absence correct |

#### OPTIONS

| Option | Status | Notes |
|---|---|---|
| `-c` prefix count | CONFORMS | `"{} {}"` |
| `-d` only-repeated | CONFORMS | `count > 1` |
| `-u` only-unique | CONFORMS | `count == 1` |
| `-f fields` | PARTIAL | leading-blank bug (#2); over-skip (#5) |
| `-s chars` | PARTIAL | byte-index crash (#1); over-skip (#5) |
| `-f` then `-s` ordering | CONFORMS | fields first (132-148), then chars (151-157) |
| `-c` with `-d`/`-u` | DIVERGES | undefined per spec; impl errors (#4) |

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| input absent → stdin | CONFORMS | line 81 |
| input `-` → stdin | CONFORMS | lines 75-76 |
| output absent → stdout | CONFORMS | line 86 |
| output `-` → stdout | DIVERGES | creates file `-` (#3) |
| last line without newline | CONFORMS | `writeln!` re-adds |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL` | CONFORMS | `setlocale` line 203 |
| `LC_CTYPE` | PARTIAL | blank detection uses `is_whitespace()` (Unicode), not `[[:blank:]]` |
| `LC_COLLATE` | MISSING (listed) | uniq compares byte-identical per spec rationale; LC_COLLATE need not affect comparison |
| `LC_MESSAGES` | CONFORMS | gettextrs lines 204-205 |
| `NLSPATH` | CONFORMS | |

#### ASYNCHRONOUS EVENTS

- [x] Default — CONFORMS.

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| unique/dup lines to stdout | CONFORMS | |
| `-c` format `"%d %s\n"` | CONFORMS | |
| non-`-c` `"%s\n"` | CONFORMS | |
| diagnostics to stderr | CONFORMS | line 215 |

#### EXTENDED DESCRIPTION

| Item | Status | Notes |
|---|---|---|
| adjacent-line comparison | CONFORMS | `last_line` |
| first copy of group written | CONFORMS | |
| trailing newline ignored in comparison | CONFORMS | `lines()` strips |
| `-f N` field = `[[:blank:]]*[^[:blank:]]*` | PARTIAL | wrong boundary (#2) |
| `-f N` over-skip → null string | DIVERGES | returns original (#5) |
| `-s N` after `-f N` | CONFORMS | |
| `-s N` over-skip → null string | DIVERGES | `clear()` then returns original (#5) |
| `-s N` char vs byte index | DIVERGES | byte index, crashes (#1) |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Item | Status | Notes |
|---|---|---|
| 0 success / >0 error | CONFORMS | |
| I/O errors to stderr | CONFORMS | line 215 |

#### Cross-cutting (i18n, locale, collation)

Spec rationale clarifies uniq uses byte-identical comparison (not collation), so Rust `==` is correct. The two locale gaps: blank classification for `-f` uses Unicode `is_whitespace()` not LC_CTYPE `[[:blank:]]`, and `-s` must count characters not bytes (#1).

### Test coverage signal

Not covered:
- [ ] `-s N` with multi-byte UTF-8 (crash, #1)
- [ ] line with leading blanks under `-f N` (#2)
- [ ] `-f N`/`-s N` over-skip → null-string rule (#5)
- [ ] output operand `-` → stdout (#3)
- [ ] `-c` with multi-digit counts (format spacing)
- [ ] named input file (only stdin tested)
- [ ] empty file; single-line file

### Suggested PR groupings

- **PR A — "Char-count indexing for `-s`"**: #1.
- **PR B — "Field-skip rewrite + null-string over-skip"**: #2, #5.
- **PR C — "Output operand `-` → stdout"**: #3.

---

## `wc`

**Implementation:** `text/wc.rs` (219 lines)
**Tests:** `text/tests/wc/mod.rs` (46 lines)
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/wc.md`
**Date:** 2026-06-25

### TL;DR

Two confirmed bugs dominate: (1) a single named file operand prints no filename, contradicting the mandatory `"%d %d %d %s\n"` output format; (2) the `-` operand is not recognized as stdin, opening a literal file named `-`. The locale/multibyte story is also incomplete — `-m` always assumes UTF-8 regardless of LC_CTYPE, and the word-boundary whitespace table is ASCII-only. Default operation and flag arithmetic are otherwise correct.

### Priority issues

#### Critical

- none.

#### Major

- [ ] **#1 — Single named-file operand omits the filename.** `text/wc.rs:105`. The format is `"%d %d %d %s\n"` with `<file>`; the "no name" exception applies only when no file operands are given. `build_display_str` tests `args.files.len() > 1`, so `wc -c foo.txt` emits `42` not `42 foo.txt`. Fix: append the filename whenever `!filename.is_empty()`.
- [x] **#2 — `-` operand does not invoke stdin.** `text/wc.rs:120`. FIXED (Phase 2): `wc_file_bytes` opens via `plib::io::input_stream_dashed`; `-` reads stdin.
- [ ] **#3 — `-m` character counting is unconditionally UTF-8.** `text/wc.rs:138` (`(ch >> 6) != 0b10`). Correct only in UTF-8 locales; spec requires LC_CTYPE encoding. Fix: `mbrlen`-based loop under the process locale.

#### Minor

- [ ] **#4 — `-w` whitespace detection is ASCII-only.** `text/wc.rs:58-69,141-145`. `BYTE_TABLE` covers only the six ASCII whitespace bytes; multibyte locale whitespace (e.g. U+00A0) is not recognized, and the byte-by-byte loop can't detect multibyte whitespace. Fix: `iswspace` on decoded wide chars.
- [ ] **#5 — Field width fixed at 8 (`{:>8}`)** (`text/wc.rs:78,89,100`); not a spec violation (the format doesn't mandate a width) but misaligns the total line for >8-digit counts; GNU uses an adaptive width.
- [ ] **#6 — Totals line accumulates zero counts for files that failed to open** (`text/wc.rs:200-209`); acceptable per "Default" consequences but potentially misleading.

### Detailed conformance matrix

#### SYNOPSIS / argv parsing

| Item | Status | Notes |
|---|---|---|
| `[-c\|-m]` mutually exclusive | PARTIAL | both accepted; `-m` wins (`args.bytes=false`, line 182) |
| `[-lw]` flags | CONFORMS | |
| `--` end-of-options | CONFORMS | clap (untested) |
| `-` operand → stdin | DIVERGES | (#2) |
| no operands → stdin | CONFORMS | line 190 |

#### OPTIONS

| Option | Status | Notes |
|---|---|---|
| `-c` bytes | CONFORMS | `n_read` accumulation (line 135) |
| `-l` newlines | CONFORMS | counts `0x0A` (line 143) |
| `-m` characters | PARTIAL | always UTF-8 (#3) |
| `-w` words | PARTIAL | ASCII whitespace only (#4) |
| default `-lwc` | CONFORMS | lines 177-180 |
| `-m` overrides `-c` | CONFORMS | line 182 |

#### OPERANDS / STDIN / INPUT FILES

| Item | Status | Notes |
|---|---|---|
| multiple file operands | CONFORMS | loop line 200 |
| no operand → stdin | CONFORMS | empty `PathBuf` |
| `-` → stdin | DIVERGES | (#2) |
| any file type | CONFORMS | |

#### ENVIRONMENT VARIABLES

| Variable | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL` | CONFORMS | `setlocale` line 168 |
| `LC_CTYPE` for `-m` | DIVERGES | UTF-8 bitmask, not `mblen` (#3) |
| `LC_CTYPE` for `-w` | PARTIAL | ASCII-only `BYTE_TABLE` (#4) |
| `LC_MESSAGES` | CONFORMS | gettextrs lines 169-170 |
| `NLSPATH` | N/A | XSI |

#### ASYNCHRONOUS EVENTS

- [x] Default — CONFORMS.

#### STDOUT / STDERR

| Item | Status | Notes |
|---|---|---|
| default `"%d %d %d %s\n"` with filename | DIVERGES | single-file omits filename (#1) |
| multi-file: one line per file + filename | CONFORMS | lines 105-113 |
| total line when >1 file | CONFORMS | lines 212-215 |
| column order lines/words/bytes (fixed) | CONFORMS | `build_display_str` |
| no filename for stdin/no-operand | CONFORMS | |
| diagnostics to stderr | CONFORMS | lines 195, 205 |

#### EXTENDED DESCRIPTION

| Rule | Status | Notes |
|---|---|---|
| `-l` counts newlines | CONFORMS | |
| `-w` uses LC_CTYPE whitespace | PARTIAL | ASCII only (#4) |
| `-c` counts bytes | CONFORMS | |
| `-m` counts chars per LC_CTYPE | PARTIAL | hardcoded UTF-8 (#3) |
| column order fixed regardless of flag order | CONFORMS | |
| right-justified numbers | CONFORMS | `{:>8}` (#5) |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

| Item | Status | Notes |
|---|---|---|
| 0 success / >0 error | CONFORMS | |
| error message to stderr | CONFORMS | |

#### Cross-cutting (i18n, locale, multibyte counting)

`setlocale` initializes the locale, but neither byte-counting nor word-boundary code queries it at runtime — both are hard-wired ASCII/UTF-8. POSIX multibyte support needs `mbrlen`/`mbrtowc` and `iswspace` on decoded wide chars (#3, #4).

### Test coverage signal

Only three tests (empty, single char, two-word line), all stdin. Not covered:
- [ ] single named file operand (would expose #1)
- [ ] `-` operand as stdin (#2)
- [ ] multiple file operands (total line, per-file filenames)
- [ ] default output (no flags) format and column order
- [ ] `-m` with multibyte UTF-8 input (#3)
- [ ] `-l -w`/`-l -c` combined (alignment, #5)
- [ ] non-ASCII word-boundary detection (#4)
- [ ] non-existent/unreadable file; exit code propagation
- [ ] `--` end of options

### Suggested PR groupings

- **PR A — "Filename for single-file operand"**: #1.
- **PR B — "`-` operand → stdin"**: #2.
- **PR C — "Locale-aware `-m` + `-w`"**: #3, #4.
- **PR D — "Adaptive field width + tests"**: #5.

---

## Cross-cutting patterns

Several conformance gaps recur across the crate. Grouping fixes by pattern may be more efficient than the per-utility PRs above.

### 1. The `-` operand is not routed to stdin (`dashed_stdin: false`)

A workspace-wide antipattern: `plib::io::input_stream`/`input_reader` is called with `dashed_stdin: false`, so a `-` operand opens a literal file named `-` rather than reading standard input. Confirmed in **`expand` (#7), `fold` (#5), `head` (#1, Major), `tsort` (#3, Major), `wc` (#2, Major)**, and as a "sole-operand only" partial in **`cut` (#2, Critical), `sort` (#21), `unexpand` (#8), `uniq`** (output side, #3). A single sweep flipping `dashed_stdin` to `true` (and routing `-` at its operand position, not just when it is the sole argument) closes this class. `asa`, `lex`, and `paste` already do this correctly and are the reference pattern.

### 2. Locale is initialized but never used (LC_COLLATE / LC_CTYPE)

Nearly every utility calls `setlocale(LcAll, "")` + `textdomain`, but few consult the locale at runtime:
- **Collation via `strcoll` missing where the spec mandates the current collating sequence:** `comm` (#1, Major), `join` (#8, Major), `sort` (#1, Critical). `plib::locale::strcoll` exists and is unused.
- **Character classification hardcoded ASCII instead of LC_CTYPE:** `tr` classes (#2), `sort` `-d`/`-i` (#12,#13), `uniq` `-f` blanks, `wc` `-w` whitespace (#4).
- **Multibyte column width (`wcwidth`) absent** where column math matters: `expand` (#5), `fold` (#2), `unexpand` (#10), `pr`, `cut`.
- **Character vs byte counting under LC_CTYPE:** `wc -m` (#3), `cut -c`/`-b`.

`plib::locale` already wraps `isprint`/`strcoll`/`strftime`; extending it with `strcoll`-based comparison and `iswctype`/`wcwidth` wrappers would let these utilities share one correct implementation.

### 3. Hardcoded-English diagnostics (LC_MESSAGES)

Most utilities route help strings through `gettext` but emit runtime diagnostics via bare `eprintln!` with the raw `io::Error`, so `LC_MESSAGES` has no effect. Pervasive (`asa` #2, `comm` #3, `grep` #3, `head` #4, `tail` #5, and most others). Mirrors the `dev/` crate's deferred "string-level gettext" item — a `plib::diag`-style shared diagnostic surface would close it crate-wide.

### 4. POSIX.1-2024 Issue 8 additions not yet implemented

New mandatory features from the 2024 revision are missing: **`tail -r` (#1, Critical)**, **`tsort -w` (#1, Critical)**, and the **`sed s///i` flag (#5, Major)**. `head -c` (also an Issue 8 addition) *is* implemented correctly.

### 5. Untested golden paths hiding real bugs

Several test suites never exercise the utility's core transformation, so latent bugs pass: **`fold`** (no input ever exceeds the fold width — masks the `-s` index bug), **`expand`** (the entire `Stops` list path untested), **`unexpand`** (`test_2` encodes a wrong expected value that hides the repeating-stop bug), **`wc`** (no named-file or multi-file test — masks the filename bug), **`sort`** (`test_02e` asserts `-C` emits stderr, cementing a spec violation), **`tsort`** (tests assert exit 0 on cycles). Adding folding/multi-file/locale tests should precede or accompany the fixes.

### Severity tally

| Utility | Critical | Major | Minor |
|---|---|---|---|
| asa | 0 | 0 | 2 |
| comm | 0 | 1 | 2 |
| csplit | 3 | 1 | 4 |
| cut | 2 | 2 | 3 |
| diff | 2 | 4 | 5 |
| expand | 2 | 3 | 2 |
| fold | 1 | 3 | 2 |
| grep | 1 | 2 | 3 |
| head | 0 | 2 | 2 |
| join | 4 | 6 | 2 |
| nl | 0 | 2 | 4 |
| paste | 0 | 1 | 5 |
| patch | 2 | 4 | 4 |
| pr | 0 | 4 | 4 |
| sed | 4 | 6 | 6 |
| sort | 4 | 7 | 11 |
| tail | 2 | 3 | 2 |
| tr | 0 | 3 | 4 |
| tsort | 2 | 1 | 3 |
| unexpand | 2 | 3 | 5 |
| uniq | 1 | 3 | 3 |
| wc | 0 | 3 | 3 |

The highest-risk utilities are **`join`** (early-stage stub; near-total rework), **`sed`** and **`sort`** (multiple correctness defects on common paths), and **`csplit`**, **`cut`**, **`unexpand`**, **`tail`** (data-affecting bugs on the golden path). **`asa`** is essentially conformant.
