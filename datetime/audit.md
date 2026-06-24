# POSIX.1-2024 Conformance Audits — `datetime/` utilities

This file collects per-utility POSIX conformance audits for the date/time
utilities crate. Each audit follows the playbook in `audits.md`.

**Crate:** `datetime/` — `cal`, `date`, `sleep`, `time` (4 single-file
utilities, ~640 lines total).
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3.
**Reference slices:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/{cal,date,sleep,time}.md`
**Date:** 2026-06-24
**Method:** static spec-vs-code, every "absent"/"wrong-value" claim grep-verified
against the cited lines. No code was modified — audit only.

---

## Cross-cutting observations

| Theme | Status | Notes |
|---|---|---|
| `setlocale(LC_ALL, "")` at `main` | Present in all four | `cal.rs:212`, `date.rs:183`, `sleep.rs:25`, `time.rs:134`. |
| `textdomain`/`bind_textdomain_codeset` | Present in all four | `time.rs:136` additionally calls `bindtextdomain("posixutils-rs", "locale")` with a relative path; the other three omit `bindtextdomain` entirely. No `.mo` catalogs ship, so `gettext()` is an identity map regardless. |
| Runtime diagnostics via `gettext()` | **Partial** | `cal` wraps its one error string; `date` and `time` use raw `eprintln!`/`Err(&str)` hardcoded English; `sleep` has no diagnostics of its own (clap-only). `LC_MESSAGES` is therefore inert for `date`/`time`. |
| `LC_CTYPE`/`LC_TIME`/`TZ` honored via libc | Mixed | `date`/`cal` defer to libc `strftime`/`localtime_r` (honor `LC_TIME`/`TZ`); `cal`'s *own* month-name/weekday strings do **not** honor `LC_TIME` (#C1). |

As in the `dev/` and `sys/` audits, crate-wide `.mo` catalog shipping is a
tree-wide i18n concern and is not counted against individual utilities beyond a
Minor note.

---

## Summary — headline findings

| # | Util | Sev | One-liner |
|---|---|---|---|
| **#T1** | time | **Critical** | `tms_end` is zeroed but never filled by a second `libc::times()` call — CPU stats are garbage. |
| **#T2** | time | **Critical** | Reads parent `tms_utime`/`tms_stime` (and subtracts backwards) instead of child `tms_cutime`/`tms_cstime` — the wrong process is measured. |
| **#T3** | time | Major | Child exit status is discarded (`let _ = child.wait()`); `time` always exits 0, violating "exit status of `time` shall be the exit status of `utility`". |
| **#S1** | sleep | Major | `range(1..)` rejects `sleep 0`; POSIX `time` operand is a *non-negative* integer, so `sleep 0` must succeed with exit 0. |
| **#D1** | date | Major | 2-digit-year century inference is off by one: `yy=69` maps to 2069, but POSIX mandates 69–99 → 1969–1999. |
| **#C1** | cal | Minor | `LC_TIME` not honored for the calendar's month names / weekday header (always English). Spec: "`LC_TIME` Determine the format and contents of the calendar." |
| **#D2** | date | Minor | `strftime` returning 0 is always treated as an error; a legitimately-empty conversion yields a false "produced no output" exit 1. |
| **#T4/#D3/#S2** | time/date/sleep | Minor | Hardcoded-English runtime diagnostics (`LC_MESSAGES` inert). |
| **#D4** | date | Minor | `String::from_utf8_lossy` on `strftime` output can mangle bytes in non-UTF-8 locales. |
| **#T5** | time | Minor | A child terminated by a signal is not reflected in `time`'s exit status (no 128+n). |

Totals: **2 Critical, 3 Major, 6 Minor.** `cal` is the cleanest (one Minor i18n
gap); `time` carries both Criticals.

---

## `time`

**Implementation:** `datetime/time.rs` (159 lines)
**Tests:** `datetime/tests/time/mod.rs` (58 lines, 4 `#[test]`s)
**Spec:** POSIX.1-2024, Vol. 3 §3, pp. 3488–3492
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/time.md`
**Date:** 2026-06-24

### TL;DR

`time` correctly forks the utility, inherits stdio, maps 126/127, and emits the
`-p` format verbatim — but **the timing numbers it prints are wrong on every
run**. The end-of-run snapshot `tms_end` is created with `std::mem::zeroed()`
and never populated by a second `libc::times()` call (#T1), and the arithmetic
reads the parent's own `tms_utime`/`tms_stime` rather than the child's
`tms_cutime`/`tms_cstime`, subtracting start−end instead of end−start (#T2). The
net effect is that User/System CPU time is reported as a near-zero constant that
has nothing to do with the invoked utility — the utility's core purpose is
broken. Separately, the child's exit status is discarded, so `time` always exits
0 (#T3).

### Priority issues

#### Critical

- [ ] **#T1 — `tms_end` is never populated; CPU stats are computed against an all-zero struct.** `time.rs:85`. `let tms_end: libc::tms = unsafe { std::mem::zeroed() };` — there is exactly one `libc::times` call in the file (`time.rs:70`, filling `tms_start`); `grep -n 'libc::times' time.rs` → one hit. The end snapshot is required after `child.wait()` to capture accumulated child CPU. As written, `tms_end.*` is always 0. Fix: call `libc::times(&mut tms_end)` after the wait, before computing deltas.
- [ ] **#T2 — Wrong fields and wrong subtraction direction.** `time.rs:87-88`. POSIX 117331–117335: User CPU time is "the sum of the `tms_utime` and `tms_cutime` fields … for the process in which utility is executed", System CPU likewise `tms_stime + tms_cstime`. The child's CPU is accumulated into the parent's **`tms_cutime`/`tms_cstime`** after `wait()`. The code instead reads `tms_start.tms_utime - tms_end.tms_utime` (parent's *own* user time, start minus end). Even if #T1 is fixed, this measures the wrong process and has the sign inverted. Fix: `user = (tms_end.tms_cutime + tms_end.tms_utime) - (tms_start.tms_cutime + tms_start.tms_utime)`, likewise for system with the `s`-variants; divide by `_SC_CLK_TCK`.

#### Major

- [ ] **#T3 — Child exit status is discarded; `time` always exits 0.** `time.rs:82` (`let _ = child.wait()...`), `time.rs:158` (`Status::Ok.exit()` → 0). POSIX EXIT STATUS 117435: "If the utility utility is invoked, the exit status of `time` shall be the exit status of utility." A successful spawn whose child exits 5 still yields `time` exit 0. Fix: capture `ExitStatus`, and on normal exit propagate `code()`; on signal termination map to 128+signal (see #T5).

#### Minor

- [ ] **#T4 — Runtime diagnostics are hardcoded English.** `time.rs:144,148,152` (`eprintln!("Command not found: {}", …)` etc.). POSIX 117404: `LC_MESSAGES` shall affect diagnostic message contents. These strings are not routed through `gettext()` (only clap help/about is). Fix: wrap in `gettext()`.
- [ ] **#T5 — Signal-terminated child not reflected in exit status.** `time.rs:82`. Bundled with #T3: when the utility dies from a signal, the conventional shell behavior is exit 128+signum; the current discard path can't express it.
- [ ] **#T6 — `bindtextdomain("posixutils-rs", "locale")` uses a relative path.** `time.rs:136`. Diverges from the other three binaries (which omit `bindtextdomain`) and resolves relative to CWD. Harmless today (no catalogs), but inconsistent. Note only.

### Detailed conformance matrix

#### SYNOPSIS / OPTIONS / OPERANDS

- [x] `time [-p] utility [argument...]` — `-p` flag (`time.rs:28-33`), `utility` (`35-36`), `arguments` with `trailing_var_arg` (`38-43`). CONFORMS.
- [x] XBD 12.2 conformance / `--` end-of-options — clap default; tests pass `--` explicitly (`tests/time/mod.rs:42`).
- [x] `utility` invoked via PATH search — `Command::new(&args.utility)` uses `execvp`-style PATH lookup (`time.rs:72`). CONFORMS.
- [x] Special-built-in/function/intrinsic operand → unspecified — N/A (external binary only; spec leaves this unspecified). CONFORMS.

#### STDIN / INPUT FILES / STDOUT

- [x] STDIN not used; child inherits stdio — `Stdio::inherit()` (`time.rs:74-75`). CONFORMS.
- [x] STDOUT not used by `time` itself — timing goes to stderr. CONFORMS.

#### STDERR

- [x] Timing statistics written to standard error — `writeln!(io::stderr(), …)` (`time.rs:91-107`). CONFORMS.
- [x] `-p` format `"real %f\nuser %f\nsys %f\n"` — `time.rs:91-98` emits `real {:.6}\nuser {:.6}\nsys {:.6}` + `writeln!` trailing `\n`. Six fractional digits ≥ the mandated ≥1. CONFORMS (format); values are wrong (#T1/#T2).
- [x] Default (non-`-p`) format unspecified — `time.rs:100-107` "Elapsed/User/System time" is permitted. CONFORMS.
- [ ] **Timing values incorrect** — (#T1/#T2) the numbers themselves are garbage.

#### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL`/`LC_CTYPE` | CONFORMS | `setlocale(LC_ALL,"")` at `time.rs:134`. |
| `LC_MESSAGES` | PARTIAL | (#T4) diagnostics hardcoded English. |
| `LC_NUMERIC` | N/A | Rust `{:.6}` always uses `.` radix; POSIX-locale output is correct, other locales unspecified for the default format. |
| `NLSPATH` (XSI) | MISSING | No catalog support (tree-wide). |
| `PATH` | CONFORMS | Used by `Command` for utility lookup. |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

- [x] 127 when utility not found — `io::ErrorKind::NotFound → CommandNotFound → 127` (`time.rs:77-78,126,145`). CONFORMS.
- [x] 126 when found but not invocable — other spawn errors → `ExecCommand → 126` (`time.rs:79,125,149`). CONFORMS.
- [ ] **Exit status of invoked utility not propagated** — (#T3) always 0.
- [x] 1–125 on internal `time` error — `TimeError → 1` (`time.rs:124,153`). CONFORMS.

#### ASYNCHRONOUS EVENTS

- [x] Default — `grep -nE 'SIGCONT|SIGWINCH|signal' time.rs` → 0 matches; spec says "Default". CONFORMS.

### Test coverage signal

Not covered:
- [ ] No test asserts the reported `user`/`sys` values are non-zero for a CPU-bound child (would catch #T1/#T2).
- [ ] No test asserts `time` propagates a non-zero child exit code (#T3).
- [ ] No test asserts 126/127 mapping (only "not provided"/clap errors are exercised; `tests/time/mod.rs:51-57`).

---

## `date`

**Implementation:** `datetime/date.rs` (201 lines)
**Tests:** `datetime/tests/date/mod.rs` (139 lines, 6 `#[test]`s)
**Spec:** POSIX.1-2024, Vol. 3 §3, pp. 2818–2823
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/date.md`
**Date:** 2026-06-24

### TL;DR

The display path is solid: `+format` is rendered through libc `strftime` over
`localtime_r`/`gmtime_r` (honoring `TZ`, `-u`, and `LC_TIME`), a trailing
`<newline>` is always appended, and the default format matches the spec's
`%a %b %e %H:%M:%S %Z %Y`. The XSI set-time path handles the 8/10/12-digit
forms, but the 2-digit-year century inference is off by one against POSIX's
explicit ranges (#D1). Smaller issues: a legitimately-empty `strftime` result is
misreported as an error (#D2), diagnostics are hardcoded English (#D3), and
non-UTF-8 locale bytes can be mangled (#D4).

### Priority issues

#### Major

- [ ] **#D1 — 2-digit-year century inference off by one.** `date.rs:128-133`. POSIX 91707–91708: "values in the range [69,99] shall refer to years 1969 to 1999 … values in the range [00,68] shall refer to years 2000 to 2068." The code does `if year < 70 { year + 2000 } else { year + 1900 }`, so `yy=69` → 2069 (should be 1969). The boundary should be `< 69` (i.e., 00–68 → 20xx, 69–99 → 19xx). Fix: change the threshold from `70` to `69`. Affects exactly the two-digit year `69`.

#### Minor

- [ ] **#D2 — `strftime` returning 0 is always treated as a fatal error.** `date.rs:87-95`. `strftime(3)` returns 0 both when the buffer is too small **and** when the conversion legitimately produces an empty string. After trying 256- and 1024-byte buffers, the code prints "format string produced no output" and exits 1. A format whose entire output is empty (or empty in a given locale) is thus rejected. Fix: distinguish empty-but-valid (e.g. format string non-empty but `len==0` after a successful call with adequate buffer) from buffer-overflow; or special-case via a sentinel. Low impact in the POSIX locale.
- [ ] **#D3 — Runtime diagnostics hardcoded English.** `date.rs:49,56,70,94,101,144,154,161,175`. POSIX 91728: `LC_MESSAGES` shall affect diagnostic messages. The `eprintln!`/`Err(&str)` strings bypass `gettext()`. Fix: route through `gettext()` and a uniform `date:` prefix (the `Err(&str)` paths currently surface as Rust's `Error: date: …` via `main`'s `Box<dyn Error>`).
- [ ] **#D4 — `from_utf8_lossy` may mangle `strftime` output in non-UTF-8 locales.** `date.rs:88`. `strftime` emits bytes in the locale's `LC_CTYPE` encoding; `String::from_utf8_lossy` replaces invalid sequences with U+FFFD. Fix: write the raw bytes to stdout (`stdout().write_all(&buf[..len])`) instead of lossy-decoding.

### Detailed conformance matrix

#### SYNOPSIS / OPTIONS

- [x] `date [-u] [+format]` and XSI `date [-u] mmddhhmm[[cc]yy]` — both forms dispatched in `main` (`date.rs:189-198`). CONFORMS.
- [x] `-u` performs ops as if `TZ=UTC0` — uses `gmtime_r` for display (`date.rs:62-66`) and `chrono::Utc` for set (`date.rs:150-156`). CONFORMS.
- [x] XBD 12.2 / `--` — clap default; `+`-prefixed operand is treated as a value, not an option. CONFORMS.

#### OPERANDS

- [x] `+format` rendered "as if by `strftime()`" over `localtime(&now)`/`gmtime(&now)`, `now = time(0)` — `date.rs:54-86` matches the spec algorithm exactly. CONFORMS.
- [x] `<newline>` always appended to `strftime` output — `println!("{}", timestr)` (`date.rs:89`); empty format → bare `println!()` (`date.rs:42`). CONFORMS.
- [x] Set form `mmddhhmm` (len 8) → current year — `date.rs:116-122`. CONFORMS.
- [x] Set form `mmddhhmmccyy` (len 12) → explicit 4-digit year — `date.rs:135-142`. CONFORMS.
- [ ] **Set form `mmddhhmmyy` (len 10) century inference** — (#D1) off by one at `yy=69`.
- [x] Invalid field values rejected — `chrono::with_ymd_and_hms` non-`Single` → `Err` (`date.rs:151-164`). CONFORMS.

#### STDIN / STDOUT / STDERR

- [x] STDIN not used — no `stdin()` call (`grep -n stdin date.rs` → 0). CONFORMS.
- [x] Default output equals `+%a %b %e %H:%M:%S %Z %Y` — `DEF_TIMESTR` (`date.rs:17,190`). CONFORMS.
- [x] STDERR only for diagnostics — all `eprintln!`/error paths target stderr. CONFORMS (#D3 on i18n).

#### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL`/`LC_CTYPE` | CONFORMS | `setlocale` at `date.rs:183`; libc `strftime` honors them. |
| `LC_MESSAGES` | PARTIAL | (#D3) hardcoded English. |
| `LC_TIME` | CONFORMS | `strftime` over libc honors `LC_TIME`. |
| `TZ` | CONFORMS | `localtime_r` honors `TZ`; `-u` overrides to UTC. Verified by `tests/date/mod.rs:24-60`. |
| `NLSPATH` (XSI) | MISSING | Tree-wide. |

#### EXIT STATUS / ASYNCHRONOUS EVENTS

- [x] 0 on success, >0 on error — `main` returns `Ok`/propagates `Err` (exit 1); explicit `process::exit(1)` in `show_time` error paths. CONFORMS.
- [x] Default async events — no signal handling; spec says "Default". CONFORMS.

### Test coverage signal

Not covered:
- [ ] No test exercises the set-time path at all (would catch #D1) — set-time needs privilege, so a unit-level century-mapping test is the practical route.
- [ ] No test asserts `+`-empty / unusual formats (#D2).
- [ ] No test for `-u` set-form or non-UTF-8 locale output (#D4).

---

## `sleep`

**Implementation:** `datetime/sleep.rs` (39 lines)
**Tests:** none (no `datetime/tests/sleep/`).
**Spec:** POSIX.1-2024, Vol. 3 §3, pp. 3433–3435
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/sleep.md`
**Date:** 2026-06-24

### TL;DR

`sleep` is nearly conforming: it sleeps integral seconds, accepts the full
2 147 483 647-second range (`u64`), and handles `SIGALRM` by ignoring it
(spec-permitted option 2). The one real defect is that `sleep 0` is rejected by
the clap range guard, contradicting the "non-negative decimal integer" operand
definition.

### Priority issues

#### Major

- [ ] **#S1 — `sleep 0` is rejected.** `sleep.rs:17-18`: `value_parser!(u64).range(1..)` makes the minimum 1, so `sleep 0` exits non-zero with a clap range error. POSIX 115335: the `time` operand is "A non-negative decimal integer" — `0` is valid and must suspend for "at least 0 seconds" (i.e. return immediately with exit 0). Fix: change the range to `0..` (or drop the lower bound).

#### Minor

- [ ] **#S2 — clap diagnostics hardcoded English.** The range/parse error text is clap-generated and not gettext-routed; `LC_MESSAGES` inert. Tree-wide pattern; note only.

### Detailed conformance matrix

#### SYNOPSIS / OPERANDS

- [x] `sleep time` — single operand (`sleep.rs:17-21`). CONFORMS.
- [ ] **`time` non-negative integer** — (#S1) `0` rejected.
- [x] Up to 2 147 483 647 seconds supported — `u64` covers it; `thread::sleep(Duration::from_secs)` (`sleep.rs:36`). CONFORMS (RATIONALE 115397).
- [x] Integral seconds only — `u64`, no fractional/suffix parsing. CONFORMS (POSIX requires only integral).

#### STDIN / STDOUT / STDERR / INPUT FILES

- [x] STDIN/STDOUT not used; STDERR diagnostics only — no I/O in the binary beyond clap. CONFORMS.

#### ASYNCHRONOUS EVENTS

- [x] `SIGALRM` handled — `libc::signal(SIGALRM, SIG_IGN)` (`sleep.rs:31-34`) selects spec option 2 ("Effectively ignore the signal"). CONFORMS.
- [x] All other signals take the standard (default) action — no other handlers installed. CONFORMS (spec 115362).

#### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL`/`LC_CTYPE` | CONFORMS | `setlocale` at `sleep.rs:25`. |
| `LC_MESSAGES` | PARTIAL | (#S2) clap-English diagnostics. |
| `NLSPATH` (XSI) | MISSING | Tree-wide. |

#### EXIT STATUS

- [x] 0 on success / SIGALRM, >0 on error — `main` returns `Ok` (exit 0); `SIG_IGN` means SIGALRM never terminates. CONFORMS.

### Test coverage signal

Not covered:
- [ ] No tests exist for `sleep` at all. Add: `sleep 0` exits 0 (#S1); `sleep 1` returns ~promptly; invalid operand exits >0.

---

## `cal`

**Implementation:** `datetime/cal.rs` (244 lines)
**Tests:** `datetime/tests/cal/mod.rs` (377 lines, 24 `#[test]`s)
**Spec:** POSIX.1-2024, Vol. 3 §3, pp. 2731–2733 (XSI)
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/cal.md`
**Date:** 2026-06-24

### TL;DR

`cal` is the cleanest utility in the crate. Operand handling matches the
Issue-6/Issue-7 RATIONALE (no operands → current month; single operand → whole
year; both → one month), the Julian→Gregorian switch with the September 1752
11-day gap is implemented and well-tested, year/month ranges are enforced, and
`TZ` drives "the current month." The only conformance gap is that `LC_TIME` does
not affect the calendar's month-name and weekday strings (#C1) — they are a
hardcoded English array passed through `gettext()` (which has no catalogs).

### Priority issues

#### Minor

- [ ] **#C1 — `LC_TIME` not honored for month names / weekday header.** `cal.rs:14-28` (`MONTH_NAMES`), `cal.rs:152` (`gettext(MONTH_NAMES[...])`), `cal.rs:155` (`gettext("Su Mo Tu We Th Fr Sa")`). POSIX 88457: "`LC_TIME` Determine the format and contents of the calendar." The strings are English literals routed through `gettext()` (an `LC_MESSAGES` mechanism, not `LC_TIME`) with no `.mo` catalogs, so output is always English/Sunday-first regardless of `LC_TIME`. The spec leaves the *format* unspecified (so Sunday-first and the column layout are fine), but the month/day **names** are an `LC_TIME` responsibility. Fix: derive abbreviated weekday and month names from libc `nl_langinfo`/`strftime` under `LC_TIME` (as `dev/`'s `plib::locale::strftime` does). Low practical impact until catalogs/locale wiring exists.

### Detailed conformance matrix

#### SYNOPSIS / OPTIONS / OPERANDS

- [x] XSI `cal [[month] year]`, no options — `Args { month, year }` (`cal.rs:131-149`); `grep -n 'short\|long' cal.rs` → 0 option flags. CONFORMS.
- [x] No operands → current month/year — `cal.rs:219-223` via `chrono::Local::now()`. CONFORMS.
- [x] Single operand → twelve-month calendar for that year — `cal.rs:225-228` moves the lone operand into `year`. Matches RATIONALE 88486–88488. CONFORMS.
- [x] Both operands → one-month calendar — `cal.rs:234-241`. CONFORMS.
- [x] `month` 1–12 enforced — clap `range(1..=9999)` admits the single-operand-is-year trick, then `cal.rs:235-237` rejects `month > 12`; `cal 0` rejected by clap (range floor 1). CONFORMS.
- [x] `year` 1–9999 enforced — clap `range(1..=9999)` (`cal.rs:143`). `cal 83` → A.D. 83 (APPLICATION USAGE 88478). CONFORMS.

#### STDIN / STDOUT / STDERR / INPUT FILES

- [x] STDIN not used — no `stdin()` call. CONFORMS.
- [x] STDOUT displays the calendar in unspecified format — `print_month`/`print_year` (`cal.rs:151-209`). CONFORMS.
- [x] STDERR only for diagnostics — single error via `Err(gettext(...))` → `main` → stderr. CONFORMS.

#### EXTENDED DESCRIPTION — calendar correctness

- [x] Julian calendar Jan 1, 1 – Sep 2, 1752; Gregorian Sep 14, 1752 – Dec 31, 9999 — `is_julian` (`cal.rs:43-59`). CONFORMS.
- [x] September 1752 has 19 days (1, 2, 14–30) — `days_in_month` special case (`cal.rs:105-110`) + gap padding (`cal.rs:171-191`); verified by `tests/cal/mod.rs:109-181`. CONFORMS.
- [x] Julian vs Gregorian leap-year rules — `is_leap_year` (`cal.rs:64-70`); verified for 100/1600/1700 (Julian) and 1900/2000/2024 (Gregorian) in tests. CONFORMS.
- [x] Day-of-week via JDN — `julian_day_number`/`day_of_week` (`cal.rs:74-100`); Jan 1 year 1 = Saturday verified (`tests/cal/mod.rs:238-267`). CONFORMS.

#### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL`/`LC_CTYPE` | CONFORMS | `setlocale` at `cal.rs:212`. |
| `LC_MESSAGES` | PARTIAL | Catalogs absent (tree-wide); the one error string is gettext-wrapped (`cal.rs:236`). |
| `LC_TIME` | **MISSING** | (#C1) month/weekday names hardcoded English. |
| `NLSPATH` (XSI) | MISSING | Tree-wide. |
| `TZ` | CONFORMS | Current month derived from `chrono::Local::now()`, which honors `TZ` (`cal.rs:220`). |

#### EXIT STATUS / ASYNCHRONOUS EVENTS / CONSEQUENCES OF ERRORS

- [x] 0 success / >0 error — `main` returns `Ok`/`Err` (`cal.rs:236,243`). CONFORMS.
- [x] Default async events / consequences — no special handling required. CONFORMS.

### Test coverage signal

Well covered (24 tests): operand forms, Sep-1752 gap, leap-year rules across
Julian/Gregorian eras, day-of-week, month names, range errors. Gaps:
- [ ] No test asserts `LC_TIME` affects month/weekday names (#C1) — would currently fail, documents the gap.

---

## Suggested PR groupings

- **PR A — "time: fix CPU accounting"**: #T1, #T2 (one fix site — add the
  `tms_end` snapshot and rewrite the delta to use `tms_cutime`/`tms_cstime`,
  end−start). Add a test asserting non-zero `user` for a busy-loop child.
- **PR B — "time: propagate utility exit status"**: #T3, #T5. Capture
  `ExitStatus`, propagate `code()` / map signal to 128+n. Add a propagation test.
- **PR C — "sleep: accept `sleep 0`"**: #S1. One-line range change + a small
  test module (the crate currently has none for `sleep`).
- **PR D — "date: century-window off-by-one"**: #D1 (threshold `70`→`69`) plus a
  unit test over the 68/69/70/99 boundaries.
- **PR E — "date: robust strftime output"**: #D2 (empty-vs-overflow), #D4
  (raw-bytes stdout).
- **PR F — "datetime: i18n diagnostics"**: #T4, #D3 (+ #S2, #T6). Route runtime
  diagnostics through `gettext()` with a uniform `<util>:` prefix; normalize
  `bindtextdomain`. Mirrors the `dev/` `plib::diag` migration.
- **PR G — "cal: LC_TIME month/weekday names"**: #C1. Derive names from libc
  under `LC_TIME` (reuse `plib::locale::strftime`).
