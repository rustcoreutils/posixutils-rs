# POSIX.1-2024 Conformance Audits — `datetime/` utilities

This file collects per-utility POSIX conformance audits for the date/time
utilities crate. Each audit follows the playbook in `audits.md`.

**Crate:** `datetime/` — `cal`, `date`, `sleep`, `time` (4 single-file
utilities, ~640 lines total).
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3.
**Reference slices:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/{cal,date,sleep,time}.md`
**Date:** 2026-06-24
**Method:** static spec-vs-code, every "absent"/"wrong-value" claim grep-verified
against the cited lines.

> **Remediation status (2026-06-24).** All actionable findings — **2 Critical,
> 3 Major, 6 Minor** — have since been fixed on the `datetime-audit` branch
> across five themed, independently-committed phases, each with regression
> tests and a clean `build` / `clippy --all-targets` / `fmt` / datetime test
> run:
> - **Phase 1 — `time`** CPU accounting + exit-status propagation (#T1, #T2,
>   #T3, #T5).
> - **Phase 2 — `sleep`** accept `sleep 0` (#S1) + first `sleep` test module.
> - **Phase 3 — `date`** century off-by-one (#D1, `infer_century` + unit test),
>   grow-until-fits `strftime` (#D2), raw-byte output (#D4).
> - **Phase 4 — i18n** adopt `plib::diag::init_locale` + `gettext` diagnostics
>   for `date`/`time` (#T4, #D3, #T6); `#S2` deferred (clap-generated, tree-wide).
> - **Phase 5 — `cal`** `LC_TIME`-aware month/weekday names via
>   `plib::locale::strftime` (#C1).
>
> The only items left open are minor *test-coverage* niceties (a 126/127
> assertion; empty-`strftime`/non-UTF-8-locale display tests) and the tree-wide
> `.mo` catalog deferral (so `LC_MESSAGES` stays inert). All four utilities are
> promoted to README **Stage 6 — Audited**. Checkboxes below are ticked with
> `✓ fixed in Phase N`.

---

## Cross-cutting observations

| Theme | Status | Notes |
|---|---|---|
| `setlocale(LC_ALL, "")` at `main` | Present in all four | `cal.rs:212`, `date.rs:183`, `sleep.rs:25`, `time.rs:134`. |
| `textdomain`/`bind_textdomain_codeset` | Present in all four | `time.rs:136` additionally calls `bindtextdomain("posixutils-rs", "locale")` with a relative path; the other three omit `bindtextdomain` entirely. No `.mo` catalogs ship, so `gettext()` is an identity map regardless. |
| Runtime diagnostics via `gettext()` | **Fixed (Phase 4)** | ~~`cal` wraps its one error string; `date` and `time` use raw `eprintln!`/`Err(&str)` hardcoded English.~~ All four now share `plib::diag::init_locale` and route diagnostics through `plib::diag::error` + `gettext` (`sleep` emits none of its own). Only clap's own parse/range errors stay English (`#S2`, deferred). `.mo` catalog shipping remains a tree-wide deferral. |
| `LC_CTYPE`/`LC_TIME`/`TZ` honored via libc | **Fixed (Phase 5)** | `date`/`cal` defer to libc `strftime`/`localtime_r` (honor `LC_TIME`/`TZ`); `cal`'s month-name/weekday strings ~~do **not** honor `LC_TIME`~~ now derive from `plib::locale::strftime` under `LC_TIME` (#C1). |

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

- [x] **#T1 — `tms_end` is never populated; CPU stats are computed against an all-zero struct.** ✓ fixed in Phase 1 (`libc::times(&mut tms_end)` now called after `child.wait()`). `time.rs:85`. `let tms_end: libc::tms = unsafe { std::mem::zeroed() };` — there is exactly one `libc::times` call in the file (`time.rs:70`, filling `tms_start`); `grep -n 'libc::times' time.rs` → one hit. The end snapshot is required after `child.wait()` to capture accumulated child CPU. As written, `tms_end.*` is always 0. Fix: call `libc::times(&mut tms_end)` after the wait, before computing deltas.
- [x] **#T2 — Wrong fields and wrong subtraction direction.** ✓ fixed in Phase 1 (now `(end.tms_utime+end.tms_cutime) − (start…)`, likewise system, ÷ `_SC_CLK_TCK`). `time.rs:87-88`. POSIX 117331–117335: User CPU time is "the sum of the `tms_utime` and `tms_cutime` fields … for the process in which utility is executed", System CPU likewise `tms_stime + tms_cstime`. The child's CPU is accumulated into the parent's **`tms_cutime`/`tms_cstime`** after `wait()`. The code instead reads `tms_start.tms_utime - tms_end.tms_utime` (parent's *own* user time, start minus end). Even if #T1 is fixed, this measures the wrong process and has the sign inverted. Fix: `user = (tms_end.tms_cutime + tms_end.tms_utime) - (tms_start.tms_cutime + tms_start.tms_utime)`, likewise for system with the `s`-variants; divide by `_SC_CLK_TCK`.

#### Major

- [x] **#T3 — Child exit status is discarded; `time` always exits 0.** ✓ fixed in Phase 1 (`time()` returns the child's code via `Status::Utility(code)`; `main` exits with it). `time.rs:82` (`let _ = child.wait()...`), `time.rs:158` (`Status::Ok.exit()` → 0). POSIX EXIT STATUS 117435: "If the utility utility is invoked, the exit status of `time` shall be the exit status of utility." A successful spawn whose child exits 5 still yields `time` exit 0. Fix: capture `ExitStatus`, and on normal exit propagate `code()`; on signal termination map to 128+signal (see #T5).

#### Minor

- [x] **#T4 — Runtime diagnostics are hardcoded English.** ✓ fixed in Phase 4 (the three diagnostics now route through `plib::diag::error` + `gettext()`). `time.rs:144,148,152`. POSIX 117404: `LC_MESSAGES` shall affect diagnostic message contents.
- [x] **#T5 — Signal-terminated child not reflected in exit status.** ✓ fixed in Phase 1 (`status.code()` else `128 + status.signal()`). `time.rs:82`. When the utility dies from a signal, `time` now exits 128+signum.
- [x] **#T6 — `bindtextdomain("posixutils-rs", "locale")` uses a relative path.** ✓ fixed in Phase 4 (all four binaries now share `plib::diag::init_locale`, which drops the relative `bindtextdomain`). `time.rs:136`.

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
- [x] **Timing values incorrect** — (#T1/#T2) ✓ fixed in Phase 1; CPU stats now reflect the invoked utility.

#### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL`/`LC_CTYPE` | CONFORMS | `setlocale(LC_ALL,"")` at `time.rs:134`. |
| `LC_MESSAGES` | CONFORMS* | (#T4) ✓ Phase 4 routes diagnostics through `gettext`; *catalog shipping is a tree-wide deferral. |
| `LC_NUMERIC` | N/A | Rust `{:.6}` always uses `.` radix; POSIX-locale output is correct, other locales unspecified for the default format. |
| `NLSPATH` (XSI) | MISSING | No catalog support (tree-wide). |
| `PATH` | CONFORMS | Used by `Command` for utility lookup. |

#### EXIT STATUS / CONSEQUENCES OF ERRORS

- [x] 127 when utility not found — `io::ErrorKind::NotFound → CommandNotFound → 127` (`time.rs:77-78,126,145`). CONFORMS.
- [x] 126 when found but not invocable — other spawn errors → `ExecCommand → 126` (`time.rs:79,125,149`). CONFORMS.
- [x] **Exit status of invoked utility not propagated** — (#T3) ✓ fixed in Phase 1; `time` exits with the utility's status.
- [x] 1–125 on internal `time` error — `TimeError → 1` (`time.rs:124,153`). CONFORMS.

#### ASYNCHRONOUS EVENTS

- [x] Default — `grep -nE 'SIGCONT|SIGWINCH|signal' time.rs` → 0 matches; spec says "Default". CONFORMS.

### Test coverage signal

Not covered:
- [x] No test asserts the reported `user`/`sys` values are non-zero for a CPU-bound child (would catch #T1/#T2). — ✓ Phase 1 `cpu_bound_child_reports_nonzero_cpu_time`.
- [x] No test asserts `time` propagates a non-zero child exit code (#T3). — ✓ Phase 1 `propagates_child_exit_status`.
- [ ] No test asserts 126/127 mapping (only "not provided"/clap errors are exercised; `tests/time/mod.rs:51-57`). (127 behaviorally spot-checked; no automated assertion yet.)

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

- [x] **#D1 — 2-digit-year century inference off by one.** ✓ fixed in Phase 3 (extracted `infer_century(yy)` with the `< 69` threshold; unit-tested over 68/69/70/99). `date.rs:128-133`. POSIX 91707–91708: "values in the range [69,99] shall refer to years 1969 to 1999 … values in the range [00,68] shall refer to years 2000 to 2068." The code did `if year < 70 { year + 2000 } else { year + 1900 }`, so `yy=69` → 2069 (should be 1969).

#### Minor

- [x] **#D2 — `strftime` returning 0 is always treated as a fatal error.** ✓ fixed in Phase 3 (grow-until-fits loop to a 64 KiB cap; a 0 return at the cap is an empty-but-valid result → emit just the trailing newline). `date.rs:87-95`. `strftime(3)` returns 0 both when the buffer is too small **and** when the conversion legitimately produces an empty string.
- [x] **#D3 — Runtime diagnostics hardcoded English.** ✓ fixed in Phase 4 (all `eprintln!`/`Err(&str)` diagnostics route through `plib::diag::error` + `gettext()` with a uniform `date:` prefix; the awkward `Error: date: …` double-prefix is gone). `date.rs:49,56,70,94,101,144,154,161,175`. POSIX 91728.
- [x] **#D4 — `from_utf8_lossy` may mangle `strftime` output in non-UTF-8 locales.** ✓ fixed in Phase 3 (`stdout().write_all(&buf[..len])` raw bytes + newline, no lossy decode). `date.rs:88`. `strftime` emits bytes in the locale's `LC_CTYPE` encoding; `String::from_utf8_lossy` replaced invalid sequences with U+FFFD.

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
- [x] **Set form `mmddhhmmyy` (len 10) century inference** — (#D1) ✓ fixed in Phase 3 via `infer_century`.
- [x] Invalid field values rejected — `chrono::with_ymd_and_hms` non-`Single` → `Err` (`date.rs:151-164`). CONFORMS.

#### STDIN / STDOUT / STDERR

- [x] STDIN not used — no `stdin()` call (`grep -n stdin date.rs` → 0). CONFORMS.
- [x] Default output equals `+%a %b %e %H:%M:%S %Z %Y` — `DEF_TIMESTR` (`date.rs:17,190`). CONFORMS.
- [x] STDERR only for diagnostics — all `eprintln!`/error paths target stderr. CONFORMS (#D3 on i18n).

#### ENVIRONMENT VARIABLES

| Var | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL`/`LC_CTYPE` | CONFORMS | `setlocale` at `date.rs:183`; libc `strftime` honors them. |
| `LC_MESSAGES` | CONFORMS* | (#D3) ✓ Phase 4 routes diagnostics through `gettext`; *catalog shipping is a tree-wide deferral. |
| `LC_TIME` | CONFORMS | `strftime` over libc honors `LC_TIME`. |
| `TZ` | CONFORMS | `localtime_r` honors `TZ`; `-u` overrides to UTC. Verified by `tests/date/mod.rs:24-60`. |
| `NLSPATH` (XSI) | MISSING | Tree-wide. |

#### EXIT STATUS / ASYNCHRONOUS EVENTS

- [x] 0 on success, >0 on error — `main` returns `Ok`/propagates `Err` (exit 1); explicit `process::exit(1)` in `show_time` error paths. CONFORMS.
- [x] Default async events — no signal handling; spec says "Default". CONFORMS.

### Test coverage signal

Not covered:
- [x] No test exercises the set-time path at all (would catch #D1) — ✓ Phase 3 added a `century_boundaries` unit test over the `infer_century` helper (set-time itself needs privilege).
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

- [x] **#S1 — `sleep 0` is rejected.** ✓ fixed in Phase 2 (`range(1..)` → `range(0..)`; new `tests/sleep/mod.rs`). `sleep.rs:17-18`: `value_parser!(u64).range(1..)` made the minimum 1, so `sleep 0` exited non-zero with a clap range error. POSIX 115335: the `time` operand is "A non-negative decimal integer" — `0` is valid and must suspend for "at least 0 seconds" (i.e. return immediately with exit 0).

#### Minor

- [x] ~~#S2~~ — **clap diagnostics hardcoded English. DEFERRED (Phase 4).** The range/parse error text is generated by clap, not by sleep, and is uniformly English across every clap-based utility in the tree. Routing it through `gettext` would require a custom clap error formatter; deferred as a tree-wide concern (same disposition as other audits). sleep itself emits no diagnostics. `LC_MESSAGES` remains inert for clap's own errors.

### Detailed conformance matrix

#### SYNOPSIS / OPERANDS

- [x] `sleep time` — single operand (`sleep.rs:17-21`). CONFORMS.
- [x] **`time` non-negative integer** — (#S1) ✓ fixed in Phase 2; `sleep 0` accepted.
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
- [x] No tests exist for `sleep` at all. ✓ Phase 2 added `tests/sleep/mod.rs`: `sleep 0` exits 0 (#S1), `sleep 1` exits 0, non-numeric/negative operand exits >0.

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

- [x] **#C1 — `LC_TIME` not honored for month names / weekday header.** ✓ fixed in Phase 5. `cal.rs` now derives the month name (`%B`) and the 2-char weekday abbreviations (`%a`, with the reference weekday read back via `%w` so the alignment is `TZ`-correct) from `plib::locale::strftime` under `LC_TIME`, with the English literals kept only as a fallback. The C/POSIX-locale output is byte-identical to before (`January`, `Su Mo Tu We Th Fr Sa`); a non-C `LC_TIME` (e.g. `fr_FR.UTF-8` → `janvier`) now localizes the names. POSIX 88457: "`LC_TIME` Determine the format and contents of the calendar." The spec leaves the *format* unspecified (Sunday-first and the column layout are unchanged); the month/day **names** are now an `LC_TIME` responsibility.

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
| `LC_TIME` | CONFORMS | (#C1) ✓ Phase 5: month/weekday names derived from `plib::locale::strftime` under `LC_TIME`. |
| `NLSPATH` (XSI) | MISSING | Tree-wide. |
| `TZ` | CONFORMS | Current month derived from `chrono::Local::now()`, which honors `TZ` (`cal.rs:220`). |

#### EXIT STATUS / ASYNCHRONOUS EVENTS / CONSEQUENCES OF ERRORS

- [x] 0 success / >0 error — `main` returns `Ok`/`Err` (`cal.rs:236,243`). CONFORMS.
- [x] Default async events / consequences — no special handling required. CONFORMS.

### Test coverage signal

Well covered (24 tests): operand forms, Sep-1752 gap, leap-year rules across
Julian/Gregorian eras, day-of-week, month names, range errors. Gaps:
- [x] No test asserts `LC_TIME` affects month/weekday names (#C1) — ✓ Phase 5 added `test_cal_lc_time_french_month_name` (best-effort: `janvier` when `fr_FR` is installed, else `January`); the C-locale name assertions are now pinned to `LC_ALL=C` for determinism.

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
